# Load libraries
library(tidyverse)
library(speedglm)
library(broom)
library(CovSel)
library(bnlearn)
library(ggm)
library(dagitty)
library(lavaan)

# Read data
load("cleancompas.Rdata")

#############
# Filtering #
#############

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Case.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate, -COMPAS)

# Restrict to sentenced cases
data <- data %>%
  filter(Sentence. == TRUE)

# Restrict disposition and cases status
data <- data %>%
  filter(str_detect(Case.Status, "Disposed")) %>%
  filter(str_detect(Disposition, "Adjudicat")) %>%
  droplevels

# Study only Black/white disparity
data <- data %>%
  filter(Race %in% c("Black","White"))

# Add flag for prison time
data <- data %>%  
  mutate(prison = case_when(
    Confinement > 0 ~ TRUE,
    TRUE ~ FALSE))

# Drop some more variables
data <- data %>%
  dplyr::select(-Case.Status, -Sentence., -Sentence.Name, -Confinement, -Fine, -State.Probation, -Disposition, -failuretoappear, -starts_with("charge"))

# Convert logical variables to factor
data <- data %>%
  mutate_if(is.logical, as.factor) %>%
  mutate_if(is.integer, as.numeric)
  

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

# Create whitelist and blacklist
whitelist <- rbind(c("recidivism", "prison"),
                   c("violence", "prison"))
compasvars <- c("recidivism","violence")
exog <- c("Number.of.Charges", "Public.Defender.", "Race", "Gender", "Judge.Name", "Court.Type", "prison")
blacklist1 <- expand.grid(compasvars, compasvars)
blacklist2 <- expand.grid(exog, compasvars)
blacklist3 <- expand.grid(compasvars, setdiff(names(data), "prison"))
blacklist4 <- rbind(c("Court.Type","Number.of.Charges"),
                    c("Public.Defender.","Number.of.Charges"),
                    c("Race","age"),
                    c("Race","Gender"),
                    c("Gender","Race"),
                    c("Plea.s.","Number.of.Charges"),
                    c("Court.Type","age"),
                    c("Court.Type","Race"),
                    c("Public.Defender.","Race")) %>% as.data.frame
names(blacklist4) <- names(blacklist1)
blacklist5 <- expand.grid("prison", names(data))
blacklist6 <- expand.grid("Plea.s.", compasvars)
blacklist <- NULL
blacklist <- do.call(rbind, lapply(ls(patt="blacklist"), get))

# Try to find causal model
# Different commands to try include: hc, tabu, mmhc, rsmax2
dag <- hc(data, whitelist = whitelist, blacklist = blacklist)
fittedbn <- bn.fit(dag, data)
plot(dag)
arcs(dag)



myDAG <- dagitty('
dag {
bb="0,0,1,1"
"criminal history" [latent,pos="0.216,0.474"]
"social antecedents of crime" [latent,pos="0.256,0.831"]
"social stuff" [latent,pos="0.370,0.648"]
Court.Type [pos="0.588,0.491"]
Gender [pos="0.264,0.670"]
Number.of.Charges [pos="0.481,0.415"]
Plea.s. [pos="0.615,0.131"]
Public.Defender. [pos="0.700,0.541"]
Race [pos="0.346,0.755"]
age [pos="0.137,0.614"]
prison [outcome,pos="0.509,0.147"]
recidivism [exposure,pos="0.145,0.217"]
resources [latent,pos="0.526,0.636"]
violence [exposure,pos="0.283,0.274"]
"criminal history" -> Plea.s.
"criminal history" -> recidivism
"criminal history" -> violence
"social antecedents of crime" -> Gender
"social antecedents of crime" -> Race
"social antecedents of crime" -> age
"social stuff" -> Court.Type
"social stuff" -> Number.of.Charges
"social stuff" -> resources
Court.Type -> prison
Gender -> "criminal history"
Gender -> "social stuff"
Number.of.Charges -> Plea.s.
Number.of.Charges -> prison
Plea.s. -> prison
Public.Defender. -> Plea.s.
Public.Defender. -> prison
Race -> "criminal history"
Race -> "social stuff"
age -> "criminal history"
age -> recidivism
age -> violence
recidivism -> prison
resources -> Public.Defender.
violence -> prison
}
')

testdata <- data %>%
  dplyr::select(-Judge.Name) %>%
  mutate(across(where(function(x) nlevels(x) == 2), as.integer)) %>%
  mutate_if(is.factor, as.ordered)
corr <- lavCor(testdata, output = "cov")
testresults <- localTests(myDAG, sample.cov = corr, sample.nobs=nrow(testdata))
plotLocalTestResults(testresults)

