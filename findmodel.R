# Load libraries
library(tidyverse)
library(speedglm)
library(broom)
library(CovSel)
library(bnlearn)

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
                    c("Race","Gender")) %>% as.data.frame
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
