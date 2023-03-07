# Load libraries
library(tidyverse)
library(speedglm)
library(broom)
library(CovSel)
library(bnlearn)
library(ggm)

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


# Test some in/dependencies
testImplications <- function( covariance.matrix, sample.size ){
  
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("Court.Type","Gender"),
                       c("Court.Type","Number.of.Charges","Race"),
                       c("Court.Type","Plea.s.","Judge.Name"),
                       c("Court.Type","age"),
                       c("Court.Type","recidivism"),
                       c("Court.Type","violence"),
                       c("Gender","Judge.Name"),
                       c("Gender","Plea.s."),
                       c("Gender","Public.Defender."),
                       c("Gender","Race"),
                       c("Gender","age"),
                       c("Gender","prison"),
                       c("Gender","recidivism"),
                       c("Gender","violence"),
                       c("Judge.Name","Number.of.Charges","Race"),
                       c("Judge.Name","Number.of.Charges","Court.Type"),
                       c("Judge.Name","Public.Defender.","Court.Type"),
                       c("Judge.Name","Race","Court.Type"),
                       c("Judge.Name","age"),
                       c("Judge.Name","prison","Court.Type"),
                       c("Judge.Name","recidivism"),
                       c("Judge.Name","violence"),
                       c("Number.of.Charges","Plea.s.","Judge.Name"),
                       c("Number.of.Charges","Plea.s.","Court.Type"),
                       c("Number.of.Charges","Plea.s.","Race"),
                       c("Number.of.Charges","Public.Defender.","Race"),
                       c("Number.of.Charges","age"),
                       c("Number.of.Charges","prison","Public.Defender.","Court.Type"),
                       c("Number.of.Charges","prison","Race"),
                       c("Number.of.Charges","recidivism"),
                       c("Number.of.Charges","violence"),
                       c("Plea.s.","Public.Defender.","Court.Type"),
                       c("Plea.s.","Public.Defender.","Judge.Name"),
                       c("Plea.s.","Race","Court.Type"),
                       c("Plea.s.","Race","Judge.Name"),
                       c("Plea.s.","age"),
                       c("Plea.s.","prison","Court.Type"),
                       c("Plea.s.","prison","Judge.Name"),
                       c("Plea.s.","recidivism"),
                       c("Plea.s.","violence"),
                       c("Public.Defender.","age"),
                       c("Public.Defender.","recidivism"),
                       c("Public.Defender.","violence"),
                       c("Race","age"),
                       c("Race","prison","Public.Defender.","Court.Type"),
                       c("Race","recidivism"),
                       c("Race","violence"),
                       c("age","prison"),
                       c("age","recidivism"),
                       c("age","violence"),
                       c("recidivism","violence"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
}
covariance.matrix <- cov(data)