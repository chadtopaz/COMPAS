# Load libraries
library(tidyverse)
library(broom)
library(bnlearn)
library(ggm)
library(dagitty)
library(lavaan)
library(SEMgraph)
library(rcompanion)
library(caret)
library(pROC)
library(parallel)

# Make cluster
# cl <- makeCluster(35)

# Read data
load("cleancompas.Rdata")

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Case.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate, -COMPAS, -Number.of.Charges) %>%
  mutate_if(is.integer, as.numeric)

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
  dplyr::select(-Case.Status, -Sentence., -Sentence.Name, -Confinement, -Fine, -State.Probation, -Disposition, -failuretoappear)

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

# Convert logical variables to numerical
# Convert factors (except for judge) to numerical
modeldata <- data %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(function(x) nlevels(x) < 10, as.numeric)

# Set up variable groups
compasvars <- c("violence", "recidivism")
demographics <- c("age", "Gender", "Race")
allbutprison <- setdiff(names(data), "prison")
chargevars <- data %>% dplyr::select(starts_with("charge")) %>% names
offensevars <- c(chargevars, "Court.Type")
compasinputs <- c(demographics, chargevars)

# Set up whitelist
whitelist <- vector(mode = "list", length = 0)
whitelist[[1]] <- expand.grid(compasinputs, compasvars)
# whitelist[[2]] <- expand.grid(chargevars, "Court.Type")
# whitelist[[1]] <- expand.grid("violence", "prison")
# whitelist[[2]] <- expand.grid("recidivism", "prison")
# whitelist[[4]] <- expand.grid(demographics, "Public.Defender.")
# whitelist[[5]] <- expand.grid(demographics, chargevars)
whitelist <- bind_rows(whitelist)

# Set up blacklist
blacklist <- vector(mode = "list", length = 0)
blacklist[[1]] <- expand.grid("Court.Type", chargevars)
blacklist[[2]] <- expand.grid(chargevars, chargevars)
blacklist[[3]] <- expand.grid("Public.Defender.", demographics)
blacklist[[4]] <- expand.grid("prison", allbutprison)
blacklist[[5]] <- expand.grid("Public.Defender.", compasvars)
blacklist[[6]] <- expand.grid(offensevars, demographics)
# blacklist[[1]] <- expand.grid(compasvars, compasvars)
# blacklist[[2]] <- expand.grid(demographics, demographics)
# blacklist[[5]] <- expand.grid(offensevars, compasvars)
# blacklist[[6]] <- expand.grid(compasvars, offensevars)
# blacklist[[7]] <- expand.grid(compasvars, "Public.Defender.")
# blacklist[[8]] <- expand.grid("Plea.s.", compasvars)
# blacklist[[9]] <- expand.grid("Public.Defender.", "Court.Type")
# blacklist[[10]] <- expand.grid("Public.Defender.", demographics)
# blacklist[[11]] <- expand.grid("Public.Defender.", chargevars)
# blacklist[[12]] <- expand.grid("Court.Type", demographics)
blacklist <- bind_rows(blacklist)

# Learn DAG
# Possible commands: hc, mmhc, tabu, pc.stable
dag <- tabu(modeldata, whitelist = whitelist, blacklist = blacklist)
plot(dag)
arcs(dag)

# Convert learned network to dagitty
DAG <- dag %>%
  as.igraph %>%
  graph2dagitty %>%
  dagitty
exposures(DAG) <- compasvars
outcomes(DAG) <- "prison"

# Test implications
testdata <- modeldata %>%
  mutate_if(is.factor, as.numeric)
testresults <- localTests(DAG, data = testdata)
plotLocalTestResults(testresults)

# Look at adjustment sets
adjsets <- adjustmentSets(DAG, exposure = c("violence", "recidivism"), outcome = "prison", effect = "total", type = "canonical")

# Create model
adjindex <- 1
covars <- adjsets[adjindex] %>%
  unlist %>%
  unname %>%
  setdiff("violence") %>%
  # setdiff("Court.Type") %>%
  paste(collapse = "+")
# interactions <- "+ Race*violence + Race*recidivism"
interactions <- "+ Race*recidivism"
model <- paste0("prison ~ ",covars,interactions) %>% as.formula

# Run model on one judge
judgedata <- data
  # filter(Judge.Name == "Kollra, Ernest A")
M <- glm(model, data = judgedata, family = "binomial")

# Look at results
tidy(M) %>% 
  filter(str_detect(term, "Race|violence|recidivism")) %>%
  View

# Model performance
glance(M)
nagelkerke(M)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M, data, type = "response")
rocinfo <- roc(data$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(data$prison), positive = "TRUE")