# Load libraries
library(tidyverse)
library(caret)
library(ggrepel)
library(broom)
library(pROC)
library(rcompanion)
library(MatchIt)

# Read data
load("cleancompas.Rdata")

#############
# Filtering #
#############

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate, -Number.of.Charges, -Fine, -State.Probation, -violence, -recidivism)

# Keep sentenced cases only
data <- data %>%
  filter(Sentence. == TRUE) %>%
  dplyr::select(-Sentence.)

# Keep only cases that are only felonies
data <- data %>%
  filter(Court.Type == "Felony")

# Study only Black/white disparity
data <- data %>%
  filter(Race %in% c("Black","White"))

# Add flag for prison time
data <- data %>%  
  mutate(prison = case_when(
    Confinement > 0 ~ TRUE,
    TRUE ~ FALSE)) %>%
  dplyr::select(-Confinement)

# Select relevant variables
data <- data %>%
  dplyr::select(Gender, Race,
         Public.Defender., Plea.s., age, COMPAS, prison)

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

# Try matching
m.out <- matchit(COMPAS ~ Public.Defender. + Plea.s. +
                   age + Gender + Race,
                 data = data, method = "cem")
plot(summary(m.out))
m.data <- match.data(m.out, drop.unmatched = TRUE) %>%
  droplevels
M <- glm(prison ~ Race*COMPAS + subclass, weights = weights, data = m.data)
M %>%
  tidy %>%
  filter(str_detect(term, "Race|COMPAS")) %>%
  View

# Look at model performance
glance(M)
nagelkerke(M)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M, m.data, type = "response")
rocinfo <- roc(m.data$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(m.data$prison), positive = "TRUE")
