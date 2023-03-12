# Load libraries
library(tidyverse)
library(broom)
library(rcompanion)
library(caret)
library(pROC)

# Read data
load("cleancompas.Rdata")

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Case.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate, -COMPAS, -Number.of.Charges) %>%
  mutate_if(is.integer, as.numeric)

# Restrict to sentenced cases
data <- data %>%
  # filter(Court.Type == "Felony") %>%
  filter(Sentence. == TRUE) %>%
  dplyr::select(-Sentence.)

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
  dplyr::select(-Case.Status, -Sentence.Name, -Confinement, -Fine, -State.Probation, -Disposition, -failuretoappear)

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

# Create single compas variable
# Drop any level with count of 1
data <- data %>%
  mutate(compas = case_when(
    violence == "none" | recidivism == "none" ~ "none",
    TRUE ~ paste0("v", violence, "r", recidivism))) %>%
  mutate(compas = as.factor(compas)) %>%
  dplyr::select(-violence, -recidivism) %>%
  filter(compas != "vhighrlow") %>%
  droplevels

# Split data
set.seed(123)
trainindex <- createDataPartition(data$prison, p = 0.75, list = FALSE)
datatrain <- data[trainindex, ]
datatest <- data[trainindex, ]

# Create classifier
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats =  10,
                          search = 'random')

logit.CV <- train(x= datatrain[, -prison] , y= TRAIN$Diabetes, 
                  method = 'glmnet',
                  trControl = trControl,
                  family = 'binomial' )
# Calculate how many additional people incarcerated due to COMPAS

preddata <- data %>%
  filter(compas != "none") %>%
  mutate(origcompas = compas) %>%
  mutate(compas = "none")
preddata$pred <- predict(M, newdata = preddata, type = "response")
preddata <- preddata %>%
  mutate(predprison = pred > thresh)

preddata %>%
  filter(Race == "White") %>%
  dplyr::select(prison, predprison) %>%
  mutate(prison = ifelse(prison == TRUE, "detained", "notdetained")) %>%
  mutate(predprison = ifelse(predprison == TRUE, "detained", "notdetained")) %>%
  rename(actual = prison, ifnocompass = predprison) %>%
  table %>%
  prop.table %>%
  round(2)

preddata %>%
  filter(Race == "Black") %>%
  dplyr::select(prison, predprison) %>%
  mutate(prison = ifelse(prison == TRUE, "detained", "notdetained")) %>%
  mutate(predprison = ifelse(predprison == TRUE, "detained", "notdetained")) %>%
  rename(actual = prison, ifnocompass = predprison) %>%
  table %>%
  prop.table %>%
  round(2)