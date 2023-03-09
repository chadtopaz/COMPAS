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

# Define DAG
myDAG <- dagitty('
dag {
bb="0,0,1,1"
"criminal history" [latent,pos="0.216,0.474"]
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
violence [exposure,pos="0.283,0.274"]
Court.Type -> violence
Court.Type -> recidivism
Court.Type -> Plea.s.
Court.Type -> prison
Gender -> "social stuff"
Race -> "social stuff"
age -> "social stuff"
Public.Defender. -> Plea.s.
Public.Defender. -> prison
Number.of.Charges -> violence
Number.of.Charges -> recidivism
Number.of.Charges -> Plea.s.
Number.of.Charges -> prison
Plea.s. -> prison
violence -> prison
recidivism -> prison
"criminal history" -> Plea.s.
"criminal history" -> violence
"criminal history" -> recidivism
"criminal history" -> prison
"social stuff" -> Court.Type
"social stuff" -> Public.Defender.
"social stuff" -> Number.of.Charges
"social stuff" -> violence
"social stuff" -> recidivism
"social stuff" -> "criminal history"
}
')
plot(myDAG)

testdata <- data %>%
  dplyr::select(-Judge.Name) %>%
  mutate(across(where(function(x) nlevels(x) == 2), as.integer)) %>%
  mutate_if(is.factor, as.ordered)
corr <- lavCor(testdata)
testresults <- localTests(myDAG, sample.cov = corr, sample.nobs=nrow(testdata))
plotLocalTestResults(testresults)

adjustmentSets(myDAG, exposure = c("violence", "recidivism"), outcome = "prison", effect = "total")

M <- glm(prison ~ . + Court.Type + Gender + Number.of.Charges + Plea.s. + Public.Defender. + Race + age + violence:Race + recidivism:Race, data = data, family = "binomial")

# Look at results
tidy(M) %>% 
  filter(str_detect(term, "Race|violence|recidivism")) %>%
  View

# Model performance
glance(M2)
nagelkerke(M2)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M2, lmdata2, type = "response")
rocinfo <- roc(lmdata2$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(lmdata2$prison), positive = "TRUE")






# Try to find causal model
# Different commands to try include: hc, tabu, mmhc, rsmax2
data2 <- data %>%
  dplyr::select(-Judge.Name)
compasscores <- c("recidivism", "violence")
demographics <- c("Race", "age", "Gender")
nondemographics <- setdiff(names(data2), demographics)
covariates <- setdiff(names(data2), "prison")
w <- vector(mode = "list", length = 0)
w[[1]] <- expand.grid(compasscores, "prison")
whitelist <- bind_rows(w)
b <- vector(mode = "list", length = 0)
# b[[1]] <- expand.grid(compasscores, compasscores)
# b[[2]] <- expand.grid(demographics, demographics)
# b[[3]] <- expand.grid(compasscores, demographics)
# b[[4]] <- expand.grid("prison", covariates)
# b[[5]] <- expand.grid(nondemographics, demographics)
# b[[6]] <- expand.grid("Court.Type", c("Number.of.Charges", compasscores))
# b[[7]] <- expand.grid("Public.Defender.", c("Court.Type", "Number.of.Charges", compasscores))
# b[[8]] <- expand.grid(compasscores, c("Court.Type", "Number.of.Charges", "Public.Defender."))
# b[[9]] <- expand.grid("Plea.s.", covariates)
blacklist <- bind_rows(b)
dag <- hc(data2, whitelist = whitelist)
plot(dag)
arcs(dag)
g <- as.igraph(dag)
myDAG <- dagitty(graph2dagitty(g))
testdata <- data2 %>%
  mutate(across(where(function(x) nlevels(x) == 2), as.integer)) %>%
  mutate_if(is.factor, as.ordered)
corr <- lavCor(testdata)
testresults <- localTests(myDAG, sample.cov = corr, sample.nobs=nrow(testdata))
plotLocalTestResults(testresults)
adjustmentSets(myDAG, exposure = c("violence", "recidivism"), outcome = "prison", effect = "total")
M <- glm(prison ~ . + Court.Type + Public.Defender. + violence:Race + recidivism:Race, data = data, family = "binomial")
tidy(M) %>% 
  filter(str_detect(term, "Race|violence|recidivism")) %>%
  View
glance(M)
nagelkerke(M)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M, data, type = "response")
rocinfo <- roc(data$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(data$prison), positive = "TRUE")



