# Load libraries
library(tidyverse)
library(caret)
library(pbmcapply)
library(recipes)
library(speedglm)
library(multcomp)
library(ggrepel)
library(broom)
library(pROC)
library(rcompanion)
library(glmnet)

# Read data
load("cleancompas.Rdata")

#############
# Filtering #
#############

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Case.Type, -Court.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate)

# Restrict to certain cases??
data <- data %>%
  filter(Sentence. == TRUE) %>%
  filter(str_detect(Case.Status, "Disposed"))

# Disposition, Sentence., Case.Status, Sentence.Name
# data %>%
#   group_by(Disposition, Case.Status, Sentence.Name) %>%
#   summarise(count = n()) %>%
#   ungroup %>%
#   mutate(total = sum(count)) %>%
#   mutate(prop = count/total) %>%
#   mutate(prop = round(prop, 3)) %>%
#   dplyr::select(-count, -total) %>%
#   arrange(desc(prop)) %>%
#   View

# # Restrict to certain case status categories
# data <- data %>%
#   filter(!str_detect(Case.Status, "Post|Modification|Mitigation")) %>%
#   dplyr::select(-Case.Status)

# Study only Black/white disparity
data <- data %>%
  filter(Race %in% c("Black","White"))

# Remove sentencing types that probably don't make sense
# data <- data %>%
#   filter(!str_detect(Sentence.Name, "Amended|Reopen|Resentencing|Time|BCJ")) %>%
#   dplyr::select(-Sentence.Name)

# Remove obvious error (4368 months of confinement)
data <- data %>%
  filter(Confinement < 4000)

# Add flag for prison time
data <- data %>%  
  mutate(prison = case_when(
    Confinement > 0 ~ TRUE,
    TRUE ~ FALSE))

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

# Fit model to predict incarceration or not
lmdata <- data %>%
  dplyr::select(Gender, Race, Public.Defender.,
                Plea.s., COMPAS, age,
                starts_with("charge"), prison) %>%
  droplevels
M <- glm(prison ~ . + COMPAS:Race, data = lmdata, family = "binomial")
tidy(M) %>% 
  filter(str_detect(term, "Race|COMPAS")) %>%
  View

# Look at model performance
glance(M)
nagelkerke(M)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M, lmdata, type = "response")
rocinfo <- roc(lmdata$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(lmdata$prison), positive = "TRUE")

# Plot coefficients
tidy(M) %>%
  filter(str_detect(term, "Race|violence|recidivism")) %>%
  na.omit %>%
  mutate(padj = p.adjust(p.value, method = "BH")) %>%
  View

####################
# Second Analysis: #
# Judge Level      #
####################

# Drop small number of cases with unassigned judges
lmdata <- data %>%
  dplyr::select(Judge.Name, Gender, Race, Public.Defender.,
                Plea.s., COMPAS, age,
                starts_with("charge"), prison) %>%
  filter(Judge.Name != "Unassigned") %>%
  droplevels
 
# Check number of cases for each judge, for each race, before/after COMPAS
tab <- lmdata %>%
  group_by(Judge.Name, Race) %>%
  summarise(before = sum(!COMPAS), after = sum(COMPAS))
tab <- pivot_wider(tab, names_from = c("Race"), values_from = c("before","after"), values_fill = 0)

# Require 30 in every category
tab <- tab %>%
  filter(before_White > 10 &
           before_Black > 10 &
           after_White > 10 &
           after_Black > 10)

judges <- tab %>%
  pull(Judge.Name)

# Study judges
studyJudge <- function(judge) {

  # Restrict data to relevant judge
  judgedata <- lmdata %>%
    filter(Judge.Name == judge) %>%
    dplyr::select(-Judge.Name)
  judgedata <- judgedata[,sapply(lapply(judgedata, unique), length) > 1]

  # Fit model
  tryCatch(
    {
      M <- glm(prison ~ . + COMPAS:Race, data = judgedata, family = "binomial")
      tidy(M) %>% 
        filter(str_detect(term, "Race|COMPAS"))},
    error = function(cond){
      return(NULL)
    }
  )
}
byjudge <- pbmclapply(judges, studyJudge, mc.cores = 35)
organizeResults <- function(i) {
  byjudge[[i]] %>%
    dplyr::select(term, estimate, p.value) %>%
    mutate(judge = judges[i]) %>%
    pivot_wider(names_from = term, values_from = c("estimate", "p.value"))
}
results <- lapply(1:length(byjudge), organizeResults) %>% bind_rows

# # Plotty time!
# tmp <- lmdata %>%
#   group_by(Race,violence,recidivism,prison) %>%
#   tally() %>%
#   group_by(Race,violence,recidivism) %>%
#   mutate(total = sum(n)) %>%
#   mutate(p = n/total) %>%
#   dplyr::select(-n, -total) %>%
#   filter(prison == TRUE) %>%
#   ggplot(aes(x = Race, y = p, fill = COMPAS)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   xlab("Race of Defendant") +
#   ylab("Proportion of Convicted Defendants Confined") +
#   scale_fill_discrete(labels = c("Before","After")) +
#   theme(legend.position = c(0.37,0.87))