# Load libraries
library(tidyverse)
library(caret)
library(pbmcapply)
library(speedglm)
library(broom)
library(pROC)
library(rcompanion)
library(misty)

# Read data
load("cleancompas.Rdata")

#############
# Filtering #
#############

# Drop irrelevant variables
data <- data %>%
  dplyr::select(-Date, -Case.Number, -Case.Type, -Court.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair, -DOB, -Birth.Location, -Date.Filed.of.First.Charge, -Current.Statutes.of.All.Charges, -Charge.Name.of.First.Charge, -filedate)

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

# Keep complete records only
data <- data %>%
  na.omit %>%
  droplevels

#########################################
# Exploration 1                         #
# Distribution of COMPAS scores by race #
#########################################

tmp1 <- data %>%
  filter(violence != "none") %>%
  group_by(Race, violence) %>%
  summarise(count = n()) %>%
  ungroup %>%
  group_by(Race) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  rename(score = violence) %>%
  mutate(whichscore = "violence") 
tmp2 <- data %>%
  filter(recidivism != "none") %>%
  group_by(Race, recidivism) %>%
  summarise(count = n()) %>%
  ungroup %>%
  group_by(Race) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  rename(score = recidivism) %>%
  mutate(whichscore = "recidivism") 
scoresbyrace <- bind_rows(tmp1, tmp2) %>%
  mutate(score = factor(score, levels = c("high", "medium", "low")))
facetlabels <- c("Recidivism","Violence")
names(facetlabels) <- c("recidivism","violence")
scoresbyrace %>%
  ggplot(aes(x = Race, y = prop, fill = score)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ whichscore, labeller = labeller(whichscore = facetlabels)) +
  scale_fill_discrete(breaks = c("low", "medium", "high"), labels = c("Low", "Medium", "High"), name = "Score") +
  ylab("Proportion")

###################################################
# Exploration 2                                   #
# Distribution of prison decision by COMPAS score #
###################################################

tmp1 <- data %>%
  filter(violence != "none") %>%
  group_by(prison, violence) %>%
  summarise(count = n()) %>%
  ungroup %>%
  group_by(violence) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  rename(score = violence) %>%
  mutate(whichscore = "violence") 
tmp2 <- data %>%
  filter(recidivism != "none") %>%
  group_by(prison, recidivism) %>%
  summarise(count = n()) %>%
  ungroup %>%
  group_by(recidivism) %>%
  mutate(total = sum(count)) %>%
  mutate(prop = count/total) %>%
  rename(score = recidivism) %>%
  mutate(whichscore = "recidivism") 
prisonbyscore <- bind_rows(tmp1, tmp2) %>%
  mutate(score = factor(score, levels = c("high", "medium", "low")))
facetlabels <- c("Recidivism","Violence")
names(facetlabels) <- c("recidivism","violence")
prisonbyscore %>%
  ggplot(aes(x = score, y = prop, fill = prison)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ whichscore, labeller = labeller(whichscore = facetlabels)) +
  scale_x_discrete(limits = c("low", "medium", "high"), labels = c("Low", "Medium", "High"), name = "Score") +
  scale_fill_discrete(breaks = c(TRUE, FALSE), labels = c("TRUE","FALSE"), name = "Prison") +
  ylab("Proportion")

############################################
# Analysis 1:                              #
# Association between decision to imprison #
# and existence of COMPAS score            #
############################################

# Prep data
lmdata1 <- data %>%
  dplyr::select(Gender, Race, Public.Defender.,
                Plea.s., COMPAS, age,
                starts_with("charge"), prison) %>%
  droplevels

# Fit model
M1 <- glm(prison ~ . + COMPAS:Race, data = lmdata1, family = "binomial")

# Look at results
tidy(M1) %>% 
  View

# Model performance
glance(M1)
nagelkerke(M1)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M1, lmdata1, type = "response")
rocinfo <- roc(lmdata1$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(lmdata1$prison), positive = "TRUE")

##################################################
# Analysis 2:                                    #
# Association between decision to imprison       #
# and actual COMPAS violence + recidivism scores #
##################################################

# Prep data
lmdata2 <- data %>%
  dplyr::select(Gender, Race, Public.Defender.,
                Plea.s., violence, recidivism, age,
                starts_with("charge"), prison) %>%
  droplevels

# Fit model
M2 <- glm(prison ~ . + violence:Race + recidivism:Race, data = lmdata2, family = "binomial")

# Look at results
tidy(M2) %>% 
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

#####################################################
# Analysis 3:                                       #
# Association between decision to imprison          #
# and existence of COMPAS score, separated by judge #
#####################################################

# Prep data
lmdata3 <- data %>%
  filter(Judge.Name != "Unassigned") %>%
  dplyr::select(Gender, Race, Public.Defender.,
                Plea.s., COMPAS, age,
                starts_with("charge"), prison, Judge.Name) %>%
  droplevels
judges <- lmdata3 %>%
  group_by(Judge.Name, COMPAS, Race) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c("COMPAS","Race"), values_from = "count", values_fill = NA) %>%
  na.omit %>%
  pull(Judge.Name)
lmdata3 <- lmdata3 %>%
  filter(Judge.Name %in% judges)

# Study each judge
modeljudge <- function(thisjudge){
  judgedata <- lmdata3 %>%
    filter(Judge.Name == thisjudge) %>%
    dplyr::select(-Judge.Name)
  M <- glm(prison ~ . + COMPAS:Race, data = judgedata, family = "binomial")
  M %>%
    tidy %>%
    filter(str_detect(term, "COMPAS|Race")) %>%
    mutate(judge = thisjudge) %>%
    as.data.frame %>%
    relocate(judge)
}
res <- pbmclapply(judges, modeljudge, mc.cores = min(35, length(judges)))
res <- res %>% bind_rows

# Look at results
res %>%
  filter(term == "RaceBlack:COMPASTRUE") %>%
  mutate(p.adj = p.adjust(p.value, method = "holm")) %>%
  View