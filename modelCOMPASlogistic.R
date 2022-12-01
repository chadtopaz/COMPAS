# Load libraries
library(tidyverse)
library(caret)
library(pbmcapply)
library(recipes)
library(speedglm)
library(multcomp)
library(ggrepel)
library(AICcmodavg)
library(broom)
library(pROC)
 
# Read data
load("cleancompas.Rdata")

# Add ID number for merging
data <- data %>%
  mutate(ID = 1:nrow(data))

###################
# First Analysis: #
# System Level    #
###################

# Restrict to sentenced felonies
lmdata <- data %>%
  filter(Sentence. == TRUE)

# Study only Black/white disparity
lmdata <- lmdata %>%
  filter(Race %in% c("Black","White")) %>%
  droplevels()

# Add flag for prison time
lmdata <- lmdata %>%  
  mutate(prison = case_when(
    Confinement > 0 ~ TRUE,
    TRUE ~ FALSE))

# Keep variables with most potential for relevancy
lmdata <- lmdata %>%
  dplyr::select(ID,prison,COMPAS,Race,Public.Defender.,Plea.s.,age,starts_with("charge",ignore.case = FALSE))

# Keep complete records only
lmdata <- lmdata %>%
  na.omit %>%
  droplevels

# Fit model
M <- speedglm(prison ~ . - ID + COMPAS:Race, data = lmdata, family = binomial())
tidy(M) %>% View

# Look at model performance
glance(M)
lmdata$prediction <- predict(M, lmdata, type = "response")
rocinfo <- roc(lmdata$prison ~ lmdata$prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5
lmdata$prediction <- lmdata$prediction > thresh
confusionMatrix(factor(lmdata$prediction), factor(lmdata$prison), positive = "TRUE")


####################
# Second Analysis: #
# Judge Level      #
####################

# # Restrict to sentenced felonies
# lmdata <- data %>%
#   filter(Sentence. == TRUE)
# 
# # Study only Black/white disparity
# lmdata <- lmdata %>%
#   filter(Race %in% c("Black","White")) %>%
#   droplevels()
# 
# # Add flag for prison time
# lmdata <- lmdata %>%
#   mutate(prison = case_when(
#     Confinement > 0 ~ TRUE,
#     TRUE ~ FALSE))
# 
# lmorig <- lmdata
# 
# # Drop small number of cases with unassigned judges
# lmdata <- lmdata %>%
#   filter(Judge.Name != "Unassigned") %>%
#   droplevels
# 
# # Check number of cases for each judge, for each race, before/after COMPAS
# tab <- table(lmdata$Judge.Name,lmdata$Race,factor(lmdata$COMPAS, levels = c("FALSE","TRUE"))) %>%
#   data.frame
# names(tab) <- c("Judge.Name","Race","COMPAS","count")
# tab <- pivot_wider(tab, names_from = c("Race","COMPAS"), values_from = "count")
# judges <- tab %>%
#   filter(White_FALSE >= 10 & Black_FALSE >= 10 & White_TRUE >= 10 & Black_TRUE >= 10) %>%
#   pull(Judge.Name) %>%
#   factor(levels = levels(lmdata$Judge.Name))
# lmdata <- lmdata %>%
#   filter(Judge.Name %in% judges) %>%
#   droplevels
# 
# # Restrict data to variables from preferred model from above
# lmdata <- lmdata %>%
#   dplyr::select(Judge.Name, prison, COMPAS, Race, Public.Defender., Plea.s., chargeF2, chargeF3, chargeM1)
# 
# # Keep complete cases only
# lmdata <- lmdata %>%
#   na.omit
# 
# studyJudge <- function(judge) {
# 
#   # Restrict data to relevant judge
#   judgedata <- lmdata %>%
#     filter(Judge.Name == judge) %>%
#     dplyr::select(-Judge.Name)
# 
#   # Fit model
#   tryCatch(
#     {
#       M <- glm(prison ~ . + COMPAS:Race, data = judgedata, family = "binomial")
#       L <- matrix(rep(0,3*length(coef(M))),nrow = 3)
#       colnames(L) <- names(coef(M))
#       L[1,"RaceBlack"] <- 1
#       L[2,c("RaceBlack","COMPASTRUE:RaceBlack")] <- 1
#       L[3, "COMPASTRUE:RaceBlack"] <- 1
#       rownames(L) <- c("before","after","delta")
#       glht(M, linfct = L, alternative = "greater") %>%
#         summary(test = adjusted("none")) %>%
#         tidy %>%
#         dplyr::select(contrast,estimate,p.value) %>%
#         rename("disparity" = "contrast") %>%
#         mutate(accuracy = accuracy) %>%
#         mutate(judge = judge)
#     },
#     error = function(cond){
#       return(NULL)
#     }
#   )
# }
# byjudge <- pbmclapply(judges, studyJudge) %>%
#   bind_rows %>%
#   mutate(adj.p.vale = p.adjust(p.value))

# Plotty time!
lmdata %>%
  group_by(Race,COMPAS,prison) %>%
  tally() %>%
  group_by(Race,COMPAS) %>%
  mutate(total = sum(n)) %>%
  mutate(p = n/total) %>%
  dplyr::select(-n, -total) %>%
  filter(prison == TRUE) %>%
  ggplot(aes(x = Race, y = p, fill = COMPAS)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Race of Defendant") +
  ylab("Proportion of Convicted Defendants Confined") +
  scale_fill_discrete(labels = c("Before","After")) +
  theme(legend.position = c(0.37,0.87))