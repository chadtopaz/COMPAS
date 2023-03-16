# Load libraries
library(tidyverse)
library(broom)
library(bnlearn)
library(dagitty)
library(SEMgraph)
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

# Before anything else, assess unconditioned differences

# Look at proportions
data %>%
  mutate(compas = violence != "none") %>%
  group_by(compas, Race, prison) %>%
  summarise(count = n()) %>%
  ungroup(prison) %>%
  mutate(total = sum(count)) %>%
  mutate(p_prison = count/total) %>%
  select(-count, -total) %>%
  filter(prison == TRUE) %>%
  select(-prison) %>%
  pivot_wider(names_from = c(Race), values_from = p_prison)

# Simple model for significance
M0 <- data %>%
  mutate(compas = violence != "none") %>%
  glm(prison ~ Race*compas, data = ., family = "binomial")
M0 %>% tidy %>% View

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

# Convert logical variables to numerical
# Convert factors (except for judge) to numerical
modeldata <- data %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(function(x) nlevels(x) < 50, as.numeric)

# Set up variable groups
compasvars <- c("compas")
demographics <- c("age", "Gender", "Race")
allbutprison <- setdiff(names(data), "prison")
chargevars <- data %>% dplyr::select(starts_with("charge")) %>% names
compasinputs <- c(demographics, chargevars)

# Set up whitelist
whitelist <- vector(mode = "list", length = 0)
whitelist[[1]] <- expand.grid(compasinputs, compasvars)
whitelist[[2]] <- expand.grid(compasvars, "prison")
whitelist[[3]] <- expand.grid(chargevars, "prison")
whitelist <- bind_rows(whitelist)

# Set up blacklist
blacklist <- vector(mode = "list", length = 0)
blacklist[[2]] <- expand.grid(chargevars, chargevars)
blacklist[[3]] <- expand.grid("Public.Defender.", demographics)
blacklist[[4]] <- expand.grid("prison", allbutprison)
blacklist[[5]] <- expand.grid(compasvars, allbutprison)
blacklist[[6]] <- expand.grid("Public.Defender.", compasvars)
blacklist[[7]] <- expand.grid(chargevars, demographics)
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
testresults <- localTests(DAG, data = testdata, max.conditioning.variables = 3)
plotLocalTestResults(testresults)

# Look at adjustment sets
adjsets <- adjustmentSets(DAG, exposure = compasvars, outcome = "prison", effect = "total", type = "canonical")

# Create model
adjindex <- 1
covars <- adjsets[adjindex] %>%
  unlist %>%
  unname %>%
  paste(collapse = "+")
treatment <- "+ compas"
model <- paste0("prison ~ ", covars, treatment) %>% as.formula

# Run model
M <- glm(model, data = data, family = binomial())

# Model performance
glance(M)
nagelkerke(M)$Pseudo.R.squared.for.model.vs.null
prediction <- predict(M, data, type = "response")
rocinfo <- roc(data$prison ~ prediction, plot = TRUE, print.auc = TRUE, print.thres = TRUE)
thresh <- 0.5 # 0.576
prediction <- prediction > thresh
confusionMatrix(factor(prediction), factor(data$prison), positive = "TRUE")

# Look at results
results <- M %>% tidy() %>% 
  filter(str_detect(term, "compas")) %>%
  mutate(term = str_replace_all(term, "compas", ""))
results %>% View

# Get cell proportions and incorporate effects
final <- data %>%
  filter(compas != "none") %>%
  group_by(compas, Race) %>%
  summarize(count = n()) %>%
  group_by(Race) %>%
  mutate(racetotal = sum(count)) %>%
  ungroup %>%
  mutate(prop = count/racetotal) %>%
  dplyr::select(compas, Race, prop) %>%
  merge(results, by.x = "compas", by.y = "term") %>%
  dplyr::select(-std.error, -statistic)

# Split compas variable levels
suppressMessages(compaslevels <- str_match_all(final$compas,"low|medium|high") %>%
                   bind_cols() %>%
                   unname %>%
                   t)
colnames(compaslevels) <- c("violence", "recidivism")
final <- final %>%
  cbind(compaslevels, .) %>%
  dplyr::select(-compas) %>%
  mutate(violence = paste("violence", violence)) %>%
  mutate(recidivism = paste("recidivism", recidivism)) %>%
  mutate(violence = factor(violence, levels = c("violence low", "violence medium", "violence high"))) %>%
  mutate(recidivism = factor(recidivism, levels = c("recidivism low", "recidivism medium", "recidivism high"))) %>%
  mutate(Race = case_match(Race, "White" ~ "white", "Black" ~ "Black")) %>%
  mutate(Race = as.factor(Race)) %>%
  mutate(oddsratio = exp(estimate)) %>%
  dplyr::select(-estimate)

# Make plot
p1 <- final %>%
  filter(p.value <= 0.05) %>%
  ggplot(aes(x = Race, y = prop, fill = Race)) +
  ylab("Proportion of racial group with these COMPAS scores") +
  geom_col() +
  facet_grid(rows = vars(violence), cols = vars(recidivism)) +
  theme(aspect.ratio = 1, legend.position = "none") +
  geom_text(aes(x = 1.5, y = 0.6, label = paste("odds ratio = ", round(oddsratio, 1))))
ggsave(p1, filename = "~/Desktop/compas.pdf", width = 6.5, units = "in")

# Calculate how many additional people incarcerated due to COMPAS

# preddata <- data %>%
#   filter(compas != "none") %>%
#   mutate(origcompas = compas) %>%
#   mutate(compas = "none")
# preddata$pred <- predict(M, newdata = preddata, type = "response")
# preddata <- preddata %>%
#   mutate(predprison = pred > thresh)
# 
# preddata %>%
#   dplyr::select(predprison, Race) %>%
#   mutate(predprison = ifelse(predprison == TRUE, "Detained", "Not Detained")) %>%
#   rename("Without COMPAS (theoretical)" = predprison) %>%
#   table %>%
#   prop.table(2)
# 
# preddata %>%
#   dplyr::select(prison, Race) %>%
#   mutate(prison = ifelse(prison == TRUE, "Detained", "Not Detained")) %>%
#   rename("With COMPAS (actual)" = prison) %>%
#   table %>%
#   prop.table(2)
# 
# preddata %>%
#   filter(Race == "White") %>%
#   dplyr::select(prison, predprison) %>%
#   mutate(prison = ifelse(prison == TRUE, "detained", "notdetained")) %>%
#   mutate(predprison = ifelse(predprison == TRUE, "detained", "notdetained")) %>%
#   rename(actual = prison, ifnocompass = predprison) %>%
#   table %>%
#   prop.table %>%
#   round(2)
# 
# preddata %>%
#   filter(Race == "Black") %>%
#   dplyr::select(prison, predprison) %>%
#   mutate(prison = ifelse(prison == TRUE, "detained", "notdetained")) %>%
#   mutate(predprison = ifelse(predprison == TRUE, "detained", "notdetained")) %>%
#   rename(actual = prison, ifnocompass = predprison) %>%
#   table %>%
#   prop.table %>%
#   round(2)