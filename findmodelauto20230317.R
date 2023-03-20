  # Load libraries
library(tidyverse)
library(broom)
library(bnlearn)
library(dagitty)
library(SEMgraph)
library(rcompanion)
library(caret)
library(pROC)
library(pbmcapply)

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
data <- data %>%
  mutate(compas = case_when(
    violence == "none" | recidivism == "none" ~ "none",
    TRUE ~ paste0("v", violence, "r", recidivism))) %>%
  mutate(compas = as.factor(compas)) %>%
  dplyr::select(-violence, -recidivism) %>%
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
blacklist[[1]] <- expand.grid("Court.Type", chargevars)
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

# Drop unassigned judge
judgedata <- data %>%
  filter(Judge.Name != "Unassigned") %>%
  droplevels
length(unique(judgedata$Judge.Name))

# Keep only judges who had pre and post compas (at least 2 cases each)
judgekeeps <- judgedata %>%
  group_by(Judge.Name) %>%
  summarise(compasno = sum(compas == "none"), compasyes = sum(compas != "none")) %>%
  filter(compasno >= 2 & compasyes >= 2) %>%
  pull(Judge.Name)
judgedata <- judgedata %>%
  filter(Judge.Name %in% judgekeeps) %>%
  droplevels
length(unique(judgedata$Judge.Name))

# Find judges who had more than one court type
judgedata %>%
  group_by(Judge.Name) %>%
  summarise(felony = sum(Court.Type == "Felony"), misdemeanor = sum(Court.Type == "Traffic and Misdemeanor")) %>%
  filter(felony > 0 & misdemeanor > 0)
# The only one is Judge Porth, with 226 felonies and 2 misdemeanors. Drop the misdemeanors.
judgedata <- judgedata %>%
  filter(!(Judge.Name == "Porth, Ari Abraham" & Court.Type == "Traffic and Misdemeanor"))
# Drop Court.Type
judgedata <- judgedata %>%
  select(-Court.Type)

# Split data
judgedata <- judgedata %>% split(judgedata$Judge.Name)

# Create per-judge model (excludes Judge.Name and Court.Type)
adjindex <- 1
covars <- adjsets[adjindex] %>%
  unlist %>%
  unname %>%
  setdiff("Judge.Name") %>%
  setdiff("Court.Type") %>%
  paste(collapse = "+")
treatment <- "+ compas"
judgemodel <- paste0("prison ~ ", covars, treatment) %>% as.formula

# Fit each model
judgefits <- pbmclapply(judgedata, function(x) glm(judgemodel, data = x, family = binomial))

# Compile results
getresults <- function(x) {
  results <- x %>% tidy() %>% 
    filter(str_detect(term, "compas")) %>%
    mutate(term = str_replace_all(term, "compas", "")) %>%
    rename(compas = term) %>%
    mutate(Judge.Name = x$data$Judge.Name[1])
}
judgeresults <- lapply(judgefits, getresults) %>% bind_rows
# Adjust p values -- bonferroni is THE most conservative
judgeresults <- judgeresults %>%
  mutate(p.adj = p.adjust(p.value, method = "bonferroni"))

# Look at results
sigresults <- judgeresults %>%
  filter(p.adj < 0.05) %>%
  mutate(oddsratio = exp(estimate))

# For each of these judges, compute their racial breakdown in low/low category
judgelist <- sigresults$Judge.Name
racetabs <- data %>%
  filter(Judge.Name %in% judgelist) %>%
  filter(compas != "none") %>%
  group_by(Judge.Name, Race) %>%
  mutate(cases = n()) %>%
  filter(compas == "vlowrlow") %>%
  mutate(lowlowcases = n()) %>%
  select(Judge.Name, Race, cases, lowlowcases) %>%
  unique %>%
  mutate(prop = lowlowcases/cases) %>%
  select(Judge.Name, Race, prop) %>%
  pivot_wider(names_from = "Race", values_from = "prop")

# Compile results
sigresults <- sigresults %>%
  merge(racetabs) %>%
  select(Judge.Name, oddsratio, White, Black) %>%
  pivot_longer(c("White","Black"), names_to = "Race", values_to = "prop")

# Make plot
p2 <- sigresults %>%
  ggplot(aes(x = Race, y = prop, fill = Race, group = Race)) +
  geom_col() +
  ylim(0, 0.8) +
  xlab(NULL) +
  ylab("For each judge's set of cases that have a COMPAS score,\nproportion of racial group with Low-Low score") +
  facet_wrap(vars(Judge.Name), nrow = 3) +
  theme(legend.position = "none") +
  geom_text(aes(x = 1.5, y = 0.76, label = paste("odds ratio = ", format(round(oddsratio, 2), nsmall = 2))))
ggsave(p2, filename = "~/Desktop/compas.pdf", width = 6.5, units = "in")
