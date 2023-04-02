# Load libraries
library(tidyverse)
library(cobalt)
library(scales)
library(broom)
library(texreg)
library(bnlearn)
library(dagitty)
library(SEMgraph)
library(knitr)
library(grid)
library(pbmcapply)

hex <- hue_pal()(6)

# Read data
load("cleancompas.Rdata")
data <- data %>%
  filter(compas != "vhighrlow")

#######################################
### Study unconditioned differences ###
#######################################

# Look at proportions
toplineresults <- data %>%
  group_by(compasflag, Race, prison) %>%
  summarise(count = n()) %>%
  ungroup(prison) %>%
  mutate(total = sum(count)) %>%
  mutate(p_prison = count/total) %>%
  filter(prison == TRUE) %>%
  select(-prison) %>%
  mutate(compasflag = case_match(compasflag,
                             FALSE ~ "Before COMPAS",
                             TRUE ~ "With COMPAS")) %>%
  mutate(compasflag = factor(compasflag)) %>%
  rename("Time Period" = compasflag, "Defendant Race" = Race, "Frequency of Imprisonment" = p_prison)

topline <- toplineresults %>%
  mutate(`Frequency of Imprisonment` = round(`Frequency of Imprisonment`, 3)) %>%
  ggplot(aes(x = `Time Period`, y = `Frequency of Imprisonment`, fill = `Defendant Race`, group = `Defendant Race`)) +
  geom_col(position = position_dodge()) +
  ylim(0, 1) +
  scale_fill_manual(values = hex[c(2,4)]) +
  geom_text(aes(label=`Frequency of Imprisonment`), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_text(aes(y = 0, label=paste0("out of\n",total)), position=position_dodge(width=0.9), vjust=-0.35) +
  theme(legend.position = "top",
        axis.title.x = element_blank())
ggsave("topline.pdf", plot = topline, width = 3.3, height = 3.3, units = "in")  

# Simple model for significance
M0 <- data %>%
  glm(prison ~ Race*compasflag, data = ., family = "binomial")

# Format output
M0 %>%
  texreg(single.row = TRUE,
         digits = 3,
         booktabs = TRUE,
         siunitx = TRUE,
         custom.model.names = "Estimate (Std. Error)",
         custom.coef.names = c("Intercept", "Black", "COMPAS", "Black x COMPAS"),
         caption.above = TRUE,
         caption = "Logistic regression modeling judges' choices (no/yes) to mandate prison
         as part of criminal sentences in Broward County, Florida. The predictors in this
         simple model are defendant race (white/Black), existence of a COMPAS risk score for
         the defendant (no/yes), and the interaction of the two. The positive coefficient for Black defendants
         indicates that they have a higher probability of receiving prison as part of their sentences. The
         negative coefficient on COMPAS indicates that use of the risk algorithm is associated with
         a lower probabilty prison time (decarceration). The positive coefficient on the interaction of race
         and COMPAS suggests that Black defendants experience less COMPAS-induced decarceration than white
         defendants.",
         label = "tab:topline")

### DROP COMPAS FLAG
data <- data %>%
  dplyr::select(-compasflag)

#################
### Learn DAG ###
#################

# Convert factors (except for judge) to numerical
modeldata <- data %>%
  mutate_if(is.logical, as.numeric) %>%
  mutate_if(function(x) nlevels(x) < 50, as.numeric)

# Set up variable groups
compasvars <- c("compas")
demographics <- c("age", "Gender", "Race")
allbutprison <- setdiff(names(data), "prison")
allbutprisonandplea <- setdiff(names(data), c("prison","Plea.s."))
chargevars <- data %>% dplyr::select(starts_with("charge")) %>% names
compasinputs <- c("age", "Gender")

# Set up whitelist
whitelist <- vector(mode = "list", length = 0)
whitelist[[1]] <- expand.grid(compasinputs, compasvars)
whitelist[[2]] <- NULL#expand.grid(compasvars, "prison")
whitelist[[3]] <- expand.grid(chargevars, "Court.Type")
whitelist[[4]] <- expand.grid(chargevars, "prison")
whitelist[[5]] <- expand.grid("Plea.s.", "prison")
whitelist[[6]] <- expand.grid("Judge.Name", "prison")
whitelist[[7]] <- expand.grid("Public.Defender", "Plea.s.")
whitelist <- bind_rows(whitelist)

# Set up blacklist
blacklist <- vector(mode = "list", length = 0)
blacklist[[1]] <- expand.grid(demographics, demographics)
blacklist[[2]] <- NULL#expand.grid(demographics, "Court.Type")
blacklist[[3]] <- expand.grid(chargevars, demographics)
blacklist[[4]] <- expand.grid(chargevars, chargevars)
blacklist[[5]] <- expand.grid(chargevars, compasvars)
blacklist[[6]] <- expand.grid("Court.Type", demographics)
blacklist[[7]] <- expand.grid("Court.Type", chargevars)
blacklist[[8]] <- expand.grid("Court.Type", compasvars)
blacklist[[9]] <- expand.grid("Public.Defender.", demographics)
blacklist[[10]] <- expand.grid("Public.Defender.", compasvars)
blacklist[[11]] <- expand.grid("Judge.Name", demographics)
blacklist[[12]] <- expand.grid("Judge.Name", "Court.Type")
blacklist[[13]] <- expand.grid("Judge.Name", "Public.Defender.")
blacklist[[14]] <- expand.grid("Judge.Name", compasvars)
blacklist[[15]] <- expand.grid("Plea.s.", allbutprison)
blacklist[[16]] <- expand.grid(compasvars, allbutprisonandplea)
blacklist[[17]] <- expand.grid("prison", allbutprison)
blacklist[[18]] <- expand.grid("Court.Type", "Court.Type")
blacklist[[19]] <- expand.grid("Public.Defender.", "Public.Defender.")
blacklist[[20]] <- expand.grid("Judge.Name", "Judge.Name")
blacklist[[21]] <- expand.grid("prison", "prison")
blacklist <- bind_rows(blacklist)

# Learn DAG
# Possible commands: hc, mmhc, tabu, pc.stable
daghc <- hc(modeldata, whitelist = whitelist, blacklist = blacklist)
dagtabu <- tabu(modeldata, tabu = 25, whitelist = whitelist, blacklist = blacklist)
identical(arcs(daghc), arcs(dagtabu))

# Convert learned network to dagitty
DAG <- dagtabu %>%
  as.igraph %>%
  graph2dagitty %>%
  dagitty
exposures(DAG) <- compasvars
outcomes(DAG) <- "prison"

# Look at adjustment sets
adjsets <- adjustmentSets(DAG, exposure = compasvars, outcome = "prison", effect = "total", type = "canonical")
length(unlist(adjsets))

#####################
### Visualize DAG ###
#####################

blacklist2 <- blacklist %>%
  rename(From = Var1, To = Var2) %>%
  mutate(value = -Inf)

whitelist2 <- whitelist %>%
  rename(From = Var1, To = Var2) %>%
  mutate(value = Inf)

A <- dagtabu %>%
  as.igraph %>%
  as_adjacency_matrix() %>%
  as.matrix %>%
  as.data.frame %>%
  rownames_to_column("From") %>%
  relocate(From) %>%
  pivot_longer(cols = -one_of("From"), names_to = "To")

A <- A %>%
  anti_join(whitelist2, by = c("From", "To")) %>%
  anti_join(blacklist2, by = c("From", "To")) %>%
  bind_rows(whitelist2, blacklist2)

A <- A %>%
  mutate(across(c("From", "To"), .fns = function(x)
    case_match(x,
               "age" ~ "Age",
               "compas" ~ "COMPAS Score",
               "Court.Type" ~ "Court Type",
               "Judge.Name" ~ "Judge",
               "Plea.s." ~ "Plea",
               "prison" ~ "Prison",
               "Public.Defender." ~ "Public Defender",
               .default = x))) %>%
  mutate(across(c("From", "To"), .fns = function(x)
    str_replace_all(x, "charge", "Charge "))) 

varorder <- c("Gender",
              "Age",
              "Race",
              A %>% filter(str_detect(From, "Charge")) %>% pull(From) %>% unique %>% sort,
              "Court Type",
              "Public Defender",
              "Judge",
              "Plea",
              "COMPAS Score",
              "Prison")

A <- A %>%
  mutate(From = factor(From, levels = rev(varorder))) %>%
  mutate(To = factor(To, levels = varorder)) %>%
  mutate(value = as.character(value)) %>%
  mutate(value = replace(value, To == From, "self")) %>%
  mutate(value = factor(value, levels = c("-Inf","0","1","Inf","self")))

dagmatrix <- A %>% ggplot(aes(x = To, y = From, fill = value)) +
  geom_tile(color = "white") +
  scale_x_discrete(position = "top", name = "Effect") +
  scale_y_discrete(name = "Cause") +
  scale_fill_manual(breaks = c("self", "-Inf", "0" , "1", "Inf"), values = c("black", hex[1], "grey90", hex[5], hex[3]), labels = c("Not allowed (self-causality)", "Manually excluded", "Excluded by algorithm", "Included by algorithm", "Manually included"), name = NULL) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = -90, hjust=1, vjust = 0.5, size = 7),
        axis.text.y = element_text(size = 7),
        aspect.ratio = 1)
ggsave(plot = dagmatrix, "dagmatrix.pdf", width = 6.5, units = "in")
plot_crop("dagmatrix.pdf")

#############################
### Test for independence ###
#############################

# Convert to numeric
testdata <- data %>%
  mutate_if(is.factor, as.numeric)

# Test implications
testresults <- localTests(DAG, data = testdata, R = 500) %>%
  arrange(estimate) %>%
  mutate(order = factor(row_number())) %>%
  mutate(danger = case_when(
    `97.5%` < -0.10 | `2.5%` > 0.1 ~ TRUE,
    TRUE ~ FALSE))

# Plot
independence <- testresults %>%
  ggplot(aes(x = order, color = danger)) +
  geom_point(aes(y = estimate), size = 0.3) +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c(hex[6], "grey40")) +
  scale_x_discrete(expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.1), limits = c(-0.26, 0.26), name = "Test Statistic") +
  geom_hline(yintercept = c(-0.1, 0.1), lty = "longdash", color = hex[6]) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(plot = independence, "independence.pdf", width = 6.5, height = 2.5, units = "in")
plot_crop("independence.pdf")

###########################################################
### Introduce latent variable to eliminate dependencies ###
###########################################################

latentstuff <- "socialfactors[latent]
  crimhist[latent]
  age -> socialfactors
  Race -> socialfactors
  Gender -> socialfactors
  socialfactors -> crimhist
  crimhist -> charge0
  crimhist -> chargeCO3
  crimhist -> chargeF1
  crimhist -> chargeF2
  crimhist -> chargeF3
  crimhist -> chargeF5
  crimhist -> chargeF6
  crimhist -> chargeF7
  crimhist -> chargeM1
  crimhist -> chargeM2
  crimhist -> chargeMO3
  crimhist -> chargeNI0
  crimhist -> chargeTC4
  crimhist -> chargeTCX
  crimhist -> Plea.s.
  crimhist -> prison
}"

DAGlatent <- DAG %>%
  as.character %>%
  str_remove("\\}\n$") %>%
  paste0(latentstuff) %>%
  dagitty

# Test implications again
testresultslatent <- localTests(DAGlatent, data = testdata) %>%
  arrange(estimate) %>%
  mutate(order = factor(row_number())) %>%
  mutate(danger = case_when(
    `97.5%` < -0.1 | `2.5%` > 0.1 ~ TRUE,
    TRUE ~ FALSE))

# Plot
testresultslatent %>%
  ggplot(aes(x = order, color = danger)) +
  geom_point(aes(y = estimate), size = 0.3) +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c(hex[6], "grey40")) +
  scale_x_discrete(expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.1), limits = c(-0.26, 0.26), name = "Test Statistic") +
  geom_hline(yintercept = c(-0.1, 0.1), lty = "longdash", color = hex[6]) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# Look at adjustment sets
adjsetslatent <- adjustmentSets(DAGlatent, exposure = compasvars, outcome = "prison", effect = "total", type = "canonical")
length(unlist(adjsetslatent))
identical(adjsets, adjsetslatent)

#######################
### Run Main Models ###
#######################

# Minimal
adjsetsminimal <- adjustmentSets(DAG, exposure = compasvars, outcome = "prison", effect = "total", type = "minimal")
adjsetscanonical <- adjustmentSets(DAG, exposure = compasvars, outcome = "prison", effect = "total", type = "canonical")

# Create models
modelminimal <- adjsetsminimal[[2]] %>%
  unlist %>%
  unname %>%
  paste(collapse = "+") %>%
  paste0("prison ~ ", ., "+ compas") %>%
  as.formula
modelcanonical <- adjsetscanonical %>%
  unlist %>%
  unname %>%
  paste(collapse = "+") %>%
  paste0("prison ~ ", ., "+ compas") %>%
  as.formula
modelcanonicalint <- adjsetscanonical %>%
  unlist %>%
  unname %>%
  paste(collapse = "+") %>%
  paste0("prison ~ ", ., "+ Race*compas") %>%
  as.formula

# Run models
Mminimal <- glm(modelminimal, data = data, family = binomial())
Mcanonical <- glm(modelcanonical, data = data, family = binomial())
Mcanonicalint <- glm(modelcanonicalint, data = data, family = binomial())

# Compile results
texreg(l = list(Mminimal, Mcanonical, Mcanonicalint),
          single.row = TRUE,
          digits = 2,
          booktabs = TRUE,
          siunitx = TRUE,
          caption.above = TRUE,
          custom.model.names = c("(1) Minimal Adjustment", "(2) Canonical Adjustment", "(3) Canonical Adjustment + Interaction"),
          custom.coef.map = list(
            "compasvlowrlow" = "Low/Low",
            "compasvlowrmedium" = "Low/Medium",
            "compasvlowrhigh" = "Low/High",
            "compasvmediumrlow" = "Medium/Low",
            "compasvmediumrmedium" = "Medium/Medium",
            "compasvmediumrhigh" = "Medium/High",
            "compasvhighrlow" = "High/Low",
            "compasvhighrmedium" = "High/Medium",
            "compasvhighrhigh" = "High/High",
            "chargeCO3" = "Comm. Ordinace",
            "chargeF1" = "Felony 1",
            "chargeF2" = "Felony 2",
            "chargeM1" = "Misdemeanor 1",
            "GenderMale" = "Male",
            "RaceBlack" = "Black",
            "age" = "Age",
            "Court.TypeFelony" = "Felony Court",
            "Public.Defender.TRUE" = "Public Defender",
            "Plea.s.nolo" = "Plea Nolo",
            "Plea.s.notguilty" = "Plea Not Guilty"),
          groups = list("COMPAS Score" = 1:8, "Charges" = 9:12, "Demographics" = 13:15, "Courtroom Variables" = 16:18),
          custom.gof.rows = list("\\quad Judge Fixed Effects" = c("No", "Yes", "Yes"),
                                 "\\quad Interaction of Race and COMPAS" = c("No", "No", "Yes")),
          caption = "Model summaries for logistic regressions using the minimal and canonical
          adjustment sets arising from the causal model specified in Figure~\\ref{fig:dag}. The outcome variable is
          whether a defendant found guilty was given prison as part of their sentence. The top section of the table provides
          coefficients and (in parentheses) standard errors. The bottom section provides goodness-of-fit measures.
          The Aikake Information Criterion (AIC) and Bayes Information Criterion (BIC) suggest that the model with canonical
          adjustment set and no interaction terms is preferred.
          For brevity, we only present charge variables that are statistically significant, and we do not present
          the individual judge effect coefficients when the canonical adjustment set is used. See Figure~\\ref{fig:compasresult} for
          interpretation of COMPAS score coefficients.",
          label = "tab:mainmodel")

# Compile results
results <- Mcanonical %>% tidy() %>% 
  filter(str_detect(term, "compas")) %>%
  mutate(term = str_replace_all(term, "compas", ""))

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
  mutate(oddsratio = exp(estimate)) %>%
  dplyr::select(-estimate)
  # complete(violence, recidivism, Race, fill = list(prop = 0, p.value = 1, oddsratio = NA)) 

# Make plot
oddsratiolabels <- final %>%
  mutate(oddsratio =paste("OR = ", round(oddsratio, 2))) %>%
  mutate(oddsratio = replace(oddsratio, p.value >= 0.05, "")) %>%
  pull(oddsratio)
compasresult <- final %>%
  ggplot(aes(x = Race, y = prop, fill = Race)) +
  geom_col() +
  geom_text(aes(label=round(prop,3)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(name = "Proportion of racial group\nhaving these COMPAS scores", limits = c(0,1), breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = hex[c(2,4)]) +
  facet_grid(rows = vars(violence), cols = vars(recidivism)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_text(aes(x = 1.5, y = 0.85, label = oddsratiolabels))

# Remove unused facet
grob <- ggplotGrob(compasresult);
idx <- which(grob$layout$name == "panel-3-1");
grob$grobs[[idx]] <- nullGrob();
idx <- which(grob$layout$name %in% c("axis-b-1"));
grob$layout[idx, c("t", "b")] <- grob$layout[idx, c("t", "b")] - 2
idx <- which(grob$layout$name %in% c("axis-l-3"));
grob$layout[idx, c("l", "r")] <- grob$layout[idx, c("l", "r")] + 2
# grid.newpage()
# grid.draw(grob)

# Save plot
ggsave(plot = grob, filename = "compasresult.pdf", width = 6.5, height = 6.5/1.618, units = "in")

######################################
### Run Model on Individual Judges ###
######################################

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
  filter(Judge.Name != "Gilman, Allison") %>%
  pull(Judge.Name)
judgedata <- judgedata %>%
  filter(Judge.Name %in% judgekeeps) %>%
  droplevels
length(unique(judgedata$Judge.Name))

# All judges except one had only one court type.
# The only one is Judge Porth, with both: 226 felonies and 2 misdemeanors. Drop the misdemeanors.
judgedata %>%
  group_by(Judge.Name) %>%
  summarise(felony = sum(Court.Type == "Felony"), misdemeanor = sum(Court.Type == "Traffic and Misdemeanor")) %>%
  print(n = 50)
judgedata <- judgedata %>%
  filter(!(Judge.Name == "Porth, Ari Abraham" & Court.Type == "Traffic and Misdemeanor")) %>%
  filter(!(Judge.Name == "Merrigan, Edward H, Jr" & Court.Type == "Traffic and Misdemeanor"))
# Drop Court.Type
judgedata <- judgedata %>%
  select(-Court.Type)

# Split data
judgedata <- judgedata %>% split(judgedata$Judge.Name)


# Create per-judge model (excludes Judge.Name and Court.Type)
judgecanonical <- adjsetscanonical %>%
  unlist %>%
  unname %>%
  setdiff("Judge.Name") %>%
  setdiff("Court.Type") %>%
  paste(collapse = "+") %>%
  paste0("prison ~ ", ., "+ compas") %>%
  as.formula

# Fit each model
judgefits <- pbmclapply(judgedata, function(x) glm(judgecanonical, data = x, family = binomial))

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
  group_by(Judge.Name, Race, compas) %>%
  summarise(cases = n()) %>%
  ungroup %>%
  group_by(Judge.Name, Race) %>%
  mutate(casesforthisrace = sum(cases)) %>%
  ungroup %>%
  filter(compas == "vlowrlow") %>%
  mutate(lowlowraceprop = cases/casesforthisrace) %>%
  select(Judge.Name, Race, lowlowraceprop, casesforthisrace)

# Compile results
sigresults <- sigresults %>%
  merge(racetabs) %>%
  select(Judge.Name, oddsratio, Race, lowlowraceprop, casesforthisrace)
judgeorder <- sigresults %>%
  select(-casesforthisrace) %>%
  pivot_wider(names_from = "Race", values_from = "lowlowraceprop") %>%
  mutate(disparity = white - Black) %>%
  arrange(desc(disparity)) %>%
  pull(Judge.Name)
sigresults <- sigresults %>%
  mutate(Judge.Name = factor(Judge.Name, levels = judgeorder))

# Make plot
judgeresults <- ggplot() +
  geom_col(data = sigresults, aes(x = Race, y = lowlowraceprop, fill = Race, group = Race), position = position_dodge()) +
  scale_y_continuous(name = "Proportion of racial group having low-low score",
                     limits = c(0,1),
                     breaks = c(0, 0.5, 1)) +
  geom_text(data = distinct(sigresults, Judge.Name, oddsratio), aes(x = 1.5, y = 0.8, label = paste0("OR ",format(round(oddsratio, 2), nsmall = 2))), size = 7*0.35) +
  geom_text(data = sigresults, aes(x = Race, y = lowlowraceprop, label = format(round(lowlowraceprop,2), nlow = 2)), position=position_dodge(width=0.9), vjust=-0.25, size = 7*0.35) +
  geom_text(data = sigresults, aes(x = Race, y = 0, label = casesforthisrace), position=position_dodge(width=0.9), vjust=-0.25, size = 7*0.35) +
  scale_fill_manual(values = hex[c(2,4)]) +
  facet_wrap(~ Judge.Name, nrow = 1) +
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(0.1, "inches"))
ggsave(filename = "judges.pdf", plot = judgeresults, width = 6.5, height = 6.5/1.618, units = "in")
