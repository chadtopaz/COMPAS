# Load Libraries
library(tidyverse)
library(lubridate)
library(pbmcapply)

# Read files from data scraping
data2006 <- read.csv("BrowardScapped2006.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))
data2007 <- read.csv("BrowardScapped2007.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))
data2007 <- data2007[-1,]
datacompas <- read.csv("BrowardScappedProPublica.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))
datacompas <- datacompas[-1,] 
datacompas <- datacompas %>%
  filter(Case.Number != "") %>%
  filter(str_sub(Case.Number,-5,-4) %in% c("CF","MM")) %>%
  filter(str_sub(Case.Number,1,2) %in% c("13","14")) %>%
  filter(Sentence. == "Yes")
  
# Work with precompas data
precompas <- rbind(data2006,data2007) %>%
  unique %>%
  filter(Sentence. == "Yes") %>%
  mutate(violence = "none", recidivism = "none") %>%
  select(-Case.Type, -Language, -Complexion, -Height, -Weight, -Eye, -Hair,
         -Birth.Location, -Number.of.Charges, -Date.Filed.of.First.Charge, -State.Probation,
         -Fine, -Sentence.Name, -Disposition, -Charge.Name.of.First.Charge)

# Work with COMPAS data
postcompas <- read.csv("compas-scores.csv") %>%
  filter(c_case_number != "") %>%
  filter(str_sub(c_case_number,-5,-4) %in% c("CF","MM")) %>%
  filter(str_sub(c_case_number,1,2) %in% c("13","14")) %>%
  merge(datacompas, ., by.x = "Case.Number", by.y = "c_case_number") %>%
  filter(Sentence. == "Yes") %>%
  rename(violence = v_score_text, recidivism = score_text) %>%
  mutate(violence = tolower(violence), recidivism = tolower(recidivism)) %>%
  select(all_of(names(precompas)))

# Combine data
data1 <- bind_rows(precompas, postcompas) %>%
  mutate(violence = factor(violence, levels = c("none","low","medium","high"))) %>%
  mutate(recidivism = factor(recidivism, levels = c("none","low","medium","high"))) %>%
  dplyr::select(-Sentence., -Case.Number)

#################
### FILTERING ###
#################

# Restrict disposition and cases status
data2 <- data1 %>%
  filter(str_detect(Case.Status, "Disposed")) %>%
  dplyr::select(-Case.Status)

# Study only Black/white disparity
data2 %>% pull(Race) %>% table
data3 <- data2 %>%
  filter(Race %in% c("Black","White")) %>%
  droplevels %>%
  mutate(Race = case_match(Race,
                           "White" ~ "white",
                           .default = Race)) %>%
  mutate(Race = factor(Race, levels = c("white", "Black")))

################
### CLEANING ###
################

# Turn court type into factor
data4 <- data3 %>%
  mutate(Court.Type = factor(Court.Type, levels = c("Traffic and Misdemeanor", "Felony")))

# Clean up judge names  
data5 <- data4 %>%
  mutate(Judge.Name = replace(Judge.Name, Judge.Name == "", NA)) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"\\.","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name," \\- [:upper:]{2,}","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"\\- [:upper:]{2,}","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"^[:upper:]{2,} ","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"Schiff, Louis H","Schiff, Louis")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"Judge, Unassigned",NA_character_)) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"MT, Unassigned",NA_character_)) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"Unassigned",NA_character_)) %>%
  mutate(Judge.Name = factor(Judge.Name))

# Clean gender
data6 <- data5 %>%
  mutate(Gender = replace(Gender, Gender == "", NA)) %>%
  mutate(Gender = factor(Gender))

# Calculate age
data7 <- data6 %>%
  mutate(DOB = as.Date(DOB,"%m/%d/%Y")) %>%
  mutate(age = as.numeric(Date - DOB)/365) %>%
  dplyr::select(-DOB, -Date)

# Clean public defender
data8 <- data7 %>%
  mutate(Public.Defender. = case_when(
    Public.Defender. == "Yes" ~ TRUE,
    Public.Defender. == "No" ~ FALSE,
    TRUE ~ NA))

# Handle charges
charges <- data8 %>%
  pull(Current.Statutes.of.All.Charges) %>%
  str_extract_all("(?<=(^|; )\\()[:alnum:]+") %>%
  unlist %>%
  unique %>%
  sort() %>%
  paste0("\\(", ., "\\)")
counts <- matrix(NA,nrow = nrow(data8),ncol = length(charges))
countCharges <- function(i){
  as.numeric(sapply(charges,function(x) str_count(data8[i,]$Current.Statutes.of.All.Charges,x)))
}
counts <- pbmclapply(1:nrow(data8), countCharges)
counts <- do.call(rbind,counts)
counts <- as.data.frame(counts)
names(counts) <- paste0("charge",charges) %>%
  str_replace_all("(\\(|\\)|\\\\)", "")
data9 <- cbind(data8,counts) %>%
  dplyr::select(-Current.Statutes.of.All.Charges)

# Clean confinement
data10 <- data9 %>%
  mutate(Confinement = str_squish(Confinement)) %>%
  mutate(years = str_extract(Confinement,"[:digit:]+(?= Year)")) %>%
  mutate(months = str_extract(Confinement,"[:digit:]+(?= Month)")) %>%
  mutate(days = str_extract(Confinement,"[:digit:]+(?= Day)")) %>%
  mutate(across(c("years","months","days"), as.numeric)) %>%
  mutate(across(c("years","months","days"), replace_na, replace = 0)) %>%
  mutate(Confinement = 12*years + months + (days/30)) %>%
  dplyr::select(-years,-months, -days) %>%
  mutate(prison = Confinement > 0) %>%
  dplyr::select(-Confinement)

# Clean plea
data11 <- data10 %>%
  mutate(Plea.s. = replace(Plea.s., Plea.s. == "", NA)) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Nolo"), "nolo")) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"lightgray"), NA)) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Not Guilty"), "notguilty")) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Guilty"), "guilty")) %>%
  mutate(Plea.s. = as.factor(Plea.s.))

# Create flag for before/after COMPAS and collapse compas into single variable
data12 <- data11 %>%
  mutate(compasflag = ifelse(violence == "none" & recidivism == "none", FALSE, TRUE)) %>%
  mutate(compas = case_when(
    violence == "none" | recidivism == "none" ~ "none",
    TRUE ~ paste0("v", violence, "r", recidivism))) %>%
  mutate(compas = as.factor(compas)) %>%
  dplyr::select(-violence, -recidivism) %>%
  droplevels

# Make sure numeric variables are numeric
data13 <- data12 %>% 
  mutate_if(is.integer, as.numeric)

# Get rid of incomplete cases
data14 <- data13 %>%
  drop_na()

# Set final data
data <- data14

# Write data
save(data, file = "cleancompas.Rdata")