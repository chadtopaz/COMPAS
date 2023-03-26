# Load Libraries
library(tidyverse)
library(lubridate)
library(RSQLite)
library(pbmcapply)

# Read files from data scraping
data1 <- read.csv("BrowardScapped2006.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))
data2 <- read.csv("BrowardScapped2007.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))
data2 <- data2[-1,]
data3 <- read.csv("BrowardScappedProPublica.csv") %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
  filter(year(Date) %in% c(2013, 2014)) %>%
  mutate(filedate = as.Date(Date,Date.Filed.of.First.Charge,"%m/%d/%Y")) %>%
  dplyr::select(-Date.Filed.of.First.Charge)
data3 <- data3[-1,]

# Deal with compas scores
# take case number and date from data3
# go to case arrest and get person id
# go to compas and take scores that are
# before and most proximate to case date
con <- dbConnect(SQLite(), 'compas.db')
# dbListTables(con)
casearrest <- dbGetQuery(con, "SELECT * FROM casearrest") %>%
  dplyr::select(name, case_number, person_id) %>%
  unique
compas <- dbGetQuery(con, "SELECT * FROM compas") %>%
  mutate(screening_date = as.Date(screening_date))
# i <- 188
getScores <- function(i) {
  casenumber <- data3[i,]$Case.Number
  filedate <- data3[i,]$filedate
  personID <- casearrest %>%
    filter(case_number == casenumber) %>%
    pull(person_id) %>%
    unique
  personID <- ifelse(length(personID) == 1, personID, NA)
  if(is.na(personID)){
    return(data.frame(violence = NA, recidivism = NA, failuretoappear = NA))
  }
  compasdata <- compas %>%
    filter(person_id == personID) %>%
    mutate(timediff = abs(as.numeric(screening_date - filedate))) %>%
    filter(timediff >= 0) %>%
    filter(timediff == min(timediff)) %>%
    dplyr::select(-id, -scale_set, -compas_assessment_id) %>%
    unique()
  if(nrow(compasdata) != 3){
    return(data.frame(violence = NA, recidivism = NA, failuretoappear = NA))
  }
  scores <- compasdata %>%
    dplyr::select(type_of_assessment, score_text) %>%
    mutate(type_of_assessment = str_replace_all(type_of_assessment, "Risk of", "")) %>%
    mutate(type_of_assessment = str_replace_all(type_of_assessment, " ", "")) %>%
    mutate(type_of_assessment = tolower(type_of_assessment)) %>%
    mutate(score_text = tolower(score_text)) %>%
    pivot_wider(names_from = "type_of_assessment", values_from = "score_text")
  return(scores)
}
scores <- pbmclapply(1:nrow(data3), getScores, mc.cores = 35) %>%
  bind_rows
data3 <- cbind(data3, scores)

# Combine data
data12 <- bind_rows(data1, data2) %>%
  mutate(violence = "none", recidivism = "none", failuretoappear = "none")
data <- bind_rows(data12, data3) %>%
  mutate(violence = factor(violence, levels = c("none","low","medium","high"))) %>%
  mutate(recidivism = factor(recidivism, levels = c("none","low","medium","high"))) %>%
  mutate(failuretoappear = factor(failuretoappear, levels = c("none","low","medium","high")))

# Clean data
data <- data %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y"))

data <- data %>%
  mutate(Court.Type = replace(Court.Type, Court.Type == "", NA)) %>%
  mutate(Court.Type = factor(Court.Type, levels = c("Traffic and Misdemeanor", "Felony")))

data <- data %>%
  mutate(Case.Type = replace(Case.Type, Case.Type == "", NA)) %>%
  mutate(Case.Type = factor(Case.Type))

data <- data %>%
  mutate(Case.Status = factor(Case.Status))

data <- data %>%
  mutate(Judge.Name = replace(Judge.Name, Judge.Name == "", NA)) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"\\.","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name," \\- [:upper:]{2,}","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"\\- [:upper:]{2,}","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"^[:upper:]{2,} ","")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"Schiff, Louis H","Schiff, Louis")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"Judge, Unassigned","Unassigned")) %>%
  mutate(Judge.Name = str_replace(Judge.Name,"MT, Unassigned","Unassigned")) %>%
  mutate(Judge.Name = factor(Judge.Name))

data <- data %>%
  mutate(Gender = replace(Gender, Gender == "", NA)) %>%
  mutate(Gender = factor(Gender))

data <- data %>%
  mutate(Race = replace(Race, Race %in% c("","Unavailable"), NA)) %>%
  mutate(Race = factor(Race)) %>%
  mutate(Race = relevel(Race, ref = "White"))

data <- data %>%
  mutate(Language = replace(Language, Language == "", NA)) %>%
  mutate(Language = factor(Language))

data <- data %>%
  mutate(Complexion = replace(Complexion, Complexion == "", NA)) %>%
  mutate(Complexion = factor(Complexion))

data <- data %>%
  mutate(feet = str_extract(Height,"[:digit:]+(?=')")) %>%
  mutate(feet = as.numeric(feet)) %>%
  mutate(inches = str_extract(Height,"(?<=')[:digit:]+(?=\")")) %>%
  mutate(inches = replace(inches, is.na(inches), 0)) %>%
  mutate(inches = as.numeric(inches)) %>%
  mutate(Height = 12*feet + inches) %>%
  dplyr::select(-feet,-inches)

data <- data %>%
  mutate(Eye = as.factor(Eye))

data <- data %>%
  mutate(Hair = as.factor(Hair))

data <- data %>%
  mutate(DOB = as.Date(DOB,"%m/%d/%Y")) %>%
  mutate(age = as.numeric(Date - DOB)/365)

data <- data %>%
  mutate(Public.Defender. = case_when(
    Public.Defender. == "Yes" ~ TRUE,
    Public.Defender. == "No" ~ FALSE,
    TRUE ~ NA))

data <- data %>%
  mutate(Date.Filed.of.First.Charge = as.Date(Date.Filed.of.First.Charge,"%m/%d/%Y"))

charges <- data %>%
  pull(Current.Statutes.of.All.Charges) %>%
  str_extract_all("(?<=(^|; )\\()[:alnum:]+") %>%
  unlist %>%
  unique %>%
  sort()
charges2 <- paste0("\\(",charges,"\\)")
counts <- matrix(NA,nrow = nrow(data),ncol = length(charges2))
countCharges <- function(i){
  as.numeric(sapply(charges2,function(x) str_count(data[i,]$Current.Statutes.of.All.Charges,x)))
}
counts <- pbmclapply(1:nrow(data), countCharges)
counts <- do.call(rbind,counts)
counts <- as.data.frame(counts)
names(counts) <- paste0("charge",charges)
data <- cbind(data,counts)

data <- data %>%
  mutate(Sentence. = case_when(
    Sentence. == "Yes" ~ TRUE,
    Sentence. == "No" ~ FALSE,
    TRUE ~ NA))

data <- data %>%
  mutate(Sentence.Name = replace(Sentence.Name, Sentence.Name == "", NA)) %>%
  mutate(Sentence.Name = as.factor(Sentence.Name))

data <- data %>%
  mutate(Confinement = str_squish(Confinement)) %>%
  mutate(years = str_extract(Confinement,"[:digit:]+(?= Year)")) %>%
  mutate(months = str_extract(Confinement,"[:digit:]+(?= Month)")) %>%
  mutate(days = str_extract(Confinement,"[:digit:]+(?= Day)")) %>%
  mutate(across(c("years","months","days"), as.numeric)) %>%
  mutate(across(c("years","months","days"), replace_na, replace = 0)) %>%
  mutate(Confinement = 12*years + months + (days/30)) %>%
  dplyr::select(-years,-months, -days)

data <- data %>%
  mutate(Fine = replace(Fine, Fine == "", 0)) %>%
  mutate(Fine = gsub("\\$|\\,","",Fine)) %>%
  mutate(Fine = str_squish(Fine)) %>%
  mutate(Fine = as.numeric(Fine))

data <- data %>%
  mutate(State.Probation = str_squish(State.Probation)) %>%
  mutate(years = str_extract(State.Probation,"[:digit:]+(?= Year)")) %>%
  mutate(months = str_extract(State.Probation,"[:digit:]+(?= Month)")) %>%
  mutate(days = str_extract(State.Probation,"[:digit:]+(?= Day)")) %>%
  mutate(across(c("years","months","days"), as.numeric)) %>%
  mutate(across(c("years","months","days"), replace_na, replace = 0)) %>%
  mutate(State.Probation = 12*years + months + (days/30)) %>%
  dplyr::select(-years,-months, -days)

data <- data %>%
  mutate(Disposition = replace(Disposition, Disposition == "", NA)) %>%
  mutate(Disposition = factor(Disposition))

data <- data %>%
  mutate(Plea.s. = replace(Plea.s., Plea.s. == "", NA)) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Nolo"), "nolo")) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"lightgray"), NA)) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Not Guilty"), "notguilty")) %>%
  mutate(Plea.s. = replace(Plea.s., str_detect(Plea.s.,"Guilty"), "guilty")) %>%
  mutate(Plea.s. = as.factor(Plea.s.))

# Filter out cases that are outside of target dates
data <- data %>%
  filter(year(Date) %in% c(2006, 2007, 2013, 2014))

# Create flag for before/after COMPAS
data <- data %>%
  mutate(COMPAS = ifelse(year(Date) <= 2007, FALSE, TRUE))

# Write data
save(data, file = "cleancompas.Rdata")