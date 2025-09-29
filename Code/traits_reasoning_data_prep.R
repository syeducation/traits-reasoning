#### Traits and Reasoning Project ####
# Script for cleanining: variable labels
# Created on Sep 28, 2025, by Moin Syed
# Checked on DATE, by NAME

#############################################

#### Workspace setup ####

library(dplyr)
library(codebook)
library(labelled)
library(haven)

sessionInfo()

# R version 4.4.2
# haven_2.5.4     
#labelled_2.14.0 
#codebook_0.9.6  
#dplyr_1.1.4    

#############################################

#### Data import ####

# loading in data from Box directory, changed to location outside of file
# from: ~\Box\P-STEM\Data\Raw Data - DO NOT MODIFY\Wave 1
dat <- read.csv("PSTEM_Wave_1_WORKING_RAW_2022-02-02.csv")
names(dat)

#############################################

#### Data preparation ####

# just taking the variables I want
# dat <- dat %>% dplyr::select(participant_id, srs_1:srs_11, bfas_1:bfas_100, age, gender, race, year, socialclass, usborn)
# names(dat)

# saving this to project directory, change outside of file
# to: ~\Documents\traits-reasoning\Data
# write.csv(dat, "traits_reasoning_data_2025-09-25.csv", row.names = FALSE)

# read in data for processing
# wd is the broader project directory ~\Documents\traits-reasoning

dat <- read.csv("Data/traits_reasoning_data_2025-09-25.csv")
names(dat)
head(dat)

#############################################

#### Label Variable Names ####

# use data dictionary to apply variable labels and value labels

# read in dictionary

dict <- read.csv("Data/traits_reasoning_dictionary_2025-09-15.csv")
names(dict)

# variable labels
# make list from two columns in dictionary, apply the dictionary, and save to the data file

labelled::var_label(dat) <- dict %>%
  dplyr::select(variable, label) %>%
  codebook::dict_to_list()

#############################################

#### Label Variable Values ####

# value labels
# need to be done separately for each set of response options

#add value labels 1-2 true/false

{
  tf <- dict %>% 
    dplyr::filter (value_labels == "1 True 2 False") %>%
    dplyr::pull(variable)
  add_tf <- function(x) {
    val_labels(x) <- c("True" = 1,
                       "False" = 2)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(tf, 
                     add_tf)
  }

#check
dat$srs_1

#add value labels likert 1-5 Agree w/ Neither

{
  likert5 <- dict %>% 
    dplyr::filter (value_labels == "1 Strongly Disagree 2 Disagree 3 Neither Agree nor Disagree 4 Agree 5 Strongly Agree") %>%
    dplyr::pull(variable)
  add_likert5 <- function(x) {
    val_labels(x) <- c("Strongly Disagree" = 1,
                       "Disagree" = 2,
                       "Neither Agree nor Disagree" = 3,
                       "Agree" = 4,
                       "Strongly Agree" = 5)
    x
  }
  dat <- dat %>%
    dplyr::mutate_at(likert5, 
                     add_likert5)
}

#check
dat$bfas_1

#############################################

#### Save these Files for Future Use ####

write.csv(dat, "Data/traits_reasoning_data_working_2025-09-28.csv", row.names = F)  

saveRDS(dat, "Data/traits_reasoning_data_working_2025-09-28.RData")

haven::write_sav(dat, "Data/traits_reasoning_data_working_2025-09-28.sav")

#############################################

#### Project Notes ####

# these still have demographics, so add files to .gitignore so that they don't get posted

# also, for some reason these are not retaining the value labels when reloaded! 

