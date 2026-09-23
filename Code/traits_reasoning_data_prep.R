#############################################
#### Traits and Reasoning Project ####
# Script for cleanining: variable labels
# Created on Sep 28, 2025, by Moin Syed
# Modified on Sep 22, 2026, by Moin Syed
# Checked on DATE, by NAME
#############################################

#### Workspace setup ####

library(here) # for directory management
library(dplyr) # for wrangling
library(codebook) # for labeling variables
library(labelled) # for labeling variables
library(haven) # for reading/writing various date file types

sessionInfo()

# R version 4.6.0
# here_1.0.2 
# dplyr_1.2.1 
# codebook_0.10.1 
# labelled_2.16.0 
# haven_2.5.5     

#############################################

#### Data import ####

# loading in data from Box directory, changed to location outside of file
# from: ~\Box\P-STEM\Data\Raw Data - DO NOT MODIFY\Wave 1

dat <- read.csv("PSTEM_Wave_1_WORKING_RAW_2022-02-02.csv")
names(dat)
head(dat)
dim(dat)

#############################################

#### Data preparation ####

# just taking the variables I want
dat <- dat %>% dplyr::select(participant_id, srs_1:srs_11, bfas_1:bfas_100, age, gender, race, year, socialclass, usborn)
names(dat)

# saving this to project directory, change outside of file
# to: ~\Documents\traits-reasoning
here()

write.csv(dat, here("Data", "traits_reasoning_data_2026-09-22.csv"), row.names = FALSE)

# read in data for processing
# working directory is the root project directory ~\Documents\traits-reasoning

dat <- read.csv(here("Data", "traits_reasoning_data_2026-09-22.csv"))
names(dat)
head(dat)
View(dat)

# creating a new data file without demographics, which can be posted
# save this version to Data subdirectory
# this file is only for posting, analyses should use traits_reasoning_data_2026-09-22.csv

dat_nodemos <- dat %>% select(participant_id:bfas_100)
names(dat_nodemos)

write.csv(dat_nodemos, here("Data", "traits_reasoning_data_nodemos_2026-09-22.csv"), row.names = FALSE)

#############################################

#### Label Variable Names ####

# use data dictionary to apply variable labels and value labels

# read in dictionary

dict <- read.csv(here("Data", "traits_reasoning_dictionary_2025-09-15.csv"))
names(dict)

# variable labels
# make list from two columns in dictionary, apply the dictionary, and save to the data file

labelled::var_label(dat) <- dict %>%
  dplyr::select(variable, label) %>%
  codebook::dict_to_list()

View(dat)

#############################################

#### Label Variable Values ####

# value labels
# need to be done separately for each set of response options

#add value labels 1-2 true/false

# precheck

dat$srs_1

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

# check

dat$srs_1

# add value labels likert 1-5 Agree w/ Neither

# precheck

dat$bfas_1

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

write.csv(dat, here("Data", "traits_reasoning_data_working_2026-09-22.csv"), row.names = F)  

haven::write_sav(dat, here("Data", "traits_reasoning_data_working_2026-09-22.sav"))

dat_spss <- read_sav(here("Data", "traits_reasoning_data_working_2026-09-22.sav"), encoding = "latin1")
names(dat_spss)
head(dat_spss)
View(dat_spss)

dat_spss$srs_1
dat_spss$bfas_1

#############################################

#### Data Wrangling ####

# subsetting - filter and select

# recoding, renaming, reversing

dat %>% select(starts_with("srs")) %>% names()

# rename with leading zeros



# recode so that 1 always means correct response and 0 incorrect

dat <- dat %>% mutate(srs_1_scored = recode(srs_1, `1` = 1, `2` = 0),
                      srs_2_scored = recode(srs_2, `1` = 0, `2` = 1),
                      srs_3_scored = recode(srs_3, `1` = 1, `2` = 0),
                      srs_4_scored = recode(srs_4, `1` = 0, `2` = 1),
                      srs_5_scored = recode(srs_5, `1` = 0, `2` = 1),
                      srs_6_scored = recode(srs_6, `1` = 0, `2` = 1),
                      srs_7_scored = recode(srs_7, `1` = 1, `2` = 0),
                      srs_8_scored = recode(srs_8, `1` = 0, `2` = 1),
                      srs_9_scored = recode(srs_9, `1` = 0, `2` = 1),
                      srs_10_scored = recode(srs_10, `1` = 0, `2` = 1),
                      srs_11_scored = recode(srs_11, `1` = 0, `2` = 1))

# check 

cor.test(dat$srs_1, dat$srs_1_scored)

# creating variables

dat <- dat %>% dplyr::mutate(srs = rowMeans(pick(srs_1_scored:srs_11_scored), na.rm = TRUE))

# give that new variable a label

labelled::var_label(dat$srs) <- "Scientific Reasoning Scale - Mean"

# check that all looks ok

psych::describe(dat$srs)
dat$srs

# pivot longer - tidy data

# pivot wider

# joining

