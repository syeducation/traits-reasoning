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

dat <- dat %>%  rename(srs_01 = srs_1,
                       srs_02 = srs_2,
                       srs_03 = srs_3,
                       srs_04 = srs_4,
                       srs_05 = srs_5,
                       srs_06 = srs_6,
                       srs_07 = srs_7,
                       srs_08 = srs_8,
                       srs_09 = srs_9)

dat %>% select(starts_with("srs")) %>% names()

# recode so that 1 always means correct response and 0 incorrect

dat <- dat %>% mutate(srs_01_scored = recode(srs_01, `1` = 1, `2` = 0),
                      srs_02_scored = recode(srs_02, `1` = 0, `2` = 1),
                      srs_03_scored = recode(srs_03, `1` = 1, `2` = 0),
                      srs_04_scored = recode(srs_04, `1` = 0, `2` = 1),
                      srs_05_scored = recode(srs_05, `1` = 0, `2` = 1),
                      srs_06_scored = recode(srs_06, `1` = 0, `2` = 1),
                      srs_07_scored = recode(srs_07, `1` = 1, `2` = 0),
                      srs_08_scored = recode(srs_08, `1` = 0, `2` = 1),
                      srs_09_scored = recode(srs_09, `1` = 0, `2` = 1),
                      srs_10_scored = recode(srs_10, `1` = 0, `2` = 1),
                      srs_11_scored = recode(srs_11, `1` = 0, `2` = 1))

# check 

cor.test(dat$srs_01, dat$srs_01_scored)

# creating variables

dat <- dat %>% dplyr::mutate(srs = rowMeans(pick(srs_01_scored:srs_11_scored), na.rm = TRUE))

# give that new variable a label

labelled::var_label(dat$srs) <- "Scientific Reasoning Scale - Mean"

# check that all looks ok

psych::describe(dat$srs)
dat$srs

# personality traits - C-I and O-I

dat$bfas_1

# Industriousness: 3, 13r, 23r, 33r, 43, 53r, 63, 73, 83r, 93r

dat %>% select(ends_with("3")) %>% var_label()

dat <- dat %>% mutate(bfas_13r = (6-bfas_13),
                      bfas_23r = (6-bfas_23),
                      bfas_33r = (6-bfas_33),
                      bfas_53r = (6-bfas_53),
                      bfas_83r = (6-bfas_83),
                      bfas_93r = (6-bfas_93))

cor.test(dat$bfas_13, dat$bfas_13r)

dat <- dat %>% 
  dplyr::mutate(bfas_indust = 
                  rowMeans(pick(bfas_3,
                                bfas_13r,
                                bfas_23r,
                                bfas_33r,
                                bfas_43,
                                bfas_53r,
                                bfas_63,
                                bfas_73,
                                bfas_83r,
                                bfas_93r), na.rm = TRUE))

# give that new variable a label

labelled::var_label(dat$bfas_indust) <- "BFAS Industriousness - Mean"

# check that all looks ok

psych::describe(dat$bfas_indust)
dat$bfas_indust

# Intellect: 5, 15r, 25, 35, 45r, 55r, 65, 75, 85r, 95

dat %>% select(starts_with("bfas") & ends_with("5")) %>% var_label()

dat <- dat %>% mutate(bfas_15r = (6-bfas_15),
                      bfas_45r = (6-bfas_45),
                      bfas_55r = (6-bfas_55),
                      bfas_85r = (6-bfas_85))

cor.test(dat$bfas_15, dat$bfas_15r)

dat <- dat %>% 
  dplyr::mutate(bfas_intellect = 
                  rowMeans(pick(bfas_5,
                                bfas_15r,
                                bfas_25,
                                bfas_35,
                                bfas_45,
                                bfas_55r,
                                bfas_65,
                                bfas_75,
                                bfas_85r,
                                bfas_95), na.rm = TRUE))

# give that new variable a label

labelled::var_label(dat$bfas_intellect) <- "BFAS Intellect - Mean"

# check that all looks ok

psych::describe(dat$bfas_intellect)
dat$bfas_intellect

# pivot longer - tidy data

# pivot wider

# joining

