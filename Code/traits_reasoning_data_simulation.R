#############################################
#### Traits and Reasoning Project ####
# Script for Simulating Data - Demo For Class
# Created on Oct 31, 2025, by Moin Syed
# Checked on DATE, by NAME
#############################################

#### Workspace setup ####

library(groundhog)
groundhog.library(dplyr, "2025-04-01")
groundhog.library(faux, "2025-04-01")
groundhog.library(summarytools, "2025-04-01")
groundhog.library(missMethods, "2025-04-01")

sessionInfo()

# R version 4.4.2
# groundhog_3.2.3     
# dplyr_1.1.4 
# faux_1.2.2   
# summarytools_1.1.3
# missMethods_0.4.0

# setting seed for reproducibility - use any number!

set.seed(1978)

#############################################

#### Using rnorm() ####

# rnorm raw

rnorm(n = 10, mean = 3.5, sd = 0.75)


# rnorm, but make it a data frame

outcome <- rnorm(n = 10, mean = 3.5, sd = 0.75)

dat_rnorm <- data.frame(outcome)
head(dat_rnorm)

mean(dat_rnorm$outcome)
sd(dat_rnorm$outcome)


# rnorm, setting up a function

data_generate <- function(n, m, sd) {
  
  set.seed(1978)
  
  g <- rnorm(n = n, mean = m, sd = sd)
  
  data <- data.frame(
    outcome = g
  )
  return(data)
}

# set paramters and create data

dat_rnorm_func <- data_generate(n = 10, m = 3.5, sd = 0.75)
dat_rnorm_func
mean(dat_rnorm_func$outcome)
sd(dat_rnorm_func$outcome)


# rnorm, but now two groups

data_generate <- function(n1, n2, m1, m2, sd1, sd2) {
  
  set.seed(1978)
  
  g1 <- rnorm(n = n1, mean = m1, sd = sd1)
  g2 <- rnorm(n = n2, mean = m2, sd = sd2)
  
  data <- data.frame(
    group = rep(c("1","2"), times = c(n1, n2)), 
    outcome = c(g1, g2)
  )
  return(data)
}

# set paramters and create data

dat_rnorm_func_two <- data_generate(n1 = 50, n2 = 50, 
                                    m1 = 0.39, m2 = -0.42, 
                                    sd1 = 0.93, sd2 = 0.80)

# do a few checks to make sure all looks right

head(dat_rnorm_func_two)

dat_rnorm_func_two %>% dplyr::group_by(group) %>% 
  dplyr::summarise(mean = mean(outcome,  na.rm = TRUE), sd = sd(outcome,  na.rm = TRUE))


#############################################

#### Using rnorm_multi() from the faux package ####

# set the parameters

n <- 100
m1 <- 0.39
sd1 <- 0.93
m2 <- -0.42
sd2 <- 0.80

# create the data file

dat_faux <- faux::rnorm_multi(n = n,
                              mu = c(m1, m2),
                              sd = c(sd1, sd2),
                              varnames = c("g1", "g2"))

# do a few checks to make sure all looks right

head(dat_faux)
mean(dat_faux$g1)
mean(dat_faux$g2)
sd(dat_faux$g1)
sd(dat_faux$g2)


#############################################

#### Using sample() to simulate items - most useful for preregistration ####

# get info on data specs from dictionary

dict <- read.csv("./Data/traits_reasoning_dictionary_2025-09-15.csv")
dict

# create empty data frame and populate with id variable based on your N (N = 100 here)

dat_sim <- data.frame(id = c(1:100))
head(dat_sim)

# now simulate individual items based on properties of interest

# binary item, equal probability

dat_sim$srs_1_sim <- sample(1:2, size = 100, replace = TRUE, prob = c(.50, .50))
table(dat_sim$srs_1_sim)

# binary item, skewed probability

dat_sim$srs_2_sim <- sample(1:2, size = 100, replace = TRUE, prob = c(.10, .90))
table(dat_sim$srs_2_sim)

# five-point item, normal distribution

dat_sim$bfas_1_sim <- sample(1:5, size = 100, replace = TRUE, prob = c(.05, .25, .40, .25, .05))
table(dat_sim$bfas_1_sim)

# five-point item, skewed distribution

dat_sim$bfas_2_sim <- sample(1:5, size = 100, replace = TRUE, prob = c(.05, .10, .15, .35, .35))
table(dat_sim$bfas_2_sim)

# do a few checks to make sure all looks right

dim(dat_sim)

head(dat_sim)

dat_sim %>% dplyr::select(srs_1_sim:bfas_2_sim) %>% summarytools::freq()

# include missing data in the results

dat_sim <- missMethods::delete_MCAR(dat_sim, .04, "srs_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim, .20, "srs_2_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim, .01, "bfas_1_sim")
dat_sim <- missMethods::delete_MCAR(dat_sim, .08, "bfas_2_sim")

# check

dat_sim %>% dplyr::select(srs_1_sim:bfas_2_sim) %>% summarytools::freq()
dat_sim

# save the new file out and use for preregistration! 

write.csv(dat_sim, "./Data/traits_reasoning_data_simulated_2025-10-31.csv", row.names = FALSE)



