#load packages
library(tidyverse)
library(here)
library(cobalt)

# load data

experimental_data <- read.csv(
  here("yeung_replication_files/experimental_data.csv"))

# create an easier-to-read treatment assignment variable
# originally = Increasing Severity condition = 1; Decreasing
#   Severity condition = 2; Sporadic Severity condition = 3; No Transgressions
#   condition = 4
# change to: No Transgressions (= no treatment) = 0, Other (= treatment) = 1

exp_data_better <- experimental_data %>%
  mutate(treatment = case_when(
    exp_1 == 4 ~ 0,
    exp_1 != 4 ~ 1
  ))

# generate balance table

balance_table <- bal.tab(treatment ~ yob + state + , 
                         data = exp_data_better,
                         stats = c("m", "v", "ks.statistics"))

exp_data_better$state <- as.factor(exp_data_better$state)
exp_data_better$sex <- as.factor(exp_data_better$sex)
exp_data_better$race <- as.factor(exp_data_better$race)
exp_data_better$ideo <- as.factor(exp_data_better$ideo)
exp_data_better$educ <- as.factor(exp_data_better$educ)

balance_table <- bal.tab(treatment ~ yob + ideo + educ + state + sex + race,
                         data = exp_data_better,
                         stats = c("m", "v", "ks.statistics"),
                         un = TRUE)


