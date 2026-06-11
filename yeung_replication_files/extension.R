library(tidyverse)
library(lme4)
library(discSurv)
library(stargazer)

# Load data into the environment
experimental_data <- read.csv("experimental_data.csv")

# Manipulate recall time variables to be able to use them  
exp_dta_pooling <- experimental_data %>%
  mutate(respondent_id = row_number()-1) %>%
  mutate(recallt1.c = coalesce(recall_t1.1, 
                               recall_t1.2, recall_t1.3, recall_t1)) %>%
  mutate(recallt2.c = coalesce(recall_t2, recall_t2.1, 
                               recall_t2.2, recall_t2.3)) %>%
  mutate(recallt3.c = coalesce(recall_t3, recall_t3.1, 
                               recall_t3.2, recall_t3.3)) %>%
  mutate(recallt4.c = coalesce(recall_t4, recall_t4.1, 
                               recall_t4.2, recall_t4.3)) %>%
  mutate(recallt5.c = coalesce(recall_t5, recall_t5.1, 
                               recall_t5.2, recall_t5.3)) %>%
  mutate(recallt6.c = coalesce(recall_t6, recall_t6.1, 
                               recall_t6.2, recall_t6.3)) %>%
  mutate(ever_recalled = case_when(
    recallt1.c == 1 ~ 1,
    recallt2.c == 1 ~ 1,
    recallt3.c == 1 ~ 1,
    recallt4.c == 1 ~ 1,
    recallt5.c == 1 ~ 1,
    recallt6.c == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    fst_recall_pd = case_when(
      recallt1.c == 1 ~ 1,
      recallt2.c == 1 ~ 2,
      recallt3.c == 1 ~ 3,
      recallt4.c == 1 ~ 4,
      recallt5.c == 1 ~ 5,
      recallt6.c == 1 ~ 6,
      TRUE ~ 7  
    )) %>%
  filter(!(is.na(pid_2d) & is.na(pid_2r))) %>%
  mutate(par_strength = coalesce(pid_2d, pid_2r)) %>%
  select(respondent_id, exp_1, pid_2r, pid_2d, par_strength, 
         fst_recall_pd, ever_recalled, 
         starts_with("recallt"))

# Make Person-Period table instead
exp_dta_person_pd <- discSurv::dataLong(
  dataShort = exp_dta_pooling,
  timeColumn = "fst_recall_pd",
  eventColumn = "ever_recalled",
  timeAsFactor = FALSE) %>%
  as_tibble() %>%
  mutate(
    enter = timeInt - 1, 
    period = timeInt) %>%
  rename(event = y) %>%
  left_join(
    exp_dta_pooling %>% select(respondent_id, exp_1, pid_2r, 
                           pid_2d, par_strength, 
                           fst_recall_pd, ever_recalled, 
                           starts_with("recallt")),
    by = "respondent_id"
  )
  
model <- glmer(
  formula = event ~ period + par_strength.x + exp_1.x + (1 | respondent_id),
  family = binomial, #defaults to logit
  data = exp_dta_person_pd,
  control = glmerControl(optimizer = "bobyqa", 
                         optCtrl = list(maxfun = 2e5))
)

summary(model)
stargazer(model, 
          type = "latex",
          out = "model_latex_output.tex")
