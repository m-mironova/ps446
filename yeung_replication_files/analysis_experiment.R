######################################
### Dynamic Democratic Backsliding ###
### [Main Replication Code]        ###
### Author: Eddy S. F. Yeung       ###
### Date: July 8, 2025             ###
######################################

### Set-up ----
## Clean the working environment and set up the working directory
rm(list = ls())
#setwd("~/Desktop/dynamic_backsliding/replication") # set your working directory here, which should also contain the survey data ("experimental_data.csv")

## Import the dataset
df <- read.csv("experimental_data.csv")

## Load the required packages
library(tidyverse)
library(estimatr)
library(scales)
library(survival)
library(survminer)
library(texreg)
library(car)
library(marginaleffects)
library(gridExtra)
library(cowplot)
library(ggdist)

## Generate a log file
library(logr)
options("logr.autolog" = TRUE)

# Open the log
lf <- log_open(file.path("logmain.log"))

# Send messages to the log
log_code()

### Sample demographics: Table S2 and Figure S1 ----
## Sex
df$sex <- factor(df$sex, levels = 1:3, labels = c("Male", "Female", "Nonbinary"))
table(df$sex) %>% prop.table() * 100
df$male <- ifelse(df$sex == "Male", 1, 0) 

## Age
df$age <- df$yob + 2023 - 2006
df <- df %>% mutate(age6 = case_when(
  age >= 18 & age <= 29 ~ 1,
  age >= 30 & age <= 39 ~ 2,
  age >= 40 & age <= 49 ~ 3,
  age >= 50 & age <= 59 ~ 4,
  age >= 60 & age <= 69 ~ 5,
  age >= 70             ~ 6
))
df$age6 <- factor(df$age6, levels = 1:6, 
                  labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))
table(df$age6) %>% prop.table() * 100

## Race
# Separate the comma-separated values into multiple rows
df_long <- df %>%
  mutate(race = strsplit(as.character(race), ",")) %>%
  unnest(race)

# Create a column indicating the presence of each race category
df_long <- df_long %>%
  mutate(present = 1)

# Use pivot_wider to reshape the data
df <- df_long %>%
  pivot_wider(names_from = race, names_prefix = "race_", values_from = present, values_fill = 0)

# Rename the race variables
df <- df %>% rename(white = race_1, black = race_2, hispanic = race_3)
table(df$white) %>% prop.table() * 100
table(df$black) %>% prop.table() * 100
table(df$hispanic) %>% prop.table() * 100

## State of residence
df$state <- factor(df$state, levels = 1:52,
                   labels = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE",
                              "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA",
                              "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
                              "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                              "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
                              "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA",
                              "WA", "WV", "WI", "WY"))
respondents_by_state <- df %>% 
  filter(state != "PR") %>% 
  group_by(state, .drop = FALSE) %>% 
  summarize(n = n()) %>%
  mutate(prop = n / sum(n) * 100) %>% 
  filter(state != "PR")
state_pop <- read.csv("state_population.csv")
respondents_by_state <- data.frame(state = respondents_by_state$state,
                                   prop = respondents_by_state$prop,
                                   percentage = state_pop$Percentage)
p <- ggplot(respondents_by_state, aes(x = state, y = prop)) +
  geom_bar(stat = "identity", colour = "black", fill = "grey80") +
  geom_point(aes(y = percentage), size = 1.5, color = "black", shape = 4) +
  scale_y_continuous(
    name = "Percentage of respondents in the sample",
    sec.axis = sec_axis(~., name = "Percentage of adults according to census")
  ) + 
  labs(x = "") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Times", size = 10, color = "black"),
        axis.text.y = element_text(family = "Times", size = 10, color = "black"),
        axis.text.x = element_text(family = "Times", angle = 90, size = 8, vjust = 0.3, color = "black"),
        legend.title = element_text(family = "Times", size = 10, color = "black"),
        legend.text = element_text(family = "Times", size = 12, color = "black"))
mean((respondents_by_state$percentage - respondents_by_state$prop)^2) # mean squared error
p <- p + 
  annotate("label", x = 44, y = 10.5, family = "Times", label = "Mean squared error = 0.61")
ggsave("Figure S1.pdf", width = 8, height = 4)

## Education (1 = college degree; 0 = no college degree)
df$college <- ifelse(df$educ == 5 | df$educ == 6, 1, 0)
table(df$college) %>% prop.table() * 100

## Partisanship
df <- df %>% mutate(pid3 = case_when(
  pid_1 == 1 | pid_2i == 1 ~ "Republican",
  pid_1 == 2 | pid_2i == 2 ~ "Democrat",
  pid_2i == 3              ~ "Independent"
))
df$pid3 <- factor(df$pid3, levels = c("Independent", "Democrat", "Republican"))
table(df$pid3) %>% prop.table() * 100

## Self-reported ideology (1 = extremely liberal; 7 = extremely conservative)
table(df$ideo) %>% prop.table() * 100

### Recoding variables ----
## Experimental condition
df <- df %>% mutate(condition = case_when(
  exp_1 == 1 ~ "Increasing Severity",
  exp_1 == 2 ~ "Decreasing Severity",
  exp_1 == 3 ~ "Sporadic Severity",
  exp_1 == 4 ~ "No Transgressions"
))
df$condition <- factor(df$condition, 
                       levels = c("No Transgressions", "Increasing Severity",
                                  "Decreasing Severity", "Sporadic Severity"))

## Recall decision in the first period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t1 = case_when(
  exp_1 == 1 ~ 2 - recall_t1,
  exp_1 == 2 ~ 2 - recall_t1.1,
  exp_1 == 3 ~ 2 - recall_t1.2,
  exp_1 == 4 ~ 2 - recall_t1.3
))
df <- df[ , !(names(df) %in% c("recall_t1.1", "recall_t1.2", "recall_t1.3"))]

## Recall decision in the second period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t2 = case_when(
  exp_1 == 1 ~ 2 - recall_t2,
  exp_1 == 2 ~ 2 - recall_t2.1,
  exp_1 == 3 ~ 2 - recall_t2.2,
  exp_1 == 4 ~ 2 - recall_t2.3
))
df <- df[ , !(names(df) %in% c("recall_t2.1", "recall_t2.2", "recall_t2.3"))]

## Recall decision in the third period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t3 = case_when(
  exp_1 == 1 ~ 2 - recall_t3,
  exp_1 == 2 ~ 2 - recall_t3.1,
  exp_1 == 3 ~ 2 - recall_t3.2,
  exp_1 == 4 ~ 2 - recall_t3.3
))
df <- df[ , !(names(df) %in% c("recall_t3.1", "recall_t3.2", "recall_t3.3"))]

## Recall decision in the fourth period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t4 = case_when(
  exp_1 == 1 ~ 2 - recall_t4,
  exp_1 == 2 ~ 2 - recall_t4.1,
  exp_1 == 3 ~ 2 - recall_t4.2,
  exp_1 == 4 ~ 2 - recall_t4.3
))
df <- df[ , !(names(df) %in% c("recall_t4.1", "recall_t4.2", "recall_t4.3"))]

## Recall decision in the fifth period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t5 = case_when(
  exp_1 == 1 ~ 2 - recall_t5,
  exp_1 == 2 ~ 2 - recall_t5.1,
  exp_1 == 3 ~ 2 - recall_t5.2,
  exp_1 == 4 ~ 2 - recall_t5.3
))
df <- df[ , !(names(df) %in% c("recall_t5.1", "recall_t5.2", "recall_t5.3"))]

## Recall decision in the sixth period (1 = yes; 0 = no)
df <- df %>% mutate(recall_t6 = case_when(
  exp_1 == 1 ~ 2 - recall_t6,
  exp_1 == 2 ~ 2 - recall_t6.1,
  exp_1 == 3 ~ 2 - recall_t6.2,
  exp_1 == 4 ~ 2 - recall_t6.3
))
df <- df[ , !(names(df) %in% c("recall_t6.1", "recall_t6.2", "recall_t6.3"))]

## Number of periods until the first recall
df <- df %>% mutate(first_recall = case_when(
  recall_t1 == 1 ~ 1,
  recall_t1 == 0 & recall_t2 == 1 ~ 2,
  recall_t1 == 0 & recall_t2 == 0 & recall_t3 == 1 ~ 3,
  recall_t1 == 0 & recall_t2 == 0 & recall_t3 == 0 & recall_t4 == 1 ~ 4,
  recall_t1 == 0 & recall_t2 == 0 & recall_t3 == 0 & recall_t4 == 0 &
    recall_t5 == 1 ~ 5,
  recall_t1 == 0 & recall_t2 == 0 & recall_t3 == 0 & recall_t4 == 0 &
    recall_t5 == 0 & recall_t6 == 1 ~ 6,
  recall_t1 == 0 & recall_t2 == 0 & recall_t3 == 0 & recall_t4 == 0 &
    recall_t5 == 0 & recall_t6 == 0 ~ 7
))

## Approval rating at the baseline
df <- df %>% mutate(approval_t0 = case_when(
  approval_t0_9 >= 0 & approval_t0_9 <= 100 ~ approval_t0_9,
  approval_t0_9.1 >= 0 & approval_t0_9.1 <= 100 ~ approval_t0_9.1
))

## Approval rating in the first period
df <- df %>% mutate(approval_t1 = case_when(
  exp_1 == 1 ~ approval_t1_9,
  exp_1 == 2 ~ approval_t1_9.1,
  exp_1 == 3 ~ approval_t1_9.2,
  exp_1 == 4 ~ approval_t1_9.3
))

## Approval rating in the second period
df <- df %>% mutate(approval_t2 = case_when(
  exp_1 == 1 ~ approval_t2_9,
  exp_1 == 2 ~ approval_t2_9.1,
  exp_1 == 3 ~ approval_t2_9.2,
  exp_1 == 4 ~ approval_t2_9.3
))

## Approval rating in the third period
df <- df %>% mutate(approval_t3 = case_when(
  exp_1 == 1 ~ approval_t3_9,
  exp_1 == 2 ~ approval_t3_9.1,
  exp_1 == 3 ~ approval_t3_9.2,
  exp_1 == 4 ~ approval_t3_9.3
))

## Approval rating in the fourth period
df <- df %>% mutate(approval_t4 = case_when(
  exp_1 == 1 ~ approval_t4_9,
  exp_1 == 2 ~ approval_t4_9.1,
  exp_1 == 3 ~ approval_t4_9.2,
  exp_1 == 4 ~ approval_t4_9.3
))

## Approval rating in the fifth period
df <- df %>% mutate(approval_t5 = case_when(
  exp_1 == 1 ~ approval_t5_9,
  exp_1 == 2 ~ approval_t5_9.1,
  exp_1 == 3 ~ approval_t5_9.2,
  exp_1 == 4 ~ approval_t5_9.3
))

## Approval rating in the sixth period
df <- df %>% mutate(approval_t6 = case_when(
  exp_1 == 1 ~ approval_t6_9,
  exp_1 == 2 ~ approval_t6_9.1,
  exp_1 == 3 ~ approval_t6_9.2,
  exp_1 == 4 ~ approval_t6_9.3
))

## Assessment of whether the governor will be recalled at some point (1 = yes; 0 = no)
df <- df %>% mutate(overall_assess = case_when(
  exp_1 == 1 ~ 2 - overall,
  exp_1 == 2 ~ 2 - overall.1,
  exp_1 == 3 ~ 2 - overall.2,
  exp_1 == 4 ~ 2 - overall.3
))

## Self-reported political ideology (0 = extremely liberal; 1 = extremely conservative)
df$ideo <- scales::rescale(df$ideo, to = c(0, 1), from = c(1, 7))

## Affective polarization (0 = least affectively polarized; 1 = most affectively polarized)
df <- df %>% mutate(affect = case_when(
  pid3 == "Republican" ~ affect_1 - affect_2,
  pid3 == "Democrat"   ~ affect_2 - affect_1
))
df$affect <- scales::rescale(df$affect, to = c(0, 1), from = c(-100, 100))

## Antiestablishment (0 = least antiestablishment; 1 = most antiestablishment)
df$antiest <- scales::rescale(df$antiest_1 + df$antiest_2 + df$antiest_3, 
                              to = c(0, 1), from = c(3, 15))

### Cox modeling: Table S3, Table S5, Figure 1B ----
## Estimate the main Cox models
# Recode variables for the survival analysis
df$recalled <- ifelse(df$first_recall == 7, 0, 1)
df$surv_time <- ifelse(df$first_recall == 7, 6, df$first_recall)
df$increasing <- ifelse(df$condition == "Increasing Severity", 1, 0)
df$decreasing <- ifelse(df$condition == "Decreasing Severity", 1, 0)
df$sporadic <- ifelse(df$condition == "Sporadic Severity", 1, 0)
df$control <- ifelse(df$condition == "No Transgressions", 1, 0)

# Estimate the preregistered Cox model (Table S3)
cox <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
               age6 + male + white + college + pid3 + ideo, 
             data = df)
summary(cox)
summary_cox <- cox %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(model = "With covariates") %>% 
  slice(1:3)

# Test the proportional hazards assumption (Table S5)
cox.zph(cox)

# Estimate the Cox model without covariates (Table S3)
cox_alt <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic, 
                 data = df)
summary(cox_alt)
summary_cox_alt <- cox_alt %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(model = "Without covariates")

## Plot the Cox coefficients (Figure 1B)
summary_main_cox <- bind_rows(summary_cox, summary_cox_alt)
summary_main_cox$model <- 
  factor(summary_main_cox$model,
         levels = c("Without covariates", "With covariates"))
summary_main_cox$term <- 
  factor(summary_main_cox$term,
         levels = c("sporadic", "decreasing", "increasing"),
         labels = c("Sporadic\nSeverity", "Decreasing\nSeverity", "Increasing\nSeverity"))
ggcox <- ggplot(summary_main_cox, aes(y = term, color = model, shape = model)) +
  geom_point(aes(x = estimate), position = position_dodge(-.3), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(-.3), height = 0) +
  geom_vline(xintercept = 0, lty = 2, color = "grey") +
  scale_color_manual(values = c("grey0", "grey50")) +
  theme_classic() +
  xlab("Cox coefficient with 95% CI") +
  ylab("") +
  coord_cartesian(xlim = c(-0.2, 0.8)) +
  theme(text = element_text(color = "black", family = "Times", size = 11),
        axis.text = element_text(family = "Times", size = 11),
        legend.text = element_text(family = "Times", size = 11),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.key.size = unit(1.5, "line"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = -0.15, l = 0.15, b = 0.10, r = 0.15, unit = "cm"),
        legend.title = element_blank())
ggcox <- ggcox +
  ggtitle("Treatment Effect Estimates") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

## Test the statistical difference between coefficient estimates
# H1: Increasing Severity vs. Sporadic Severity
cox_H1 <- coxph(Surv(surv_time, recalled) ~ sporadic + decreasing + control +
                  age6 + male + white + college + pid3 + ideo, 
                data = df)
exp(cox_H1$coefficients[1]) # difference in hazard ratios between the two conditions
tidy(cox_H1)[1, 5] # p-value of the estimated difference
summary(cox_H1)

# H2: Decreasing Severity vs. Sporadic Severity
cox_H2 <- coxph(Surv(surv_time, recalled) ~ sporadic + increasing + control +
                  age6 + male + white + college + pid3 + ideo, 
                data = df)
exp(cox_H2$coefficients[1]) # difference in hazard ratios between the two conditions
tidy(cox_H2)[1, 5] # p-value of the estimated difference
summary(cox_H2)

cox_H2 <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + control +
                  age6 + male + white + college + pid3 + ideo, 
                data = df)
exp(cox_H2$coefficients[1]) # difference in hazard ratios between the two conditions
tidy(cox_H2)[1, 5] # p-value of the estimated difference
summary(cox_H2)

# H3: Increasing Severity vs. Decreasing Severity
cox_H3 <- coxph(Surv(surv_time, recalled) ~ decreasing + sporadic + control +
                  age6 + male + white + college + pid3 + ideo, 
                data = df)
exp(cox_H3$coefficients[1]) # difference in hazard ratios between the two conditions
tidy(cox_H3)[1, 5] # p-value of the estimated difference
summary(cox_H3)

## Estimate the preregistered model subset by partisanship
# Democrats
cox_dem <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
                   age6 + male + white + college + ideo, 
                 data = df, subset = pid3 == "Democrat")
summary(cox_dem)

# Republicans
cox_gop <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
                   age6 + male + white + college + ideo, 
                 data = df, subset = pid3 == "Republican")
summary(cox_gop)

# Independents
cox_ind <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
                   age6 + male + white + college + ideo, 
                 data = df, subset = pid3 == "Independent")
summary(cox_ind)

### Plot survival curves all at once: Figure 1A ----
fit <- survfit(Surv(surv_time, recalled) ~ condition, data = df)
ggsurv <- 
  ggsurvplot(fit,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("No Transgressions", "Increasing Severity", "Decreasing Severity", "Sporadic Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 11),
             font.ylab = list(size = 11),
             font.legend = list(size = 11),
             palette = c("gray", "darkred", "darkblue", "darkgreen"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv <- 
  ggsurv$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 11))
ggsurv <- ggsurv +
  ggtitle("Survival Curves by Experimental Condition") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
plot_grid(ggsurv, ggcox, labels = "AUTO", nrow = 1, 
          label_fontfamily = "Times", label_size = 12)
ggsave("Figure 1.pdf", width = 9, height = 4.5)
# ggsave("Figure 1.png", dpi = 600, width = 9, height = 4.5)

### Kaplan-Meier curves: Figure 2 ----
## Visualize the survival curves and conduct log-rank tests to compare the curves
# Increasing Severity vs. Sporadic Severity
fit_H1 <- survfit(Surv(surv_time, recalled) ~ increasing, 
                  data = subset(df, condition == "Increasing Severity" | condition == "Sporadic Severity"))
fit_H1_p <- survdiff(Surv(surv_time, recalled) ~ increasing, 
                     data = subset(df, condition == "Increasing Severity" | condition == "Sporadic Severity"))
fit_H1_p$pvalue
ggsurv_H1 <- 
  ggsurvplot(fit_H1,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("Sporadic Severity", "Increasing Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("darkgreen", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H1 <- 
  ggsurv_H1$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) == 0.2188", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H1 <- ggsurv_H1 +
  ggtitle("Increasing vs. Sporadic Severity") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

# Decreasing Severity vs. Sporadic Severity
fit_H2 <- survfit(Surv(surv_time, recalled) ~ decreasing, 
                  data = subset(df, condition == "Decreasing Severity" | condition == "Sporadic Severity"))
fit_H2_p <- survdiff(Surv(surv_time, recalled) ~ decreasing, 
                     data = subset(df, condition == "Decreasing Severity" | condition == "Sporadic Severity"))
fit_H2_p$pvalue
ggsurv_H2 <- 
  ggsurvplot(fit_H2,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("Sporadic Severity", "Decreasing Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("darkgreen", "darkblue"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H2 <- 
  ggsurv_H2$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) < '0.0001'", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H2 <- ggsurv_H2 +
  ggtitle("Decreasing vs. Sporadic Severity") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

# Increasing Severity vs. Decreasing Severity
fit_H3 <- survfit(Surv(surv_time, recalled) ~ decreasing, 
                  data = subset(df, condition == "Decreasing Severity" | condition == "Increasing Severity"))
fit_H3_p <- survdiff(Surv(surv_time, recalled) ~ decreasing, 
                     data = subset(df, condition == "Decreasing Severity" | condition == "Increasing Severity"))
fit_H3_p$pvalue
ggsurv_H3 <- 
  ggsurvplot(fit_H3,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("Increasing Severity", "Decreasing Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("darkred", "darkblue"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H3 <- 
  ggsurv_H3$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) < '0.0001'", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H3 <- ggsurv_H3 +
  ggtitle("Increasing vs. Decreasing Severity") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

# Increasing Severity vs. No Transgressions
fit_H4a <- survfit(Surv(surv_time, recalled) ~ increasing, 
                   data = subset(df, condition == "Increasing Severity" | condition == "No Transgressions"))
fit_H4a_p <- survdiff(Surv(surv_time, recalled) ~ increasing, 
                      data = subset(df, condition == "Increasing Severity" | condition == "No Transgressions"))
fit_H4a_p$pvalue
ggsurv_H4a <- 
  ggsurvplot(fit_H4a,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("No Transgressions", "Increasing Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("grey50", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H4a <- 
  ggsurv_H4a$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) < '0.0001'", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H4a <- ggsurv_H4a +
  ggtitle("Increasing Severity vs. No Transgressions") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

# Decreasing Severity vs. No Transgressions
fit_H4b <- survfit(Surv(surv_time, recalled) ~ decreasing, 
                   data = subset(df, condition == "Decreasing Severity" | condition == "No Transgressions"))
fit_H4b_p <- survdiff(Surv(surv_time, recalled) ~ decreasing, 
                      data = subset(df, condition == "Decreasing Severity" | condition == "No Transgressions"))
fit_H4b_p$pvalue
ggsurv_H4b <- 
  ggsurvplot(fit_H4b,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("No Transgressions", "Decreasing Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("grey50", "darkblue"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H4b <- 
  ggsurv_H4b$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) < '0.0001'", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H4b <- ggsurv_H4b +
  ggtitle("Decreasing Severity vs. No Transgressions") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

# Sporadic Severity vs. No Transgressions
fit_H4c <- survfit(Surv(surv_time, recalled) ~ sporadic, 
                   data = subset(df, condition == "Sporadic Severity" | condition == "No Transgressions"))
fit_H4c_p <- survdiff(Surv(surv_time, recalled) ~ sporadic, 
                      data = subset(df, condition == "Sporadic Severity" | condition == "No Transgressions"))
fit_H4c_p$pvalue
ggsurv_H4c <- 
  ggsurvplot(fit_H4c,
             conf.int = TRUE,
             ggtheme = theme_classic(),
             legend.labs = c("No Transgressions", "Sporadic Severity"),
             legend.title = "",
             legend = c(0.78, 0.92),
             font.xlab = list(size = 9),
             font.ylab = list(size = 9),
             font.legend = list(size = 8),
             palette = c("grey50", "darkgreen"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_H4c <- 
  ggsurv_H4c$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 3.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) < '0.0001'", parse = TRUE,
           size = 3.5, family = "Times")
ggsurv_H4c <- ggsurv_H4c +
  ggtitle("Sporadic Severity vs. No Transgressions") +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))

plot_grid(ggsurv_H4a, ggsurv_H4b, ggsurv_H4c, ggsurv_H1, ggsurv_H2, ggsurv_H3, 
          labels = "AUTO", nrow = 2, 
          label_fontfamily = "Times", label_size = 12)
ggsave("Figure 2.pdf", width = 9, height = 8)
# ggsave("Figure 2.png", dpi = 600, width = 9, height = 8)

### Kaplan-Meier curves by condition and partisanship: Figure 4 ----
## Visualize the survival curves and conduct log-rank tests to compare the curves
# No Transgressions
fit_T0 <- survfit(Surv(surv_time, recalled) ~ pid3, 
                  data = subset(df, condition == "No Transgressions"))
fit_T0_p <- survdiff(Surv(surv_time, recalled) ~ pid3, 
                     data = subset(df, condition == "No Transgressions"))
fit_T0_p$pvalue
ggsurv_T0 <- 
  ggsurvplot(fit_T0,
             conf.int = TRUE,
             legend.labs = c("Independent", "Democrat", "Republican"),
             linetype = "strata",
             risk.table.col = "strata",
             ggtheme = theme_classic(),
             legend.title = "",
             font.xlab = list(size = 12),
             font.ylab = list(size = 12),
             font.legend = list(size = 12),
             palette = c("darkgreen", "darkblue", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_T0 <- 
  ggsurv_T0$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 4.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) == 0.2949", parse = TRUE,
           size = 4.5, family = "Times")
ggsurv_T0 <- ggsurv_T0 +
  ggtitle("No Transgressions Condition") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(family = "Times", size = 12))

# Increasing Severity
fit_T1 <- survfit(Surv(surv_time, recalled) ~ pid3, 
                  data = subset(df, condition == "Increasing Severity"))
fit_T1_p <- survdiff(Surv(surv_time, recalled) ~ pid3, 
                     data = subset(df, condition == "Increasing Severity"))
fit_T1_p$pvalue
ggsurv_T1 <- 
  ggsurvplot(fit_T1,
             conf.int = TRUE,
             legend.labs = c("Independent", "Democrat", "Republican"),
             linetype = "strata",
             risk.table.col = "strata",
             ggtheme = theme_classic(),
             legend.title = "",
             font.xlab = list(size = 12),
             font.ylab = list(size = 12),
             font.legend = list(size = 12),
             palette = c("darkgreen", "darkblue", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_T1 <- 
  ggsurv_T1$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 4.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) == 0.0328", parse = TRUE,
           size = 4.5, family = "Times")
ggsurv_T1 <- ggsurv_T1 +
  ggtitle("Increasing Severity Condition") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(family = "Times", size = 12))

# Decreasing Severity
fit_T2 <- survfit(Surv(surv_time, recalled) ~ pid3, 
                  data = subset(df, condition == "Decreasing Severity"))
fit_T2_p <- survdiff(Surv(surv_time, recalled) ~ pid3, 
                     data = subset(df, condition == "Decreasing Severity"))
fit_T2_p$pvalue
ggsurv_T2 <- 
  ggsurvplot(fit_T2,
             conf.int = TRUE,
             legend.labs = c("Independent", "Democrat", "Republican"),
             linetype = "strata",
             risk.table.col = "strata",
             ggtheme = theme_classic(),
             legend.title = "",
             font.xlab = list(size = 12),
             font.ylab = list(size = 12),
             font.legend = list(size = 12),
             palette = c("darkgreen", "darkblue", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_T2 <- 
  ggsurv_T2$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 4.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) == 0.0097", parse = TRUE,
           size = 4.5, family = "Times")
ggsurv_T2 <- ggsurv_T2 +
  ggtitle("Decreasing Severity Condition") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(family = "Times", size = 12))

# Sporadic Severity
fit_T3 <- survfit(Surv(surv_time, recalled) ~ pid3, 
                  data = subset(df, condition == "Sporadic Severity"))
fit_T3_p <- survdiff(Surv(surv_time, recalled) ~ pid3, 
                     data = subset(df, condition == "Sporadic Severity"))
fit_T3_p$pvalue
ggsurv_T3 <- 
  ggsurvplot(fit_T3,
             conf.int = TRUE,
             legend.labs = c("Independent", "Democrat", "Republican"),
             linetype = "strata",
             risk.table.col = "strata",
             ggtheme = theme_classic(),
             legend.title = "",
             font.xlab = list(size = 12),
             font.ylab = list(size = 12),
             font.legend = list(size = 12),
             palette = c("darkgreen", "darkblue", "darkred"),
             size = 1, censor.size = 3) +
  xlab("Number of periods") +
  ylab("Probability of respondents not voting to recall the governor")
ggsurv_T3 <- 
  ggsurv_T3$plot + 
  theme(text = element_text(family = "Times", color = "black"),
        axis.text = element_text(family = "Times", size = 9)) +
  annotate("text", x = 5, y = 0.68,
           label = "Log-rank test:",
           size = 4.5, family = "Times") +
  annotate("text", x = 5, y = 0.62,
           label = "italic(p) == 0.0217", parse = TRUE,
           size = 4.5, family = "Times")
ggsurv_T3 <- ggsurv_T3 +
  ggtitle("Sporadic Severity Condition") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(family = "Times", size = 12))

plot_grid(ggsurv_T1, ggsurv_T2, ggsurv_T3, ggsurv_T0, 
          labels = "AUTO", nrow = 2, 
          label_fontfamily = "Times", label_size = 15)
ggsave("Figure 4.pdf", width = 11, height = 11)
# ggsave("Figure 4.png", dpi = 600, width = 11, height = 11)

### Recall decisions by treatment condition: Figure S8 ----
## First period
summary_recall_t1 <- df %>% 
  group_by(condition) %>% 
  count(recall_t1) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 1) %>% 
  filter(recall_t1 == 1)

## Second period
summary_recall_t2 <- df %>% 
  group_by(condition) %>% 
  count(recall_t2) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 2) %>% 
  filter(recall_t2 == 1)

## Third period
summary_recall_t3 <- df %>% 
  group_by(condition) %>% 
  count(recall_t3) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 3) %>% 
  filter(recall_t3 == 1)

## Fourth period
summary_recall_t4 <- df %>% 
  group_by(condition) %>% 
  count(recall_t4) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 4) %>% 
  filter(recall_t4 == 1)

## Fifth period
summary_recall_t5 <- df %>% 
  group_by(condition) %>% 
  count(recall_t5) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 5) %>% 
  filter(recall_t5 == 1)

## Sixth period
summary_recall_t6 <- df %>% 
  group_by(condition) %>% 
  count(recall_t6) %>% 
  mutate(prop = n / sum(n),
         lower = lapply(n, prop.test, n = sum(n)), 
         upper = sapply(lower, function(x) x$conf.int[2]), 
         lower = sapply(lower, function(x) x$conf.int[1]),
         period = 6) %>% 
  filter(recall_t6 == 1)

## Visualize the estimates
summary_recall <- 
  bind_rows(list(summary_recall_t1, summary_recall_t2, summary_recall_t3,
                 summary_recall_t4, summary_recall_t5, summary_recall_t6))
ggplot(summary_recall, aes(x = period, y = prop, color = condition, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(.9), color = "black") +
  scale_fill_manual(values = alpha(c("grey80", "darkred", "darkblue", "darkgreen"), 0.7)) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(labels = scales::percent) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                linewidth = .3, width = .2, position = position_dodge(.9), color = "black") +
  theme_classic() +
  xlab("Period") +
  ylab("Percentage of recall votes") +
  coord_cartesian(ylim = c(0, 1)) +
  theme(text = element_text(color = "black", family = "Times", size = 12),
        axis.text = element_text(family = "Times", size = 12),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.key.size = unit(1.5, "line"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = -0.15, l = 0.15, b = 0.10, r = 0.15, unit = "cm"),
        legend.title = element_blank())
ggsave("Figure S8.pdf", width = 8, height = 4)

### Approval ratings by treatment condition: Figure S9 ----
## Baseline period
summary_approval_t0 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t0),
            lower = mean(approval_t0) - qt(1 - .05/2, (n() - 1)) * sd(approval_t0)/sqrt(n()),
            upper = mean(approval_t0) + qt(1 - .05/2, (n() - 1)) * sd(approval_t0)/sqrt(n())) %>% 
  mutate(period = 0)

## First period
summary_approval_t1 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t1),
            lower = mean(approval_t1) - qt(1 - .05/2, (n() - 1)) * sd(approval_t1)/sqrt(n()),
            upper = mean(approval_t1) + qt(1 - .05/2, (n() - 1)) * sd(approval_t1)/sqrt(n())) %>% 
  mutate(period = 1)

## Second period
summary_approval_t2 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t2),
            lower = mean(approval_t2) - qt(1 - .05/2, (n() - 1)) * sd(approval_t2)/sqrt(n()),
            upper = mean(approval_t2) + qt(1 - .05/2, (n() - 1)) * sd(approval_t2)/sqrt(n())) %>% 
  mutate(period = 2)

## Third period
summary_approval_t3 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t3),
            lower = mean(approval_t3) - qt(1 - .05/2, (n() - 1)) * sd(approval_t3)/sqrt(n()),
            upper = mean(approval_t3) + qt(1 - .05/2, (n() - 1)) * sd(approval_t3)/sqrt(n())) %>% 
  mutate(period = 3)

## Fourth period
summary_approval_t4 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t4),
            lower = mean(approval_t4) - qt(1 - .05/2, (n() - 1)) * sd(approval_t4)/sqrt(n()),
            upper = mean(approval_t4) + qt(1 - .05/2, (n() - 1)) * sd(approval_t4)/sqrt(n())) %>% 
  mutate(period = 4)

## Fifth period
summary_approval_t5 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t5),
            lower = mean(approval_t5) - qt(1 - .05/2, (n() - 1)) * sd(approval_t5)/sqrt(n()),
            upper = mean(approval_t5) + qt(1 - .05/2, (n() - 1)) * sd(approval_t5)/sqrt(n())) %>% 
  mutate(period = 5)

## Sixth period
summary_approval_t6 <- df %>% 
  group_by(condition) %>% 
  summarize(mean = mean(approval_t6),
            lower = mean(approval_t6) - qt(1 - .05/2, (n() - 1)) * sd(approval_t6)/sqrt(n()),
            upper = mean(approval_t6) + qt(1 - .05/2, (n() - 1)) * sd(approval_t6)/sqrt(n())) %>% 
  mutate(period = 6)

## Visualize the estimates
summary_approval <- 
  bind_rows(list(summary_approval_t0, summary_approval_t1, summary_approval_t2, 
                 summary_approval_t3, summary_approval_t4, summary_approval_t5,
                 summary_approval_t6))
ggplot(summary_approval, aes(x = period, y = mean, color = condition, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(.9), color = "black") +
  scale_fill_manual(values = alpha(c("grey80", "darkred", "darkblue", "darkgreen"), 0.7)) +
  scale_x_continuous(breaks = 0:6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                linewidth = .3, width = .2, position = position_dodge(.9), color = "black") +
  theme_classic() +
  xlab("Period") +
  ylab("Approval rating of the governor") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(text = element_text(color = "black", family = "Times", size = 12),
        axis.text = element_text(family = "Times", size = 12),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.key.size = unit(1.5, "line"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = -0.15, l = 0.15, b = 0.10, r = 0.15, unit = "cm"),
        legend.title = element_blank())
ggsave("Figure S9.pdf", width = 8, height = 4)

### Predictors of loyal supporters: Tables S7-S8 and Figure 3 ----
## Identify loyal supporters (i.e., respondents reluctant to recall the governor in all periods)
df$loyal <- ifelse(df$first_recall == 7, 1, 0)
table(df$loyal, df$condition)

## Descriptive analysis: percentage of loyal supporters in treatment vs. control conditions
prop.test(x = c(202 + 184 + 204, 422),
          n = c(859 + 862 + 863 + 202 + 184 + 204, 638 + 422),
          correct = FALSE)

## OLS regression predicting loyal supporters in each condition
# Model 1: No Transgressions condition
mod1 <- lm_robust(loyal ~ age6 + male + white + college + pid3 + ideo + antiest, 
                  data = df, subset = condition == "No Transgressions")

# Model 2: Increasing Severity condition
mod2 <- lm_robust(loyal ~ age6 + male + white + college + pid3 + ideo + antiest, 
                  data = df, subset = condition == "Increasing Severity")

# Model 3: Decreasing Severity condition
mod3 <- lm_robust(loyal ~ age6 + male + white + college + pid3 + ideo + antiest, 
                  data = df, subset = condition == "Decreasing Severity")

# Model 4: Sporadic Severity condition
mod4 <- lm_robust(loyal ~ age6 + male + white + college + pid3 + ideo + antiest, 
                  data = df, subset = condition == "Sporadic Severity")

# Generate a regression table (Table S7)
texreg(list(mod1, mod2, mod3, mod4), 
       include.ci = FALSE, stars = numeric(0),
       custom.header = list("DV: Not Recalling the Governor in All Periods (= 1)" = 1:4),
       custom.note = "Entries are OLS estimates with robust standard errors in parentheses.",
       fontsize = "small")

## OLS regression probing the role of affective polarization in each condition
# Model 1.1: No Transgressions condition (bivariate correlation)
mod1.1 <- 
  tidy(lm_robust(loyal ~ scale(affect), 
                 data = df, subset = condition == "No Transgressions")) %>% 
  mutate(condition = "No Transgressions", model = "Bivariate correlation")

# Model 1.2: No Transgressions condition (OLS estimate with covariates)
mod1.2 <- 
  tidy(lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                 data = df, subset = condition == "No Transgressions")) %>% 
  mutate(condition = "No Transgressions", model = "OLS estimate with covariates")

# Model 2.1: Increasing Severity condition (bivariate correlation)
mod2.1 <- 
  tidy(lm_robust(loyal ~ scale(affect), 
                 data = df, subset = condition == "Increasing Severity")) %>% 
  mutate(condition = "Increasing Severity", model = "Bivariate correlation")

# Model 2.2: Increasing Severity condition (OLS estimate with covariates)
mod2.2 <- 
  tidy(lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                 data = df, subset = condition == "Increasing Severity")) %>% 
  mutate(condition = "Increasing Severity", model = "OLS estimate with covariates")

# Model 3.1: Decreasing Severity condition (bivariate correlation)
mod3.1 <- 
  tidy(lm_robust(loyal ~ scale(affect), 
                 data = df, subset = condition == "Decreasing Severity")) %>% 
  mutate(condition = "Decreasing Severity", model = "Bivariate correlation")

# Model 3.2: Decreasing Severity condition (OLS estimate with covariates)
mod3.2 <- 
  tidy(lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                 data = df, subset = condition == "Decreasing Severity")) %>% 
  mutate(condition = "Decreasing Severity", model = "OLS estimate with covariates")

# Model 4.1: Sporadic Severity condition (bivariate correlation)
mod4.1 <- 
  tidy(lm_robust(loyal ~ scale(affect), 
                 data = df, subset = condition == "Sporadic Severity")) %>% 
  mutate(condition = "Sporadic Severity", model = "Bivariate correlation")

# Model 4.2: Sporadic Severity condition (OLS estimate with covariates)
mod4.2 <- 
  tidy(lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                 data = df, subset = condition == "Sporadic Severity")) %>% 
  mutate(condition = "Sporadic Severity", model = "OLS estimate with covariates")

# Generate a coefficient plot (Figure 3)
summary_loyal <- 
  bind_rows(mod1.1, mod1.2, mod2.1, mod2.2, mod3.1, mod3.2, mod4.1, mod4.2) %>% 
  filter(term == "scale(affect)")
summary_loyal$model <- 
  factor(summary_loyal$model,
         levels = c("Bivariate correlation", "OLS estimate with covariates"))
summary_loyal$condition <- 
  factor(summary_loyal$condition,
         levels = c("Sporadic Severity", "Decreasing Severity", "Increasing Severity", "No Transgressions"))
ggplot(summary_loyal, aes(y = condition, color = model, shape = model)) +
  geom_point(aes(x = estimate), position = position_dodge(-.3), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(-.3), height = 0) +
  geom_vline(xintercept = 0, lty = 2, color = "grey") +
  scale_color_manual(values = c("grey0", "grey50")) +
  theme_classic() +
  xlab("Standardized coefficient estimate of partisan affect with 95% CI") +
  ylab("") +
  coord_cartesian(xlim = c(-0.025, 0.125)) +
  theme(text = element_text(color = "black", family = "Times", size = 12),
        axis.text = element_text(family = "Times", size = 11),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.key.size = unit(1.5, "line"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = -0.15, l = 0.15, b = 0.10, r = 0.15, unit = "cm"),
        legend.title = element_blank()) +
  ggtitle("DV: Not Recalling the Governor in All Periods (= 1)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
ggsave("Figure 3.pdf", width = 7, height = 4.5)
# ggsave("Figure 3.png", dpi = 600, width = 7, height = 4.5)

## Generate a regression table (Table S8)
# Model 1.1: No Transgressions condition (bivariate correlation)
mod1.1 <- lm_robust(loyal ~ scale(affect), 
                    data = df, subset = condition == "No Transgressions")

# Model 1.2: No Transgressions condition (OLS estimate with covariates)
mod1.2 <- lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                    data = df, subset = condition == "No Transgressions")

# Model 2.1: Increasing Severity condition (bivariate correlation)
mod2.1 <- lm_robust(loyal ~ scale(affect), 
                    data = df, subset = condition == "Increasing Severity")

# Model 2.2: Increasing Severity condition (OLS estimate with covariates)
mod2.2 <- lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                    data = df, subset = condition == "Increasing Severity")

# Model 3.1: Decreasing Severity condition (bivariate correlation)
mod3.1 <- lm_robust(loyal ~ scale(affect),
                    data = df, subset = condition == "Decreasing Severity")

# Model 3.2: Decreasing Severity condition (OLS estimate with covariates)
mod3.2 <- lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                    data = df, subset = condition == "Decreasing Severity")

# Model 4.1: Sporadic Severity condition (bivariate correlation)
mod4.1 <- lm_robust(loyal ~ scale(affect), 
                    data = df, subset = condition == "Sporadic Severity")

# Model 4.2: Sporadic Severity condition (OLS estimate with covariates)
mod4.2 <- lm_robust(loyal ~ scale(affect) + age6 + male + white + college + pid3 + ideo, 
                    data = df, subset = condition == "Sporadic Severity")

texreg(list(mod1.1, mod1.2, mod2.1, mod2.2, mod3.1, mod3.2, mod4.1, mod4.2), 
       include.ci = FALSE, stars = numeric(0),
       custom.header = list("DV: Not Recalling the Governor in All Periods (= 1)" = 1:8),
       custom.note = "Entries are OLS estimates with robust standard errors in parentheses.",
       fontsize = "small")

## Correlation between loyal support and beliefs about recall outcomes
# No Transgressions condition
cor.test(df$loyal[df$condition == "No Transgressions"], 
         df$overall_assess[df$condition == "No Transgressions"])

# Increasing Severity condition
cor.test(df$loyal[df$condition == "Increasing Severity"], 
         df$overall_assess[df$condition == "Increasing Severity"])

# Decreasing Severity condition
cor.test(df$loyal[df$condition == "Decreasing Severity"], 
         df$overall_assess[df$condition == "Decreasing Severity"])

# Sporadic Severity condition
cor.test(df$loyal[df$condition == "Sporadic Severity"], 
         df$overall_assess[df$condition == "Sporadic Severity"])

### Assessment of whether the governor will be recalled by treatment condition: Table S9 ----
## OLS regression estimating the average treatment effects on the outcome
mod1 <- lm_robust(overall_assess ~ increasing + decreasing + sporadic,
                  data = df)
mod1_tidy <- tidy(mod1) %>% mutate(model = "Without covariates")
mod2 <- lm_robust(overall_assess ~ increasing + decreasing + sporadic +
                    age6 + male + white + college + pid3 + ideo, 
                  data = df)
mod2_tidy <- tidy(mod2) %>% mutate(model = "With covariates")

## Generate a regression table
texreg(list(mod1, mod2), 
       include.ci = FALSE, stars = numeric(0),
       custom.header = list("DV: Recall Would Be Successful at Some Point (= 1)" = 1:2),
       custom.note = "Entries are OLS estimates with robust standard errors in parentheses.",
       fontsize = "small")

### Heterogeneous treatment effects: Table S4 ----
## Partisanship
cox_pid <- 
  coxph(Surv(surv_time, recalled) ~ (increasing + decreasing + sporadic) * pid3, 
        data = df)
summary(cox_pid)

## Partisan affect
cox_affect <- 
  coxph(Surv(surv_time, recalled) ~ (increasing + decreasing + sporadic) * affect + pid3, 
        data = df)
summary(cox_affect)

## By antiestablishment orientations
cox_antiest <- 
  coxph(Surv(surv_time, recalled) ~ (increasing + decreasing + sporadic) * antiest + pid3, 
        data = df)
summary(cox_antiest)

### Response time in the experimental module (partisans vs. independents): Figure S11 ----
## Calculate individual response time given the experimental condition
df <- df %>% mutate(response_time = case_when(
  exp_1 == 1 ~ group1_t1_timer_Page.Submit + group1_t2_timer_Page.Submit + group1_t3_timer_Page.Submit +
    group1_t4_timer_Page.Submit + group1_t5_timer_Page.Submit + group1_t6_timer_Page.Submit +
    t1_timer_1_Page.Submit + t2_timer_1_Page.Submit + t3_timer_1_Page.Submit +
    t4_timer_1_Page.Submit + t5_timer_1_Page.Submit + t6_timer_1_Page.Submit,
  exp_1 == 2 ~ group2_t1_timer_Page.Submit + group2_t2_timer_Page.Submit + group2_t3_timer_Page.Submit +
    group2_t4_timer_Page.Submit + group2_t5_timer_Page.Submit + group2_t6_timer_Page.Submit +
    t1_timer_1_Page.Submit.1 + t2_timer_1_Page.Submit.1 + t3_timer_1_Page.Submit.1 +
    t4_timer_1_Page.Submit.1 + t5_timer_1_Page.Submit.1 + t6_timer_1_Page.Submit.1,
  exp_1 == 3 ~ group3_t1_timer_Page.Submit + group3_t2_timer_Page.Submit + group3_t3_timer_Page.Submit +
    group3_t4_timer_Page.Submit + group3_t5_timer_Page.Submit + group3_t6_timer_Page.Submit +
    t1_timer_1_Page.Submit.2 + t2_timer_1_Page.Submit.2 + t3_timer_1_Page.Submit.2 +
    t4_timer_1_Page.Submit.2 + t5_timer_1_Page.Submit.2 + t6_timer_1_Page.Submit.2,
  exp_1 == 4 ~ group4_t1_timer_Page.Submit + group4_t2_timer_Click.Count + group4_t3_timer_Page.Submit +
    group4_t4_timer_Page.Submit + group4_t5_timer_Page.Submit + group4_t6_timer_Page.Submit +
    t1_timer_1_Page.Submit.3 + t2_timer_1_Page.Submit.3 + t3_timer_1_Page.Submit.3 +
    t4_timer_1_Page.Submit.3 + t5_timer_1_Page.Submit.3 + t6_timer_1_Page.Submit.3
))

## Visualize the distribution of response time by partisanship
temp <- plyr::ddply(df, "pid3", summarize, grp.median = median(response_time))
ggplot(df, aes(x = response_time)) +
  geom_density(aes(fill = pid3), color = "black", linewidth = 0.5) +
  facet_grid(pid3 ~ .) +
  scale_fill_manual(values = alpha(c("darkgreen", "darkblue", "darkred"), 0.5)) +
  geom_vline(data = temp, aes(xintercept = grp.median), 
             color = "black", linetype = "dashed", linewidth = 0.7) +
  coord_cartesian(xlim = c(0, 300)) +
  labs(x = "Response time in seconds", y = "Density") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Times", size = 12, color = "black"),
        axis.text.y = element_text(family = "Times", size = 10, color = "black"),
        axis.text.x = element_text(family = "Times", size = 10, color = "black"),
        legend.title = element_text(family = "Times", size = 10, color = "black"),
        legend.text = element_text(family = "Times", size = 12, color = "black"),
        strip.text = element_text(family = "Times", size = 11, color = "black"))
ggsave("Figure S11.pdf", width = 8, height = 6)
lm_robust(response_time ~ pid3, data = df)

### Partisanship and antiestablishment orientations: Figure S10 ----
ggplot(df, aes(x = pid3, y = antiest, fill = pid3)) +
  stat_halfeye(adjust = 0.5, width = 0.3, .width = 0, justification = -0.3, point_color = NA) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  scale_fill_manual(values = alpha(c("darkgreen", "darkblue", "darkred"), 0.5)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Respondent partisanship", y = "Antiestablishment orientation") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Times", size = 12, color = "black"),
        axis.text.y = element_text(family = "Times", size = 10, color = "black"),
        axis.text.x = element_text(family = "Times", size = 10, color = "black"),
        legend.title = element_text(family = "Times", size = 10, color = "black"),
        legend.text = element_text(family = "Times", size = 12, color = "black"),
        strip.text = element_text(family = "Times", size = 11, color = "black"))
ggsave("Figure S10.pdf", width = 5, height = 5)
lm_robust(antiest ~ pid3, data = df)

### Cox modeling by existence of recall institutions: Table S6 and Figure S7 ----
## Identify the state with recall
df$recall_exists <- 
  ifelse(df$state == "AK" | df$state == "AZ" | df$state == "CA" | df$state == "CO" | 
           df$state == "GA" | df$state == "ID" | df$state == "IL" | df$state == "KS" | 
           df$state == "LA" | df$state == "MI" | df$state == "MN" | df$state == "MT" | 
           df$state == "NV" | df$state == "NJ" | df$state == "ND" | df$state == "OR" | 
           df$state == "RI" | df$state == "WA" | df$state == "WI", 1, 0)

## Estimate the preregistered Cox model for the states with recall
cox1 <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
                age6 + male + white + college + pid3 + ideo, 
              data = df, subset = recall_exists == 1)
summary(cox1)
summary_cox1 <- cox1 %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(model = "States with recall") %>% 
  slice(1:3)

## Estimate the preregistered Cox model for the states without recall
cox0 <- coxph(Surv(surv_time, recalled) ~ increasing + decreasing + sporadic +
                age6 + male + white + college + pid3 + ideo, 
              data = df, subset = recall_exists == 0)
summary(cox0)
summary_cox0 <- cox0 %>% 
  tidy(conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  mutate(model = "States without recall") %>% 
  slice(1:3)

## Plot the Cox coefficients
summary_main_cox_recall <- bind_rows(summary_cox1, summary_cox0)
summary_main_cox_recall$model <- 
  factor(summary_main_cox_recall$model,
         levels = c("States without recall", "States with recall"))
summary_main_cox_recall$term <- 
  factor(summary_main_cox_recall$term,
         levels = c("sporadic", "decreasing", "increasing"),
         labels = c("Sporadic Severity", "Decreasing Severity", "Increasing Severity"))
ggplot(summary_main_cox_recall, aes(y = term, color = model, shape = model)) +
  geom_point(aes(x = estimate), position = position_dodge(-.3), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(-.3), height = 0) +
  geom_vline(xintercept = 0, lty = 2, color = "grey") +
  scale_color_manual(values = c("grey0", "grey50")) +
  theme_classic() +
  xlab("Cox coefficient with 95% CI") +
  ylab("") +
  coord_cartesian(xlim = c(-0.2, 0.8)) +
  theme(text = element_text(color = "black", family = "Times", size = 12),
        axis.text = element_text(family = "Times", size = 11),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.key.size = unit(1.5, "line"),
        legend.key.height = unit(0.5, "cm"),
        legend.margin = margin(t = -0.15, l = 0.15, b = 0.10, r = 0.15, unit = "cm"),
        legend.title = element_blank())
ggsave("Figure S7.pdf", width = 7, height = 4.5)

## Close the log file
log_close()