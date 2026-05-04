######################################
### Dynamic Democratic Backsliding ###
### [Pretest]                      ###
### Author: Eddy S. F. Yeung       ###
### Date: July 8, 2025             ###
######################################

### Set-up ----
## Clean the working environment and set up the working directory
rm(list = ls())
setwd("~/Desktop/dynamic_backsliding/replication") # set your working directory here, which should also contain the survey data ("pretest_data.csv")

## Import the dataset
df <- read.csv("pretest_data.csv")

## Load the required packages
library(tidyverse)
library(estimatr)

### Sample demographics ----
## Sex
df$sex <- factor(df$sex, levels = 1:3, labels = c("Male", "Female", "Nonbinary"))
table(df$sex) %>% prop.table() * 100

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

### Mean perceived harmfulness of each statement of governor action: Figure S5 (publication), Figure 1 (PAP) ----
## Estimate the results
# Create an empty list to store the results
results_list <- list()

# Loop through the variable names and fit lm_robust models
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df)
  results_list[[i]] <- tidy(model)
}

# Combine the results into a single data frame
results_df <- do.call(rbind, results_list)

# Create the statement names
statement_names <- 
  c("Ignoring unfavorable court rulings by opposing party's judges",
    "Ruling by executive order if opposing party does not concede",
    "Reducing polling stations in opposition strongholds",
    "Signing a redistricting plan to advantage own party",
    "Banning rallies by opposing party's extremists",
    "Prosecuting journalists who accuse them of misconduct",
    "Supervising law enforcement investigations of politicians",
    "Changing voter ID laws to disadvantage opposing party",
    "Making voting less convenient for opposing party's voters",
    "Trying to impeach justices who ruled against them",
    "Proposing to eliminate bipartisan election commission",
    "Removing inactive voters from the voter rolls",
    "Using government agencies to monitor or punish opponents",
    "Interfering with law enforcement investigations of officials",
    "Banning funds from NGOs that aim to help administer voting",
    "Accepting campaign contributions from millionaire donors",
    "Eliminating bargaining rights for most government employees",
    "Sending voter data to an NGO to improve voter roll accuracy",
    "Vetoing a bill opposed by most state residents",
    "Vetoing a new policy supported by most state residents",
    "Deferring the new state budget due to continued debates",
    "Granting clemency to people who are convicted of crimes",
    "Providing inaccessible and inefficient constituent services",
    "Implementing federal programs despite low citizen support",
    "Declaring a state of emergency due to a natural disaster")

# Reorder the factors based on the size of the estimates in ascending order
results_df$statement <- 
  factor(results_df$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df$statement <- fct_reorder(results_df$statement, results_df$estimate)

# Indicate democratic transgressions (1 = yes; 0 = no)
results_df$transgression <- c(rep(1, 15), rep(0, 10))

# Indicate selected democratic transgressions (2 = selected)
results_df$transgression <- 
  ifelse(
    results_df$statement == "Ruling by executive order if opposing party does not concede" |
      results_df$statement == "Signing a redistricting plan to advantage own party" |
      results_df$statement == "Banning rallies by opposing party's extremists" |
      results_df$statement == "Supervising law enforcement investigations of politicians" |
      results_df$statement == "Proposing to eliminate bipartisan election commission" |
      results_df$statement == "Banning funds from NGOs that aim to help administer voting",
    2, results_df$transgression
  )
results_df$transgression <- as.factor(results_df$transgression)

## Visualize the results
ggplot(data = results_df, 
       aes(x = estimate, y = statement, color = transgression)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0) +
  scale_color_manual(values = c("grey75", "black", "red")) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Times"))
ggsave("Figure S5.pdf", width = 8, height = 6)

### Subgroup analysis by partisanship: Figure 3 (PAP) ----
## Generate the partisanship variable
df <- df %>% mutate(pid3 = case_when(
  pid_1 == 1 | pid_2i == 1 ~ "Republican",
  pid_1 == 2 | pid_2i == 2 ~ "Democrat",
  pid_2i == 3              ~ "Independent"
))
table(df$pid3)

## Estimate the results
# Democrats
results_list_pid_dem <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = pid3 == "Democrat")
  results_list_pid_dem[[i]] <- tidy(model)
}
results_df_pid_dem <- do.call(rbind, results_list_pid_dem)
results_df_pid_dem$pid <- "Democrat"

# Republicans
results_list_pid_gop <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = pid3 == "Republican")
  results_list_pid_gop[[i]] <- tidy(model)
}
results_df_pid_gop <- do.call(rbind, results_list_pid_gop)
results_df_pid_gop$pid <- "Republican"

# Independents
results_list_pid_ind <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = pid3 == "Independent")
  results_list_pid_ind[[i]] <- tidy(model)
}
results_df_pid_ind <- do.call(rbind, results_list_pid_ind)
results_df_pid_ind$pid <- "Independent"

# Merge the estimates and indicate the statements
results_df_pid <- rbind(results_df_pid_dem, results_df_pid_gop, results_df_pid_ind)
results_df_pid$statement <- 
  factor(results_df_pid$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_pid$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_pid$pid <- factor(results_df_pid$pid, 
                             levels = c("Independent", "Republican", "Democrat"),
                             labels = c("Independent", "Republican", "Democrat"))
results_df_pid$transgression <- ifelse(results_df_pid$statement == results_df$statement, results_df$transgression, NA)
results_df_pid$transgression <- as.factor(results_df_pid$transgression)

## Visualize the results
ggplot(data = results_df_pid, 
       aes(x = estimate, y = statement, color = transgression, shape = pid)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(4, 17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.87, .05),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 3.pdf", width = 8, height = 6)

### Subgroup analysis by Biden approval: Figure 5 (PAP) ----
## Generate the Biden approval variable
df <- df %>% mutate(biden = case_when(
  biden_approval == 1 | biden_approval == 2 ~ "Approve of Biden",
  biden_approval == 3 | biden_approval == 4 ~ "Disapprove of Biden"
))
table(df$biden)

## Estimate the results
# Respondents who approve of Biden
results_list_biden_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = biden == "Approve of Biden")
  results_list_biden_1[[i]] <- tidy(model)
}
results_df_biden_1 <- do.call(rbind, results_list_biden_1)
results_df_biden_1$biden <- "Approve of Biden"

# Respondents who disapprove of Biden
results_list_biden_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = biden == "Disapprove of Biden")
  results_list_biden_0[[i]] <- tidy(model)
}
results_df_biden_0 <- do.call(rbind, results_list_biden_0)
results_df_biden_0$biden <- "Disapprove of Biden"

# Merge the estimates and indicate the statements
results_df_biden <- rbind(results_df_biden_1, results_df_biden_0)
results_df_biden$statement <- 
  factor(results_df_biden$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_biden$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_biden$biden <- factor(results_df_biden$biden, 
                                 levels = c("Disapprove of Biden", "Approve of Biden"),
                                 labels = c("Disapprove of Biden", "Approve of Biden"))
results_df_biden$transgression <- ifelse(results_df_biden$statement == results_df$statement, results_df$transgression, NA)
results_df_biden$transgression <- as.factor(results_df_biden$transgression)

## Visualize the results
ggplot(data = results_df_biden, 
       aes(x = estimate, y = statement, color = transgression, shape = biden)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.82, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 5.pdf", width = 8, height = 6)

### Subgroup analysis by Trump approval: Figure 4 (PAP) ----
## Generate the Trump approval variable
df <- df %>% mutate(trump = case_when(
  trump_approval == 1 | trump_approval == 2 ~ "Approve of Trump",
  trump_approval == 3 | trump_approval == 4 ~ "Disapprove of Trump"
))
table(df$trump)

## Estimate the results
# Respondents who approve of Trump
results_list_trump_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = trump == "Approve of Trump")
  results_list_trump_1[[i]] <- tidy(model)
}
results_df_trump_1 <- do.call(rbind, results_list_trump_1)
results_df_trump_1$trump <- "Approve of Trump"

# Respondents who disapprove of Trump
results_list_trump_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = trump == "Disapprove of Trump")
  results_list_trump_0[[i]] <- tidy(model)
}
results_df_trump_0 <- do.call(rbind, results_list_trump_0)
results_df_trump_0$trump <- "Disapprove of Trump"

# Merge the estimates and indicate the statements
results_df_trump <- rbind(results_df_trump_1, results_df_trump_0)
results_df_trump$statement <- 
  factor(results_df_trump$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_trump$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_trump$trump <- factor(results_df_trump$trump, 
                                 levels = c("Disapprove of Trump", "Approve of Trump"),
                                 labels = c("Disapprove of Trump", "Approve of Trump"))
results_df_trump$transgression <- ifelse(results_df_trump$statement == results_df$statement, results_df$transgression, NA)
results_df_trump$transgression <- as.factor(results_df_trump$transgression)

## Visualize the results
ggplot(data = results_df_trump, 
       aes(x = estimate, y = statement, color = transgression, shape = trump)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.82, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 4.pdf", width = 8, height = 6)

### Subgroup analysis by education spending preference: Figure 6 (PAP) ----
## Generate the education spending preference variable
df <- df %>% mutate(educ_pref = case_when(
  policy_educ == 1 | policy_educ == 2 ~ "Increase Education Spending",
  policy_educ == 3 | policy_educ == 4 ~ "Decrease Education Spending"
))
table(df$educ_pref)

## Estimate the results
# Respondents who prefer increases in education spending
results_list_educ_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = educ_pref == "Increase Education Spending")
  results_list_educ_1[[i]] <- tidy(model)
}
results_df_educ_1 <- do.call(rbind, results_list_educ_1)
results_df_educ_1$educ <- "Increase Education Spending"

# Respondents who prefer decreases in education spending
results_list_educ_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = educ_pref == "Decrease Education Spending")
  results_list_educ_0[[i]] <- tidy(model)
}
results_df_educ_0 <- do.call(rbind, results_list_educ_0)
results_df_educ_0$educ <- "Decrease Education Spending"

# Merge the estimates and indicate the statements
results_df_educ <- rbind(results_df_educ_1, results_df_educ_0)
results_df_educ$statement <- 
  factor(results_df_educ$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_educ$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_educ$educ <- factor(results_df_educ$educ, 
                               levels = c("Decrease Education Spending", "Increase Education Spending"),
                               labels = c("Decrease Education Spending", "Increase Education Spending"))
results_df_educ$transgression <- ifelse(results_df_educ$statement == results_df$statement, results_df$transgression, NA)
results_df_educ$transgression <- as.factor(results_df_educ$transgression)

## Visualize the results
ggplot(data = results_df_educ, 
       aes(x = estimate, y = statement, color = transgression, shape = educ)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.765, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 6.pdf", width = 8, height = 6)

### Subgroup analysis by tax preference: Figure 7 (PAP) ----
## Generate the tax preference variable
df <- df %>% mutate(tax_pref = case_when(
  policy_tax == 1 | policy_tax == 2 ~ "Increase State Taxes",
  policy_tax == 3 | policy_tax == 4 ~ "Decrease State Taxes"
))
table(df$tax_pref)

## Estimate the results
# Respondents who prefer increases in state taxes
results_list_tax_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = tax_pref == "Increase State Taxes")
  results_list_tax_1[[i]] <- tidy(model)
}
results_df_tax_1 <- do.call(rbind, results_list_tax_1)
results_df_tax_1$tax <- "Increase State Taxes"

# Respondents who prefer decreases in state taxes
results_list_tax_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = tax_pref == "Decrease State Taxes")
  results_list_tax_0[[i]] <- tidy(model)
}
results_df_tax_0 <- do.call(rbind, results_list_tax_0)
results_df_tax_0$tax <- "Decrease State Taxes"

# Merge the estimates and indicate the statements
results_df_tax <- rbind(results_df_tax_1, results_df_tax_0)
results_df_tax$statement <- 
  factor(results_df_tax$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_tax$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_tax$tax <- factor(results_df_tax$tax, 
                             levels = c("Decrease State Taxes", "Increase State Taxes"),
                             labels = c("Decrease State Taxes", "Increase State Taxes"))
results_df_tax$transgression <- ifelse(results_df_tax$statement == results_df$statement, results_df$transgression, NA)
results_df_tax$transgression <- as.factor(results_df_tax$transgression)

## Visualize the results
ggplot(data = results_df_tax, 
       aes(x = estimate, y = statement, color = transgression, shape = tax)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.82, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 7.pdf", width = 8, height = 6)

### Subgroup analysis by immigration preference: Figure 8 (PAP) ----
## Generate the immigration preference variable
df <- df %>% mutate(immi_pref = case_when(
  policy_immi == 1 | policy_immi == 2 ~ "Passive Immigration Control",
  policy_immi == 3 | policy_immi == 4 ~ "Active Immigration Control"
))
table(df$immi_pref)

## Estimate the results
# Respondents who prefer a passive immigration control
results_list_immi_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = immi_pref == "Passive Immigration Control")
  results_list_immi_1[[i]] <- tidy(model)
}
results_df_immi_1 <- do.call(rbind, results_list_immi_1)
results_df_immi_1$immi <- "Passive Immigration Control"

# Respondents who prefer decreases in state taxes
results_list_immi_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = immi_pref == "Active Immigration Control")
  results_list_immi_0[[i]] <- tidy(model)
}
results_df_immi_0 <- do.call(rbind, results_list_immi_0)
results_df_immi_0$immi <- "Active Immigration Control"

# Merge the estimates and indicate the statements
results_df_immi <- rbind(results_df_immi_1, results_df_immi_0)
results_df_immi$statement <- 
  factor(results_df_immi$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_immi$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_immi$immi <- factor(results_df_immi$immi, 
                               levels = c("Active Immigration Control", "Passive Immigration Control"),
                               labels = c("Active Immigration Control", "Passive Immigration Control"))
results_df_immi$transgression <- ifelse(results_df_immi$statement == results_df$statement, results_df$transgression, NA)
results_df_immi$transgression <- as.factor(results_df_immi$transgression)

## Visualize the results
ggplot(data = results_df_immi, 
       aes(x = estimate, y = statement, color = transgression, shape = immi)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.77, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 8.pdf", width = 8, height = 6)

### Subgroup analysis by marijuana preference: Figure 9 (PAP) ----
## Generate the marijuana preference variable
df <- df %>% mutate(mari_pref = case_when(
  policy_mari == 1 | policy_mari == 2 ~ "Favor Legalizing Marijuana",
  policy_mari == 3 | policy_mari == 4 ~ "Oppose Legalizing Marijuana"
))
table(df$mari_pref)

## Estimate the results
# Respondents who prefer a passive immigration control
results_list_mari_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = mari_pref == "Favor Legalizing Marijuana")
  results_list_mari_1[[i]] <- tidy(model)
}
results_df_mari_1 <- do.call(rbind, results_list_mari_1)
results_df_mari_1$mari <- "Favor Legalizing Marijuana"

# Respondents who prefer decreases in state taxes
results_list_mari_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = mari_pref == "Oppose Legalizing Marijuana")
  results_list_mari_0[[i]] <- tidy(model)
}
results_df_mari_0 <- do.call(rbind, results_list_mari_0)
results_df_mari_0$mari <- "Oppose Legalizing Marijuana"

# Merge the estimates and indicate the statements
results_df_mari <- rbind(results_df_mari_1, results_df_mari_0)
results_df_mari$statement <- 
  factor(results_df_mari$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_mari$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_mari$mari <- factor(results_df_mari$mari, 
                               levels = c("Oppose Legalizing Marijuana", "Favor Legalizing Marijuana"),
                               labels = c("Oppose Legalizing Marijuana", "Favor Legalizing Marijuana"))
results_df_mari$transgression <- ifelse(results_df_mari$statement == results_df$statement, results_df$transgression, NA)
results_df_mari$transgression <- as.factor(results_df_mari$transgression)

## Visualize the results
ggplot(data = results_df_mari, 
       aes(x = estimate, y = statement, color = transgression, shape = mari)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.77, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 9.pdf", width = 8, height = 6)

### Subgroup analysis by racial identity: Figure 10 (PAP) ----
## Estimate the results
# White
results_list_white_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = white == 1)
  results_list_white_1[[i]] <- tidy(model)
}
results_df_white_1 <- do.call(rbind, results_list_white_1)
results_df_white_1$white <- "White"

# Nonwhite
results_list_white_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = white == 0)
  results_list_white_0[[i]] <- tidy(model)
}
results_df_white_0 <- do.call(rbind, results_list_white_0)
results_df_white_0$white <- "Nonwhite"

# Merge the estimates and indicate the statements
results_df_white <- rbind(results_df_white_1, results_df_white_0)
results_df_white$statement <- 
  factor(results_df_white$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_white$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_white$white <- factor(results_df_white$white, 
                                 levels = c("Nonwhite", "White"),
                                 labels = c("Nonwhite", "White"))
results_df_white$transgression <- ifelse(results_df_white$statement == results_df$statement, results_df$transgression, NA)
results_df_white$transgression <- as.factor(results_df_white$transgression)

## Visualize the results
ggplot(data = results_df_white, 
       aes(x = estimate, y = statement, color = transgression, shape = white)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.89, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 10.pdf", width = 8, height = 6)

### Subgroup analysis by sex: Figure 11 (PAP) ----
## Estimate the results
# Male
results_list_male_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = sex == "Male")
  results_list_male_1[[i]] <- tidy(model)
}
results_df_male_1 <- do.call(rbind, results_list_male_1)
results_df_male_1$male <- "Male"

# Female
results_list_male_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = sex == "Female")
  results_list_male_0[[i]] <- tidy(model)
}
results_df_male_0 <- do.call(rbind, results_list_male_0)
results_df_male_0$male <- "Female"

# Merge the estimates and indicate the statements
results_df_male <- rbind(results_df_male_1, results_df_male_0)
results_df_male$statement <- 
  factor(results_df_male$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_male$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_male$male <- factor(results_df_male$male, 
                               levels = c("Female", "Male"),
                               labels = c("Female", "Male"))
results_df_male$transgression <- ifelse(results_df_male$statement == results_df$statement, results_df$transgression, NA)
results_df_male$transgression <- as.factor(results_df_male$transgression)

## Visualize the results
ggplot(data = results_df_male, 
       aes(x = estimate, y = statement, color = transgression, shape = male)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.91, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 11.pdf", width = 8, height = 6)

### Subgroup analysis by college education: Figure 12 (PAP) ----
## Generate the college education variable
df <- df %>% mutate(college = case_when(
  educ >= 1 & educ <= 4 ~ "No College Degree",
  educ == 5 | educ == 6 ~ "College Degree"
))
table(df$college)

## Estimate the results
# College degree
results_list_college_1 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = college == "College Degree")
  results_list_college_1[[i]] <- tidy(model)
}
results_df_college_1 <- do.call(rbind, results_list_college_1)
results_df_college_1$college <- "College Degree"

# No college degree
results_list_college_0 <- list()
for (i in 1:25) {
  formula <- as.formula(paste0("statement_", i, " ~ 1"))
  model <- lm_robust(formula, data = df, subset = college == "No College Degree")
  results_list_college_0[[i]] <- tidy(model)
}
results_df_college_0 <- do.call(rbind, results_list_college_0)
results_df_college_0$college <- "No College Degree"

# Merge the estimates and indicate the statements
results_df_college <- rbind(results_df_college_1, results_df_college_0)
results_df_college$statement <- 
  factor(results_df_college$outcome, 
         levels = paste0("statement_", 1:25), 
         labels = statement_names)
results_df_college$statement <- fct_reorder(results_df$statement, results_df$estimate)
results_df_college$college <- factor(results_df_college$college, 
                                     levels = c("No College Degree", "College Degree"),
                                     labels = c("No College Degree", "College Degree"))
results_df_college$transgression <- ifelse(results_df_college$statement == results_df$statement, results_df$transgression, NA)
results_df_college$transgression <- as.factor(results_df_college$transgression)

## Visualize the results
ggplot(data = results_df_college, 
       aes(x = estimate, y = statement, color = transgression, shape = college)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = .5, width = 0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("grey75", "black", "red"), guide = "none") +
  scale_shape_manual(values = c(17, 16), guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  coord_cartesian(xlim = c(25, 75)) +
  xlab("Perceived Harmfulness to Democracy\n(0 = not at all harmful; 100 = extremely harmful)") +
  ylab("Governor Action") +
  theme(legend.position = c(.835, .035),
        legend.key = element_rect(fill = NA, color = NA),
        legend.box.background = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.size = unit(.75, "line"),
        legend.title = element_blank(),
        legend.margin = margin(t = 0, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        text = element_text(size = 12, family = "Times"))
ggsave("PAP_Figure 12.pdf", width = 8, height = 6)

### Formal tests of perceived harmfulness of democracy by different pairs of statements ----
## Proposing to eliminate bipartisan election commission (statement_11) vs.
## Signing a redistricting plan to advantage own party (statement_4)
t.test(x = df$statement_11, y = df$statement_4)
df$first.vs.second <- ifelse(df$statement_11 >= df$statement_4, 1, 0)
table(df$first.vs.second) %>% prop.table() * 100

## Signing a redistricting plan to advantage own party (statement_4) vs.
## Ruling by executive order if opposing party does not concede (statement_2)
t.test(x = df$statement_4, y = df$statement_2)
df$second.vs.third <- ifelse(df$statement_4 >= df$statement_2, 1, 0)
table(df$second.vs.third) %>% prop.table() * 100

## Ruling by executive order if opposing party does not concede (statement_2) vs.
## Banning rallies by opposing party's extremists (statement_5)
t.test(x = df$statement_2, y = df$statement_5)
df$third.vs.fourth <- ifelse(df$statement_2 >= df$statement_5, 1, 0)
table(df$third.vs.fourth) %>% prop.table() * 100

## Banning rallies by opposing party's extremists (statement_5) vs.
## Supervising law enforcement investigations of politicians (statement_7)
t.test(x = df$statement_5, y = df$statement_7)
df$fourth.vs.fifth <- ifelse(df$statement_5 >= df$statement_7, 1, 0)
table(df$fourth.vs.fifth) %>% prop.table() * 100

## Supervising law enforcement investigations of politicians (statement_7) vs.
## Banning funds from NGOs that aim to help administer voting (statement_15)
t.test(x = df$statement_7, y = df$statement_15)
df$fifth.vs.sixth <- ifelse(df$statement_7 >= df$statement_15, 1, 0)
table(df$fifth.vs.sixth) %>% prop.table() * 100

### Individual ordering of severity ----
## Mean rating of statement_11, statement_4, and statement_2 at the individual level
df <- df %>% 
  rowwise() %>% 
  mutate(more_severe_group = mean(c_across(c(statement_11, statement_4, statement_2)), na.rm = TRUE))
summary(df$more_severe_group)

## Mean rating of statement_5, statement_7, and statement_15 at the individual level
df <- df %>% 
  rowwise() %>% 
  mutate(less_severe_group = mean(c_across(c(statement_5, statement_7, statement_15)), na.rm = TRUE))
summary(df$less_severe_group)

## Percentage of respondents believing that the more severe transgressions are less severe than the less severe ones
table(ifelse(df$more_severe_group < df$less_severe_group, 1, 0))

## Test how the percentage changes if we focus on the attentive respondents
# Attentiveness based on timers
df_attentive1 <- subset(df, statement_timer_Page.Submit >= 150) # spent at least 10 seconds to evaluate severity
table(ifelse(df_attentive1$more_severe_group >= df_attentive1$less_severe_group, 1, 0))

# Attentiveness based on screeners
df_attentive2 <- subset(df, screener_2 == 3 & screener_4 == 2)
table(ifelse(df_attentive2$more_severe_group >= df_attentive2$less_severe_group, 1, 0))
