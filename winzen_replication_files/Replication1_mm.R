# This R code is based on the Replication1.do file accessible here: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L5W9NT

# The translation was based largely on the following material: 
# https://www.hertiecodingclub.com/learn/rstudio/stata_to_r/
# https://raw.githubusercontent.com/rstudio/cheatsheets/master/stata2r.pdf 

# The following code only includes the code in the original Stata file
# For full comments, please refer to the original Stata file (Replication1.do)

# The replication materials are originally in relation to the following paper:
# Winzen, Thomas. 2023. “Replication Data for: Winzen, Thomas.
# How Backsliding Governments Keep the European Union Hospitable for Autocracy: 
# Evidence from Intergovernmental Negotiations.&rdquo; The Review of 
# International Organizations.” Harvard Dataverse. https://doi.org/10.7910/DVN/L5W9NT.

# Load packages
library(here)
library(readr)
library(dplyr)
library(ggplot2)

here("PS446/winzen_replication_files")

###################### LOADING DATA FOR ANALYSIS IN R
analysisR <- read.delim(here("PS446/winzen_replication_files/analysis.tab")) 
write.csv(analysisR, here("PS446", "winzen_replication_files", 
                          "analysisR.csv"))

# Filter to include only yintro>2000
analysisRwaves2and3 <- analysisR %>%
  filter(yintro>2000)
# load as a .csv file into directory
write.csv(analysisRwaves2and3, here("PS446", "winzen_replication_files", 
                                    "analysisRwaves2and3.csv"))
  

###################### COMPARISON WITH DATA CODED BY WRATIL 

# Based on review of analysisR df, there are the following variable types: 
# proeu = continuous, int_position = continuous, proeubin = binary, 
# int_binary = binary

cor(analysisR$proeu, analysisR$int_position, use = "complete.obs")
cor(analysisR$proeubin, analysisR$int_binary, use = "complete.obs")

# Create manual flag var (diff) and enter values of 1 or NA
analysisR_check <- analysisR %>%
  mutate(diff = ifelse(
    !is.na(analysisR$proeubin) & !is.na(analysisR$int_binary)
    & analysisR$proeubin != analysisR$int_binary, 
    1, NA
  ))

# Order columns
analysisR_check <- analysisR_check %>%
  select(ctr, antieu, proeu, int_position, proeubin, 
         int_binary, diff, position, everything())

#Sort columns by isnrmc, then by ctr within each isnrmc
analysisR_check <- analysisR_check %>%
  arrange(isnrnmc, ctr)

# create df mapping isnrmc, ctr, and check values as specified
check_map <- data.frame(
  isnrnmc = c(18, 52, 73, 74, 215, 241, 247, 254, 255, 329,
              39, 72, 92, 96, 133, 150, 154, 185, 305, 313, 320, 325, 330),
  ctr = c("at","at","at","at","at","at","at","at","at","at",
          "fi","dk","dk","be","be","es","nl","de","nl","de","be","cy","be"),
  check = c(1,1,1,0.5,1,0.5,1,1,1,1,
            1,1,0.5,1,1,1,1,1,1,1,1,1,1)
)

# Join map and existing analysisR_check df
analysisR_check <- analysisR_check %>%
  left_join(check_map, by = c("isnrnmc", "ctr"))

# Reorder columns, put check first
analysisR_check <- analysisR_check %>%
  select(check, everything())


######################### DESCRIPTIVE PLOTS
proeu_hist <- ggplot(analysisR, aes(x = proeu)) +
  geom_histogram()

position_hist <- ggplot(analysisR, aes(x = position)) +
  geom_histogram()

tab_proeu <- table(analysisR$proeu)
tab_proeubin <- table(analysisR$proeubin)

######################### DEU ISSUES 
analysisR_deu <- analysisR %>%
  select(prnrnmc, isnrnmc, dintro, mintro, yintro, sensitive, sensitivearea) %>%
  group_by(prnrnmc, isnrnmc) %>%
  summarize(
    sensitive = mean(sensitive),
    sensitivearea = mean(sensitivearea),
    yintro = mean(yintro),
    mintro = mean(mintro),
    dintro = mean(dintro),
    .groups = "drop"
  ) %>%
  filter(sensitivearea != 0)




