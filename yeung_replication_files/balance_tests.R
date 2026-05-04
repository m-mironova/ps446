######################################
### Dynamic Democratic Backsliding ###
### [Balance Tests]                ###
### Author: Eddy S. F. Yeung       ###
### Date: July 8, 2025             ###
######################################

### Set-up ----
## Clean the working environment and set up the working directory
rm(list = ls())
setwd("~/Desktop/dynamic_backsliding/replication") # set your working directory here, which should also contain the survey data ("experimental_data.csv")

## Import the dataset
df <- read.csv("experimental_data.csv")

## Load the required packages
library(tidyverse)
library(gridExtra)
library(cowplot)

### Sample demographics ----
## Sex
df$male <- ifelse(df$sex == 1, 1, 0)

## Age
df$age <- df$yob + 2023 - 2006

## Race
df$white <- grepl("1", df$race)

## Education (1 = college degree; 0 = no college degree)
df$college <- ifelse(df$educ == 5 | df$educ == 6, 1, 0)

## Partisanship
df$gop <- ifelse(df$pid_1 == 1 | df$pid_2i == 1, 1, 0)
df$dem <- ifelse(df$pid_1 == 2 | df$pid_2i == 2, 1, 0)

### Covariate balance between experimental groups ----
## Create functions using code from Hartman and Hidalgo (2018)
equiv.t.test <- function(x, y, alpha = .05, epsilon = .2, std.err = "nominal", 
                         cluster.x = NULL, cluster.y = NULL) {
  x = x[!is.na(x)]
  y = y[!is.na(y)]
  
  dbar <- mean(x) - mean(y)
  m <- as.double(length(x))
  n <- as.double(length(y))
  N <- m + n
  x.var <-  var(x)
  y.var <- var(y)
  non.cent <- (m * n * epsilon^2) / N
  critical.const <- sqrt(qf(alpha, 1, N - 2, non.cent))
  
  se = sqrt((m - 1) * x.var + (n - 1) * y.var) / sqrt(m * n * (N - 2) / N)
  df = N - 2
  
  t.stat <- dbar / se
  p = pf(abs(t.stat)^2, 1, df, non.cent)
  obs_smd = (mean(x) - mean(y)) / sd(y)
  inverted <- try(uniroot(function(x) pf(abs(t.stat)^2, 1, N - 2, 
                                         ncp = (m * n * x ^ 2) / N) - 
                            ifelse(pf(abs(t.stat)^2, 1, N - 2, 
                                      ncp = (m * n * 0 ^ 2) / N) < alpha, 
                                   pf(abs(t.stat)^2, 1, N - 2, 
                                      ncp = (m * n * obs_smd^2) / N), alpha), 
                          c(0, 10 * abs(t.stat)), tol = 0.0001)$root, 
                  silent = TRUE)
  if(class(inverted) == "try-error") {
    inverted = NA
  }
  rej = abs(t.stat) <= critical.const
  return(list(t.stat = t.stat, critical.const = critical.const, 
              power = 2 * pt(critical.const, N - 2) - 1, 
              rej = rej, p = p, inverted = inverted))
}

run_equiv <- function(X, Tr, epsilon.method = "std.effect", Y = NULL, 
                      custom.epsilon = NULL, std.err = "nominal", 
                      type = "equiv.t.test", fdr_correct = FALSE, 
                      cluster.x = NULL, cluster.y = NULL) {
  switch(epsilon.method,
         std.effect = {
           tol = abs(mean(Y[Tr == 1], na.rm = TRUE) - 
                       mean(Y[Tr == 0], na.rm = TRUE)) / 
             sd(Y[Tr == 0], na.rm = TRUE)
         }, 
         custom = {
           if(is.null(custom.epsilon)) stop("ERROR")
           tol = custom.epsilon
         },
         strict = {
           tol = 0.36
         }, 
         liberal = {
           tol = 0.74
         }, 
         stop("ERROR")
  )
  
  ranges = rep(tol, ncol(X))
  names(ranges) = names(X)
  print(ranges)
  
  # Conduct equivalence test for each variable
  tests = do.call("rbind", lapply(names(X), function(var) {
    print(var)
    
    res = equiv.t.test(X[Tr == 1, var], X[Tr == 0, var], 
                       epsilon = ranges[var], std.err = std.err, 
                       cluster.x = cluster.x, cluster.y = cluster.y)
    res$stat = res$t.stat
    res$obs_diff = (mean(X[Tr == 1, var], na.rm = TRUE) - 
                      mean(X[Tr == 0, var], na.rm = TRUE))
    res$obs_smd = ((mean(X[Tr == 1, var], na.rm = TRUE) - 
                      mean(X[Tr == 0, var], na.rm = TRUE))) / 
      sd(X[Tr == 0, var], na.rm = TRUE)
    res$sd = sd(X[Tr == 0, var], na.rm = TRUE)
    
    return(c(res$inverted
             , round(res$p, 4)
             , res$obs_smd
             , res$obs_diff
             , res$power
             , res$sd))
    
  }))
  
  p.vals = unlist(tests[, 2])
  names(p.vals) = names(ranges)
  
  power = unlist(tests[, 5])
  names(power) = names(ranges)
  
  inverted = unlist(tests[, 1])
  names(inverted) = names(ranges)
  
  sd = unlist(tests[, 6])
  names(sd) = names(ranges)
  
  # Return to scale of var
  inverted.scaled = unlist(lapply(names(inverted), function(var) {
    return(inverted[var] * sd[var])
  }))
  names(inverted.scaled) = names(ranges)
  
  observed.smd = unlist(tests[, 3])
  names(observed.smd) = names(ranges)
  
  observed.diff = unlist(tests[, 4])
  names(observed.diff) = names(ranges)
  
  # Conduct BH FDR adjustment
  if(fdr_correct) {
    p.vals = p.adjust(p.vals, method = "BH")  
  }
  
  return(list(tol = ranges, inverted = inverted, 
              inverted.scaled = inverted.scaled, p.vals = p.vals, 
              observed.diff = observed.diff, observed.smd = observed.smd, 
              power = power))
}

generate_plot <- function(equiv_tests, panel.widths = c(1, 1, 5, 1, 1), 
                          display.names = NULL, var.rounding = 1, 
                          pval.rounding = 2, fdr_correct = FALSE) {
  .e <- environment()
  if(!is.null(display.names)) {
    equiv_tests$names = factor(display.names, levels = rev(display.names))
  } else {
    equiv_tests$names = factor(row.names(equiv_tests), 
                               levels = rev(row.names(equiv_tests)))
  }
  equiv_tests$const = rep(1, nrow(equiv_tests))
  equiv_tests = equiv_tests[nrow(equiv_tests):1, ]
  
  g = ggplot(equiv_tests, aes(x = names))
  print(length(unique(equiv_tests$tol)))
  g = g + geom_linerange(aes(ymin = -1 * inverted, ymax = inverted), 
                         size = 5, color = "darkgray", alpha = 0.9)
  if(length(unique(equiv_tests$tol)) > 1) {
    g = g + geom_linerange(aes(ymin = -1 * tol, ymax = tol), 
                           size = 10, color = "gray")   
  } else {
    print("here")
    print(unique(equiv_tests$tol))
    lines = unique(equiv_tests$tol)
    g = g + geom_hline(yintercept = c(-1 * lines, lines), 
                       linetype = 2, size = 0.75)
  }
  g = g + scale_shape_identity() + 
    geom_point(aes(y = observed.smd), color = "black", shape = 18, size = 4)
  g = (g + coord_flip() + theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14)) + 
      labs(x = NULL, 
           y = paste("Equivalence Range (in pooled SD)"), 
           font = 5) + 
      theme_bw()
  )
  if(length(unique(equiv_tests$tol)) == 1) {
    g = g + ggtitle(paste0("Equivalence Tests  \n \n", "Equivalence Range:\n+/- ", 
                           round(unique(equiv_tests$tol), 2), " pooled SD"))
  } else {
    g = g + ggtitle("Equivalence Tests  \n \n \n")
  }
  
  g_inv = ggplot(equiv_tests, aes(x = names, y = const, 
                                  label = round(inverted.scaled, var.rounding)), 
                 environment = .e)
  g_inv = g_inv + geom_text()
  g_inv = (g_inv + theme_bw() + coord_flip() + 
             theme(panel.grid.minor = element_blank(), 
                   panel.grid.major=element_blank(), 
                   axis.text.x = element_text(color = "white", size = 14), 
                   axis.ticks.x = element_line(color = "white"), 
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank(), 
                   axis.title.x = element_text(size = 14)) +
             ylim(1 - .05, 1.05) +
             ggtitle("Equivalence\nConfidence\nInterval (+/-)\n(Scale of Var)") +
             labs(y = " ", x = NULL)
  )
  
  g_obs = ggplot(equiv_tests, aes(x = names, y = const, 
                                  label = round(observed.diff, var.rounding)), 
                 environment = .e)
  g_obs = g_obs + geom_text()
  g_obs = (g_obs + theme_bw() + coord_flip() + 
             theme(panel.grid.minor = element_blank(), 
                   panel.grid.major = element_blank(), 
                   axis.text.x = element_text(color = "white", size = 14), 
                   axis.ticks.x = element_line(color = "white"), 
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank(), 
                   axis.title.x = element_text(size = 14)) + 
             ylim(1 - .05, 1.05) + 
             ggtitle("Observed\nMean\nDifference\n(Scale of Var)") + 
             labs(y = " ", x = NULL)
  )
  
  g_pval = ggplot(equiv_tests, aes(x = names, y = const, 
                                   label = round(p.vals, pval.rounding)), 
                  environment = .e)
  g_pval = g_pval + geom_text()
  g_pval = (g_pval + theme_bw() + coord_flip() + 
              theme(panel.grid.minor = element_blank(), 
                    panel.grid.major = element_blank(), 
                    axis.text.x = element_text(color = "white", size = 14), 
                    axis.ticks.x = element_line(color = "white"), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(), 
                    axis.title.x = element_text(size = 14)) + 
              ylim(1 - .05, 1.05) + 
              ggtitle(ifelse(fdr_correct, "\nFDR\nCorrected\nP-value", 
                             "\n\n\nP-value")) + 
              labs(y = " ", x = NULL)
  )
  
  g_var = ggplot(equiv_tests, aes(x = names, y = const, label = names))
  g_var = g_var + geom_text()
  g_var = (g_var + theme_bw() + coord_flip() + 
             theme(panel.grid.minor = element_blank(), 
                   panel.grid.major=element_blank(), 
                   axis.text.x = element_text(color = "white", size = 14), 
                   axis.ticks.x = element_line(color = "white"), 
                   axis.text.y = element_blank(), 
                   axis.ticks.y = element_blank(), 
                   panel.border = element_blank(), 
                   axis.title.x = element_text(size = 14)) + 
             ylim(1-.05, 1.05) + 
             ggtitle("\n \n \nVariable") + 
             theme(plot.title = element_text(hjust = 0.5)) + 
             labs(y = " ", x = NULL)
  )
  
  grid.arrange(g_var, g_obs, g, g_inv, g_pval, ncol = 5, nrow = 1, 
               widths = panel.widths, heights = c(4))
}

## Equivalence tests between No Transgressions and Increasing Severity conditions
df <- df %>% mutate(group01 = case_when(
  exp_1 == 4 ~ 0,
  exp_1 == 1 ~ 1,
  exp_1 == 2 | exp_1 == 3 ~ -99
))
df_group01 <- subset(df, group01 == 0 | group01 == 1)
replicate_strict <- as.data.frame(
  run_equiv(X = subset(df_group01, 
                       select = c("age", "male", "white", "college", "dem", "gop", "ideo")), 
            Tr = df_group01$group01, epsilon.method = "custom", custom.epsilon = 0.36,
            fdr_correct = TRUE)
)
p <- generate_plot(replicate_strict, panel.widths = c(1.2, 1.2, 4, 1.2, 1.2), 
                   display.names = c("Age", "Male", "White", "College",
                                     "Democrat", "Republican", "Ideology"), 
                   pval.rounding = 3, fdr_correct = TRUE)

ggsave(file = "Figure S2.pdf", p, width = 9, height = 5)

## Equivalence tests between No Transgressions and Decreasing Severity conditions
df <- df %>% mutate(group02 = case_when(
  exp_1 == 4 ~ 0,
  exp_1 == 2 ~ 1,
  exp_1 == 1 | exp_1 == 3 ~ -99
))
df_group02 <- subset(df, group02 == 0 | group02 == 1)
replicate_strict <- as.data.frame(
  run_equiv(X = subset(df_group02, 
                       select = c("age", "male", "white", "college", "dem", "gop", "ideo")), 
            Tr = df_group02$group02, epsilon.method = "custom", custom.epsilon = 0.36,
            fdr_correct = TRUE)
)
p <- generate_plot(replicate_strict, panel.widths = c(1.2, 1.2, 4, 1.2, 1.2), 
                   display.names = c("Age", "Male", "White", "College",
                                     "Democrat", "Republican", "Ideology"), 
                   pval.rounding = 3, fdr_correct = TRUE)

ggsave(file = "Figure S3.pdf", p, width = 9, height = 5)

## Equivalence tests between No Transgressions and Sporadic Severity conditions
df <- df %>% mutate(group03 = case_when(
  exp_1 == 4 ~ 0,
  exp_1 == 3 ~ 1,
  exp_1 == 1 | exp_1 == 2 ~ -99
))
df_group03 <- subset(df, group03 == 0 | group03 == 1)
replicate_strict <- as.data.frame(
  run_equiv(X = subset(df_group03, 
                       select = c("age", "male", "white", "college", "dem", "gop", "ideo")), 
            Tr = df_group03$group03, epsilon.method = "custom", custom.epsilon = 0.36,
            fdr_correct = TRUE)
)
p <- generate_plot(replicate_strict, panel.widths = c(1.2, 1.2, 4, 1.2, 1.2), 
                   display.names = c("Age", "Male", "White", "College",
                                     "Democrat", "Republican", "Ideology"), 
                   pval.rounding = 3, fdr_correct = TRUE)

ggsave(file = "Figure S4.pdf", p, width = 9, height = 5)
