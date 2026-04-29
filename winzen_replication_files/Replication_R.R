# R script 1: This is the first of 3 R scripts replicating the analyses reported in:

#Winzen, Thomas. 2023. “How Backsliding Governments Keep the European Union Hospitable for Autocracy: 
#Evidence from Intergovernmental Negotiations.” The Review of International Organizations.

#Please consult the instructions on the replication materials for the required packages and further information. 
#Please do ensure to install the required packages correctly before running this script.



rm(list=ls())

setwd("")

library(rethinking)
library(rstan)
library(dplyr)
library(Hmisc)
library(shinystan)



rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


##########################################################################################
#Load data START
##########################################################################################

myvars1<- c("proeu", "proeubin", "post2010", "post2015", "backslider",
            "sensitive", "vdem_backslider", "yintro", "jha", "councilid",
            "prnrnmc", "ctrid", "east2004", "north", "centralwest", "south", "vdem_ctrs", 
            "ctr", "backnarrow", "hu", "pl", 
            "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
            "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south")

myvars2<- c("proeu", "proeubin", "post2010", "post2015", "backslider",
            "sensitive", "vdem_backslider", "yintro", "jha", "councilid",
            "prnrnmc", "ctrid", "east2004", "north", "centralwest", "south", "gov_eupos", "goveu01", 
            "pl", "hu", "vdem_ctrs", "ctr", "backnarrow", 
            "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
            "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south")

myvars4<- c("proeu", "proeubin", "proeubin_nomiddle", "post2010", "post2015", "backslider",
            "sensitive", "vdem_backslider", "yintro", "jha", "councilid",
            "prnrnmc", "ctrid", "east2004", "north", "centralwest", "south", "gov_eupos", "goveu01", 
            "pl", "hu", "vdem_ctrs", "ctr", "backnarrow", 
            "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
            "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south")


data<-read.csv("analysisR.csv")

#Narrow backsliding variable
attach(data)
data$backnarrow<- ifelse((ctr=="pl" & yintro>=2015) | (ctr=="hu" & yintro>=2010), 1, 0)
data$backnarrow[ctr=="hu" & yintro>=2010]
data$backnarrow[ctr=="hu"]
data$backnarrow[ctr=="pl" & yintro>=2015]
data$backnarrow[ctr=="pl"]
detach(data)

#smaller and larger datasets
data1<- data[myvars1]
data1<- data1[complete.cases(data1),]

data2<- data[myvars2]
data2<- data2[complete.cases(data2),]

data4<- data[myvars4]
data4<- data4[complete.cases(data4),]


#Identifiers without gaps (needed for stan)
data1 <- data1 %>%
  group_by(ctrid) %>%
  dplyr::mutate(ctrn = cur_group_id())

data1 <- data1 %>%
  group_by(prnrnmc) %>%
  dplyr::mutate(proposaln = cur_group_id())

data1 <- data1 %>%
  group_by(councilid) %>%
  dplyr::mutate(counciln = cur_group_id())

data1 <- data1 %>%
  group_by(yintro) %>%
  dplyr::mutate(yearn = cur_group_id())


data2 <- data2 %>%
  group_by(ctrid) %>%
  dplyr::mutate(ctrn = cur_group_id())

data2 <- data2 %>%
  group_by(prnrnmc) %>%
  dplyr::mutate(proposaln = cur_group_id())

data2 <- data2 %>%
  group_by(councilid) %>%
  dplyr::mutate(counciln = cur_group_id())

data2 <- data2 %>%
  group_by(yintro) %>%
  dplyr::mutate(yearn = cur_group_id())



data4 <- data4 %>%
  group_by(ctrid) %>%
  dplyr::mutate(ctrn = cur_group_id())

data4 <- data4 %>%
  group_by(prnrnmc) %>%
  dplyr::mutate(proposaln = cur_group_id())

data4 <- data4 %>%
  group_by(councilid) %>%
  dplyr::mutate(counciln = cur_group_id())

data4 <- data4 %>%
  group_by(yintro) %>%
  dplyr::mutate(yearn = cur_group_id())

#standardized government EU position
data2$goveustd <- (data2$gov_eupos - mean(data2$gov_eupos)) / sd(data2$gov_eupos)
data4$goveustd <- (data4$gov_eupos - mean(data4$gov_eupos)) / sd(data4$gov_eupos)

#Chain input
iterations <- 8000
warm <- 4000
chain_n <- 4
core_n <- 4

##########################################################################################
#Load data END
##########################################################################################
















##########################################################################################
#Figure 1a, model 1 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk")

d <- data1[modelvars]
yobs <- d$proeubin


ml1 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_back * vdem_backslider + b_inhibit * sensitive + b_interact * vdem_backslider * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_ml1.RData")
rm("ml1")


##########################################################################################
#Figure 1a, model 1 END
##########################################################################################







##########################################################################################
#Figure 1b, model 1 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "backnarrow")
d <- data1[modelvars]
yobs <- d$proeubin


ml1narrow <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_back * backnarrow + b_inhibit * sensitive + b_interact * backnarrow * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_ml1narrow.RData")
rm("ml1narrow")




##########################################################################################
#Figure 1b, model 1 END
##########################################################################################










##########################################################################################
#Figure 1a, model 2 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- data2[modelvars]
yobs <- d$proeubin


ml2 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_back * vdem_backslider + b_inhibit * sensitive + b_interact * vdem_backslider * sensitive +
      b_time * yearn + b_goveu * goveustd + b_jha * jha,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact, b_time, b_goveu, b_jha) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)



save.image("results_ml2.RData")
rm("ml2")




##########################################################################################
#Figure 1a, model 2 END
##########################################################################################






##########################################################################################
#Figure 1b, model 2 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "backnarrow")
d <- data2[modelvars]
yobs <- d$proeubin





ml2narrow <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_back * backnarrow + b_inhibit * sensitive + b_interact * backnarrow * sensitive +
      b_time * yearn + b_goveu * goveustd + b_jha * jha,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact, b_time, b_goveu, b_jha) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_ml2narrow.RData")
rm("ml2narrow")



##########################################################################################
#Figure 1b, model 2 END
##########################################################################################









##########################################################################################
#Figure 1a, model 3 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "pl", "hr", "cz", "el", "si", "hu", "vdem_ctrs")
d <- data2[modelvars]
d <- subset(d, vdem_ctrs==1)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())



a <- rnorm(1000, 0, 2)
dens(a)

ml3 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_prob * a_prop[proposaln] + 
      b_back * vdem_backslider + b_inhibit * sensitive + b_interact * vdem_backslider * sensitive +
      b_pl * pl + b_hr * hr + b_cz * cz + b_el * el + b_si * si,
    a ~ dnorm(0, 2),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact) ~ dnorm(0, 2),
    c(b_pl, b_hr, b_cz, b_el, b_si) ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)



save.image("results_ml3.RData")
rm("ml3")

##########################################################################################
#Figure 1a, model 3 END
##########################################################################################









##########################################################################################
#Figure 1b, model 3 START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "pl", "hr", "cz", "el", "si", "hu", "vdem_ctrs", "backnarrow")
d <- data2[modelvars]
d<- subset(d, (pl==1 | hu==1))
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())




ml3narrow <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_prob * a_prop[proposaln] + 
      b_back * backnarrow + b_inhibit * sensitive + b_interact * backnarrow * sensitive +
      b_pl * pl,
    a ~ dnorm(0, 2),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact) ~ dnorm(0, 2),
    c(b_pl) ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_ml3narrow.RData")
rm("ml3narrow")


##########################################################################################
#Figure 1b, model 3 END
##########################################################################################




##########################################################################################
#Figure A2, CWE START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((centralwest==1 & yintro>=2015), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo1 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_placebo1.RData")
rm("placebo1")

##########################################################################################
#Figure A2, CWE END
##########################################################################################






##########################################################################################
#Figure A2, North START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((north==1 & yintro>=2015), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo2 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_placebo2.RData")
rm("placebo2")


##########################################################################################
#Figure A2, North END
##########################################################################################








##########################################################################################
#Figure A2, East START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse(((east2004==1 | bg==1 | ro==1) & yintro>=2015), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo3 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_placebo3.RData")
rm("placebo3")

##########################################################################################
#Figure A2, East END
##########################################################################################




##########################################################################################
#Figure A2, South START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((south==1 & yintro>=2015), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo4 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_placebo4.RData")
rm("placebo4")


##########################################################################################
#Figure A2, South END
##########################################################################################











##########################################################################################
#Figure A1, CWE START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((centralwest==1 & yintro>=2010), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo1_2010 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)


save.image("results_placebo1_2010.RData")
rm("placebo1_2010")


##########################################################################################
#Figure A1, CWE END
##########################################################################################






##########################################################################################
#Figure A1, North START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((north==1 & yintro>=2010), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo2_2010 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_placebo2_2010.RData")
rm("placebo2_2010")

##########################################################################################
#Figure A1, North END
##########################################################################################








##########################################################################################
#Figure A1, East START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse(((bg==1 | ro==1 | hr==1 | cz==1 | ee==1 | lt==1 | lv==1 | si==1 | sk==1) & yintro>=2010), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo3_2010 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_placebo3_2010.RData")
rm("placebo3_2010")

##########################################################################################
#Figure A1, East END
##########################################################################################




##########################################################################################
#Figure A1, South START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "pl", "hu", "at", "be", "bg", "hr", "cy", "cz", "de", "dk", "ee", "el", "es", "fi", "fr", "ie",
              "it", "lt", "lu", "lv", "mt", "nl", "pt", "ro", "se", "si", "sk", "uk", "east2004", "north", "centralwest", "south", 
              "yintro", "vdem_ctrs")

d <- data1[modelvars]
attach(d)
d$backplacebo <- ifelse((south==1 & yintro>=2010), 1, 0)
detach(d)
d <- subset(d, vdem_ctrs==0)

unique(d$si)
yobs <- d$proeubin


d <- d %>%
  group_by(ctrn) %>%
  dplyr::mutate(ctrn = cur_group_id())

d <- d %>%
  group_by(proposaln) %>%
  dplyr::mutate(proposaln = cur_group_id())

d <- d %>%
  group_by(counciln) %>%
  dplyr::mutate(counciln = cur_group_id())

d <- d %>%
  group_by(yearn) %>%
  dplyr::mutate(yearn = cur_group_id())





placebo4_2010 <- ulam(
  alist(
    proeubin ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_backplacebo * backplacebo + b_inhibit * sensitive + b_interact * backplacebo * sensitive +
      b_time * yearn,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_backplacebo, b_inhibit, b_interact, b_time) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_placebo4_2010.RData")
rm("placebo4_2010")

##########################################################################################
#Figure A1, South END
##########################################################################################





##########################################################################################
#Figure 1a, model 2 - but excluding middle ('50') observations from the dichotomous DV START
##########################################################################################
modelvars<- c("proeu", "proeubin", "proeubin_nomiddle", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- data4[modelvars]
yobs <- d$proeubin_nomiddle


ml2_nomiddle <- ulam(
  alist(
    proeubin_nomiddle ~ dbinom(1, p),
    logit(p) <- a + sigma_ctr * a_ctr[ctrn] + sigma_prob * a_prop[proposaln] + 
      b_back * vdem_backslider + b_inhibit * sensitive + b_interact * vdem_backslider * sensitive +
      b_time * yearn + b_goveu * goveustd + b_jha * jha,
    a ~ dnorm(0, 2),
    a_ctr[ctrn] ~ dnorm(0, 1),
    a_prop[proposaln] ~ dnorm(0, 1),
    c(b_back, b_inhibit, b_interact, b_time, b_goveu, b_jha) ~ dnorm(0, 2),
    sigma_ctr ~ dnorm(0, 2),
    sigma_prob ~ dnorm(0, 2)
  ),
  data=d,
  iter=iterations,
  warmup = warm,
  chains=chain_n,
  cores =core_n,
  constraints=list(sigma="lower=0", sigma_ctr="lower=0", sigma_prob="lower=0"),
  control = list(adapt_delta = 0.95),
  cmdstan=FALSE
)

save.image("results_ml2_nomiddle.RData")
rm("ml2_nomiddle")



##########################################################################################
#Figure 1a, model 2 - but excluding middle ('50') observations from the dichotomous DV END
##########################################################################################






##########################################################################################
##########################################################################################
#Postestimation START
##########################################################################################
##########################################################################################






##########################################################################################
#Load results START
##########################################################################################


load("results_ml1.RData")
load("results_ml2.RData")
load("results_ml3.RData")
load("results_ml2_nomiddle.RData")

load("results_ml1narrow.RData")
load("results_ml2narrow.RData")
load("results_ml3narrow.RData")


##########################################################################################
#Load results END
##########################################################################################






##########################################################################################
#Tables A1 START
##########################################################################################

write.csv(precis(ml2, prob=.90), file="TA1_1.csv")
write.csv(precis(ml3, prob=.90), file="TA1_3.csv")

write.csv(precis(ml2narrow, prob=.90), file="TA1_2.csv")
write.csv(precis(ml3narrow, prob=.90), file="TA1_4.csv")

##########################################################################################
#Table A1 END
##########################################################################################





##########################################################################################
#Dot charts START
##########################################################################################
coefwidth <- 6
coefheight <- 10
coefresolution <- 300




##########################################################################################
#Results for Model 2 (Figure 1a) with and without 50 observations in DV 
#(not shown in the manuscript) START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(ml2, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(ml2_nomiddle, prob = hpdi)
m1b <- precis(ml2, prob = hpdi2)
m2b <- precis(ml2_nomiddle, prob = hpdi2)
modellabels <- c("M2 (fewer obs)", "M2 (original)")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("a","b_back", "b_inhibit","b_interact", "b_time", "b_goveu", "b_jha", "sigma_ctr", "sigma_prob")     #which parameters should be compared?
paralabels <- c("Constant", "Backsliding", "Inhibiting competence", "Backsliding*Inhibiting", "Year", "Gov't EU position", "JHA", "SD country", "SD proposal")      #or set equal to pars
nparameters <- length(pars)

#coefficients in matrix form and labels of parameters and models
coefs <- c(m2[pars,]$mean, m1[pars,]$mean)     #add more models here
coefs <- matrix(coefs, ncol=nparameters, nrow=nmodels, byrow=TRUE)
colnames(coefs) <- paralabels
rownames(coefs) <- modellabels



#lower HPDI boundaries
lower <- c(m2[pars,]$`2.5%`, m1[pars,]$`2.5%`)     #add more models here
lower <- matrix(lower, ncol=nparameters, nrow=nmodels, byrow=TRUE)

lower2<- c(m2b[pars,]$`5%`, m1b[pars,]$`5%`)     #add more models here
lower2 <- matrix(lower2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#upper HPDI boundaries
upper <- c(m2[pars,]$`97.5%`, m1[pars,]$`97.5%`)     #add more models here
upper <- matrix(upper, ncol=nparameters, nrow=nmodels, byrow=TRUE)

upper2 <- c(m2b[pars,]$`95%`, m1b[pars,]$`95%`)     #add more models here
upper2 <- matrix(upper2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#coefs <- t(coefs)   #only if you want to order by model (over several parameters)
#lower <- t(lower) #only if you want to order by model (over several parameters)
#upper <- t(upper) #only if you want to order by model (over several parameters)

left <- lower
right <- upper
left2 <- lower2
right2 <- upper2
llim <- min(left, na.rm = TRUE)        #x-axis limit (left)
rlim <- max(right, na.rm = TRUE)       #x-axis limit (right)
xlabel <- "Parameter estimate"

llim <- -3       #x-axis limit (left)
rlim <- 3       #x-axis limit (right)



par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}




dev.off()
png("dot_M2withwithoutmiddle.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}
dev.off()

##########################################################################################
#Results for Model 2 (Figure 1a) with and without 50 observations in DV 
#(not shown in the manuscript) END
##########################################################################################














##########################################################################################
#Figure 1a START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(ml1, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(ml2, prob = hpdi)
m3 <- precis(ml3, prob = hpdi)
m1b <- precis(ml1, prob = hpdi2)
m2b <- precis(ml2, prob = hpdi2)
m3b <- precis(ml3, prob = hpdi2)
modellabels <- c("M3","M2", "M1")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("a","b_back", "b_inhibit","b_interact", "b_time", "b_goveu", "b_jha", "sigma_ctr", "sigma_prob")     #which parameters should be compared?
paralabels <- c("Constant", "Backsliding", "Inhibiting competence", "Backsliding*Inhibiting", "Year", "Gov't EU position", "JHA", "SD country", "SD proposal")      #or set equal to pars
nparameters <- length(pars)

#coefficients in matrix form and labels of parameters and models
coefs <- c(m3[pars,]$mean, m2[pars,]$mean, m1[pars,]$mean)     #add more models here
coefs <- matrix(coefs, ncol=nparameters, nrow=nmodels, byrow=TRUE)
colnames(coefs) <- paralabels
rownames(coefs) <- modellabels



#lower HPDI boundaries
lower <- c(m3[pars,]$`2.5%`, m2[pars,]$`2.5%`, m1[pars,]$`2.5%`)     #add more models here
lower <- matrix(lower, ncol=nparameters, nrow=nmodels, byrow=TRUE)

lower2<- c(m3b[pars,]$`5%`, m2b[pars,]$`5%`, m1b[pars,]$`5%`)     #add more models here
lower2 <- matrix(lower2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#upper HPDI boundaries
upper <- c(m3[pars,]$`97.5%`, m2[pars,]$`97.5%`, m1[pars,]$`97.5%`)     #add more models here
upper <- matrix(upper, ncol=nparameters, nrow=nmodels, byrow=TRUE)

upper2 <- c(m3b[pars,]$`95%`, m2b[pars,]$`95%`, m1b[pars,]$`95%`)     #add more models here
upper2 <- matrix(upper2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#coefs <- t(coefs)   #only if you want to order by model (over several parameters)
#lower <- t(lower) #only if you want to order by model (over several parameters)
#upper <- t(upper) #only if you want to order by model (over several parameters)

left <- lower
right <- upper
left2 <- lower2
right2 <- upper2
llim <- min(left, na.rm = TRUE)        #x-axis limit (left)
rlim <- max(right, na.rm = TRUE)       #x-axis limit (right)
xlabel <- "Parameter estimate"

llim <- -3       #x-axis limit (left)
rlim <- 3       #x-axis limit (right)



par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}




dev.off()
png("F1a_dot_M1to3long.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}
dev.off()

##########################################################################################
#Figure 1a END
##########################################################################################















##########################################################################################
#Figure 1b START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(ml1narrow, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(ml2narrow, prob = hpdi)
m3 <- precis(ml3narrow, prob = hpdi)
m1b <- precis(ml1narrow, prob = hpdi2)
m2b <- precis(ml2narrow, prob = hpdi2)
m3b <- precis(ml3narrow, prob = hpdi2)
modellabels <- c("M3","M2", "M1")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("a","b_back", "b_inhibit","b_interact", "b_time", "b_goveu", "b_jha", "sigma_ctr", "sigma_prob")     #which parameters should be compared?
paralabels <- c("Constant", "Backsliding", "Inhibiting competence", "Backsliding*Inhibiting", "Year", "Gov't EU position", "JHA", "SD country", "SD proposal")      #or set equal to pars
nparameters <- length(pars)

#coefficients in matrix form and labels of parameters and models
coefs <- c(m3[pars,]$mean, m2[pars,]$mean, m1[pars,]$mean)     #add more models here
coefs <- matrix(coefs, ncol=nparameters, nrow=nmodels, byrow=TRUE)
colnames(coefs) <- paralabels
rownames(coefs) <- modellabels



#lower HPDI boundaries
lower <- c(m3[pars,]$`2.5%`, m2[pars,]$`2.5%`, m1[pars,]$`2.5%`)     #add more models here
lower <- matrix(lower, ncol=nparameters, nrow=nmodels, byrow=TRUE)

lower2<- c(m3b[pars,]$`5%`, m2b[pars,]$`5%`, m1b[pars,]$`5%`)     #add more models here
lower2 <- matrix(lower2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#upper HPDI boundaries
upper <- c(m3[pars,]$`97.5%`, m2[pars,]$`97.5%`, m1[pars,]$`97.5%`)     #add more models here
upper <- matrix(upper, ncol=nparameters, nrow=nmodels, byrow=TRUE)

upper2 <- c(m3b[pars,]$`95%`, m2b[pars,]$`95%`, m1b[pars,]$`95%`)     #add more models here
upper2 <- matrix(upper2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#coefs <- t(coefs)   #only if you want to order by model (over several parameters)
#lower <- t(lower) #only if you want to order by model (over several parameters)
#upper <- t(upper) #only if you want to order by model (over several parameters)

left <- lower
right <- upper
left2 <- lower2
right2 <- upper2
llim <- min(left, na.rm = TRUE)        #x-axis limit (left)
rlim <- max(right, na.rm = TRUE)       #x-axis limit (right)
xlabel <- "Parameter estimate"

llim <- -3       #x-axis limit (left)
rlim <- 3       #x-axis limit (right)



par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}




dev.off()
png("F1b_dot_M1to3narrowlong.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}
dev.off()

##########################################################################################
#Figure 1b END
##########################################################################################




rm("ml1narrow", "ml2narrow", "ml3narrow", "ml1", "ml2", "ml3")

load("results_placebo1.RData")
load("results_placebo2.RData")
load("results_placebo3.RData")
load("results_placebo4.RData")

load("results_placebo1_2010.RData")
load("results_placebo2_2010.RData")
load("results_placebo3_2010.RData")
load("results_placebo4_2010.RData")






##########################################################################################
##Figure A1 START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(placebo1_2010, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(placebo2_2010, prob = hpdi)
m3 <- precis(placebo3_2010, prob = hpdi)
m4 <- precis(placebo4_2010, prob = hpdi)
m1b <- precis(placebo1_2010, prob = hpdi2)
m2b <- precis(placebo2_2010, prob = hpdi2)
m3b <- precis(placebo3_2010, prob = hpdi2)
m4b <- precis(placebo4_2010, prob = hpdi2)
modellabels <- c("South","East","North", "CWE")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("b_back", "b_inhibit","b_interact")     #which parameters should be compared?
paralabels <- c("Backsl. (placebo)", "Inhibiting competence", "Backsliding*Inhibiting")      #or set equal to pars
nparameters <- length(pars)

#coefficients in matrix form and labels of parameters and models
coefs <- c(m4[pars,]$mean, m3[pars,]$mean, m2[pars,]$mean, m1[pars,]$mean)     #add more models here
coefs <- matrix(coefs, ncol=nparameters, nrow=nmodels, byrow=TRUE)
colnames(coefs) <- paralabels
rownames(coefs) <- modellabels



#lower HPDI boundaries
lower <- c(m4[pars,]$`2.5%`, m3[pars,]$`2.5%`, m2[pars,]$`2.5%`, m1[pars,]$`2.5%`)     #add more models here
lower <- matrix(lower, ncol=nparameters, nrow=nmodels, byrow=TRUE)

lower2<- c(m4b[pars,]$`5%`, m3b[pars,]$`5%`, m2b[pars,]$`5%`, m1b[pars,]$`5%`)     #add more models here
lower2 <- matrix(lower2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#upper HPDI boundaries
upper <- c(m4[pars,]$`97.5%`, m3[pars,]$`97.5%`, m2[pars,]$`97.5%`, m1[pars,]$`97.5%`)     #add more models here
upper <- matrix(upper, ncol=nparameters, nrow=nmodels, byrow=TRUE)

upper2 <- c(m4b[pars,]$`95%`, m3b[pars,]$`95%`, m2b[pars,]$`95%`, m1b[pars,]$`95%`)     #add more models here
upper2 <- matrix(upper2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#coefs <- t(coefs)   #only if you want to order by model (over several parameters)
#lower <- t(lower) #only if you want to order by model (over several parameters)
#upper <- t(upper) #only if you want to order by model (over several parameters)

left <- lower
right <- upper
left2 <- lower2
right2 <- upper2
llim <- min(left, na.rm = TRUE)        #x-axis limit (left)
rlim <- max(right, na.rm = TRUE)       #x-axis limit (right)
xlabel <- "Parameter estimate"

llim <- -3       #x-axis limit (left)
rlim <- 3       #x-axis limit (right)



par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}




dev.off()
png("FA1_dot_placebo2010.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}
dev.off()

##########################################################################################
#Figure A1 END
##########################################################################################












##########################################################################################
##Figure A2 START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(placebo1, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(placebo2, prob = hpdi)
m3 <- precis(placebo3, prob = hpdi)
m4 <- precis(placebo4, prob = hpdi)
m1b <- precis(placebo1, prob = hpdi2)
m2b <- precis(placebo2, prob = hpdi2)
m3b <- precis(placebo3, prob = hpdi2)
m4b <- precis(placebo4, prob = hpdi2)
modellabels <- c("South","East","North", "CWE")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("b_back", "b_inhibit","b_interact")     #which parameters should be compared?
paralabels <- c("Backsl. (placebo)", "Inhibiting competence", "Backsliding*Inhibiting")      #or set equal to pars
nparameters <- length(pars)

#coefficients in matrix form and labels of parameters and models
coefs <- c(m4[pars,]$mean, m3[pars,]$mean, m2[pars,]$mean, m1[pars,]$mean)     #add more models here
coefs <- matrix(coefs, ncol=nparameters, nrow=nmodels, byrow=TRUE)
colnames(coefs) <- paralabels
rownames(coefs) <- modellabels



#lower HPDI boundaries
lower <- c(m4[pars,]$`2.5%`, m3[pars,]$`2.5%`, m2[pars,]$`2.5%`, m1[pars,]$`2.5%`)     #add more models here
lower <- matrix(lower, ncol=nparameters, nrow=nmodels, byrow=TRUE)

lower2<- c(m4b[pars,]$`5%`, m3b[pars,]$`5%`, m2b[pars,]$`5%`, m1b[pars,]$`5%`)     #add more models here
lower2 <- matrix(lower2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#upper HPDI boundaries
upper <- c(m4[pars,]$`97.5%`, m3[pars,]$`97.5%`, m2[pars,]$`97.5%`, m1[pars,]$`97.5%`)     #add more models here
upper <- matrix(upper, ncol=nparameters, nrow=nmodels, byrow=TRUE)

upper2 <- c(m4b[pars,]$`95%`, m3b[pars,]$`95%`, m2b[pars,]$`95%`, m1b[pars,]$`95%`)     #add more models here
upper2 <- matrix(upper2, ncol=nparameters, nrow=nmodels, byrow=TRUE)


#coefs <- t(coefs)   #only if you want to order by model (over several parameters)
#lower <- t(lower) #only if you want to order by model (over several parameters)
#upper <- t(upper) #only if you want to order by model (over several parameters)

left <- lower
right <- upper
left2 <- lower2
right2 <- upper2
llim <- min(left, na.rm = TRUE)        #x-axis limit (left)
rlim <- max(right, na.rm = TRUE)       #x-axis limit (right)
xlabel <- "Parameter estimate"


llim <- -3       #x-axis limit (left)
rlim <- 3       #x-axis limit (right)


par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}




dev.off()
png("FA2_dot_placebo2015.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
par(cex=1.3, mar=c(4, 2, 1, 1),mgp=c(2, 1, 0))
dotchart(coefs, xlab =xlabel, xlim = c(llim, rlim), labels = modellabels, bg = col.alpha("red", 1))
abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
for (k in 1:nrow(coefs)) {
  for (m in 1:ncol(coefs)) {
    if (!is.na(left[k, m])) {
      kn <- nrow(coefs)
      ytop <- ncol(coefs) * (kn + 2) - 1
      ypos <- ytop - (m - 1) * (kn + 2) - (kn - k + 1)
      lines(c(left[k, m], right[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 0.6))
      lines(c(left2[k, m], right2[k, m]), c(ypos, ypos), 
            lwd = 2, col = col.alpha("blue", 1))
    }}}
dev.off()

##########################################################################################
#Figure A2 END
##########################################################################################





rm("placebo1", "placebo2", "placebo3", "placebo4", "placebo1_2010", "placebo2_2010", "placebo3_2010", "placebo4_2010")






##########################################################################################
##########################################################################################
#Predicted probabilities START
##########################################################################################
##########################################################################################


load("results_ml1.RData")
load("results_ml2.RData")
load("results_ml3.RData")

load("results_ml1narrow.RData")
load("results_ml2narrow.RData")
load("results_ml3narrow.RData")




##########################################################################################
#Settings START
##########################################################################################

#y axis upper boundary in the plots
ylimupper <- 1

#draws from posterior
draws <- ((iterations-warm)*chain_n)

#data and model
d <- data1
model <- ml1

#baseline country, timepoint, area effects (set 0 for assuming no deviation from constant term)
a <- precis(model, prob = .95)
a <- a["a",]$mean
base_ctr <- 0
base_prop <- 0
inv_logit(a + (base_prop+base_ctr))   #This calculates the approximate baseline probability underlying the predicted probabilities, given the chosen model

#Figure dimensions
fwidth <- 6
fheight <- 5

##########################################################################################
#Settings END
##########################################################################################





##########################################################################################
#Figure 2a
#Predicted probabilities, backslider, no backsliding-inhibiting comp. (based on model set in settings) START
##########################################################################################

n_ctrs <- max(d$ctrn)
n_props <- max(d$proposaln)

draws <- draws  #draws from posterior
ctrs <- n_ctrs      
props <- n_props 

x <- d$vdem_backslider
y <- d$proeubin

summary(x)
dens(x)


#prediction data
r.seq <- seq(from=min(x), to=max(x), length.out = 2)


backslidervalue <- if(identical(x,d$vdem_backslider)) r.seq else 0
sensitivevalue <- if(identical(x,d$sensitive)) r.seq else 0
yearvalue <- max(d$yearn)


d.pred <- list(
  vdem_backslider=backslidervalue,
  sensitive=c(sensitivevalue, sensitivevalue),
  yearn=rep(yearvalue,length(r.seq)),
  ctrn=rep(1,length(r.seq)),
  proposaln=rep(1,length(r.seq))
)

matrix_ctrs <- matrix(base_ctr, draws, ctrs)
matrix_props <- matrix(base_prop, draws, props)



#predictions

linksamples <- link(model, n=draws, data = d.pred,
                    replace=list(a_prop=matrix_props,
                                 a_ctr=matrix_ctrs))

linksamples.mean_0 <- apply(linksamples, 2, mean)
linksamples.median_0 <- apply(linksamples, 2, median)
linksamples.HPDI_0 <- apply(linksamples, 2, HPDI, prob=.95) 
linksamples.HPDI70_0 <- apply(linksamples, 2, HPDI, prob=.70)
linksamples.HPDI90_0 <- apply(linksamples, 2, HPDI, prob=.90)


median(linksamples[,2])
median(linksamples[,1])

linksamples
x1=0
x2=1
y1=0
y2=10

dev.off()
tiff("F2a_prob_back_inhibit0.png",width = fwidth, height = fheight, units = 'in',  res = 150)
par(cex=1.5, mar=c(4, 4, 1, 1), mgp=c(2, 1, 0))
maxi <- density(linksamples[,2])
mini <- density(linksamples[,1])
plot(maxi, ylim=c(y1, y2), xlim=c(x1, x2), main=NA, ylab="Posterior density", xlab="Probability: pro-EU position") # plots the results
par(new=TRUE, cex=1.5, mar=c(4, 4, 1, 1), mgp=c(2, 1, 0))
plot(mini, ylim=c(y1, y2), xlim=c(x1, x2), main=NA, ylab=NA, yaxt='n', xaxt='n', xlab=NA, lty="longdash")
text(.035, 3.5, "No backslider", pos = 4)
text(.55, 1.9, "Backslider", pos = 4)
dev.off()





##########################################################################################
#Figure 2a
#Predicted probabilities, backslider, no backsliding-inhibiting comp. (based on model set in settings) END
##########################################################################################





##########################################################################################
#Figure 2b
#Predicted probabilities, backslider, Yes backsliding-inhibiting comp. (based on model set in settings) START
##########################################################################################

n_ctrs <- max(d$ctrn)
n_props <- max(d$proposaln)

draws <- draws  #draws from posterior
ctrs <- n_ctrs      
props <- n_props 

x <- d$vdem_backslider
y <- d$proeubin

summary(x)
dens(x)


#prediction data
r.seq <- seq(from=min(x), to=max(x), length.out = 2)


backslidervalue <- if(identical(x,d$vdem_backslider)) r.seq else 0
sensitivevalue <- 1
yearvalue <- max(d$yearn)


d.pred <- list(
  vdem_backslider=backslidervalue,
  sensitive=c(sensitivevalue, sensitivevalue),
  yearn=rep(yearvalue,length(r.seq)),
  timepoint_n=rep(1,length(r.seq)),
  ctrn=rep(1,length(r.seq)),
  proposaln=rep(1,length(r.seq))
)


matrix_ctrs <- matrix(base_ctr, draws, ctrs)
matrix_props <- matrix(base_prop, draws, props)


#predictions
linksamples <- link(model, n=draws, data = d.pred,
                    replace=list(a_prop=matrix_props,
                                 a_ctr=matrix_ctrs))

linksamples.mean_0 <- apply(linksamples, 2, mean)
linksamples.median_0 <- apply(linksamples, 2, median)
linksamples.HPDI_0 <- apply(linksamples, 2, HPDI, prob=.95) 
linksamples.HPDI70_0 <- apply(linksamples, 2, HPDI, prob=.70)
linksamples.HPDI90_0 <- apply(linksamples, 2, HPDI, prob=.90)


median(linksamples[,2])
median(linksamples[,1])


x1=0
x2=1
y1=0
y2=10

dev.off()
tiff("F2b_prob_back_inhibit1.png",width = fwidth, height = fheight, units = 'in',  res = 150)
par(cex=1.5, mar=c(4, 4, 1, 1), mgp=c(2, 1, 0))
maxi <- density(linksamples[,2])
mini <- density(linksamples[,1])
plot(maxi, ylim=c(y1, y2), xlim=c(x1, x2), main=NA, ylab="Posterior density", xlab="Probability: pro-EU position") # plots the results
par(new=TRUE, cex=1.5, mar=c(4, 4, 1, 1), mgp=c(2, 1, 0))
plot(mini, ylim=c(y1, y2), xlim=c(x1, x2), main=NA, ylab=NA, yaxt='n', xaxt='n', xlab=NA, lty="longdash")
text(-.07, 7.1, "Backslider", pos = 4)
text(.30, 2.5, "No backslider", pos = 4)
dev.off()


##########################################################################################
#Figure 2b
#Predicted probabilities, backslider, Yes backsliding-inhibiting comp. (based on model set in settings) END
##########################################################################################


