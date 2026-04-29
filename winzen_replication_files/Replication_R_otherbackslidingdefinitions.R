# R script 3: This is the third of 3 R scripts replicating the analyses reported in:

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
#Figure A4, Model 1, with MT, BG, RO as additional backsliding countries START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "ctr", "yintro")
d <- data2[modelvars]
yobs <- d$proeubin

d$vdem_backslider <- ifelse((d$ctr=="mt" & d$yintro>=2010) | (d$ctr=="bu" & d$yintro>=2010) | (d$ctr=="ro" & d$yintro>=2010), 1, d$vdem_backslider)

d$vdem_backslider[d$ctr=="pl" & d$yintro>=2015]
d$vdem_backslider[d$ctr=="el" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="mt" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="bu" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="ro" & d$yintro>=2010]

modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- d[modelvars]
yobs <- d$proeubin



ml2plus3 <- ulam(
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

save.image("results_ml2plus3.RData")
rm("ml2plus3")



##########################################################################################
#Figure A4, Model 1, with MT, BG, RO as additional backsliding countries END
##########################################################################################






##########################################################################################
#Figure A4, Model 2, with EL not coded as backsliding country START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "ctr", "yintro")
d <- data2[modelvars]
yobs <- d$proeubin

d$vdem_backslider <- ifelse((d$ctr=="el"), 0, d$vdem_backslider)

d$vdem_backslider[d$ctr=="pl" & d$yintro>=2015]
d$vdem_backslider[d$ctr=="el"]
d$vdem_backslider[d$ctr=="mt" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="bu" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="ro" & d$yintro>=2010]

modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- d[modelvars]
yobs <- d$proeubin



ml2noEL <- ulam(
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

save.image("results_ml2noEL.RData")
rm("ml2noEL")



##########################################################################################
#Figure A4, Model 2, with EL not coded as backsliding country END
##########################################################################################






##########################################################################################
#Figure A4, Model 3, with EL not coded as backsliding country and MT included START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "ctr", "yintro")
d <- data2[modelvars]
yobs <- d$proeubin

d$vdem_backslider <- ifelse((d$ctr=="el"), 0, d$vdem_backslider)
d$vdem_backslider <- ifelse((d$ctr=="mt" & d$yintro>=2010), 1, d$vdem_backslider)

d$vdem_backslider[d$ctr=="pl" & d$yintro>=2015]
d$vdem_backslider[d$ctr=="el"]
d$vdem_backslider[d$ctr=="mt" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="bu" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="ro" & d$yintro>=2010]

modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- d[modelvars]
yobs <- d$proeubin



ml2noELMT <- ulam(
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

save.image("results_ml2noELMT.RData")
rm("ml2noELMT")


##########################################################################################
#Figure A4, Model 3, with EL not coded as backsliding country and MT included END
##########################################################################################







##########################################################################################
#Figure A4, Model 4, with EL, MT not coded as backsliding country and BG, RO included START
##########################################################################################
modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha", "ctr", "yintro")
d <- data2[modelvars]
yobs <- d$proeubin

d$vdem_backslider <- ifelse((d$ctr=="el"), 0, d$vdem_backslider)
d$vdem_backslider <- ifelse((d$ctr=="bu" & d$yintro>=2010) | (d$ctr=="ro" & d$yintro>=2010), 1, d$vdem_backslider)


d$vdem_backslider[d$ctr=="pl" & d$yintro>=2015]
d$vdem_backslider[d$ctr=="el"]
d$vdem_backslider[d$ctr=="mt" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="bu" & d$yintro>=2010]
d$vdem_backslider[d$ctr=="ro" & d$yintro>=2010]

modelvars<- c("proeu", "proeubin", "sensitive", "vdem_backslider", "yearn", "counciln",
              "proposaln", "ctrn", "goveustd", "jha")
d <- d[modelvars]
yobs <- d$proeubin



ml2noELBGRO <- ulam(
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

save.image("results_ml2noELBGRO.RData")
rm("ml2noELBGRO")




##########################################################################################
#Figure A4, Model 4, with EL, MT not coded as backsliding country and BG, RO included END
##########################################################################################










##########################################################################################
#Postestimation
##########################################################################################



##########################################################################################
#Load results START
##########################################################################################


load("results_ml2plus3.RData")
load("results_ml2noEL.RData")
load("results_ml2noELMT.RData")
load("results_ml2noELBGRO.RData")



##########################################################################################
#Load results END
##########################################################################################





##########################################################################################
#Dot charts START
##########################################################################################
coefwidth <- 6
coefheight <- 12
coefresolution <- 300





##########################################################################################
# Figure A4 Models with alternative backsliding countries (all parameters) START
##########################################################################################


#obtain results of models with desired HPDI, set labels for the models
hpdi=.95
hpdi2=.90
m1 <- precis(ml2plus3, prob = hpdi)     #results of all the models (map2stan objects)
m2 <- precis(ml2noEL, prob = hpdi)
m3 <- precis(ml2noELMT, prob = hpdi)
m4 <- precis(ml2noELBGRO, prob = hpdi)
m1b <- precis(ml2plus3, prob = hpdi2)
m2b <- precis(ml2noEL, prob = hpdi2)
m3b <- precis(ml2noELMT, prob = hpdi2)
m4b <- precis(ml2noELBGRO, prob = hpdi2)
modellabels <- c("4: VDem+BG,+RO,-EL","3: VDem+MT,-EL","2: VDem-EL", "1: VDem+BG,+RO,+MT")    #note reverse order (list highest model first)
nmodels <- length(modellabels)     #how many models will be compared?

#which parameters should be modelled and how should they be labelled
pars <- c("a","b_back", "b_inhibit","b_interact", "b_time", "b_goveu", "b_jha", "sigma_ctr", "sigma_prob")     #which parameters should be compared?
paralabels <- c("Constant", "Backsliding", "Inhibiting competence", "Backsliding*Inhibiting", "Year", "Gov't EU position", "JHA", "SD country", "SD proposal")      #or set equal to pars
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
png("FA4_dot_Malternativebacksliding.png", width = coefwidth, height = coefheight, units = 'in', res = coefresolution)
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
# Figure A4 Models with alternative backsliding countries END
##########################################################################################





