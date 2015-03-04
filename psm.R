###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 01/23/2015
# Code        : psm.R
# Description : Replication code for psm.R 
#              (Propensity Score Matching)
#--------------------------------------------------
###################################################

library(dplyr)
library(foreign)    # For converting *.dta
library(Matching)
library(MatchIt)
library(ggplot2)
library(survey)
library(nonrandom)

setwd("/home/john/Dropbox/UHM/Classes/Econ 610 - Economic Development/Problem Sets")

# Convert hh_98.dta to hh_98.csv with foreign package

yourData <- read.dta("Data/hh_98.dta")
write.csv(yourData, file = "Data/hh_98.csv")

# Load hh_98.csv into a data.frame

hh_98.df <- read.csv("Data/hh_98.csv")


###########
# Subset
###########


hh_98.df <- mutate(hh_98.df, lexptot = log(1 + exptot)) %>%
  mutate(lnland = log((1 + hhland/100)))


###################################
# Impacts of program participation
###################################

# First Regression (Unbalanced)

des1 <- svydesign(id = ~X,  weights = ~weight, data = hh_98.df)
prog.lm <- svyglm(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                  design=des1, family = quasibinomial(link = "probit"))   
X <- prog.lm$fitted
Tr <- hh_98.df$dmmfd
Y <- hh_98.df$lexptot

m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001)
summary(m.out)

MatchBalance(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, data = hh_98.df, nboots = 500, ks = TRUE)

fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values 

fit.control <- filter(fit, dmmfd == 0)
fit.treated <- filter(fit, dmmfd == 1)

ggplot() + 
  geom_density(aes(x=fit.control$fvalues, linetype = '2')) +
  geom_density(aes(x=fit.treated$fvalues, linetype = '3')) +
  xlim(-.1,.6) +
  xlab("") +
  scale_linetype_discrete(name = "", labels = c("Control", "Treated")) +
  ggtitle("Control and Treated Densities")

# Second Regression (Balanced)

des1 <- svydesign(id = ~X,  weights = ~weight, data = hh_98.df)
prog.lm <- svyglm(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                  design=des1, family = quasibinomial(link = "probit"))   

X <- prog.lm$fitted
Tr <- hh_98.df$dmmfd
Y <- hh_98.df$lexptot

m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001, M = 1, CommonSupport = TRUE)
summary(m.out)

fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values 

fit.control <- filter(fit, dmmfd == 0)
fit.treated <- filter(fit, dmmfd == 1)

ggplot() + 
  geom_density(aes(x=fit.control$fvalues, linetype = '2')) +
  geom_density(aes(x=fit.treated$fvalues, linetype = '3')) +
  xlim(-.1,.6) +
  xlab("") +
  scale_linetype_discrete(name = "", labels = c("Control", "Treated")) +
  ggtitle("Control and Treated Densities")

####################
# Male Matching
####################

# Nearest Neighbor
des1 <- svydesign(id = ~X,  weights = ~weight, data = hh_98.df)
prog.lm <- svyglm(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                  design=des1, family = quasibinomial(link = "probit"))   

X <- prog.lm$fitted.values
Tr <- hh_98.df$dmmfd
Y <- hh_98.df$lexptot
m.out <- Match(Tr = Tr, X = X, Y = Y, M = 1, caliper = 0.001, replace = TRUE)
summary(m.out)

# Stratification Matching
m.out <- matchit(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
                 data = hh_98.df, method = "nearest", distance = "probit", caliper = 0.001)
summary(m.out)

####################
# Female Matching
####################

# Nearest Neighbor
glm.female <- glm(dfmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, family = binomial, data = hh_98.df)
X <- glm.female$fitted
Tr <- hh_98.df$dmmfd
Y <- hh_98.df$lexptot
m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001, M = 1, replace = TRUE)
summary(m.out)



psm_m.lm <- pscore(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                   name.pscore = "pscore", data = hh_98.df, family = quasibinomial(link = "probit"))
summary(psm_m.lm)

mathc <- matchit(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, method = "nearest", data = hh_98.df)

psm_m.match <- ps.match(object = psm_m.lm,
                        matched.by = "pscore", )
summary(psm_m.match)
