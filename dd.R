###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 03/12/2015
# Code        : dd.R
# Description : Replication code for dd.R 
#              (Diff-in-Diff with PSM)
#--------------------------------------------------
###################################################

library(dplyr)
library(foreign)    # For converting *.dta
library(ggplot2)
library(survey)
library(VIF)
library(plm)
library(car)
library(Matching)

setwd("/home/john/Dropbox/UHM/Classes/Econ 610 - Economic Development/Problem Sets")

# Convert hh_98.dta to hh_98.csv with foreign package

yourData <- read.dta("Data/hh_9198.dta")
write.csv(yourData, file = "Data/hh_9198.csv")

# Load hh_98.csv into a data.frame

hh_9198.df <- read.csv("Data/hh_9198.csv")


###########
# Subset
###########

# Simplest Implementation
hh_9198.df <- read.csv("Data/hh_9198.csv")  

hh_9198.df <- mutate(hh_9198.df, exptot0=ifelse(year == 0, exptot, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(exptot91 = max(exptot0))
hh_9198.df <- subset(hh_9198.df, year == 1)
hh_9198.df <- mutate(hh_9198.df, lexptot91=ifelse(year == 1, log(1+exptot91), 0))
hh_9198.df <- mutate(hh_9198.df, lexptot98=ifelse(year == 1, log(1+exptot), 0))
hh_9198.df <- mutate(hh_9198.df, lexptot9891 = lexptot98-lexptot91)
hh_9198.df <- ungroup(hh_9198.df)

# Regression Implementation
hh_9198.df <- read.csv("Data/hh_9198.csv")

hh_9198.df <- mutate(hh_9198.df, lexptot = log(1 + exptot))
hh_9198.df <- mutate(hh_9198.df, lnland = log(1 + hhland / 100))
hh_9198.df <- mutate(hh_9198.df, dmmfd1=ifelse(dmmfd == 1 & year == 1, 1, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dmmfd98 = max(dmmfd1))
hh_9198.df <- mutate(hh_9198.df, dfmfd1=ifelse(dfmfd == 1 & year == 1, 1, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))
hh_9198.df <- mutate(hh_9198.df, dmmfdyr = dmmfd98*year)
hh_9198.df <- mutate(hh_9198.df, dfmfdyr = dfmfd98*year)
hh_9198.df <- ungroup(hh_9198.df)


###########################
# Diff-in-Diff Regression
###########################

# Basic Model

lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = hh_9198.df)
summary(lm)

# Basic Model with FE on nh

lm <- lm(lexptot ~ year + dfmfdyr + dfmfd98 + factor(nh), data = hh_9198.df)
summary(lm)

  #Check for multicolinearity
  
  sqrt(vif(lm))     #Error in vif.default(lm) : there are aliased coefficients in the model
  
  # Contains multicollinearity
  check <- alias(lm)   # Notice that dfmfd98 = -1, therefore highly correlated with dfmfdyr

  # Remove dfmfd98
  lm <- lm(lexptot ~ year + dfmfdyr + factor(nh), data = hh_9198.df)
  sqrt(vif(lm))
  
  # Output is fine now, so can proceed

  # GVIF       Df GVIF^(1/(2*Df))
  # year       1.453455  1.00000        1.205593
  # dfmfdyr    1.764237  1.00000        1.328246
  # factor(nh) 1.414214 28.72281        1.000210

  # Second method for testing for multicollinearity kappa()
  test <- model.matrix(~ year + dfmfdyr + dfmfd98 + factor(nh), data = hh_9198.df)
  kappa(test)   # Output : 2.017073e+16

  #### Because of an extra large kappa, there is collinearity in our model and should be dealt with
  #### conditional number must be less than 30
  
summary(lm)

# Using plm for fixed-effect

lm <- plm(lexptot ~ year + dfmfdyr + dfmfd98 + nh, data = hh_9198.df, model = "within", index = "nh")
summary(lm)

###############
# PSM with DD
###############

# Data setup

hh_9198.df <- read.csv("Data/hh_9198.csv")
hh_9198.df <- mutate(hh_9198.df, lnland = log(1 + hhland / 100))
hh_9198.df <- mutate(hh_9198.df, dfmfd1=ifelse(dfmfd == 1 & year == 1, 1, 0))
hh_9198.df <- group_by(hh_9198.df,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))
hh_9198.df <- filter(hh_9198.df, year == 0)
hh_9198.df$X <- 1:nrow(hh_9198.df)

# First Regression (Unbalanced)

des1 <- svydesign(id = ~X,  weights = ~weight, data = hh_9198.df)
prog.lm <- svyglm(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                  design=des1, family = quasibinomial(link = "probit"))   

X <- prog.lm$fitted
Tr <- hh_9198.df$dfmfd

m.out <- Match(Tr = Tr, X = X, caliper = 0.01)
summary(m.out)

MatchBalance(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, data = hh_9198.df, nboots = 1000)

#Graph density of propensity scores
fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values 

fit.control <- filter(fit, dfmfd == 0)
fit.treated <- filter(fit, dfmfd == 1)

ggplot() + 
  geom_density(aes(x=fit.control$fvalues, linetype = '2')) +
  geom_density(aes(x=fit.treated$fvalues, linetype = '3')) +
  xlim(-.3,1) +
  xlab("") +
  scale_linetype_discrete(name = "", labels = c("Control", "Treated")) +
  ggtitle("Control and Treated Densities")


# Build data frame with ps and nh, then drop ps not matched
ps_dropped <- m.out$index.dropped
ps_hh_9198.df <- data.frame(psm = prog.lm$fitted.values)
ps_hh_9198.df$nh <- prog.lm$data$nh
ps_hh_9198.df <- ps_hh_9198.df[-ps_dropped,]
rownames(ps_hh_9198.df) <- NULL

#Merge to original data frame by nh
hh_9198.df <- read.csv("Data/hh_9198.csv")
psm_hh_9198.df <- right_join(hh_9198.df, ps_hh_9198.df, by = "nh")

# Re-estimate baseline model with matched data set

psm_hh_9198.df <- mutate(psm_hh_9198.df, lexptot = log(1 + exptot))
psm_hh_9198.df <- mutate(psm_hh_9198.df, lnland = log(1 + hhland / 100))
psm_hh_9198.df <- mutate(psm_hh_9198.df, dfmfd1=ifelse(dfmfd == 1 & year == 1, 1, 0))
psm_hh_9198.df <- group_by(psm_hh_9198.df,nh) %>%
  mutate(dfmfd98 = max(dfmfd1))
psm_hh_9198.df <- mutate(psm_hh_9198.df, dfmfdyr = dfmfd98*year)
psm_hh_9198.df <- ungroup(psm_hh_9198.df)

# Re-estimate Basic Model

lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = psm_hh_9198.df)
summary(lm)

# Create Analytical Weights

psm_hh_9198.df$a_weight <- 1
psm_hh_9198.df$a_weight <- ifelse(psm_hh_9198.df$dfmfd == 0, psm_hh_9198.df$psm/(1-psm_hh_9198.df$psm), 1)

# Re-estimate with analytical weights

lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = psm_hh_9198.df, weights = a_weight)
summary(lm)

######### End of Script

