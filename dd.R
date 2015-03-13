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

# Basic Model

lm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = hh_9198.df)
summary(lm)

# Basic Model with FE on nh

lm <- lm(lexptot ~ year + dfmfdyr + dfmfd98 + factor(nh), data = hh_9198.df)
 
  #Check for multicolinearity
  
  sqrt(vif(lm))     #Error in vif.default(lm) : there are aliased coefficients in the model
  
  # Contains multicolinearity
  check <- alias(lm)   # Notice that dfmfd98 = -1, therefore highly neg. correlated 

  # Remove dfmfd98
  lm <- lm(lexptot ~ year + dfmfdyr + factor(nh), data = hh_9198.df)
  sqrt(vif(lm))
  
  # Output is fine now, so can proceed

  # GVIF       Df GVIF^(1/(2*Df))
  # year       1.453455  1.00000        1.205593
  # dfmfdyr    1.764237  1.00000        1.328246
  # factor(nh) 1.414214 28.72281        1.000210

summary(lm)

