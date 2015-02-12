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
library(nonrandom)  # stratifi-cation, matching and covariate adjustment by PSM
library(Matching)
library(MatchIt)

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

pscore(formula = dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
       data = hh_98.df)

a <- matchit(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
        data = hh_98.df)

MatchBalance(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
             data = hh_98.df)

match(formula = dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg,
      data = hh_98.df)
