###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 04/02/2015
# Code        : iv.R
# Description : Replication code for iv.do 
#              (Instrument Variable Regressions)
#--------------------------------------------------
###################################################

library(dplyr)  # Data cleaning and recoding
library(AER)    # IV implementation : ivreg()

setwd("/home/john/Dropbox/UHM/Classes/Econ 610 - Economic Development/Problem Sets")

# Load data

hh_98.df <- read.csv("Data/hh_98.csv")

# Recode Variables

hh_98.df <- mutate(hh_98.df, lexptot = log(1 + exptot))
hh_98.df <- mutate(hh_98.df, lnland = log(1 + hhland / 100))
hh_98.df <- mutate(hh_98.df, vill = thanaid*10 + villid)
hh_98.df <- group_by(hh_98.df, vill) %>%
  mutate(villmmf = max(dmmfd))
hh_98.df <- mutate(hh_98.df, mchoice = ifelse(villmmf == 1 & hhland < 50, 1, 0))
var <- c("agehead", "sexhead", "educhead", "lnland", "vaccess", "pcirr", "rice", "wheat", "milk", "potato", "egg", "oil")
for (i in 1:length(var)) hh_98.df[[paste("mch", var[[i]], sep = "")]] <- hh_98.df$mchoice*hh_98.df[[var[[i]]]]
hh_98.df <- group_by(hh_98.df, vill) %>%
  mutate(villfmf = max(dfmfd))
hh_98.df <- mutate(hh_98.df, fchoice = ifelse(villfmf == 1 & hhland < 50, 1, 0))
for (i in 1:length(var)) hh_98.df[[paste("fch", var[[i]], sep = "")]] <- hh_98.df$fchoice*hh_98.df[[var[[i]]]]

#########################################################################################
#----------------------------------------------------------------------------------------
# Estimation without interaction terms while just using fchoice as an instrument
#----------------------------------------------------------------------------------------
#########################################################################################

# First Stage
fsls <- lm(dfmfd ~ agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + egg + 
             oil + fchoice, data = hh_98.df)
summary(fsls)

# Omit potato because of collinearity

ivreg <- ivreg(lexptot ~ dfmfd + agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk +   
                 egg + oil | agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk +  
                 egg + oil + fchoice, data = hh_98.df)                   
summary(ivreg)

# Diagnostic Tests

summary(ivreg, vcov = sandwich, df = Inf, diagnostics = TRUE)




#####################################################
#---------------------------------------------------
# Female Participation with instrument interactions
#---------------------------------------------------
#####################################################

# First Stage Least Squares

fsls <- lm(dfmfd ~ agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + potato + egg + 
             oil + fchoice + fchagehead + fchsexhead + fcheduchead + fchlnland + fchvaccess + fchpcirr +
                 fchrice + fchwheat + fchmilk + fchegg + fchoil, data = hh_98.df)
summary(fsls)

# Second Stage

# Ommit potato because of collinearity in the model as shown in FSLS)

ivreg2 <- ivreg(lexptot ~ dfmfd + agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk +   
                 egg + oil + fchoice| agehead + sexhead + educhead + lnland + vaccess + pcirr + rice + wheat + milk +  
                 egg + oil + fchoice  + fchagehead + fchsexhead + fcheduchead + fchlnland + fchvaccess + fchpcirr +
                 fchrice + fchwheat + fchmilk + fchegg + fchoil, data = hh_98.df)                   
summary(ivreg2)

# Diagnostic Tests

summary(ivreg2, vcov = sandwich, df = Inf, diagnostics = TRUE)

############# End of Code #############