###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 04/15/2015
# Code        : rd.R
# Description : Replication code for rd.do 
#              (Regression Discontinuity)
#--------------------------------------------------
###################################################

library(dplyr)
library(ggplot2)

setwd("/home/john/Dropbox/UHM/Classes/Econ 610 - Economic Development/Problem Sets")

# Load data

hh_98.df <- read.csv("Data/hh_98.csv")

# Subsetting

hh_98.df <- filter(hh_98.df, hhland <= 500)

# Because of package design, no need to drop because sharp is determined
#hh_98.df <- filter(hh_98.df, hhland >= 50 & dmmfd == 0 | dfmfd == 0)
#hh_98.df <- filter(hh_98.df, hhland < 50 & dmmfd == 1 | dfmfd == 1)

hh_98.df$lexptot <- log(1+hh_98.df$exptot) 
hh_98.df$lnland <- log(1+hh_98.df$hhland/100)

# Probability distribution for treated males and females 
males_hh_98.df <- filter(hh_98.df, dmmfd == 1 | dmmfd == 0)
males_hh_98.df <- filter(hh_98.df, dfmfd == 1 )

setwd("HW5/")
ggplot(hh_98.df, aes(x=dmmfd)) + geom_density() + ggtitle("Males Density of Treatment") + xlim(-.5,1.5)
ggsave("male_density.png", width = 3, height = 3)  
ggplot(hh_98.df, aes(x=dfmfd)) + geom_density() + ggtitle("Females Density of Treatment") + xlim(-.5,1.5)
ggsave("female_density.png", width = 3, height = 3)  

#RDDtools

library(RDDtools)

# Setup
data <- RDDdata(y = hh_98.df$lexptot, x = hh_98.df$hhland, cutpoint = 50)

# Local linear polynomial regression (Imbens and Kalyanaraman 2012 bandwidth)
data2 <- RDDbw_IK(data)
rd2.lm <- RDDreg_np(RDDobject = data, bw = data2)
print(rd2.lm)
plot(rd2.lm, xlab = "", ylab = "", cex = .2)
abline(v = 50)

# Parametric 2nd order polynomial
rd.lm <- RDDreg_lm(RDDobject = data, order = 2)
plot(rd.lm, xlab = "", ylab = "", cex = .2)
abline(v = 50)

# Bootstrapping
library(boot)

# Function to run RDDreg_lm for bootstrapping
rd <- function(data, i) {
  d <- data[i, ]
  r <- RDDdata(y = d$lexptot, x = d$hhland, cutpoint = 50)
  d2 <- RDDbw_IK(r)
  fit <- RDDreg_np(RDDobject = r, bw = d2)
  return(fit$coefficients)
}

boot(hh_98.df, statistic = rd, 1000)

