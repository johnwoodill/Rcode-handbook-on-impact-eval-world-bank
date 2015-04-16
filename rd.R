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
library(rdd)
library(ggplot2)

setwd("/home/john/Dropbox/UHM/Classes/Econ 610 - Economic Development/Problem Sets")

# Load data

hh_98.df <- read.csv("Data/hh_98.csv")

# Subsetting

hh_98.df <- filter(hh_98.df, hhland <= 500)

hh_98.df <- filter(hh_98.df, hhland < 50 & dmmfd == 0 | dfmfd == 0)
hh_98.df <- filter(hh_98.df, hhland >= 50 & dmmfd == 1 | dfmfd == 1)

hh_98.df$lexptot <- log(1+hh_98.df$exptot) 
hh_98.df$lnland <- log(1+hh_98.df$hhland/100)

# Probability distribution for treated males and females 
males_hh_98.df <- filter(hh_98.df, dmmfd == 1 | dmmfd == 0)
males_hh_98.df <- filter(hh_98.df, dfmfd == 1 )
plot(density(hh_98.df$dfmfd))
plot(density(hh_98.df$dmmfd))

setwd("HW5/")
ggplot(hh_98.df, aes(x=dmmfd)) + geom_density() + ggtitle("Males Density of Treatment") + xlim(-.5,1.5)
ggsave("male_density.png", width = 3, height = 3)  
ggplot(hh_98.df, aes(x=dfmfd)) + geom_density() + ggtitle("Females Density of Treatment") + xlim(-.5,1.5)
ggsave("female_density.png", width = 3, height = 3)  

# Local polynomial regressions above and below 50 hhland

rd.lm <- RDestimate(lexptot ~ hhland, data = hh_98.df, cutpoint = 50)
a <- rd.lm$model
plot(rd.lm)

# Polynomial
rd1 <- filter(hh_98.df, hhland < 50)
rd1.lm <- lm(lexptot ~ hhland, data = rd1)
rd1$fit <- rd1.lm$fitted.values

rd2 <- filter(hh_98.df, hhland >= 50)
rd2.lm <- lm(lexptot ~ hhland, data = rd2)
rd2$fit <- rd2.lm$fitted.values

plot <- bind_rows(rd1, rd2)
ggplot(plot, aes(x=fit, y=hhland)) + geom_point()
ggplot(rd1, aes(x=fit, y=hhland)) + geom_point()

x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)
RDestimate(y~x)
# Efficiency gains can be made by including covariates
a <- RDestimate(y~x|cov)
plot(a)
