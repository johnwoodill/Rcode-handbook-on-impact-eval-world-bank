###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 01/15/2015
# Code        : random.R
# Description : Replication code for random.do 
#--------------------------------------------------
###################################################

library(dplyr)
library(foreign)    # For converting *.dta


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
  mutate(lnland = log((1 + hhland/100))) %>%
  mutate(vill = thanaid * 10 + villid) %>%
  group_by(vill) %>%
  mutate(progvillm = max(dmmfd), progvillf = max(dfmfd))


################################
# Impacts of program placement
################################


# t-test

  attach(hh_98.df)
  t.test(lexptot ~ progvillf, var.equal = TRUE)
  t.test(lexptot ~ progvillm, var.equal = TRUE)
  detach(hh_98.df)

# Regression Implementation


  prog_place_1.lm <- lm(lexptot ~ progvillm, data = hh_98.df)
  summary(prog_place_1.lm)
  
  prog_place_2.lm <- lm(lexptot ~ progvillf, data = hh_98.df)
  summary(prog_place_2.lm)

  prog_place_3.lm <- lm(lexptot ~ progvillm + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, data = hh_98.df, weight = weight)
  summary(prog_place_3.lm)

  prog_place_4.lm <- lm(lexptot ~ progvillf + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, data = hh_98.df, weight = weight)
  summary(prog_place_4.lm)


###################################
# Impacts of program participation
###################################


# t-test
  
  attach(hh_98.df)
  t.test(lexptot ~ dmmfd)
  t.test(lexptot ~ dfmfd)
  detach(hh_98.df)


# Regression Implementation

  prog_part_1.lm <- lm(lexptot ~ dmmfd, data = hh_98.df)
  summary(prog_part_1.lm)
  
  prog_part_2.lm <- lm(lexptot ~ dfmfd, data = hh_98.df)
  summary(prog_part_2.lm)
  
  prog_part_3.lm <- lm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                          pcirr + rice + wheat + milk + oil + egg, data = hh_98.df, weight = weight)
  summary(prog_part_3.lm)
  
  prog_part_4.lm <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                          pcirr + rice + wheat + milk + oil + egg, data = hh_98.df, weight = weight)
  summary(prog_part_4.lm)

# Expanded regression: capturing both program placement and participation

  prog_place_part_1.lm <- lm(lexptot ~ dmmfd + progvillm + sexhead + agehead + educhead +
                               lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                             data = hh_98.df, weight = weight) 

  summary(prog_place_part_1.lm)
  
  prog_place_part_2.lm <- lm(lexptot ~ dfmfd + progvillm + sexhead + agehead + educhead +
                             lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                           data = hh_98.df, weight = weight) 

  summary(prog_place_part_2.lm)

# Impacts of program participation in program villages

  progvill_1 <- filter(hh_98.df, progvillm == 1)
  progvill_1.lm <- lm(lexptot ~ dmmfd, data = progvill_1, weight = weight)
  summary(progvill_1.lm)

  progvill_2 <- filter(hh_98.df, progvillf == 1)
  progvill_2.lm <- lm(lexptot ~ dmmfd, data = progvill_2, weight = weight)
  summary(progvill_2.lm)

  progvill_3 <- lm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                     pcirr + rice + wheat + milk + oil + egg, data = progvill_1, weight = weight)
  summary(progvill_2)


  progvill_4 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                   pcirr + rice + wheat + milk + oil + egg, data = progvill_2, weight = weight)

# Spillover effects of program placement

  progplace_1 <- filter(hh_98.df, dmmfd == 0)
  progplace_1.lm <- lm(lexptot ~ progvillm, data = progplace_1, weight = weight)
  summary(progplace_1.lm)

  progplace_2 <- filter(hh_98.df, dfmfd == 0)
  progplace_2.lm <- lm(lexptot ~ progvillf, data = progplace_2, weight = weight)
  summary(progplace_2.lm)
  
  progplace_3.lm <- lm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, data = progplace_1, weight = weight)
  summary(progplace_3.lm)

  progplace_4.lm <- lm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                         pcirr + rice + wheat + milk + oil + egg, data = progplace_2, weight = weight)
  summary(progplace_3.lm)


################
# End of Code
################