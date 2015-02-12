###################################################
#--------------------------------------------------
# Author      : A. John Woodill
# Date        : 01/15/2015
# Code        : random.R
# Description : Replication code for random.do 
#--------------------------------------------------
###################################################

library(dplyr)
library(survey)
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

  des1 <- svydesign(id = ~X,  weights = ~weight, data = hh_98.df)

  prog_place_1.lm <- lm(lexptot ~ progvillm, data = hh_98.df)
  summary(prog_place_1.lm)
  
  prog_place_2.lm <- lm(lexptot ~ progvillf, data = hh_98.df)
  summary(prog_place_2.lm)

  prog_place_3.svyglm <- svyglm(lexptot ~ progvillm + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, design = des1)
  
  summary(prog_place_3.svyglm)

  prog_place_4.svyglm <- svyglm(lexptot ~ progvillf + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, design = des1)
  summary(prog_place_4.svyglm)


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
  
  prog_part_3.svyglm <- svyglm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                          pcirr + rice + wheat + milk + oil + egg, design = des1)
  summary(prog_part_3.svyglm)
  
  prog_part_4.svyglm <- svyglm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                          pcirr + rice + wheat + milk + oil + egg, design = des1)
  summary(prog_part_4.svyglm)

# Expanded regression: capturing both program placement and participation

  prog_place_part_1.svyglm <- svyglm(lexptot ~ dmmfd + progvillm + sexhead + agehead + educhead +
                               lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                               design = des1) 

  summary(prog_place_part_1.svyglm)
  
  prog_place_part_2.svyglm <- svyglm(lexptot ~ dfmfd + progvillm + sexhead + agehead + educhead +
                             lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                             design = des1) 

  summary(prog_place_part_2.svyglm)

### Impacts of program participation in program villages

# Fit Design Survey

  progvill_1 <- filter(hh_98.df, progvillm == 1)
  des2 <- svydesign(id = ~X,  weights = ~weight, data = progvill_1)

  progvill_2 <- filter(hh_98.df, progvillf == 1)
  des3 <- svydesign(id = ~X, weights = ~weight, data = progvill_2)

# Regressions
  progvill_1.lm <- lm(lexptot ~ dmmfd, data = progvill_1)
  summary(progvill_1.lm)

  progvill_2.lm <- lm(lexptot ~ dmmfd, data = progvill_2)
  summary(progvill_2.lm)

  progvill_3.svyglm <- svyglm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                     pcirr + rice + wheat + milk + oil + egg, design = des2)
  summary(progvill_3.svyglm)


  progvill_4.svyglm <- svyglm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                   pcirr + rice + wheat + milk + oil + egg, design = des3)
  summary(progvill_4.svyglm)



# Spillover effects of program placement

  progplace_1 <- filter(hh_98.df, dmmfd == 0)
  des4 <- svydesign(id = ~X,  weights = ~weight, data = progplace_1)
  
  progplace_2 <- filter(hh_98.df, dfmfd == 0)
  des5 <- svydesign(id = ~X,  weights = ~weight, data = progplace_2)

  progplace_1.lm <- lm(lexptot ~ progvillm, data = progplace_1)
  summary(progplace_1.lm)

  progplace_2.lm <- svyglm(lexptot ~ progvillf, design = des5)
  summary(progplace_2.lm)
  
  progplace_3.svyglm <- svyglm(lexptot ~ progvillm + sexhead + agehead + educhead + lnland + vaccess + 
                        pcirr + rice + wheat + milk + oil + egg, design = des4)
  summary(progplace_3.svyglm)

  progplace_4.svyglm <- svyglm(lexptot ~ progvillf + sexhead + agehead + educhead + lnland + vaccess + 
                         pcirr + rice + wheat + milk + oil + egg, design = des5)
  summary(progplace_4.svyglm)



################
# End of Code
################