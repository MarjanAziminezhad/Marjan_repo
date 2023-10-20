########################################################
##
##  R-script for paper helicopter experiment, part 3
##  author: Bernhard Spangl
##  date: 2018-12-19
##
########################################################

library(rsm)

##  4. Design - re-generating the used design 2

##  Basic RSM design
design2 <- ccd(2,
               n0=2, 
               coding = c(x1 ~ (RoLe - 13.5)/1.66,
                          x2 ~ (RoWi - 1.5)/0.83),
               randomize = FALSE) 

##  Setting up experiment 2
set.seed(xxx)    # setting your own seed
expt2 <- dupe(design2)

expt2

##  5. Analysis - Optimization (part 3)

dat2 <- read.csv2("helicopterMeasurements2.csv")

expt2$Time <- dat2$Time

rsm6 <- rsm(Time ~ SO(x1, x2), data=expt2)
summary(rsm6)

##  Plotting
contour(rsm6, ~ x1 + x2, image=TRUE)

