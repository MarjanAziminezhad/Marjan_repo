########################################################
##
##  R-script for paper helicopter experiment, part 1
##  author: Bernhard Spangl
##  date: 2018-12-05
##
########################################################

library(rsm)

##  1. Design

##  basic design
design <- cube(basis = ~ x1 + x2 + x3,
                generators = x4 ~ x1*x2*x3, 
                n0 = 0,
                reps = 1,
                coding = c(x1 ~ (RoLe - 8.5)/3,
                           x2 ~ (RoWi - 4.0)/1,
                           x3 ~ (BoLe - 3.5)/2,
                           x4 ~ (FoLe - 7.0)/2),
                randomize = FALSE) 

as.data.frame(design)
design

##  Setting up the experiment

set.seed(xxx)    # setting your own seed
expt <- djoin(dupe(design), dupe(design))

as.data.frame(expt)
expt

##  Writing CSV-File for measurements

expt.coded <- code2val(expt, attr(expt, "codings"))
write.csv2(cbind(expt.coded[, c(1:2, 7, 3:6)], expt[, 3:6]),
           file="helicopter.csv", row.names=FALSE)

