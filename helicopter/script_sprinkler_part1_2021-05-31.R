########################################################
##
##  R-script for garden sprinkler experiment, part 1
##  author: Bernhard Spangl
##  date: 2021-05-31
##
########################################################

library(rsm)

##  1. Design

##  basic design
design <- cube(basis = ~ x1 + x2 + x3,
                generators = x4 ~ x1*x2*x3, 
                n0 = 0,
                reps = 1,
                coding = c(x1 ~ (alpha - 30)/15,
                           x2 ~ (beta - 15)/15,
                           x3 ~ (Aq - 2.5e-06)/0.5e-06,
                           x4 ~ (d - 0.15)/0.05),
                randomize = FALSE) 

as.data.frame(design)
design

##  Setting up the experiment

set.seed(xxx)    # setting your own seed
expt <- djoin(dupe(design), dupe(design))

as.data.frame(expt)
expt

##  Writing CSV-File for measurements

expt.add <- data.frame(mt = 0.015,
                       mf = 0.015,
                       pin = 1.5,
                       dzul = 7.5)

expt.coded <- code2val(expt, attr(expt, "codings"))

write.table(cbind(expt.coded[, c(3:6)], expt.add),
            "RS_expt1.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

