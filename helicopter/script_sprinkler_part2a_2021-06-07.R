########################################################
##
##  R-script for garden sprinkler experiment, part 2a
##  author: Bernhard Spangl
##  date: 2021-06-07
##
########################################################

library(rsm)

##  1. Design - re-generating the used design

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

##  Setting up the experiment

set.seed(123)    # setting your own seed
expt <- djoin(dupe(design), dupe(design))

expt

##  2. Analysis

##  Assume: The range measurements are stored in column
##  titled 'range' and the document containing the original 
##  design and the additional column 'range' with the
##  measurements is saved as TSV-file and named
##  'RS_results1_withNoise.tsv'.

dat <- read.table("RS_results1_withNoise.tsv",
                  header=TRUE)

expt$range <- dat$range

##  Different models that can be fitted to the data:

##  First order model (main effects only) of 'Block 1'
rsm1 <- rsm(range ~ FO(x1, x2, x3, x4), data=expt, subset=(Block==1))
summary(rsm1)

##  First order and two-way interaction model of 'Block 1';
##  => only least squares estimates, perfect fit, no testing!!!
rsm2 <- rsm(range ~ FO(x1, x2, x3, x4) + TWI(x1, x2, x3, x4),
            data=expt, subset=(Block==1))
summary(rsm2)

##  First order and two-way interaction model of all data
rsm3 <- rsm(range ~ FO(x1, x2, x3, x4) + TWI(x1, x2, x3, x4),
            data=expt)
summary(rsm3)

##  First order and two-way interaction model of all data with blocking
##  effect; => no significant blocking effect, that's exactly what we
##  would expect!
rsm4 <- rsm(range ~ Block + FO(x1, x2, x3, x4) + TWI(x1, x2, x3, x4),
            data=expt)
summary(rsm4)

##  Model 'rsm3' is exactly what we want!!!
summary(rsm3)

##  3. Further analysis

##  Results: Here, all considered main effects, 'alpha', 'beta', 'Aq', 
##  and 'd' are significant. To keep things simple we choose to only 
##  use the two variables 'alpha' and 'Aq' to optimize our garden sprinkler. 
##  The other parameters are fixed: beta = 15, d = 0.15, mt = 0.015, 
##  mf  = 0.015, pin = 1.5, dzul = 7.5.

##  3.1. Design

##  basic design
design2 <- cube(basis = ~ x1 + x2,
                n0 = 0,
                reps = 1,
                coding = c(x1 ~ (alpha - 30)/15,
                           x2 ~ (Aq - 2.5e-06)/0.5e-06),
                randomize = FALSE) 

as.data.frame(design2)
design2

##  Setting up the experiment

set.seed(xxx)    # setting your own seed
expt2 <- djoin(dupe(design2), dupe(design2))

as.data.frame(expt2)
expt2

##  Writing TSV-File for measurements

expt2.add <- data.frame(beta = 15,   # mittlere Einstellung
                        d = 0.15,   # mittlere Einstellung
                        mt = 0.015,
                        mf = 0.015,
                        pin = 1.5,
                        dzul = 7.5)

expt2.coded <- code2val(expt2, attr(expt2, "codings"))

## ACHTUNG: Auf Reihenfolge achten!
write.table(cbind(alpha = expt2.coded[, 3], beta = expt2.add[, 1],
                  Aq = expt2.coded[, 4], expt2.add[, 2:6]),
            "RS_expt2.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

