########################################################
##
##  R-script for garden sprinkler experiment, part 3
##  author: Bernhard Spangl
##  date: 2021-06-21
##
########################################################

library(rsm)

##  7. Further analysis

##  7.1. Design - re-generating the used ccd design

##  basic RSM design
design4 <- ccd(2,
               n0=2, 
               coding = c(x1 ~ (alpha - 42)/12,
                          x2 ~ (Aq - 2.8e-06)/3e-07), 
               randomize = FALSE) 

##  Setting up experiment 4
set.seed(xxx)    # setting your own seed
expt4 <- dupe(design4)

expt4

##  7.2. Analysis - Optimization (part 3)

dat4 <- read.table("RS_results4_withNoise.tsv",
                   header=TRUE)

expt4$range <- dat4$range

rsm6 <- rsm(range ~ SO(x1, x2), data=expt4)
rsm6.summary <- summary(rsm6)
rsm6.summary

##  Plotting
contour(rsm6, ~ x1 + x2, image=TRUE)

##  8. Estimation 'best' model

##  stationary point
rsm6.sp <- code2val(rsm6.summary$canonical$xs, rsm6.summary$coding)
rsm6.sp

## ACHTUNG: Auf Reihenfolge achten!
write.table(cbind(alpha = rsm6.sp[1], beta = expt2.add[, 1],
                  Aq = rsm6.sp[2], expt2.add[, 2:6]),
            "RS_best.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

##  hier:

## ACHTUNG: Auf Reihenfolge achten!
write.table(cbind(alpha = 39, beta = expt2.add[, 1],
                  Aq = 4e-06, expt2.add[, 2:6]),
            "RS_final.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

