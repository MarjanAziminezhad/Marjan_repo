########################################################
##
##  R-script for garden sprinkler experiment, part 2b
##  author: Bernhard Spangl
##  date: 2021-06-07
##
########################################################

library(rsm)

##  3. Further analysis

##  3.1. Design - re-generating the used design

##  basic design
design2 <- cube(basis = ~ x1 + x2,
                n0 = 0,
                reps = 1,
                coding = c(x1 ~ (alpha - 30)/15,
                           x2 ~ (Aq - 2.5e-06)/0.5e-06),
                randomize = FALSE) 

##  Setting up the experiment

set.seed(xxx)    # setting your own seed
expt2 <- djoin(dupe(design2), dupe(design2))

expt2

##  3.2. Reading measurement data

##  Assume: The range measurements are stored in column
##  titled 'range' and the document containing the original 
##  design and the additional column 'range' with the
##  measurements is saved as TSV-file and named
##  'RS_results2_withNoise.tsv'.

dat2 <- read.table("RS_results2_withNoise.tsv",
                   header=TRUE)

expt2$range2 <- dat2$range

##  4. Optimization (part 1)

##  Fitting a first order model for x1 and x2 only; => regression plane!
rsm5 <- rsm(range2 ~ FO(x1, x2), data=expt2)
summary(rsm5)

##  Plotting
contour(rsm5, ~ x1 + x2, image=TRUE)

##  Optimization according to steepest ascent
##  => see at the end of the summary output!!!

##  5-step direction accoring to first-order model
rsm5.steepest <- steepest(rsm5, dist=0:5)
rsm5.steepest

##  Note: In the last column the predicted (according to our
##  model) ranges are calculated. It is clear (because our model
##  represents a regression plane) that the ranges increase. 
##  However, this has to be checked! => HOMEWORK

##  HOMEWORK

##  Design new garden sprinklers according to your values for 'alpha'
##  and 'Aq' given in the above resulting table of R function 'steepest()' 
##  and the fixed values for the other variables as given above, then 
##  simulate the range once again. Keep in mind that you can simulate
##  results only if all parameters are within the parameter space.

##  4.1. Design

expt2.add <- data.frame(beta = 15,   # mittlere Einstellung
                        d = 0.15,   # mittlere Einstellung
                        mt = 0.015,
                        mf = 0.015,
                        pin = 1.5,
                        dzul = 7.5)

##  ACHTUNG: Auf Reihenfolge und Design-Space / Definitionsbereich achten!
write.table(cbind(alpha = rsm5.steepest[1:5, 5], beta = expt2.add[, 1],
                  Aq = rsm5.steepest[1:5, 6], expt2.add[, 2:6]),
            "RS_expt3.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

##  6. Optimization (part 2)

##  Now, to finally optimize our garden sprinkler we will fit an RSM-model
##  at exactly the point of our design space where we measured the maximum
##  range. Hence, we need a new design for our experiments and 9 new
##  garden sprinklers.

##  Basic RSM design
design4 <- ccd(2,
               n0=2, 
               coding = c(x1 ~ (alpha - 42)/12,
                          x2 ~ (Aq - 2.8e-06)/3e-07), 
               randomize = FALSE) 

as.data.frame(design4)
design4

##  Setting up experiment 4
set.seed(xxx)    # setting your own seed
expt4 <- dupe(design4)

as.data.frame(expt4)
expt4

##  Writing TSV-File for measurements

expt4.coded <- code2val(expt4, attr(expt4, "codings"))

## ACHTUNG: Auf Reihenfolge achten!
write.table(cbind(alpha = expt4.coded[, 3], beta = expt2.add[, 1],
                  Aq = expt4.coded[, 4], expt2.add[, 2:6]),
            "RS_expt4.tsv",
            sep = "\t", quote = FALSE, dec = ".", row.names = FALSE)

