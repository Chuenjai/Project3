# Required packages
library(foreign)
library(dplyr)
library(sqldf)
library(lavaan) # apply riclpmModel
library(restriktor) # to call GORICA

#load dataset

data <- read.csv("QuestionaireT1toT3_NP.csv") 

Psy.fac <- data[, c(20:25,33,51:64)]
Psy.fac[1:21] <- replace(Psy.fac[1:21], Psy.fac[1:21] == -999.00, NA)
colnames(Psy.fac)<-c("a1", "b1", "c1","d1", "e1", "f1", "g1",
                    "a2", "a3", "b2","b3", "c2", "c3", "d2",
                    "d3", "e2", "e3","f2", "f3", "g2", "g3")

#standardizing
Psy.fac <- scale(Psy.fac) 

# hypothesis about RI variances
H0 <- "kappaV == 0 ; omegaV == 0; epsilonV == 0; zetaV == 0; etaV == 0; chiV == 0; psiV == 0"
Ha <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0" #the specified RI-CLPM

# hypotheses for selecting the right RI-CLPM
HRi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0" # RI-CLPM (full)
HCl <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0" # CLPM

H1Ri.kappa    <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0"
H1Ri.omega    <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0" 
H1Ri.epsilon  <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0" 
H1Ri.zeta     <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0" 
H1Ri.eta      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0" 
H1Ri.chi      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0" 
H1Ri.psi      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0" 

H2Ris.kappa.omega   <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.kappa.epsilon <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.kappa.zeta    <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.kappa.eta     <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H2Ris.kappa.chi     <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H2Ris.kappa.psi     <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H2Ris.omega.epsilon <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.omega.zeta    <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.omega.eta     <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H2Ris.omega.chi     <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H2Ris.omega.psi     <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H2Ris.epsilon.zeta  <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H2Ris.epsilon.eta   <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H2Ris.epsilon.chi   <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H2Ris.epsilon.psi   <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H2Ris.zeta.eta      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H2Ris.zeta.chi      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H2Ris.zeta.psi      <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H2Ris.eta.chi       <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H2Ris.eta.psi       <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H2Ris.chi.psi       <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"

H3Ris.kappa.omega.epsilon <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV < 0"
H3Ris.kappa.omega.zeta <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H3Ris.kappa.omega.eta <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.kappa.omega.chi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.kappa.omega.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.kappa.epsilon.zeta <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H3Ris.kappa.epsilon.eta <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.kappa.epsilon.chi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.kappa.epsilon.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.kappa.zeta.eta <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.kappa.zeta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.kappa.zeta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.kappa.eta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H3Ris.kappa.eta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H3Ris.kappa.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H3Ris.omega.epsilon.zeta <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H3Ris.omega.epsilon.eta <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.omega.epsilon.chi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.omega.epsilon.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.omega.zeta.eta <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.omega.zeta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.omega.zeta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.omega.eta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H3Ris.omega.eta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H3Ris.omega.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H3Ris.epsilon.zeta.eta <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H3Ris.epsilon.zeta.chi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H3Ris.epsilon.zeta.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H3Ris.epsilon.eta.chi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H3Ris.epsilon.eta.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H3Ris.epsilon.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H3Ris.zeta.eta.chi <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H3Ris.zeta.eta.psi <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H3Ris.zeta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H3Ris.eta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"

H4Ris.kappa.omega.epsilon.zeta <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV < 0"
H4Ris.kappa.omega.epsilon.eta <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV < 0"
H4Ris.kappa.omega.epsilon.chi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV < 0"
H4Ris.kappa.omega.epsilon.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV < 0; psiV > 0"
H4Ris.kappa.omega.zeta.eta <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H4Ris.kappa.omega.zeta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H4Ris.kappa.omega.zeta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H4Ris.kappa.omega.eta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.kappa.omega.eta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.kappa.omega.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.kappa.epsilon.zeta.eta <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H4Ris.kappa.epsilon.zeta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H4Ris.kappa.epsilon.zeta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H4Ris.kappa.epsilon.eta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.kappa.epsilon.eta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.kappa.epsilon.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.kappa.zeta.eta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.kappa.zeta.eta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.kappa.zeta.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.kappa.eta.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H4Ris.omega.epsilon.zeta.eta <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H4Ris.omega.epsilon.zeta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H4Ris.omega.epsilon.zeta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H4Ris.omega.epsilon.eta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.omega.epsilon.eta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.omega.epsilon.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.omega.zeta.eta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.omega.zeta.eta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.omega.zeta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.omega.eta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H4Ris.epsilon.zeta.eta.chi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H4Ris.epsilon.zeta.eta.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H4Ris.epsilon.zeta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H4Ris.epsilon.eta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H4Ris.zeta.eta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"

H5Ris.kappa.omega.epsilon.zeta.eta <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV < 0"
H5Ris.kappa.omega.epsilon.zeta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV < 0"
H5Ris.kappa.omega.epsilon.zeta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV < 0; psiV > 0"
H5Ris.kappa.omega.epsilon.eta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV < 0"
H5Ris.kappa.omega.epsilon.eta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV < 0; psiV > 0"
H5Ris.kappa.omega.epsilon.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV < 0; chiV > 0; psiV > 0"
H5Ris.kappa.omega.zeta.eta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H5Ris.kappa.omega.zeta.eta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H5Ris.kappa.omega.zeta.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H5Ris.kappa.omega.eta.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H5Ris.kappa.epsilon.zeta.eta.chi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H5Ris.kappa.epsilon.zeta.eta.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H5Ris.kappa.epsilon.zete.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H5Ris.kappa.epsilon.eta.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H5Ris.kappa.zeta.eta.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"
H5Ris.omega.epsilon.zeta.eta.chi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H5Ris.omega.epsilon.zeta.eta.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H5Ris.omega.epsilon.zeta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H5Ris.omega.epsilon.eta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H5Ris.omega.zeta.eta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"
H5Ris.epsilon.zeta.eta.chi.psi <- "kappaV < 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"

H6Ris.kappa.omega.epsilon.zeta.eta.chi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV < 0"
H6Ris.kappa.omega.epsilon.zeta.eta.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV < 0; psiV > 0"
H6Ris.kappa.omega.epsilon.zeta.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV < 0; chiV > 0; psiV > 0"
H6Ris.kappa.omega.epsilon.eta.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV > 0; zetaV < 0; etaV > 0; chiV > 0; psiV > 0"
H6Ris.kappa.omega.zeta.eta.chi.psi <- "kappaV > 0 ; omegaV > 0; epsilonV < 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"
H6Ris.kappa.epsilon.zeta.eta.chi.psi <- "kappaV > 0 ; omegaV < 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"
H6Ris.omega.epsilon.zeta.eta.chi.psi <- "kappaV < 0 ; omegaV > 0; epsilonV > 0; zetaV > 0; etaV > 0; chiV > 0; psiV > 0"

H1c <- "abs(ba) > abs(ab); abs(dg) > abs(gd)"
H2c <- "abs(ba) < abs(ab); abs(dg) < abs(gd)"
H3c <- "abs(ba) > abs(ab); abs(dg) < abs(gd)"
H4c <- "abs(ba) < abs(ab); abs(dg) > abs(gd)"

# Fitting a CLPM

clpmModel<-
  '

  kappa =~ 1*a1 + 1*a2 + 1*a3
  omega =~ 1*b1 + 1*b2 + 1*b3
  epsilon =~ 1*c1 + 1*c2 + 1*c3
  zeta =~ 1*d1 + 1*d2 + 1*d3
  eta =~ 1*e1 + 1*e2 + 1*e3
  chi =~ 1*f1 + 1*f2 + 1*f3
  psi =~ 1*g1 + 1*g2 + 1*g3

  a1 ~ mu(a1)*1
  a2 ~ mu(a2)*1
  a3 ~ mu(a3)*1
  b1 ~ mu(b1)*1
  b2 ~ mu(b2)*1
  b3 ~ mu(b3)*1
  c1 ~ mu(c1)*1
  c2 ~ mu(c2)*1
  c3 ~ mu(c3)*1
  d1 ~ mu(d1)*1
  d2 ~ mu(d2)*1
  d3 ~ mu(d3)*1
  e1 ~ mu(e1)*1
  e2 ~ mu(e2)*1
  e3 ~ mu(e3)*1
  f1 ~ mu(f1)*1
  f2 ~ mu(f2)*1
  f3 ~ mu(f3)*1
  g1 ~ mu(g1)*1
  g2 ~ mu(g2)*1
  g3 ~ mu(g3)*1


  # Random intercept parts
  kappa ~~ 0*kappa #variance
  omega ~~ 0*omega #variance
  epsilon ~~ 0*epsilon #variance
  zeta ~~ 0*zeta #variance
  eta ~~ 0*eta #variance
  chi ~~ 0*chi #variance
  psi ~~ 0*psi #variance
  

  #latent vars for AR and cross-lagged effects
  p1 =~ 1*a1 #each factor loading set to 1
  p2 =~ 1*a2
  p3 =~ 1*a3
  q1 =~ 1*b1
  q2 =~ 1*b2
  q3 =~ 1*b3
  r1 =~ 1*c1
  r2 =~ 1*c2
  r3 =~ 1*c3
  s1 =~ 1*d1
  s2 =~ 1*d2
  s3 =~ 1*d3
  t1 =~ 1*e1
  t2 =~ 1*e2
  t3 =~ 1*e3
  u1 =~ 1*f1
  u2 =~ 1*f2
  u3 =~ 1*f3
  v1 =~ 1*g1
  v2 =~ 1*g2
  v3 =~ 1*g3

  #constrain autoregression and cross-lagged effects to be the same across both lags.
  p3 ~ phi(a)*p2 + phi(ba)*q2+ phi(ca)*r2+ phi(da)*s2+ phi(ea)*t2+ phi(fa)*u2+ phi(ga)*v2
  p2 ~ phi(a)*p1 + phi(ba)*q1+ phi(ca)*r1+ phi(da)*s1+ phi(ea)*t1+ phi(fa)*u1+ phi(ga)*v1

  q3 ~ phi(b)*q2 + phi(ab)*p2+ phi(cb)*r2+ phi(db)*s2+ phi(eb)*t2+ phi(fb)*u2+ phi(gb)*v2
  q2 ~ phi(b)*q1 + phi(ab)*p1+ phi(cb)*r1+ phi(db)*s1+ phi(eb)*t1+ phi(fb)*u1+ phi(gb)*v1
  
  r3 ~ phi(c)*r2 + phi(ac)*p2+ phi(bc)*q2+ phi(dc)*s2+ phi(ec)*t2+ phi(fc)*u2+ phi(gc)*v2
  r2 ~ phi(c)*r1 + phi(ac)*p1+ phi(bc)*q1+ phi(dc)*s1+ phi(ec)*t1+ phi(fc)*u1+ phi(gc)*v1
  
  s3 ~ phi(d)*s2 + phi(ad)*p2+ phi(bd)*q2+ phi(cd)*r2+ phi(ed)*t2+ phi(fd)*u2+ phi(gd)*v2
  s2 ~ phi(d)*s1 + phi(ad)*p1+ phi(bd)*q1+ phi(cd)*r1+ phi(ed)*t1+ phi(fd)*u1+ phi(gd)*v1

  t3 ~ phi(e)*t2 + phi(ae)*p2+ phi(be)*q2+ phi(ce)*r2+ phi(de)*s2+ phi(fe)*u2+ phi(ge)*v2
  t2 ~ phi(e)*t1 + phi(ae)*p1+ phi(be)*q1+ phi(ce)*r1+ phi(de)*s1+ phi(fe)*u1+ phi(ge)*v1

  u3 ~ phi(f)*u2 + phi(af)*p2+ phi(bf)*q2+ phi(cf)*r2+ phi(df)*s2+ phi(ef)*t2+ phi(gf)*v2
  u2 ~ phi(f)*u1 + phi(af)*p1+ phi(bf)*q1+ phi(cf)*r1+ phi(df)*s1+ phi(ef)*t1+ phi(gf)*v1

  v3 ~ phi(g)*v2 + phi(ag)*p2+ phi(bg)*q2+ phi(cg)*r2+ phi(dg)*s2+ phi(eg)*t2+ phi(fg)*u2
  v2 ~ phi(g)*v1 + phi(ag)*p1+ phi(bg)*q1+ phi(cg)*r1+ phi(dg)*s1+ phi(eg)*t1+ phi(fg)*u1


  p1 ~~ p1 #variance
  p2 ~~ h*p2
  p3 ~~ h*p3
  q1 ~~ q1 #variance
  q2 ~~ ii*q2
  q3 ~~ ii*q3
  r1 ~~ r1 #variance
  r2 ~~ j*r2
  r3 ~~ j*r3
  s1 ~~ s1 #variance
  s2 ~~ k*s2
  s3 ~~ k*s3
  t1 ~~ t1 #variance
  t2 ~~ l*t2
  t3 ~~ l*t3
  u1 ~~ u1 #variance
  u2 ~~ m*u2
  u3 ~~ m*u3
  v1 ~~ v1 #variance
  v2 ~~ n*v2
  v3 ~~ n*v3


  p1 ~~ q1 #p1 and q1 covariance
  p2 ~~ hii*q2 #p2 and q2 covariance should also be constrained to be the same
  p3 ~~ hii*q3 
  p1 ~~ r1
  p2 ~~ hj*r2
  p3 ~~ hj*r3
  p1 ~~ s1
  p2 ~~ hk*s2
  p3 ~~ hk*s3
  p1 ~~ t1
  p2 ~~ hl*t2
  p3 ~~ hl*t3
  p1 ~~ u1
  p2 ~~ hm*u2
  p3 ~~ hm*u3
  p1 ~~ v1
  p2 ~~ hn*v2
  p3 ~~ hn*v3
  q1 ~~ r1
  q2 ~~ iij*r2
  q3 ~~ iij*r3
  q1 ~~ s1
  q2 ~~ iik*s2
  q3 ~~ iik*s3
  q1 ~~ t1
  q2 ~~ iil*t2
  q3 ~~ iil*t3
  q1 ~~ u1
  q2 ~~ iim*u2
  q3 ~~ iim*u3
  q1 ~~ v1
  q2 ~~ iin*v2
  q3 ~~ iin*v3
  r1 ~~ s1
  r2 ~~ jk*s2
  r3 ~~ jk*s3
  r1 ~~ t1
  r2 ~~ jl*t2
  r3 ~~ jl*t3
  r1 ~~ u1
  r2 ~~ jm*u2
  r3 ~~ jm*u3
  r1 ~~ v1
  r2 ~~ jn*v2
  r3 ~~ jn*v3
  s1 ~~ t1
  s2 ~~ kl*t2
  s3 ~~ kl*t3
  s1 ~~ u1
  s2 ~~ km*u2
  s3 ~~ km*u3
  s1 ~~ v1
  s2 ~~ kn*v2
  s3 ~~ kn*v3
  t1 ~~ u1
  t2 ~~ lm*u2
  t3 ~~ lm*u3
  t1 ~~ v1
  t2 ~~ ln*v2
  t3 ~~ ln*v3
  u1 ~~ v1
  u2 ~~ mn*v2
  u3 ~~ mn*v3 '

clpm <- lavaan(clpmModel, data = Psy.fac,
                 missing = 'ML', #for the missing data!
                 int.ov.free = F,
                 int.lv.free = F,
                 auto.fix.first = F,
                 auto.fix.single = F,
                 auto.cov.lv.x = F,
                 auto.cov.y = F,
                 auto.var = F)

# Fitting a RI-CLPM

riclpmModel<-
  '

  kappa =~ 1*a1 + 1*a2 + 1*a3
  omega =~ 1*b1 + 1*b2 + 1*b3
  epsilon =~ 1*c1 + 1*c2 + 1*c3
  zeta =~ 1*d1 + 1*d2 + 1*d3
  eta =~ 1*e1 + 1*e2 + 1*e3
  chi =~ 1*f1 + 1*f2 + 1*f3
  psi =~ 1*g1 + 1*g2 + 1*g3

  a1 ~ mu(a1)*1
  a2 ~ mu(a2)*1
  a3 ~ mu(a3)*1
  b1 ~ mu(b1)*1
  b2 ~ mu(b2)*1
  b3 ~ mu(b3)*1
  c1 ~ mu(c1)*1
  c2 ~ mu(c2)*1
  c3 ~ mu(c3)*1
  d1 ~ mu(d1)*1
  d2 ~ mu(d2)*1
  d3 ~ mu(d3)*1
  e1 ~ mu(e1)*1
  e2 ~ mu(e2)*1
  e3 ~ mu(e3)*1
  f1 ~ mu(f1)*1
  f2 ~ mu(f2)*1
  f3 ~ mu(f3)*1
  g1 ~ mu(g1)*1
  g2 ~ mu(g2)*1
  g3 ~ mu(g3)*1


  # Random intercept parts
  kappa ~~ kappa #variance
  omega ~~ omega #variance
  epsilon ~~ epsilon #variance
  zeta ~~ zeta #variance
  eta ~~ eta #variance
  chi ~~ chi #variance
  psi ~~ psi #variance
  kappa ~~ omega #covariance
  kappa ~~ epsilon #covariance
  kappa ~~ zeta #covariance
  kappa ~~ eta #covariance
  kappa ~~ chi #covariance
  kappa ~~ psi #covariance
  omega ~~ epsilon #covariance
  omega ~~ zeta #covariance
  omega ~~ eta #covariance
  omega ~~ chi #covariance
  omega ~~ psi #covariance
  epsilon ~~ zeta #covariance
  epsilon ~~ eta #covariance
  epsilon ~~ chi #covariance
  epsilon ~~ psi #covariance
  zeta ~~ eta #covariance
  zeta ~~ chi #covariance
  zeta ~~ psi #covariance
  eta ~~ chi #covariance
  eta ~~ psi #covariance
  chi ~~ psi #covariance

  #latent vars for AR and cross-lagged effects
  p1 =~ 1*a1 #each factor loading set to 1
  p2 =~ 1*a2
  p3 =~ 1*a3
  q1 =~ 1*b1
  q2 =~ 1*b2
  q3 =~ 1*b3
  r1 =~ 1*c1
  r2 =~ 1*c2
  r3 =~ 1*c3
  s1 =~ 1*d1
  s2 =~ 1*d2
  s3 =~ 1*d3
  t1 =~ 1*e1
  t2 =~ 1*e2
  t3 =~ 1*e3
  u1 =~ 1*f1
  u2 =~ 1*f2
  u3 =~ 1*f3
  v1 =~ 1*g1
  v2 =~ 1*g2
  v3 =~ 1*g3

  #constrain autoregression and cross-lagged effects to be the same across both lags.
  p3 ~ phi(a)*p2 + phi(ba)*q2+ phi(ca)*r2+ phi(da)*s2+ phi(ea)*t2+ phi(fa)*u2+ phi(ga)*v2
  p2 ~ phi(a)*p1 + phi(ba)*q1+ phi(ca)*r1+ phi(da)*s1+ phi(ea)*t1+ phi(fa)*u1+ phi(ga)*v1

  q3 ~ phi(b)*q2 + phi(ab)*p2+ phi(cb)*r2+ phi(db)*s2+ phi(eb)*t2+ phi(fb)*u2+ phi(gb)*v2
  q2 ~ phi(b)*q1 + phi(ab)*p1+ phi(cb)*r1+ phi(db)*s1+ phi(eb)*t1+ phi(fb)*u1+ phi(gb)*v1
  
  r3 ~ phi(c)*r2 + phi(ac)*p2+ phi(bc)*q2+ phi(dc)*s2+ phi(ec)*t2+ phi(fc)*u2+ phi(gc)*v2
  r2 ~ phi(c)*r1 + phi(ac)*p1+ phi(bc)*q1+ phi(dc)*s1+ phi(ec)*t1+ phi(fc)*u1+ phi(gc)*v1
  
  s3 ~ phi(d)*s2 + phi(ad)*p2+ phi(bd)*q2+ phi(cd)*r2+ phi(ed)*t2+ phi(fd)*u2+ phi(gd)*v2
  s2 ~ phi(d)*s1 + phi(ad)*p1+ phi(bd)*q1+ phi(cd)*r1+ phi(ed)*t1+ phi(fd)*u1+ phi(gd)*v1

  t3 ~ phi(e)*t2 + phi(ae)*p2+ phi(be)*q2+ phi(ce)*r2+ phi(de)*s2+ phi(fe)*u2+ phi(ge)*v2
  t2 ~ phi(e)*t1 + phi(ae)*p1+ phi(be)*q1+ phi(ce)*r1+ phi(de)*s1+ phi(fe)*u1+ phi(ge)*v1

  u3 ~ phi(f)*u2 + phi(af)*p2+ phi(bf)*q2+ phi(cf)*r2+ phi(df)*s2+ phi(ef)*t2+ phi(gf)*v2
  u2 ~ phi(f)*u1 + phi(af)*p1+ phi(bf)*q1+ phi(cf)*r1+ phi(df)*s1+ phi(ef)*t1+ phi(gf)*v1

  v3 ~ phi(g)*v2 + phi(ag)*p2+ phi(bg)*q2+ phi(cg)*r2+ phi(dg)*s2+ phi(eg)*t2+ phi(fg)*u2
  v2 ~ phi(g)*v1 + phi(ag)*p1+ phi(bg)*q1+ phi(cg)*r1+ phi(dg)*s1+ phi(eg)*t1+ phi(fg)*u1



  p1 ~~ p1 #variance
  p2 ~~ h*p2
  p3 ~~ h*p3
  q1 ~~ q1 #variance
  q2 ~~ ii*q2
  q3 ~~ ii*q3
  r1 ~~ r1 #variance
  r2 ~~ j*r2
  r3 ~~ j*r3
  s1 ~~ s1 #variance
  s2 ~~ k*s2
  s3 ~~ k*s3
  t1 ~~ t1 #variance
  t2 ~~ l*t2
  t3 ~~ l*t3
  u1 ~~ u1 #variance
  u2 ~~ m*u2
  u3 ~~ m*u3
  v1 ~~ v1 #variance
  v2 ~~ n*v2
  v3 ~~ n*v3


  p1 ~~ q1 #p1 and q1 covariance
  p2 ~~ hii*q2 #p2 and q2 covariance should also be constrained to be the same
  p3 ~~ hii*q3 
  p1 ~~ r1
  p2 ~~ hj*r2
  p3 ~~ hj*r3
  p1 ~~ s1
  p2 ~~ hk*s2
  p3 ~~ hk*s3
  p1 ~~ t1
  p2 ~~ hl*t2
  p3 ~~ hl*t3
  p1 ~~ u1
  p2 ~~ hm*u2
  p3 ~~ hm*u3
  p1 ~~ v1
  p2 ~~ hn*v2
  p3 ~~ hn*v3
  q1 ~~ r1
  q2 ~~ iij*r2
  q3 ~~ iij*r3
  q1 ~~ s1
  q2 ~~ iik*s2
  q3 ~~ iik*s3
  q1 ~~ t1
  q2 ~~ iil*t2
  q3 ~~ iil*t3
  q1 ~~ u1
  q2 ~~ iim*u2
  q3 ~~ iim*u3
  q1 ~~ v1
  q2 ~~ iin*v2
  q3 ~~ iin*v3
  r1 ~~ s1
  r2 ~~ jk*s2
  r3 ~~ jk*s3
  r1 ~~ t1
  r2 ~~ jl*t2
  r3 ~~ jl*t3
  r1 ~~ u1
  r2 ~~ jm*u2
  r3 ~~ jm*u3
  r1 ~~ v1
  r2 ~~ jn*v2
  r3 ~~ jn*v3
  s1 ~~ t1
  s2 ~~ kl*t2
  s3 ~~ kl*t3
  s1 ~~ u1
  s2 ~~ km*u2
  s3 ~~ km*u3
  s1 ~~ v1
  s2 ~~ kn*v2
  s3 ~~ kn*v3
  t1 ~~ u1
  t2 ~~ lm*u2
  t3 ~~ lm*u3
  t1 ~~ v1
  t2 ~~ ln*v2
  t3 ~~ ln*v3
  u1 ~~ v1
  u2 ~~ mn*v2
  u3 ~~ mn*v3 '

riclpm <- lavaan(riclpmModel, data = Psy.fac,
                  missing = 'ML', #for the missing data!
                  int.ov.free = F,
                  int.lv.free = F,
                  auto.fix.first = F,
                  auto.fix.single = F,
                  auto.cov.lv.x = F,
                  auto.cov.y = F,
                  auto.var = F)


#AIC
AIC_clpm <- fitMeasures(clpm, "aic")
AIC_riclpm <- fitMeasures(riclpm, "aic")

#GORICA 
RIvar <- coef(riclpm)[c(22,23,24,25,26,27,28)]
names(RIvar) <- c("kappaV", "omegaV", "epsilonV", "zetaV", "etaV", "chiV", "psiV")
RIVcov <- vcov(riclpm)[c(22,23,24,25,26,27,28), c(22,23,24,25,26,27,28)]
isSymmetric.matrix(RIVcov)

# AIC based on gorica: H0 vs Hunc (complement of H0)
gorica <- goric(RIvar, VCOV = RIVcov, hypotheses = list(H0 = H0), comparison = "complement", type = "gorica")


# The full covariance matrix of the random intercepts is:
S <- vcov(riclpm)[c(22,23,24,25,26,27,28), c(22,23,24,25,26,27,28)]
is.matrix(S)

# Chi-square values
Chi2_riclpm <- fitMeasures(riclpm, "chisq") 
Chi2_clpm <- fitMeasures(clpm, "chisq")

#Degrees of freedom (df)
df_clpm <- fitMeasures(clpm, "df") 
df_riclpm <- fitMeasures(riclpm, "df")

# Run function to do Chi-bar-square test (and also obtain Chi-bar-square weigths)
ChiBar2DiffTest <- ChiBarSq.DiffTest(7, S, Chi2_clpm, Chi2_riclpm, df_clpm, df_riclpm)

#p-value
pValue <- ChiBar2DiffTest$p_value
pValue

#select the suitable number of RIs in model
goricaResult1 <- goric(RIvar, VCOV = RIVcov, 
                      hypotheses = list(HRi = HRi, HCl = HCl,
                                        H1Ri.kappa = H1Ri.kappa, H1Ri.omega = H1Ri.omega, H1Ri.epsilon = H1Ri.epsilon, H1Ri.zeta = H1Ri.zeta, H1Ri.eta = H1Ri.eta, H1Ri.chi = H1Ri.chi, H1Ri.psi = H1Ri.psi,
                                        H2Ris.kappa.omega = H2Ris.kappa.omega, H2Ris.kappa.epsilon = H2Ris.kappa.epsilon, H2Ris.kappa.zeta = H2Ris.kappa.zeta, H2Ris.kappa.eta = H2Ris.kappa.eta, H2Ris.kappa.chi = H2Ris.kappa.chi, H2Ris.kappa.psi = H2Ris.kappa.psi,
                                        H2Ris.omega.epsilon = H2Ris.omega.epsilon, H2Ris.omega.zeta = H2Ris.omega.zeta, H2Ris.omega.eta = H2Ris.omega.eta, H2Ris.omega.chi = H2Ris.omega.chi, H2Ris.omega.psi = H2Ris.omega.psi,
                                        H2Ris.epsilon.zeta = H2Ris.epsilon.zeta, H2Ris.epsilon.eta = H2Ris.epsilon.eta, H2Ris.epsilon.chi = H2Ris.epsilon.chi, H2Ris.epsilon.psi = H2Ris.epsilon.psi, H2Ris.zeta.eta = H2Ris.zeta.eta,
                                        H2Ris.zeta.chi = H2Ris.zeta.chi, H2Ris.zeta.psi = H2Ris.zeta.psi, H2Ris.eta.chi = H2Ris.eta.chi, H2Ris.eta.psi = H2Ris.eta.psi, H2Ris.chi.psi = H2Ris.chi.psi,
                                        H3Ris.kappa.omega.epsilon = H3Ris.kappa.omega.epsilon, H3Ris.kappa.omega.zeta = H3Ris.kappa.omega.zeta, H3Ris.kappa.omega.eta = H3Ris.kappa.omega.eta, H3Ris.kappa.omega.chi = H3Ris.kappa.omega.chi, H3Ris.kappa.omega.psi = H3Ris.kappa.omega.psi, H3Ris.kappa.epsilon.zeta = H3Ris.kappa.epsilon.zeta, H3Ris.kappa.epsilon.eta = H3Ris.kappa.epsilon.eta,
                                        H3Ris.kappa.epsilon.chi = H3Ris.kappa.epsilon.chi, H3Ris.kappa.epsilon.psi = H3Ris.kappa.epsilon.psi, H3Ris.kappa.zeta.eta = H3Ris.kappa.zeta.eta, H3Ris.kappa.zeta.chi = H3Ris.kappa.zeta.chi, H3Ris.kappa.zeta.psi = H3Ris.kappa.zeta.psi, H3Ris.kappa.eta.chi = H3Ris.kappa.eta.chi, H3Ris.kappa.eta.psi = H3Ris.kappa.eta.psi,
                                        H3Ris.kappa.chi.psi = H3Ris.kappa.chi.psi, H3Ris.omega.epsilon.zeta = H3Ris.omega.epsilon.zeta, H3Ris.omega.epsilon.eta = H3Ris.omega.epsilon.eta, H3Ris.omega.epsilon.chi = H3Ris.omega.epsilon.chi, H3Ris.omega.epsilon.psi = H3Ris.omega.epsilon.psi, H3Ris.omega.zeta.eta = H3Ris.omega.zeta.eta, H3Ris.omega.zeta.chi = H3Ris.omega.zeta.chi,
                                        H3Ris.omega.zeta.psi = H3Ris.omega.zeta.psi, H3Ris.omega.eta.chi = H3Ris.omega.eta.chi, H3Ris.omega.eta.psi = H3Ris.omega.eta.psi, H3Ris.omega.chi.psi = H3Ris.omega.chi.psi, H3Ris.epsilon.zeta.eta = H3Ris.epsilon.zeta.eta, H3Ris.epsilon.zeta.chi = H3Ris.epsilon.zeta.chi, H3Ris.epsilon.zeta.psi = H3Ris.epsilon.zeta.psi,
                                        H3Ris.epsilon.eta.chi = H3Ris.epsilon.eta.chi, H3Ris.epsilon.eta.psi = H3Ris.epsilon.eta.psi, H3Ris.epsilon.chi.psi = H3Ris.epsilon.chi.psi, H3Ris.zeta.eta.chi = H3Ris.zeta.eta.chi, H3Ris.zeta.eta.psi = H3Ris.zeta.eta.psi, H3Ris.zeta.chi.psi = H3Ris.zeta.chi.psi, H3Ris.eta.chi.psi = H3Ris.eta.chi.psi,
                                        H4Ris.kappa.omega.epsilon.zeta = H4Ris.kappa.omega.epsilon.zeta, H4Ris.kappa.omega.epsilon.eta = H4Ris.kappa.omega.epsilon.eta, H4Ris.kappa.omega.epsilon.chi = H4Ris.kappa.omega.epsilon.chi, H4Ris.kappa.omega.epsilon.psi = H4Ris.kappa.omega.epsilon.psi, H4Ris.kappa.omega.zeta.eta = H4Ris.kappa.omega.zeta.eta, H4Ris.kappa.omega.zeta.chi = H4Ris.kappa.omega.zeta.chi, H4Ris.kappa.omega.zeta.psi = H4Ris.kappa.omega.zeta.psi,
                                        H4Ris.kappa.omega.eta.chi = H4Ris.kappa.omega.eta.chi, H4Ris.kappa.omega.eta.psi = H4Ris.kappa.omega.eta.psi, H4Ris.kappa.omega.chi.psi = H4Ris.kappa.omega.chi.psi, H4Ris.kappa.epsilon.zeta.eta = H4Ris.kappa.epsilon.zeta.eta, H4Ris.kappa.epsilon.zeta.chi = H4Ris.kappa.epsilon.zeta.chi, H4Ris.kappa.epsilon.zeta.psi = H4Ris.kappa.epsilon.zeta.psi, H4Ris.kappa.epsilon.eta.chi = H4Ris.kappa.epsilon.eta.chi,
                                        H4Ris.kappa.epsilon.eta.psi = H4Ris.kappa.epsilon.eta.psi, H4Ris.kappa.epsilon.chi.psi = H4Ris.kappa.epsilon.chi.psi, H4Ris.kappa.zeta.eta.chi = H4Ris.kappa.zeta.eta.chi, H4Ris.kappa.zeta.eta.psi = H4Ris.kappa.zeta.eta.psi, H4Ris.kappa.zeta.chi.psi = H4Ris.kappa.zeta.chi.psi, H4Ris.kappa.eta.chi.psi = H4Ris.kappa.eta.chi.psi, H4Ris.omega.epsilon.zeta.eta = H4Ris.omega.epsilon.zeta.eta,
                                        H4Ris.omega.epsilon.zeta.chi = H4Ris.omega.epsilon.zeta.chi, H4Ris.omega.epsilon.zeta.psi = H4Ris.omega.epsilon.zeta.psi, H4Ris.omega.epsilon.eta.chi = H4Ris.omega.epsilon.eta.chi, H4Ris.omega.epsilon.eta.psi = H4Ris.omega.epsilon.eta.psi, H4Ris.omega.epsilon.chi.psi = H4Ris.omega.epsilon.chi.psi, H4Ris.omega.zeta.eta.chi = H4Ris.omega.zeta.eta.chi, H4Ris.omega.zeta.eta.psi = H4Ris.omega.zeta.eta.psi,
                                        H4Ris.omega.zeta.chi.psi = H4Ris.omega.zeta.chi.psi, H4Ris.omega.eta.chi.psi = H4Ris.omega.eta.chi.psi, H4Ris.epsilon.zeta.eta.chi = H4Ris.epsilon.zeta.eta.chi, H4Ris.epsilon.zeta.eta.psi = H4Ris.epsilon.zeta.eta.psi, H4Ris.epsilon.zeta.chi.psi = H4Ris.epsilon.zeta.chi.psi, H4Ris.epsilon.eta.chi.psi = H4Ris.epsilon.eta.chi.psi, H4Ris.zeta.eta.chi.psi = H4Ris.zeta.eta.chi.psi,
                                        H5Ris.kappa.omega.epsilon.zeta.eta = H5Ris.kappa.omega.epsilon.zeta.eta, H5Ris.kappa.omega.epsilon.zeta.chi = H5Ris.kappa.omega.epsilon.zeta.chi, H5Ris.kappa.omega.epsilon.zeta.psi = H5Ris.kappa.omega.epsilon.zeta.psi, H5Ris.kappa.omega.epsilon.eta.chi = H5Ris.kappa.omega.epsilon.eta.chi, H5Ris.kappa.omega.epsilon.eta.psi = H5Ris.kappa.omega.epsilon.eta.psi, H5Ris.kappa.omega.epsilon.chi.psi = H5Ris.kappa.omega.epsilon.chi.psi,
                                        H5Ris.kappa.omega.zeta.eta.chi = H5Ris.kappa.omega.zeta.eta.chi, H5Ris.kappa.omega.zeta.eta.psi = H5Ris.kappa.omega.zeta.eta.psi, H5Ris.kappa.omega.zeta.chi.psi = H5Ris.kappa.omega.zeta.chi.psi, H5Ris.kappa.omega.eta.chi.psi = H5Ris.kappa.omega.eta.chi.psi, H5Ris.kappa.epsilon.zeta.eta.chi = H5Ris.kappa.epsilon.zeta.eta.chi, H5Ris.kappa.epsilon.zeta.eta.psi = H5Ris.kappa.epsilon.zeta.eta.psi,
                                        H5Ris.kappa.epsilon.zete.chi.psi = H5Ris.kappa.epsilon.zete.chi.psi, H5Ris.kappa.epsilon.eta.chi.psi = H5Ris.kappa.epsilon.eta.chi.psi, H5Ris.kappa.zeta.eta.chi.psi = H5Ris.kappa.zeta.eta.chi.psi, H5Ris.omega.epsilon.zeta.eta.chi = H5Ris.omega.epsilon.zeta.eta.chi, H5Ris.omega.epsilon.zeta.eta.psi = H5Ris.omega.epsilon.zeta.eta.psi, H5Ris.omega.epsilon.zeta.chi.psi = H5Ris.omega.epsilon.zeta.chi.psi,
                                        H5Ris.omega.epsilon.eta.chi.psi = H5Ris.omega.epsilon.eta.chi.psi, H5Ris.omega.zeta.eta.chi.psi = H5Ris.omega.zeta.eta.chi.psi, H5Ris.epsilon.zeta.eta.chi.psi = H5Ris.epsilon.zeta.eta.chi.psi,
                                        H6Ris.kappa.omega.epsilon.zeta.eta.chi = H6Ris.kappa.omega.epsilon.zeta.eta.chi, H6Ris.kappa.omega.epsilon.zeta.eta.psi = H6Ris.kappa.omega.epsilon.zeta.eta.psi, H6Ris.kappa.omega.epsilon.zeta.chi.psi = H6Ris.kappa.omega.epsilon.zeta.chi.psi, H6Ris.kappa.omega.epsilon.eta.chi.psi = H6Ris.kappa.omega.epsilon.eta.chi.psi, H6Ris.kappa.omega.zeta.eta.chi.psi = H6Ris.kappa.omega.zeta.eta.chi.psi,
                                        H6Ris.kappa.epsilon.zeta.eta.chi.psi = H6Ris.kappa.epsilon.zeta.eta.chi.psi, H6Ris.omega.epsilon.zeta.eta.chi.psi = H6Ris.omega.epsilon.zeta.eta.chi.psi)
                      , comparison = "none", type = "gorica")


goricaResult <- goric(RIvar, VCOV = RIVcov, 
                       hypotheses = list(HRi = HRi, HCl = HCl, H6Ris.kappa.epsilon.zeta.eta.chi.psi = H6Ris.kappa.epsilon.zeta.eta.chi.psi, H6Ris.omega.epsilon.zeta.eta.chi.psi = H6Ris.omega.epsilon.zeta.eta.chi.psi)
                       , comparison = "unconstrained", type = "gorica")


