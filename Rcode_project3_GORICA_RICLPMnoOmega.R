# Required packages
library(dplyr)
library(sqldf)
library(lavaan) # to apply riclpmModel
library(restriktor) # to call GORICA

#load dataset
setwd("C:/Users/csukp/OneDrive/เดสก์ท็อป/Project3/Project 3-20230822T115025Z-001/Project 3/R code/Project3")
data <- read.csv("QuestionaireT1toT3_NP.csv") 

Psy.fac <- data[, c(20:25,33,51:64)]
Psy.fac[1:21] <- replace(Psy.fac[1:21], Psy.fac[1:21] == -999.00, NA)
colnames(Psy.fac)<-c("a1", "b1", "c1","d1", "e1", "f1", "g1",
                     "a2", "a3", "b2","b3", "c2", "c3", "d2",
                     "d3", "e2", "e3","f2", "f3", "g2", "g3")

#standardizing
Psy.fac <- scale(Psy.fac) 

# hypotheses
H1a <- "abs(ba) > abs(ab); abs(dg) > abs(gd)"
H2a <- "abs(ba) < abs(ab); abs(dg) < abs(gd)"
H3a <- "abs(ba) > abs(ab); abs(dg) < abs(gd)"
H4a <- "abs(ba) < abs(ab); abs(dg) > abs(gd)"

# Fitting a model (RICLPM (kappa, epsilon, zeta, eta, chi, psi), without omega)

riclpmModel <-
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
  omega ~~ 0*omega #variance
  epsilon ~~ epsilon #variance
  zeta ~~ zeta #variance
  eta ~~ eta #variance
  chi ~~ chi #variance
  psi ~~ psi #variance
  kappa ~~ 0*omega #covariance
  kappa ~~ epsilon #covariance
  kappa ~~ zeta #covariance
  kappa ~~ eta #covariance
  kappa ~~ chi #covariance
  kappa ~~ psi #covariance
  omega ~~ 0*epsilon #covariance
  omega ~~ 0*zeta #covariance
  omega ~~ 0*eta #covariance
  omega ~~ 0*chi #covariance
  omega ~~ 0*psi #covariance
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

riclpm.No.omega <- lavaan(riclpmModel, data = Psy.fac,
                          missing = 'ML', #for the missing data!
                          int.ov.free = F,
                          int.lv.free = F,
                          auto.fix.first = F,
                          auto.fix.single = F,
                          auto.cov.lv.x = F,
                          auto.cov.y = F,
                          auto.var = F)

#subtract parameter estimates and variance-covariance matix
est <- coef(riclpm.No.omega)[c(51,65,91,131)]
vcov <- lavInspect(riclpm.No.omega, "vcov")[c(51,65,91,131), c(51,65,91,131)]

#evaluate hypotheses using GORICA
goricaResult1 <- goric(est, VCOV = vcov, hypotheses = list(H1a = H1a), comparison = "complement", type = "gorica")
goricaResult2 <- goric(est, VCOV = vcov, hypotheses = list(H1a = H1a, H2a = H2a, H3a = H3a, H4a = H4a), comparison = "unconstrained", type = "gorica")
goricaResult3 <- goric(est, VCOV = vcov, hypotheses = list(H3a = H3a), comparison = "complement", type = "gorica")
