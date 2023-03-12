#######################################################################
#  Effect size type:
#  Standardized Mean Difference 
#
#######################################################################
#######################################################################
#  Step 1: Data preparation with the package metavcov
#######################################################################
library("metavcov")
# load and print the data
Geeganage2010 <- as.data.frame(Geeganage2010) 
subset(Geeganage2010, select = c(nt_SBP, nt_DBP,nc_SBP, nc_DBP,SMD_SBP, SMD_DBP))
## set the correlation coefficients list r
r12 <- 0.71
r.Gee <- lapply(1:nrow(Geeganage2010), function(i){matrix(c(1, r12, r12, 1), 2, 2)})
## compute the variance-covariance matrix 
computvcov <- smd.vcov(nt = subset(Geeganage2010, select = c(nt_SBP, nt_DBP)),
                       nc = subset(Geeganage2010, select = c(nc_SBP, nc_DBP)),
                       d = subset(Geeganage2010, select = c(SMD_SBP, SMD_DBP)),
                       r = r.Gee,
                       name = c("SBP", "DBP"))
head(computvcov$ef)            ## Hedge's g
head(computvcov$matrix.vcov)   ## variance-covariances for Hedge's g (matrix)
computvcov$list.vcov           ## variance-covariances for Hedge's g (list)



#######################################################################
#  Step 2: Fixed Effect Meta-Analysis with the package metavcov or mixmeta
#######################################################################
## from the package "metavcov"
y <- computvcov$ef
Slist <- computvcov$list.vcov
MMA_FE <- summary(metafixed(y = y, Slist = Slist))
MMA_FE

## An alternative way
library(mixmeta)
S <- computvcov$matrix.vcov
MMA_FE <- summary(mixmeta(cbind(SBP,DBP)~1,
                         S = S, data = y, method = "fixed"))
MMA_FE
#######################################################################
#  Step 2: Random Effect Meta-Analysis with the package mixmeta or metaSEM
#######################################################################
## from the package "mixmeta"
MMA_RE <- summary(mixmeta(cbind(SBP,DBP)~1,
                         S = S, data = y, method = "reml"))
MMA_RE

## An alternative way
library(metaSEM)
MMA_RE <- summary(meta(y = y, v = S, data = data.frame(y,S))	)
MMA_RE

#######################################################################
#  Step 3: Visualization with the package metavcov
#######################################################################
obj <- MMA_FE
## from the package "metavcov"

# pdf("CI.pdf", width = 4, height = 7)

plotCI(y = computvcov$ef, v = computvcov$list.vcov,
       name.y = NULL, name.study = Geeganage2010$studyID,
       y.all = obj$coefficients[,1],
       y.all.se = obj$coefficients[,2],
       up.bound = Inf, low.bound = -Inf)

 # dev.off()
#######################################################################
#   Effect size type:
#   Correlation Coefficients 
#
#######################################################################
#######################################################################
#  Step 1: Data preparation with the package metavcov
#######################################################################
data(Craft2003)
computvcov <- r.vcov(n = Craft2003$N,
                     corflat = subset(Craft2003, select = C1:C6))
y <- computvcov$ef
S <- computvcov$matrix.vcov

#######################################################################
#  Step 2: Random Effect Meta-Analysis with the package mixmeta or metaSEM
#######################################################################
## from the package "mixmeta"
MMA_RE <- summary(mixmeta(cbind(C1, C2, C3, C4, C5, C6)~1,
                         S = S, data = y, method = "reml"))
## meta-regression
summary(mixmeta(cbind(C1, C2, C3, C4, C5, C6)~ p_male,
               S = S, data = data.frame(y, p_male = Craft2003$p_male),
               method = "reml"))

## An alternative way
## from the package "metaSEM"

MMA_RE <- summary(meta(y = y, v = S, data = data.frame(y,S))	)
MMA_RE

#######################################################################
#   
#   Correlation Coefficients with Multiple Imputation for Missing Data
#
#######################################################################

#######################################################################
# Generating data with missing values 
#######################################################################
Craft2003.mnar <- Craft2003[, c(2, 4:10)]
Craft2003.mnar[sample(which(Craft2003$C4 < 0), 6), "C4"] <- NA
dat <- Craft2003.mnar

#######################################################################
# Steps 1-3: fixed effect model with the package metavcov
#######################################################################
n.name <- "N"
ef.name <- c("C1", "C2", "C3", "C4", "C5", "C6")

o1 <- metami(dat, M = 2, vcov = "r.vcov",        ## M = 2 for fast demonstration; it should be M = 20
             n.name, ef.name,
             func = "metafixed")
## make a plot
computvcov <- r.vcov(n = Craft2003$N,
                     corflat = subset(Craft2003.mnar, select = C1:C6),
                     method = "average")
plotCI(y = computvcov$ef, v = computvcov$list.vcov,
       name.y = NULL, name.study = Craft2003$ID,
       y.all = o1$coefficients[,1],
       y.all.se = o1$coefficients[,2])

#######################################################################
# Steps 1-2: random effect model with packages metavcov and metaSEM
#######################################################################

# maximum likelihood estimators from the metaSEM package
#library(metaSEM)
o2 <- metami(dat, M = 2, vcov = "r.vcov",        ## M = 2 for fast demonstration; it should be M = 20
             n.name, ef.name,
             func = "meta")

# meta-regression
o3 <- metami(dat, M = 2, vcov = "r.vcov",        ## M = 2 for fast demonstration; it should be M = 20
             n.name, ef.name, x.name = "p_male",
             func = "meta")

#######################################################################
#  Effect size type:
#  Combination of All Effect Sizes with mix.vcov() from metavcov
#
#######################################################################
# use 4 outcomes/effect sizes from Geeganage2010
## set the correlation coefficients list r
r12 <- 0.71
r13 <- 0.5
r14 <- 0.25
r23 <- 0.6
r24 <- 0.16
r34 <- 0.16
r <- vecTosm(c(r12, r13, r14, r23, r24, r34))
diag(r) <- 1
mix.r <- lapply(1:nrow(Geeganage2010), function(i){r})
attach(Geeganage2010)
## compute variances and  covariances

#######################################################################
#  Step 1: Data preparation with the package metavcov
#######################################################################
computvcov <- mix.vcov(type = c("MD", "MD", "RD", "lgOR"),
                       d = cbind(MD_SBP, MD_DBP, NA, NA),
                       sdt = cbind(sdt_SBP, sdt_DBP, NA, NA),
                       sdc = cbind(sdc_SBP, sdc_DBP, NA, NA),
                       nt = cbind(nt_SBP, nt_DBP, nt_DD, nt_D),
                       nc = cbind(nc_SBP, nc_DBP, nc_DD, nc_D),
                       st = cbind(NA, NA, st_DD, st_D),
                       sc = cbind(NA, NA, sc_DD, sc_D),
                       r = mix.r,
                       name = c("MD.SBP", "MD.DBP", "RD.DD", "lgOR.D"))
detach(Geeganage2010)
# save different effect sizes in y
y <- computvcov$ef
head(y)
# save variances and covariances of all the effect sizes in a matrix S
S <- computvcov$matrix.vcov
head(S)

# use y and S for steps 2-3 in the same way as before