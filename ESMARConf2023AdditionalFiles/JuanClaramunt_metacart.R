###Data metacart package
library(metacart)
set.seed(1234)
#Simulated datasets
data("dat.balanced")
data("SimData")

#Real data
data(dat.BCT2009)
?dat.BCT2009

###Fixed effects model

#Main function
?FEmrt
FEtree <- FEmrt(g ~ T1 + T2+ T4 + T25, vi = vi, data = dat.BCT2009, c = 0)

#Options
##Select only a subset
#Select rows
FEtree2 <- FEmrt(g ~ T1 + T2+ T4 + T25, subset = 5:50, vi = vi, data = dat.BCT2009, c = 0)
#use logical expression
FEtree3 <- FEmrt(g ~ T1 + T2+ T4 + T25, subset = g > 0.5 , vi = vi, data = dat.BCT2009, c = 0)

FEtree2
FEtree3

##c: pruning parameter
FEtree4 <- FEmrt(g ~ T1 + T2+ T4 + T25, vi = vi, data = dat.BCT2009, c = 0)

FEtree5 <- FEmrt(g ~ T1 + T2+ T4 + T25, vi = vi, data = dat.BCT2009, c = 0.5)

FEtree4
FEtree5

##control (rpart)
FEtree6 <- FEmrt(g ~ T1 + T2+ T4 + T25, vi = vi, data = dat.BCT2009, c = 0,
                 control = rpart.control(xval = 10, minbucket = 10, minsplit = 10, cp = 1e-04))


#Output functions
print(FEtree)

summary(FEtree)

plot(FEtree)




###Random effect model

#Main function
?REmrt
REtree <- REmrt(g ~ T1 + T2+ T4 +T25, vi = vi, data = dat.BCT2009, c = 0)

#Options
##c: pruning parameter
REtree2 <- REmrt(g ~ T1 + T2+ T4 +T25, vi = vi, data = dat.BCT2009, c = 0.1)

REtree
REtree2

##Control parameters are incorporated (maxL, minsplit, cp, minbucket, xval)
REtree3 <- REmrt(g ~ T1 + T2+ T4 +T25, vi = vi, data = dat.BCT2009, c = 0,
                maxL = 8,
                minsplit = 10,
                cp = 1e-04,
                minbucket = 5,
                xval = 10)


##Lookahead
REtree4 <- REmrt(g ~ T1 + T2+ T4 +T25, vi = vi, data = dat.BCT2009, c = 0.1,
                 lookahead = FALSE)

#Output functions
print(REtree)

summary(REtree)

plot(REtree)


