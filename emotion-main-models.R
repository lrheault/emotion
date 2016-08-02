#!/usr/bin/Rscript

#===========================================================================================#
# 
#   Description:
#   R script for empirical analysis --- Models and figures 
#
#   Usage: 
#   For interactive computing in IDE.
#
#   1) Set working directory using setwd() below:
setwd("/home/user")
#
#   2) Requires data files emotion-final-y.csv and emotion-final-q.csv in wd.
#
#   3) Output files will be saved in a subdirectory named of/ in the wd.
#
#   Computed using R 3.2.4.
#
#   Author: L. Rheault
#
#===========================================================================================#

#-------------------------------------------------------------------------------------------#
# Loading required packages
#-------------------------------------------------------------------------------------------#
# Note: Installation of those packages will be required if they are not already.

library(zoo) # For basic time-series tools.
library(urca) # For unit root testing.
library(fUnitRoots) # For unit root testing.
library(TSA) # For ARIMAX models.
library(vars) # For vector autoregression models (VAR, VECM).
library(nlWaldTest) # Non-linear Wald Tests.
library(lmtest) # For tests of serial correlation.
library(tseries) # For tests such as normality of residuals.
library(quantmod) # For an alternative lag function.
library(tsDyn) # For vector autoregression models (VAR, VECM).
library(sandwich) # For HAC standard errors.
library(aod) # For flexible Wald tests used for Granger causality analysis.
library(forecast) # For automatic lag selection in ARIMA models and other tools.

# For plotting:
library(ggplot2) 
library(reshape2)
library(colorRamps)
library(gridExtra)
#-------------------------------------------------------------------------------------------#
# Note: Some figures below include transparency, incompatible with eps format.
# Save figures as tiff or pdf to see intended result, or else reset alpha=1 to save as eps.
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Loading data
#-------------------------------------------------------------------------------------------#

dy <- read.csv("emotion-final-y.csv")
dq <- read.csv("emotion-final-q.csv")

#-------------------------------------------------------------------------------------------#
# Table 3. Mean difference tests (t-tests).
#-------------------------------------------------------------------------------------------#

# 1) The usual t-test, one-sided following theoretical expectation.
# 2) Two-sided t-test.
# 3) Welch t-test with unequal group variances.

# Recession binary variable
t.test(dpolar~recession, data = dy, var.equal=TRUE, alternative = "greater")
t.test(dpolar~recession, data = dy, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~recession, data = dy, var.equal=FALSE, alternative = "greater")

# Election binary variable
t.test(dpolar~election, data = dy, var.equal=TRUE, alternative = "less")
t.test(dpolar~election, data = dy, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~election, data = dy, var.equal=FALSE, alternative = "less")

# Wars binary variable
t.test(dpolar~wars, data = dy, var.equal=TRUE, alternative = "greater")
t.test(dpolar~wars, data = dy, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~wars, data = dy, var.equal=FALSE, alternative = "greater")

#-------------------------------------------------------------------------------------------#
# Quarterly Version

# Recession binary variable
t.test(dpolar~recession, data = dq, var.equal=TRUE, alternative = "greater")
t.test(dpolar~recession, data = dq, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~recession, data = dq, var.equal=FALSE, alternative = "greater")

# Election binary variable
t.test(dpolar~election, data = dq, var.equal=TRUE, alternative = "less")
t.test(dpolar~election, data = dq, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~election, data = dq, var.equal=FALSE, alternative = "less")

# Wars binary variable
t.test(dpolar~wars, data = dq, var.equal=TRUE, alternative = "greater")
t.test(dpolar~wars, data = dq, var.equal=TRUE, alternative = "two.sided")
t.test(dpolar~wars, data = dq, var.equal=FALSE, alternative = "greater")

#-------------------------------------------------------------------------------------------#
# Tables S5, S6, S12, S13.  Unit root tests.
#-------------------------------------------------------------------------------------------#
# Will save six output files in /of directory.

pol <- ts(dy$polar,start=c(1909),frequency=1, names="Polarity")
disp <- ts(dy$ldisp,start=c(1909),frequency=1, names="Disputes")
mis <- ts(dy$misery,start=c(1909),frequency=1, names="Misery")
gdpg <- ts(dy$gdpg,start=c(1909),frequency=1, names="GDPg")
unemp <- ts(dy$unemp,start=c(1909),frequency=1, names="Unemployment")
polg <- ts(dy$polarg[38:nrow(dy)],start=c(1946),frequency=1, names="Polarity-Gov")
polo <- ts(dy$polaro[38:nrow(dy)],start=c(1946),frequency=1, names="Polarity-Opp")
series <- c("pol","polg","polo","disp","mis","gdpg","unemp")

#Storing results in matrices.
uradf <- matrix(nrow=14,ncol=7)
colnames(uradf)<-c("Statistic","1pct","5pct","10pct","Lag","Model","Outcome")
rownames(uradf) <- c(rep("Polarity",2), rep("Polarity-Gov",2), rep("Polarity-Opp",2),
                     rep("Disputes",2), rep("Misery",2), rep("GDPg",2), rep("Unemployment",2))
urkpss <- matrix(nrow=14,ncol=8)
colnames(urkpss)<-c("Statistic","10pct","5pct","2.5pct","1pct","Type","Lag","Outcome")
rownames(urkpss) <- c(rep("Polarity",2), rep("Polarity-Gov",2), rep("Polarity-Opp",2),
                      rep("Disputes",2), rep("Misery",2), rep("GDPg",2), rep("Unemployment",2))

for (l in 1:2){
    j = 1
    for (i in 1:length(series)){
        adf1 <- ur.df(get(series[i]) , type = "drift", lags=l)
        adf2 <- ur.df(get(series[i]) , type = "trend", lags=l)
        uradf[j,1:4] <- cbind(adf1@teststat[1], t(adf1@cval[1,1:3])); uradf[j,5]<-adf1@lags; uradf[j,6]<-adf1@model;
        uradf[j+1,1:4] <- cbind(adf2@teststat[1], t(adf2@cval[1,1:3])); uradf[j+1,5]<-adf2@lags; uradf[j+1,6]<-adf2@model;
        j = j+2  
    }
    for (i in 1:nrow(uradf)){
        if (as.numeric(uradf[i,1])<=as.numeric(uradf[i,3])){  
            uradf[i,7] <- "Stationary"
        }
        else {
            uradf[i,7] <- "Non-Stationary"
        }
    }
    write.table(uradf,paste("of/unitroot-y-adfs-",l,".csv",sep=""),sep=",",row.names=T,col.names=NA)
} 

j = 1
for (i in 1:length(series)){
    kpss1 <- ur.kpss(get(series[i]), type = "mu", lags="short")
    kpss2 <- ur.kpss(get(series[i]), type = "tau", lags="short")
    urkpss[j,1:7] <- cbind(kpss1@teststat, kpss1@cval, kpss1@type, kpss1@lag) 
    urkpss[j+1,1:7] <- cbind(kpss2@teststat, kpss2@cval, kpss2@type, kpss2@lag)
    j = j+2 
}
rm(j) 
for (i in 1:nrow(urkpss)){
  if (as.numeric(urkpss[i,1])>as.numeric(urkpss[i,3])){  
    urkpss[i,8] <- "Non-Stationary"
  }
  else {
    urkpss[i,8] <- "Stationary"
  } 
}
write.table(urkpss,"of/unitroot-y-kpss.csv",sep=",",row.names=T,col.names=NA)

#-------------------------------------------------------------------------------------------#
# Quarterly Version

pol <- ts(dq$polar,start=c(1909, 1),frequency=4, names="Polarity")
disp <- ts(dq$ldisp[89:nrow(dq)],start=c(1931, 1),frequency=4, names="Disputes")
mis <- ts(dq$misery[186:nrow(dq)],start=c(1955, 2),frequency=4, names="Misery")
gdpg <- ts(dq$gdpg[186:nrow(dq)],start=c(1955, 2),frequency=4, names="GDPg")
unemp <- ts(dq$unemp[185:nrow(dq)],start=c(1955, 1),frequency=4, names="Unemployment")
polg <- ts(dq$polarg[147:nrow(dq)],start=c(1945, 3),frequency=4, names="Polarity-Gov")
polo <- ts(dq$polaro[147:nrow(dq)],start=c(1945, 3),frequency=4, names="Polarity-Opp")
series <- c("pol","polg","polo","disp","mis","gdpg","unemp")

#Storing results in matrices.
uradf <- matrix(nrow=14,ncol=7)
colnames(uradf)<-c("Statistic","1pct","5pct","10pct","Lag","Model","Outcome")
rownames(uradf) <- c(rep("Polarity",2), rep("Polarity-Gov",2), rep("Polarity-Opp",2),
                     rep("Disputes",2), rep("Misery",2), rep("GDPg",2), rep("Unemployment",2))
urkpss <- matrix(nrow=14,ncol=8)
colnames(urkpss)<-c("Statistic","10pct","5pct","2.5pct","1pct","Type","Lag","Outcome")
rownames(urkpss) <- c(rep("Polarity",2), rep("Polarity-Gov",2), rep("Polarity-Opp",2),
                      rep("Disputes",2), rep("Misery",2), rep("GDPg",2), rep("Unemployment",2))

for (l in 4:5){
  j = 1
  for (i in 1:length(series)){
    adf1 <- ur.df(get(series[i]) , type = "drift", lags=l)
    adf2 <- ur.df(get(series[i]) , type = "trend", lags=l)
    uradf[j,1:4] <- cbind(adf1@teststat[1], t(adf1@cval[1,1:3])); uradf[j,5]<-adf1@lags; uradf[j,6]<-adf1@model;
    uradf[j+1,1:4] <- cbind(adf2@teststat[1], t(adf2@cval[1,1:3])); uradf[j+1,5]<-adf2@lags; uradf[j+1,6]<-adf2@model;
    j = j+2  
  }
  for (i in 1:nrow(uradf)){
    if (as.numeric(uradf[i,1])<=as.numeric(uradf[i,3])){  
      uradf[i,7] <- "Stationary"
    }
    else {
      uradf[i,7] <- "Non-Stationary"
    }
  }
  write.table(uradf,paste("of/unitroot-q-adfs-",l,".csv",sep=""),sep=",",row.names=T,col.names=NA)
} 

j = 1
for (i in 1:length(series)){
  kpss1 <- ur.kpss(get(series[i]), type = "mu", lags="short")
  kpss2 <- ur.kpss(get(series[i]), type = "tau", lags="short")
  urkpss[j,1:7] <- cbind(kpss1@teststat, kpss1@cval, kpss1@type, kpss1@lag) 
  urkpss[j+1,1:7] <- cbind(kpss2@teststat, kpss2@cval, kpss2@type, kpss2@lag)
  j = j+2 
}
rm(j) 
for (i in 1:nrow(urkpss)){
  if (as.numeric(urkpss[i,1])>as.numeric(urkpss[i,3])){  
    urkpss[i,8] <- "Non-Stationary"
  }
  else {
    urkpss[i,8] <- "Stationary"
  } 
}
write.table(urkpss,"of/unitroot-q-kpss.csv",sep=",",row.names=T,col.names=NA)

#-------------------------------------------------------------------------------------------#
# Tables 2, S10, S15. ARIMA models
#-------------------------------------------------------------------------------------------#

dpol <- ts(dy$dpolar[2:nrow(dy)],start=c(1910),frequency=1, names="Polarity")
X <- ts(cbind(dy$recession[2:nrow(dy)],dy$election[2:nrow(dy)]),start=c(1910),frequency=1, 
                names=c("Recession","Election"))

a1 <- arimax(dpol, order = c(1, 0, 0), xreg= X)

a2 <- arimax(dpol, order = c(2, 0, 0), xreg= X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 1: Arima(1,0,0)")
a1
AIC(a1)
BIC(a1)
a1$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a1))
cat("Normality test:")
jarque.bera.test(resid(a1))
cat("ARMAX model 2: Arima(2,0,0)")
a2
AIC(a2)
BIC(a2)
a2$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a2))
cat("Normality test:")
jarque.bera.test(resid(a2))
cat(paste(replicate(72, "-"),collapse = ""))


X <- ts(cbind(dy$recession[2:nrow(dy)], dy$election[2:nrow(dy)], 
              dy$wars[2:nrow(dy)]),
        start=c(1909),frequency=1, 
        names=c("Recession","Election","Wars"))
a3 <- arimax(dpol, order = c(1, 0, 0), xreg= X)
a4 <- arimax(dpol, order = c(2, 0, 0), xreg= X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 3")
a3
AIC(a3)
BIC(a3)
a3$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a3))
cat("Normality test:")
jarque.bera.test(resid(a3))
cat("ARMAX model 4")
a4
AIC(a4)
BIC(a4)
a4$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a4))
cat("Normality test:")
jarque.bera.test(resid(a4))
cat(paste(replicate(72, "-"),collapse = ""))

dpol <- ts(dy$dpolar[38:nrow(dy)],start=c(1946),frequency=1, names="Polarity")
X <- ts(cbind(dy$recession[38:nrow(dy)], dy$election[38:nrow(dy)], 
              dy$wars[38:nrow(dy)], dy$conservative[38:nrow(dy)]),
        start=c(1946),frequency=1, 
        names=c("Recession","Election","Wars", "Party"))
a5 <- arimax(dpol, order = c(1, 0, 0), xreg= X)
a6 <- arimax(dpol, order = c(2, 0, 0), xreg= X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 5")
a5
AIC(a5)
BIC(a5)
a5$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a5))
cat("Normality test:")
jarque.bera.test(resid(a5))
cat("ARMAX model 6")
a6
AIC(a6)
BIC(a6)
a6$loglik
# Stability test: Uncomment to perform.  Requires function arroots from forecast package.
# plot(arroots(a6))
cat("Normality test:")
jarque.bera.test(resid(a6))
cat(paste(replicate(72, "-"),collapse = ""))

#-------------------------------------------------------------------------------------------#
# Quarterly Version; In SI only.

dpol <- ts(dq$dpolar[45:nrow(dq)],start=c(1920,2),frequency=4, names="Polarity")
X <- ts(cbind(dq$recession[45:nrow(dq)],dq$election[45:nrow(dq)]),start=c(1920,2),frequency=4, 
        names=c("Recession","Election"))

# Automatic selection based on IC (package forecast required.)
auto.arima(dpol,xreg=X)
# Best model: (3,0,0)(2,0,0)[4]

a1 <- arima( dpol, order = c(3, 0, 0), seasonal = list(order = c(2,0,0), period = 4), xreg=X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 1: Arima(3,0,0)(2,0,0)[4]")
a1
AIC(a1)
BIC(a1)
a1$loglik
cat(paste(replicate(72, "-"),collapse = ""))

X <- ts(cbind(dq$recession[45:nrow(dq)], dq$election[45:nrow(dq)], dq$wars[45:nrow(dq)]),
        start=c(1920,2),frequency=4,
        names=c("Recession","Election", "Wars"))
a2 <- arima( dpol, order = c(3, 0, 0), seasonal = list(order = c(2,0,0), period = 4), xreg=X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 2")
a2
AIC(a2)
BIC(a2)
a2$loglik
cat(paste(replicate(72, "-"),collapse = ""))

dpol <- ts(dq$dpolar[186:nrow(dq)],start=c(1955,2),frequency=4, names="Polarity")
X <- ts(cbind(dq$recession[186:nrow(dq)], dq$election[186:nrow(dq)], dq$wars[186:nrow(dq)],
        dq$conservatives[186:nrow(dq)]), 
        start=c(1955,2),frequency=4, names=c("Recession","Election", "Wars", "Party"))
a3 <- arima( dpol, order = c(3, 0, 0), seasonal = list(order = c(2,0,0), period = 4), xreg=X)
cat(paste(replicate(72, "-"),collapse = ""))
cat("ARMAX model 3")
a3
AIC(a3)
BIC(a3)
a3$loglik
cat(paste(replicate(72, "-"),collapse = ""))

#-------------------------------------------------------------------------------------------#
# Table 4, Table S9, Table S14. Granger Causality/Cointegration
#-------------------------------------------------------------------------------------------#
# Will save 8 output files in the \of directory.

# Declaring variables as multivariate time series.
pol.disp <- ts(cbind(dy$polar, dy$ldisp), start=c(1909), frequency=1, names=c("Polarity", "Disputes"))
pol.mis <- ts(cbind(dy$polar, dy$misery), start=c(1909), frequency=1, names=c("Polarity", "Misery"))
pol.gdpg <- ts(cbind(dy$polar, dy$gdpg), start=c(1909), frequency=1, names=c("Polarity", "GDPg"))
pol.unemp <- ts(cbind(dy$polar, dy$unemp), start=c(1909), frequency=1, names=c("Polarity", "Unemp"))
# Government only.
polg.disp <- ts(cbind(dy$polarg, dy$ldisp)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Gov", "Disputes"))
# Opposition only.
polo.disp <- ts(cbind(dy$polaro, dy$ldisp)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Opp", "Disputes"))

series <- c("pol.disp","pol.mis","pol.gdpg","pol.unemp", 
            "polg.disp","polo.disp")

# Automatic lag length selection (Bayesian criterion)
bc <- matrix(nrow=length(series),ncol=1)
for (i in 1:nrow(bc)){
  bc[i] <- VARselect(get(series[i]),lag=12,type="const")$selection[3]
}

# Automatic lag length selection (Hannan-Quinn criterion)
hq <- matrix(nrow=length(series),ncol=1)
for (i in 1:nrow(hq)){
  hq[i] <- VARselect(get(series[i]),lag=12,type="const")$selection[2]
}

gc <- matrix(ncol=4,nrow=2*length(series))
colnames(gc) <- c("Chi-2","Lags","p-value","Outcome")
rownames(gc) <- c("Disputes -> Polarity", "Polarity -> Disputes",  
                  "Misery -> Polarity", "Polarity -> Misery",
                  "GDPg -> Polarity", "Polarity -> GDPg",
                  "Unemployment -> Polarity", "Polarity -> Unemployment",
                  "Disputes -> Polarity-Gov", "PolarityGov -> Disputes",  
                  "Disputes -> Polarity-Opp", "Polarity-Opp -> Disputes")

j <- 1
for (i in 1:length(series)){
  tempvar <- VAR(get(series[i]),p=bc[i]+1,type="const")
  gc[j,1:3] <- wald.test(b=coef(tempvar$varresult[[1]]), 
                         Sigma=vcov(tempvar$varresult[[1]]), 
                         Terms=seq(from=2,to=bc[i]*2,by=2), df=bc[i])$result[[1]]
  gc[j+1,1:3] <- wald.test(b=coef(tempvar$varresult[[2]]), 
                           Sigma=vcov(tempvar$varresult[[2]]), 
                           Terms=seq(from=1,to=bc[i]*2-1,by=2), df=bc[i])$result[[1]]
  j = j + 2
}
rm(j)
for (i in 1:nrow(gc)){
  if (as.numeric(gc[i,3])<0.05){  
    gc[i,4] <- "*"
  }
  else {
    gc[i,4] <- ""
  }
}
write.table(gc,"of/y-granger-bic.csv",sep=",",row.names=T,col.names=NA)
j <- 1
for (i in 1:length(series)){
  tempvar <- VAR(get(series[i]),p=hq[i]+1,type="const")
  gc[j,1:3] <- wald.test(b=coef(tempvar$varresult[[1]]), 
                         Sigma=vcov(tempvar$varresult[[1]]), 
                         Terms=seq(from=2,to=hq[i]*2,by=2), df=hq[i])$result[[1]]
  gc[j+1,1:3] <- wald.test(b=coef(tempvar$varresult[[2]]), 
                           Sigma=vcov(tempvar$varresult[[2]]), 
                           Terms=seq(from=1,to=hq[i]*2-1,by=2), df=hq[i])$result[[1]]
  j = j + 2
}
rm(j)
for (i in 1:nrow(gc)){
  if (as.numeric(gc[i,3])<0.05){  
    gc[i,4] <- "*"
  }
  else {
    gc[i,4] <- ""
  }
}
write.table(gc,"of/y-granger-hq.csv",sep=",",row.names=T,col.names=NA)

# Cointegration tests (Johansen rank tests).
# Restricting to pairs of series that are both I(1) according to the ADF tests:
polg.mis <- ts(cbind(dy$polarg, dy$misery)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Gov", "Misery"))
polg.unemp <- ts(cbind(dy$polarg, dy$unemp)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Gov", "Unemp"))
polo.mis <- ts(cbind(dy$polaro, dy$misery)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Opp", "Misery"))
polo.unemp <- ts(cbind(dy$polaro, dy$unemp)[38:nrow(dy),], start=c(1946), frequency=1, names=c("Polarity-Opp", "Unemp"))

series <- c("pol.disp","pol.mis","pol.unemp", 
            "polg.disp","polg.mis","polg.unemp",
            "polo.disp","polo.mis","polo.unemp")

co <- matrix(nrow=18,ncol=6)
colnames(co) <- c("trace","p-val","var1","var2","lags","outcome")
rownames(co) <- c("r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1",
                  "r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1",
                  "r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1")

j <- 1
for (i in 1:length(series)){
  temp.vecm <- suppressWarnings( VECM(get(series[i]), 0, LRinclude="const", estim="ML") )
  jo <- rank.test(temp.vecm)
  co[j:(j+1),1] <- jo[["res_df"]]$trace; co[j:(j+1),2] <- jo[["res_df"]]$trace_pval
  co[j,3:4] <- attributes(get(series[i]))$dimnames[[2]]; co[j+1,3:4] <- attributes(get(series[i]))$dimnames[[2]]
  co[j,5] <- 1; co[(j+1),5] <- 1
  j = j + 2
}
rm(j)
for (i in 1:nrow(co)){
  if (as.numeric(co[i,2])<0.05){  
    co[i,6] <- "Cointegrated"
  }
  else {
    co[i,6] <- "Not Cointegrated"
  }
}
write.table(co,"of/y-coint-1lag.csv",sep=",",row.names=T,col.names=NA)

j <- 1
for (i in 1:length(series)){
  temp.vecm <- suppressWarnings( VECM(get(series[i]), 1, LRinclude="const", estim="ML") )
  jo <- rank.test(temp.vecm)
  co[j:(j+1),1] <- jo[["res_df"]]$trace; co[j:(j+1),2] <- jo[["res_df"]]$trace_pval
  co[j,3:4] <- attributes(get(series[i]))$dimnames[[2]]; co[j+1,3:4] <- attributes(get(series[i]))$dimnames[[2]]
  co[j,5] <- 2; co[(j+1),5] <- 2
  j = j + 2
}
rm(j)
for (i in 1:nrow(co)){
  if (as.numeric(co[i,2])<0.05){  
    co[i,6] <- "Cointegrated"
  }
  else {
    co[i,6] <- "Not Cointegrated"
  }
}
write.table(co,"of/y-coint-2lags.csv",sep=",",row.names=T,col.names=NA)


#-------------------------------------------------------------------------------------------#
# Quarterly Version

# Declaring variables as multivariate time series.
pol.disp <- ts(cbind(dq$polar,dq$ldisp)[89:nrow(dq),], start=c(1931, 1),frequency=4, names=c("Polarity", "Disputes"))
pol.mis <- ts(cbind(dq$polar,dq$misery)[186:nrow(dq),], start=c(1955, 2),frequency=4, names=c("Polarity", "Misery"))
pol.gdpg <- ts(cbind(dq$polar,dq$gdpg)[186:nrow(dq),], start=c(1955, 2),frequency=4, names=c("Polarity", "GDPg"))
pol.unemp <- ts(cbind(dq$polar,dq$unemp)[185:nrow(dq),], start=c(1955, 1),frequency=4, names=c("Polarity", "Unemp"))
# Government only.
polg.disp <- ts(cbind(dq$polarg,dq$ldisp)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("Polarity-gov", "Disputes"))
# Opposition only.
polo.disp <- ts(cbind(dq$polaro,dq$ldisp)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("Polarity-opp", "Disputes"))

series <- c("pol.disp","pol.mis","pol.gdpg","pol.unemp",  
            "polg.disp","polo.disp") 

# Automatic lag length selection (Bayesian criterion)
bc <- matrix(nrow=length(series),ncol=1)
for (i in 1:nrow(bc)){
  bc[i] <- VARselect(get(series[i]),lag=12,type="const")$selection[3]
}

# Automatic lag length selection (Hannan-Quinn criterion)
hq <- matrix(nrow=length(series),ncol=1)
for (i in 1:nrow(hq)){
  hq[i] <- VARselect(get(series[i]),lag=12,type="const")$selection[2]
}

gc <- matrix(ncol=4,nrow=2*length(series))
colnames(gc) <- c("Chi-2","Lags","p-value","Outcome")
rownames(gc) <- c("Disputes -> Polarity", "Polarity -> Disputes",  
                  "Misery -> Polarity", "Polarity -> Misery",
                  "GDPg -> Polarity", "Polarity -> GDPg",
                  "Unemployment -> Polarity", "Polarity -> Unemployment",
                  "Disputes -> Polarity-Gov", "PolarityGov -> Disputes",  
                  "Disputes -> Polarity-Opp", "Polarity-Opp -> Disputes")

j <- 1
for (i in 1:length(series)){
  tempvar <- VAR(get(series[i]),p=bc[i]+1,type="const")
  gc[j,1:3] <- wald.test(b=coef(tempvar$varresult[[1]]), 
                         Sigma=vcov(tempvar$varresult[[1]]), 
                         Terms=seq(from=2,to=bc[i]*2,by=2), df=bc[i])$result[[1]]
  gc[j+1,1:3] <- wald.test(b=coef(tempvar$varresult[[2]]), 
                           Sigma=vcov(tempvar$varresult[[2]]), 
                           Terms=seq(from=1,to=bc[i]*2-1,by=2), df=bc[i])$result[[1]]
  j = j + 2
}
rm(j)
for (i in 1:nrow(gc)){
  if (as.numeric(gc[i,3])<0.05){  
    gc[i,4] <- "*"
  }
  else {
    gc[i,4] <- ""
  }
}
write.table(gc,"of/q-granger-bic.csv",sep=",",row.names=T,col.names=NA)
j <- 1
for (i in 1:length(series)){
  tempvar <- VAR(get(series[i]),p=hq[i]+1,type="const")
  gc[j,1:3] <- wald.test(b=coef(tempvar$varresult[[1]]), 
                         Sigma=vcov(tempvar$varresult[[1]]), 
                         Terms=seq(from=2,to=hq[i]*2,by=2), df=hq[i])$result[[1]]
  gc[j+1,1:3] <- wald.test(b=coef(tempvar$varresult[[2]]), 
                           Sigma=vcov(tempvar$varresult[[2]]), 
                           Terms=seq(from=1,to=hq[i]*2-1,by=2), df=hq[i])$result[[1]]
  j = j + 2
}
rm(j)
for (i in 1:nrow(gc)){
  if (as.numeric(gc[i,3])<0.05){  
    gc[i,4] <- "*"
  }
  else {
    gc[i,4] <- ""
  }
}
write.table(gc,"of/q-granger-hq.csv",sep=",",row.names=T,col.names=NA)

# Cointegration tests (Johansen rank tests).
# Restricting to pairs of series that are both I(1), at least according to the ADF tests:
# Government only.
polg.mis <- ts(cbind(dq$polarg,dq$misery)[186:nrow(dq),], start=c(1955, 2),frequency=4, names=c("Polarity-gov", "Misery"))
polg.unemp <- ts(cbind(dq$polarg,dq$unemp)[185:nrow(dq),], start=c(1955, 1),frequency=4, names=c("Polarity-gov", "Unemp"))
# Opposition only.
polo.mis <- ts(cbind(dq$polaro,dq$misery)[186:nrow(dq),], start=c(1955, 2),frequency=4, names=c("Polarity-opp", "Misery"))
polo.unemp <- ts(cbind(dq$polaro,dq$unemp)[185:nrow(dq),], start=c(1955, 1),frequency=4, names=c("Polarity-opp", "Unemp"))

series <- c("pol.disp","pol.mis","pol.unemp", 
            "polg.disp","polg.mis","polg.unemp",
            "polo.disp","polo.mis","polo.unemp")


co <- matrix(nrow=18,ncol=6)
colnames(co) <- c("trace","p-val","var1","var2","lags","outcome")
rownames(co) <- c("r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1",
                  "r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1",
                  "r = 0","r = 1", "r = 0","r = 1", "r = 0","r = 1")

j <- 1
for (i in 1:length(series)){
  temp.vecm <- suppressWarnings( VECM(get(series[i]), 4, include="const", estim="ML") )
  jo <- rank.test(temp.vecm)
  co[j:(j+1),1] <- jo[["res_df"]]$trace; co[j:(j+1),2] <- jo[["res_df"]]$trace_pval
  co[j,3:4] <- attributes(get(series[i]))$dimnames[[2]]; co[j+1,3:4] <- attributes(get(series[i]))$dimnames[[2]]
  co[j,5] <- 5; co[(j+1),5] <- 5 
  j = j + 2
}
rm(j)
for (i in 1:nrow(co)){
  if (as.numeric(co[i,2])<0.05){  
    co[i,6] <- "Cointegrated"
  }
  else {
    co[i,6] <- ""
  }
}
write.table(co,"of/q-coint-5lags.csv",sep=",",row.names=T,col.names=NA)

j <- 1
for (i in 1:length(series)){
  temp.vecm <- suppressWarnings( VECM(get(series[i]), 5, include="const", estim="ML") )
  jo <- rank.test(temp.vecm)
  co[j:(j+1),1] <- jo[["res_df"]]$trace; co[j:(j+1),2] <- jo[["res_df"]]$trace_pval
  co[j,3:4] <- attributes(get(series[i]))$dimnames[[2]]; co[j+1,3:4] <- attributes(get(series[i]))$dimnames[[2]]
  co[j,5] <- 6; co[(j+1),5] <- 6 
  j = j + 2
}
rm(j)
for (i in 1:nrow(co)){
  if (as.numeric(co[i,2])<0.05){  
    co[i,6] <- "Cointegrated"
  }
  else {
    co[i,6] <- ""
  }
}
write.table(co,"of/q-coint-6lags.csv",sep=",",row.names=T,col.names=NA)

#-------------------------------------------------------------------------------------------#
# Tables S11, S16; Figures 3, S5, and main text: Vector Error Correction Models (VECMs)  
#-------------------------------------------------------------------------------------------#
# VECM via tsDyn package

#-------------------------------------------------------------------------------------------#
# Yearly Version
pol.disp <- ts(cbind(dy$polar,dy$ldisp), start=c(1909),frequency=1, names=c("Polarity","Disputes"))
polg.disp <- ts(cbind(dy$polarg,dy$ldisp)[38:nrow(dy),], start=c(1946),frequency=1, names=c("PolarityGov","Disputes"))
polo.disp <- ts(cbind(dy$polaro,dy$ldisp)[38:nrow(dy),], start=c(1946),frequency=1, names=c("PolarityOpp","Disputes"))

pol.disp.exg <- ts(cbind(dy$polar,dy$ldisp)[38:nrow(dy),], start=c(1946),frequency=1, names=c("Polarity","Disputes"))
exogvar <- ts(cbind(dy$conservative,dy$election)[38:nrow(dy),], start=c(1946),frequency=1, names=c("Party", "Elections"))

ll0 <- VARselect(pol.disp,lag=12,type="const")$selection[3]
ll1 <- VARselect(polg.disp,lag=12,type="const")$selection[3]
ll2 <- VARselect(polo.disp,lag=12,type="const")$selection[3]

# Note: VECM uses lags in difference as input, as opposed to lags in levels obtained from VARselect.
# For the purposes of this study, we use 1 lag in first differences.
vecm.00 <- VECM(pol.disp, ll0, include = "const", r=1, estim="ML")
vecm.01 <- VECM(pol.disp.exg, ll0, include = "const", r=1, estim="ML", exogen=exogvar)
vecm.1 <- VECM(polg.disp, ll1, include = "const", r=1, estim="ML")
vecm.2 <- VECM(polo.disp, ll2, include = "const", r=1, estim="ML")
summary(vecm.00)
summary(vecm.01)
summary(vecm.1)
summary(vecm.2)

### Reorder variables from exogenous to endogenous in the Cholesky decomposition.
pol.disp <- ts(cbind(dy$ldisp,dy$polar), start=c(1909),frequency=1, names=c("Disputes","Polarity"))
pol.disp.exg <- ts(cbind(dy$ldisp,dy$polar)[38:nrow(dy),], start=c(1946),frequency=1, names=c("Disputes","Polarity"))
polg.disp <- ts(cbind(dy$ldisp,dy$polarg)[38:nrow(dy),], start=c(1946),frequency=1, names=c("Disputes","PolarityGov"))
polo.disp <- ts(cbind(dy$ldisp,dy$polaro)[38:nrow(dy),], start=c(1946),frequency=1, names=c("Disputes","PolarityOpp"))
vecm.00 <- VECM(pol.disp, ll0, include = "const", r=1, estim="ML")
vecm.01 <- VECM(pol.disp.exg, ll0, include = "const", r=1, estim="ML", exogen=exogvar)
vecm.1 <- VECM(polg.disp, ll1, include = "const", r=1, estim="ML")
vecm.2 <- VECM(polo.disp, ll2, include = "const", r=1, estim="ML")

# Impulse response functions.
ir <- irf(vecm.00, boot = T, impulse = "Disputes", response="Polarity", ortho=T,
          cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],Polarity=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = Polarity), size = 2, colour="#002A48") + 
  geom_line(aes(y = LB), linetype="dashed", size = 0.5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = 0.5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  labs(x="Time",y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0))) 

ir <- irf(vecm.01, boot = T, impulse = "Disputes", response="Polarity", ortho=T,
          cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],Polarity=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = Polarity), size = 2, colour="#002A48") + 
  geom_line(aes(y = LB), linetype="dashed", size = .5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = .5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  labs(x="Time",y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0))) 

# Impulse response functions (Government Only, S5 A)
ir <- irf(vecm.1, boot = T, impulse = "Disputes", response="PolarityGov", 
          ortho=T,cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],PolarityGov=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
irfp1 <- ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = PolarityGov), size = 2, colour="#002A48") + 
  geom_line(aes(y = LB), linetype="dashed", size = .5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = .5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  labs(x=expression(atop("Time", paste("A. Polarity of Government (Yearly)"))),y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0))) +
  scale_y_continuous(lim=c(-0.7,0.1))   

# Impulse response functions (Opposition Only, S5 B))
ir <- irf(vecm.2, boot = T, impulse = "Disputes", response="PolarityOpp", ortho=T,
          cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],PolarityOpp=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
irfp2 <- ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = PolarityOpp), size = 2, colour="#002A48") + 
  geom_line(aes(y = LB), linetype="dashed", size = .5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = .5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  labs(x=expression(atop("Time", paste("B. Polarity of Opposition (Yearly)"))),y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0))) +
  scale_y_continuous(lim=c(-0.7,0.1))   

#-------------------------------------------------------------------------------------------#
# Quarterly Version

pol.disp <- ts(cbind(dq$polar,dq$ldisp)[89:nrow(dq),], start=c(1931, 1),frequency=4, names=c("Polarity","Disputes"))
polg.disp <- ts(cbind(dq$polarg,dq$ldisp)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("PolarityGov","Disputes"))
polo.disp <- ts(cbind(dq$polaro,dq$ldisp)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("PolarityOpp","Disputes"))

ll0 <- VARselect(pol.disp,lag=10,type="const")$selection[3]
ll1 <- VARselect(polg.disp,lag=10,type="const")$selection[3]
ll2 <- VARselect(polo.disp,lag=10,type="const")$selection[3]

# We augment number of lags at 4 (5 in levels) to cover full cycle of periodicity.
vecm.0 <- VECM(pol.disp, 4, include = "const", r=1, estim="ML")
vecm.1 <- VECM(polg.disp, 4, include = "const", r=1, estim="ML")
vecm.2 <- VECM(polo.disp, 4, include = "const", r=1, estim="ML")
summary(vecm.0)
summary(vecm.1)
summary(vecm.2)

### Reorder variables from exogenous to endogenous in the Cholesky decomposition.
pol.disp <- ts(cbind(dq$ldisp,dq$polar)[89:nrow(dq),], start=c(1931, 1),frequency=4, names=c("Disputes","Disputes"))
polg.disp <- ts(cbind(dq$ldisp,dq$polarg)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("Disputes","PolarityGov"))
polo.disp <- ts(cbind(dq$ldisp,dq$polaro)[147:nrow(dq),], start=c(1945, 3),frequency=4, names=c("Disputes","PolarityOpp"))
vecm.0 <- VECM(pol.disp, 4, include = "const", r=1, estim="ML")
vecm.1 <- VECM(polg.disp, 4, include = "const", r=1, estim="ML")
vecm.2 <- VECM(polo.disp, 4, include = "const", r=1, estim="ML")

# Computing impulse responses.
# Impulse response functions (Government Only, Fig S5 C and D)
ir <- irf(vecm.1, boot = T, impulse = "Disputes", response="PolarityGov", 
          ortho=T,cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],PolarityGov=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
irfp3 <- ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = PolarityGov), size = 2, colour="#002A48") + 
  geom_line(aes(y = LB), linetype="dashed", size = .5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = .5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  labs(x=expression(atop("Time", paste("C. Polarity of Government (Quarterly)"))),y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0))) +
  scale_y_continuous(lim=c(-0.7,0.1))   

# Impulse response functions (Opposition Only, SI6(d))
ir <- irf(vecm.2, boot = T, impulse = "Disputes", response="PolarityOpp", 
          ortho=T,cumulative = FALSE, n.ahead = 40, seed=42)
irdata <- data.frame(time=1:lengths(ir$irf)[[1]],PolarityOpp=ir$irf[[1]],
                     LB=as.vector(ir$Lower[[1]]),UB=as.vector(ir$Upper[[1]]))
irfp4 <- ggplot(irdata, aes(x = time)) + theme_bw() + 
  geom_ribbon(aes(ymin = LB, ymax = UB), fill = "#BFC9D1") + 
  geom_line(aes(y = LB), linetype="dashed", size = .5, colour="#002A48") +
  geom_line(aes(y = UB), linetype="dashed", size = .5, colour="#002A48") +
  geom_hline(aes(yintercept=0), size = .5, colour="#3f3f3f") +
  geom_line(aes(y = PolarityOpp), size = 2, colour="#002A48") + 
  labs(x=expression(atop("Time", paste("D. Polarity of Opposition (Quarterly)"))),y="Response") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)))  +
  scale_y_continuous(lim=c(-0.7,0.1))   

grid.arrange(irfp1, irfp2, irfp3, irfp4, ncol = 2)


#-------------------------------------------------------------------------------------------#
# Dynamic OLS (DOLS) Models - Not in Text 
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Custom lag function (this version based on StackOverflow snippet)

l<-function(x,shift_by){
  if (length(shift_by)>1)
    return(sapply(shift_by,l, x=x))
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

# First-Differencing function
d1 <- function(x) {
  x-l(x,-1) 
}
#-------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------#
# Yearly Version

dols <- matrix(nrow=12,ncol=6)
colnames(dols) <- c("Delta Leads/Lags = 1", "Delta Leads/Lags = 2",
                    "Delta Leads/Lags = 1", "Delta Leads/Lags = 2",
                    "Delta Leads/Lags = 1", "Delta Leads/Lags = 2")
rownames(dols) <- c("Labor Disputes","","Misery Index","","Unemployment","","Intercept","",
                    "","N","Adj-R2","sigma")

# Main Polarity Measure and Labour Disputes

dols.1 <- lm(polar ~ ldisp + l(d1(ldisp),-1:1), data=dy)
dols.2 <- lm(polar ~ ldisp + l(d1(ldisp),-2:2), data=dy)
coeftest(dols.1, vcov=vcovHAC)
coeftest(dols.2, vcov=vcovHAC)
dols[1:2,1] = t(coeftest(dols.1, vcov=vcovHAC)[2,1:2])
dols[7:8,1] = t(coeftest(dols.1, vcov=vcovHAC)[1,1:2])
dols[1:2,2] = t(coeftest(dols.2, vcov=vcovHAC)[2,1:2])
dols[7:8,2] = t(coeftest(dols.2, vcov=vcovHAC)[1,1:2])
dols[10,1] <- nobs(dols.1)
dols[11,1] <- summary(dols.1)$adj.r.squared
dols[12,1] <- summary(dols.1)$sigma
dols[10,2] <- nobs(dols.2)
dols[11,2] <- summary(dols.2)$adj.r.squared
dols[12,2] <- summary(dols.2)$sigma

# Main Polarity Measure and Misery Index

dols.3 <- lm(polar ~ misery + l(d1(misery),-1:1), data=dy)
dols.4 <- lm(polar ~ misery + l(d1(misery),-2:2), data=dy)
coeftest(dols.3, vcov=vcovHAC)
coeftest(dols.4, vcov=vcovHAC)
dols[3:4,3] = t(coeftest(dols.3, vcov=vcovHAC)[2,1:2])
dols[7:8,3] = t(coeftest(dols.3, vcov=vcovHAC)[1,1:2])
dols[3:4,4] = t(coeftest(dols.4, vcov=vcovHAC)[2,1:2])
dols[7:8,4] = t(coeftest(dols.4, vcov=vcovHAC)[1,1:2])
dols[10,3] <- nobs(dols.3)
dols[11,3] <- summary(dols.3)$adj.r.squared
dols[12,3] <- summary(dols.3)$sigma
dols[10,4] <- nobs(dols.4)
dols[11,4] <- summary(dols.4)$adj.r.squared
dols[12,4] <- summary(dols.4)$sigma

# Main Polarity Measure and Unemployment

dols.5 <- lm(polar ~ unemp + l(d1(unemp),-1:1), data=dy)
dols.6 <- lm(polar ~ unemp + l(d1(unemp),-2:2), data=dy)
summary(dols.5)
summary(dols.6)
dols[5:6,5] = t(coeftest(dols.5, vcov=vcovHAC)[2,1:2])
dols[7:8,5] = t(coeftest(dols.5, vcov=vcovHAC)[1,1:2])
dols[5:6,6] = t(coeftest(dols.6, vcov=vcovHAC)[2,1:2])
dols[7:8,6] = t(coeftest(dols.6, vcov=vcovHAC)[1,1:2])
dols[10,5] <- nobs(dols.5)
dols[11,5] <- summary(dols.5)$adj.r.squared
dols[12,5] <- summary(dols.5)$sigma
dols[10,6] <- nobs(dols.6)
dols[11,6] <- summary(dols.6)$adj.r.squared
dols[12,6] <- summary(dols.6)$sigma

write.table(dols,"of/y-dols.csv",sep=",",row.names=T,col.names=NA,na="")

#-------------------------------------------------------------------------------------------#
# Quarterly Version

dols <- matrix(nrow=12,ncol=6)
colnames(dols) <- c("Delta Leads/Lags = 1", "Delta Leads/Lags = 4",
                    "Delta Leads/Lags = 1", "Delta Leads/Lags = 4",
                    "Delta Leads/Lags = 1", "Delta Leads/Lags = 4")
rownames(dols) <- c("Labor Disputes","","Misery Index","","Unemployment","","Intercept","",
                    "","N","Adj-R2","sigma")

# Main Polarity Measure and Labour Disputes

dols.1 <- lm(polar ~ ldisp + l(d1(ldisp),-1:1), data=dq)
dols.2 <- lm(polar ~ ldisp + l(d1(ldisp),-4:4), data=dq)
coeftest(dols.1, vcov=vcovHAC)
coeftest(dols.2, vcov=vcovHAC)
dols[1:2,1] = t(coeftest(dols.1, vcov=vcovHAC)[2,1:2])
dols[7:8,1] = t(coeftest(dols.1, vcov=vcovHAC)[1,1:2])
dols[1:2,2] = t(coeftest(dols.2, vcov=vcovHAC)[2,1:2])
dols[7:8,2] = t(coeftest(dols.2, vcov=vcovHAC)[1,1:2])
dols[10,1] <- nobs(dols.1)
dols[11,1] <- summary(dols.1)$adj.r.squared
dols[12,1] <- summary(dols.1)$sigma
dols[10,2] <- nobs(dols.2)
dols[11,2] <- summary(dols.2)$adj.r.squared
dols[12,2] <- summary(dols.2)$sigma

# Main Polarity Measure and Misery Index

dols.3 <- lm(polar ~ misery + l(d1(misery),-1:1), data=dq)
dols.4 <- lm(polar ~ misery + l(d1(misery),-4:4), data=dq)
coeftest(dols.3, vcov=vcovHAC)
coeftest(dols.4, vcov=vcovHAC)
dols[3:4,3] = t(coeftest(dols.3, vcov=vcovHAC)[2,1:2])
dols[7:8,3] = t(coeftest(dols.3, vcov=vcovHAC)[1,1:2])
dols[3:4,4] = t(coeftest(dols.4, vcov=vcovHAC)[2,1:2])
dols[7:8,4] = t(coeftest(dols.4, vcov=vcovHAC)[1,1:2])
dols[10,3] <- nobs(dols.3)
dols[11,3] <- summary(dols.3)$adj.r.squared
dols[12,3] <- summary(dols.3)$sigma
dols[10,4] <- nobs(dols.4)
dols[11,4] <- summary(dols.4)$adj.r.squared
dols[12,4] <- summary(dols.4)$sigma

# Main Polarity Measure and Unemployment

dols.5 <- lm(polar ~ unemp + l(d1(unemp),-1:1), data=dq)
dols.6 <- lm(polar ~ unemp + l(d1(unemp),-4:4), data=dq)
summary(dols.5)
summary(dols.6)
dols[5:6,5] = t(coeftest(dols.5, vcov=vcovHAC)[2,1:2])
dols[7:8,5] = t(coeftest(dols.5, vcov=vcovHAC)[1,1:2])
dols[5:6,6] = t(coeftest(dols.6, vcov=vcovHAC)[2,1:2])
dols[7:8,6] = t(coeftest(dols.6, vcov=vcovHAC)[1,1:2])
dols[10,5] <- nobs(dols.5)
dols[11,5] <- summary(dols.5)$adj.r.squared
dols[12,5] <- summary(dols.5)$sigma
dols[10,6] <- nobs(dols.6)
dols[11,6] <- summary(dols.6)$adj.r.squared
dols[12,6] <- summary(dols.6)$sigma

write.table(dols,"of/q-dols.csv",sep=",",row.names=T,col.names=NA,na="")

#-------------------------------------------------------------------------------------------#
# Single Equation Error Correction Models (ECM) - Not in Text
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Yearly Version

# Main Polarity Measure and Labour Disputes

ecms <- matrix(nrow=22,ncol=3)
colnames(ecms) <- c("Labor Disputes Model", "Misery Index Model",
                    "Unemployment Model")
rownames(ecms) <- c("Lag-Polarity","",
                    "Diff-Labor Disputes","","Lag-Labor Disputes","",
                    "Diff-Misery Index","","Lag-Misery Index","",
                    "Diff-Unemployment","","Lag-Unemployment","",
                    "Intercept","","","Long-Run Effect","",
                    "N","Adj-R2","sigma")


ecm.1 <- lm(d1(polar) ~ d1(ldisp) + l(polar,-1) + l(ldisp,-1), data=dy)
coeftest(ecm.1, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.1)[[4]]/coef(ecm.1)[[3]]
ecms[1:2,1] <- t(coeftest(ecm.1, vcov=vcovHAC)[3,1:2])
ecms[3:4,1] <- t(coeftest(ecm.1, vcov=vcovHAC)[2,1:2])
ecms[5:6,1] <- t(coeftest(ecm.1, vcov=vcovHAC)[4,1:2])
ecms[15:16,1] <- t(coeftest(ecm.1, vcov=vcovHAC)[1,1:2])
ecms[18,1] <- lr
ecms[20,1] <- nobs(ecm.1)
ecms[21,1] <- summary(ecm.1)$adj.r.squared
ecms[22,1] <- summary(ecm.1)$sigma

# Main Polarity Measure and Misery Index

ecm.2 <- lm(d1(polar) ~ d1(misery) +  l(polar,-1) + l(misery,-1), data=dy)
coeftest(ecm.2, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.2)[[4]]/coef(ecm.2)[[3]]
ecms[1:2,2] = t(coeftest(ecm.2, vcov=vcovHAC)[3,1:2])
ecms[7:8,2] = t(coeftest(ecm.2, vcov=vcovHAC)[2,1:2])
ecms[9:10,2] = t(coeftest(ecm.2, vcov=vcovHAC)[4,1:2])
ecms[15:16,2] = t(coeftest(ecm.2, vcov=vcovHAC)[1,1:2])
ecms[18,2] = lr
ecms[20,2] <- nobs(ecm.2)
ecms[21,2] <- summary(ecm.2)$adj.r.squared
ecms[22,2] <- summary(ecm.2)$sigma

# Main Polarity Measure and Unemployment

ecm.3 <- lm(d1(polar) ~ d1(unemp) + l(polar,-1) + l(unemp,-1), data=dy)
coeftest(ecm.3, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.3)[[4]]/coef(ecm.3)[[3]]
ecms[1:2,3] = t(coeftest(ecm.3, vcov=vcovHAC)[3,1:2])
ecms[11:12,3] = t(coeftest(ecm.3, vcov=vcovHAC)[2,1:2])
ecms[13:14,3] = t(coeftest(ecm.3, vcov=vcovHAC)[4,1:2])
ecms[15:16,3] = t(coeftest(ecm.3, vcov=vcovHAC)[1,1:2])
ecms[18,3] = lr
ecms[20,3] <- nobs(ecm.3)
ecms[21,3] <- summary(ecm.3)$adj.r.squared
ecms[22,3] <- summary(ecm.3)$sigma

write.table(ecms,"of/y-ecms.csv",sep=",",row.names=T,col.names=NA,na="")

#-------------------------------------------------------------------------------------------#
# Quarterly Version

# Main Polarity Measure and Labour Disputes

ecms <- matrix(nrow=22,ncol=3)
colnames(ecms) <- c("Labor Disputes Model", "Misery Index Model",
                    "Unemployment Model")
rownames(ecms) <- c("Lag-Polarity","",
                    "Diff-Labor Disputes","","Lag-Labor Disputes","",
                    "Diff-Misery Index","","Lag-Misery Index","",
                    "Diff-Unemployment","","Lag-Unemployment","",
                    "Intercept","","","Long-Run Effect","",
                    "N","Adj-R2","sigma")


ecm.1 <- lm(d1(polar) ~ d1(ldisp) + l(polar,-1) + l(ldisp,-1), data=dq)
coeftest(ecm.1, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.1)[[4]]/coef(ecm.1)[[3]]
ecms[1:2,1] = t(coeftest(ecm.1, vcov=vcovHAC)[3,1:2])
ecms[3:4,1] = t(coeftest(ecm.1, vcov=vcovHAC)[2,1:2])
ecms[5:6,1] = t(coeftest(ecm.1, vcov=vcovHAC)[4,1:2])
ecms[15:16,1] = t(coeftest(ecm.1, vcov=vcovHAC)[1,1:2])
ecms[18,1] = lr
ecms[20,1] <- nobs(ecm.1)
ecms[21,1] <- summary(ecm.1)$adj.r.squared
ecms[22,1] <- summary(ecm.1)$sigma

# Main Polarity Measure and Misery Index

ecm.2 <- lm(d1(polar) ~ d1(misery) +  l(polar,-1) + l(misery,-1), data=dq)
coeftest(ecm.2, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.2)[[4]]/coef(ecm.2)[[3]]
ecms[1:2,2] = t(coeftest(ecm.2, vcov=vcovHAC)[3,1:2])
ecms[7:8,2] = t(coeftest(ecm.2, vcov=vcovHAC)[2,1:2])
ecms[9:10,2] = t(coeftest(ecm.2, vcov=vcovHAC)[4,1:2])
ecms[15:16,2] = t(coeftest(ecm.2, vcov=vcovHAC)[1,1:2])
ecms[18,2] = lr
ecms[20,2] <- nobs(ecm.2)
ecms[21,2] <- summary(ecm.2)$adj.r.squared
ecms[22,2] <- summary(ecm.2)$sigma

# Main Polarity Measure and Unemployment

ecm.3 <- lm(d1(polar) ~ d1(unemp) + l(polar,-1) + l(unemp,-1), data=dq)
coeftest(ecm.3, vcov=vcovHAC)
# Long-run effect:
lr <- -coef(ecm.3)[[4]]/coef(ecm.3)[[3]]
ecms[1:2,3] = t(coeftest(ecm.3, vcov=vcovHAC)[3,1:2])
ecms[11:12,3] = t(coeftest(ecm.3, vcov=vcovHAC)[2,1:2])
ecms[13:14,3] = t(coeftest(ecm.3, vcov=vcovHAC)[4,1:2])
ecms[15:16,3] = t(coeftest(ecm.3, vcov=vcovHAC)[1,1:2])
ecms[18,3] = lr
ecms[20,3] <- nobs(ecm.3)
ecms[21,3] <- summary(ecm.3)$adj.r.squared
ecms[22,3] <- summary(ecm.3)$sigma

write.table(ecms,"of/q-ecms.csv",sep=",",row.names=T,col.names=NA,na="")


# Testing a model with longer lag length (not reported in text)

ecm.1 <- lm(d1(polar) ~ d1(ldisp) + l(polar,-4:-1) + l(ldisp,-4:-1), data=dq)
coeftest(ecm.1, vcov=vcovHAC)
# Long-run effect:
- (coef(ecm.1)[[7]] + coef(ecm.1)[[7]] + coef(ecm.1)[[7]] + coef(ecm.1)[[7]]) / 
  (coef(ecm.1)[[3]] + coef(ecm.1)[[4]] + coef(ecm.1)[[5]] + coef(ecm.1)[[6]])

#-------------------------------------------------------------------------------------------#
# Figure 1. Emotional Polarity 
#-------------------------------------------------------------------------------------------#

mblue <- "#002A48"
pblue <- "#3F5F75"

#-------------------------------------------------------------------------------------------#
# Yearly

dy$polsm <- smooth.spline(dy$polar,spar=.5)$y
p1a <- ggplot(dy, aes(x = year)) + theme_bw() +  
  geom_line(aes(y = polar), size = .1, colour=pblue) +
  geom_point(aes(y = polar), size = 6, colour=pblue, shape=1, stroke=.25) +
  geom_line(aes(y = polsm), size = 2, colour=mblue) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  labs(x="A. Year", y="Emotional Polarity") +
  scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))  +
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3),limits = c(-2.5,3.5)) +
  geom_hline(aes(yintercept=0), linetype="dashed", size = .25, colour="#3f3f3f")

#-------------------------------------------------------------------------------------------#
# Quarterly

dq$polsm <- smooth.spline(dq$polar)$y
dq$time <- 1:420
p1b <-  ggplot(dq, aes(x = time)) + theme_bw() +  
  geom_line(aes(y = polar), size = .1, colour=pblue) +
  geom_point(aes(y = polar), size = 6, colour=pblue, shape=1, stroke=.25) +
  geom_line(aes(y = polsm), size = 2, colour=mblue) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  labs(x="B. Quarter",y="Emotional Polarity") +
  scale_x_continuous(breaks=c(5,85,165,245,325,405),
                     labels=c("1910-Q1","1930-Q1","1950-Q1","1970-Q1","1990-Q1","2010-Q1"))  +
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3),limits = c(-2.5,3.5)) +
  geom_hline(aes(yintercept=0), linetype="dashed", size = .25, colour="#3f3f3f")

grid.arrange(p1a, p1b, ncol = 1)

#-------------------------------------------------------------------------------------------#
# Figure 2. Emotional Polarity by Government/Opposition
#-------------------------------------------------------------------------------------------#

dys <- dy[38:nrow(dy),]
dys$polgsm <- smooth.spline(dys$polarg_graph,spar=.5)$y
dys$polosm <- smooth.spline(dys$polaro_graph,spar=.5)$y

ggplot(dys, aes(x = year)) + theme_bw() +  
  geom_line(aes(y = polarg_graph), size = .1, colour="#3F5F75") +
  geom_point(aes(y = polarg_graph), size = 6, colour="#3F5F75", shape=1, stroke=.25) +
  geom_line(aes(y = polaro_graph), size = .1, colour="#3FA8C3") +
  geom_point(aes(y = polaro_graph), size = 6, colour="#3FA8C3", shape=1, stroke=.25) +
  geom_line(aes(y = polgsm, colour="Government"), size = 2) +
  geom_line(aes(y = polosm, colour="Opposition"), size = 2) +
  scale_colour_manual(name="", values=c(Government="#002A48",Opposition="#008BB0")) +
  theme(legend.position=c(0.25,0.8),
        legend.text = element_text(size = 10),
        legend.key.height=unit(1.5,"line"),
        legend.key.size=unit(2.5,"line"),
        legend.key = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
  labs(x="Year",y="Emotional Polarity") +
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010))  


#-------------------------------------------------------------------------------------------#
# Figure S1: Autocorrelation Functions
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Yearly

poly <- ts(dy$polar,start=c(1909),freq=1)

conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(poly))
acfy <- acf(poly, plot = TRUE, lag.max=20)
dacfy <- data.frame(acf=acfy[[1]][2:21],lag=1:20)

pacf1 <- ggplot(data=dacfy, mapping=aes(x=lag, y=acf)) + theme_bw() +
  geom_bar(stat = "identity", position = "identity", colour="#002A48", fill="#002A48", width=.7, size=.1) + 
  labs(x=expression(atop("Lags", paste("A. Yearly"))),y="Autocorrelation Function") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_y_continuous(breaks=c(0.00,0.25,0.50,0.75,1.00), lim=c(0,1)) +
  geom_hline(aes(yintercept=(-ciline)), linetype="dashed", size = .5, colour="#000000") 

#-------------------------------------------------------------------------------------------#
# Quarterly

polq <- ts(dq$polar,start=c(1909,1),freq=4)

conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(polq))
acfq <- acf(polq, plot = TRUE, lag.max=40)
dacfq <- data.frame(acf=acfq[[1]][2:41],lag=1:40)

pacf2 <- ggplot(data=dacfq, mapping=aes(x=lag, y=acf)) + theme_bw() +
  geom_bar(stat = "identity", position = "identity", colour="#002A48", fill="#002A48", width=.7, size=.1) + 
  labs(x=expression(atop("Lags", paste("B. Quarterly"))) ,y="Autocorrelation Function") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_y_continuous(breaks=c(0.00,0.25,0.50,0.75,1.00), lim=c(0,1)) +
  geom_hline(aes(yintercept=(-ciline)), linetype="dashed", size = .5, colour="#000000")  

grid.arrange(pacf1, pacf2, ncol = 1)

#-------------------------------------------------------------------------------------------#
# Figure S2: Power Spectral Densities
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Yearly

x <- dy$polar
N <- nrow(dy)
yspec <- (abs(fft(x))^2/(2*pi*N))[2:(((N+1)/2)+1)]
yfreq = seq(1,(N+1)/2)*(2*pi/N)
plot(log(yspec)~log(yfreq),type="l") 
regalpha <- lm(log(yspec)~log(yfreq))
# Coefficients mentioned in the text (alpha, CI lower bound, CI upper bound):
-coef(regalpha)[[2]]; confint(regalpha)[2,2]*-1; confint(regalpha)[2,1]*-1 
powsy = data.frame(frequency=log(yfreq),spectrum=log(yspec))

psp1 <- ggplot(powsy, aes(x = frequency)) + theme_bw() +  
  geom_line(aes(y = spectrum, colour="#b20000", size = 2),colour="#002A48", size = 1.5) +
  labs(x=expression(atop("Log Frequency", paste("A. Yearly"))),y="Log Power Spectrum") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_y_continuous(lim=c(-10,3))   

#-------------------------------------------------------------------------------------------#
# Quarterly

x <- dq$polar
N <- nrow(dq)
qspec <- (abs(fft(x))^2/(2*pi*N))[2:((N/2)+1)]
qfreq = seq(1,N/2)*(2*pi/N)
plot(log(qspec)~log(qfreq),type="l") 
regalpha <- lm(log(qspec)~log(qfreq))
# Coefficients mentioned in the text (alpha, CI lower bound, CI upper bound):
-coef(regalpha)[[2]]; confint(regalpha)[2,2]*-1; confint(regalpha)[2,1]*-1 
powsq = data.frame(frequency=log(qfreq),spectrum=log(qspec))

psp2 <- ggplot(powsq, aes(x = frequency)) + theme_bw() +  
  geom_line(aes(y = spectrum, colour="#002A48", size = 2),colour="#002A48", size = 1.5) +
  labs(x=expression(atop("Log Frequency", paste("B. Quarterly"))),y="Log Power Spectrum") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm"))   +
  scale_y_continuous(lim=c(-10,3))   

grid.arrange(psp1, psp2, ncol = 1)

#-------------------------------------------------------------------------------------------#
# Figure 3: Superimposed Time Series - Mood and the National Economy
#-------------------------------------------------------------------------------------------#

sp1 <- ggplot(dy, aes(x = year)) + theme_bw() +  
  geom_line(aes(y = polar, colour="Polarity", size = 2), size = 2) +
  geom_line(aes(y = ldisp, colour="Labor Disputes", size = 2), size = 2) +
  theme(legend.position=c(0.4,0.85),
        legend.text = element_text(size = 12),
        legend.key.height=unit(1.3,"line"),
        legend.key.size=unit(2,"line"),
        legend.key = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_colour_manual(name="", values=c('Labor Disputes'="#b20000", 'Polarity'="#002A48")) +
  labs(x="Year",y="Standard Scores") +
  scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)) +
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3), lim=c(-2.5,3.5)) 

sp2 <- ggplot(dy, aes(x = year)) + theme_bw() +  
  geom_line(aes(y = polar, colour="Polarity", size = 2), size = 2) +
  geom_line(aes(y = misery, colour="Misery Index", size = 2), size = 2) +
  theme(legend.position=c(0.4,0.85),
        legend.text = element_text(size = 12),
        legend.key.height=unit(1.3,"line"),
        legend.key.size=unit(2,"line"),
        legend.key = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_colour_manual(name="", values=c('Misery Index'="#FF8C00", 'Polarity'="#002A48")) +
  labs(x="Year",y="Standard Scores") +
  scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)) +
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3), lim=c(-2.5,3.5))   

grid.arrange(sp1, sp2, ncol = 1)

#-------------------------------------------------------------------------------------------#
# Figure S3. Heat Map
#-------------------------------------------------------------------------------------------#

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

d2 <- data.frame(year=dy$year,polarity=range01(dy$polar),
                 disputes=range01(dy$ldisp),
                 unemployment=range01(dy$unemp),
                 misery=range01(dy$misery),
                 gdpg=range01(dy$gdpg))
d2 <- melt(d2,id.vars="year")
colf <- colorRampPalette(c("#00007F","#7F0000"))
jetc <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

ggplot(d2, aes(x = year, y = variable, fill = value)) + theme_bw() +
  geom_tile() +
  scale_fill_gradientn(colours = jetc(100),name = "", 
                       breaks=c(0,1),
                       labels=c("Low","High")) +
  scale_y_discrete(limits=c("gdpg","misery","unemployment","disputes","polarity"),
                   labels=c("GDP Growth","Misery Index","Unemployment","Labor Disputes","Polarity"),
                   name="") +
  labs(x="Year") +
  theme(axis.text=element_text(size=10),
       axis.title=element_text(size=12), 
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.key.size = unit(1.5, "lines"),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) 

#-------------------------------------------------------------------------------------------#
# Figure S4. Alternative, General-Purpose Polarity Lexicons
#-------------------------------------------------------------------------------------------#

dq$time <- 1:nrow(dq)
dq$polsn <- smooth.spline(dq$nrcpol)$y
dq$polss <- smooth.spline(dq$swnpol)$y
dq$polso <- smooth.spline(dq$ofpol)$y

ggplot(dq, aes(x = time)) + theme_bw() +  
  geom_point(aes(y = nrcpol), size = 6, colour="#C53F3F", shape=1, stroke=.1) +
  geom_point(aes(y = ofpol), size = 6, colour="#3F8C3F", shape=1, stroke=.1) +
  geom_point(aes(y = swnpol), size = 6, colour="#033FC5", shape=1, stroke=.1) +
  geom_line(aes(y = polsn, colour="NRC", size = 2), size = 2) +
  geom_line(aes(y = polso, colour="OpinionFinder"), size = 2) +
  geom_line(aes(y = polss, colour="SentiWordNet"), size = 2) +

  theme(legend.position=c(0.2,0.82),
        legend.text = element_text(size = 12),
        legend.key.height=unit(1.1,"line"),
        legend.key.size=unit(2,"line"),
        legend.key = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        plot.margin = unit(c(.3,.3,.3,.3), "cm")) +
  scale_colour_manual(name="", values=c(NRC="#b20000",OpinionFinder="#006600",
                                        SentiWordNet="#0000b2")) +
  labs(x="Quarter",y="Emotional Polarity") +
  scale_x_continuous(breaks=c(5,85,165,245,325,405),
                     labels=c("1910-Q1","1930-Q1","1950-Q1","1970-Q1","1990-Q1","2010-Q1"))  +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3,4)) +
  geom_hline(aes(yintercept=0), linetype="dashed", size = .5, colour="#3f3f3f") 

