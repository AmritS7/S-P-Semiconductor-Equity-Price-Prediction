library("YRmisc")

#import data
library(readxl)
sp1500 <- read_excel("C:/Users/Amritpal/Desktop/BUA 633/Data/sp1500.xlsx", 
                     sheet = "allToRStudio")
View(sp1500)

dim(sp1500)
names(sp1500)

#Analyze a Sector
idf<-sp1500[sp1500$industry=="Semiconductors & Semiconductor Equipment",]
idf<-as.data.frame(idf)  

names(idf)
dim(idf)

#Histograms
par(mfrow=c(3,3))
hist(idf$price,xlab="Price",ylab="Frequency",main="Fig. 1 Hist of Price", col="darkseagreen1")
hist(idf$bvps,xlab="Book Value Per Share",ylab="Frequency",main="Fig. 2 Hist of BVPS", col="cyan3")
hist(idf$eps,xlab="Earnings Per Share",ylab="Frequency",main="Fig. 3 Hist of EPS", col="bisque3")
hist(idf$cr,xlab="Current Ratio",ylab="Frequency",main="Fig. 4 Hist of CR", col="chocolate1")
hist(idf$dta,xlab="Debt/TotAssets",ylab="Frequency",main="Fig. 5 Hist of DTA", col="darkred")

#Scatterplots
par(mfrow=c(2,2))
scatter.smooth(idf$bvps,idf$price,xlab="Book Value Per Share",ylab="Price",main="Fig. 6 BVPS vs. Price",type="n")
text(idf$bvps,idf$price,as.character(idf$tkr),cex=.8, col="cyan3")
scatter.smooth(idf$eps,idf$price,xlab="Earnings Per Share",ylab="Price",main="Fig. 7 EPS vs. Price",type="n")
text(idf$eps,idf$price,as.character(idf$tkr),cex=.8, col="bisque3") #overlay, cex- character expansion
scatter.smooth(idf$cr,idf$price,xlab="Current Ratio",ylab="Price",main="Fig. 8 CR vs. Price",type="n")
text(idf$cr,idf$price,as.character(idf$tkr),cex=.8, col="chocolate1")
scatter.smooth(idf$dta,idf$price,xlab="Debt/TotAssets",ylab="Price",main="Fig. 9 DTA vs. Price",type="n")
text(idf$dta,idf$price,as.character(idf$tkr),cex=.8, col="darkred")

#Descriptive Statistics
ds.summ(idf[,c("price","bvps","eps","cr","dta")],3)[,-c(3,4,7,8)]

#Correlation Matrix
ds.corm(idf[,c("price","bvps","eps","cr","dta")],3)


#Regression - Linear Model (Parametric)

idf1<-na.omit(idf)
names(idf1)
dim(idf1)

fit<-lm(price~bvps+eps+cr+dta,na.action=na.omit,data=idf1)
summary(fit)

#Residual and Prediction Analysis
par(mfrow=c(2,2))
hist(fit$residuals, main = "Fig. 10 Hist of LM Residuals", xlab="Residuals", ylab="Frequency", col="blueviolet")
scatter.smooth(fit$fitted.values,idf1$price, main="Fig. 11 Predicted vs Actual Price LM", xlab="Predicted Price", ylab="Actual Price", type="n")
text(fit$fitted.values,idf$price,as.character(idf$tkr),cex=.8, col="blueviolet")

#Regression - robust (linear parametric with outlier mitigation)
library("robust")
fit<-lmRob(price~bvps+eps+cr+dta,na.action=na.omit,data=idf1)
summary(fit)

par(mfrow=c(2,2))
hist(fit$residuals, main = "Fig. 12 Hist of Rob Residuals", xlab="Residuals", ylab="Frequency", col="darkgoldenrod1")
scatter.smooth(fit$fitted.values,idf1$price, main="Fig. 13 Predicted vs Actual Price Rob", xlab="Predicted Price", ylab="Actual Price", type="n")
text(fit$fitted.values,idf$price,as.character(idf$tkr),cex=.8, col="darkgoldenrod1")

#Regression - General Additive Model (nonlinear nonparametric)
library("gam")
fit<-gam(price~s(bvps,2)+s(eps,2)+s(cr,2)+s(dta,2),na.action=na.omit,data=idf1)
plot(fit)

par(mfrow=c(2,2))
plot(fit, main="Fig. 14 Gam of BVPS", col="cyan3") #repeat forall 4 can dcombine into a final image
cor(idf1$price, fit$fitted.values)^2
