#packages
library(RCurl)
library(olsrr)
library(readr)
library(DAAG)
library(quantreg)
library(MASS)
#settings
options(max.print=1000000) #Extend amount of printed lines
#data
realEstate <-read.csv("train.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
realEstate.dataframe  = data.frame(realEstate)
#Dataset with additional Dummy Variables for other models and less variables

train_withDummy <- read.csv("train_dummy.csv", header=TRUE, sep = ";")

#Dataset with less columns for the faster execution of some algorithms
train_lessColumns <- read.csv("train_clean.csv", header=TRUE, sep = ";")


train_qual_price <- read.csv("train_qual_price.csv", header=TRUE, sep = ";")

train_dummy_3 <- read.csv("train_dummy_3.csv", header=TRUE, sep = ",")

#BasicSummary Data about the all realEstate sold in one year
Years=2006:2010
for(i in Years){
  print(summary(realEstate[which(realEstate$YrSold==toString(i)),]))
}
#Summary Data about the whole Dataset
summary(realEstate)

#Q1

#bestSubset variable selection - probably takes several hours to run
#includes all variables that shown a less or euqal 5% significant in a lm with all variables
fit.bestSub = lm(SalePrice ~ 0+OverallQual +	OverallCond  +	roofDummy	+ MasVnrArea  +	extDummy +	BsmtExposure +	BsmtFinSF1 +	stFlrSF +	ndFlrSF +	KitchenQual+	LotArea +	Condition2 +	Neighborhood        ,data=train_withDummy)
bestSubsets <- ols_best_subset(fit.bestSub) #can take several hours to run
bestSubsets

#result bets subset: OverallQual stFlrSF ndFlrSF KitchenQual Neighborhood

#Q2

fit.best = lm(log(SalePrice) ~ 0+OverallQual + stFlrSF + ndFlrSF +  KitchenQual +Neighborhood,data=train_withDummy)
summary(fit.best)
cv.lm(data=train_withDummy, fit.best, m=5)
#fit fit.best: R^2: 1 MS: 0.03

fit.best2 = lm(log(SalePrice) ~ 0+OverallQual + stFlrSF + ndFlrSF +  KitchenQual +neigDummy,data=train_withDummy)
summary(fit.best2)
cv.lm(data=train_withDummy, fit.best2, m=5)
#fit fit.best2: R^2: 1 MS: 0.04

fit.best3 = lm(log(SalePrice) ~ 0+OverallQual + stFlrSF + ndFlrSF +  kitchDummy +neigDummy,data=train_withDummy)
summary(fit.best3)
cv.lm(data=train_withDummy, fit.best3, m=5)
#fit fit.best3: R^2: 0.979 MS: 3.06


#stepwise Forward variable Selection 
model <- lm(SalePrice ~ MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = realEstate.dataframe)
m <- ols_step_forward(model)

#test of different models based on the result of stepwise forward selection
fit.a = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  extDummy +roofDummy,data=train_withDummy)
summary(fit.a)
cv.lm(data=train_withDummy, fit.a, m=5) 
#fit a: R^2: 0.95 MS: 2.26e9

fit.b = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  ExterQual +RoofMatl,data=realEstate.dataframe)
summary(fit.b)
cv.lm(data=realEstate.dataframe, fit.b, m=5)
#fit b: R^2: 0.97 MS: NA

fit.c = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  ExterQual +TotalBsmtSF        ,data=realEstate.dataframe)
summary(fit.c)
cv.lm(data=realEstate.dataframe, fit.c, m=5)
#fit c: R^2: 0.964 MS: 2.41e9

fit.d = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF        ,data=train_casparTest)
summary(fit.d)
cv.lm(data=train_withDummy, fit.d, m=5)
#fit d: R^2: 0.951 MS: 2.16e9

fit.e = lm(SalePrice ~ 0+MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = realEstate.dataframe)
summary(fit.e)
cv.lm(data=realEstate.dataframe, fit.e, m=5)
#fit e: R^2: 0.989 MS: NA

fit.f = lm(SalePrice ~ 0+MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = realEstate.dataframe)
summary(fit.f)
cv.lm(data=realEstate.dataframe, fit.f, m=5)
#fit f: R^2: 0.989 MS: NA

fit.g = lm(SalePrice ~ 0+OverallQual + OverallCond + RoofMatl +  ndFlrSF +BsmtFinSF1        ,data=realEstate.dataframe)
summary(fit.g)
cv.lm(data=realEstate.dataframe, fit.g, m=5)
#fit g: R^2: 0.954 MS: NA

fit.h = lm(SalePrice ~ 0+OverallQual + OverallCond + roofDummy +  ndFlrSF +BsmtFinSF1        ,data=train_withDummy)
summary(fit.h)
cv.lm(data=train_withDummy, fit.h, m=5)
#fit h: R^2: 0.946 MS: 2.48e9

fit.i = lm(log(SalePrice) ~ 0+OverallQual+ I(OverallQual^2)  + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF        ,data=train_withDummy)
summary(fit.i)
cv.lm(data=train_withDummy, fit.i, m=5)
#fit i: R^2: 0.997 MS: 0.39

fit.i2 = lm(log(SalePrice) ~ OverallQual+ I(OverallQual^2)  + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF        ,data=train_withDummy)
summary(fit.i2)
cv.lm(data=train_withDummy, fit.i2, m=5)
#fit i2: R^2: 0.781 MS: 0.04

fit.i3 = lm(log(SalePrice) ~ OverallQual+ I(OverallQual^2)  + GrLivArea + BsmtFinSF1 +  ExterQual +TotalBsmtSF        ,data=train_withDummy)
summary(fit.i3)
cv.lm(data=train_withDummy, fit.i3, m=5)
#fit i2: R^2: 0.789 MS: 0.04


fit.j = lm(SalePrice ~ 0+OverallQual+ I(OverallQual^2)  + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF        ,data=train_withDummy)
summary(fit.j)
cv.lm(data=train_withDummy, fit.j, m=5)
#fit j: R^2: 0.964 MS: 1.77e9

fit.k = lm(SalePrice ~ 0+OverallQual+ I(OverallQual^2)  + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF +   I(TotalBsmtSF^2)      ,data=train_withDummy)
summary(fit.k)
cv.lm(data=train_withDummy, fit.k, m=5)
#fit k: R^2: 0.969 MS: 1.76e9

fit.l = lm(log(SalePrice) ~ 0+OverallQual + I(OverallQual^2) + GrLivArea + I(GrLivArea^2) + BsmtFinSF1 +I(BsmtFinSF1^2) +  extDummy +I(extDummy^2) +TotalBsmtSF +I(TotalBsmtSF^2)        ,data=train_withDummy)
summary(fit.l)
cv.lm(data=train_withDummy, fit.l, m=5)
#fit l: R^2: 0.997 MS: 0.46

#Simple LM with graph - most important attribute OverallQual

fit.gg = lm(SalePrice ~ OverallQual, data = train_qual_price)
summary(fit.gg)
cv.lm(data=realEstate.dataframe, fit.gg, m=5)
#fit gg: R^2: 0.626 MS: 2.36e9

fit.gg2 = lm(SalePrice ~ 0 + OverallQual, data = train_qual_price)
summary(fit.gg2)
cv.lm(data=realEstate.dataframe, fit.gg2, m=5)
#fit gg2: R^2: 0.928 MS: 2.65e9

plot(train_qual_price)
abline(fit.gg)  
abline(fit.gg2)

#Q3

#regression by year
ylm=lm(SalePrice~as.factor(YrSold),data=realEstate)
summary(ylm)

#regression by cummulative month
cmsold=c()
for(s in 1:1460){
  sms=((realEstate$YrSold[s]-2006)*12+realEstate$MoSold[s])
  cmsold=c(cmsold,sms)
}

mlm=lm(realEstate$SalePrice~cmsold)
summary(mlm)


#HousePriceIndex from the US for 2006-2010
HPI <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/HousePriceIndex.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
HPI = head(HPI,-5)

#Changing the values so that January 2006 equals 100 (Indexing to base January 2006)
HPI_ind=HPI$USA.House.Price.Index/HPI[1,4]*100

#Procedurs for indexing our values later
arrayjan06=realEstate$SalePrice[which(realEstate$MoSold==1&realEstate$YrSold==2006)]
avgjan06=mean(arrayjan06)

#for loop which gives us the average prices in our dataset for every month
avgprices=c()
my=c()

for (year in (2006:2010)){
  for (month in(1:12)){
    my=c(my,paste(year,"/",month))
    pricesmonth=realEstate$SalePrice[which(realEstate$MoSold==month&realEstate$YrSold==year)]
    meanprice=mean(pricesmonth)
    avgprices=c(avgprices,meanprice)
  }
}

avgprices=head(avgprices,-5)

#Indexing our dataset to January2006
IndexIowa=avgprices/avgjan06*100

#Smoothing values by taking 3 month average
IndexIowaS=mean(c(IndexIowa[1],IndexIowa[2]))
for (w in 2:(length(IndexIowa)-1)){
  tmp=mean(c(IndexIowa[w-1],IndexIowa[w],IndexIowa[w+1]))
  IndexIowaS=c(IndexIowaS,tmp)
}
IndexIowaS=c(IndexIowaS,mean(c(IndexIowa[(length(IndexIowa)-1)],IndexIowa[(length(IndexIowa))])))

my
Time=(1:55)
cumo=cbind(my,Time)

#Plotting the values
x11()
plot(Time,IndexIowa,type="l",col="red",ylim=c(0,120),xlim=c(1,55),ylab="Index",xlab="Cumulated month (Starting from January 2006)",xaxt="n")
axis(1,at=seq(0,60,6))
legend(1,40,legend=c("American house price index", "Iowa house prices (our dataset)","Iowa house prices (smoothed)"),col=c("blue","red","darkseagreen"),lty=1)
lines(Time,HPI_ind,type="l",col="blue")
lines(Time,IndexIowaS,type="l",col="darkseagreen")
lines(c(-10,100),c(100,100),lty=2)


#Correlation
#x-Axis is cummulative month (January 2006 -> 1, see "cumo")
cor.test(Time,HPI_ind)
cor.test(Time,IndexIowa)
cor.test(Time,IndexIowaS)
cor.test(HPI_ind,IndexIowa)
cor.test(HPI_ind,IndexIowaS)
pairs(cbind(HPI_ind,IndexIowa,IndexIowaS,Time))


#Iowa Prices also dropped due to the Real Estate Crisis. Our dataset is not big enough for a
#representative analysis. For example the average price per month is not exactly accurate. There
#are many factors which distort our results. Firstly, there could be exceptions from the usual 
#price in the Iowa market which have quite a big impact on the average price in a given month. Also
#it is possible that in a month only expensive (cheap) houses were sold which distort the average
#in a month. So our plotted graph is not really an index.
#huge variance

#Q4

attach(train_dummy_3)
plot(IfRemod,SalePrice)


fit.remod = lm(log(SalePrice) ~ 0 + IfRemod,data=train_dummy_3)
summary(fit.remod)
cv.lm(data=train_dummy_3, fit.remod, m=5)
#fit fit.remod: R^2:0.474 MS:79.3

fit.ovaq = lm(log(SalePrice) ~ 0 + OverallQual,data=train_dummy_3)
summary(fit.ovaq)
cv.lm(data=train_dummy_3, fit.ovaq, m=5)
#fit fit.ovaq: R^2:0.962 MS:5.69

fit.ovaqremod = lm(log(SalePrice) ~ 0 + OverallQual + IfRemod,data=train_dummy_3)
summary(fit.ovaqremod)
cv.lm(data=train_dummy_3, fit.ovaqremod, m=5)
#fit fit.ovaqremod: R^2:0.965 MS:5.26

fit.best = lm(log(SalePrice) ~ 0+OverallQual + stFlrSF + ndFlrSF +  KitchenQual +Neighborhood,data=train_withDummy)
summary(fit.best)
cv.lm(data=train_withDummy, fit.best, m=5)
#fit fit.best: R^2: 1 MS: 0.03

fit.bestremod = lm(log(SalePrice) ~ 0+OverallQual + stFlrSF + ndFlrSF +  KitchenQual +Neighborhood + IfRemod,data=train_dummy_3)
summary(fit.bestremod)
cv.lm(data=train_dummy_3, fit.bestremod, m=5)
#fit fit.best: R^2: 1 MS: 0.03

