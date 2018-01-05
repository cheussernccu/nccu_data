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
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
realEstate.dataframe  = data.frame(realEstate)

#Dataset with additional Dummy Variables for other models and less variables
train_withDummy <- read_delim("~/nccu/train_dummy_2.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
#Dataset with less columns for the faster execution of some algorithms
train_lessColumns <- read_delim("~/nccu/train_clean.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
#train_withDummy <- read_delim("~/nccu_data/train_dummy.csv", 
#                              ";", escape_double = FALSE, trim_ws = TRUE)
#train_lessColumns <- read_delim("~/nccu_data/train_clean.csv", 
#                                ";", escape_double = FALSE, trim_ws = TRUE)

#BasicSummary Data about the all realEstate sold in one year
Years=2006:2010
for(i in Years){
  print(summary(realEstate[which(realEstate$YrSold==toString(i)),]))
}
#Summary Data about the whole Dataset
summary(realEstate)



#bestSubset variable selection - probably takes several hours to run
#includes all variables that shown a less or euqal 5% significant in a lm with all variables
fit.bestSub = lm(SalePrice ~ 0+OverallQual +	OverallCond  +	roofDummy	+ MasVnrArea  +	extDummy +	BsmtExposure +	BsmtFinSF1 +	stFlrSF +	ndFlrSF +	KitchenQual+	LotArea +	Condition2 +	Neighborhood        ,data=train_withDummy)
model2 <- ols_best_subset(fit.bestSub) #can take several hours to run
model2

#result bets subset: OverallQual stFlrSF ndFlrSF KitchenQual Neighborhood

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

 



#Simple LM with graph
train_qual_price <- read_delim("~/nccu/train_qual_price.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE)
fit.gg = lm(log(SalePrice) ~ 0+OverallQual, data = train_qual_price)
summary(fit.gg)
cv.lm(data=realEstate.dataframe, fit.gg, m=5)#MS NA
#fit gg: R^2: 0.962 MS: 5.07

fit.gg2 = lm(log(SalePrice) ~ OverallQual, data = train_qual_price)
summary(fit.gg2)
cv.lm(data=realEstate.dataframe, fit.gg2, m=5)#MS NA
#fit gg2: R^2: 0.668 MS: 0.06

plot(train_qual_price)
abline(fit.gg)







#test of a backward selection
library(MASS)
data_clean<-na.omit(realEstate.dataframe)
data_clean
model2_clean <- lm(SalePrice ~ MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = data_clean)
model2_clean
step <- stepAIC(model2, direction="backward") 
#
#***
#ndFlrSF
#BsmtFinSF1
#RoofMatl
#OverallQual           
#OverallCond  
#fit.g = lm(SalePrice ~ 0+OverallQual + OverallCond + RoofMatl +  ndFlrSF +BsmtFinSF1        ,data=realEstate.dataframe)
#fit.h = lm(SalePrice ~ 0+OverallQual + OverallCond + roofDummy +  ndFlrSF +BsmtFinSF1        ,data=train_casparTest)
#**
#stFlrSF
#LotArea
#*
#  BsmtExposure
#ExterQual
#MasVnrArea









#Test of another best subset selection
#library(leaps)
#leaps<-regsubsets(SalePrice ~ MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	,data=realEstate.dataframe,nbest=10, really.big = TRUE)
#library(MASS)
#fit <- lm(SalePrice ~ MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical  +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	ScreenPorch  +	MoSold +	YrSold +	SaleType +	SaleCondition    ,data=train_casparTest)
#step <- stepAIC(fit, direction="both")
#step # display results 












