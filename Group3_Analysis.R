#install package RCurl
library(RCurl)
options(max.print=1000000)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
realEstateSum <- summary(realEstate)
realEstate.dataframe  = data.frame(realEstate)
library(fBasics)
a = summary(realEstate[which(realEstate$YearBuilt == "2008"),])
a
realEstateSum
write.csv(a)
realEstate
basicStats(realEstate)
attach(realEstate)



Years=2006:2010
for(i in Years){
  print(summary(realEstate[which(realEstate$YrSold==toString(i)),]))
}
summary(realEstate)

#install package olsrr

library(olsrr)

model2 <- lm(SalePrice ~ MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = realEstate.dataframe)

m <- ols_step_forward(model2)
m

library(readr)
train_casparTest <- read_delim("~/nccu/train_casparTest.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
library(DAAG)
fit.a = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  extDummy +roofDummy,data=train_casparTest)
fit.b = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  ExterQual +RoofMatl,data=realEstate.dataframe)
fit.c = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  ExterQual +TotalBsmtSF        ,data=realEstate.dataframe)
fit.d = lm(SalePrice ~ 0+OverallQual + GrLivArea + BsmtFinSF1 +  extDummy +TotalBsmtSF        ,data=train_casparTest)
fit.e = lm(SalePrice ~ 0+MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	, data = realEstate.dataframe)
#0+  because b0 shouldnt be negative
summary(fit.d)
cv.lm(data=train_casparTest, fit.a, m=5)
cv.lm(data=realEstate.dataframe, fit.b, m=5)
cv.lm(data=realEstate.dataframe, fit.c, m=5)
cv.lm(data=train_casparTest, fit.d, m=5) #beste
cv.lm(data=realEstate.dataframe, model2, m=5)
lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 +  ExterQual +RoofMatl,data=realEstate.dataframe)

#












#library(leaps)
#leaps<-regsubsets(SalePrice ~ MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	,data=realEstate.dataframe,nbest=10, really.big = TRUE)
#library(MASS)
#fit <- lm(SalePrice ~ MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical  +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	ScreenPorch  +	MoSold +	YrSold +	SaleType +	SaleCondition    ,data=train_casparTest)
#step <- stepAIC(fit, direction="both")
#step # display results 









#model3 <- lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + RoofMatl, data = realEstate)
#ols_all_subset(model3)
#which(m$rsquare == max(m$rsquare))
#model <- lm(SalePrice ~ Neighborhood + HouseStyle + YearBuilt + Fireplaces, data = realEstate)

#ols_all_subset(model)
#m <- ols_all_subset(model)
#which(m$rsquare == max(m$rsquare))



MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street +	Alley +	LotShape +	LandContour +	Utilities +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea +	PoolQC +	Fence +	MiscFeature +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition
Alley Utilities PoolQC Fence MiscFeature
MSSubClass +	MSZoning + LotFrontage +	LotArea +	Street  +	LotShape  +	LandContour +	LotConfig +	LandSlope +	Neighborhood +	Condition1 +	Condition2 +	BldgType +	HouseStyle +	OverallQual +	OverallCond +	YearBuilt +	YearRemodAdd +	RoofStyle +	RoofMatl +	Exterior1st +	Exterior2nd +	MasVnrType +	MasVnrArea +	ExterQual +	ExterCond +	Foundation +	BsmtQual +	BsmtCond +	BsmtExposure +	BsmtFinType1 +	BsmtFinSF1 +	BsmtFinType2 +	BsmtFinSF2 +	BsmtUnfSF +	TotalBsmtSF +	Heating +	HeatingQC +	CentralAir +	Electrical +	stFlrSF +	ndFlrSF +	LowQualFinSF +	GrLivArea +	BsmtFullBath +	BsmtHalfBath +	FullBath +	HalfBath +	BedroomAbvGr +	KitchenAbvGr +	KitchenQual +	TotRmsAbvGrd +	Functional +	Fireplaces +	FireplaceQu	+ GarageType	+ GarageYrBlt +	GarageFinish +	GarageCars +	GarageArea +	GarageQual +	GarageCond +	PavedDrive +	WoodDeckSF +	OpenPorchSF +	EnclosedPorch +	SsnPorch +	ScreenPorch +	PoolArea  +	MiscVal +	MoSold +	YrSold +	SaleType +	SaleCondition    	

1    OverallQual        0.6257      0.6254    2105.5160    35659.4925    48622.7618    
2    GrLivArea          0.7142      0.7138    1265.3316    35267.5843    42501.3029    
3    BsmtFinSF1         0.7459      0.7454     965.0198    35097.5221    40083.0126    
4    RoofMatl           0.7835      0.7820     609.9235    34878.1170    37093.0276    
5    ExterQual          0.8136      0.8119     325.2916    34665.3632    34451.3989    
6    TotalBsmtSF        0.8238      0.8221     230.5830    34585.4869    33510.3946    
7    LotFrontage        0.8335      0.8315     250.4999    28503.8778    34226.3179    
8    Condition2         0.8487      0.8462     121.8532    28399.2752    32700.6828    
9    SaleCondition      0.8594      0.8565      31.5812    28321.1948    31590.2232    
10    LandSlope          0.8599      0.8568      29.0184    28320.6670    31557.5772    
11    YearBuilt          0.8648      0.8617     -11.0181    28280.1303    31016.8598    
12    LotArea            0.8688      0.8656     -43.4457    28246.1346    30568.5493    
13    OverallCond        0.8745      0.8714     -90.7384    28194.6430    29908.0853    
14    BedroomAbvGr       0.8786      0.8755    -124.4182    28156.3813    29423.5179    
15    FireplaceQu        0.8669      0.8599     144.6355    14307.8369    34809.8151    
16    GarageType         0.8694      0.8612     126.2626    14112.1235    34506.5737    
17    KitchenAbvGr       0.8702      0.8618     124.2399    14110.6322    34436.0698    
18    SaleType           0.8713      0.8612     120.4988    14119.6136    34508.5784    
19    Exterior2nd        0.8748      0.8615     103.7160    14130.8911    34467.7425 



attach(Data_Words_Final)
model2 <- lm(a ~ .	, data = Data_Words_Final)
ols_step_forward(model2)
