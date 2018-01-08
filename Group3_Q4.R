library(RCurl)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
train_withDummy <- read_delim("~/nccu_data/train_dummy_2.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
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
