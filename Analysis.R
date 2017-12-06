
#install package RCurl
library(RCurl)

realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",")
realEstate <- summary(realEstate)
attach(realEstate)

#install package olsrr
listname = ""
for(i in names(realEstate)){
  listname = c(listname,"+",i)
}
library(olsrr)
model <- lm(SalePrice ~ Neighborhood + HouseStyle + YearBuilt + Fireplaces, data = realEstate)
ols_all_subset(model)
m <- ols_all_subset(model)


