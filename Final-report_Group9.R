#R Final paper_Group9


#Packages
library(RCurl)
library(olsrr)

#Data set
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Real%20Estate_Group9.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
realEstate.dataframe = data.frame(realEstate)

#Adjusting the Data (categorial to numerical)
realEstate[which(realEstate$Property.Type == "Condo"),5] = 1
realEstate[which(realEstate$Property.Type == "Single Family"),5] = 0

#The estate market summarized by every year 
Years=1960:2015
for(i in Years){
  print(summary(realEstate[which(realEstate$Year.Built == toString(i)),]))
}

#Create a new dummyvariable for research question 5
R_Population <- read.csv("Population _Group9.csv", header=TRUE, sep = ";")
for(i in (1960:2010)){
  R_Population[which(R_Population$Year == i),14] = R_Population[which(R_Population$Year == i+5),13]
}


#Overview of the data

#House data
x11(width=10,height=7)
par(mar=c(3,4,2,2))
par(mfrow=c(2,2))

#House Type
housetype=cbind(R_Population[,7],1-R_Population[,7])
barplot(t(housetype),names.arg  = c(1960:2015),main ="Rate of House Type in New Hampshire", xlab="Year",ylab="Rate",ylim = c(0,1),legend.text =  c("Condo","Single"),col = c("lightblue","lightpink"),args.legend = list(x = "topright",cex=1,bg="white"))

#Rooms
plot(R_Population[1:56,1],R_Population[1:56,10],type="l",col="black",xlab="Year",ylab="Rooms",main="Average Rooms in New Hampshire",ylim=c(0,12),lwd=2)
lines(R_Population[1:56,1],R_Population[1:56,8],col="red",lwd=2)
lines(R_Population[1:56,1],R_Population[1:56,9],col="blue",lwd=2)
legend("topright", c("Total","Bedrooms","Bathrooms"), ncol = 1, cex = 1,col=c("black","red","blue"),
       lty=c(1,1,1),lwd=c(2,2,2))

#Size
plot(R_Population[1:56,1],R_Population[1:56,12]*375.965264,type="l",col="black",xlab="Year",ylab="Size",main="Average Sizes in New Hampshire",lwd=2,ylim=c(0,13000))
lines(R_Population[1:56,1],R_Population[1:56,11],col="red",lwd=2)
legend("topright", c("Total","Indoor"), ncol = 1, cex = 1,col=c("black","red"),
       lty=c(1,1),lwd=c(2,2))

#Total houses
plot(R_Population[,1],R_Population[,13],type="l",xlab="Year",ylab="Houses",main="Total Houses in New Hampshire")

#Descriptive statistics 
overview_house=matrix(NA,ncol =7,nrow = 4 )
colnames(overview_house)=c("Rate of Condo","Bedrooms","Bathrooms","Total rooms","Indoor size(square feets)","Total size(Acres)","Total Houses")
rownames(overview_house)=c("mean","maximun","minimun","standard_deviation")
for(i in 1:7){
  overview_house[1,i]=mean(R_Population[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[2,i]=max(R_Population[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[3,i]=min(R_Population[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[4,i]=sd(R_Population[,i+6],na.rm = TRUE)
}


#Demographic data
x11(width=10,height=8)
par(mar=c(3,4,3,2))
par(mfrow=c(3,2))

#Population
plot(R_Population[,1],R_Population[,2],type="l",xlab="Year",ylab="Population",main="Total Population in New Hampshire")

#Average size of a household
plot(R_Population[11:56,1],R_Population[11:56,3],type="l",xlab="Year",ylab="Household",main="Average Households in United States")

#Fertility rate
plot(R_Population[,1],R_Population[,4],type="l",xlab="Year",ylab="Fertility Rate",main="Fertility Rate in United States")

#Birth rate
plot(R_Population[,1],R_Population[,5],type="l",xlab="Year",ylab="Birth Rate",main="Birth Rate in United States")

#Death rate
plot(R_Population[,1],R_Population[,6],type="l",xlab="Year",ylab="Death Rate",main="Death Rate in United States")


#Descriptive statistics 
overview_Pop=matrix(NA,ncol =5,nrow = 4 )
colnames(overview_Pop)=c("Population","Average Households","Fertility Rate","Birth Rate","Death Rate")
rownames(overview_Pop)=c("mean","maximun","minimun","standard_deviation")
for(i in 1:5){
  overview_Pop[1,i]=mean(R_Population[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[2,i]=max(R_Population[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[3,i]=min(R_Population[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[4,i]=sd(R_Population[,i+1],na.rm = TRUE)
}


#Regression
attach(R_Population)

#Question 1
fit.test=lm(SingleCondo~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.1=lm(SingleCondo~PopTotal+Fertility+Death) 
summary(fit.1)

#library(DAAG)
#cv.lm(data = R_Population, fit.1, m=5)

fit=lm(SingleCondo~PopTotal+Avhousehold+Fertility+Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Fertility+Birth)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Fertility+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Fertility+Birth+Death)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Fertility+Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Fertility)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Birth)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold+Death)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Fertility+Birth)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Fertility+Death)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Birth+Death)
summary(fit)
fit=lm(SingleCondo~Fertility+Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Fertility+Birth)
summary(fit)
fit=lm(SingleCondo~PopTotal+Fertility+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal+Avhousehold)
summary(fit)
fit=lm(SingleCondo~PopTotal+Fertility)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Fertility)
summary(fit)
fit=lm(SingleCondo~PopTotal+Birth)
summary(fit)
fit=lm(SingleCondo~PopTotal+Death)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Birth)
summary(fit)
fit=lm(SingleCondo~Avhousehold+Death)
summary(fit)
fit=lm(SingleCondo~Fertility+Birth)
summary(fit)
fit=lm(SingleCondo~Fertility+Death)
summary(fit)
fit=lm(SingleCondo~Birth+Death)
summary(fit)
fit=lm(SingleCondo~PopTotal)
summary(fit)
fit=lm(SingleCondo~Avhousehold)
summary(fit)
fit=lm(SingleCondo~Fertility)
summary(fit)
fit=lm(SingleCondo~Birth)
summary(fit)
fit=lm(SingleCondo~Death)
summary(fit)

#Question 2
fit.test=lm(TotalRooms~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.2a=lm(TotalRooms~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
summary(fit.2a)

fit.test=lm(Bedrooms~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.2b=lm(Bedrooms~Avhousehold+Death, data = R_Population)
summary(fit.2b)

fit.test=lm(Bathrooms~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.2c=lm(Bathrooms~PopTotal+Fertility+Death, data = R_Population)
summary(fit.2c)


#Question 3
fit.test=lm(SizeIndoor~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.3=lm(SizeIndoor~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
summary(fit.3)


#Question 4
fit.test=lm(SizeTotal~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2

fit.4=lm(SizeTotal~Fertility+Birth)
summary(fit.4)

#Question 5
fit.test=lm(HousesTotal~PopTotal+Avhousehold+Fertility+Birth+Death, data = R_Population)
m2 <- ols_best_subset(fit.test)
m2
fit.5=lm(HousesTotal~PopTotal+Fertility+Birth)
summary(fit.5)
