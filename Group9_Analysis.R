library(RCurl)
library(fBasics)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Group9%20Data3.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
#Population <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Population%20data.csv"), header=TRUE, sep = ",")

realEstate.dataframe = data.frame(realEstate)
Years=1960:2015
for(i in Years){
  print(summary(realEstate[which(realEstate$Year.Built == toString(i)),]))
}

#Overview of Data
data1 = read.csv("C:/Users/User/Desktop/Population-data_BackUp.csv")

plot(data1[,1],data1[,2],type="l",xlab="Year",ylab="Population",main="Population")
plot(data1[,1],data1[,3],type="l",xlab="Year",ylab="Household",main="Average Households of Each Year")
plot(data1[,1],data1[,4],type="l",xlab="Year",ylab="Fertility Rate",main="Fertility Rate")
plot(data1[,1],data1[,5],type="l",xlab="Year",ylab="Birth Rate",main="Birth Rate")
plot(data1[,1],data1[,6],type="l",xlab="Year",ylab="Death Rate",main="Death Rate")

overview_Pop=matrix(NA,ncol =5,nrow = 4 )
colnames(overview_Pop)=c("Population","Average Households","Fertility Rate","Birth Rate","Death Rate")
rownames(overview_Pop)=c("mean","maximun","minimun","standard_deviation")
for(i in 1:5){
  overview_Pop[1,i]=mean(data1[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[2,i]=max(data1[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[3,i]=min(data1[,i+1],na.rm = TRUE)
}
for(i in 1:5){
  overview_Pop[4,i]=sd(data1[,i+1],na.rm = TRUE)
}


housetype=cbind(data1[,7],1-data1[,7])
x11(width=8,height=5)
barplot(t(housetype),names.arg  = c(1960:2015),main ="Rate of House Type", xlab="Year",ylab="Rate",ylim = c(0,1),legend.text =  c("Condo","Single"),col = c("lightblue","lightpink"),args.legend = list(x = "topright",cex=1,bg="white"))

x11(width=8,height=5)
plot(data1[1:56,1],data1[1:56,10],type="l",col="black",xlab="Year",ylab="Rooms",main="Average Rooms of Each Year",ylim=c(0,10),lwd=2)
lines(data1[1:56,1],data1[1:56,8],col="red",lwd=2)
lines(data1[1:56,1],data1[1:56,9],col="blue",lwd=2)
legend("topright", c("Total","Bedrooms","Bathrooms"), ncol = 1, cex = 1,col=c("black","red","blue"),
       lty=c(1,1,1),lwd=c(2,2,2))

x11(width=8,height=5)
plot(data1[1:56,1],data1[1:56,12]*375.965264,type="l",col="black",xlab="Year",ylab="Size",main="Average Rooms of Each Year",lwd=2)
lines(data1[1:56,1],data1[1:56,11],col="red",lwd=2)
legend("topright", c("Total","Indoor"), ncol = 1, cex = 1,col=c("black","red"),
       lty=c(1,1),lwd=c(2,2))

plot(data1[,1],data1[,13],type="l",xlab="Year",ylab="Houses",main="Total Houses")

overview_house=matrix(NA,ncol =7,nrow = 4 )
colnames(overview_house)=c("Rate of Condo","Bedrooms","Bathrooms","Total rooms","Indoor size(square feets)","Total size(Acres)","Total Houses")
rownames(overview_house)=c("mean","maximun","minimun","standard_deviation")
for(i in 1:7){
  overview_house[1,i]=mean(data1[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[2,i]=max(data1[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[3,i]=min(data1[,i+6],na.rm = TRUE)
}
for(i in 1:7){
  overview_house[4,i]=sd(data1[,i+6],na.rm = TRUE)
}


