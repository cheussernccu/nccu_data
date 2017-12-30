library(RCurl)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)


lm(SalePrice~as.factor(YrSold),data=realEstate)
summary(realEstate)

####not working at the moment
cmsold=c()
for(s in realEstate){
  cmsold=c(cmsold+((realEstate$YrSold[s]-2006)*12+realEstate$MoSold[s]))
}
cmsold


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

my
Time=(1:55)
Time

#Plotting the values
x11()
plot(Time,IndexIowa,type="l",col="red",ylim=c(0,120),xlim=c(1,55),ylab="Index",xlab="Year/Month",xaxt="n")
axis(1,at=seq(0,60,6))
legend(1,40,legend=c("American house price index", "Iowa house prices (our dataset)"),col=c("blue","red"),lty=1)
lines(Time,HPI_ind,type="l",col="blue")
lines(c(-10,100),c(100,100),lty=2)


#Korrelation berechnen
#x-Achse andere Beschriftungen -> Januar 2006 etc
cor(Time,HPI_ind)
cor(Time,IndexIowa)
cor(HPI_ind,IndexIowa)
cor.test(HPI_ind,IndexIowa)
pairs(cbind(HPI_ind,IndexIowa,Time))
