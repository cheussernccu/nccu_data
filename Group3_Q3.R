library(RCurl)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)


lm(SalePrice~as.factor(YrSold),data=realEstate)
summary(realEstate)


#HousePriceIndex from the US for 2006-2010
HPI <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/HousePriceIndex.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)

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

#Indexing our dataset to January2006
IndexIowa=avgprices/avgjan06*100

my

#Plotting the values
x11()
plot(1:60,IndexIowa,type="l",col="red",ylim=c(0,120),xlim=c(1,60),ylab="Index",xlab="Year/Month")
lines(1:60,HPI_ind,type="l",col="blue")
lines(c(-10,100),c(100,100))
