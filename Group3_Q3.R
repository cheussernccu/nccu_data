library(RCurl)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/train.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)

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

