library(RCurl)
library(fBasics)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Group9%20Data3.csv"), header=TRUE, sep = ";", stringsAsFactors = FALSE)
#Population <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Population%20data.csv"), header=TRUE, sep = ",")

realEstate.dataframe = data.frame(realEstate)
Years=1960:1980
for(i in Years){
  asd  = summary(Group9_Data3[which(Group9_Data3$`Year Built` == toString(i)),])
}
asd = mean(Group9_Data3$`Price - Closed`)
asd


