library(RCurl)
library(fBasics)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Group9%20Data3.csv"), header=TRUE, sep = ",", stringsAsFactors = FALSE)
#Population <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Population%20data.csv"), header=TRUE, sep = ",")

realEstate.dataframe = data.frame(realEstate)
Years=1960:2015
for(i in Years){
  print(summary(realEstate[which(realEstate$Year.Built == toString(i)),]))
}




