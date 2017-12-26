library(RCurl)
realEstate <-read.csv(text=getURL("https://raw.githubusercontent.com/cheussernccu/nccu_data/master/Data%20Final%20Paper.csv"), header=TRUE, sep = ",")
