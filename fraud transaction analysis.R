rm(list=ls())
#Load the libraries
library(dplyr)
library(ggplot2)
library(chron)

help("chron")

card=read.csv("fradulent transaction data/card.txt")
head(card,3)

# Working with aplication_data
#card$desiredYear = substr(as.character(card$application_date), 1,4)
help(substr)
help("as.character")
head(card$application_date,1)
head(as.character(card$application_date),1)

card$desiredYear=substr(as.character(card$application_date), 1,4)

card$desiredMonth=substr(as.character(card$application_date),6,7)

card$desiredDay=substr(as.character(card$application_date), 9,10)

card$desiredTime=substr(as.character(card$application_date), 11,26)

time1=as.POSIXct(card$desiredTime, format='%H:%M')%>%format("%H:%M:%S")
help("POSIXct")
View(time1)

# flag <- cut(chron::times(timep) , breaks = (1/24) * c(0,5,11,16,19,24))

flag=cut(chron::times(time1), breaks = (1/24)*c(0,5,11,16,19,24))
card$flag1=c("night", 'morning', 'afternoon', 'evening', 'night')[as.numeric(flag)]

#Working with $ and ,
head(card$credit_limit,1)
card$credit_lim=as.numeric( gsub('\\$|,', '', card$credit_limit))

head(card)
card=card[-c(1,3,4,8,11,14)]

# how much fraud is there in dataset
prop.table(table(card$is_fraud))

#distribution of other variables
summary(card)

#Top 5 dist_latest_transaction_address_km
# fraud_Km=as.data.frame(table(card$is_fraud,card$dist_latest_transaction_address_km))
fraud_km=as.data.frame(table(card$is_fraud, card$dist_latest_transaction_address_km))

write.csv(fraud_km, 'fraud_km.csv')

table(card$is_fraud, card$site_visits_A)

table(card$is_fraud, card$site_visits_B)

table(card$is_fraud, card$site_visits_C)

table(card$is_fraud, card$number_of_transactions)

table(card$is_fraud, card$desiredMonth)

table(card$is_fraud, card$desiredDay)

table(card$flag1)

apply(card,2,function(x) sum(is.na(x)))

card=na.omit(card)
