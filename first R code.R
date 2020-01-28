#to clear the envirnment
rm(list = ls())

# indicates comment line
# to check current working directory

getwd()
# datasets
dataset=read.csv("C:/Users/Santosh/Downloads/Compressed/Machine Learning A-Z New/Part 2_Regression/Section 5 - Multiple Linear Regression/50_Startups.csv")

#to change/set working directory
#setwd("C:/Users/Santosh/Desktop/R")
setwd("S:/Evacity Course")
library(dplyr)
library(ggplot2)
library(dummy)
library(caret)
library(mlbench)
library(AppliedPredictiveModeling)
library(MLmetrics)
library(caTools)
library(corrgram)
library(corrplot)
#install.packages("caret")
#install.packages("mlbench")
#install.packages("AppliedPredictiveModeling")
#install.packages("corrgram")
install.packages("corrplot")

# list the contents of the library
library(help = "mlbench")

# list the contents of the library
library(help = "AppliedPredictiveModeling")

#startup_data
startup_data=read.csv("C:/Users/Santosh/Downloads/Compressed/Machine Learning A-Z New\\Part 2_Regression/Section 5 - Multiple Linear Regression/50_Startups.csv")

#to get the set of inbuild datasets
data()

#to know about a inbuild data information
help(iris)

# to viwe an existing data
View(iris)

#To get column names of data
names(iris)

# To get number of rows
nrow(iris)

# to get number of columns
ncol(iris)

#To get dimension of dataset
dim(iris)

# ROWwise operation

# extract 3rd row from iris
iris[3,]

iris[3,2] # 3rd row and second column

#Extract 19th to 26th row
iris[19:26,]

# extract 3rd 19th 97th rows
iris[c(3,19,97),]


# Colums Operation
# default is column consideration

data1=iris[,c(3,5)]

View(data1)


data2=iris[c(3,5)]

# head and tail function in R

head(iris)

head(iris,10)

tail(iris)

tail(iris,10)

#add columns or manipulation of columns using $

iris$Sepal.Length

iris$suma=0
View(iris)

iris$suma1=iris$Sepal.Length/iris$Sepal.Width
View(iris)

iris$flag=ifelse(iris$Sepal.Length>5,1,0)

View(iris)


# Data Types in R

# 1 Dimensional vector
a=c(5,10,20)

b=c(25,20,45)


rbind(a,b)
a1=rbind(a,b)
View(a1)

cbind(a,b)

#create Metric
# default: fills columnwise
M=matrix(1:12,3,4)

View(M)

#fill metric row wise
N=matrix(1:12,4,4)

View(N)

M1=matrix(1:12,3,4,byrow=TRUE)

View(M1)

mylist=list(12,"rahul",34)
mylist
View(mylist)

#creating dataframe

mytable=data.frame(cid=1:4, name=c("rahul","sachin","virat","dhoni"), age=c(47,45,31,38), Location=c("ang", "mumbai", "delhi","rachi"))

View(mytable)

library("dplyr")


View(iris)

#Select 2 columns Sepal.Lenth,Species

data1=select(iris, Sepal.Length, Species)
View(data1)

# From iris remove Sepal.Length and Species
data2=select(iris, -Sepal.Length, -Species)
View(data2)
names(iris)
names(data2)
d2=rename(iris, Santosh=Sepal.Length)
View(d2)
# extract all rows where Sepal.Length>7
data3=filter(iris, Sepal.Length>7)
View(data3)


data4=filter(iris, Sepal.Length>7.5 & Species=='virginica')
View(data4)

data5=iris%>%filter(Sepal.Length>7.5)
View(data5)

#Using mean and median

M1=mean(iris$Sepal.Length)
M1
M2=median(iris$Sepal.Length)
M2

#Summarise:Reduce multiple values down to a single value
data6=iris%>%summarise((M1=mean(iris$Sepal.Length)), (M2=median(iris$Sepal.Length)))
View(data6)
data6


# find sum of Sepal.Width and median of Petal.Length
s1=sum(iris$Sepal.Width)
M2=median(iris$Petal.Length)

data7=iris%>%summarise((s1=sum(iris$Sepal.Width)),(M2=median(iris$Petal.Length)) )
data7

data8=iris%>%group_by(Species)%>%summarise((M1=mean(iris$Sepal.Length)))
data8



# find Queries
mydata=rename(iris, Suma=Sepal.Length)
View(mydata)

data9=sample_n(iris, 5)
data9

data10=sample_frac(iris, 0.1)
data10

#==========CLASS_17/11/2019====================================

# 1.Load the attached data as my_data.
# 2.Remove duplicate rows based on Index column in my_data and save data as data1.

data1=distinct(my_data1, Index, .keep_all=TRUE)
View(data1)

# 3.Drop variables name starts with Y in my_data and save as data2.

data2=select(my_data1, -starts_with("Y"))

# 4.select variables in my_data ,contain I in their name and save as data3.

data3=select(my_data1,contains("I"))
View(data3)
# 5.Filter rows in my_data where Index=A or C and save as data4.
data4=filter(my_data1, Index %in% c("A","C"))

# 6.Filter rows in my_data where Index !=A or C and save as data5.

data6=filter(my_data1, !Index %in% c("A","C"))
View(data6)
# 7.Get the mean and sum of Y2015 column.

data7=my_data1%>%summarise(m1=sum(Y2015),(m2=mean(Y2015)))
View(data7)
# 8.Get the mean and median of Y2011 column based on Sate column.
data8=my_data1%>%group_by(State)%>%summarise(m=mean(Y2015), N2=median(Y2015))
View(data8)

#derive a column in my_data 
n1=mutate(my_data1, new_col=Y2011/Y2012)
View(n1)

getwd()
setwd("S:/Evacity Course")
#see all datasets in R
# help()
#View
#nrow
#ncol
#dim
#head()
#tail
# Row operations
# column operations
# $ specific to column
# Data Types in R
# create metric
# create dataframe
# create list
#library('dplyr')
#select
#filter
# pipe operator %>% 
# mean, median, sum
# summarise
#select 5 rows randomly from dataset
# select 10% rows
# rename a column
# 1.Load the attached data as my_data.
# 2.Remove duplicate rows based on Index column in my_data and save data as data1.
# 3.Drop variables name starts with Y in my_data and save as data2.
# 4.select variables in my_data ,contain I in their name and save as data3.
# 5.Filter rows in my_data where Index=A or C and save as data4.
# 6.Filter rows in my_data where Index !=A or C and save as data5.
# 7.Get the mean and sum of Y2015 column.
# 8.Get the mean and median of Y2011 column based on Sate column.
# 9. derive a column in my_data , s.t newcol=Y2011/Y2012
# 10 using subset same as filter


#=====================
#  23/11/2019

ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram(fill='blue')


View(mpg)
dim(mpg)
help(mpg)
names(mpg)

# barplot of "class" variable

# barplot along x-axis
ggplot(data=mpg, aes(x=class)) + geom_bar()

#barplot along y-axis(flip(x))
ggplot(data=mpg, aes(x=class)) + geom_bar(fill='blue')+coord_flip()

#plot Scatterplot

ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Width, color=Species)) + geom_point()

ggplot(data=iris)+aes(x=Sepal.Length, y=Petal.Width, color=Species) +geom_point()


#Plot boxplot for Sepal.Length against Species in iris dataset

ggplot(data=iris, aes(x=Species, y=Sepal.Length))+geom_boxplot()

# filling colors to barplot

ggplot(data=iris, aes(x=Species, y=Sepal.Length))+geom_boxplot(fill=c("yellow", "red", "green"))

p=ggplot(data=iris, aes(x=Species, y=Sepal.Length))+geom_boxplot(fill=c("yellow", "red", "green"))

# adding plotnames
p+labs(title='Relation between Sepal and Petal', x='myaxis1', y='myaxis2')

# install.packages('readxl')
# install.packages('rjson')

#=============================================
# 24/11/2019

a=c(1,2,3)
b=c(4,5,6)

rbind(a,b)
cbind(a,b)


newdata=cbind(iris,iris)
View(newdata)


newdata1=rbind(iris,iris)
View(newdata1)


my_matrix=matrix(c(5,6,7,9,20,21,25,26,10,45,46,47), 4,3)
my_matrix

my_matrix1=matrix(c(5,6,7,9,20,21,25,26,10,45,46,47), 4,3, byrow = TRUE)
my_matrix1


a1=list()


df1=data.frame(CID=1:4, name=c('R','M','T', 'S'), age=c(25,35,65,42))
df1
View(df1)

df1[2,3]


df2=data.frame(CID=c(2,4), Location=c('Bangalore', 'Chennai'))
df2


# JOINT OPERATIONS
#left join

data1=left_join(df1,df2, by='CID')
View(data1)

#right_join 
data2=right_join(df1,df2, by='CID')
View(data2)

#apply function, margin=1=> row
              # margin=2=> column

apply(my_matrix, 1, sum) # row wise sum
my_matrix


apply(my_matrix, 2, sum) # column wise

apply(my_matrix, 1, mean) # row wise sum
my_matrix

apply(my_matrix, 2, mean) # column wise

apply(my_matrix, 1, median) # row wise sum
my_matrix

apply(my_matrix, 2, median) # column wise

my_data2=read.csv("Computer_Data.csv")
View(my_data2)
dim(my_data2)


apply(my_data2[2:4], 2, sum)

d_data=apply(my_data2[2:4],2, function(x) x/(sum(x)))
View(d_data)

d_data1=apply(my_data2[2:4],2, function(x) x/2)
View(d_data1)

d_data2=apply(my_data2[2:4], 2, function(x) ifelse(x>200, 1, 0))
View(d_data2)

#ggplot
#histogram
# barplot & flip
#flip
# scatter plot
#box plot
#======================DATA TYPES================================
# character, Numeric, String 
#create a vector
#create List
#=========MATRIX=====================================
# Joint operations
#left join
#==============apply function==============


# One Hot Encoding

#install.packages("dummy")
#library(dummy)

str(iris)

dummy_data=dummy(iris)
View(dummy_data)

new_data=cbind(iris, dummy_data)
View(new_data)


#============== STATISTICS====================
#EDA 
#1)Variable Identification : depenent and independent
#==> Classification(quality and categorical variable)
#==> Regression(quantity and continous variable)
#2) Variable Category Identification : continuous and categorical(ordinal & Nominal)
# One Hot Encoding for Nominal variables
# BINNING: create different groups
#FEATURE TRANSFORMATION
# FEATURE SELECTION


#install.packages("dummy")
#library(dummy)


#structure of data: gives number of observations and variables 
#with variable names and details
str(iris)

#Factor w/ 3 levels "setosa","versicolor",....
# w/3:  with 3 levels

#creating dummy variables One Hot Encoding

dummy_data=dummy(iris)
new_data=cbind(iris, dummy_data)

new_data1=cbind(dummy_data, iris)

# BINNING: create different groups
#FEATURE TRANSFORMATION
# FEATURE SELECTION

#Variable Identification : depenent and independent===
#==> Classification(quality and categorical variable)
#==> Regression(quantity and continous variable)


#2)== Univariate Analysis==

comp_data=read.csv("Computer_Data.csv")

#create two different datasets, each one for continous and categorical variables
names(comp_data)

cont_data=select(comp_data, price, speed, hd, ram, screen, ads, trend )

cat_data=select(comp_data, cd, multi, premium)


# for continuous variable : check min, max, mean&median, 1st quartile(25%), 3rd quartile(75%)
summary(cont_data)
summary(cont_data[1:2])
summary(cont_data[c(1,3,5,2)])

# for categorical variable: check Frequency/count, Ratio, Percentage
#frequency/count
table(cat_data$cd)
# apply for all columns
apply(cat_data, 2, function(x) table(x))


#ratio
prop.table(table(cat_data))
#apply for all columns
apply(cat_data, 2, function(x) prop.table(table(x)))

#percentage
prop.table(table(cat_data$cd))*100
#apply for all the columns
apply(cat_data, 2, function(x) prop.table(table(x)))

#===== MISSING VALUES======

# To check missing values in R
is.na(cont_data$price)

#To check presence of missing values in R
any(is.na(cont_data$price))

#to check presence of missing values in all columns
apply(cont_data, 2, function(x) any(is.na(x)))

#to get number of missing values in a column
sum(is.na(cont_data$price))

#to get number of missing values of all columns
apply(cont_data,2,function(x) sum(is.na(x)))

#==Ratio of missing values==
sum(is.na(cont_data$price))/length(cont_data$price)

#ratio of missing values of all columns
apply(cont_data, 2, function(x) sum(is.na(x))/length(x))


#==============================================
#7/12/2019
#===========================

#titanic data===============

#Read Titanic data replacing empty fields as NA
titanic=read.csv("titanic.csv")
titanic=read.csv("titanic.csv", na.strings = '')
names(titanic)
#Remove unwanted columns
titanic=titanic[-c(1,4,9)]
names(titanic)
#recognise dependent variable=Survived

#Extract all continuous variables
cont_data_t=titanic[4:7]

#Extract all categorical data
cat_data_t=titanic[c(1:3, 8:9)]

#structure of data
str(cont_data_t) #not required
str(cat_data_t)

#declare survived as category
cat_data_t$Survived=as.factor(cat_data_t$Survived)
str(cat_data_t)

#===UNIVARIATE ANALYSIS====
#univariate analysis for continuous data
summary(cont_data_t)

#get the % of missing values of cont_data
sum(is.na(cont_data_t$Age))/length(cont_data_t$Age)

apply(cont_data_t,2,function(x) sum(is.na(x))/length(x)*100)

#check mean and median of missing column of cont_data
mean(titanic$Age, na.rm=TRUE)
median(titanic$Age,na.rm=TRUE)

#Replace na's in column with mean or median
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age, na.rm=TRUE)
apply(titanic,2,function(x) sum(is.na(x))/length(x)*100)

#univariate analysis for categorical data
table(cat_data_t$Sex)
prop.table(table(cat_data_t$Sex))*100

apply(cat_data_t,2,function(x) prop.table(table(x))*100)

#get the % of missing values of cat_data
apply(cat_data_t,2,function(x) sum(is.na(x))/length(x)*100)

#remove column with 50%data missing
titanic=titanic[-8]

#mode: which value occuring frequently replace with it.
titanic$Embarked[is.na(titanic$Embarked)]='S'

#===================================================
#====CORRELATION AND MULTICOLLINEARITY=============
#read computer data
comp_data=read.csv("Computer_Data.csv")
#correlation between price and speed
cor(comp_data$price, comp_data$speed)
cor(comp_data$speed, comp_data$price)
#plot variables speed and speed
plot(comp_data$speed, comp_data$price)
#find correlation between many variables: MULTI-COLINEARITY:[2,3,4,5,10,11]
cor(comp_data[c(2,3,4,5,10,11)])

#===FEATURE TRANSFRMATION========
#help(cut)
help(cut)
# cut the price column and divide as low, medium and high price
comp_data$price_category=cut(comp_data$price, breaks = c(0,1000,3000,Inf),
                             labels=c('Low Price', "Medium Price", "High Price"))



cut(comp_data$price, breaks = 1000)

comp_data$price_category=cut(comp_data$price, breaks = c(0,1000,3000,Inf),
                             labels=c('Low Price', "Medium Price", "High Price"))
#========SUPERVISED LEARNING==========
#========LINEAR REGRESSION===========
#Load the computer data
comp_data=read.csv("Computer_Data.csv")

#split the data
sample=sample.split(comp_data$price, SplitRatio = 0.70)
View(sample)
prop.table(table(sample))*100

#declare train and test data
train_data=subset(comp_data, sample==TRUE)
test_data=subset(comp_data, sample==FALSE)

#===28/12/2019====
#=== SIMPLE LINEAR REGRESSION EXAMPLE===
#Business problem: Develop price prediction model using speed

#Load the Computer Data
rm(list=ls())
comp_data=read.csv("Computer_Data.csv")

#Extract Speed and Price column in a dataset(mydata)
head(comp_data,2)
mydata=comp_data[2:3]
head(mydata)

library(caTools)
#divide the data into train and test set
sample=sample.split(mydata$price, SplitRatio = 0.70)

train_data=subset(mydata, sample==TRUE)
test_data=subset(mydata, sample==FALSE)

# Model Development
model=lm(price~speed, data=train_data)

summary(model)
#Adjusted R-squared:  0.09441

#model prediction
test_data$pred_price=predict(model, test_data)

library(MLmetrics)
#Validation/Evaluation, checking the accuracy using MAPE
MAPE(test_data$price, test_data$pred_price)

#MAPE=0.1995092, ~20% error== + or - 20 error of prediction



#Multiple Linear Regression

comp_data1=read.csv('Computer_Data.csv')

head(comp_data1,3)
comp_data=comp_data1[c(2:6, 10:11)]
head(comp_data)
#split the data as train and test
sample=sample.split(comp_data$price, SplitRatio = 0.70)
train_data=subset(comp_data, sample==TRUE)
test_data=subset(comp_data, sample=FALSE)

# apply the model
model=lm(price~(speed+hd+ram+screen+ads+trend), data=train_data)
# to include all independent variable
model=lm(price~., data=train_data)

summary(model)

test_data$price_predicted=predict(model, test_data)

MAPE(test_data$price, test_data$price_predicted)

#===29/12/2019===
#===MULTIPLE LINEAR REGRESSION EXAMPLE===
#===CHECKING MULTICOLLINEARITY===
#===FEATURE SELECTION===

#Business problem: Dependent Variable=Price

#Load the data
rm(list=ls())
comp_data=read.csv("Computer_Data.csv")

#Remove first column(serial number)
comp_data=comp_data[-1]

#Check multicollinearity among continuous 'independent variables'
head(comp_data,2)
cor(comp_data[c(2:5,9:10)])
#ram and hd=> 77% multicollinearity

#feature selection
# model1 with ram column, drop hd
library(dplyr)
head(comp_data)
ram_comp_data=comp_data[-3]
#ram_comp_data=select(comp_data, -hd)
head(ram_comp_data,3)

#model2 with hd column, drop ram
head(comp_data)
hd_comp_data=comp_data[-4]
#hd_comp_data=select(comp_data, -ram)
head(hd_comp_data)

library(caTools)
#splitting the data as train as test

sample1=sample.split(ram_comp_data$price, SplitRatio = 0.70)
train_ram=subset(ram_comp_data, sample1==TRUE)
test_ram=subset(ram_comp_data, sample1==FALSE)

sample2=sample.split(hd_comp_data$price, SplitRatio = 0.70)
train_hd=subset(hd_comp_data, sample2==TRUE)
test_hd=subset(hd_comp_data, sample2==FALSE)

#Model development
model1=lm(price~., data=ram_comp_data)
model2=lm(price~., data=hd_comp_data)

test_ram$pred_ram_price=predict(model1, test_ram)
test_hd$pred_hd_price=predict(model2, test_hd)

library(MLmetrics)
#summary to check: Adjusted R squared value
summary(model1)
#Adjusted R-squared with ram:  0.7465 
summary(model2)
#Adjusted R-squared with hd:  0.7016 

##keep ram, remove hd as ram is having more Adjusted R-squared value

#Checking the Error with MAPE
MAPE(test_ram$price, test_ram$pred_ram_price)
# MAPE gives 0.09888487 error with ram~~ 10%

MAPE(test_hd$price, test_hd$pred_hd_price)
# MAPE gives 0.1139551 error with ram~~ 11%

#=>ram is choosen as better feature over hd as it has lesser MAPE and greater Absolute R squared value


#===============12/01/2020===============
#=====LOGISTIC REGRESSION=====
#===TITANIC DATA ANALYSIS===========

#Buisiness Problem:TITANIC DATA:
#Develop a predictive model to predict survival probability
rm(list=ls())
getwd()
#Load the data
titanic=read.csv("titanic.csv", na.strings = '')
head(titanic,2)

#=Data Cleaning=
#Remove the unwanted columns
library(dplyr)
titanic=select(titanic, -PassengerId, -Name, -Ticket)

#Check the missing values in the data
apply(titanic, 2, function(x) sum(is.na(x))/length(x))

#Remove the column/variable which has 50% data missing
titanic=select(titanic, -Cabin)

#Replace missing values in Age & Embarked column
#Age
mean(titanic$Age, na.rm = TRUE)
median(titanic$Age, na.rm = TRUE)
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age, na.rm = TRUE)
#Embarked
prop.table(table(titanic$Embarked))

titanic$Embarked[is.na(titanic$Embarked)]='S'

#=Check the multicollinearity among continuous independent variables
cor(titanic[4:7])

#=Check for Outliers=
#Check for Outliers presence using plot
boxplot(titanic$Age)
#check 5th percentile and 95th percentile
quantile(titanic$Age, c(0.05,0.95))

#5th percentile value:in age column if any value less than 4, replace it by 6 
titanic$Age=ifelse(titanic$Age<4, 6, titanic$Age)
#95th percentile value:in age column if any value less than 54, replace it by 54
titanic$Age=ifelse(titanic$Age>54, 54, titanic$Age)

#=Modeling
#divide the data into training set and test set
library(caTools)
sample=sample.split(titanic$Survived, SplitRatio = 0.80)
trainset=subset(titanic, sample==TRUE)
testset=subset(titanic, sample==FALSE)

model=glm(Survived~., data=trainset, family = binomial())

summary(model)

#apply the model into testset
testset$predicted_probability=predict(model, testset, type='response')

#convert probabity values of predicted_probabity same as testset, i.e 0 and 1
testset$predicted_binary=ifelse(testset$predicted_probability>0.5, 1,0)

#Confusion Matrix
table(testset$Survived, testset$predicted_binary)

#   0  1
# 0 96 14
# 1 20 48

#accuracy=0.80%
#TPR/Recall/sensitivity=0.70
#TNR/Specificity=0.87
#FPR(1-Specificity)=0.13/0.127
#FNR=0.294
#precision=0.77
#F1score=0.73

