rm(list=ls())
setwd("E:/deepak/study material/edwisor ,dataanlytics/business case study/major test/Major Project")
getwd()
loan <- read.csv("Data for cleaning & modeling.csv",header =T ,stringsAsFactors=T)
loan1 <- read.csv("Holdout for Testing.csv",header=T ,stringsAsFactors = T)
#understanding the structure of data
class(loan)
dim(loan) #400000 rows & 32 column
names(loan)
str(loan)

#view top
head(loan[,-16])
#view the bottom 6 data
tail(loan[,-16])
#convert into proper data type
#remove % to convert factor to numeric 
for(i in c(1,30)){
  loan[,i] = gsub("%" , "", loan[,i])
  loan[,i] = as.numeric(loan[,i])/100
   }
#convert 4:6 column into numeric 
for(i in c(4:6)){
  loan[,i]= gsub("\\$","",loan[,i])
  loan[,i]= gsub(",", "" ,loan[,i])
  }
str(loan[,4:6])
 for(i in c(4:6,22,27:31)) {
  loan[,i]= with(loan,as.numeric(loan[,i]))
 }
str(loan[,4:6])
#convert X10 to character
loan$X10 = as.character(loan$X10)
class(loan$X10)
apply(loan[,c(11,17)],2,table)

#replace white space with NA

loan[loan==""] =  NA
loan[loan=="n/a"] = NA
#let us derive year and month seperately
for(i in c(15,23)){
  loan[,i] = as.character(loan[,i])
  loan[,i] = as.Date(loan[,i],format="%d-%m-%y")
} 
loan$borrow_year= format(loan$X15,"%Y")
loan$credit_year = format(loan$X23,"%Y")
loan$borrow_month =format(loan$X15,"%b")
loan$credit_month =format(loan$X23,"%b")

#find length of unique value 
for(i in 1:ncol(loan)){
  print(length(unique(loan[,i])))
}
str(loan)

#visualize borrow_year
plot(loan$borrow_year,las=1,main="borrower_yearwise",col=2)
#missing value analysis
sum(is.na(loan)) #large no. of missing value
#analyse missing value by variable
apply(loan,2,function(x)sum(is.na(x)))
#store value in data frame 
Missing_loan_data <- data.frame(variables = colnames(loan),Missinginfo = apply(loan,2,function(x)sum(is.na(x))))
row.names( Missing_loan_data) = NULL
#drop variable X2 X3,,X18  
data = subset(loan,select = -c(X2,X3,X18))
#var. X16, X25 ,X26 have more than 50 % missing value ,so drop the respective variable as dropping it does not affect model
#before that lets check missing value in holdout data
apply(loan1,2,function(x)sum(is.na(x)))
Missing_loan_data1 <- data.frame(variables = colnames(loan1),Missinginfo = apply(loan1,2,function(x)sum(is.na(x))))
row.names( Missing_loan_data1) = NULL
data= subset(data, select = -c(X16,X25,X26))
apply(data,2,function(x)sum(is.na(x)))
data = data[complete.cases(data),]# as in holding data ,NA iS less so, no need for missing value imputation
#again explore the data
str(data)
#visualisation
#install.packages(ggplot2)
library(ggplot2)
#visualise the percent of interest rate
qplot(X1,data=data,main="distribution of IR",bins=25,col="blue")+geom_bar()
#visualise relateion between X7 &X8
table1<-table(data$X8,data$X7)
table1<-table1[-1,-1]
plot(table1,col=c(3,4),main="loan grade ~ payment",ylab="payment",xlab="loan grade",las=1)
#now i am interested to know the relation between X1,X7, X8 & X9
#try to find out Boxplot b/w x1 & x7
#plot(data$X7,data$X1,col=c(3,4),main='PAYMENT~ IR',las=1,Xlab= "payment",ylab="IR percent ")
data$X9= substr(data$X9,2,2)
data$X9 = as.numeric(data$X9)
qplot(X9,X1,data=data,colour=X7,main="distribution of IR wrt type of payment and grade of loan")+geom_point()+facet_wrap(~X8)
#tabulate 11 th column so as to binning 
table(data$X11)
#convert data to numeric then Bin year exp.
data$X11=gsub("<" ,"",data$X11) 
data$X11 =gsub("\\+", "", data$X11)
data$X11=gsub("years","",data$X11)
data$X11=gsub("year","",data$X11)
data$X11 =as.numeric(data$X11)
data$X11[data$X11 <= 3]= "entry"
data$X11[data$X11 > 3 & data$X11 <= 9] = "mid"
data$X11[data$X11 == 10 ] = "high"
class(data$X11)
table(data$X11)
data$X11=factor(data$X11)
#visualisation of X1 & x12
qplot(X1,X12,data=data,colour= X11)+geom_point()+facet_wrap(~X7)
boxplot(data$X1~data$X12,las=2,col=c(1:7),ylab="interest rate" ,main="IR~house ownership status")
qplot(X1,X13,data=data,colour= X11,main="IR~Annual income")+geom_point()+facet_wrap(~X7)

# classification
data$X14=gsub("VERIFIED - income source" , "verified",data$X14)
data$X14=gsub("VERIFIED - income" , "verified",data$X14)
table(data$X14)
unique(data$X17)
apply(data[,c(17,18,21:23,25)],2,summary)

#calculate rate of debt paid
data$X21 = data$X21 /100
#Normalize the data
str(data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data[,c(2:4,11,17,20:25)]  = as.data.frame(lapply(data[ ,c(2:4,11,17,20:25)], normalize))  

#outlier analysis 
#boxplot method
boxplot(data[,c(1:4,11,23,24,25)],ylab="IR percent",main="boxplot before outlier analysis ",col="darkblue",las=2)
outlier_function = function(x){ 
  for(i in x){ 
    y =  data [,i][! data [,i] %in% boxplot.stats( data [,i])$out]
  }
  return(y)
}
y= outlier_function(11)
z =outlier_function(24)
x=outlier_function(23)
new_data=data[-y,]
new_data=new_data[-z,]
new_data=new_data[-x,]
boxplot(new_data[,c(11,23,24,25)],ylab="IR percent",col=c(3:5),main="after outlier analysis")

#corelation 
model_data<- new_data[sapply(new_data ,function(x) length(levels(factor(x)))>1)]
model_data<-model_data[,c(1:4,11,17,18,20,21:25)]
M<-cor(model_data)
#cor.  
#install.packages("corrplot")
library(corrplot)
plot =corrplot(M,title ="correlation plot",t1.cex=0.5,t1.col="black")
#remove highly corelated variable 
new_data <- subset(new_data,select = -c(X5,X6))
#visualise IR wrt month,year
new_data$borrow_year<-factor(new_data$borrow_year)
new_data$borrow_month<-factor(new_data$borrow_month)
new_data$credit_year<-factor(new_data$credit_year)
new_data$credit_month<-factor(new_data$credit_month)
table<- table(new_data$X1,new_data$borrow_year)
table<-table[ ,-1]
barplot(table,las=2,col=c(2,3),beside=T,main="IR~Borrow yr",ylab="IR frequency",xlab= "borrower year")
#table1<- table(new_data$borrow_year,new_data$X7)
#table1<-table1[,-1]
#plot(table1,main="payment type~borrow year",col=c(2,3))
table2<-table(new_data$borrow_month,new_data$X7)
table2<-table2[,-1]
plot(table2,col=c(5,6),main="month~payment type") 
#sampling method
#lets check skewness of data
hist(new_data$X1,,col= "purple")   # uniform
hist(new_data$X4,col="purple")#-ve skew
hist(new_data$X13,col="purple")
hist(new_data$X21,col="purple")#uniform

#systematic sampling method
sys.sample = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
  #cat("The selected systematic sample is: \"", sys.samp, "\"\n")
}

lis = sys.sample(193262, 5000) #select the repective rows
new_data$index = 1:193262
systematic_data = new_data[which(new_data$index %in% lis),]
summary(systematic_data$X1)
summary(new_data$X1)
t.test(systematic_data$X1)
#train & test data
indexes <- sample(1:nrow(systematic_data),size = 0.67*nrow(systematic_data))
train=systematic_data[indexes,]
test =systematic_data[-indexes,]
t.test(train[,1])
t.test(test[,1])
#multicolinearity test
#check multicollearity
#install.packages(usdm)
library(usdm) 
vif(train[,c(1,2,9,15,16,18:23)])

#linear regression model
#lm_model = lm(X1 ~   X7 +X8+X9+X13+X14+X24+X28+X30 + borrow_year, data = train)
lm_model1 =lm(X1 ~   X7 +X8+X9+X13+X14+X24+X28+X30, data = train)
summary(lm_model1)
pred = predict(lm_model1, test[ ,c(3,4,5,9,10,16,18,20,22,25)])
test1=data.frame(pred)
summary(pred)
#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

mape(test[,1], pred)
#extract coefficient 
coeff =data.frame(lm_model1$coefficients)
coeff



#predict IR for holding data

loan1 <- read.csv("Holdout for Testing.csv",header=T )
#lets remove % to convert into numeric
for(i in 30) {
  loan1[,i] = gsub("%" , "", loan1[,i])
  loan1[,i] = as.numeric(loan1[,i])/100
}
#let us derive year and month seperately
#feature engineering
for(i in 15){
  loan1[,i] = as.character(loan1[,i])
  loan1[,i] = as.Date(loan1[,i],format="%d-%m-%y")
}
loan1$borrow_year= format(loan1$X15,"%Y") 
loan1$borrow_month =format(loan1$X15,"%b") 

# let us remove loan grade from subgrade
loan1$X9 = substr(loan1$X9,2,2)
#classification
loan1$X14=gsub("VERIFIED - income source" , "verified",loan1$X14)
loan1$X14=gsub("VERIFIED - income" , "verified",loan1$X14)
table(loan1$X14)
#remove variable and keep those which will fit in model
loan1 = loan1[,c(1,7,8,9,13,14,24,28,30)]
str(loan1)
loan1$X9 =as.numeric(loan1$X9)
#missing value analysis
sum(is.na(loan1[,-1]))
apply(loan1[,-1],2,function(x)sum(is.na(x)))
#install.packages("lattice")
loan1$X30[is.na(loan1$X30)] = mean(loan1$X30,na.rm=TRUE)
#normalise 
loan1[ ,c(5,7:9)]  = as.data.frame(lapply(loan1[ ,c(5,7:9)], normalize))
#fit model to predict IR
loan1$X1 =predict(lm_model1,loan1)
summary(loan1$X1)
#Visualisation
qplot(X9,X1,data=loan1,colour=X7,main="IR ~ type of payment and grade of loan1")+geom_point()+facet_wrap(~X8)
boxplot(loan1$X1,col=4,main="distribution of IR",ylab="IR percent")
plot(loan1$X1~loan1$X8,main="IR~loan grade")
loan1$X1= with(loan1,loan1$X1*100)
summary(loan1$X1)
#lets drive the major reason for loan
#text mining
post<- data.frame(loan$X16)
sum(is.na(post))
names(post)[1]= "reason"
row.names(post)=NULL
#load libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(SnowballC)
#select specific word
post$reason = substr(post$reason,10,100)
#delete the leading spaces 
post$reason=str_trim(post$reason)
class(post$reason)
post$reason =as.character(post$reason)
##preprocessing
#convert comment into corpus
postCorpus=Corpus(VectorSource(post$reason))
writeLines(as.character(postCorpus[[2]]))
#case folding 
postCorpus =tm_map(postCorpus,tolower)
#remove stop words
postCorpus=tm_map(postCorpus,removeWords,stopwords('english'))
#remove punctuation marks
postCorpus =tm_map(postCorpus,removePunctuation)
#remove numbers
postCorpus =tm_map(postCorpus,removeNumbers)
#remove unnecessary spaces
postCorpus =tm_map(postCorpus,stripWhitespace)
#remove common words ending 
postCorpus= tm_map(postCorpus,stemDocument)
#convert into plain text 
postCorpus =tm_map(postCorpus,PlainTextDocument) 
#create corpus
postCorpus=Corpus(VectorSource(postCorpus))
#remove the defined stop word
postCorpus_Wc =postCorpus
postCorpus_Wc=tm_map(postCorpus,removeWords, c('I','need','it','want','finance','am','Hi','debit card','borrower','added',stopwords('english')))
#word cloud
pal2 =brewer.pal(8,"Dark2")
png("wordcloud1.png",width=12,height = 8,units='in',res=300)
wordcloud(postCorpus_Wc,scale=c(5,.2),min.freq=10,max.words=150,random.order=FALSE,rot.per=.15,colors=pal2)
dev.off()




