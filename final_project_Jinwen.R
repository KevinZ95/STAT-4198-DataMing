
data <- read.csv(file = "D:/file/STAT4198 Final/fordTrain.csv",stringsAsFactors=TRUE)
indexes <- read.csv(file = "D:/file/STAT4198 Final/indexes.csv",header=FALSE, stringsAsFactors=TRUE)
# G49803308
last.two.digits<- 08
k<-last.two.digits+1

newdata<-data.frame(data[data[,1]%in%indexes[k,], ])
summary(newdata)
sum(is.na(newdata))

drops <- c("P8","V7","V9")
newdata<-newdata[ , !(names(newdata) %in% drops)]

TrialID <- as.factor(newdata$TrialID)
levels(TrialID)

table(newdata$IsAlert)


#P1 <- as.factor(newdata$P1)
#levels(P1)
hist(newdata$P1) 
summary(newdata$P1)

#P2 <- as.factor(newdata$P2)
#levels(P2)
hist(newdata$P2)
summary(newdata$P2)

#P3 <- as.factor(newdata$P3)
#levels(P3)
hist(newdata$P3)
#barplot(table(newdata$P3),xlab= 'P3')
summary(newdata$P3)

#P4 <- as.factor(newdata$P4)
#levels(P4)
hist(newdata$P4)
#barplot(table(newdata$P4),xlab= 'P4')
summary(newdata$P4)


#P5 <- as.factor(newdata$P5)
#levels(P5)
hist(newdata$P5)
#barplot(table(newdata$P5),xlab= 'P5')
summary(newdata$P5)

#P6 <- as.factor(newdata$P6)
#levels(P6)
hist(newdata$P6)
#barplot(table(newdata$P6),xlab= 'P6')
summary(newdata$P6)

#P7 <- as.factor(newdata$P7)
#levels(P7)
hist(newdata$P7)
#barplot(table(newdata$P7),xlab= 'P7')
summary(newdata$P7)

#E1 <- as.factor(newdata$E1)
#levels(E1)
hist(newdata$E1)
#barplot(table(newdata$E1),xlab= 'E1')
summary(newdata$E1)

#E2 <- as.factor(newdata$E2)
#levels(E2)
hist(newdata$E2)
#barplot(table(newdata$E2),xlab= 'E2')
summary(newdata$E2)

E3 <- as.factor(newdata$E3)
levels(E3)
#hist(newdata$E3)
barplot(table(newdata$E3),xlab= 'E3')
#summary(newdata$E3)

#E4 <- as.factor(newdata$E4)
#levels(E4)
hist(newdata$E4)
#barplot(table(newdata$E4),xlab= 'E4')
summary(newdata$E4)

#E5 <- as.factor(newdata$E5)
#levels(E5)
hist(newdata$E5)
#barplot(table(newdata$E5),xlab= 'E5')
summary(newdata$E5)

#E6 <- as.factor(newdata$E6)
#levels(E6)
hist(newdata$E6)
#barplot(table(newdata$E6),xlab= 'E6')
summary(newdata$E6)

E7 <- as.factor(newdata$E7)
levels(E7)
#hist(newdata$E7)
barplot(table(newdata$E7),xlab= 'E7')
#summary(newdata$E7)

E8 <- as.factor(newdata$E8)
levels(E8)
#hist(newdata$E8)
barplot(table(newdata$E8),xlab= 'E8')
#summary(newdata$E8)

E9 <- as.factor(newdata$E9)
levels(E9)
#hist(newdata$E9)
barplot(table(newdata$E9),xlab= 'E9')
#summary(newdata$E9)

#E10 <- as.factor(newdata$E10)
#levels(E10)
hist(newdata$E10)
#barplot(table(newdata$E10),xlab= 'E10')
summary(newdata$E10)

#E11 <- as.factor(newdata$E11)
#levels(E11)
hist(newdata$E11)
#barplot(table(newdata$E11),xlab= 'E11')
summary(newdata$E11)

#V1 <- as.factor(newdata$V1)
#levels(V1)
hist(newdata$V1)
#barplot(table(newdata$V1),xlab= 'V1')
summary(newdata$V1)

#V2 <- as.factor(newdata$V2)
#levels(V2)
hist(newdata$V2)
#barplot(table(newdata$V2),xlab= 'V2')
summary(newdata$V2)

#V3 <- as.factor(newdata$V3)
#levels(V3)
hist(newdata$V3)
#barplot(table(newdata$V3),xlab= 'V3')
summary(newdata$V3)

#V4 <- as.factor(newdata$V4)
#levels(V4)
hist(newdata$V4)
#barplot(table(newdata$V4),xlab= 'V4')
summary(newdata$V4)

V5 <- as.factor(newdata$V5)
levels(V5)
#hist(newdata$V5)
barplot(table(newdata$V5),xlab= 'V5')
#summary(newdata$V5)

#V6 <- as.factor(newdata$V6)
#levels(V6)
hist(newdata$V6)
#barplot(table(newdata$V6),xlab= 'V6')
summary(newdata$V6)

#V8 <- as.factor(newdata$V8)
#levels(V8)
hist(newdata$V8)
#barplot(table(newdata$V8),xlab= 'V8')
summary(newdata$V8)

V10 <- as.factor(newdata$V10)
levels(V10)
#hist(newdata$V10)
barplot(table(newdata$V10),xlab= 'V10')
#summary(newdata$V10)

#V11 <- as.factor(newdata$V11)
#levels(V11)
hist(newdata$V11)
#barplot(table(newdata$V11),xlab= 'V11')
summary(newdata$V11)

library(gridExtra)
library(ggplot2)
#P1
p1_1<-ggplot(data,aes(x=P1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
p1_2<-ggplot(data,aes(x=P1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(p1_1, p1_2, ncol=2)
#P2
p2_1<-ggplot(data,aes(x=P2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
p2_2<-ggplot(data,aes(x=P2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(p2_1, p2_2, ncol=2)
#P3
p3_1<-ggplot(data,aes(x=P3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=100)+theme_bw()
p3_2<-ggplot(data,aes(x=P3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=100)+theme_bw()
grid.arrange(p3_1, p3_2, ncol=2)
#P4
p4_1<-ggplot(data,aes(x=P4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
p4_2<-ggplot(data,aes(x=P4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(p4_1, p4_2, ncol=2)
#P5
p5_1<-ggplot(data,aes(x=P5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
p5_2<-ggplot(data,aes(x=P5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(p5_1, p5_2, ncol=2)
#P6
p6_1<-ggplot(data,aes(x=P6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=1000)+theme_bw()
p6_2<-ggplot(data,aes(x=P6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=1000)+theme_bw()
grid.arrange(p6_1, p6_2, ncol=2)
#P7
p7_1<-ggplot(data,aes(x=P7,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
p7_2<-ggplot(data,aes(x=P7,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(p7_1, p7_2, ncol=2)

#E1
e1_1<-ggplot(data,aes(x=E1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e1_2<-ggplot(data,aes(x=E1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e1_1, e1_2, ncol=2)
#E2
e2_1<-ggplot(data,aes(x=E2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e2_2<-ggplot(data,aes(x=E2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e2_1, e2_2, ncol=2)
#E3
e3_1<-ggplot(data,aes(x=E3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e3_2<-ggplot(data,aes(x=E3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e3_1, e3_2, ncol=2)
#E4
e4_1<-ggplot(data,aes(x=E4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e4_2<-ggplot(data,aes(x=E4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e4_1, e4_2, ncol=2)
#E5
e5_1<-ggplot(data,aes(x=E5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e5_2<-ggplot(data,aes(x=E5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e5_1, e5_2, ncol=2)
#E6
e6_1<-ggplot(data,aes(x=E6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e6_2<-ggplot(data,aes(x=E6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e6_1, e6_2, ncol=2)
#E7
e7_1<-ggplot(data,aes(x=E7,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e7_2<-ggplot(data,aes(x=E7,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e7_1, e7_2, ncol=2)
#E8
e8_1<-ggplot(data,aes(x=E8,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e8_2<-ggplot(data,aes(x=E8,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e8_1, e8_2, ncol=2)
#E9
e9_1<-ggplot(data,aes(x=E9,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e9_2<-ggplot(data,aes(x=E9,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e9_1, e9_2, ncol=2)
#E10
e10_1<-ggplot(data,aes(x=E10,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e10_2<-ggplot(data,aes(x=E10,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e10_1, e10_2, ncol=2)
#E11
e11_1<-ggplot(data,aes(x=E11,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
e11_2<-ggplot(data,aes(x=E11,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(e11_1, e11_2, ncol=2)

#V1
v1_1<-ggplot(data,aes(x=V1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v1_2<-ggplot(data,aes(x=V1,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v1_1, v1_2, ncol=2)
#V2
v2_1<-ggplot(data,aes(x=V2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v2_2<-ggplot(data,aes(x=V2,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v2_1, v2_2, ncol=2)
#V3
v3_1<-ggplot(data,aes(x=V3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=200)+theme_bw()
v3_2<-ggplot(data,aes(x=V3,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=200)+theme_bw()
grid.arrange(v3_1, v3_2, ncol=2)
#V4
v4_1<-ggplot(data,aes(x=V4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v4_2<-ggplot(data,aes(x=V4,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v4_1, v4_2, ncol=2)
#V5
v5_1<-ggplot(data,aes(x=V5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v5_2<-ggplot(data,aes(x=V5,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v5_1, v5_2, ncol=2)
#V6
v6_1<-ggplot(data,aes(x=V6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=250)+theme_bw()
v6_2<-ggplot(data,aes(x=V6,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=250)+theme_bw()
grid.arrange(v6_1, v6_2, ncol=2)
#V8
v8_1<-ggplot(data,aes(x=V8,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v8_2<-ggplot(data,aes(x=V8,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v8_1, v8_2, ncol=2)
#V10
v10_1<-ggplot(data,aes(x=V10,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v10_2<-ggplot(data,aes(x=V10,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v10_1, v10_2, ncol=2)
#V11
v11_1<-ggplot(data,aes(x=V11,group=IsAlert,fill=IsAlert))+  geom_histogram(position="dodge",binwidth=70)+theme_bw()
v11_2<-ggplot(data,aes(x=V11,group=IsAlert,fill=IsAlert))+  geom_histogram(position="fill",binwidth=70)+theme_bw()
grid.arrange(v11_1, v11_2, ncol=2)

#P~
pairs(newdata[,c(3,4,5,6,7,8,9,10)])
#E~
pairs(newdata[,c(3,11, 12,13,14,15,16,17,18,19,20,21)])
#V~
pairs(newdata[,c(3,22, 23,24,25,26,27,28, 29, 30)])

drops <- c("E1","E2","E5","E10","E11")
newdata<-newdata[ , !(names(newdata) %in% drops)]

drops <- c("P4","P7")
newdata<-newdata[ , !(names(newdata) %in% drops)]


plot(newdata$V1, newdata$V6)
drops <- c("V6")
newdata<-newdata[ , !(names(newdata) %in% drops)]

#install.packages("caret", repos = "http://cran.r-project.org",  dependencies = c("Depends", "Imports", "Suggests"))
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("Rcpp", dependencies = TRUE) 
library(ggplot2)
library(caret)
library(randomForest)

temp_train <- newdata[1:100,]
temp_train$IsAlert <- as.factor(temp_train$IsAlert)
#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 3, verbose = FALSE)
outcomeName<-'IsAlert'
predictors<-names(temp_train)[!names(temp_train) %in% outcomeName]
result <- rfe(temp_train[,predictors], temp_train[,outcomeName],rfeControl = control)
result
#just wannt to show you that I know how to do it, and hope I could get bonus points for this question

#Check indicator variables
unique(newdata$E3)
unique(newdata$E7)
unique(newdata$E8)
unique(newdata$E9)
unique(newdata$V5)
unique(newdata$V10)

#Create indicator variable
newdata$E3_1 = c(rep(0, length(newdata$IsAlert)))
newdata$E3_0 = newdata$E3_4 = newdata$E7_low = newdata$E7_high = newdata$E8_low = newdata$E8_high = newdata$E9_0 = newdata$E9_1 = newdata$V5_0 =  newdata$V5_1 = newdata$V10_low = newdata$V10_high = newdata$E3_1 

for (i in 1:length(newdata$IsAlert)) {
  #E3  
  if(newdata$E3[i] == 1) newdata$E3_1[i] <- 1
  if(newdata$E3[i] == 0) newdata$E3_0[i] <- 1
  if(newdata$E3[i] == 4) newdata$E3_4[i] <- 1
  
  #E7
  if(newdata$E7[i] <= 10) newdata$E7_low[i] <- 1
  if(newdata$E7[i] > 10) newdata$E7_high[i] <- 1
  
  #E8
  if(newdata$E8[i] <= 4) newdata$E7_low[i] <- 1
  if(newdata$E8[i] > 4) newdata$E7_high[i] <- 1
  
  #E9
  if(newdata$E9[i] == 1) newdata$E9_1[i] <- 1
  if(newdata$E9[i] == 0) newdata$E9_0[i] <- 1
  
  #V5
  if(newdata$V5[i] == 1) newdata$V5_1[i] <- 1
  if(newdata$V5[i] == 0) newdata$V5_0[i] <- 1
  
  #V10
  if(newdata$E8[i] <= 3) newdata$V10_low[i] <- 1
  if(newdata$E8[i] > 3) newdata$V10_high[i]<- 1
}


# Minimax transform the continuous variables
#P1
newdata$P1_mm <- (newdata$P1 - min(newdata$P1)) / (max(newdata$P1) - min(newdata$P1))
#P2
newdata$P2_mm <- (newdata$P2 - min(newdata$P2)) / (max(newdata$P2) - min(newdata$P2))
#P3
newdata$P3_mm <- (newdata$P3 - min(newdata$P3)) / (max(newdata$P3) - min(newdata$P3))
#P5
newdata$P5_mm <- (newdata$P5 - min(newdata$P5)) / (max(newdata$P5) - min(newdata$P5))
#P6
newdata$P6_mm <- (newdata$P6 - min(newdata$P6)) / (max(newdata$P6) - min(newdata$P6))
#E4
newdata$E4_mm <- (newdata$E4 - min(newdata$E4)) / (max(newdata$E4) - min(newdata$E4))
#E6
newdata$E6_mm <- (newdata$E6 - min(newdata$E6)) / (max(newdata$E6) - min(newdata$E6))
#V1
newdata$V1_mm <- (newdata$V1 - min(newdata$V1)) / (max(newdata$V1) - min(newdata$V1))
#V2
newdata$V2_mm <- (newdata$V2 - min(newdata$V2)) / (max(newdata$V2) - min(newdata$V2))
#V3
newdata$V3_mm <- (newdata$V3 - min(newdata$V3)) / (max(newdata$V3) - min(newdata$V3))
#V4
newdata$V4_mm <- (newdata$V4 - min(newdata$V4)) / (max(newdata$V4) - min(newdata$V4))
#V8
newdata$V8_mm <- (newdata$V8 - min(newdata$V8)) / (max(newdata$V8) - min(newdata$V8))
#V11
newdata$V11_mm <- (newdata$V11 - min(newdata$V11)) / (max(newdata$V11) - min(newdata$V11))

#new dataset
mydata_nn <- newdata[ , -c(4:22)]
mydata_nn <- mydata_nn[, -c(1, 2)]
mydata_nn <- mydata_nn[1:10000,]
#training data
training <- mydata_nn[1:7000, ]
#testing data
testing <- mydata_nn[7001:10000,]

library(nnet) # Requires package nnet
set.seed(2018)
myNewFrame <- data.frame(node = integer(0),maxit = integer(0), decay = integer(0) ,misscls.train = integer(0), misscls.test = integer(0))

for (m in c(10, 100, 10000)) {
  for (d in c(0, 0.1, 0.2)) {
    for (s in c(5, 10, 15)) {
      net.dat <- nnet(IsAlert~., data = training, size = s, decay = d, maxit = m)
      #table(round(net.dat$fitted.values, 1))
      
      estimated_isAlert=as.numeric(net.dat$fitted.values>0.5)
      
      T = table(estimated_isAlert,training$IsAlert)
      
      misscls.train<-sum((estimated_isAlert - training$IsAlert)^2)/length(training$IsAlert)
      misscls.train
      presdicted.testing<-as.numeric(predict(net.dat,testing )>0.5)
      Ttest = table(presdicted.testing, testing$IsAlert)
      misscls.test<-sum((presdicted.testing - testing$IsAlert)^2)/length(testing$IsAlert)
      misscls.test
      
      df <- data.frame(s, m, d, misscls.train, misscls.test)
      names(df)<-c("node", "maxit", "decay", "misscls.train", "misscls.test")
      myNewFrame <- rbind(myNewFrame, df)
      
    }
  }
}

myNewFrame

best_nn <- nnet(IsAlert~., data = newdata, size = 5, decay = 0, maxit = 100)
misscls.train<-sum((estimated_isAlert - training$IsAlert)^2)/length(training$IsAlert)
misscls.train
presdicted.testing<-as.numeric(predict(net.dat,testing )>0.5)
Ttest = table(presdicted.testing, testing$IsAlert)
misscls.test<-sum((presdicted.testing - testing$IsAlert)^2)/length(testing$IsAlert)
misscls.test
presdicted.testing.all<-as.numeric(predict(net.dat, newdata )>0.5)
Ttest = table(presdicted.testing.all, newdata$IsAlert)
misscls.test.all<-sum((presdicted.testing.all - newdata$IsAlert)^2)/length(newdata$IsAlert)
misscls.test.all

#new data set
drops <- c("E3", "E7", "E8", "E9" , "V5","V10")
mydata_lm<-newdata[ , !(names(newdata) %in% drops)]
mydata_lm <- mydata_lm[ , -c(30:42)]

mydata_lm <- mydata_lm[1:1000,]
#training data
training <- mydata_lm[1:700, ]
#testing data
testing <- mydata_lm[701:1000,]

summary(training)

logistic.model <- glm(IsAlert~., data = training, family = binomial(link="logit"))
summary(logistic.model)


logistic.model.cur <- glm(IsAlert~P3 + P6 + E4 + E6 + V1 +V2 + V3 + V4 + E9_1, data = training, family = binomial(link="logit"))
summary(logistic.model.cur)

library(MASS)
set.seed(10)
birthwt.step <- stepAIC(logistic.model.cur, trace = 1, direction="both")
birthwt.step$anova

logistic.model.best <- glm(IsAlert~P3 + E4 + E6 + V1 +V2 + V3 + V4, data = training, family = binomial(link="logit"))
estimated_isAlert<- predict(logistic.model.best, training, type = "response")
misscls.train<-sum((estimated_isAlert-training$IsAlert)^2)/ length(training$IsAlert)
misscls.train


presdicted.testing <- predict(logistic.model.best, testing, type = "response")
misscls.test<-sum((presdicted.testing - testing$IsAlert)^2)/length(testing$IsAlert)
misscls.test

presdicted.testing.all <- predict(logistic.model.best, mydata_lm, type = "response")
misscls.test.all<-sum((presdicted.testing.all - mydata_lm$IsAlert)^2)/length(mydata_lm$IsAlert)
misscls.test.all

#new data set
mydata_tree <- newdata[ , 3:22]
mydata_tree <- mydata_tree[1:1000,]
#training data
training <- mydata_tree[1:700, ]
#testing data
testing <- mydata_tree[701:1000,]

summary(training)

IsAlert=ifelse(mydata_tree$IsAlert == 1," yes"," No ")
mydata_tree <-  data.frame(mydata_tree[,-1],IsAlert)
summary(mydata_tree)

library(tree)
tree.mydata_tree =tree(IsAlert~.,data=mydata_tree )
summary (tree.mydata_tree )

#plot(tree.mydata_tree)
#text(tree.mydata_tree ,pretty =0)
library(rpart.plot)
rpart.plot::rpart.plot(rpart(IsAlert~ .,data = mydata_tree),main = "Tree")

ntrain=700
set.seed(2020)
train=sample(1: nrow(mydata_tree ),ntrain )
training = mydata_tree[train,]
testing=mydata_tree[-train ,]
tree.one =tree(IsAlert~.,data=training )
summary (tree.one )

IsAlert.test=IsAlert[-train ]
tree.pred=predict(tree.one, testing,type="class")
table(tree.pred ,IsAlert.test)

set.seed (2021)
cv.one =cv.tree(tree.one ,FUN=prune.misclass )
names(cv.one )
cv.one
plot(cv.one$size ,cv.one$dev ,type="b")

mybest = 17
prune.one = prune.misclass (tree.one ,best = mybest)
plot(prune.one)
text(prune.one ,pretty =0)
summary(prune.one)

IsAlert.train=IsAlert[train ]
tree.pred=predict(prune.one, training,type="class")
table(tree.pred ,IsAlert.train)
IsAlert.test=IsAlert[-train ]
tree.pred=predict(prune.one, testing,type="class")
table(tree.pred ,IsAlert.test)

library(randomForest)
bag.one =randomForest(IsAlert~.,data=mydata_tree,subset =train , mtry=4, ntree =200,importance=TRUE)
IsAlert.test=IsAlert[-train]
tree.pred = predict ( bag.one ,newdata =mydata_tree[-train ,],type="class")
table(tree.pred , IsAlert.test)

library("mlbench")
library("pROC")
library(caret)
#names(getModelInfo())

set.seed(2022)
mydata_last <- newdata[1:10000, ]
mydata_last$IsAlert <- as.factor(ifelse(mydata_last$IsAlert == 0, "No", "Yes"))

inTrain <- createDataPartition(y = mydata_last$IsAlert, p = .75, list = FALSE)
trainSet <- mydata_last[ inTrain,]
testSet <- mydata_last[-inTrain,]

#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
#based on the result from logistic regression
predictors<-c("P3", "E4", "E6", "V1", "V2", "V3", "V4")
outcomeName<-'IsAlert'


#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method = 'rf')

#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors],na.action = na.pass)


#Checking the accuracy of the random forest model
confusionMatrix(testSet$IsAlert,testSet$pred_rf)

#Training the knn model
model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn')

#Predicting using knn model
testSet$pred_knn<-predict(object = model_knn,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$IsAlert,testSet$pred_knn)

#Training the Logistic regression model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
#Predicting using Logistic regression model
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$IsAlert,testSet$pred_lr)

#Defining the training control
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)


#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)

#Training the knn model
model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Training the logistic regression model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#Predicting the out of fold prediction probabilities for training data
trainSet$OOF_pred_rf<-model_rf$pred$Y[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn<-model_knn$pred$Y[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_lr<-model_lr$pred$Y[order(model_lr$pred$rowIndex)]

#Predicting probabilities for the test data
testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$Y
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$Y
testSet$OOF_pred_lr<-predict(model_lr,testSet[predictors],type='prob')$Y

#Predictors for top layer models 
predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_lr') 

#GBM as top layer model 
model_gbm<-train(trainSet[,predictors_top],trainSet[,outcomeName],method='gbm')

#Logistic regression as top layer model
model_glm<-train(trainSet[,predictors_top],trainSet[,outcomeName],method='glm')

#predict using GBM top layer model
testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])

#predict using logictic regression top layer model
testSet$glm_stacked<-predict(model_glm,testSet[,predictors_top])

confusionMatrix(testSet$IsAlert,testSet$gbm_stacked)
confusionMatrix(testSet$IsAlert,testSet$glm_stacked)
















