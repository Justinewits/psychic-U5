#Packages
library(survival)
library(MASS)
library(prodlim)
library(pec)
library(mlbench)
library(quantreg)
library(randomForestSRC)
library(foreign)
library(ggplot2)
library(ggRandomForests)
library(ipred)
library(haven)
library("tcltk2")
library("party")
library("partykit")
library("caret")
library(ggplot2)
library(gridExtra)
library(grid)


set.seed(290875)

### Datasets ###

#Preparing data##
##################################Southern Africa######################################
#Zimbabwe
Data <- read.csv("1999.csv")
head(Data)
New1999Zim<-data.frame(Data$Time, Data$status, Data$B4,Data$V025,Data$V149)
#write.csv(New1999Zim, "New1999Zim.csv")
Data <- read.csv("2006.csv")
New2006Zim<-data.frame(Data$Time, Data$status, Data$B4,Data$V025,Data$V149, Data$V190)
#write.csv(New2006Zim, "New2006Zim.csv")
Data <- read.csv("2011.csv")
New2011Zim<-data.frame(Data$Time, Data$status, Data$B4,Data$V025,Data$V149, Data$V190)
#write.csv(New2011Zim, "New2011Zim.csv")
Data <- read.csv("2015.csv")
New2015Zim<-data.frame(Data$Time, Data$status, Data$B4,Data$V025,Data$V149, Data$V190)
#write.csv(New2015Zim, "New2015Zim.csv")

#####Southern Africa | Data
## Zimbabwe
data1<-read.csv("New1999Zim.csv")
data2<-read.csv("New2006Zim.csv")
data3<-read.csv("New2011Zim.csv")
data4<-read.csv("New2015Zim.csv")

## Eastern Africa
## Uganda
data1<-read.csv("New2001Uganda.csv")
data2<-read.csv("New2006Uganda.csv")
data3<-read.csv("New2011Uganda.csv")
data4<-read.csv("New2016Uganda.csv")

## Western Africa 
## Ghana
data1<-read.csv("New1998Ghana.csv")
data2<-read.csv("New2003Ghana.csv")
data3<-read.csv("New2008Ghana.csv")
data4<-read.csv("New2014Ghana.csv")

## Central Africa 
# Chad
data1<-read.csv("New1996Chad.csv")
data2<-read.csv("New2004Chad.csv")
data3<-read.csv("New2014Chad.csv")




## Random forest Model building for each of the country 

Sambple1<-sort(sample(nrow(data1), nrow(data1)*.8))
Sambple2<-sort(sample(nrow(data2), nrow(data2)*.8))
Sambple3<-sort(sample(nrow(data3), nrow(data3)*.8))
Sambple4<-sort(sample(nrow(data4), nrow(data4)*.8))
model1 <- party::cforest(Surv(Data.Time,Data.status)~Data.B4+Data.V025+ Data.V149,control = cforest_unbiased(mtry=2, ntree = 200), data=data1[,-1][Sambple1,])
model2 <- party::cforest(Surv(Data.Time,Data.status)~.,control = cforest_unbiased(mtry=2, ntree = 200),data=data2[,-1][Sambple2,])
model3 <- party::cforest(Surv(Data.Time,Data.status)~.,control = cforest_unbiased(mtry=2, ntree = 200),data=data3[,-1][Sambple3,])
model4 <- party::cforest(Surv(Data.Time,Data.status)~.,control = cforest_unbiased(mtry=2, ntree = 200),data=data4[,-1][Sambple4,])
I1.1<-party::varimp(model1)
I1.2<-party::varimp(model2)
I1.3<-party::varimp(model3)
I1.4<-party::varimp(model4)
#names(I1.1)<-c("Sex of the child","Place of residence", "Mother's education level")
names(I1.2)<-c("Sex of the child","Place of residence", "Mother's education level", "Wealth index")
names(I1.3)<-c("Sex of the child","Place of residence", "Mother's education level", "Wealth index")
names(I1.4)<-c("Sex of the child","Place of residence", "Mother's education level","Wealth index")
FIPart1<-data.frame(I1.1)
FIPart2<-data.frame(I1.2,I1.3,I1.4)
colnames(FIPart1)<-"1999"
colnames(FIPart2)<-c("2006","2011","2015")

##Plots

FIPart2$Var_NAME<-c("Sex of the child","Place of residence", "Mother's education", "Wealth index")

pz1= ggplot(FIPart2, aes(x=reorder(Var_NAME,FIPart2[,1] ), y=FIPart2[,1]))+ 
  geom_point() +
  geom_segment(aes(x=Var_NAME,xend=Var_NAME,y=0,yend=FIPart2[,1],color=factor(Var_NAME)),size = 1) +
  ylab("CIF covariate  importance") +
  xlab("Variable Name")+
  ggtitle("2006")+
  coord_flip()+
  theme(legend.position = "none")
pz2=ggplot(FIPart2, aes(x=reorder(Var_NAME, FIPart2[,2]), y=FIPart2[,2]))+ 
  geom_point() +
  geom_segment(aes(x=Var_NAME,xend=Var_NAME,y=0,yend=FIPart2[,2],color=factor(Var_NAME)),size = 1) +
  ylab("CIF covariate  importance") +
  xlab("")+
  ggtitle("2011")+
  coord_flip()+
  theme(legend.position = "none")

pz3=ggplot(FIPart2, aes(x=reorder(Var_NAME, FIPart2[,3]), y=FIPart2[,3]))+ 
  geom_point() +
  geom_segment(aes(x=Var_NAME,xend=Var_NAME,y=0,yend=FIPart2[,3],color=factor(Var_NAME)),size = 1) +
  ylab("CIF covariate  importance") +
  xlab("")+
  ggtitle("2015")+
  coord_flip()+
  theme(legend.position = "none")

grid.arrange(pz1, pz2,pz3, nrow = 1)


# Model evaluation- 10 fold cross-validation

d<-function(df){
  errcv=list()
  folds <- cut(seq(1,nrow(df)),breaks=10,labels=FALSE)
  for(i in 1:10){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df[testIndexes, ]
    trainData <- df[-testIndexes, ]
    f <- as.formula(Surv(Data.Time,Data.status)~.)
    model <-  pecCforest(Surv(Data.Time,Data.status)~.,
                             control = cforest_unbiased(mtry=2, ntree = 200),
                             data=trainData)
    prederror<-cindex(model,formula=f,data=testData )
    errcv[i] = prederror$Concordant[[1]]/prederror$Pairs[[1]]
  }
  return(errcv)
}

d1<-d(data1)
d2<-d(data2)
d3<-d(data3)
#d4<-d(data4)
New<-data.frame(unlist(d1),unlist(d2),unlist(d3))
colnames(New)<-c( "1996","2004","2014")
write.csv(New,file="Chad_RSF.csv")

