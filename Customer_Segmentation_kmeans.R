setwd("E:\\Customer Segmentation Project-BML-R")
dat<-read.csv("Rejuvenated.csv")
head(dat)
str(dat)
colSums(is.na(dat))
#unique(dat$city)
dat[-is.na(dat$gender),]
dat<-dat[,-c(1,2,4,6,7,8)]

str(dat)

which(is.na(dat$gender))
dat<-dat[-which(is.na(dat$gender)),]

dat$gender<-ifelse(dat$gender=="M",1,0)

##--------------------------------------Step2 : Scaling the data ---------------------------------------------
#(column  - mean(column))/sd(column)
#Repeat for all columns

list<-names(dat)
scaled_data<-data.frame(rownum<-1:717)
for(i in 1:length(list))

  {
  
  x<-(dat[,i]-mean(dat[,i]))/(sd(dat[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}

head(scaled_data)

scaled_data<-scaled_data[,-1]
# 
dat<-cbind(dat,scaled_data)
names(dat)


##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------

#syntax : kmeans(scaled_data,k) ; where k refers to the number of clusters
set.seed(200)#Cluster being generated are permanently fixed
names(dat)
fit.km<-kmeans(dat[,13:24],7)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares of withinss an betweenss
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss


##--------------------------------------Step4 : find the optimal number of clusters (k value) ---------------------------------------------

#Create a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15
number<-1:15

for (i in 1:15)
  
{
  wss[i]<-kmeans(dat[,13:24],i)$tot.withinss
}

#Shortlised optimal number of clusters : between 7 and 9

#Better plot using ggplot2
library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))


##--------------------------------------Step5a : Rerun the algorithm with k=8(optimal no)---------------------------------------------

#Build 8 cluster model
set.seed(100)
fit.km<-kmeans(dat[,13:24],8)


##Merging the cluster output with original data

dat$cluster<-fit.km$cluster
str(dat)

##--------------------------------------Step5b : Profile the clusters---------------------------------------------


#Cluster wise Aggregates
cmeans<-aggregate(dat[,1:6],by=list(dat$cluster),FUN=mean)
cmeans
dim(cmeans)

#Population Aggregates
apply(dat[,1:6],2,mean)
apply(dat[,1:6],2,sd)

list1<-names(cmeans)

#Z score calculation
#calculated to show how far the cluster is from the population mean
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list1))
{
  y<-(cmeans[,i+1] - apply(dat,2,mean)[i])/(apply(dat,2,sd)[i])
  cmeans<-cbind(cmeans,y)
  names(cmeans)[i+7]<-paste("z",list1[i+1],sep="_")
  print(list1[i+1])
}

cmeans<-cmeans[,-14]
names(cmeans)

write.csv(cmeans,"cmeans.csv",row.names = F)
