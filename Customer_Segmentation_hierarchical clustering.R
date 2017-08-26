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
#unique(dat$Stage)
head(dat)
#Daisy function in R scales the data and computes the distance measures
library(cluster)
#?daisy
dmatrix<-daisy(dat,metric="euclidean",stand=TRUE)#stand=TRUE will scale the data
class(dmatrix)

distmatrix<-dist(dmatrix)
str(distmatrix)
d<-as.matrix(distmatrix)
class(d)

write.csv(d,"distmatrix.csv",row.names=F)
?hclust
Custcluster<-hclust(distmatrix,method="average")
str(Custcluster)

##--------------------------------------Step4 : Plotting a Dendogram---------------------------------------------

plot(as.dendrogram(Custcluster))
#plot(as.dendrogram(euroclust),labels=europe$Country)
#Devide to 5 clusters
rect.hclust(Custcluster, 7)


##--------------------------------------Step5 : Examining the hclust object---------------------------------------------


#The cluster height used in the dendogram
#Dissimilarity data
Custcluster$height

#labels for each of the objects being clustered
#Custcluster$labels<-europe$Country


#distance measure used
Custcluster$dist.method

##----------------------------------Step6 : Slicing the dendogram to get finite number of clusters---------------------------------------------

#To get flat clustering : 
k<-cutree(Custcluster,k = 7)
head(k)
#Once you have k which is the cluster no, attach it to your dataset
dat$cluster<-k

##----------------------------------Step7 : Profiling clusters---------------------------------------------
#You can now profile your data similar to kmeans clustering example

#Cluster wise Summaries
cmeans<-aggregate(dat[,2:8],
                  by=list(dat$cluster),
                  FUN=mean)
names(cmeans)
dim(cmeans)

#Population Summaries
options(scipen=999)
apply(dat[,2:8],2,mean)
apply(dat[,2:8],2,sd)

#Z value normalisation

#Function to calculate Z values
list<-names(cmeans)
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list))
{
  y<-(cmeans[,i+1] - apply(dat,2,mean)[i])/(apply(dat,2,sd)[i])
  cmeans<-cbind(cmeans,y)
  names(cmeans)[i+8]<-paste("z",list[i+1],sep="_")
  print(list[i+1])
}

cmeans<-cmeans[,-16]
write.csv(cmeans,"cmeans.csv",row.names=F)

library(sqldf)
sqldf("select count(*) from dat group by cluster")

