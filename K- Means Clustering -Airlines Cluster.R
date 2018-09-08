
#======================Clustring Project- Airlines Cluster By SHASHANK TANWAR=========================================

#Setting up Enviromnent for Clustering

list.of.packages <- c("datasets", "ggplot2","cluster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(cluster)
library(ggplot2)
library(caret)
#Importing Dataset
getwd()

Airlines<- read.csv("AirlinesCluster.csv")
str(Airlines)
dim(Airlines)
summary(Airlines)

# Checking Null Values

colSums(is.na(Airlines))

# Standarising the dataset
library(standardize)

Airlines.scaled<- scale(Airlines, center = TRUE, scale = TRUE)
summary(Airlines.scaled)

# K-means Clustering
set.seed(24)

# geting within sum of squares for different no of clusters.
WSS<- list() 
K <- 1:20
for (i in K) {
  kmeans(Airlines.scaled, i,iter.max = 1000)
  WSS<- append(WSS,values = sum(kmeans(Airlines.scaled,i, iter.max = 1000)$withinss))
}
WSS

#WSS dataframe

WSSdata<- cbind(K, WSS)
  
WSSdata<-  as.data.frame(WSSdata)

plot(WSSdata, xlab = 'No of Clusters', ylab = 'WSS' )

# Hence the required no of clusters can be 5 or 6 clusters.

K= 5

AirlinesCluster<- kmeans(Airlines.scaled, K, iter.max = 1000)
summary(AirlinesCluster)

AirlinesCluster$withinss
table(AirlinesCluster$cluster)
AirlinesCluster$centers

#Adding Cluster column in the data frame
Airlines$Cluster<- AirlinesCluster$cluster
write.csv(Airlines, "K-means ClusterResults.csv")

#Cluster summary
library(dplyr)
colnames(Airlines)
Cluster_means<- summarise(group_by(Airlines, Cluster), Balance = mean(Balance, na.rm = TRUE), 
                          QualMiles= mean(QualMiles, na.rm = TRUE),BonusMiles= mean(BonusMiles, na.rm = TRUE),
                          BonusTrans= mean(BonusTrans, na.rm = TRUE),FlightMiles= mean(FlightMiles, na.rm = TRUE),
                          FlightTrans = mean(FlightTrans, na.rm = TRUE), DaysSinceEnroll = mean(DaysSinceEnroll, na.rm = TRUE))

write.csv(Cluster_means, "K-means Cluster Means summary.csv")

#===================Creating Hierarchical Clustering=====================================


#Hierarchical Clustering

Airlines$Cluster<- NULL 
distances<- dist(Airlines,method = "euclidean") #calculating distances
AirlinesHcluster<- hclust(distances, method = "ward.D")
plot(AirlinesHcluster) #ploting dendrogram

# getting clusters using cutree
Hclusters<-cutree(AirlinesHcluster, k=5)
table(Hclusters)

# Creating Hcluster column
Airlines$hcluster<- Hclusters
write.csv(Airlines, "Hierarchical Cluster Result.csv")
#Creating cluster means summary table

Hcluster_means<- summarise(group_by(Airlines, hcluster), Balance = mean(Balance, na.rm = TRUE), 
                          QualMiles= mean(QualMiles, na.rm = TRUE),BonusMiles= mean(BonusMiles, na.rm = TRUE),
                          BonusTrans= mean(BonusTrans, na.rm = TRUE),FlightMiles= mean(FlightMiles, na.rm = TRUE),
                          FlightTrans = mean(FlightTrans, na.rm = TRUE), DaysSinceEnroll = mean(DaysSinceEnroll, na.rm = TRUE))

write.csv(Hcluster_means, "Hierarchical Clusters mean summary.csv")



