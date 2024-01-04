getwd()
setwd("Desktop/DatamininigProject")
dataset = read.csv('Exportedindian.csv') 

##prepreocessing 
#Data types should be transformed into numeric types before clustering.
dataset$Gender=factor(dataset$Gender,levels=c("F","M"),labels=c(1,0))
dataset$Gender= as.numeric(dataset$Gender)
dataset$Age=dataset$Age/10
#for deleting the target class
dataset=subset(dataset,select=-c(Liver_Problem))

# k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
dataset=scale(dataset)
  # run kmeans clustering to find 4 clusters
  kmeans.result <- kmeans(dataset, 4)
# print the clusterng result
kmeans.result
## visualize clustering
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
view(dataset)
###Cluster Validation
library(cluster)
#average for each cluster
avg_sil <- silhouette(kmeans.result$cluster,dist(dataset))
fviz_silhouette(avg_sil)#k-means clustering with estimating k and initializations
# silhoutte width for each cluster

dataset=scale(dataset)
# run kmeans clustering to find 2 clusters
kmeans.result <- kmeans(dataset, 2)
# print the clusterng result
kmeans.result
## visualize clustering
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
view(dataset)
###Cluster Validation
library(cluster)
#average for each cluster
avg_sil <- silhouette(kmeans.result$cluster,dist(dataset))
fviz_silhouette(avg_sil)#k-means clustering with estimating k and initializations
# silhoutte width for each cluster


dataset=scale(dataset)
# run kmeans clustering to find 3clusters
kmeans.result <- kmeans(dataset, 3)
# print the clusterng result
kmeans.result
## visualize clustering
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
view(dataset)
###Cluster Validation
library(cluster)
#average for each cluster
avg_sil <- silhouette(kmeans.result$cluster,dist(dataset))
fviz_silhouette(avg_sil)#k-means clustering with estimating k and initializations
# silhoutte width for each cluster



# silhoutte width for all cluster
fviz_nbclust(dataset,kmeans,method="silhouette")+labs(subtitle="Silhouette method")