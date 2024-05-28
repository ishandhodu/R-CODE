setwd("C:/Users/idhodapkar1/OneDrive - Babson College/AQM 2000/Data")
rm(list = ls())          
gc()                    
cat("\f")

# LOAD THE DATA
library(caret)
source('BabsonAnalytics.R')
library(factoextra)
Yelp_DF = read.csv("Yelp.csv")

# MANAGE THE DATA
Yelp_DF$ID = NULL
Yelp_DF = na.omit(Yelp_DF)
Standardizer = preProcess(Yelp_DF, method = c("scale","center"))
Yelp_DF = predict(Standardizer, Yelp_DF)

# PLOT THE ELBOW CHART
fviz_nbclust(Yelp_DF, kmeans, method = "wss")

# MAKE CLUSTERS
Num_clusters = 5
set.seed(4922)

Kmeans_std = kmeans(Yelp_DF, Num_clusters)
fviz_cluster(Kmeans_std, Yelp_DF, geom = c("point", "text"))
print(Kmeans_std)

Yelp_DF = removeOutliers(Yelp_DF)

Kmeans_std = kmeans(Yelp_DF, Num_clusters)
fviz_cluster(Kmeans_std, Yelp_DF, geom = c("point", "text"))
print(Kmeans_std)





















