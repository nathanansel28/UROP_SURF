setwd("C:\\SUTD\\UROP\\UROP_ScrapingTrafficData\\24.02.20 Activity Plan ML\\TripSample")
set.seed(1234)
trip_OD <- read.csv("trip_OD random sample 2 (10,000).csv")
# trip_OD only consists of 10,000 random data points 
# Because R cannot process 40,000 datapoints due to computational limitations
head(trip_OD)

# ================================
#       K MEANS CLUSTERING
# ================================
# Step 1: Choosing the optimal K (the optimal number of clusters)
if (!require("factoextra")) {
  install.packages("factoextra")
}
library(factoextra)

fviz_nbclust(trip_OD, kmeans, method = "wss") # Elbow Method
fviz_nbclust(trip_OD, kmeans, method = "silhouette") # Silhouette Method

# Setting K=4
kmeans.result <- kmeans(trip_OD, 4)
kmeans.result
kmeans.result$tot.withinss

# Setting K=14
kmeans.result <- kmeans(trip_OD, 14)
kmeans.result
kmeans.result$tot.withinss

# REMOVING SEVERAL VARIABLES
trip_OD_filtered <- trip_OD[,6:8]
head(trip_OD_filtered)
fviz_nbclust(trip_OD_filtered, kmeans, method = "wss") # Elbow Method
fviz_nbclust(trip_OD_filtered, kmeans, method = "silhouette") # Silhouette Method

# Setting K=4
kmeans4.result <- kmeans(trip_OD_filtered, 4)
kmeans4.result
kmeans4.result$tot.withinss

# Setting K=14
kmeans14.result <- kmeans(trip_OD_filtered, 14)
kmeans14.result
kmeans14.result$tot.withinss

kmeans4.result$tot.withinss
kmeans14.result$tot.withinss

# FOR LOOP
for (i in 11:20) {
  # print("==================")
  # print(paste("Executing for K=", i))
  # print("==================")
  # print(i)
  kmeans.result <- NULL
  kmeans.result <- kmeans(trip_OD_filtered, i)
  print(kmeans.result)
  # print(kmeans.result$size)
  # print(kmeans.result$centers)
  # print(kmeans.result$tot.withinss)
}

kmeans.result <- kmeans(trip_OD_filtered, 14)
print(kmeans.result)

