library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

wine <- read.csv("Wine_data.csv")
head(wine)
type <- wine$Type[1:178]

wine <- wine[,2:13]
df <- scale(wine)
df <- na.omit(df)
dist_mat <- dist(df)

hc_complete <- hclust(dist_mat, method = "complete")
plot(hc_complete, main = "Hierarchical Clustering - Complete Linkage", xlab = "", sub = "")

## single linkage
hc_single <- hclust(dist_mat, method = "single")
plot(hc_single, main = "single linkage")
hc_single$order
hc_single$height


#### kmean clustering
set.seed(123)

kmeans_res <- kmeans(df, centers = 3, nstart = 5)

# Cut tree to form 3 clusters (same as K=3)
hc_complete_cut <- cutree(hc_complete, k = 3)


# Create a confusion matrix to compare Type with K-means clusters
table(type, kmeans_res$cluster)

# Create a confusion matrix to compare Type with hierarchical clustering results
table(type, hc_complete_cut)

within_cluster_scatter <- function(data, clusters) {
  total_within_ss <- 0
  for (i in unique(clusters)) {
    cluster_data <- data[clusters == i, ]
    centroid <- colMeans(cluster_data)
    total_within_ss <- total_within_ss + sum(rowSums((cluster_data - centroid) ^ 2))
  }
  return(total_within_ss)
}
within_cluster_scatter(df,hc_complete_cut)
within_cluster_scatter(df,kmeans_res$cluster)




#### rape dataset
crime <- read.csv("CrimesOnWomenData.csv")
head(crime)

crime <- na.omit(crime)
state <- crime$State
year <- crime[crime$Year==2001,] ##For year 2001
year <- year[4:10]

## calculating distance matrix
crime_dat <- dist(year)

clust_com <- hclust(crime_dat, method = "complete")
clust_sin <- hclust(crime_dat, method = "single")
plot(clust_com)
plot(clust_sin)


kmeans_clust <- kmeans(year, centers = 5, nstart = 5)
clust_com_cut <- cutree(clust_com, k =5) ## cutting complete heirirchical clustering into k = 5
clust_sin_cut <- cutree(clust_sin, k = 5)

table(clust_com_cut, kmeans_clust$cluster)
table(clust_sin_cut, kmeans_clust$cluster)

# Function to calculate within-cluster scatter
within_cluster_scatter <- function(data, clusters) {
  total_within_ss <- 0
  for (i in unique(clusters)) {
    cluster_data <- data[clusters == i, ]
    centroid <- colMeans(cluster_data)
    total_within_ss <- total_within_ss + sum(rowSums((cluster_data - centroid) ^ 2))
  }
  return(total_within_ss)
}

# Function to calculate between-cluster scatter
between_cluster_scatter <- function(data, clusters) {
  overall_centroid <- colMeans(data)
  total_between_ss <- 0
  for (i in unique(clusters)) {
    cluster_data <- data[clusters == i, ]
    cluster_size <- nrow(cluster_data)
    centroid <- colMeans(cluster_data)
    total_between_ss <- total_between_ss + cluster_size * sum((centroid - overall_centroid) ^ 2)
  }
  return(total_between_ss)
}

# Compute within-cluster and between-cluster scatter for complete-linkage hierarchical clustering
within_ss_complete <- within_cluster_scatter(year, clust_com_cut)
between_ss_complete <- between_cluster_scatter(year, clust_com_cut)

# Compute within-cluster and between-cluster scatter for single-linkage hierarchical clustering
within_ss_single <- within_cluster_scatter(year, clust_sin_cut)
between_ss_single <- between_cluster_scatter(year, clust_sin_cut)

# Compute within-cluster and between-cluster scatter for K-means clustering
within_ss_kmeans <- within_cluster_scatter(year, kmeans_clust$cluster)
between_ss_kmeans <- between_cluster_scatter(year, kmeans_clust$cluster)

