######################## dataset - 3 #####################

protein_data <- read.csv("eur_protein_consump.csv")
head(protein)

na.omit(protein)
countries <- protein$Country

protein <- protein[, -1]
protein <- scale(protein)

pca <- prcomp(protein)
summary(pca)
biplot(pca)
plot(pca)
dim(pca$rotation)


### variation
var<- (pca$sdev)^2
var1 <- var/sum(var)

plot(var1, type = "b", main = "Scree Plot", 
     xlab = "Principal Components", ylab = "Proportion of Variance Explained")


#### calculating outliers 
distance <- sqrt(rowSums(pca$x[, 1:3]^2))
outliers <- which(distance > quantile(distance, 0.90))
outliers
protein_data[outliers,]
plot(distance)

### clustering k means
set.seed(123)
kmeans_cluster <- kmeans(pca$x[, 1:2], centers = 4, nstart = 25)
fviz_cluster(kmeans_cluster, data = pca$x[, 1:2], geom = "point", 
             ellipse.type = "convex", ggtheme = theme_minimal(), 
             main = "Clusters of Countries in PCA Projection")

cluster_centers <- aggregate(protein, by = list(cluster = kmeans_cluster$cluster), mean)
print(cluster_centers)

mean(kmeans_cluster$cluster ==1)



###################################### dataset - 4 ############################
bank_data <- read.csv("PS_bank_fin_ratio.csv")
head(bank_data)

na.omit(bank_data)
bank_data <- bank_data[-1,]
bank <- bank_data[, -1]
for(i in 1 : ncol(bank)){
  bank[, i] <- as.numeric(bank[,i])
}
bank <- scale(bank)
str(bank)

pca_res <- prcomp(bank)
summary(pca_res)

### calculting outliers
distances <- sqrt(rowSums(pca_res$x[, 1:2]^2))
outlier <- which(distances > quantile(distances, 0.99))
bank_data[c(outlier),]
plot(distances, pch = 16)


#### clustering
clusters <- kmeans(pca_res$x, centers = 4, nstart = 10)

between <- function(data, cluster){
  total_bet <- 0
  for(i in unique(cluster)){
    points <- data[cluster == i,]
    centroid <- colMeans(points)
    total_bet <- total_bet + sum(rowSums((points - centroid)^2))
  }
  return (total_bet)
}
between(bank, clusters$cluster)

############################ dataset-1 ###################

library(readr)
library(ggplot2)
eco_data <- read.csv("eco_dev_data.csv")
head(eco_data)

eco_data <- eco_data[ -1, ] 
colnames(eco_data)[1] <- "Country"

for(i in 2:ncol(eco_data)){
  eco_data[, i] <- as.numeric(eco_data[, i])
}

pca <- prcomp(eco_data[, -1], scale = TRUE)
pca_data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], country = eco_data$Country)

ggplot(pca_data, aes(x = PC1, y = PC2, label = eco_data$Country))+
  geom_point(alpha = 0.7)+
  geom_text(vjust = -0.5, size = 2) +
  ggtitle("PCA of Economic Development Indicators") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()


pca_summary <- summary(pca)

pca_summary$importance[2,]
pca_summary$importance[3,]


# Get the proportion of variance explained by each principal component
explained_variance <- pca$sdev^2 / sum(pca$sdev^2)

# Create a scree plot
plot(explained_variance, type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot")

# Add a line for cumulative variance
cumulative_variance <- cumsum(explained_variance)
lines(cumulative_variance, type = "b", pch = 19, col = "red")

# Optionally, add a horizontal line at y = 1 to highlight the variance explained by each component
abline(h = 0.1, col = "blue", lty = 2)
