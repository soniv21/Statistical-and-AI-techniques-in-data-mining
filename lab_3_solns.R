library(FactoMineR)
eco_data <- read.csv("eco_dev_data.csv")
head(eco_data)

na.omit(eco_data)
eco_data <- eco_data[-1,]
eco <- eco_data[, -1]

for(i in 1:ncol(eco)){
  eco[,i] = as.numeric(eco[,i])
}
eco <- scale(eco)

pca_com <- prcomp(eco,center = TRUE, scale = TRUE)
kmean_clust <- kmeans(pca_com$x[,1:2],center = 3, nstart = 15)

dist <- sqrt(rowSums(pca_com$x[,1:2]^2))
outlierss <- which(dist > quantile(dist, 0.99))
outlierss
print(eco_data[c(outlierss),])

clean_data <- eco[c(-4,-15),]
clean_pca <- prcomp(clean_data)

ranking <- data.frame(country = eco_data$X[c(-4,-15)], PC1 = clean_pca$x[,1:2])  
ranking

kmean_clust <- kmeans(pca_com$x[,1:2],center = 3, nstart = 15)
eco_data$Cluster <- factor(kmean_clust$cluster)

colors <- c("red", "blue", "green") 
plot(eco_data$X.2, eco_data$X.1, 
     col = colors[eco_data$Cluster], 
     pch = 16,  # Use solid circles
     xlab = "GDP Growth Rate", 
     ylab = "GNP per Capita", 
     main = "Feature Clusters Based on PCA Components")

# Add a legend for the clusters
legend("topright", legend = unique(eco_data$Cluster), 
       col = colors, pch = 19)



### checking normality
pcascores <- pca_com$x[,1]
qqnorm(pcascores)
qqline(pcascores, col = "red")



############################### dataset - 2 #####################
bank_data <- read.csv("PS_bank_fin_ratio.csv")
head(bank_data)

na.omit(bank_data)
bank_data <- bank_data[-1,]
data_pca <- bank_data[, -1]
for(i in 1 : ncol(bank)){
  data_pca[, i] <- as.numeric(bank[,i])
}
data_pca <- scale(bank)

pca_res <- prcomp(data_pca, center = TRUE, scale = TRUE)

pca_scores <- pca_res$x[,1]
outliers <- abs(pca_scores) > 2*sd(pca_scores)

clean_data <- data_pca[c(-outliers)]
clean_data <- data_pca[, -1]

clean_pca <- prcomp(clean_data, center = TRUE, scale = TRUE)
ranking <- data.frame(Bank = bank_data$X, 
                      PC1 = clean_pca$x[,1])


set.seed(123)
kmeans_res <- kmeans(clean_pca$x[,1:2], center = 3, nstart = 15)
clean_data <- cbind(clean_data, Cluster = factor(kmeans_res$cluster))

