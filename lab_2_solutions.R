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
