pca <- readRDS("~/Desktop/pca.rds")
pca13 <- pca$x[, 1:3]
clust <- hclust(dist(pca13), method = "complete")
