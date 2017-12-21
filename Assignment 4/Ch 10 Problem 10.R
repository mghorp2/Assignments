set.seed(2)
X = rbind(matrix(rnorm(20 * 50), nrow = 20, byrow = T),
          matrix(rnorm(20 * 50), nrow = 20, byrow = T)+6,
          matrix(rnorm(20 * 50), nrow = 20, byrow = T)+12)
y = rep(c(1, 2, 3), c(20, 20, 20))


#part b
pca = prcomp(X)
plot(pca$x[,1:2], col = y)


#part c
k_means_cluster = kmeans(X, centers = 3)
table(y, k_means_cluster$cluster)


#part d
k_means_clus_2 = kmeans(X, centers = 2)
table(y, k_means_clus_2$cluster)

#part e
k_means_clus_4 = kmeans(X, centers = 4)
table(y, k_means_clus_4$cluster)

#part f
k_means_pca_3 = kmeans(pca$x[,1:2], centers = 3)
table(y, k_means_pca_3$cluster)

#part g
k_scale = kmeans(scale(X), centers = 3)
table(y, k_scale$cluster)


