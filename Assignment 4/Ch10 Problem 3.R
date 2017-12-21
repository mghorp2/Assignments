
#part a
x1<-c(1, 1, 0, 5, 6, 4)
x2<-c(4, 3, 4, 1, 2, 0)
df <- as.data.frame(cbind(x1,x2))
plot(df$x1,df$x2,xlab = "x1",ylab = "x2")

#part b
set.seed(3)
df$Y<-NA
df$Y <- sample(2, nrow(df), replace = T)
df
plot(df$x1, df$x2, col = (df$Y + 1), pch = 20, cex = 2,xlab = "x1",ylab = "x2")

#part c

# calcualting centroids
centroid1 <- c(mean(df[df$Y == 1, 1]), mean(df[df$Y == 1, 2]))
centroid2 <- c(mean(df[df$Y == 2, 1]), mean(df[df$Y == 2, 2]))
plot(df$x1,df$x2, col=(df$Y + 1), pch = 20, cex = 2,xlab = "x1",ylab = "x2")
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)


centroids = aggregate(df[,1:2], list(Cluster = df$Y), mean)
centroids

#part d
library(class)
clusters = knn(centroids[,2:3], df[,1:2], factor(centroids[,1]))
clusters

n=6
col = rep("red", n)
col[clusters == 2] = "blue"
plot(df[,1:2], col = col, pch = 20, cex = 2,xlab = "x1",ylab = "x2")
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)


#part e

centroid1 <- c(mean(df[df$Y == 1, 1]), mean(df[df$Y == 1, 2]))
centroid2 <- c(mean(df[df$Y == 2, 1]), mean(df[df$Y == 2, 2]))
plot(df$x1,df$x2, col=(df$Y + 1), pch = 20, cex = 2,xlab = "x1",ylab = "x2")
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

#part f
plot(df$x1,df$x2, col=(df$Y + 1), pch = 20, cex = 2,xlab = "x1",ylab = "x2")
