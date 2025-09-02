data('USArrests')
df <- scale(USArrests)
head(df,3)

set.seed(123)
# ==== Compute k-means with k=4 ====
km.res <- kmeans(df,4,nstart = 25)
km.res$cluster

km.res$cluster
head(km.res$cluster,4)

km.res$size

km.res$centers

# ==== Hierarchical Clustering ====

# Dissimilarity matrix
d <- dist(df,method='euclidean')

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d,method='complete')

# Plot hte obtarined dendrogram
plot(hc1,cex=0.6,hand=-1)

library(cluster)
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient ; amount of clustering structure found
hc4$dc

# Plot dendrogram
pltree(hc4,cex=0.6,hand=-1,main='Dendrogram of diana')

# Ward's method
hc5 <- hclust(d,method='ward.D2')

# Cut tree into 4 groups
sub_grp <- cutree(hc5,k=4)

# Number of members in each cluster
table(sub_grp)

plot(hc5,cex=0.6)
rect.hclust(hc5,k=4,border=2:5)






