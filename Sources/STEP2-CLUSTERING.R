############################################ NBRE CLUSTER HCLUST


pkgs <- c("factoextra",  "NbClust", "ggplot2", "fastcluster", "FactoMineR", "cluster", "dendextend", "dplyr", "purrr", "IntClust")
install.packages(pkgs)
library("factoextra")
library("NbClust")
library("ggplot2")
library("fastcluster")
library("FactoMineR")
library("cluster")
library("dendextend")
library("dplyr")
library("purrr")

# Silhouette method
fviz_nbclust(mydata, hcut, k.max = 50, method = "wss") +
  labs(subtitle = "Elbow method")
#Find manually the best value(the elbow)
nk2 <- 20

############################################ CLUSTER HCLUST
#DIANA
hc <- diana(mydata,metric = "euclidean")
sub_grp <-cutree(as.hclust(hc), k = nk2)


#AGNES methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(mydata, method = x)$ac
}
bestagnes <- map_dbl(m, ac)
flex <- agnes(mydata, method = "flexible", par.meth = 0.625)$ac
bestagnesvalue <- max(bestagnes)
bestagnes <- which.max(bestagnes)
if(flex>bestagnesvalue)
{
  bestagnes <- 5
}

if(bestagnes==4)
{
  hc2<- agnes(mydata, method = "ward")
  pltree(hc2,cex = 0.6, main = "Dendrogram of agnes-ward")
}else if(bestagnes==5)
{
  hc2<- agnes(mydata, method = "flexible", par.meth = 0.625)
  pltree(hc2,cex = 0.6, main = "Dendrogram of agnes-flexible")
}else if(bestagnes==1)
{
  hc2<- agnes(mydata, method = "average")
  pltree(hc2,cex = 0.6, main = "Dendrogram of agnes-average")
}else if(bestagnes==2)
{
  hc2<- agnes(mydata, method = "single")
  pltree(hc2,cex = 0.6, main = "Dendrogram of agnes-single")
}else if(bestagnes==3)
{
  hc2<- agnes(mydata, method = "complete")
  pltree(hc2,cex = 0.6, main = "Dendrogram of agnes-complete")
}

sub_grp2 <- cutree(as.hclust(hc2), k = nk2)

#see the silhouette of best hclust
d1 <- dist(mydata, method = "euclidean")
sil1 <- silhouette(sub_grp, d1)
sil2 <- silhouette(sub_grp2, d1)

#keep the best hclust
if(mean(sil1[,3])>mean(sil2[,3]))
{
  rect.hclust(hc, k = nk2, border = 2:5)
  bestcluster <- sub_grp
  bestsil <- sil1
}else{
  rect.hclust(hc2, k = nk2, border = 2:5)
  bestcluster <- sub_grp2
  bestsil <- sil2
}
fviz_silhouette(bestsil)

# see difference between the two methods
# hc3 <- as.dendrogram(hc)
# hc4 <- as.dendrogram(hc2)
# tanglegram(hc3,hc4)

############################################ ADD CLUSTER TO THE WHOLE DATASET
mysample <- mutate(mysample, cluster = bestcluster)
rang <- aggregate.data.frame(mysample$slope, list(mysample$cluster), function(x) c(min(x), max(x), mean(x)))

#add cluster if point is in range
seqen <- mutate(seqen, meancluster = NA, ncluster= NA)
for(i in 1:nrow(seqen))
{
  test=0
  j=1
  while(test==0 && j<=nrow(rang))
  {
    
    if(seqen$slope[i]>=rang$x[j,1] && seqen$slope[i]<=rang$x[j,2])
    {
      seqen$meancluster[i]=rang$x[j,3]
      seqen$ncluster[i]=j
      test <- 1
    }
    j=j+1
  }
}

#add closest or new cluster to outlier points at most than 5% of its value to the closest cluster
for(i in 1:nrow(seqen))
{
  if(is.na(seqen$ncluster[i]))
  {
    dis=abs(seqen$slope[i]-rang$x[1,3])
    temp=1
    j=1
    while(j <= nrow(rang))
    {
      if(dis>abs(seqen$slope[i]-rang$x[j,3]))
      {
        dis=abs(seqen$slope[i]-rang$x[j,3])
        temp=j
      }
      j=j+1
    }
    # if(dis>abs(0.1*seqen$slope[i]))
    # {
    #   maxr <- max(rang$Group.1)
    #   seqen$ncluster[i]<- maxr+1
    #   rang <- rbind(rang, data.frame(Group.1=maxr+1, x=c(seqen$slope[i], seqen$slope[i], seqen$slope[i])))
    # }else{
      seqen$meancluster[i] <- rang$x[temp,3]
      seqen$ncluster[i] <- temp
    # }
  }
}

#silhouette with all the data
d3 <- dist(seqen$slope, method = "euclidean")
sil3 <- silhouette(seqen$ncluster, d3)
fviz_silhouette(sil3)

#wss of clusters
x.SS <- aggregate(seqen$slope, by=list(seqen$nclustkmeans), function(x) sum(scale(x, scale=FALSE)^2))
SS <- rowSums(x.SS) # Sum of squares for each cluster
xTSS <- sum(x.SS)  # Total (within) sum of squares

#rearrange the clusters with the new data&clusters
cl <- kmeans(seqen$slope,nstart=25, max(rang$Group.1))
seqen <- mutate(seqen, nclustkmeans=cl$cluster, meankmeans= NA)
for(i in 1:nrow(seqen))
{
  seqen$meankmeans[i]<-cl$centers[seqen$nclustkmeans[i]]
}
d3 <- dist(seqen$slope, method = "euclidean")
sil3 <- silhouette(seqen$nclustkmeans, d3)
fviz_silhouette(sil3)

#compare clustering
diff <- 0
for(i in 1:nrow(seqen))
{
  if(seqen$ncluster[i]!=seqen$nclustkmeans[i])
  {
    diff <- diff+1
  }
}

#mean wss and percent of the average element per cluster
x.mn <- aggregate(seqen$slope, by=list(seqen$ncluster), function(x) c(mean(x), max(x), min(x)))
x.mn
