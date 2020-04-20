---
title: "SmartHomeV1"
Authors: Guillaume GUERARD & Sonia DJEBALI
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Overview

The first step is to analyse the time serie and to decompose it into several sequences.

Take an overview of the time serie:

```{r}
filename <- read.csv("C:/Users/Rhaada/Downloads/HomeC/2016/HomeC-meter1_2016.csv")
seqen <- mutate(filename, id = rownames(filename))
names(seqen)[names(seqen) == "Dishwasher..kW."] <- "appliance"
seqen <- select(seqen, id, appliance)

#see the whole dataset
x <- seqen$appliance
h<-hist(x, breaks=500, col="red", xlab="data",main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

#log histogram
h <- hist(filename$WashingMachine..kW., breaks="Scott", plot=FALSE)
plot(h$mids, h$density, log="y", type='h', xlab= "Wh", ylab = "Density")
```

## Threshold "zero" value  

Method 1: take x% of the total consumption

```{r}
tempseqen <- seqen[order(seqen$appliance),]
sum <- sum(tempseqen$appliance, na.rm=TRUE)
threshold <- 0.70
threshold2 <- 0.50
espilon <- 0.03# threshold+- 0.03
idthreshold <- floor(nrow(tempseqen)*threshold)
tempseqen2 <- tempseqen$appliance[idthreshold:nrow(tempseqen)]
sum2 <- sum(tempseqen2, na.rm=TRUE)

while((sum2/sum)<=(threshold-espilon) || (sum2/sum)>=(threshold+espilon))
{
  if((sum2/sum)<=(threshold-espilon))
  {
    threshold2 <- threshold2*0.5
    print("seuil if")
    print(threshold2)

  }else{
    threshold2 <- threshold2+1.5
    print("seuil else")
    print(threshold2)

  }
  idthreshold <- floor(nrow(tempseqen)*threshold2)
  tempseqen2 <- tempseqen$appliance[idthreshold:nrow(tempseqen)]
  sum2 <- sum(tempseqen2, na.rm=TRUE)
  print("sum")
  print(sum2/sum)
}

valuethreshold <- tempseqen$appliance[idthreshold]
```

Method 2: look at the percentile to manually set the threshold
```{r}
x <-quantile(seqen$appliance, seq(0,1,.001))
plot(x, type = "s", xlab= "Percentile", ylab = "Wh")
valuethreshold <- 0.5
```

## New dataset

The code below shows the dataset after the cleaning:
```{r}
seqen <- filter(seqen, appliance >= valuethreshold)
x <- seqen$appliance
h<-hist(x, breaks=500, col="red", xlab="Wh",  main="Histogram of clean data with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
```

Add the sequence ID and the position to each value (uncomment to merge sequence of the same cycle):
```{r}
seqen <- mutate(seqen, idseq = 0, timestamp = NA)
seqid=1
pos=1
gap=0
for(i in 1:nrow(seqen))
{
  if(i==1)
  {
    seqen$idseq[i]=seqid
    seqen$timestamp[i]=pos
    gap=gap+1
  }
  else if(as.numeric(seqen$id[i])-as.numeric(seqen$id[i-1])==1)
  {
    seqen$idseq[i]=seqid
    pos=pos+1
    seqen$timestamp[i]=pos
    gap=gap+1
  }
  else
  {
    # if(as.numeric(seqen$id[i])-as.numeric(seqen$id[i-1])<floor(sqrt(gap)))#join sequence if small gap
    # {
    #   seqen$idseq[i]=seqid
    #   pos=pos+1
    #   seqen$timestamp[i]=pos
    #   gap=1   
    # }else{
      seqid=seqid+1
      seqen$idseq[i]=seqid
      pos=1
      seqen$timestamp[i]=pos
    # }
  }
}
```

## Slope

Compute the slope between two value of the same sequence:
```{r}
seqen <- mutate(seqen, slope = 0)

for(i in 2:nrow(seqen))
{
  if(as.numeric(seqen$idseq[i])-as.numeric(seqen$idseq[i-1])==0)
  {
    seqen$slope[i]=(as.numeric(seqen$appliance[i])-as.numeric(seqen$appliance[i-1]))/as.numeric(seqen$appliance[i-1])
  }
}

x <- seqen$slope
h<-hist(x, breaks=500, col="red", xlab="data",  main="Histogram of the slope")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2, log="y")

plot(h$mids, h$density, log="y", type='h', xlab= "Wh", ylab = "Density")
```

## Sampling

Create a representative sample of 1000 values (uncomment to normalize the data):
```{r}
#or use DiversitySampler
if(nrow(seqen)>1000)
{
  mysample<-sample_n(seqen, 1000)
}else
{
  mysample <- seqen
}

############################################ NORMALIZE

# minvec <- min(mysample$slope)
# maxvec <- max(mysample$slope)
# 
# normalize <- function(x) {
#   return ((x - minvec) / (maxvec - minvec))
# }
# mydata <- data.frame(normalize(mysample$slope))
# mydata <- data.frame(mysample$slope)
mydata <- mysample$slope
mydata <- as.data.frame(mydata)
```

## Elbow method

The next step is to reduce the noise between data. We choose to double clustering the dataset with a hierarchic clustering to keep extremal values. Then a Kmeans to reduce the silhouette. First at all, we need to find the number of cluster which have a small WSS with the Elbow method:
```{r}
fviz_nbclust(mydata, hcut, k.max = 50, method = "wss") +
  labs(subtitle = "Elbow method")
#Find manually the best value(the elbow)
nk2 <- 4
```

Divisive and agglomerate hierarchical clustering are compared with various metric. We keep the best average Silhouette:
```{r}
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
```

## Add clusters to the whole dataset

The following methods add cluster id to the whole dataset (uncomment to create new cluster if the distance to the cluster is too high):
```{r}
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
x.SS <- aggregate(seqen$slope, by=list(seqen$ncluster), function(x) sum(scale(x, scale=FALSE)^2))
SS <- rowSums(x.SS[, -1]) # Sum of squares for each cluster
xTSS <- sum(x.SS[, -1])  # Total (within) sum of squares
```

## Kmeans to reduce silhouette

Since new data may reduce the average Silhouette, a Kmeans adjusts the cluster to the whole dataset:
```{r}
cl <- kmeans(seqen$slope,nstart=25, max(rang$Group.1))
seqen <- mutate(seqen, nclustkmeans=cl$cluster, meankmeans= NA)
for(i in 1:nrow(seqen))
{
  seqen$meankmeans[i]=cl$centers[seqen$nclustkmeans]
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
```

## Export data
The following code create a SPMF format:
```{r}
pred <- data.frame(Sequence = character(), stringsAsFactors = FALSE)
temp <- seqen$idseq[1]
stringseq <- as.character(seqen$nclustkmeans[1])

for(i in 1:nrow(seqen))
{
  if(temp==seqen$idseq[i])
  {
    stringseq <- paste(stringseq,"-1", as.character(seqen$nclustkmeans[i]))
  }else{
    stringseq <- paste(stringseq,"-1 -2")
  pred <- rbind(pred, stringseq, stringsAsFactors = FALSE)
  temp <- seqen$idseq[i]
  stringseq <- as.character(seqen$nclustkmeans[i])
  }
}

write.table(pred, "output.csv", row.names=FALSE, col.names=FALSE)

```

