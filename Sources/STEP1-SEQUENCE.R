############################################ SELECT THE SEQUENCE

library("dplyr")
library("ggplot2")



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

#select a threshold here 90% of the sum value to clean the dataset

#FIRST METHOD WITH A % OF THE CONSUMPTION

# tempseqen <- seqen[order(seqen$appliance),]
# sum <- sum(tempseqen$appliance, na.rm=TRUE)
# threshold <- 0.70
# threshold2 <- 0.50
# espilon <- 0.03# threshold+- 0.03
# idthreshold <- floor(nrow(tempseqen)*threshold)
# tempseqen2 <- tempseqen$appliance[idthreshold:nrow(tempseqen)]
# sum2 <- sum(tempseqen2, na.rm=TRUE)
# 
# while((sum2/sum)<=(threshold-espilon) || (sum2/sum)>=(threshold+espilon))
# {
#   if((sum2/sum)<=(threshold-espilon))
#   {
#     threshold2 <- threshold2*0.5
#     print("seuil if")
#     print(threshold2)
# 
#   }else{
#     threshold2 <- threshold2+1.5
#     print("seuil else")
#     print(threshold2)
# 
#   }
#   idthreshold <- floor(nrow(tempseqen)*threshold2)
#   tempseqen2 <- tempseqen$appliance[idthreshold:nrow(tempseqen)]
#   sum2 <- sum(tempseqen2, na.rm=TRUE)
#   print("sum")
#   print(sum2/sum)
# }
# 
# valuethreshold <- tempseqen$appliance[idthreshold]

#SECOND METHOD WITH A MANUAL THRESHOLD VALUE
x <-quantile(seqen$appliance, seq(0,1,.001))
plot(x, type = "s", xlab= "Percentile", ylab = "Wh")
valuethreshold <- 0.5

#AFTER ONE OF THE 2 METHODS
seqen <- filter(seqen, appliance >= valuethreshold)

x <- seqen$appliance
h<-hist(x, breaks=500, col="red", xlab="Wh",  main="Histogram of clean data with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#create sequence ID and add the position of each data in the sequence
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

#create slope

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


############################################ SAMPLING
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
