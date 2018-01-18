# This code is edited from  Steve Pittard code available at https://github.com/steviep42/youtube/blob/master/YOUTUBE.DIR/BB_phys_stats_ex1.R 
library(calibrate)
setwd("C:/Users/amr01007778867/Desktop")
my.classes <- read.delim("t_reg_only.txt")
head(my.classes)

my.classes2=my.classes[,c(1,2)]
rownames(my.classes2) <- NULL
my.classes=my.classes2
plot(my.classes,cex=0.9,col="blue",main="Plot of 1st 2 samples")

# Scale the data
standardize <- function(x) {(x - mean(x))}
my.scaled.classes <- apply(my.classes,2,function(x) (x-mean(x)))
head(my.scaled.classes)
plot(my.scaled.classes,cex=0.9,col="blue",main="Plot of 1st 2 samples",sub="Mean Scaled")

# Find Eigen values of covariance matrix

my.cov <- cov(my.scaled.classes)
my.eigen <- eigen(my.cov)
rownames(my.eigen$vectors) <- c("1st sample","2nd sample")
colnames(my.eigen$vectors) <- c("PC1","PC")
# Note that the sum of the eigen values equals the total variance of the data

sum(my.eigen$values)
var(my.scaled.classes[,1]) + var(my.scaled.classes[,2])

# The Eigen vectors are the principal components. We see to what extent each variable contributes

loadings <- my.eigen$vectors

# Let's plot them 

pc1.slope <- my.eigen$vectors[1,1]/my.eigen$vectors[2,1]
pc2.slope <- my.eigen$vectors[1,2]/my.eigen$vectors[2,2]

abline(0,pc1.slope,col="red")
abline(0,pc2.slope,col="green")

#textxy(12,10,"(-0.710,-0.695)",cx=0.9,dcol="red")
#textxy(-12,10,"(0.695,-0.719)",cx=0.9,dcol="green")

# See how much variation each eigenvector accounts for

pc1.var <- 100*round(my.eigen$values[1]/sum(my.eigen$values),digits=2)
pc2.var <- 100*round(my.eigen$values[2]/sum(my.eigen$values),digits=2)
xlab=paste("PC1 - ",pc1.var," % of variation",sep="")
ylab=paste("PC2 - ",pc2.var," % of variation",sep="")

# Multiply the scaled data by the eigen vectors (principal components)

scores <- my.scaled.classes %*% loadings
sd <- sqrt(my.eigen$values)
rownames(loadings) = colnames(my.classes)

plot(scores,main="Data in terms of EigenVectors / PCs",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

# Correlation BiPlot

scores.min <- min(scores[,1:2])
scores.max <- max(scores[,1:2])

plot(scores[,1]/sd[1],scores[,2]/sd[2],main="My First BiPlot",xlab=xlab,ylab=ylab,type="n")
rownames(scores)=seq(1:nrow(scores))
abline(0,0,col="red")
abline(0,90.0,col="green")

# This is to make the size of the lines more apparent
factor <- 5

# First plot the variables as vectors
arrows(0,0,loadings[,1]*sd[1]/factor,loadings[,2]*sd[2]/factor,length=0.1, lwd=2,angle=20, col="red")
text(loadings[,1]*sd[1]/factor*1.2,loadings[,2]*sd[2]/factor*1.2,rownames(loadings), col="red", cex=1.2)

# Second plot the scores as points
text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),col="blue", cex=0.7)

somelabs <- paste(round(my.classes[,1],digits=1),round(my.classes[,2],digits=1),sep=" , ")
#identify(scores[,1]/sd[1],scores[,2]/sd[2],labels=somelabs,cex=0.8)
