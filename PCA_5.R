# this is written on R version 3.4.3 
#PCA is done without transformation "not sure" in this script as we need to keep unwanted variation to analyze seperately as it stabilizes variance.
# reading the data
setwd("C:/Users/amr01007778867/Desktop")
TregData  <- read.delim("t_reg_only.txt")
head(TregData )
#normalization 
TregData <- TregData[,-c(1,1)] # the first variable is string
mean <- apply(TregData,2,mean) # data is normalized
st.dev <- apply(TregData,2,sd)
TregData <- scale (TregData,mean,st.dev)
#in PCA We can Obtain the Eigenvectors and Eigenvalues from the covariance matrix (eigenvalue decomposition),or perform SVD like here
princo <- prcomp(TregData[,-1], center=TRUE, scale=TRUE)
summary(princo)
# we have 31 components, the first component accounts for 82.3 % of the variation
# to determine which components (have eigenvalues >1 ) to keep calculate eigenvalue 
princo$sdev ^ 2
# only the first 2 have eigenvalues >1 and confirmed from graphs 
screeplot(princo , main="Scree Plot", xlab="Components")
screeplot(princo, main="Scree Plot", type="line" )

# draw the BiPloT of PCs to assure
biplot(princo)
# construct the projection matrix W from  the first 2 eigenvectors 
W=TregData[,c(1,2)]
rownames(W) <- NULL
plot(W,cex=0.9,col="blue",main="Plot of 1st 2 principle components")


 
