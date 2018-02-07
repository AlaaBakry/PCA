# this is written on R version 3.4.3 
#PCA is done without transformation in this script as we need to keep unwanted variation "not sure" to analyze seperately as it stabilizes variance.
# reading the data
setwd("C:/Users/amr01007778867/Desktop")
TregData  <- read.delim("t_reg_only.txt")
head(TregData)
#in PCA We can Obtain the Eigenvectors and Eigenvalues from the covariance matrix (eigenvalue decomposition),or perform SVD like here
princo <- prcomp(TregData, center=TRUE, scale=TRUE)
summary(princo)
# we have 32 components, the first component accounts for 82.54 % of the variation
# to determine which components (have eigenvalues >1 ) to keep calculate eigenvalue 
princo$sdev ^ 2
# only the first 2 have eigenvalues >1 and confirmed from graphs 
screeplot(princo , main="Scree Plot", xlab="Components")
screeplot(princo, main="Scree Plot", type="line" )

# construct the projection matrix W from  the first principle component
FPC <- princo$rotation[,1]


