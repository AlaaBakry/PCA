# this is written on R version 3.4.3 
#PCA is done without transformation in this script as we need to keep unwanted variation "not sure" to analyze seperately as it stabilizes variance.
#install.packages("ggpubr")
#install.packages("readxl")
# reading the data
library(readxl)
setwd("C:/Users/Admin/Desktop")
TregData  <- read.delim("t_reg_only.txt")
head(TregData)
metadata<- read_excel(path= "metadata_treg.xlsx", sheet = 1)
batchnum <- metadata[,4]
#in PCA We can Obtain the Eigenvectors and Eigenvalues from the covariance matrix (eigenvalue decomposition),or perform SVD like here
princo <- prcomp(TregData, center=TRUE, scale=FALSE)
summary(princo)
# we have 32 components, the first component accounts for 82.54 % of the variation
# to determine which components (have eigenvalues >1 ) to keep calculate eigenvalue 
princo$sdev ^ 2
# only the first 2 have eigenvalues >1 and confirmed from graphs 
screeplot(princo , main="Scree Plot", xlab="Components")
screeplot(princo, main="Scree Plot", type="line" )
boxplot(princo$x)
y<- princo$rotation
trans<- t(princo$rotation) #not sure
#now the eigenvectors are the rows of trans as they were columns of rotation
#FPCT <- trans[,1]; batchnum ["PC"] <- FPCT; no need to get the first PC till assuring of the highest correlation 
# test the correlation between batchnumber and PCs of data to know
x<- batchnum[1];#repeated 
cor(x, trans, method = c("pearson", "kendall", "spearman"))
#E:the highest correlation between sample GSM998923.CEL and batch number 25 / if not transposed Pc # 7 has the highest correlation with batch #      
