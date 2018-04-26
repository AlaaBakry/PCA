# this is written on R version 3.4.3 
#PCA is done without transformation in this script as we need to keep unwanted variation "not sure" to analyze seperately as it stabilizes variance.
#install.packages("ggpubr")
#install.packages("readxl")
#install.packages("factoextra")
# reading the data
library(readxl)
setwd("C:/Users/Admin/Desktop")
TregData  <- read.delim("t_reg_only.txt")
TregData <- t(TregData)
#View(TregData)
metadata<- read_excel(path= "metadata_treg.xlsx", sheet = 1)
batchnum <- metadata[,4] #is x

#in PCA We can Obtain the Eigenvectors and Eigenvalues from the covariance matrix (eigenvalue decomposition),or perform SVD like here
princo <- prcomp(TregData, center=TRUE, scale=FALSE)
library(factoextra)
fviz_eig(princo)
summary(princo)
fviz_pca_ind(princo,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping
            

eig.val <- get_eigenvalue(princo)

fviz_pca_var(princo,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )    # Avoid text overlapping
fviz_pca_biplot(princo, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" ) # Individuals color

# we have 32 components, the first component accounts for 82.54 % (in case of scaling 83.07: without scaling)of the variation
# to determine which components (have eigenvalues >1 ) to keep calculate eigenvalue 
princo$sdev ^ 2
# only the first 2 have eigenvalues >1 and confirmed from graphs 
screeplot(princo , main="Scree Plot", xlab="Components")
#screeplot(princo, main="Scree Plot", type="line" )
boxplot(princo$x)
i=15
y<- princo$x[,i] # the columns of rotation are the eigenvectors
#View(y)
#trans<- t(princo$rotation) 
#FPCT <- trans[,1]; batchnum ["PC"] <- FPCT; no need to get the first PC till assuring of the highest correlation 
# test the correlation between batchnumber and PCs of data to know
x1<- c(5,5,5,5,5,5,15,15) ;
x2<- c(17,17,17,20,21,24,24,24)
x3<- c(25,25,32,32,32,32,32,32)
x4 <-c(47,47,48,48,48,55,55,55)
x<- c(x1, x2, x3, x4)
#cor(x,t(y), method = c(  "kendall"))

anov1 = aov(x ~ y)
summary(anov1)
#pvalue = 0.638 so there is a correlation
#      
PCs_minusfirst2<-princo$x[,-c(1,2)]
View(PCs_minusfirst2)
i=1
y<- PCs_minusfirst2[,i] # the columns of rotation are the eigenvectors
View(y)
anov2 = aov(x ~ y)
summary(anov2)
#pvalue =0.357
boxplot(PCs_minusfirst2)
PCs_minus_fir_2and4_5<-PCs_minusfirst2[,-c(2,3)]
boxplot(PCs_minus_fir_2and4_5)
i=1
y<-PCs_minus_fir_2and4_5 [,i] # the columns of rotation are the eigenvectors
View(y)
anov2 = aov(x ~ y)
summary(anov2)
View(princo$x)
# PCs_minus_1_2and4_5_6_7<- PCs_minus_fir_2and4_5[,-c(1,2)]
# boxplot(PCs_minus_1_2and4_5_6_7)
