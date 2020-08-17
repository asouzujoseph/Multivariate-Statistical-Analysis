library(factoextra); 
library (GGally); 
library(tidyverse); 
library (MASS); 
library(MVN)

### read data
soil_data = read.csv ("soilData.csv", header = TRUE) 	# set working directory to folder with data set
soil = soil_data [ ,4:17] 	# select the numeric columns or variables
rownames(soil) = soil_data [, 3] 	# set row names to Soil IDs
plot(soil) # graphically explore data set and check for outliers / correlation
soil = soil [-9,] 	# remove outlier

### Principal component analysis
ggcorr(soil, label = TRUE) 	# graphical Pearson correlation result
pca = princomp(soil, cor = TRUE) 	# principal component function
screeplot(pca, type = "lines") 	# pick 4 principal components
pca$loadings 	# the contribution of variables to each principal component
summary(pca) 	# the first four PCs explains ~ 80% of the variance
pca$sdev 	# standard deviation of PCs
pca.var = pca$sdev^2 	# eigenvalues of the principal components
pca$scores # values of the principal components / new variables
fviz_pca_biplot (pca) 	# biplot of PCA
fviz_pca_var (pca) 	# graph of variables
PCs = as.data.frame(pca$scores[,1:4]) 	# a new data frame of the scores of the principal components

### k means clustering
my_data = scale(PCs [,1:4]) 	# scales data to eliminate effect of variance
fviz_nbclust(my_data, kmeans, method = "wss") 	# compute total within-cluster sum of square
groups = kmeans(PCs, centers = 4, nstart = 25) 	# group into clusters
fviz_cluster(groups, data = PCs) 	# cluster plot

### Linear discriminant analysis
mvn(my_data,mvnTest = "mardia") 	# test of multivariate normality
PCs$Clusters = as.factor(groups$cluster) 	# cluster results to PCs data frame
soil.lda=lda(Clusters~.,PCs) 	# fits an LDA model
soil.lda 	# shows details of LDA model
plot(soil.lda, col = as.integer(PCs$Clusters)) 	# plot of LDA model
soil.pred=predict(soil.lda) 	# predict cluster of observations using LDA model
soil.pred 	# shows posterior probability of observations
hist(soil.pred$x) 	# histogram of projections
table(PCs$Clusters,soil.pred$class,dnn=c("From","Classified into")) 	# accuracy table
soil.ldacv = lda(Clusters~.,PCs, CV=TRUE) 	# evaluation of LDA model with cross-validation
table(PCs$Clusters,soil.ldacv$class,dnn=c("From","Classified into")) 	# accuracy of LDA