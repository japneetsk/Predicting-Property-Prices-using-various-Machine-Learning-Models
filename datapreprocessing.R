setwd("Final Project")

library(car)
library(ISLR)
library(ggplot2)
library(MASS)

library(tidyverse)
library(psycho)       # The main goal of the psycho package is to provide tools for psychologists, neuropsychologists and neuroscientists,
                      # to facilitate and speed up the time spent on data analysis. It aims at supporting best practices and tools to format the output
                      # of statistical methods to directly paste them into a manuscript, ensuring statistical reporting standardization and conformity.


library(rpart)        # Recursive Partitioning and Regression Trees
library(rattle)       # Rattle (Williams, 2011) is a package written in R providing a graphical user interface to very many other R packages
                      # that provide functionality for data mining.
library(rpart.plot)   # Plot an rpart model. A simplified interface to the prp function.
library(RColorBrewer) # Creates nice looking color palettes especially for thematic maps

library(randomForest) # Classification and Regression with Random Forest
library(caret)        # The caret package (short for Classification And REgression Training) contains functions to streamline the model training process 
                      # for complex regression and classification problems. 

raw<- read.csv("train.csv")
#raw<-raw[c(524,1299)]# removing the lines with outlier values

fact<-raw[,sapply(raw,is.factor)] #factor variables

# adding variables that are infact factor variables in the dataset
fact$MSSubClass<-as.factor(raw$MSSubClass)
fact$MoSold<-as.factor(raw$MoSold)

#the remaining numeric variables, for some reason it was still picking up MSSubClass, hence the -2
# changed below code to get only the different columns based on names
num<-setdiff(names(raw),names(fact))#[,-2]

# make num into a dataset. It should have only numeric columns. ID should be removed later.
num <- raw[,num]
  
#counting the NA values in each col
check<-colSums(is.na(fact)>0)
navalues<-data.frame(check)

navalues # looking at this we can easily see that there are 4 variables that have a very high count of na values.

#removing these variables from the dataset as they will not add much value
fact<-fact[,!(colnames(fact)%in% c("Alley","PoolQC","Fence","MiscFeature"))]

#because NA stands for not applicable and not missing value in this data set, NA is added as an additional level
for (i in 1:ncol(fact)) {fact[,i]<-addNA(fact[,i])}

#extracting the ordinal variables
ordi<-(fact[,(colnames(fact)%in% c("ExterQual","ExterCond","BsmtQual","BsmtCond","BsmtExposure","HeatingQC","KitchenQual","FireplaceQu","GarageQual","GarageCond"))])
ordi[]<-lapply(ordi,as.character)

#changing these variables to numeric
for (j in 1:10) {ordi[,j]<-ifelse(ordi[,j]=="Ex",5,ifelse(ordi[,j]=="Gd",4,ifelse(ordi[,j]=="TA",3,ifelse(ordi[,j]=="Fa",2,1))))}
ordi[is.na(ordi)]<-0

#removing the ordinal variables from the categorical variable dataset
fact<-fact[setdiff(names(fact),names(ordi))]

#using the year sold data to calculate age of the the property and other aspects
num$propertyage<-num$YrSold-num$YearBuilt
num$remodage<-num$YrSold-num$YearRemodAdd
num$garageage<-num$YrSold-num$GarageYrBlt

#removing the year data, as this is no longer required
num<-num[,!colnames(num)%in%c("YrSold","YearBuilt","YearRemodAdd","GarageYrBlt")]

#replacing NA with 0, again becuase NA does not stand for not available, but not applicable
num[is.na(num)]<-0

#combining the numeric and recoded ordinal data
num_or<-cbind(num,ordi)

#standardizing the numeric variables so that they variables with bigger numbers do not overshadow smaller ones in the model
z_numeric<-num_or[,-32]%>%psycho::standardize(normalize = TRUE)

#=============================Linear Regression==========================#

lmdata<-cbind(SalePrice=raw$SalePrice,z_numeric,fact)[,-2]

Model2 <- lm(SalePrice~ ., data = lmdata)

#backward selection

backward2<-step(Model2, direction='backward');

summary(backward2)
BIC(backward2)

plot(Model2)

#=================Decision Tree (Entire dataset)=========================#
#combining the numerical and categorical dataset
treedata<-cbind(SalePrice=raw$SalePrice,z_numeric,fact)[,-2]

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(treedata), size = floor(.70*nrow(treedata)), replace = F)
trainingtd<-treedata[sample,]
testingtd<-treedata[-sample,-1]
testingSP<-treedata[-sample,1]#SalePrice of test data to evalutae model accuracy

#fitting a tree
tree_iowa<-rpart(SalePrice~.,trainingtd)
summary(tree_iowa)
fancyRpartPlot(tree_iowa)

#pruning the tree, 
#cp is kept at 0 so that it can be plotted to see what the optimal value
pruned<-prune(tree_iowa,cp=0.1)
plotcp(pruned)
summary(pruned)
fancyRpartPlot(pruned)

#assessing the model performance
treepred<-predict(pruned,newdata=testingtd)
MSE1<-mean((treepred-testingSP)^2)
RMSE1<-(MSE1)^0.5

#=================Decision Tree (without neighborhood)=========================#
#combining the numerical and categorical dataset
treedata1<-cbind(SalePrice=raw$SalePrice,z_numeric,fact)[,c(-2,-53)]

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(treedata), size = floor(.70*nrow(treedata)), replace = F)
trainingtd1<-treedata1[sample,]
testingtd1<-treedata1[-sample,-1]
testingSP1<-treedata1[-sample,1]#SalePrice of test data to evalutae model accuracy

#fitting a tree
tree_iowa1<-rpart(SalePrice~.,trainingtd1)
summary(tree_iowa1)
fancyRpartPlot(tree_iowa1)

#pruning the tree, 
#cp is kept at 0 so that it can be plotted to see what the optimal value
pruned1<-prune(tree_iowa1,cp=0)
plotcp(pruned1)
summary(pruned)
fancyRpartPlot(pruned1)

#assessing the model performance
treepred1<-predict(pruned1,newdata=testingtd1)
MSE5<-mean((treepred1-testingSP1)^2)
RMSE5<-(MSE5)^0.5
#=================Random Forest(Entire Dataset)=========================#

#aparrently the randomforest does not accept NA as level, creating problems during predict
#therefore it needs to be recoded
fd<-treedata
fd_factor<-fd[,sapply(fd,is.factor)]
for (a in 1:31) {levels(fd_factor[,a])[is.na(levels(fd_factor[,a]))]<-"Not App"}

#adding the changed data back to the full dataset
forestdata<-cbind(fd[,1:44],fd_factor)

#splitting the data, the same set is used for consistency and comparability between models
set.seed(12345)
sample1 <- sample.int(n = nrow(forestdata), size = floor(.70*nrow(forestdata)), replace = F)
trainingfd<-forestdata[sample1,]
testingfd<-forestdata[-sample1,-1]
forestdata<-cbind(fd[,1:44],fd_factor)

#splitting the data, the same set is used for consistency and comparability between models
set.seed(12345)
sample1 <- sample.int(n = nrow(forestdata), size = floor(.70*nrow(forestdata)), replace = F)
trainingfd<-forestdata[sample1,]
testingfd<-forestdata[-sample1,-1]
testingfSP<-forestdata[-sample1,1]#SalePrice of test data to evalutae model accuracy

#building the model
forest_iowa=randomForest(SalePrice~.,data=trainingfd,mtry=9,ntree=1000,importance=TRUE)

#assessing the important variables
impvar<-data.frame(importance(forest_iowa))
impvar<-impvar[order(-impvar$X.IncMSE),]

#assessing the model
forestpred<-predict(forest_iowa,newdata=testingfd)
MSE2<-mean((forestpred-testingfSP)^2)
RMSE2<-(MSE2)^0.5
#==============RandomForest for categorical=============#
#using just the categorical data
catforest<-cbind(fd_factor,SalePrice=raw$SalePrice)

#splitting the data, the same set is used for consistency and comparability between models
set.seed(12345)
sample2 <- sample.int(n = nrow(catforest), size = floor(.70*nrow(catforest)), replace = F)
trainingcfd<-catforest[sample2,]
testingcfd<-catforest[-sample2,-32]
testingcfSP<-catforest[-sample2,32]#SalePrice of test data to evalutae model accuracy

#building the model

#tree to see number of variables
tree_cat<-rpart(SalePrice~.,trainingcfd)
pruned1<-prune(tree_cat,cp=-1)
summary(pruned1)

#forest
forest_cat=randomForest(SalePrice~.,data=trainingcfd,mtry=14,ntree=5000,importance=TRUE)

#assessing the important variables
impcat<-data.frame(importance(forest_cat))
impcat<-impcat[order(-impcat$X.IncMSE),]
impcat<-cbind(variables=rownames(impcat),data.frame(impcat,row.names=NULL))
d <- cbind(rownames(d), data.frame(d, row.names=NULL))

#assessing the model
catforestpred<-predict(forest_cat,newdata=testingcfd)
MSE3<-mean((catforestpred-testingcfSP)^2)
RMSE3<-(MSE3)^0.5
#==============Principal Component======================#
#conducting principal component analysis on the numeric variables
PCA<-prcomp(z_numeric[,c(-1,-32)],scale=TRUE)

#computing standard deviation of each principal component
std_dev <- PCA$sdev

#computing proportion of variance
PCAvar <- std_dev^2
var_prp <- PCAvar/sum(PCAvar)

#Cumulative Scree Plot
x = 1:length(var_prp)
qplot(x,cumsum(var_prp), xlab="Principal Component",
      ylab="Cumulative Proportion of Variance Explained",
      main="  ",ylim=c(0,1))+
  geom_line()+geom_point(shape=21,fill="red",cex=3)
#==================Linear Regression============================#
#since the screeplot does not have a very obvious elbow, 
#and wanting to retain atleast 90% of the information on the variance we keep 30 PCA to build the model
PCAvalues<-data.frame(PCA$x)
PCAvalues<-PCAvalues[1:30]

#using the important categorical date from the forest model
categorical<-fd_factor[,c("Neighborhood","MSSubClass","GarageFinish","GarageType")]

#compiling the data to use for regression
regdata<-cbind(PCAvalues,categorical,SalePrice=raw$SalePrice)

#splitting the data, the same set is used for consistency and comparability between models
set.seed(12345)
sample3 <- sample.int(n = nrow(regdata), size = floor(.70*nrow(regdata)), replace = F)
trainingrd<-regdata[sample3,]
testingrd<-regdata[-sample3,-35]
testingrSP<-regdata[-sample3,35]#SalePrice of test data to evalutae model accuracy

#regression model
model<-lm(SalePrice~.,data=trainingrd)
backward<-step(model, direction='backward')
summary(backward)

#assessing the model
regpred<-predict(backward,newdata=testingrd)
MSE4<-mean((regpred-testingrSP)^2)
RMSE4<-(MSE4)^0.5

#=====================Clustering (for neighbourhoods)=============================#
#Using clustering to understand what is happening with the neighborhood variable
clustdata<-forestdata[,c(-1,-52)]
##Heirarchical Clustering

# Dissimilarity matrix
d <- dist(clustdata, method = "euclidean")

# Plotting Dendogram 
hc1 <- hclust(d, method = "complete" )
plot(hc1, main="Complete Linkage", cex = 0.6, hang = -1)

#Cut the dendogram tree in 5 groups based on the dendogram
grp <- cutree(hc1, k = 5)

# Number of members in each cluster
table(grp)
clustdata1<-mutate(clustdata,cluster=grp)

#combining the neighborhood data to see  if the clusters 
#will be able to reveal information about the neighborhoods
write_csv(ngbrhd,path = "n.csv")
