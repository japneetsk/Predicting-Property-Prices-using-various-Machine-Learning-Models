library(car)
library(ISLR)
library(ggplot2)
library(MASS)

library(tidyverse)
library(psycho)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

library(randomForest)
library(caret)

library(sqldf)

#Please Change below to your working directory or location the data is saved in.

raw<- read.csv("train.csv")

# Below scatterplot identifies outliers in the bottom right side of the plot
scat <- ggplot(raw,aes(x=GrLivArea,y=SalePrice))+geom_point(color='blue')+labs(title='Correlation Plot for Ames, Iowa Housing Data - Above Grade Living Area and Sales Price', subtitle='Outliers Identified in Bottom Right')+xlab(label='Above Grade Living Area in Square Feet')+ylab(label='House Sales Price in $')
scat

#identify these points
outlierobservations<-raw[raw$GrLivArea>4500,1]
outlierobservations
#Data observations 524 and 1299 are outliers.

raw<-raw[-c(524,1299),]#removing the identified outliers

fact<-raw[,sapply(raw,is.factor)] #factor variables

# adding variables that are infact factor variables in the dataset
fact$MSSubClass<-as.factor(raw$MSSubClass)
fact$MoSold<-as.factor(raw$MoSold)

#the remaining numeric variables
numerical<-setdiff(names(raw),names(fact))
num<-raw[numerical]

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

#changing the other ordinal variables to numeric
fact$LotShape<-lapply(fact$LotShape,as.character)
fact$LotShape<- ifelse(fact$LotShape=="Reg",4,ifelse(fact$LotShape=="IR1",3,ifelse(fact$LotShape=="IR2",2,1)))
fact$Utilities<-lapply(fact$Utilities,as.character)
fact$Utilities<- ifelse(fact$Utilities=="AllPub",4,ifelse(fact$Utilities=="NoSewr",3,ifelse(fact$Utilities=="NoSeWa",2,1)))
fact$LandSlope<-lapply(fact$LandSlope,as.character)
fact$LandSlope<- ifelse(fact$LandSlope=="Sev",3,ifelse(fact$LandSlope=="Mod",2,1))
fact$Electrical<-lapply(fact$Electrical,as.character)
fact$Electrical<- ifelse(fact$Electrical=="SBrkr",5,ifelse(fact$Electrical=="FuseA",4,ifelse(fact$Electrical=="FuseF",3,ifelse(fact$Electrical=="FuseP",2,1))))
fact$Functional<-lapply(fact$Functional,as.character)
fact$Functional<- ifelse(fact$Functional=="Typ",8,ifelse(fact$Functional=="Min1",7,ifelse(fact$Functional=="Min2",6,ifelse(fact$Functional=="Mod",5,ifelse(fact$Functional=="Maj1",4,ifelse(fact$Functional=="Maj2",3,ifelse(fact$Functional=="Sev",2,1)))))))
fact$GarageFinish<-lapply(fact$GarageFinish,as.character)
fact$GarageFinish<- ifelse(fact$GarageFinish=="Fin",3,ifelse(fact$GarageFinish=="RFn",2,1))
fact$GarageFinish[is.na(fact$GarageFinish)]<-0
fact$PavedDrive<-lapply(fact$PavedDrive,as.character)
fact$PavedDrive<- ifelse(fact$PavedDrive=="Y",3,ifelse(fact$PavedDrive=="P",2,1))
fact$BsmtFinType1<-lapply(fact$BsmtFinType1,as.character)
fact$BsmtFinType1<- ifelse(fact$BsmtFinType1=="GLQ",6,ifelse(fact$BsmtFinType1=="ALQ",5,ifelse(fact$BsmtFinType1=="BLQ",4,ifelse(fact$BsmtFinType1=="Rec",3,ifelse(fact$BsmtFinType1=="LwQ",2,1)))))
fact$BsmtFinType1[is.na(fact$BsmtFinType1)]<-0
fact$BsmtFinType2<-lapply(fact$BsmtFinType2,as.character)
fact$BsmtFinType2<- ifelse(fact$BsmtFinType2=="GLQ",6,ifelse(fact$BsmtFinType2=="ALQ",5,ifelse(fact$BsmtFinType2=="BLQ",4,ifelse(fact$BsmtFinType2=="Rec",3,ifelse(fact$BsmtFinType2=="LwQ",2,1)))))
fact$BsmtFinType2[is.na(fact$BsmtFinType2)]<-0

#combining all ordinal variables converted into numeric to ordi
ordi <-cbind(ordi,
             'LotShape'=fact$LotShape,
             'Utilities'=fact$Utilities,
             'LandSlope'=fact$LandSlope,
             'Electrical'=fact$Electrical,
             'Functional'=fact$Functional,
             'GarageFinish'=fact$GarageFinish,
             'PavedDrive'=fact$PavedDrive,
             'BsmtFinType1'=fact$BsmtFinType1,
             'BsmtFinType2'=fact$BsmtFinType2)
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
sum(is.na(num_or))
#standardizing the numeric variables so that they variables with bigger numbers do not overshadow smaller ones in the model.
#Not including column 32 SalePrice in below data frame
z_numeric<-num_or[,-32]%>%psycho::standardize()
sum(is.na(z_numeric))


#=================Decision Tree (Entire dataset)=========================#

#combining the numerical and categorical dataset
treedata1<-cbind(SalePrice=raw$SalePrice,z_numeric,fact)
treedata1<-treedata1[colnames(treedata1)!="Id"]#removing the ID variable

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(treedata1), size = floor(.70*nrow(treedata1)), replace = F)
trainingtd1<-treedata1[sample,]
testingtd1<-treedata1[-sample,-1]
testingSP1<-treedata1[-sample,1]#SalePrice of test data to evaluate model accuracy

#fitting a tree
tree1<-rpart(SalePrice~.,trainingtd1)
summary(tree1)
fancyRpartPlot(tree1,main='Sales Price Prediction for Houses in Ames, Iowa',sub='RPart Decision Tree Using All Variables')

#pruning the tree, 
#cp is kept at 0 so that it can be plotted to see what the optimal value
pruned1<-prune(tree1,cp=0)
plotcp(pruned1,sub='Complexity Parameter (CP) Plot')
summary(pruned1)

#from the plot it seems that a cp=0.018 would be a good tradeoff between size and error
pruned2<-prune(tree1,cp=0.018)
summary(pruned2)
fancyRpartPlot(pruned2,main='Sales Price Prediction for Houses in Ames, Iowa',sub='Pruned RPart Decision Tree')

#assessing the model performance
#for tree with 10 splits using all the data
treepred1<-predict(tree1,newdata=testingtd1)
MSEtree1<-mean((treepred1-testingSP1)^2)
RMSEtree1<-(MSEtree1)^0.5
RMSEtree1

#for tree with 6 splits using all the data
prunedtreepred1<-predict(pruned2,newdata=testingtd1)
MSEprunedtree1<-mean((prunedtreepred1-testingSP1)^2)
RMSEprunedtree1<-(MSEprunedtree1)^0.5
RMSEprunedtree1

#=================Decision Tree (without neighborhood)=========================#

#removing neighborhood data
treedata2<-treedata1[colnames(treedata1)!="Neighborhood"]

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(treedata2), size = floor(.70*nrow(treedata2)), replace = F)
trainingtd2<-treedata2[sample,]
testingtd2<-treedata2[-sample,-1]
testingSP2<-treedata2[-sample,1]#SalePrice of test data to evaluate model accuracy

#fitting a tree
tree2<-rpart(SalePrice~.,trainingtd2)
summary(tree2)
fancyRpartPlot(tree2,main='Sales Price Prediction for Houses in Ames, Iowa',sub='RPart Decision Tree Using All Variables Except Neighborhood')

#assessing the model performance
treepred2<-predict(tree2,newdata=testingtd2)
MSEtree2<-mean((treepred2-testingSP2)^2)
RMSEtree2<-(MSEtree2)^0.5
RMSEtree2

#=================Random Forest(w/o neighborhood)=======================#

#aparrently the randomforest does not accept NA as level, creating problems during predict
#therefore it needs to be recoded
fd<-treedata2
fd_factor<-fd[,sapply(fd,is.factor)]
for (a in 1:ncol(fd_factor)) {levels(fd_factor[,a])[is.na(levels(fd_factor[,a]))]<-"Not App"}

#adding the changed data back to the full dataset
forestdata<-cbind(fd[,1:53],fd_factor)

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(forestdata), size = floor(.70*nrow(forestdata)), replace = F)
trainingfd<-forestdata[sample,]
testingfd<-forestdata[-sample,-1]
testingfSP<-forestdata[-sample,1]#SalePrice of test data to evaluate model accuracy

#building the model and testing performance
forest1=randomForest(SalePrice~.,data=trainingfd,mtry=6,ntree=500,importance=TRUE)
forestpred1<-predict(forest1,newdata=testingfd)
MSEforest1<-mean((forestpred1-testingfSP)^2)
RMSEforest1<-(MSEforest1)^0.5
RMSEforest1

forest2=randomForest(SalePrice~.,data=trainingfd,mtry=9,ntree=1000,importance=TRUE)
forestpred2<-predict(forest2,newdata=testingfd)
MSEforest2<-mean((forestpred2-testingfSP)^2)
RMSEforest2<-(MSEforest2)^0.5
RMSEforest2

forest3=randomForest(SalePrice~.,data=trainingfd,mtry=12,ntree=2000,importance=TRUE)
forestpred3<-predict(forest3,newdata=testingfd)
MSEforest3<-mean((forestpred3-testingfSP)^2)
RMSEforest3<-(MSEforest3)^0.5
RMSEforest3

forest4=randomForest(SalePrice~.,data=trainingfd,mtry=16,ntree=3000,importance=TRUE)
forestpred4<-predict(forest4,newdata=testingfd)
MSEforest4<-mean((forestpred4-testingfSP)^2)
RMSEforest4<-(MSEforest4)^0.5
RMSEforest4

forest5=randomForest(SalePrice~.,data=trainingfd,mtry=15,ntree=2000,importance=TRUE)
forestpred5<-predict(forest5,newdata=testingfd)
MSEforest5<-mean((forestpred5-testingfSP)^2)
RMSEforest5<-(MSEforest5)^0.5
RMSEforest5

forest6=randomForest(SalePrice~.,data=trainingfd,mtry=10,ntree=1000,importance=TRUE)
forestpred6<-predict(forest6,newdata=testingfd)
MSEforest6<-mean((forestpred6-testingfSP)^2)
RMSEforest6<-(MSEforest6)^0.5
RMSEforest6

forest7=randomForest(SalePrice~.,data=trainingfd,mtry=12,ntree=500,importance=TRUE)
forestpred7<-predict(forest7,newdata=testingfd)
MSEforest7<-mean((forestpred7-testingfSP)^2)
RMSEforest7<-(MSEforest7)^0.5
RMSEforest7

forest8=randomForest(SalePrice~.,data=trainingfd,mtry=15,ntree=500,importance=TRUE)
forestpred8<-predict(forest8,newdata=testingfd)
MSEforest8<-mean((forestpred8-testingfSP)^2)
RMSEforest8<-(MSEforest8)^0.5
RMSEforest8

#============================PCA========================================#

#conducting principal component analysis on the numeric variables
PCA<-prcomp(z_numeric[colnames(z_numeric)!="Id"],scale=TRUE)

#look at PC1 and PC2 to understand which are the most important variables. 
PCA$rotation[,1:2]

#computing standard deviation of each principal component
std_dev <- PCA$sdev

#computing proportion of variance
PCAvar <- std_dev^2
var_prp <- PCAvar/sum(PCAvar)

#Cumulative Scree Plot
x = 1:length(var_prp)
qplot(x,cumsum(var_prp), xlab="Principal Component",
      ylab="Cumulative Proportion of Variance Explained",
      main="Scree Plot for Principal Component Analysis",ylim=c(0,1))+
  geom_line()+geom_point(shape=21,fill="red",cex=3)+
  theme(plot.title = element_text(hjust = 0.5))

#==================Linear Regression (only PCA)============================#

#since the screeplot does not have a very obvious elbow, 
#and wanting to retain atleast 80% of the information on the variance we keep 23 PCs to build the model
PCAvalues<-data.frame(PCA$x)
PCAvalues<-PCAvalues[1:23]

#compiling the data to use for regression
regdata<-cbind(PCAvalues,SalePrice=raw$SalePrice)

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(regdata), size = floor(.70*nrow(regdata)), replace = F)
trainingrd<-regdata[sample,]
testingrd<-regdata[-sample,-24]
testingrSP<-regdata[-sample,24]#SalePrice of test data to evaluate model accuracy

#regression model using cross validation and backward selection
train.control<-trainControl(method = "cv", number = 10)
regmodel1<-train(SalePrice~.,data = trainingrd,method='leapBackward', trControl=train.control)


#assessing the model
regpred1<-predict(regmodel1,newdata=testingrd)
MSEreg1<-mean((regpred1-testingrSP)^2)
RMSEreg1<-(MSEreg1)^0.5
RMSEreg1

#diagnostic plots
PCAreg <-lm(SalePrice~.,data=trainingrd)
plot(PCAreg,las=1)

#==================Adding categorical variables to regression===========#

#randomforest to identify the best categorical variables
catforest<-cbind(fd_factor,SalePrice=raw$SalePrice)

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(catforest), size = floor(.70*nrow(catforest)), replace = F)
trainingfd2<-catforest[sample,]
testingfd2<-catforest[-sample,-22]
testingfSP2<-catforest[-sample,22]#SalePrice of test data to evalutae model accuracy

#building the model and testing performance
forest9=randomForest(SalePrice~.,data=trainingfd2,mtry=10,ntree=500,importance=TRUE)
forestpred9<-predict(forest9,newdata=testingfd2)
MSEforest9<-mean((forestpred9-testingfSP2)^2)
RMSEforest9<-(MSEforest9)^0.5
RMSEforest9

#Identifying the top 10 variables
impvar<-data.frame(importance(forest9))
impvar<-impvar[order(-impvar$X.IncMSE),]
impvar

#compiling the data to use for regression
regdata2<-cbind(PCAvalues,SalePrice=raw$SalePrice,
                MSSubClass=fd_factor$MSSubClass,
                GarageType=fd_factor$GarageType,
                Foundation=fd_factor$Foundation,
                MasVnrType=fd_factor$MasVnrType,
                MSZoning=fd_factor$MSZoning,
                RoofStyle=fd_factor$RoofStyle,
                SaleType=fd_factor$SaleType,
                BldgType=fd_factor$BldgType,
                HouseStyle=fd_factor$HouseStyle,
                Exterior2nd=fd_factor$Exterior2nd)

#splitting the data
set.seed(12345)
sample <- sample.int(n = nrow(regdata2), size = floor(.70*nrow(regdata2)), replace = F)
trainingrd2<-regdata2[sample,]
testingrd2<-regdata2[-sample,-24]
testingrSP2<-regdata2[-sample,24]#SalePrice of test data to evaluate model accuracy

#regression model using cross validation and backward selection
train.control<-trainControl(method = "cv", number = 10)
regmodel2<-train(SalePrice~.,data = trainingrd2,method='leapBackward', trControl=train.control)

#assessing the model
regpred2<-predict(regmodel2,newdata=testingrd2)
MSEreg2<-mean((regpred2-testingrSP2)^2)
RMSEreg2<-(MSEreg2)^0.5
RMSEreg2

#=====================Clustering (for neighbourhoods)=============================#
#Using clustering to understand what is happening with the neighborhood variable
clustdata<-forestdata[,-1]
##Heirarchical Clustering

# Dissimilarity matrix
HousingData <- dist(clustdata, method = "euclidean")

# Plotting Dendogram 
hc1 <- hclust(HousingData, method = "complete" )
plot(hc1, main="Dendrogram for Ames, Iowa Housing Data Clusters", cex = 0.6,hang=-1,labels = FALSE, xlab = NULL)
rect.hclust(hc1, k = 5, border = 1:5)

#Cut the dendogram tree in 5 groups based on the dendogram
grp <- cutree(hc1, k = 5)

# Number of members in each cluster
table(grp)
clustdata1<-mutate(clustdata,cluster=grp)
ngbrhd<- cbind(clustdata1,Neighborhood=treedata1$Neighborhood)

#combining the neighborhood data to see  if the clusters 
#will be able to reveal information about the neighborhoods

ClusterByNgbrGrp <- sqldf('SELECT NEIGHBORHOOD,
                                  SUM(CASE WHEN CAST(CLUSTER AS INT)=1 THEN 1 ELSE 0 END) AS CLUSTER1,
                                  SUM(CASE WHEN CAST(CLUSTER AS INT)=2 THEN 1 ELSE 0 END) AS CLUSTER2,
                                  SUM(CASE WHEN CAST(CLUSTER AS INT)=3 THEN 1 ELSE 0 END) AS CLUSTER3,
                                  SUM(CASE WHEN CAST(CLUSTER AS INT)=4 THEN 1 ELSE 0 END) AS CLUSTER4,
                                  SUM(CASE WHEN CAST(CLUSTER AS INT)=5 THEN 1 ELSE 0 END) AS CLUSTER5,
                                  count(NEIGHBORHOOD) AS NO_OF_PROPERTIES
                           FROM ngbrhd
                           GROUP BY NEIGHBORHOOD')

sum(ClusterByNgbrGrp$NO_OF_PROPERTIES)

ClusterByNgbrGrp <- ClusterByNgbrGrp[order(-ClusterByNgbrGrp$NO_OF_PROPERTIES),]

ClusterByNgbrGrp$CLUSTER1 <- round(ClusterByNgbrGrp$CLUSTER1/ClusterByNgbrGrp$NO_OF_PROPERTIES*100)/100
ClusterByNgbrGrp$CLUSTER2 <- round(ClusterByNgbrGrp$CLUSTER2/ClusterByNgbrGrp$NO_OF_PROPERTIES*100)/100
ClusterByNgbrGrp$CLUSTER3 <- round(ClusterByNgbrGrp$CLUSTER3/ClusterByNgbrGrp$NO_OF_PROPERTIES*100)/100
ClusterByNgbrGrp$CLUSTER4 <- round(ClusterByNgbrGrp$CLUSTER4/ClusterByNgbrGrp$NO_OF_PROPERTIES*100)/100
ClusterByNgbrGrp$CLUSTER5 <- round(ClusterByNgbrGrp$CLUSTER5/ClusterByNgbrGrp$NO_OF_PROPERTIES*100)/100

ClusterByNgbrGrp

#write.csv(ClusterByNgbrGrp,file="NeighborhoodClusters.csv",row.names=FALSE)

#==========================Best Model Based on RMSE======================#

RMSEcum <- data.frame('Model'=c('RMSEforest1',
                                'RMSEforest2',
                                'RMSEforest3',
                                'RMSEforest4',
                                'RMSEforest5',
                                'RMSEforest6',
                                'RMSEforest7',
                                'RMSEforest8',
                                'RMSEforest9',
                                'RMSEprunedtree1',
                                'RMSEreg1',
                                'RMSEreg2',
                                'RMSEtree1',
                                'RMSEtree2'),
                      'RMSE'=c(RMSEforest1,
                               RMSEforest2,
                               RMSEforest3,
                               RMSEforest4,
                               RMSEforest5,
                               RMSEforest6,
                               RMSEforest7,
                               RMSEforest8,
                               RMSEforest9,
                               RMSEprunedtree1,
                               RMSEreg1,
                               RMSEreg2,
                               RMSEtree1,
                               RMSEtree2)
                      );
RMSEcum <- RMSEcum[order(RMSEcum$RMSE),]
RMSEcum
#RMSEforest4 has lowest RMSE of 25,201.14. Therefore, Random Forest model 4 (mtry=16,ntree=3000) is the best fit out of all models tested.
avgsaleprice <- mean(raw$SalePrice)
avgsaleprice
RMSEoverAvgPrice <- RMSEcum[1,2]/avgsaleprice
RMSEoverAvgPrice
#Lowest RMSE is 14% of average sales price. Too high! 