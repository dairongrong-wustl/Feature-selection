# read in case.gex and ctrl.gex the first 801 lines(classes + 800 genes), transpose the data, 
# set class for control data to be 1 and -1 for case data . Set the structure of class to 
# be factor

case <- read.table("case.gex.csv", header = TRUE, sep = ",", na.strings = "NA")
case_first800 <- case[1:801,-1]
rownames(case_first800) <- case[1:801,1] 
case_trans <- as.data.frame(t(case_first800))
case_trans$Classes=-1
control <- read.table("ctrl.gex.csv", header = TRUE, sep = ",", na.strings = "NA")
control$WGACON89 = as.numeric(as.character(control$WGACON89))
control_first800 <- control[1:801,-1]
rownames(control_first800) <- control[1:801,1]
control_trans <- as.data.frame(t(control_first800))
control_trans$Classes=1
training_data <- rbind.data.frame(case_trans,control_trans)
training_data$Classes = as.factor(training_data$Classes)

# Using knnImputation to fill in missing data, setA and setB are created for dataset without 
# missing data and with missing data. For every observation to be imputed in set B, 
# it identifies 5 closest observations in setA based on the euclidean distance and computes 
# the weighted average (weighted based on distance) of these '5' obs as the imputed values.
# Then the setB with missing data imputed is combined with setA to create the training dataset,
# which will be used in the 3 homework problems. 

setA = training_data[complete.cases(training_data),]
setB = training_data[!complete.cases(training_data),]
library("DMwR")
setB_Complete1 <- knnImputation(setB, k = 5, scale = T, meth = 'weighAvg',
                                               distData = setA)
training_data_complete = rbind(setA, setB_Complete1) 
write.csv(training_data_complete,"training data complete.csv",sep = ",", row.names = TRUE, col.names = TRUE)
write.table(as.data.frame(names(training_data_complete)), "features.txt", row.names = FALSE, col.names = FALSE)


for(i in 2:length(names(training_data_complete))){
  names(training_data_complete)[i] = paste(unlist(strsplit(names(training_data_complete)[i],split ="-"))[1], 
                                         "_", unlist(strsplit(names(training_data_complete)[i],split ="-"))[2],sep = "")}

library(randomForest)

# Fix the number of features (genes) at 70% of the total number of features, vary the number 
# of instances from 50% to 90%, with an increment of 5%

Important_feature = matrix(0,nrow=1,ncol=801)
for(i in seq(from = 0.5, to = 0.9, by = 0.05)){
  n_train = round(nrow(training_data_complete) * i)
  Important_feature[1] = n_train 
  train = sample(1:nrow(training_data_complete),n_train)
  RF_AD = randomForest(Classes~.,data = training_data_complete,subset=train,mtry=560,ntree=500)
  Important_feature[2:801]=varUsed(RF_AD,count=TRUE)
  filename = paste("Important_feature_",i,"_of_instances.txt",sep = "")
  #write.table(Important_feature, filename, row.names = FALSE, col.names = FALSE)
  print(paste("the percentage of instances: ", i))
  print(paste("the mean of out of the bag error rate: ", mean(RF_AD$err.rate[,1])))
  } 

# Fix the number of instances at 70% of the total number of instances, vary the number
# of features (genes) from 50% to 90%, with an increment of 5%

Important_feature = matrix(0,nrow=1,ncol=801)
for(i in seq(from = 0.5, to = 0.9, by = 0.05)){
  n_train = round(nrow(training_data_complete) * 0.7)
  n_feature = round(800 * i)
  Important_feature[1] = n_feature 
  train = sample(1:nrow(training_data_complete),n_train)
  RF_AD = randomForest(Classes~.,data = training_data_complete,subset=train,mtry=n_feature,ntree=500)
  Important_feature[2:801]=varUsed(RF_AD,count=TRUE)
  filename = paste("Important_feature_",i,"_of_features.txt",sep = "")
  #print(paste("the percentage of features: ", i))
  write.table(Important_feature, filename, row.names = FALSE, col.names = FALSE)
  #print(paste("the percentage of features: ", i))
  #print(paste("the mean of out of the bag error rate: ", mean(RF_AD$err.rate[,1])))
} 

Important_feature = matrix(0,nrow=1,ncol=801)
for(i in 1:4){
  n_train = round(nrow(training_data_complete) * 0.7)
  n_feature = round(800 * 0.9)
  Important_feature[1] = n_feature 
  train = sample(1:nrow(training_data_complete),n_train)
  RF_AD = randomForest(Classes~.,data = training_data_complete,subset=train,mtry=n_feature,ntree=500)
  Important_feature[2:801]=varUsed(RF_AD,count=TRUE)
  filename = paste("0.9_features_0.7_instances_try_",i,".txt",sep = "")
  #print(paste("the percentage of features: ", i))
  write.table(Important_feature, filename, row.names = FALSE, col.names = FALSE)
  #print(paste("the percentage of features: ", i))
  #print(paste("the mean of out of the bag error rate: ", mean(RF_AD$err.rate[,1])))
} 
