rm(list=(ls(all=T)))
setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
# ==== 讀取資料 =====
setwd('/Users/ray/Downloads/meeting/資料集/breast+cancer+wisconsin+original')
BCW.data <- read.csv("breast-cancer-wisconsin.data", header = F)

BCW.data <- BCW.data %>% rename(y=V11)
# 刪除遺失值(?) 
missing_rows <- apply(BCW.data, 1, function(row) any(row == "?"))

BCW.clean <- BCW.data[!missing_rows, ]
BCW.clean <- BCW.clean %>% select(-V1)
BCW.clean$y[BCW.clean$y == 2] <- 0
BCW.clean$y[BCW.clean$y == 4] <- 1

# 轉資料型態
num_var <- setdiff(names(BCW.clean),"y")
BCW.scale <- BCW.clean

BCW.scale[num_var] <- BCW.clean[num_var] %>%
  mutate(across(everything(), ~ scale(as.numeric(as.character(.))) %>% as.numeric()))
BCW.scale$y <- as.factor(BCW.scale$y)

BCW.x <- BCW.scale %>% select(-y)
BCW.y <- BCW.scale$y

#===== end code =====
iter=100## the number of iterations
tr=0.75  ## the training rate

set.seed(123)

acc.CBT.test=sen.CBT.test=spe.CBT.test=array()
acc.CBT.train=sen.CBT.train=spe.CBT.train=array()


for (i in 1:iter){
  # CBT.S2 with BCW
  dat.S=dataSplit(BCW.x ,BCW.y)
  xtrain=dat.S$x.train
  ytrain=dat.S$y.train
  smote.train <- Smote(xtrain, ytrain)
  xtrain=smote.train$x
  ytrain=smote.train$y
  ytrain.c=ifelse(ytrain==0, "F", "S")
  data.train=data.frame(xtrain, factor(ytrain.c))
  xtest=dat.S$x.test;  ytest=dat.S$y.test
  ytest.c=ifelse(ytest==0, "F", "S")
  data.test=data.frame(xtest, factor(ytest.c))
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  
  cat(paste0(i,"'"))
  
  ## Catboost model
  train.pool <- CBT.dataChange(data.train,data.test,"Y")$train.pool
  test.pool <- CBT.dataChange(data.train,data.test,"Y")$test.pool
  model.CBT <- catboost.train( train.pool,
                               params = list(loss_function = 'Logloss',iterations=1000,verbose = 0))
  pred.CBT1.test <- catboost.predict(model.CBT, test.pool, prediction_type = 'Class')  # 'Class' 會返回預測的類別
  pred.CBT.test=factor(ifelse(pred.CBT1.test<=0.5, "F", "S"))
  
  conM.CBT.test=confusionMatrix(data=pred.CBT.test, reference=data.test$Y)
  acc.CBT.test[i]=conM.CBT.test$overall["Accuracy"] %>% round(4)
  spe.CBT.test[i]=conM.CBT.test$byClass["Sensitivity"] %>% round(4)
  sen.CBT.test[i]=conM.CBT.test$byClass["Specificity"]  %>% round(4)
  ## Training acc,spe,sen
  pred.CBT1.train <- catboost.predict(model.CBT, train.pool, prediction_type = 'Class')  # 'Class' 會返回預測的類別
  pred.CBT.train=factor(ifelse(pred.CBT1.train<=0.5, "F", "S"))
  
  conM.CBT.train=confusionMatrix(data=pred.CBT.train, reference=data.train$Y)
  acc.CBT.train[i]=conM.CBT.train$overall["Accuracy"] %>% round(4)
  spe.CBT.train[i]=conM.CBT.train$byClass["Sensitivity"] %>% round(4)
  sen.CBT.train[i]=conM.CBT.train$byClass["Specificity"]  %>% round(4)
  
}
metric.CBT.test=data.frame(acc.CBT.test, sen.CBT.test, spe.CBT.test)
metric.CBT.train=data.frame(acc.CBT.train, sen.CBT.train, spe.CBT.train)

cat(end='\n')
apply(metric.CBT.test,2,mean) %>% round(4)
apply(metric.CBT.train,2,mean) %>% round(4)

setwd('/Users/ray/Downloads/meeting/結果/BCW/CBT')
write.csv(metric.CBT.test, file = paste0("CBT_S2_testMetric spambase",".csv"), row.names = FALSE)
write.csv(metric.CBT.train, file = paste0("CBT_S2_trainMetric spambase",".csv"), row.names = FALSE)

