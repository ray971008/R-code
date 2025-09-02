rm(list=(ls(all=T)))
setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
# ==== 讀取資料 =====

setwd('/Users/ray/Downloads/meeting/資料集/Customer Churn')

CustomerChurn <- read.csv("Customer Churn.csv", header = 1)
names(CustomerChurn)

CustomerChurn <- CustomerChurn %>% rename(y=Churn)

# 刪除 age 變數 
CustomerChurn.data <- CustomerChurn %>% select(-Age)

fac_var <- c("Complains","Tariff.Plan" ,"Status","y")
ord_var <- c("Charge..Amount","Age.Group")
num_var <- setdiff(names(CustomerChurn.data), c(fac_var,ord_var))

CustomerChurn <- data.ecd(CustomerChurn.data,c(ord_var,num_var),fac_var)

CustomerChurn.factor.x <- CustomerChurn$factor %>% select(-y)
CustomerChurn.factor.y <- CustomerChurn$factor$y


#===== end code =====
set.seed(123)
iter=100# the number of iterations
tr=0.75  ## the training rate

acc.CBT.test=sen.CBT.test=spe.CBT.test=array()
acc.CBT.train=sen.CBT.train=spe.CBT.train=array()

for (i in 1:iter){
  # CBT with bank
  dat.S=dataSplit(CustomerChurn.factor.x ,CustomerChurn.factor.y)
  xtrain=dat.S$x.train; ytrain=dat.S$y.train
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
apply(metric.CBT.train,2,mean) %>% round(4)
apply(metric.CBT.test,2,mean) %>% round(4)


setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/CBT')
write.csv(metric.CBT.test, file = paste0("CBT_testMetric cc",".csv"), row.names = FALSE)
write.csv(metric.CBT.train, file = paste0("CBT_trainMetric cc",".csv"), row.names = FALSE)


