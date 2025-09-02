rm(list=(ls(all=T)))
setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
# ==== 讀取資料 =====

setwd('/Users/ray/Downloads/meeting/資料集/Customer Churn')

CustomerChurn <- read.csv("Customer Churn.csv", header = 1)
CustomerChurn <- CustomerChurn %>% rename(y=Churn)

CustomerChurn.data <- CustomerChurn %>% select(-c(Age,Customer.Value,Frequency.of.use))

fac_var <- c("Complains","Tariff.Plan" ,"Status")
ord_var <- c("Charge..Amount","Age.Group")
num_var <- setdiff(names(CustomerChurn.data), c(fac_var,ord_var,"y"))

CustomerChurn <- data.ecd(CustomerChurn.data,c(ord_var,num_var),fac_var)

CustomerChurn.dummy.x <- CustomerChurn$dummy %>% select(-y)
CustomerChurn.dummy.y <- CustomerChurn$dummy$y
#===== end code =====
iter=1## the number of iterations
tr=0.75  ## the training rate

set.seed(123)

acc.ann.test=sen.ann.test=spe.ann.test=array()
acc.ann.train=sen.ann.train=spe.ann.train=array()

for (i in 1:iter){
  # ann with bank
  dat.S=dataSplit(CustomerChurn.dummy.x,CustomerChurn.dummy.y)
  xtrain=dat.S$x.train; ytrain=dat.S$y.train
  ytrain.c=ifelse(ytrain==0, "F", "S")
  data.train=data.frame(xtrain, factor(ytrain.c))
  xtest=dat.S$x.test;  ytest=dat.S$y.test

  cat(paste0(i,"'"))
  ytest.c=ifelse(ytest==0, "F", "S")
  data.test=data.frame(xtest, factor(ytest.c))
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  model <- train(Y ~ ., 
                 data = data.train,
                 method = "nnet", 
                 tuneGrid = expand.grid(size = c(1:20), decay = 0), # decay 設定為固定值
                 trace = FALSE,
                 maxit = 1000,
                 MaxNWts = 5000)
  size <- which.max(model$resample$Kappa)
  model.ann <- nnet(Y~., data=data.train, size = size, maxit = 1000, MaxNWts = 5000, trace=FALSE)
  pred.ann1.test=predict(model.ann, newdata=data.test)
  pred.ann.test=factor(ifelse(pred.ann1.test<=0.5, "F", "S"))
  conM.ann.test=confusionMatrix(data=pred.ann.test, reference=data.test$Y)  ## 注意敏感度和特異度相反
  ## accuracy
  acc.ann.test[i]=conM.ann.test$overall["Accuracy"] %>% round(4)
  ## 真陰性: specificity
  spe.ann.test[i]=conM.ann.test$byClass["Sensitivity"] %>% round(4)
  ## 真陽性: sensitivity
  sen.ann.test[i]=conM.ann.test$byClass["Specificity"] %>% round(4)
  
  pred.ann1.train=predict(model.ann, newdata=data.train)
  pred.ann.train=factor(ifelse(pred.ann1.train<=0.5, "F", "S"))
  conM.ann.train=confusionMatrix(data=pred.ann.train, reference=data.train$Y)
  acc.ann.train[i]=conM.ann.train$overall["Accuracy"] %>% round(4)
  spe.ann.train[i]=conM.ann.train$byClass["Sensitivity"] %>% round(4)
  sen.ann.train[i]=conM.ann.train$byClass["Specificity"] %>% round(4)
  
}

metric.ann.test=data.frame(acc.ann.test, sen.ann.test, spe.ann.test)
metric.ann.train=data.frame(acc.ann.train, sen.ann.train, spe.ann.train)

cat(end='\n')
apply(metric.ann.train,2,mean) %>% round(4)
apply(metric.ann.test,2,mean) %>% round(4)


setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ANN')
write.csv(metric.ann.test, file = paste0("ann_testMetric cc",".csv"), row.names = FALSE)
write.csv(metric.ann.train, file = paste0("ann_trainMetric cc",".csv"), row.names = FALSE)
