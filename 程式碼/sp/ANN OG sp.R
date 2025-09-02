setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
setwd('/Users/ray/Downloads/meeting/資料集/spambase')
spambase <- read.csv("spambase.data", header = FALSE)
spambase.clean <- spambase %>% rename(y=V58)

num <- names(spambase.clean) %>% setdiff("y")

spambase.clean <- data.ecd(spambase.clean,num,NULL)

spambase.dummy.x <- spambase.clean$dummy %>% select(-y)
spambase.dummy.y <- spambase.clean$dummy$y

spambase.factor.x <- spambase.clean$factor %>% select(-y)
spambase.factor.y <- spambase.clean$factor$y

#===== end code =====
iter=10## the number of iterations
tr=0.75  ## the training rate

acc.ann.test=sen.ann.test=spe.ann.test=array()
acc.ann.train=sen.ann.train=spe.ann.train=array()


for (i in 1:iter){
  # ann with spambase
  dat.S=dataSplit(spambase.dummy.x,spambase.dummy.y)
  xtrain=dat.S$x.train; ytrain=dat.S$y.train
  ytrain.c=ifelse(ytrain==0, "F", "S")
  data.train=data.frame(xtrain, factor(ytrain.c))
  
  xtest=dat.S$x.test;  ytest=dat.S$y.test
  ytest.c=ifelse(ytest==0, "F", "S")
  data.test=data.frame(xtest, factor(ytest.c))
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  
  cat(paste0(i,"'"))
  
  ## ann model
  model <- train(Y ~ ., 
                 data = data.train,
                 method = "nnet", 
                 tuneGrid = expand.grid(size = c(1:20), decay = 0), # decay 設定為固定值
                 trace = FALSE,
                 maxit = 1000,
                 MaxNWts = 5000)
  size <- which.max(model$resample$Kappa)
  model.ann <- nnet(Y~., data=data.train, size = size, maxit = 1000,MaxNWts = 5000, trace=FALSE)
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
apply(metric.ann.test,2,mean) %>% round(4)
apply(metric.ann.train,2,mean) %>% round(4)

setwd('/Users/ray/Downloads/meeting/結果/spambase')
write.csv(metric.ann.test, file = paste0("ann_testMetric sp",".csv"), row.names = FALSE)
write.csv(metric.ann.train, file = paste0("ann_trainMetric sp",".csv"), row.names = FALSE)
