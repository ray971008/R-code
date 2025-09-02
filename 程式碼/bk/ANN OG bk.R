rm(list=(ls(all=T)))

library(dplyr)
library(fastDummies)
library(caret)
library(neuralnet)
library(nnet)  
library(e1071)
library(smotefamily)
library(catboost)
data.ecd <- function(data, numerical, categorical){
  if (length(numerical) > 0) {
    data.scale <- data %>%
      mutate(across(all_of(numerical), ~as.numeric(scale(.))))
    if (length(categorical) <= 0){
      data.dummy <- data.scale
      data.factor <- data.scale
    }
  }
  if (length(categorical) > 0) {
    if (length(numerical) <= 0){
      data.scale <- data
    }
    data.dummy <- dummy_cols(data.scale,
                             select_columns = categorical,
                             remove_selected_columns = TRUE,
                             remove_first_dummy = TRUE)
    data.factor <- data.scale %>%
      mutate(across(categorical, as.factor))
  }
  return(list(dummy = data.dummy, factor = data.factor))
}

dataSplit=function(x, y){
  datxy=cbind(x,y)
  ind=which(y==0)
  x0=x[ind,]; y0=y[ind]  ## for y=0
  x1=x[-ind,]; y1=y[-ind] ## for y=1
  n20=floor(tr*length(y0)); ind20=sample(1:length(y0), n20)
  n21=floor(tr*length(y1)); ind21=sample(1:length(y1), n21)
  x.train=rbind(x0[ind20,], x1[ind21,])
  y.train=c(y0[ind20], y1[ind21])
  x.test=rbind(x0[-ind20,], x1[-ind21,])
  y.test=c(y0[-ind20], y1[-ind21])
  return(list(x.train=x.train, y.train=y.train, x.test=x.test, y.test=y.test))
}

## catboost data change
CBT.dataChange <- function(train_data,test_data,Y){
  # 檢查 Y 欄位是否存在於資料框中
  train_data[[Y]]=ifelse(train_data[[Y]]=="F", 0, 1)
  test_data[[Y]]=ifelse(test_data[[Y]]=="F", 0, 1)

  # 創建訓練池
  train.pool <- catboost.load_pool(
    data = train_data[, -which(names(train_data) == "Y")],  # 特徵資料
    label = train_data$Y  # 目標變數是數值型
  )
  test.pool <- catboost.load_pool(
    data = test_data[, -which(names(test_data) == "Y")],  # 特徵資料
    label = test_data$Y  # 目標變數是數值型
  )
  return(list(train.pool=train.pool, test.pool=test.pool))
}


# ==== 讀取資料 =====
## ===== bank  =====
setwd('/Users/ray/Downloads/meeting/資料集/bank')

lines <- readLines("bank-full.csv")
bank.full <- read.csv(text = lines, header = TRUE, sep = ";")
bank.full$y <- ifelse(bank.full$y == "yes", 1, 0)

num <- c("age", "balance", "duration", "campaign", "pdays", "previous")
cag <- c("job", "marital", "education", "default", "housing", "loan", "contact",
         "day","month", "poutcome")
bank <- data.ecd(bank.full,num,cag)

bank.dummy.x <- bank$dummy %>% select(-y)
bank.dummy.y <- bank$dummy$y

bank.factor.x <- bank$factor %>% select(-y)
bank.factor.y <- bank$factor$y

#===== end code =====
iter=3## the number of iterations
tr=0.75  ## the training rate

set.seed(123)

acc.ann.test=sen.ann.test=spe.ann.test=array()
acc.ann.train=sen.ann.train=spe.ann.train=array()
# acc.CBT.test=sen.CBT.test=spe.CBT.test=array()
# acc.CBT.train=sen.CBT.train=spe.CBT.train=array()


for (i in 1:iter){
  # ann with bank
  dat.S=dataSplit(bank.dummy.x,bank.dummy.y)
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
apply(metric.ann.test,2,mean) %>% round(4)
apply(metric.ann.train,2,mean) %>% round(4)

setwd('/Users/ray/Downloads/meeting/結果/bank')
write.csv(metric.ann.test, file = paste0("ann_testMetric bank",".csv"), row.names = FALSE)
write.csv(metric.ann.train, file = paste0("ann_trainMetric bank",".csv"), row.names = FALSE)
