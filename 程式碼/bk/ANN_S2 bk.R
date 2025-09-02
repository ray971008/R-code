
setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
setwd('/Users/ray/Downloads/meeting/資料集/bank')

lines <- readLines("bank-full.csv")
bank.full <- read.csv(text = lines, header = TRUE, sep = ";")
bank.full$y <- ifelse(bank.full$y == "yes", 1, 0)

num <- c("age", "balance", "duration", "campaign", "pdays", "previous")
cag <- c("job", "marital", "education", "default", "housing", "loan", "contact",
         "day","month", "poutcome","y")
bank <- data.ecd(bank.full,num,cag)

bank.factor.x <- bank$factor %>% select(-y)
bank.factor.y <- bank$factor$y

# ==== end code ====
iter=3## the number of iterations
tr=0.75  ## the training rate

set.seed(123)

acc.ann.test=sen.ann.test=spe.ann.test=array()
acc.ann.train=sen.ann.train=spe.ann.train=array()


for (i in 1:iter){
  # ann.S2 with bank
  
  dat.S=dataSplit(bank.factor.x ,bank.factor.y) # 先切割資料
  xtrain=dat.S$x.train
  ytrain=dat.S$y.train
  data.train <- data.frame(xtrain,ytrain)
  data.train <- smote_nc(data.train,"ytrain") # 插補 
  data.train$ytrain =ifelse(data.train$ytrain==0, "F", "S") %>% as.factor()
  data.train <- data.ecd(data.train,num,setdiff(cag, "y")) # 轉 dummy
  data.train <- data.train$dummy
  xtrain <- data.train %>% select(-ytrain)
  ytrain <- data.train$ytrain
  data.train <- data.frame(xtrain,ytrain)
  
  xtest=dat.S$x.test;  ytest=dat.S$y.test
  ytest.c=ifelse(ytest==0, "F", "S") %>% as.factor()
  data.test=data.frame(xtest, ytest.c)
  data.test <- data.ecd(data.test,num,setdiff(cag, "y")) # 轉 dummy
  data.test <- data.test$dummy
  xtest <- data.test %>% select(-ytest.c)
  ytest <- data.test$ytest.c
  data.test <- data.frame(xtest,ytest)
  
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  
  cat(paste0(i,"'"))
  
  # ann model
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
write.csv(metric.ann.test, file = paste0("ann_S2_testMetric bank",".csv"), row.names = FALSE)
write.csv(metric.ann.train, file = paste0("ann_S2_trainMetric bank",".csv"), row.names = FALSE)
