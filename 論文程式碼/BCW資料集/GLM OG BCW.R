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

names(BCW.scale)

BCW.x <- BCW.scale %>% select(-y)
BCW.y <- BCW.scale$y

acc.GLM.test=sen.GLM.test=spe.GLM.test=array()
acc.GLM.train=sen.GLM.train=spe.GLM.train=array()

# ===== end code =====
set.seed(123)
tr=0.75
iter=100

for (i in 1:iter){
  dat.S=dataSplit(BCW.x ,BCW.y)
  
  # train data
  xtrain=dat.S$x.train; ytrain=dat.S$y.train
  data.train=data.frame(xtrain, factor(ytrain))
  
  # test data
  xtest=dat.S$x.test;  ytest=dat.S$y.test
  data.test=data.frame(xtest, factor(ytest))
  
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  
  logit.model <- glm(Y ~ ., data = data.train, family = binomial)
  logit.prob.train <- predict(logit.model, newdata = data.train, type = "response")
  
  logit.pred.train <- ifelse(logit.prob.train >= 0.5, 1, 0) %>% as.factor()
  conM.GLM.train=confusionMatrix(data=logit.pred.train, reference=data.train$Y)
  acc.GLM.train[i]=conM.GLM.train$overall["Accuracy"] %>% round(4)
  spe.GLM.train[i]=conM.GLM.train$byClass["Sensitivity"] %>% round(4)
  sen.GLM.train[i]=conM.GLM.train$byClass["Specificity"]  %>% round(4)
  
  logit.prob.test <- predict(logit.model, newdata = data.test, type = "response")
  logit.pred.test <- ifelse(logit.prob.test >= 0.5, 1, 0) %>% as.factor()
  
  conM.GLM.test=confusionMatrix(data=logit.pred.test, reference=data.test$Y)
  acc.GLM.test[i]=conM.GLM.test$overall["Accuracy"] %>% round(4)
  spe.GLM.test[i]=conM.GLM.test$byClass["Sensitivity"] %>% round(4)
  sen.GLM.test[i]=conM.GLM.test$byClass["Specificity"]  %>% round(4)
}

metric.GLM.test=data.frame(acc.GLM.test, sen.GLM.test, spe.GLM.test)
metric.GLM.train=data.frame(acc.GLM.train, sen.GLM.train, spe.GLM.train)

cat(end='\n')
apply(metric.GLM.train,2,mean) %>% round(4)
apply(metric.GLM.test,2,mean) %>% round(4)

setwd('/Users/ray/Downloads/meeting/結果/BCW/GLM')
write.csv(metric.GLM.test, file = paste0("GLM_testMetric BCW",".csv"), row.names = FALSE)
write.csv(metric.GLM.train, file = paste0("GLM_trainMetric BCW",".csv"), row.names = FALSE)
