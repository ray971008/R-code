rm(list=(ls(all=T)))
setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
# ==== 讀取資料 =====

setwd('/Users/ray/Downloads/meeting/資料集/Customer Churn')

CustomerChurn <- read.csv("Customer Churn.csv", header = 1)
names(CustomerChurn)

CustomerChurn <- CustomerChurn %>% rename(y=Churn)

# 刪除 age 變數 
CustomerChurn.data <- CustomerChurn %>% select(-c(Age,Customer.Value,Frequency.of.use))

fac_var <- c("Complains","Tariff.Plan" ,"Status","y")
ord_var <- c("Charge..Amount","Age.Group")
num_var <- setdiff(names(CustomerChurn.data), c(fac_var,ord_var))

CustomerChurn<- data.ecd(CustomerChurn.data,c(num_var,ord_var),fac_var) #資料集真維度
CustomerChurn.factor.x <- CustomerChurn$factor %>% select(-y)
CustomerChurn.factor.y <- CustomerChurn$factor$y
# ==== 參數設定 ====
setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ZIB')
sink("ZIB_S2 cc.lst")

m=length(names(CustomerChurn$dummy))-1+1 ## number of features+1 (including intercept)
del.y=0.50 ## delta
del.v=seq(0, 0.85, 0.05)  ## the domain to search delta
## generate 50 alpha in (0,1) for ridge penalty
## malp=mean of 50 alp's; sdalp=the sd of alp's
## alp.v=c(0,malp, max(0,malp-sdalp), malp+sdalp)
alp.unif=runif(50, min=0, max=2)
malp=mean(alp.unif); sdalp=sd(alp.unif); alp.v=c(0,max(0,malp-sdalp),  malp+sdalp)

iter=100  ## the number of iterations
tr=0.75  ## the training rate

## Implementation
## generate ZIB samples
## be.simu: the mle of beta
## del.simu: the mle of delta
## loss.simu: the loss function value 
## alp.simu: the best alpha with an minimal loss
## err.v: error rates
## cut.p: the best cut point

## generate all data sets for simulations
set.seed(123)

be.simu=array(dim=c(iter,m))
del.simu=cutP.simu=acc.train=acc.test=array()
sen.train=spe.train=sen.test=spe.test=array()



# ==== 迴圈 ====

for (i in 1:iter){
  
  dat.S=dataSplit(CustomerChurn.factor.x ,CustomerChurn.factor.y) # 先切割資料
  
  # train data
  xtrain=dat.S$x.train
  ytrain=dat.S$y.train
  data.train <- data.frame(xtrain,ytrain)
  
  # 順序型轉factor，插補才不會失去順序型態
  for (var in ord_var) {
    data.train[[var]] <- factor(data.train[[var]], ordered = TRUE) 
  }
  
  data.train <- smote_nc(data.train,"ytrain") # 插補
  
  for (var in ord_var) {
    data.train[[var]] <- as.numeric(as.character(data.train[[var]]))
  }
  
  # 將類別變數轉成 dummy
  data.train <- dummy_cols(data.train, 
                           select_columns = fac_var[fac_var != "y"], 
                           remove_first_dummy = TRUE,  # 避免多重共線性
                           remove_selected_columns = TRUE)  # 移除原本的
  
  xtrain <- data.train %>% select(-ytrain)
  ytrain<- data.train$ytrain
  data.train <- data.frame(xtrain,ytrain)
  data.train$ytrain =data.train$ytrain %>% as.factor()
  
  
  # test data
  xtest=dat.S$x.test;  ytest=dat.S$y.test
  ytest.c=ytest %>% as.factor()
  data.test=data.frame(xtest, ytest.c)
  
  # 將類別變數轉成 dummy
  data.test <- dummy_cols(data.test, 
                          select_columns = fac_var[fac_var != "y"], 
                          remove_first_dummy = TRUE,  # 避免多重共線性
                          remove_selected_columns = TRUE)  # 移除原本的
  
  xtest <- data.test %>% select(-ytest.c)
  ytest <- data.test$ytest.c
  data.test <- data.frame(xtest,ytest)
  
  colnames(data.train)=colnames(data.test)=c(names(data.train[1:length(names(data.train))-1]), "Y")
  data.train <- data.train[sample(nrow(data.train)),] # catboost 需要
  
  # train and test data sets
  x.train <- data.train %>% select(-Y)
  y.train <- data.train$Y
  x.test <- data.test %>% select(-Y)
  y.test <- data.test$Y
  
  # train and test data sets
  x.train=cbind(rep(1,nrow(x.train)), x.train)
  y.train=as.vector(as.numeric(y.train)-1)
  x.test=cbind(rep(1,nrow(x.test)), x.test)
  y.test=as.vector(as.numeric(y.test)-1)
  
  ## model fitting
  modelFit.tra=model.y(x.train, y.train, alp.v)
  predict.tra=predict.f(x.train, y.train, modelFit.tra$be.mle, modelFit.tra$del.mle, modelFit.tra$cut.p)
  predict.tes=predict.f(x.test, y.test, modelFit.tra$be.mle, modelFit.tra$del.mle, modelFit.tra$cut.p)
  
  ## save results
  be.simu[i,]=modelFit.tra$be.mle
  del.simu[i]=modelFit.tra$del.mle
  cutP.simu[i]=modelFit.tra$cut.p
  acc.train[i]=predict.tra[1]
  sen.train[i]=predict.tra[2]
  spe.train[i]=predict.tra[3]
  
  acc.test[i]=predict.tes[1]
  sen.test[i]=predict.tes[2]
  spe.test[i]=predict.tes[3]
  
  ## print output
  cat(i, tr, modelFit.tra$be.mle, modelFit.tra$del.mle, predict.tra, predict.tes, "\n")
}
indd=which(sen.test==0)
NoFailures=length(indd)

cat("All:", "acc.tra=", mean(acc.train), "acc.tes=", mean(acc.test), "sen.tra=", mean(sen.train), "sen.tes=", mean(sen.test), 
    "spe.tra=", mean(spe.train), "spe.tes=", mean(spe.test),"\n")

cat("No of mle failing:", NoFailures, "\n")

cat("No mle failures:", "acc.tra=", mean(acc.train[-indd]), "acc.tes=", mean(acc.test[-indd]), "sen.tra=", mean(sen.train[-indd]), 
    "sen.tes=", mean(sen.test[-indd]), "spe.tra=", mean(spe.train[-indd]), "spe.tes=", mean(spe.test[-indd]),"\n")


train.metric=data.frame(acc.train, sen.train, spe.train)
test.metric=data.frame(acc.test, sen.test, spe.test)

write.csv(be.simu, file="ZIB_S2_beSimu cc.csv", row.names = F)
write.csv(del.simu, file="ZIB_S2_DelSimu cc.csv", row.names = F)
write.csv(cutP.simu, file="ZIB_S2_cutPSimu cc.csv", row.names = F)
write.csv(train.metric, file="ZIB_S2_trainMetric cc.csv", row.names = F)
write.csv(test.metric, file="ZIB_S2_testMetric cc.csv", row.names = F)

sink()

cat(end='\n')
apply(train.metric,2,mean) %>% round(4)
apply(test.metric,2,mean) %>% round(4)
