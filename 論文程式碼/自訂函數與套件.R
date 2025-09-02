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

# ===== Catboost =====
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

# ===== SMOTE =====
Smote <- function(x, y) {
  # 定義 features 和 labels
  features <- data.frame(x)    # 輸入的特徵矩陣
  labels <- y      # 輸出的標籤
  
  # 確保 labels 是因子型態
  labels <- as.factor(labels)
  # 執行 SMOTE
  smote_result <-
    SMOTE(
      X = features,
      target = labels,
      K=3,
      dup_size=0
    )
  x <- smote_result$data %>% select(-class) %>% as.matrix()
  y <- smote_result$data$class %>% as.numeric()
  return(list(x=x,y=y))
}

# 變數有類別型使用的 smote
library(themis)
library(tidymodels)
smote_nc <- function(data, y){
  # 準備 recipe
  rec <- recipe(formula = as.formula(paste(y, "~ .")), data = data) %>%
    step_smotenc(all_outcomes(), neighbors = 3, over_ratio = 1)
  
  # 準備並應用 recipe
  rec_prep <- prep(rec)
  df_balanced <- bake(rec_prep, new_data = NULL)
  
  # 回傳平衡後的資料集
  return(df_balanced)
}



# ===== ZIB 模型 =====

## alp: the multiplier of ridge penalty
loss.f=function(x, y, alp, del.v){
  x=as.matrix(x)
  
  ## -log-likelihood functions+ridge penalty
  like.f=function(beta){
    ## prevent the exp. term goes overflow
    ## exp(710) is overflowed
    expterm=x%*%beta
    ind=which(expterm>=700)
    if(length(ind)>0){expterm[ind]=700}
    e1=exp(x%*%beta)
    p.x=e1/(1+e1)
    pi.y =(1 - del.y) * p.x
    
    loglik.ce=-mean(log(1-pi.y)+(y*log(pi.y/(1-pi.y))))+alp*sum(beta^2)
    ## use cross-entropy as loss function
    return(loglik.ce)
  }
  
  ## find an approximate value of del.v
  beta.mat=matrix(nrow=length(del.v), ncol=m)  ## matrix to save beta.est
  loss.v=array()  ## the vectorto save loss
  for (i in 1:length(del.v)){
    del.y=del.v[i]
    out.e=optim(rep(1,m), like.f)
    ## find MLEs with initial value of be0=(1,1,..,1)
    beta.mat[i,]=out.e$par
    loss.v[i]=out.e$value
  }
  ind=which(loss.v==min(loss.v))
  if(length(ind)>1){ind=min(ind)}
  
  ## Find the best value of delta base3d on the approx. delta
  del.v2=seq(max(0, del.v[ind]-0.1), del.v[ind]+0.1, 0.05)
  beta.mat=matrix(nrow=length(del.v2), ncol=m)  ## matrix to save beta.est
  loss.v=array()  ## the vectorto save loss
  for (i in 1:length(del.v2)){
    del.y=del.v2[i]
    out.e=optim(rep(1,m), like.f)
    ## find MLEs with initial value of be0=(1,1,..,1)
    beta.mat[i,]=out.e$par
    loss.v[i]=out.e$value
  }
  ind1=which(loss.v==min(loss.v))
  if(length(ind1)>1){ind=min(ind1)}
  return(list(be.est=beta.mat[ind1,], del.y=del.v2[ind1], loss=loss.v[ind1]))
}

## given the data (x,y)
## A function to obtain the best estimators
## based on 50 randomly generated alpha values
est.all=function(x,y, alp.v){
  be.alp=matrix(nrow=length(alp.v), ncol=m)
  dely.alp=loss.alp=array()
  for (i in 1:length(alp.v)){
    alp=alp.v[i]
    est.val=loss.f(x, y, alp, del.v)
    be.alp[i,]=round(est.val$be.est,4)
    dely.alp[i]=round(est.val$del.y,4)
    loss.alp[i]=round(est.val$loss,4)
  }
  ind1=which(loss.alp==min(loss.alp))
  
  return(list(be.mle=be.alp[ind1,], del.mle=dely.alp[ind1], loss.val=loss.alp[ind1], alp.best=alp.v[ind1]))
}

## model for pi.hat
## x.train, y.train: training data
## x.test, y.test: testing data
model.y=function(x.train, y.train, alp.v){
  x.train = as.matrix(x.train)
  estVal=est.all(x.train,y.train, alp.v)
  ee=exp(x.train%*%estVal$be.mle)
  pp=ee/(1+ee)  
  pi.est=round((1-estVal$del.mle)*pp,4)
  
  ## error rate, given c based on testing data
  ## first using training data to find the best cut point
  err.c=function(c){
    ind=which(pi.est>=c)
    yhat=rep(0,length(y.train)); yhat[ind]=1
    return(abs(sum(y.train-yhat))/length(y.train))
  }
  c.v=seq(0.1,0.9,0.001)  ## cut point from 0.1~0.9
  err.n=sapply(c.v,err.c)
  ind=which(err.n==min(err.n))
  if (length(ind)>1) {ind=min(ind)}
  cut.p=c.v[ind] ## the best cut point based on training data
  return(list(cut.p=cut.p,be.mle=estVal$be.mle, del.mle=estVal$del.mle))
}


## predict error rate based on tesing data
predict.f=function(x.test, y.test, be.mle, del.mle, cut.p){
  ## find the err. rate of testing data
  x.test = as.matrix(x.test)
  ee=exp(x.test%*%be.mle)
  pp=ee/(1+ee)  
  pi.est=round((1-del.mle)*pp,4)
  yhat=ifelse(pi.est>=cut.p,1,0)
  conM=confusionMatrix(data=factor(yhat), reference=factor(y.test))  ## 注意敏感度和特異度相反
  ## accuracy
  acc=conM$overall["Accuracy"]
  ## 真陽性: sensitivity
  sen=conM$byClass["Specificity"]  
  ## 真陰性: specificity
  spe=conM$byClass["Sensitivity"]
  
  ## print(conM)
  ## print(c(acc, sen, spe))
  return(round(c(acc, sen, spe),4))
}