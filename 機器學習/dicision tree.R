set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <- read.csv(path) 
head(titanic)
str(titanic)
names(titanic)
titanic <- titanic[,c(3,5,6)]

#titanic$pclass <- as.factor(titanic$pclass)
titanic$survived <- as.factor(titanic$survived)
titanic$sex <- as.factor(titanic$sex)
titanic$age <- as.numeric(titanic$age)

#titanic$sibsp<- as.factor(titanic$sibsp)
#titanic$parch <- as.factor(titanic$parch)
#titanic$fare <- as.numeric(titanic$fare)
#titanic$cabin  <- as.factor(titanic$cabin)
#titanic$embarked  <- as.factor(titanic$embarked)
# 移除不必要的欄位，例如 'name'
str(titanic)

# 隨機打亂資料順序
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)
set.seed(678)
# 分割訓練集和測試集
train_index <- sample(1:nrow(titanic), 0.8 * nrow(titanic))
test_index <- setdiff(1:nrow(titanic), train_index)

X_train_all <- titanic[train_index, ]
X_test_all <- titanic[test_index, ]

# 加載 rpart 和 rpart.plot 套件
library(rpart)
library(rpart.plot)

# 訓練決策樹模型
fit <- rpart(survived ~ ., data = X_train_all, method = 'class')
fit
rpart.plot(fit, extra = 106 ,cex=1)


# 預測未見資料
predict_unseen <- predict(fit, X_test_all, type = 'class')

# 檢查預測結果
table_mat <- table(X_test_all$survived,predict_unseen)
table_mat

# accuracy
accuracy <- (131+69)/(131+30+32+69)
cat("Accuracy : ",accuracy)


