setwd('/Users/ray/Downloads/meeting/資料集/breast+cancer+wisconsin+original')
library(dplyr)
BCW.data <- read.csv("breast-cancer-wisconsin.data", header = F)
nrow(BCW.data)
BCW.data <- BCW.data %>% rename(y=V11)
# 刪除遺失值(?) 
missing_rows <- apply(BCW.data, 1, function(row) any(row == "?"))
View(BCW.data[missing_rows, ])
sum(missing_rows)

BCW.clean <- BCW.data[!missing_rows, ]
BCW.clean <- BCW.clean %>% select(-V1)
names(BCW.clean)
BCW.clean$y[BCW.clean$y == 2] <- 0
BCW.clean$y[BCW.clean$y == 4] <- 1

table(BCW.clean$y)

# 轉資料型態
num_var <- setdiff(names(BCW.clean),"y")
BCW.scale <- BCW.clean

BCW.scale[num_var] <- BCW.clean[num_var] %>%
  mutate(across(everything(), ~ scale(as.numeric(as.character(.))) %>% as.numeric()))

BCW.scale$y <- as.factor(BCW.scale$y)

# VIF
library(car)
model_full_vif <- glm(y ~ ., data = BCW.scale, family = binomial)
write.csv(vif(model_full_vif) %>% round(4), file="VIF BCW.csv", row.names = F)
vif(model_full_vif)