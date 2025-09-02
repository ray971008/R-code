setwd('/Users/ray/Downloads/meeting/資料集/blood')
library(dplyr)
blood.data <- read.csv("transfusion.data", header = 1)

blood.data <- blood.data %>% rename(y=whether.he.she.donated.blood.in.March.2007)
names(blood.data)

# for (i in names(blood.data)){
#   cat(i,":")
#   print(table(blood.data[[i]]))
# }

blood.clean <- blood.data %>% select(-Monetary..c.c..blood.)

# 轉資料型態
num_var <- setdiff(names(blood.clean),"y")
blood.scale <- blood.clean

blood.scale[num_var] <- blood.scale[num_var] %>%
  mutate(across(everything(), ~ scale(as.numeric(as.character(.))) %>% as.numeric()))

blood.scale$y <- as.factor(blood.scale$y)


# VIF
library(car)
model_full_vif <- glm(y ~ ., data = blood.scale, family = binomial)
write.csv(vif(model_full_vif) %>% round(4), file="VIF blood.csv", row.names = F)
vif(model_full_vif)


