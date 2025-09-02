library(dplyr)
setwd('/Users/ray/Downloads/meeting/資料集/Customer Churn')
CustomerChurn <- read.csv("Customer Churn.csv", header = 1)
names(CustomerChurn)

names(table(CustomerChurn$Complains))
CustomerChurn <- CustomerChurn %>% rename(y=Churn)

# 刪除 age 變數 
CustomerChurn.data <- CustomerChurn %>% select(-Age)

fac_var <- c("Complains","Tariff.Plan" ,"Status")
ord_var <- c("Charge..Amount","Age.Group")
num_var <- setdiff(names(CustomerChurn.data), c(fac_var,ord_var,"y"))

# for (i in c(fac.var,ord_var) ){
#   cat(i, ":")
#   print(names(table(CustomerChurn[[i]])))
#   print(table(CustomerChurn[[i]]), end='\n')
# 
#   for (j in names(table(CustomerChurn[[i]]))) {
#     df <- CustomerChurn %>% filter(!!sym(i) == j)
#     cat(paste0(j, "(", i, "):"))
#     print(table(df$Churn))
#   }
# }

# 轉資料型態
CustomerChurn.scale <- CustomerChurn.data
CustomerChurn.scale[fac_var] <- lapply(CustomerChurn.scale[fac_var], as.factor)

CustomerChurn.scale[c(num_var, ord_var)] <- CustomerChurn.scale[c(num_var, ord_var)] %>%
  mutate(across(everything(), ~ scale(.) %>% as.numeric()))

CustomerChurn.scale$y <- as.factor(CustomerChurn.scale$y)
str(CustomerChurn.scale)

# VIF
library(car)
model_full_vif <- glm(y ~ ., data = CustomerChurn.scale, family = binomial)
write.csv(vif(model_full_vif) %>% round(4), file="GVIF CC.csv", row.names = F)
vif(model_full_vif)

library(corrplot)

# 取出數值變數（排除 factor 和 y）
numeric_vars <- sapply(CustomerChurn.scale, is.numeric)
cor_data <- CustomerChurn.scale[, numeric_vars]

# 計算相關係數矩陣
cor_matrix <- cor(cor_data)
png("相關係數矩陣 CC.png", width = 21.93, height = 13.65, units = "cm", res = 300, bg = "transparent")
# 畫出相關係數矩陣並旋轉橫軸文字
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,  # 旋轉文字
         addCoef.col = "black",
         number.cex = 0.6,
         diag = FALSE)
dev.off()

# 刪除共線性高的變數
CustomerChurn.ch <- CustomerChurn.scale %>% select(-c(Customer.Value,Frequency.of.use))
model_ch_vif <- glm(y ~ ., data = CustomerChurn.ch, family = binomial)
write.csv(vif(model_ch_vif) %>% round(4), file="GVIF ch2 CC.csv", row.names = F)
vif(model_ch_vif)
