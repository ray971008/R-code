library(dplyr)
setwd('/Users/ray/Downloads/meeting/資料集/bank/bank-additional')

lines <- readLines("bank-additional-full.csv")
bank.full <- read.csv(text = lines, header = TRUE, sep = ";")
table(bank.full$marital)
names(bank.full)
table(bank.full$month)

setwd('/Users/ray/Downloads/meeting/資料集/bank/bank-additional')
lines <- readLines("bank-additional.csv")
bank.a <- read.csv(text = lines, header = TRUE, sep = ";")

factor_var <- c("job","marital","education","default","housing","loan","contact",
                "month","day_of_week","poutcome")
num_var <- setdiff(names(bank.a), c(factor_var, "y"))

bank.a.clean <- bank.a[!apply(bank.a, 1, function(row) any(row == "unknown")), ]
nrow(bank.a.clean)

# # 查看類別型變數
# for (i in factor_var){
#   cat(i, ":")
#   print(names(table(bank.a.clean[[i]])))
#   print(table(bank.a.clean[[i]]), end='\n')
#   
#   for (j in names(table(bank.a.clean[[i]]))) {
#     df <- bank.a.clean %>% filter(!!sym(i) == j) 
#     cat(paste0(j, "(", i, "):"))
#     print(table(df$y))
#   }
# }

names(table(bank.a.clean$job))
bank.a.clean$pdays <- ifelse(bank.a.clean$pdays == 999, -1, bank.a.clean$pdays)

# 轉資料型態並標準化
bank.a.scale <- bank.a.clean
bank.a.scale[factor_var] <- lapply(bank.a.scale[factor_var], as.factor)
bank.a.scale[num_var] <- bank.a.scale[num_var] %>%
  mutate(across(everything(), ~ scale(.) %>% as.numeric()))
bank.a.scale$y <- as.factor(bank.a.scale$y)

# 移除 duration

bank.a.scale <- bank.a.scale %>% select(-duration)

# 數值型變數處理 PCA
setwd('/Users/ray/Downloads/meeting/資料集/bank')
num_vars <- bank.a.scale %>% select(where(is.numeric))
pca_result <- prcomp(num_vars, scale. = TRUE)
summary(pca_result)
pca_summary <- summary(pca_result)$importance

Standard_Deviation = pca_summary["Standard deviation", ] %>% as.vector()
Proportion_Of_Variance = pca_summary["Proportion of Variance", ] %>% as.vector()
Cumulative_Proportion = pca_summary["Cumulative Proportion" , ] %>% as.vector()

cumulated_proportion_df <- data.frame(
  PC = 1:length(Cumulative_Proportion),
  Standard_Deviation=Standard_Deviation,
  Proportion_Of_Variance=Proportion_Of_Variance,
  Cumulative_Proportion=Cumulative_Proportion
) 
write.csv(cumulated_proportion_df %>% round(4), file="PCA-Cumulative Proportion bk.csv", row.names = F)

png("解釋變異 bk.png", width = 21.93, height = 13.65, units = "cm", res = 300, bg = "transparent")
plot(Proportion_Of_Variance, xlab = "", ylab = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8, xaxt = "n")
axis(1, at = 1:length(Proportion_Of_Variance), labels = 1:length(Proportion_Of_Variance))
points(4, Proportion_Of_Variance[4], col = "red", pch = 19)
dev.off()

png("累積解釋變異 bk.png", width = 21.93, height = 13.65, units = "cm", res = 300, bg = "transparent")
plot(Cumulative_Proportion, xlab = "", ylab = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8, xaxt = "n")
axis(1, at = 1:length(Cumulative_Proportion), labels = 1:length(Cumulative_Proportion))
points(4, Cumulative_Proportion[4], col = "red", pch = 19)
dev.off()

View(cumulated_proportion_df)
View(pca_result$rotation)
write.csv((pca_result$rotation) %>% round(4), file="PCA-loading bk.csv", row.names = F)

# PCA 與 類別變數合併成新資料
pca_vars <- as.data.frame(pca_result$x[, 1:4])
cat_vars <- bank.a.scale %>% select(where(is.factor))
final_data <- cbind(pca_vars, cat_vars)

# VIF
library(car)
View(final_data)
model_full_vif <- glm(y ~ ., data = final_data, family = binomial)
write.csv(vif(model_full_vif) %>% round(4), file="GVIF bk.csv", row.names = F)
vif(model_full_vif)



# 類別型變數處理
bank.a.scale$education3 <- case_when(
  bank.a.scale$education %in% c("basic.4y", "basic.6y","basic.9y", "illiterate") ~ "primary",
  bank.a.scale$education %in% c("professional.course", "high.school") ~ "secondary",
  bank.a.scale$education %in% c("university.degree") ~ "tertiary",
  TRUE ~ NA_character_  # 預防萬一有其他未知的情況
)

table(bank.a.scale$education3)
# 也可以把它轉成 factor
bank.a.scale$education3 <- factor(bank.a.scale$education3)
table(bank.a.scale$education3)

# 教育變數 似然比檢定
model_education3 <- glm(y ~ .-education, data = bank.a.scale, family = binomial(link='logit'))
model_full <- glm(y ~ .-education3, data = bank.a.scale, family = binomial(link='logit'))
anova(model_education3,model_full, test = "LRT")

model_job<- glm(y ~ .-education3-job, data = bank.a.scale, family = binomial(link='logit'))
model_full <- glm(y ~ .-education, data = bank.a.scale, family = binomial(link='logit'))
anova(model_job,model_full, test = "LRT")

# 建立一個最完整的模型，作為逐步回歸的起點
model_full <- glm(y ~ .-education-duration, data = bank.a.scale, family = binomial(link = "logit"))
model_step <- step(model_full, direction = "both", trace = 1)
summary(model_step)



