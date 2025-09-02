setwd('/Users/ray/Downloads/meeting/程式碼')
source("自訂函數與套件.R")
setwd('/Users/ray/Downloads/meeting/資料集/spambase')
spambase <- read.csv("spambase.data", header = FALSE)
spambase.clean <- spambase %>% rename(y=V58)

num <- names(spambase.clean) %>% setdiff("y")

x <- spambase.clean %>% select(-y) %>% scale() %>% data.frame()
x.n <- x %>% select(-V55)
y <- spambase.clean$y %>% as.factor()


library(ggcorrplot)

# 計算相關係數矩陣（假設你的資料叫 x）
corr_matrix <- cor(x)

# 繪圖：完整方形矩陣 + 顯示數值
ggcorrplot(corr_matrix,
           method = "square",    # 每格是方形
           type = "full",        # 顯示整張（不是上三角）
           tl.cex = 8,           # 標籤字大小（軸上變數名稱）
           colors = c("blue", "white", "red"))  # 顏色：負相關→正相關

# 假設 corr_matrix 是一個矩陣
# 把對角線設成 NA（排除 1）
diag(corr_matrix) <- NA

# ==== VIF ====
library(car)
# 這裡 x 是你 select(-y) 過的資料框

vif_model <- glm(y ~ ., data = x.n, family = binomial)  # 拿來建線性模型只是為了跑 VIF
vif_values <- vif(vif_model)
str(vif_values)
# 把 VIF 數值轉成資料框
vif_df <- data.frame(Variable = names(vif_values),
                     VIF = as.numeric(vif_values) %>% round(4))
vif_df
# 儲存成 CSV 檔案
write.csv(vif_df, "VIF -V55 sp.csv", row.names = FALSE)

# ==== PCA  ====
# 1. 做 PCA
pca_result <- prcomp(x, center = TRUE, scale. = TRUE)

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

View(cumulated_proportion_df)

setwd('/Users/ray/Downloads/meeting/資料集/spambase')
write.csv(cumulated_proportion_df %>% round(4), file="PCA-Cumulative Proportion sp.csv", row.names = F)

png("解釋變異 sp.png", width = 21.93, height = 13.65, units = "cm", res = 300, bg = "transparent")
plot(Proportion_Of_Variance, xlab = "", ylab = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8, xaxt = "n")
axis(1, at = 1:length(Proportion_Of_Variance), labels = 1:length(Proportion_Of_Variance))
points(39, Proportion_Of_Variance[39], col = "red", pch = 19)
dev.off()

png("累積解釋變異 sp.png", width = 21.93, height = 13.65, units = "cm", res = 300, bg = "transparent")
plot(Cumulative_Proportion, xlab = "", ylab = "", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8, xaxt = "n")
axis(1, at = 1:length(Cumulative_Proportion), labels = 1:length(Cumulative_Proportion))
points(39, Cumulative_Proportion[39], col = "red", pch = 19)
dev.off()

## ggplot 視覺化
# POF_plot <- ggplot(cumulated_proportion_df, aes(x = PC, y = Proportion_Of_Variance)) +
#   geom_line() +  # 使用藍色線條
#   geom_point(aes(color = ifelse(PC == 39, "black", "red"))) +  # 第 31 個點為黑色，其他為紅色
#   scale_color_manual(values = c("red", "black")) +  # 定義顏色
#   theme_minimal() +
#   guides(color = "none") +  # 去掉圖例
#   scale_x_continuous(breaks = 1:length(Proportion_Of_Variance)) +  # 顯示所有 x 軸標籤
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # 旋轉 x 軸標籤
#   labs(y = "Proportion Of Variance")  # 加這行改 y 軸標籤
# POF_plot
# ggsave("解釋變異 sp.png", plot = POF_plot, bg = "transparent"
#        , width = 21.93, height = 13.65, units = "cm", dpi = 300)
# 
# CP_plot <- ggplot(cumulated_proportion_df, aes(x = PC, y = Cumulative_Proportion)) +
#   geom_line() +  # 使用藍色線條
#   geom_point(aes(color = ifelse(PC == 39, "black", "red"))) +  # 第 31 個點為黑色，其他為紅色
#   scale_color_manual(values = c("red", "black")) +  # 定義顏色
#   theme_minimal() +
#   guides(color = "none") +  # 去掉圖例
#   scale_x_continuous(breaks = 1:length(Cumulative_Proportion)) +  # 顯示所有 x 軸標籤
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # 旋轉 x 軸標籤
#   labs(y = "Cumulative Proportion")  # 加這行改 y 軸標籤
# CP_plot
# ggsave("累積解釋變異 sp.png", plot = CP_plot, bg = "transparent"
#        , width = 21.93, height = 13.65, units = "cm", dpi = 300)

PCcoff <- pca_result$rotation
write.csv(round(PCcoff,4), file="PCcoff sp.csv", row.names = F)


sink("PCA loading sp.lst")
# 列出每個主成分對應的 10 個最重要變數
for (pc in 1:ncol(PCcoff)) {
  cat(paste("PC", pc, ":\n", sep = ""))  # 列印主成分名稱 (PC1, PC2, ...)
  sorted_index <- order(abs(PCcoff[, pc]), decreasing = TRUE)[1:10]
  for (i in sorted_index) {
    cat(paste0("V", i, "\tValue: ", round(PCcoff[i, pc], 4), "\n"))
  }
  cat("\n") 
}
sink()
