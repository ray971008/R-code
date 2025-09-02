# install.packages("haven")
library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr) # 載入 tidyr 套件
library(RColorBrewer) # 載入 RColorBrewer 套件
library(gridExtra)
## 顯示中文
library(patchwork)
library(showtext)

library(lavaan) # likelihood ratio test 使用


#font_families_google()
showtext_auto(enable = TRUE)
par(family = "STHeiti")

# 設定中文字型
theme_set(theme_minimal(base_family = "STHeiti"))
##

setwd('/Users/ray/Downloads/碩士班/113 上/應用多變量分析/期末報告2')
data <- read.csv('aqx_p_488-4.csv')

data <- data %>%
  rename(
    測站名稱 = sitename,
    縣市 = county,
    空氣品質指標 = aqi,
    空氣污染指標物 = pollutant,
    狀態 = status,
    二氧化硫_ppb = so2,
    一氧化碳_ppm = co,
    臭氧_ppb = o3,
    臭氧8小時移動平均_ppb = o3_8hr,
    懸浮微粒_μg_m3 = pm10,
    細懸浮微粒_μg_m3 = pm2.5,
    二氧化氮_ppb = no2,
    氮氧化物_ppb = nox,
    一氧化氮_ppb = no,
    資料發布時間 = datacreationdate,
    單位 = unit,
    一氧化碳8小時移動平均_ppm = co_8hr,
    細懸浮微粒移動平均值_μg_m3 = pm2.5_avg,
    懸浮微粒移動平均值_μg_m3 = pm10_avg,
    二氧化硫移動平均值_ppb = so2_avg,
    經度 = longitude,
    緯度 = latitude,
    測站編號 = siteid
  ) %>% 
  select(-單位) %>% 
  filter(資料發布時間 != "2024-12-28 05:00") 
count_1 <- nrow(data)
count_1

# 檢查遺失值
data_with_na <-data %>%
  filter(if_any(everything(), is.na))  # 檢查每一列是否有 NA 值 
table(data_with_na$sitename)
write.csv(data_with_na,"missimg data.csv")

data <- data %>% filter(測站名稱 != "潮州")
# 使用同一 sitename 計算每個變數的平均數，並將 NA 值進行插補
data_clean <- data %>%
  group_by(測站名稱) %>%  # 
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm = TRUE), .))) %>%  # 對數值變數進行插補
  ungroup()  # 取消分組


data_with_na2 <- data_clean %>%
  filter(if_any(everything(), is.na))  # 檢查每一列是否有 NA 值 
View(data_with_na2)
count_2 <- nrow(data_clean)
count_2
data$測站名稱
data_clean$資料發布時間 <- as.POSIXct(data_clean$資料發布時間, format = "%Y-%m-%d %H:%M")
city <- c("基隆市","臺北市","新北市","桃園市","新竹縣","新竹市","苗栗縣",
          "宜蘭縣","臺中市","彰化縣","雲林縣","嘉義縣","南投縣","臺南市",
          "高雄市","屏東縣","臺東縣","花蓮縣","澎湖縣","連江縣","金門縣")
variable <- c(  "二氧化硫_ppb", "一氧化碳_ppm ","臭氧_ppb" ,"臭氧8小時移動平均_ppb" ,
                "懸浮微粒_μg_m3" , "細懸浮微粒_μg_m3" ,"二氧化氮_ppb" , "氮氧化物_ppb",
                "一氧化氮_ppb" , "一氧化碳8小時移動平均_ppm" , "細懸浮微粒移動平均值_μg_m3",
                "懸浮微粒移動平均值_μg_m3","二氧化硫移動平均值_ppb")
# 使用 ggplot2 繪製折線圖

for (i in city){
    pic <- ggplot(data_clean[data_clean$縣市==i,], aes(x = 資料發布時間, y = 二氧化硫_ppb, color = 測站名稱, group = 測站名稱)) +
      geom_line(size = 1) +
      labs(title = paste0(i," 空氣品質指標隨時間變化"), 
           x = "觀測時間", 
           y = "空氣品質指標") +
      theme_minimal() +
      theme(legend.title = element_blank(), # 隱藏圖例標題
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")  # 調整時間刻度
    print(pic)
}



vars_to_average <- c(
  "二氧化硫_ppb", "一氧化碳_ppm", "臭氧_ppb", "臭氧8小時移動平均_ppb",
  "懸浮微粒_μg_m3", "細懸浮微粒_μg_m3", "二氧化氮_ppb", 
  "氮氧化物_ppb", "一氧化氮_ppb","一氧化碳8小時移動平均_ppm", 
  "細懸浮微粒移動平均值_μg_m3", "懸浮微粒移動平均值_μg_m3",  "二氧化硫移動平均值_ppb"
)

# 按照測站名稱分組，計算變數平均

combined_data_list <- list()

# 使用回圈處理每一個縣市
for (city in city) {
  city_avg <- data_clean %>%
    filter(縣市 == city) %>%
    group_by(測站名稱) %>%
    summarise(across(all_of(vars_to_average), mean, na.rm = TRUE)) %>% 
    mutate(縣市 = city)
  
  # 將每個縣市的結果儲存到列表中
  combined_data_list[[city]] <- city_avg
}

# 合併所有縣市的計算結果
data_avg <- bind_rows(combined_data_list)

data_avg <- data_avg %>%
  mutate(區域 = case_when(
    縣市 %in% c("臺北市", "新北市", "基隆市", "新竹市", "桃園市", "新竹縣", "宜蘭縣") ~ "北部",
    縣市 %in% c("臺中市", "苗栗縣", "彰化縣", "南投縣", "雲林縣") ~ "中部",
    縣市 %in% c("高雄市", "臺南市", "嘉義市", "嘉義縣", "屏東縣", "澎湖縣") ~ "南部",
    縣市 %in% c("花蓮縣", "臺東縣") ~ "東部",
    縣市 %in% c("金門縣", "連江縣") ~ "福建",
    TRUE ~ "其他"  # 如果縣市不在上面任何一個分類中，則為 "其他"
  ))

# ==== normalized PCA ====
# 排除不適合 PCA 的變數
setwd('/Users/ray/Downloads/碩士班/113 上/應用多變量分析/期末報告2/PCA')
x <- data_avg %>%
  select(-c("測站名稱","縣市","區域")) %>% 
  mutate_all(~ as.numeric(as.character(.)))
View(x)
x.s <- x %>% as.matrix() %>% scale()
data.s <- data.frame(x.s) 
names(data.s) <- colnames(x)
View(data.s)
View(x.s)
n <- nrow(x.s)

e.s  = eigen((n - 1) * cov(x.s)/n)   
e1.s = e.s$values
PCscore = as.matrix(x.s) %*% e.s$vectors  # data multiplied by eigenvectors
e2.s = e.s$values/sum(e.s$values)
cumulated_proportion.s <- cumsum(e2.s)

e.s$vector[,1]
e.s
# 建立 data.frame
PC.var.s <- data.frame(
  eigenvalue = e1.s ,
  Proportion_of_Variance = e2.s,
  Cumulated_Proportion = cumulated_proportion.s
)
View(PC.var.s)
write.csv(PC.var.s %>% round(4),"PC.var.s.csv")

x.s.PCcoff <- data.frame(
  PC1=e.s$vector[,1],
  PC2=e.s$vector[,2],
  PC3=e.s$vector[,3]
)
View(x.s.PCcoff)


write.csv(x.s.PCcoff %>% round(4),"x.s.PCcoff.csv")
# 調整邊距以留出空間
region_colors <- c(
  "北部" = "#fa8072",  # 橙红色
  "中部" = "#FFD700",  # 绿色
  "南部" = "#3357FF",  # 蓝色
  "東部" = "#dc143c",  # 粉色
  "福建" = "#006400"   # 金色
)
region_pch<- c(
  "北部" = 1,  # 橙红色
  "中部" = 2,  # 绿色
  "南部" = 3,  # 蓝色
  "東部" = 4,  # 粉色
  "福建" = 5   # 金色
)
par(mfrow = c(1, 1))  

# 绘制散点图
plot(
  PCscore[, 1],  # 第一主成分
  PCscore[, 2],  # 第二主成分
  pch = region_pch[data_avg$區域],  # 点的样式根据区域
  col = region_colors[data_avg$區域],  # 点的颜色根据区域
  xlab = "PC1",
  ylab = "PC2",
  main = "First vs. Second PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = names(region_colors),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.5,  # 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.4  
)

# 绘制散点图
plot(
  PCscore[, 2],  # 第一主成分
  PCscore[, 3],  # 第三主成分
  pch = region_pch[data_avg$區域],  # 点的样式根据区域
  col = region_colors[data_avg$區域],  # 点的颜色根据区域
  xlab = "PC2",
  ylab = "PC3",
  main = "Second vs. Third PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = names(region_colors),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.5,  # 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.4  
)
# 绘制散点图
plot(
  PCscore[, 1],  # 第一主成分
  PCscore[, 3],  # 第二主成分
  pch = region_pch[data_avg$區域],  # 点的样式根据区域
  col = region_colors[data_avg$區域],  # 点的颜色根据区域
  xlab = "PC1",
  ylab = "PC3",
  main = "First vs. Third PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = names(region_colors),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.5,  # 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.4  
)


# plot of the eigenvalues
plot(e1.s, xlab = "Index", ylab = "Lambda", main = "Eigenvalues of S", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8)
points(3, e1.s[3], col = "red", pch = 16, cex = 1.5)

plot(cumulated_proportion.s, xlab = "Index", ylab = "Cumulated Proportion", main = "Cumulated Proportion", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8)
points(3, cumulated_proportion.s[3], col = "red", pch = 16, cex = 1.5)

m    = apply(as.matrix(x.s), 2, mean)
temp = as.matrix(x.s - matrix(m, n, ncol(x.s), byrow = T))
m
str(temp)
r    = temp %*% e.s$vectors
r    = cor(cbind(r, x.s))  # correlation between PCs and variables
View(r)

r12  = r[14:26, 1:2]
r23  = r[14:26, 2:3]
r13  = r[14:26, c(1,3)]
r12 <- cbind(r12, sum_of_squares = r12[, 1]^2 + r12[, 2]^2)
write.csv(r12 %>% round(4),"r12.csv")
r23 <- cbind(r23, sum_of_squares = r23[, 1]^2 + r23[, 2]^2)
write.csv(r23 %>% round(4),"r23.csv")
r13 <- cbind(r13, sum_of_squares = r13[, 1]^2 + r13[, 2]^2)
write.csv(r13 %>% round(4),"r13.csv")

View(r12)
View(r23)
View(r13)

ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Second PC", 
     main = "First PC vs Second PC", cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c(sapply(1:13,function(x) paste0("X",x)))
text(r12[,1:2], label,cex=1.2)

plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Second PC", ylab = "Third PC", 
     main = "Second PC vs Third PC", cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c(sapply(1:13,function(x) paste0("X",x)))
text(r23[,1:2], label,cex=1.2)

plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Third PC", 
     main = "First PC vs Third PC", cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c(sapply(1:13,function(x) paste0("X",x)))
text(r13[,1:2], label,cex=1.2)

# ==== FA ====
setwd('/Users/ray/Downloads/碩士班/113 上/應用多變量分析/期末報告2/FA')
x.s.cor <- cor(x.s)
View(x.s.cor)
write.csv(x.s.cor %>% round(4),"x.s.cor.csv")


# Maximum Likelihood Factor Analysis after varimax rotation
mlm  = factanal(x.s, 3, rotation = "none", covmat = x.s.cor)
load = mlm$loadings                           # estimated factor loadings
ld   = cbind(load[, 1], load[, 2], load[, 3])
var  = varimax(ld)                           # rotates the factor loadings matrixㄒ
var
load = var$loadings                           # estimated factor loadings after varimax
vl   = cbind(load[, 1], load[, 2], load[, 3])
com  = diag(vl %*% t(vl))                     # communalities are calculated
psi  = diag(x.s.cor) - diag(vl %*% t(vl))         # specific variances are calculated
tbl  = cbind(load[, 1], load[, 2], load[, 3], com, psi)
View(tbl)
write.csv(tbl %>% round(4),"Estimated factor loadings.csv")


colnames(data) = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", 
                   "X13") # rename variables
par(mfcol = c(1, 1))

# plot first factor against second
plot(load[, 1], load[, 2], type = "n", xlab = "F1", ylab = "F2", main = "Factors12 - theta12", 
     font.main = 1, cex.lab = 1.1, cex.axis = 1.1, cex.main = 1.4)
text(load[, 1], load[, 2], colnames(data), cex = 1.1)
abline(h = 0, v = 0)

# plot first factor against third
plot(load[, 1], load[, 3], type = "n", xlab = "F1", ylab = "F3", main = "Factors13 - theta13", 
     font.main = 1, cex.lab = 1.1, cex.axis = 1.1, cex.main = 1.4)
text(load[, 1], load[, 3], colnames(data), cex = 1.1)
abline(h = 0, v = 0)

# plot second factor against third
plot(load[, 2], load[, 3], type = "n", xlab = "F2", ylab = "F3", main = "Factors23 - theta23", 
     font.main = 1, cex.lab = 1.1, cex.axis = 1.1, cex.main = 1.4)
text(load[, 2], load[, 3], colnames(data), cex = 1.1)
abline(h = 0, v = 0)

par(mfcol = c(1, 1))
# Goodness of fit : Likelihood Ratio Test for k=3
# 輸入的參數：
# for (k in 1:6){ 
#   n <- nrow(x.s)  # 樣本數
#   p <- ncol(x.s)  # 變數個數
#   S <- x.s.cor  # 使用的相關矩陣
#   
#   mlm = factanal(x.s, factors = k, rotation = "none", covmat = x.s.cor)
#   
#   # 取得因子負荷矩陣 (L) 和特異變異數 (Psi)
#   loadings = mlm$loadings
#   # 計算特異變異數 Ψ，這裡假設是 1 減去所有負荷平方和
#   specific_variances = 1 - rowSums(loadings^2)
#   
#   # 步驟 2: 計算似然比檢定統計量
#   
#   # S 是樣本的協方差矩陣，這裡我們使用相關矩陣
#   S = x.s.cor
#   
#   # 計算 L * L^T + Ψ 的行列式
#   L_Lt_plus_Psi = loadings %*% t(loadings) + diag(specific_variances)
#   
#   # 計算似然比檢定統計量
#   LRT_stat =  log(det(L_Lt_plus_Psi) / det(S))
#   
#   # 步驟 3: 根據 Bartlett 修正計算自由度
#   df = (p - k)^2 - (p - k)
#   corrected_n = n - 1 - (2 * p + 4 * k + 5) / 6
#   
#   # 步驟 4: 計算 p 值
#   p_value = 1 - pchisq(LRT_stat * corrected_n, df)
#   
#   # 顯示結果
#   cat("k: ",k, "\n")
#   cat("Likelihood Ratio Test Statistic: ", LRT_stat * corrected_n, "\n")
#   cat("Degrees of Freedom: ", df, "\n")
#   # cat("Corrected n: ", corrected_n, "\n")
#   # cat("Statistic: ",LRT_stat * corrected_n,"\n")
#   
#   # 判斷是否拒絕假設
#   alpha = 0.05
#   if (p_value < alpha) {
#     cat("拒絕 H0 \n\n")
#   } else {
#     cat("不拒絕 H0 \n\n")
#   }
# }


# ==== clusting ====
setwd('/Users/ray/Downloads/碩士班/113 上/應用多變量分析/期末報告2/kmeans')
View(data_avg)
names(data_avg)

# 設定資料（移除非數值欄位）
data_for_kmeans <- data_avg[, !(names(data_avg) %in% c("測站名稱", "縣市", "區域"))]

# 計算不同群數的 WSS
set.seed(123)  # 設定隨機種子以保證結果可重現
max_clusters <- 10  # 設定最大群數
wss <- numeric(max_clusters)  # 用來儲存每個群數的 WSS

for (k in 1:max_clusters) {
  kmeans_result <- kmeans(data_for_kmeans, centers = k, nstart = 10)  # 每次執行 10 次 k-means
  wss[k] <- kmeans_result$tot.withinss  # 獲取群內平方和
}

# 繪製肘部圖
plot(1:max_clusters, wss, type = "b", pch = 1, frame = FALSE,
     xlab = "Number of Clusters", ylab = "WSS (Within-Cluster Sum of Squares)",
     main = "Elbow Method for Optimal Clusters")
points(6, wss[6], col = "red", pch = 19, cex = 1)


# 最佳群數
optimal_k <- 6
# 執行 k-means
final_kmeans <- kmeans(data_for_kmeans, centers = optimal_k, nstart = 10)
print(final_kmeans$centers)  # 每群的中心
View(final_kmeans$centers)
write.csv(final_kmeans$centers %>% round(4),"kmeans center.csv")
data_avg$clustering_result <- final_kmeans$cluster
View(data_avg)

region_colors <- c(
  "1" = "#fa8072",  # 橙红色
  "2" = "#FFD700",  # 绿色
  '3' = "#3357FF",  # 蓝色
  "4" = "#dc143c",  # 粉色
  "5" = "#006400",   # 金色
  '6' = "black"  # 金色
)
region_pch<- c(
  "1" = 1,  # 橙红色
  '2' = 2,  # 绿色
  '3' = 3,  # 蓝色
  '4' = 4,  # 粉色
  '5' = 5,  # 金色
  '6' = 6   # 金色
)
par(mfrow = c(1, 1))  

# 绘制散点图
plot(
  PCscore[, 1],  # 第一主成分
  PCscore[, 2],  # 第二主成分
  pch = region_pch[data_avg$clustering_result],  # 点的样式根据区域
  col = region_colors[data_avg$clustering_result],  # 点的颜色根据区域
  xlab = "PC1",
  ylab = "PC2",
  main = "First vs. Second PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = c("第一群","第二群","第三群","第四群","第五群","第六群"),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.3,# 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.7
)


plot(
  PCscore[, 2],  # 第一主成分
  PCscore[, 3],  # 第二主成分
  pch = region_pch[data_avg$clustering_result],  # 点的样式根据区域
  col = region_colors[data_avg$clustering_result],  # 点的颜色根据区域
  xlab = "PC2",
  ylab = "PC3",
  main = "Second vs. Third PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = c("第一群","第二群","第三群","第四群","第五群","第六群"),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.3,# 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.7
)

plot(
  PCscore[, 1],  # 第一主成分
  PCscore[, 3],  # 第二主成分
  pch = region_pch[data_avg$clustering_result],  # 点的样式根据区域
  col = region_colors[data_avg$clustering_result],  # 点的颜色根据区域
  xlab = "PC1",
  ylab = "PC3",
  main = "First vs. Third PC",
  cex.lab = 1.2,
  cex.axis = 1.2,
  cex.main = 1.8
)

# 添加说明图在标题下方主图上方
legend(
  x = "topright",  # 设置说明图的基准位置为顶部
  legend = c("第一群","第二群","第三群","第四群","第五群","第六群"),  
  col = region_colors,  # 说明图的颜色
  pch = region_pch,  # 说明图的点样式
  horiz = TRUE,  # 说明图水平排列
  bty = "n",  # 去除边框
  x.intersp = 0.3,# 点与文字的间距
  cex = 0.8,  # 文字大小
  text.width = 0.7
)



data_avg$clustering_result <- final_kmeans$cluster
View(data_avg)
clustering1 <- data_avg[data_avg$clustering_result == 1, ]
clustering2 <- data_avg[data_avg$clustering_result == 2, ]
clustering3 <- data_avg[data_avg$clustering_result == 3, ]
clustering4 <- data_avg[data_avg$clustering_result == 4, ]
clustering5 <- data_avg[data_avg$clustering_result == 5, ]
clustering6 <- data_avg[data_avg$clustering_result == 6, ]

cat('第1群: \n','縣市: ',clustering1$測站名稱,'\n')
cat('第2群: \n','縣市: ',clustering2$測站名稱,'\n')
cat('第3群: \n','縣市: ',clustering3$測站名稱,'\n')
cat('第4群: \n','縣市: ',clustering4$測站名稱,'\n')
cat('第5群: \n','縣市: ',clustering5$測站名稱,'\n')
cat('第6群: \n','縣市: ',clustering5$測站名稱,'\n')
table(data_avg$區域)
cat('第1區域群: \n')
table(clustering1$區域)
cat('第2區域群: \n')
table(clustering2$區域)
cat('第3區域群: \n')
table(clustering3$區域)
cat('第4區域群: \n')
table(clustering4$區域)
cat('第5區域群: \n')
table(clustering5$區域)
cat('第6區域群: \n')
table(clustering6$區域)

clu.result <- function(data,i){
  clustering_count <- table(data$區域)
  data_avg_count <- table(data_avg$區域)
  proportion <- clustering_count / data_avg_count[names(clustering_count)]
  cat(paste0('第',i,'群:'),' \n','縣市: ',data$測站名稱,'\n')
  print(clustering_count)
  print(proportion %>% round(4))
}
clu.result(clustering1,1)
clu.result(clustering2,2)
clu.result(clustering3,3)
clu.result(clustering4,4)
clu.result(clustering5,5)
clu.result(clustering6,6)

