library(dplyr)
library(ggplot2)
library(patchwork)

setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ANN')

ANN.train <- read.csv('ANN_trainMetric cc.csv')
ANN.test <- read.csv('ANN_testMetric cc.csv')
ANN.S1.train <- read.csv('ANN_S1_trainMetric cc.csv')
ANN.S1.test <- read.csv('ANN_S1_testMetric cc.csv')
ANN.S2.train <- read.csv('ANN_S2_trainMetric cc.csv')
ANN.S2.test <- read.csv('ANN_S2_testMetric cc.csv')

setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/CBT')

CBT.train <- read.csv('CBT_trainMetric cc.csv')
CBT.test <- read.csv('CBT_testMetric cc.csv')
CBT.S1.train <- read.csv('CBT_S1_trainMetric cc.csv')
CBT.S1.test <- read.csv('CBT_S1_testMetric cc.csv')
CBT.S2.train <- read.csv('CBT_S2_trainMetric cc.csv')
CBT.S2.test <- read.csv('CBT_S2_testMetric cc.csv')

setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ZIB')

ZIB.train <- read.csv('ZIB_trainMetric cc.csv')
ZIB.test <- read.csv('ZIB_testMetric cc.csv')
ZIB.S1.train <- read.csv('ZIB_S1_trainMetric cc.csv')
ZIB.S1.test <- read.csv('ZIB_S1_testMetric cc.csv')
ZIB.S2.train <- read.csv('ZIB_S2_trainMetric cc.csv')
ZIB.S2.test <- read.csv('ZIB_S2_testMetric cc.csv')


train.list <- c("ANN.train", "ANN.S1.train", "ANN.S2.train",
                "CBT.train", "CBT.S1.train", "CBT.S2.train",
                "ZIB.train","ZIB.S1.train","ZIB.S2.train")
test.list <- c("ANN.test", "ANN.S1.test", "ANN.S2.test",
               "CBT.test", "CBT.S1.test", "CBT.S2.test",
               "ZIB.test","ZIB.S1.test","ZIB.S2.test")

names <- c("ANN","ANN-S1","ANN-S2","CBT","CBT-S1","CBT-S2","ZIB","ZIB-S1","ZIB-S2")


# 將資料的 acc sen spe 合併
get_metric_df <- function(obj_list, index, col_names) {
  df <- do.call(cbind, lapply(obj_list, function(x) get(x)[index]))
  colnames(df) <- col_names
  return(df)
}

acc.train <- get_metric_df(train.list, 1, names)
sen.train <- get_metric_df(train.list, 2, names)
spe.train <- get_metric_df(train.list, 3, names)

acc.test  <- get_metric_df(test.list, 1, names)
sen.test  <- get_metric_df(test.list, 2, names)
spe.test  <- get_metric_df(test.list, 3, names)

head(acc.train)
head(acc.test)

ANN.list <-  c("ANN","ANN-S1","ANN-S2")
CBT.list <-  c("CBT","CBT-S1","CBT-S2")
ZIB.list <-  c("ZIB","ZIB-S1","ZIB-S2")

OG <- c("ANN","CBT","ZIB")
S1 <- c("ANN-S1","CBT-S1","ZIB-S1")
S2 <- c("ANN-S2","CBT-S2","ZIB-S2")

list <- c(ANN.list,CBT.list,ZIB.list,OG,S1,S2)

# 折線圖
line.chart <- function(data,index, ylim) {
  data$n <- 1:nrow(data)  # 加入 n
  long_Format <- reshape2::melt(data, id.vars = "n", 
                                variable.name = "Method", 
                                value.name = index)
  p <- ggplot(long_Format, aes(x = n, y = .data[[index]], 
                               color = Method, group = Method, linetype = Method)) +
    geom_line(size = 0.7) +
    labs( x = "", y = "") +
    theme_minimal() +  
    theme(
      plot.title = element_text(hjust = 0.5),
      # legend.position = "top",
      # legend.direction = "horizontal",
      # legend.title = element_blank(),
      # legend.text = element_text(size = 8),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    ylim(ylim) +
    xlim(1, nrow(data))
  return(p)
}

# 信賴區間 橫的

# 計算信賴區間函數
ci_function <- function(data) {
  mean_val <- mean(data)
  ci_low <- mean_val - 1.96 * sd(data) / sqrt(length(data))  # 95% 信賴區間下限
  ci_high <- mean_val + 1.96 * sd(data) / sqrt(length(data)) # 95% 信賴區間上限
  return(c(mean = mean_val, ci_low = ci_low, ci_high = ci_high))
}

ci.pic <- function(data, list,xlim=NULL) {
  # 計算每個模型的信賴區間
  ci <- apply(data, 2, ci_function)
  ci <- t(ci)  # 轉置
  ci_df <- as.data.frame(ci)
  colnames(ci_df) <- c("mean", "ci_low", "ci_high")  # 設置列名
  ci_df$Model <- rownames(ci)
  selected_ci_df <- ci_df[ci_df$Model %in% list, ]
  
  pic <- ggplot(selected_ci_df, aes(x = mean, y = Model, color = Model)) +  # 為每個模型指定顏色
    geom_point(size = 2) +  # 使用點來表示每個模型的 mean 值
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.1) +  # 顯示信賴區間的誤差條
    scale_y_discrete(limits = rev(list)) +  # 自訂順序，從上到下畫
    labs(x = substitute(data), y = "") + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"  # 不顯示圖例
    )
  if (!is.null(xlim)) {
    pic <- pic + xlim(xlim)
  }
  return(pic)
}

# 信賴區間 直的

ci.pic.v <- function(data, list, ylim = NULL) {
  # 計算每個模型的信賴區間
  ci <- apply(data, 2, ci_function)
  ci <- t(ci)  # 轉置
  ci_df <- as.data.frame(ci)
  colnames(ci_df) <- c("mean", "ci_low", "ci_high")
  ci_df$Model <- rownames(ci)
  selected_ci_df <- ci_df[ci_df$Model %in% list, ]
  
  pic <- ggplot(selected_ci_df, aes(x = Model, y = mean, color = Model)) +  # x 軸改成 Model, y 軸是 mean
    geom_point(size = 2) +  # 點表示平均值
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1) +  # 垂直誤差條
    scale_x_discrete(limits = list) +  # 保持順序
    labs(x = "", y = "") +  # 改 y 標籤
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  if (!is.null(ylim)) {
    pic <- pic + ylim(ylim)  # 注意：這裡是 ylim，不是 xlim
  }
  
  return(pic)
}


var.list <- list(OG, S1, S2, ANN.list, CBT.list, ZIB.list)
var.names <- c("OG", "S1", "S2", "ANN", "CBT", "ZIB")

for (v in seq_along(var.list)) {
  i <- var.list[[v]]
  name <- var.names[v]
  setwd(paste0("/Users/ray/Downloads/meeting/結果/Customer Churn/指標視覺化/",name))
  p1 <- line.chart(acc.train[, i], "acc.train", c(0.6, 1))
  p2 <- ci.pic.v(acc.train, i)
  p1.2 <- wrap_plots(p1, p2, ncol = 2)
  print(p1.2)
  ggsave(paste0(name, "-acc.train.png"), plot = p1.2, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
  
  # 下面同理
  p3 <- line.chart(sen.train[, i], "sen.train", c(0, 1))
  p4 <- ci.pic.v(sen.train, i)
  p3.4 <- wrap_plots(p3, p4, ncol = 2)
  print(p3.4)
  ggsave(paste0(name, "-sen.train.png"), plot = p3.4, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
  
  p5 <- line.chart(spe.train[, i], "spe.train", c(0.6, 1))
  p6 <- ci.pic.v(spe.train, i)
  p5.6 <- wrap_plots(p5, p6, ncol = 2)
  print(p5.6)
  ggsave(paste0(name, "-spe.train.png"), plot = p5.6, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
  
  p7 <- line.chart(acc.test[, i], "acc.test", c(0.6, 1))
  p8 <- ci.pic.v(acc.test, i)
  p7.8 <- wrap_plots(p7, p8, ncol = 2)
  print(p7.8)
  ggsave(paste0(name, "-acc.test.png"), plot = p7.8, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
  
  p9 <- line.chart(sen.test[, i], "sen.test", c(0, 1))
  p10 <- ci.pic.v(sen.test, i)
  p9.10 <- wrap_plots(p9, p10, ncol = 2)
  print(p9.10)
  ggsave(paste0(name, "-sen.test.png"), plot = p9.10, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
  
  p11 <- line.chart(spe.test[, i], "spe.test", c(0.6, 1))
  p12 <- ci.pic.v(spe.test, i)
  p11.12 <- wrap_plots(p11, p12, ncol = 2)
  print(p11.12)
  ggsave(paste0(name, "-spe.test.png"), plot = p11.12, bg = "transparent",
         width = 21.93, height = 13.65, units = "cm", dpi = 300)
}

