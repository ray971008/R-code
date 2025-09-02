#=============顯示出中文的方法================
library(patchwork)
library(showtext)
#font_families_google()
showtext_auto(enable = TRUE)
par(family = 'sans')
#=============視覺化================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
setwd('/Users/ray/Downloads/meeting/結果/BCW/ANN')

ANN.train <- read.csv('ANN_trainMetric BCW.csv')
ANN.test <- read.csv('ANN_testMetric BCW.csv')

ANN.S.train <- read.csv('ANN_S2_trainMetric BCW.csv')
ANN.S.test <- read.csv('ANN_S2_testMetric BCW.csv')

setwd('/Users/ray/Downloads/meeting/結果/BCW/CBT')

CBT.train <- read.csv('CBT_trainMetric BCW.csv')
CBT.test <- read.csv('CBT_testMetric BCW.csv')

CBT.S.train <- read.csv('CBT_S2_trainMetric BCW.csv')
CBT.S.test <- read.csv('CBT_S2_testMetric BCW.csv')

setwd('/Users/ray/Downloads/meeting/結果/BCW/ZIB')

ZIB.train <- read.csv('ZIB_trainMetric BCW.csv')
ZIB.test <- read.csv('ZIB_testMetric BCW.csv')

ZIB.S.train <- read.csv('ZIB_S2_trainMetric BCW.csv')
ZIB.S.test <- read.csv('ZIB_S2_testMetric BCW.csv')


train.list <- c("ANN.train", "ANN.S.train",
                "CBT.train", "CBT.S.train",
                "ZIB.train","ZIB.S.train")
test.list <- c("ANN.test", "ANN.S.test", 
               "CBT.test", "CBT.S.test", 
               "ZIB.test","ZIB.S.test")

names <- c("ANN","S-ANN","CBT","S-CBT","ZIB","S-ZIB")


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

ANN.list <-  c("ANN","S-ANN")
CBT.list <-  c("CBT","S-CBT")
ZIB.list <-  c("ZIB","S-ZIB")

OG <- c("ANN","CBT","ZIB")
S <- c("S-ANN","S-CBT","S-ZIB")

list <- c(ANN.list,CBT.list,ZIB.list,OG,S)

# 折線圖
line.chart <- function(data,index, ylim) {
  data$n <- 1:nrow(data)  # 加入 n
  long_Format <- reshape2::melt(data, id.vars = "n", 
                                variable.name = "Method", 
                                value.name = index)
  y.name <- case_when(
    index=="acc.train"~"準確度",
    index=="acc.test"~"準確度",
    index=="sen.train"~"敏感度",
    index=="sen.test"~"敏感度",
    TRUE~"特異度"
  )
  label <- case_when(
    index=="acc.train"~"(a)",
    index=="acc.test"~"(a)",
    index=="sen.train"~"(b)",
    index=="sen.test"~"(b)",
    TRUE~"(c)"
  )
  
  p <- ggplot(long_Format, aes(x = n, y = .data[[index]], 
                               color = Method, group = Method, linetype = Method)) +
    geom_line(size = 0.7) +
    labs( x = "執行次數\n" ,y = y.name) +
    theme_minimal() +  
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 40),  # x 軸標籤文字變小
      axis.title.y = element_text(size = 40),  # y 軸標籤文字變小
      axis.text.x = element_text(size = 30),  # x 軸標籤文字變小
      axis.text.y = element_text(size = 30),  # y 軸標籤文字變小


      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    ylim(ylim) +
    xlim(1, nrow(data))
  return(p)
}


# 箱型圖
boxplot.chart <- function(data, index, ylim = NULL) {
  
  long_Format <- reshape2::melt(data, variable.name = "Method", value.name = index)
  
  y.name <- case_when(
    index=="acc.train"~"準確度",
    index=="acc.test"~"準確度",
    index=="sen.train"~"敏感度",
    index=="sen.test"~"敏感度",
    TRUE~"特異度"
  )
  label <- case_when(
    index=="acc.train"~"(d)",
    index=="acc.test"~"(d)",
    index=="sen.train"~"(e)",
    index=="sen.test"~"(e)",
    TRUE~"(f)"
  )
  p <- ggplot(long_Format, aes(x = Method, y = .data[[index]], fill = Method)) +
    geom_boxplot(width = 0.5) +
    labs(x = "模型\n", y = y.name) +
    guides(fill = "none") +  # ⬅️ 強制移除 fill 對應的圖例
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      axis.title.x = element_text(size = 40),  # x 軸標籤文字變小
      axis.title.y = element_text(size = 40),  # y 軸標籤文字變小
      axis.text.x = element_text(size = 30),  # x 軸標籤文字變小
      axis.text.y = element_text(size = 30),  # y 軸標籤文字變小
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)  # 保留離群值但調整顯示範圍
  }
  
  return(p)
}


var.list <- list(OG, S, ANN.list, CBT.list, ZIB.list)
var.names <- c("OG", "S", "ANN", "CBT", "ZIB")

for (v in seq_along(var.list)) {
  i <- var.list[[v]]
  name <- var.names[v]
  setwd(paste0("/Users/ray/Desktop/指標視覺化 合併/",name))
  p1 <- line.chart(acc.train[, i], "acc.train", c(0.6, 1))
  p2 <- boxplot.chart(acc.train[, i], "acc.train")
  
  p3 <- line.chart(sen.train[, i], "sen.train", c(0.6, 1))
  p4 <- boxplot.chart(sen.train[, i], "sen.train")
  
  p5 <- line.chart(spe.train[, i], "spe.train", c(0.6, 1))
  p6 <- boxplot.chart(spe.train[, i], "spe.train")
  
  train.plot <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2) +
    plot_layout(guides = "collect") &  # 收集所有子圖圖例
    theme(
      legend.position = "top",  # 將圖例放最上面
      legend.title = element_blank(),  # 若不想顯示圖例標題
      legend.text = element_text(size = 25),  # 圖例文字大小（可調）
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  ggsave(paste0(name, "-train.png"), plot = train.plot, bg = "transparent",
         width = 21.93, height = 31.65, units = "cm", dpi = 300)
  
  p7 <- line.chart(acc.test[, i], "acc.test", c(0.6, 1))
  p8 <- boxplot.chart(acc.test[, i], "acc.test")

  p9 <- line.chart(sen.test[, i], "sen.test", c(0.6, 1))
  p10 <- boxplot.chart(sen.test[, i], "sen.test")

  p11 <- line.chart(spe.test[, i], "spe.test", c(0.6, 1))
  p12 <- boxplot.chart(spe.test[, i], "spe.test")
  

  test.plot <- wrap_plots(p7,p8,p9,p10,p11,p12, ncol = 2) +
    plot_layout(guides = "collect") &  # 收集所有子圖圖例
    theme(
      legend.position = "top",  # 將圖例放最上面
      legend.title = element_blank(),  # 若不想顯示圖例標題
      legend.text = element_text(size = 25),  # 圖例文字大小（可調）
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  ggsave(paste0(name, "-test.png"), plot = test.plot, bg = "transparent",
         width = 21.93, height = 31.65, units = "cm", dpi = 300)

}

