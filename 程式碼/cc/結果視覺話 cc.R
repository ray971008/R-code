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
library(reshape2)


setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ANN')

ANN.train <- read.csv('ANN_trainMetric cc.csv')
ANN.test <- read.csv('ANN_testMetric cc.csv')

ANN.S.train <- read.csv('ANN_S2_trainMetric cc.csv')
ANN.S.test <- read.csv('ANN_S2_testMetric cc.csv')

setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/CBT')

CBT.train <- read.csv('CBT_trainMetric cc.csv')
CBT.test <- read.csv('CBT_testMetric cc.csv')

CBT.S.train <- read.csv('CBT_S2_trainMetric cc.csv')
CBT.S.test <- read.csv('CBT_S2_testMetric cc.csv')

setwd('/Users/ray/Downloads/meeting/結果/Customer Churn/ZIB')

ZIB.train <- read.csv('ZIB_trainMetric cc.csv')
ZIB.test <- read.csv('ZIB_testMetric cc.csv')

ZIB.S.train <- read.csv('ZIB_S2_trainMetric cc.csv')
ZIB.S.test <- read.csv('ZIB_S2_testMetric cc.csv')


train.list <- c("ANN.train", "ANN.S.train",
                "CBT.train", "CBT.S.train",
                "ZIB.train","ZIB.S.train")
test.list <- c("ANN.test", "ANN.S.test",
               "CBT.test",  "CBT.S.test",
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

head(acc.train)
head(acc.test)

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
  
  p <- ggplot(long_Format, aes(x = n, y = .data[[index]], 
                               color = Method, group = Method, linetype = Method)) +
    geom_line(size = 0.7) +
    labs( x = "執行次數", y = y.name) +
    theme_minimal() +  
    theme(
      plot.title = element_text(hjust = 0.5),
      # legend.position = "top",
      # legend.direction = "horizontal",
      # legend.title = element_blank(),
      # legend.text = element_text(size = 8),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.title.x = element_text(size = 9),
      axis.title.xy= element_text(size = 9)
    ) +
    ylim(ylim) +
    xlim(1, nrow(data))
  return(p)
}


boxplot.chart <- function(data, index, ylim = NULL) {

  long_Format <- reshape2::melt(data, variable.name = "Method", value.name = index)
  
  y.name <- case_when(
    index=="acc.train"~"準確度",
    index=="acc.test"~"準確度",
    index=="sen.train"~"敏感度",
    index=="sen.test"~"敏感度",
    TRUE~"特異度"
  )
  
  p <- ggplot(long_Format, aes(x = Method, y = .data[[index]], fill = Method)) +
    geom_boxplot() +
    labs(x = "模型", y = y.name) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.title.x = element_text(size = 9),
      axis.title.xy= element_text(size = 9)
    )
  
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)  # 保留離群值但調整顯示範圍
  }
  
  return(p)
}



var.list <- list(OG, S, ANN.list, CBT.list, ZIB.list)
var.names <- c("OG", "S", "ANN", "CBT", "ZIB")

i <- var.list[[2]]

name <- var.names[2]
p2 <- boxplot.chart(acc.train[, i],"acc.train")
p2

for (v in seq_along(var.list)) {
  i <- var.list[[v]]
  name <- var.names[v]
  setwd(paste0("/Users/ray/Desktop/指標視覺化 合併/",name))
  p1 <- line.chart(acc.train[, i], "acc.train", c(0.5, 1))
  p2 <- ci.pic.v(acc.train, i)
  
  p3 <- line.chart(sen.train[, i], "sen.train", c(0, 1))
  p4 <- ci.pic.v(sen.train, i)
  
  p5 <- line.chart(spe.train[, i], "spe.train", c(0.5, 1))
  p6 <- ci.pic.v(spe.train, i)
  
  row1 <- wrap_plots(p1, p2, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  row2 <- wrap_plots(p3, p4, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  row3 <- wrap_plots(p5, p6, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  
  row1_labeled <- ggdraw() +
    draw_plot(row1, 0, 0.05, 1, 0.95) +
    draw_label("(a)", x = 0.5, y = 0.035, size = 12)
  
  row2_labeled <- ggdraw() +
    draw_plot(row2, 0, 0.05, 1, 0.95) +
    draw_label("(b)", x = 0.5, y = 0.035, size = 12)
  
  row3_labeled <- ggdraw() +
    draw_plot(row3, 0, 0.05, 1, 0.95) +
    draw_label("(c)", x = 0.5, y = 0.035, size = 12)
  
  train.plot <- wrap_plots(row1_labeled, row2_labeled, row3_labeled, ncol = 1) +
    plot_layout(guides = "collect") &  # 收集圖例
    theme(
      legend.position = "top",
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  ggsave(paste0(name, "-train.png"), plot = train.plot, bg = "transparent",
         width = 21.93, height = 31.65, units = "cm", dpi = 300)
  
  p7 <- line.chart(acc.test[, i], "acc.test", c(0.5, 1))
  p8 <- ci.pic.v(acc.test, i)
  
  p9 <- line.chart(sen.test[, i], "sen.test", c(0, 1))
  p10 <- ci.pic.v(sen.test, i)
  
  p11 <- line.chart(spe.test[, i], "spe.test", c(0.5, 1))
  p12 <- ci.pic.v(spe.test, i)
  
  row4 <- wrap_plots(p7, p8, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  row5 <- wrap_plots(p9, p10, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  row6 <- wrap_plots(p11, p12, ncol = 2) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  
  row4_labeled <- ggdraw() +
    draw_plot(row4, 0, 0.05, 1, 0.95) +
    draw_label("(a)", x = 0.5, y = 0.035, size = 12)
  
  row5_labeled <- ggdraw() +
    draw_plot(row5, 0, 0.05, 1, 0.95) +
    draw_label("(b)", x = 0.5, y = 0.035, size = 12)
  
  row6_labeled <- ggdraw() +
    draw_plot(row6, 0, 0.05, 1, 0.95) +
    draw_label("(c)", x = 0.5, y = 0.035, size = 12)
  
  test.plot <- wrap_plots(row4_labeled, row5_labeled, row6_labeled, ncol = 1) & 
    theme(plot.background = element_rect(fill = "transparent", color = NA))
  
  ggsave(paste0(name, "-test.png"), plot = test.plot, bg = "transparent",
         width = 21.93, height = 31.65, units = "cm", dpi = 300)
}