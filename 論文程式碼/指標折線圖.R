setwd('/Users/ray/Downloads/meeting/114.01.22/視覺化表格')
test.acc <- read.csv('test_acc.csv')
test.sen <- read.csv('test_sen.csv')
test.spe <- read.csv('test_spe.csv')
train.acc <- read.csv('train_acc.csv')
train.sen <- read.csv('train_sen.csv')
train.spe <- read.csv('train_spe.csv')

update_column_names <- function(df) {
  names(df)[names(df) == "ANN.4."] <- "ANN(4)"
  names(df)[names(df) == "ANN.4..ST"] <- "ANN(4).ST"
  names(df)[names(df) == "S1.ANN"] <- "S1-ANN"
  names(df)[names(df) == "S1.CBT"] <- "S1-CBT"
  names(df)[names(df) == "S1.ZIB"] <- "S1-ZIB"
  names(df)[names(df) == "S2.ANN"] <- "S2-ANN"
  names(df)[names(df) == "S2.CBT"] <- "S2-CBT"
  names(df)[names(df) == "S2.ZIB"] <- "S2-ZIB"
  return(df)
}

test.acc <- update_column_names(test.acc)
test.sen <- update_column_names(test.sen)
test.spe <- update_column_names(test.spe)
train.acc <- update_column_names(train.acc)
train.sen <- update_column_names(train.sen)
train.spe <- update_column_names(train.spe)

# View(train.spe)
# 載入必要的套件
library(ggplot2)
library(reshape2)
library(gridExtra)
library(patchwork)

# 定義繪圖函數
line.chart <- function(data, title, index, ylim) {
  long_Format <- reshape2::melt(data, id.vars = "n", 
                                variable.name = "Method", 
                                value.name = index)
  # 繪製折線圖
  p <- ggplot(long_Format, aes(x = n, y = .data[[index]], color = Method, group = Method, linetype = Method)) +
    geom_line(size = 0.7) +  # 繪製折線
    geom_point(size = 2.5) +   # 在頂點加上圓點
    labs(title = title, x = "n", y = index) +  # 添加標題與軸標籤
    theme_minimal() +  
    theme(
      plot.title = element_text(hjust = 0.5),  # 標題置中
      legend.position = "top",  # 圖例放在每張圖下方
      legend.direction = "horizontal",  # 圖例排列為橫向
      legend.title = element_blank(),  # 去除圖例的標題
      legend.text = element_text(size = 8),  # 調整圖例文字大小
      panel.background = element_rect(fill = "transparent", color = NA),  # 讓繪圖背景透明
      plot.background = element_rect(fill = "transparent", color = NA)  # 讓整張圖的背景透明
    ) +
    ylim(ylim) +
    xlim(300, 800)
  return(p)
}

pic.plot <- function(list){
  p1 <- line.chart(test.acc[,list] , "Testing Data - Accuracy", "Accuracy", c(0.5, 1))
  p2 <- line.chart(test.sen[,list] , "Testing Data - Sensitivity", "Sensitivity", c(0.2, 1))
  p3 <- line.chart(test.spe[,list] , "Testing Data - Specificity", "Specificity", c(0.6, 1))
  
  Test.plot <- (p1 | p2 | p3) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "top",
          plot.background = element_rect(fill = "transparent", color = NA))  # 設定透明背景)  # 將圖例放在最上方
  
  p4 <-line.chart(train.acc[,list] , "Training Data - Accuracy", "Accuracy", c(0.5, 1))
  p5 <-line.chart(train.sen[,list] , "Training Data - Sensitivity", "Sensitivity", c(0.2, 1))
  p6 <-line.chart(train.spe[,list] , "Training Data - Specificity", "Specificity", c(0.6, 1))
  
  Train.plot <- (p4 | p5 | p6) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "top",
          plot.background = element_rect(fill = "transparent", color = NA))  # 設定透明背景)  # 將圖例放在最上方
  print(Train.plot)
  print(Test.plot)
  return(list(Train.plot=Train.plot,Test.plot=Test.plot))
}


setwd('/Users/ray/Downloads/meeting/114.02.12')

OG <- c("n","ANN","CBT","ZIB")
ggsave("圖1.png", plot = pic.plot(OG)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖2.png", plot = pic.plot(OG)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

S1 <- c("n","S1-ANN","S1-CBT","S1-ZIB")
# pic.plot(S1)
ggsave("圖3.png", plot = pic.plot(S1)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖4.png", plot = pic.plot(S1)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

S2 <- c("n","S2-ANN","S2-CBT","S2-ZIB")
# pic.plot(S2)
ggsave("圖5.png", plot = pic.plot(S2)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖6.png", plot = pic.plot(S2)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

ANN.pic <- c("n","ANN","S1-ANN","S2-ANN")
# pic.plot(ANN.pic)
ggsave("圖7.png", plot = pic.plot(ANN.pic)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖8.png", plot = pic.plot(ANN.pic)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

CBT.pic <- c("n","CBT","S1-CBT","S2-CBT")
# pic.plot(CBT.pic)
ggsave("圖9.png", plot = pic.plot(CBT.pic)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖10.png", plot = pic.plot(CBT.pic)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

ZIB.pic <- c("n","ZIB","S1-ZIB","S2-ZIB")
# pic.plot(ZIB.pic)
ggsave("圖11.png", plot = pic.plot(ZIB.pic)$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖12.png", plot = pic.plot(ZIB.pic)$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

OGST <- c("n","ANN","S2-ANN","CBT","S2-CBT","ZIB","S2-ZIB")
# pic.plot(ZIB.pic)
ggsave("圖13.png", plot = pic.plot(OGST )$Train.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)
ggsave("圖14.png", plot = pic.plot(OGST )$Test.plot, bg = "transparent"
       , width = 21.93, height = 13.65, units = "cm", dpi = 300)

