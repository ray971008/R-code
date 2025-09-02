library(dplyr)
setwd('/Users/ray/Downloads/meeting/結果/BCW/ZIB')
df <- read.table('ZIB OG BCW.lst', sep = "", header = FALSE, strip.white = TRUE, fill = TRUE)
df <- df[1:100,]
name <- c("i", "tr", paste0("V", 1:10),"delta",
           paste0("train.", c("acc","sen","spe")),paste0("test.", c("acc","sen","spe")))
colnames(df) <- name
View(df)
coff.matrix <- df %>% 
  select(paste0("V", 1:10)) %>% 
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

apply(coff.matrix, 2, mean) %>% round(4)

