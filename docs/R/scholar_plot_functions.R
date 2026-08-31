library(tidyverse)
library(ggplot2)
if(length(find.package("patchwork", quiet=TRUE)) == 0) {
  install.packages("patchwork")
}
library(patchwork)

make_scholar_hits_plot <- function(){
  scholar <- read_csv(file.path("data", "scholar_hits.csv"))
  scholar_hits <- scholar[scholar$type == "keywords",]
  scholar_hits$cumsum <- cumsum(scholar_hits$num_hits) # Create cumulative sum
  
  p <- ggplot(scholar_hits, aes(x = year, y = cumsum)) + 
    geom_col(fill = "#035C94") + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(breaks = c(2020, 2021, 2022, 2023, 2024, 2025, 2026)) +
    ylab("# Google Scholar mentions (cumulative)") + 
    xlab("")
  
  scholar_cites <- scholar[scholar$type == "flagship paper",]
  scholar_cites$cumsum <- cumsum(scholar_cites$num_hits) # Create cumulative sum
  
  q <- ggplot(scholar_cites, aes(x = year, y = cumsum)) + 
    geom_col(fill = "#035C94") + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(breaks = c(2020, 2021, 2022, 2023, 2024, 2025, 2026)) +
    ylab("# Flagship paper citations (cumulative)") + 
    xlab("")
  
  return(p + q)
}
