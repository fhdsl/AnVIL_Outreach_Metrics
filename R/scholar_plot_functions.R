library(tidyverse)
library(ggplot2)

make_scholar_hits_plot <- function(){
  scholar_hits <- read_csv(file.path("data", "scholar_hits.csv"))
  scholar_hits$cumsum <- cumsum(scholar_hits$num_hits) # Create cumulative sum
  
  p <- ggplot(scholar_hits, aes(x = year, y = cumsum)) + 
    geom_col(fill = "#035C94") + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(breaks = c(2020, 2021, 2022, 2023, 2024, 2025, 2026)) +
    ylab("# Google Scholar mentions (cumulative)") + 
    xlab("")
  
  return(p)
}
