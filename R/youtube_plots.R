library(tidyverse)
library(ggplot2)
library(patchwork)

make_yt_views_plot <- function(dat){
  p <- ggplot(dat, aes(x = Date, y = cumsum)) + 
    geom_line(color = "#035C94") + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    ylab("# Youtube views (cumulative)") + 
    xlab("")
  
  q <- ggplot(dat, aes(x = Date, y = Views)) + 
    geom_line(color = "#035C94") + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    ylab("# Youtube views") + 
    xlab("")
  
  return(q + p)
}
