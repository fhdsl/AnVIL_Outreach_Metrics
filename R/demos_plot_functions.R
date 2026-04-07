library(tidyverse)
library(ggplot2)

make_demos_headcount_plot <- function(){
  demos <- read_csv(file.path("data", "demos_attendance.csv")) # Ultimately, it would be great to pull this from Google Sheets
  demos$cumsum <- cumsum(demos$headcount) # Create cumulative sum
  demos$date <- mdy(demos$date) # Convert to date format
  day(demos$date) <- 15 # Set the event date to the middle of the month for aesthetic plotting purposes
  
  p <- ggplot(demos, aes(x = date, y = cumsum)) + 
    geom_col(fill = "#035C94", just = 1) + theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_date(date_breaks = "month", 
                 date_labels = "%b %Y",
                 expand = expansion(mult = 0.02)) +
    ylab("# AnVIL Demo attendees (cumulative)") + 
    xlab("") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}
