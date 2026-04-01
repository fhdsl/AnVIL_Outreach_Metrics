library(tidyverse)
library(ggplot2)
library(plotly)

# ga_dimensions should be the in_data
make_session_cumsum_sessions <- function(in_data) {
  clean_dat <-
    in_data %>%
    mutate(date = as.POSIXct(paste0(year, "-", month, "-", day)),
           month_dt = floor_date(date, "month")) %>%
    group_by(website, month_dt) %>%
    summarize(daily = n()) %>%
    mutate(cumsum = cumsum(daily))
  
    p <- plot_ly(
      clean_dat,
      x = ~ month_dt,
      y = ~ cumsum,
      color = ~ website,
      mode = 'lines',
      text = ~ paste(website)
    ) %>% layout(
      showlegend = FALSE,
      xaxis = list(title = ''),
      yaxis = list(title = 'Web sessions (cumulative)')
    )
    
    return(p)
}


# ga_dimensions should be the in_data
make_session_monthly_sessions <- function(in_data) {
  clean_dat <-
    in_data %>%
    mutate(date = as.POSIXct(paste0(year, "-", month, "-", day)),
           month_dt = floor_date(date, "month")) %>%
    group_by(website, month_dt) %>%
    summarize(daily = n())
  
  p <- plot_ly(
    clean_dat,
    x = ~ month_dt,
    y = ~ daily,
    color = ~ website,
    mode = 'lines',
    text = ~ paste(website)
  ) %>% layout(
    showlegend = FALSE,
    xaxis = list(title = ''),
    yaxis = list(title = 'Web sessions')
  )
  
  return(p)
}


# ga_metrics should be the in_data
make_book_users_barchart <- function(in_data) {
  clean_dat <-
    in_data %>%
    select(website, totalUsers) %>%
    mutate(website = case_when(
      website == "AnVIL Book: Urban Genomics: Identifying population structure among feral pigeon populations" ~ "AnVIL Book: Urban Genomics",
      website == "AnVIL Book: Epigenetics on AnVIL: Nature vs Nurture" ~ "AnVIL Book: Epigenetics on AnVIL",
      website == "AnVIL Book Champions" ~ "AnVIL Champions",
      TRUE ~ website
    )) %>% 
    mutate(website = str_remove(website, "AnVIL Book: ")) %>% 
  mutate(website = fct_reorder(website, totalUsers, .desc = T))
  
  p <- plot_ly(
    clean_dat,
    x = ~ website,
    y = ~ totalUsers,
    color = ~ website,
    mode = 'bar'
  ) %>% layout(
    showlegend = TRUE,
    xaxis = list(title = '', showticklabels = FALSE),
    yaxis = list(title = 'Web users')
  )
  
  return(p)
}

