## USING INITIAL BOX-OPENING TRACKING SHEET DATA

# packages, libraries, etc.
library(ggplot2)
library(tidyverse)

# import data from .csv

initial_data <- read.csv("box_data_initial.csv")

# plots of dates

dates_plot <- ggplot(initial_data) +
              geom_histogram(aes(appox.date),
                             binwidth = 10)
dates_plot

dates_conditions_plot <- ggplot(initial_data) +
                         geom_histogram(aes(appox.date),
                                        binwidth = 10) +
                         facet_grid(cols = vars(condition))
dates_conditions_plot

# removing some categories

limited_categories <- initial_data %>% 
  filter(category == "bodice" |
           category == "skirt" |
           category == "cape" |
           category == "coat" |
           category == "dress" |
           category == "jacket") %>% 
  filter(appox.date < 2000) %>% 
  mutate(approx.date = appox.date)


dates_conditions_categories_plot <- ggplot(limited_categories) +
                                    geom_bar(aes(approx.date, fill = condition)) +
                                    facet_wrap(vars(category)) +
                                    scale_fill_manual(values = c("E"= "#FEC108", "G" = "#00A6FB", "F" = "#0582CA", "P" = "#012538"),
                                                      na.value = "#EEF1EF")

dates_conditions_categories_plot


# HEATMAP
data <- initial_data %>% 
  mutate(approx.date = appox.date) %>% 
  select(-appox.date) %>% 
  filter(is.na(approx.date) == FALSE)

for (i in 1:nrow(data)) {
  if(data$approx.date[i] < 1870) {
    data$date.range[i] <- "Pre-1870"
  } else if (data$approx.date[i] >= 1870 & data$approx.date[i] < 1890) {
    data$date.range[i] <- "1870-1890"
  } else if (data$approx.date[i] >= 1890 & data$approx.date[i] < 1910) {
    data$date.range[i] <- "1890-1910"
  } else if (data$approx.date[i] >= 1910 & data$approx.date[i] < 1930) {
    data$date.range[i] <- "1910-1930"
  } else if (data$approx.date[i] >= 1930) {
    data$date.range[i] <- "Post-1930"
  }
}

heatmap_data <- data %>% 
  group_by(category, date.range) %>% 
  summarise(count = n()) 

ggplot(heatmap_data) +
  geom_tile(aes(category, date.range, fill = count))
