# packages, libraries, etc.
library(ggplot2)
library(tidyverse)

# import data from .csv

initial_data <- read.csv("box_data_initial.csv")
final_data <- read.csv("BCCA_all.csv")
colors <- hcl.colors(11, "Blue-Red 3")

# plots of dates

collection <- final_data %>% 
  filter(box_new != "DEACC" & box_new != "CHRIS" & box_new != "COSTUMES")

dates_plot <- ggplot(collection) +
              geom_histogram(aes(date),
                             binwidth = 10,
                             color = colors[11],
                             fill = "#dbdbdb") +
              labs(title = "Approximate Dates of Object Creation") +
              xlab("Date Range") +
              ylab("Number of Objects") +
              theme_minimal() +
              theme(text = element_text(family = "sans",
                                        color = "black"),
                    plot.title = element_text(face = "bold"),
                    axis.title = element_text(face = "italic")
                    )
# dates_plot

categories_counts <- collection %>% 
  count(category)

limit_categories <- categories_counts %>% 
  filter(n > 3) 

collection_limited_categories <- collection %>% 
  filter(category %in% limit_categories$category) %>% 
  group_by(category) %>% 
  summarize(number = n())

categories_bar <- ggplot(collection_limited_categories) +
  geom_bar(aes(x = reorder(category, -number),
               y = number),
           stat = "identity",
           color = colors[11],
           fill = "#dbdbdb") +
  labs(title = "Objects by Category") +
  xlab("Category") +
  ylab("Count") +
  theme_minimal() +
  theme(text = element_text(family = "sans",
                            color = "black"),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8)
  )

# categories_bar

## Pre-1870 categores

categories_pre1870 <- collection %>% 
  filter(date <= 1870)  %>% 
  group_by(category) %>% 
  summarize(number = n())

categories_pre1870_bar <- ggplot(categories_pre1870) +
  geom_bar(aes(x = reorder(category, -number),
               y = number),
           stat = "identity",
           color = colors[11],
           fill = "#dbdbdb") +
  labs(title = "Objects by Category",
       subtitle = "Limited to Objects Dated to 1870 and Earlier") +
  xlab("Category") +
  ylab("Count") +
  theme_minimal() +
  theme(text = element_text(family = "sans",
                            color = "black"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold.italic"),
        axis.title = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8)
  )

categories_pre1870_bar

# dates_conditions_plot <- ggplot(initial_data) +
#                          geom_histogram(aes(appox.date),
#                                         binwidth = 10) +
#                          facet_grid(cols = vars(condition))
# dates_conditions_plot

# removing some categories

# limited_categories <- initial_data %>% 
#   filter(category == "bodice" |
#            category == "skirt" |
#            category == "cape" |
#            category == "coat" |
#            category == "dress" |
#            category == "jacket") %>% 
#   filter(appox.date < 2000) %>% 
#   mutate(approx.date = appox.date)
# 
# 
# dates_conditions_categories_plot <- ggplot(limited_categories) +
#                                     geom_bar(aes(approx.date, fill = condition)) +
#                                     facet_wrap(vars(category)) +
#                                     scale_fill_manual(values = c("E"= "#FEC108", "G" = "#00A6FB", "F" = "#0582CA", "P" = "#012538"),
#                                                       na.value = "#EEF1EF")
# 
# dates_conditions_categories_plot


# HEATMAP
data <- collection %>% 
  filter(is.na(date) == FALSE)

for (i in 1:nrow(data)) {
  if(data$date[i] < 1870) {
    data$date.range[i] <- "Pre-1870"
  } else if (data$date[i] < 1890) {
    data$date.range[i] <- "1870-1890"
  } else if (data$date[i] < 1910) {
    data$date.range[i] <- "1890-1910"
  } else if (data$date[i] < 1930) {
    data$date.range[i] <- "1910-1930"
  } else {
    data$date.range[i] <- "Post-1930"
  }
}

heatmap_data <- data %>% 
  filter(category %in% limit_categories$category & category != "children's clothes") %>% 
  group_by(category, date.range) %>% 
  summarise(count = n()) %>% 
  mutate(ordered_dates = factor(date.range, ordered = TRUE, levels = c("Pre-1870",
                                                                       "1870-1890",
                                                                       "1890-1910",
                                                                       "1910-1930",
                                                                       "Post-1930")
                                )
                         ) %>% 
  arrange(desc(ordered_dates))
      

date_category_heatmap <- ggplot(heatmap_data) +
  geom_tile(aes(category, ordered_dates, fill = count)) +
  scale_fill_gradient(low = colors[7], 
                      high = colors[11], 
                      space = "Lab",
                      na.value = "dbdbdb"
                      ) +
  labs(title = "Occurences of Date-Category Pairs",
       fill = "Count") +
  xlab("Category") +
  ylab("Date Range") +
  theme_minimal() +
  theme(text = element_text(family = "sans",
                            color = "black"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold.italic"),
        axis.title = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0.8)
  )

date_category_heatmap
