library(tidyverse)

source("R/functions.R")

#### define parameters to look at
regions <- list.files("data/")[2]
h <- 2 # horizon
split_date <- as.Date("2020-04-08") # split date into test and training date

# load data
data <- load_data(regions, h, split_date)$train_data

# find stacking weights
R = data %>% pull(geography) %>% unique %>% length
K = data %>% pull(model) %>% unique %>% length
S = data %>% pull(sample_nr) %>% unique %>% length
T = data %>% pull(date) %>% unique %>% length
stack_crps(data, R = R, K = K, S = S, T = T)

data %>%
  group_by(model, date) %>%
  summarize(median = median(y_pred),
            y_obs = median(y_obs),
            ql = quantile(y_pred, 0.1),
            qh = quantile(y_pred, 0.9)) %>%
  ggplot() +
    geom_ribbon(aes(date, ymin = ql, ymax = qh), alpha = 0.25) +
    geom_point(aes(date, median)) +
    geom_point(aes(date, y_obs), color = "red") +
    facet_wrap(~ model) + 
  theme(text = element_text(family = 'Sans Serif'),
        legend.position = "bottom")
  