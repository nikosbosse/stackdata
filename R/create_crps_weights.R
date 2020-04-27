library(tidyverse)

source("R/functions.R")
# devtools::install_github("nikosbosse/stackr)
# library(stackr)

#### define parameters to look at
regions <- list.files("data/")[3]
h <- 2 # horizon
split_date <- as.Date("2020-04-08") # split date into test and training date

# load data
# data <- load_data_rt(regions, h, split_date)$train_data
traindata <- load_data_cases(regions, h, split_date)$train_data
testdata <- load_data_cases(regions, h, split_date)$test_data

# find stacking weights
R = traindata %>% pull(geography) %>% unique %>% length
K = traindata %>% pull(model) %>% unique %>% length
S = traindata %>% pull(sample_nr) %>% unique %>% length
T = traindata %>% pull(date) %>% unique %>% length

# w <- stack_crps(traindata, R = R, K = K, S = S, T = T)
# Alternative: use the stackr package function
stackr::stack_crps(traindata)


## plotting
traindata %>%
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



## make mixture model
# need to replace this by Yuling's MCMC method
mixture <- stackr::create_sampled_mixture(testdata, w)

#### score results
score_df <- rbind(testdata, mixture)

score_df %>%
  group_by(model, date) %>%
  summarise(crps = scoringutils::crps(unique(y_obs), 
                                      t(as.vector(y_pred)))) %>%
  group_by(model) %>%
  summarise(crps = mean(crps))


## plotting
score_df %>%
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


  