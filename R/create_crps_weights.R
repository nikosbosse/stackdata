library(tidyverse)

source("R/functions.R")

#### define parameters to look at
regions <- list.files("data/")[6]
h <- 7 # horizon
split_date <- as.Date("2020-04-08") # split date into test and training date

# load data
data <- load_data(regions, h, split_date)$train_data

# find stacking weights
stack_crps(data)



scores_0_7 <- list(rt = rt_scores, case = case_scores) %>% 
  purrr::map_dfr( 
    ~ dplyr::filter(., horizon <= 7) %>% 
      summarise_scores("timeseries"), .id = "type")

scores_8_plus <- list(rt = rt_scores, case = case_scores) %>% 
  purrr::map_dfr( 
    ~ dplyr::filter(., horizon > 7) %>% 
      summarise_scores("timeseries"), .id = "type")


saveRDS(data, "~/Desktop/prepared_data_Austria.rds")
saveRDS(forecast_rts, "~/Desktop/forecast_rt_Austria.rds")
saveRDS(rt_timeseries, "~/Desktop/true_rt_Austria.rds")
