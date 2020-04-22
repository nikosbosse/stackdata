# Packages ----------------------------------------------------------------

require(EpiSoon)
require(EpiNow)
require(dplyr)
require(bsts)
require(fable)
require(fabletools)
require(feasts)
require(furrr)
require(future)
require(future.apply)
require(urca)
require(dplyr)
require(ggplot2)
require(patchwork)
require(cowplot)

# Get timeseries ----------------------------------------------------------

data_samples <- 100

## Extract the Rt and case timeseries as produced by EpiNow
# timeseries <- EpiNow::get_timeseries("../covid-global/national/")
# 
# 
# ## Extract rt timeseries and format
# rt_timeseries <- timeseries$rt %>% 
#   dplyr::filter(rt_type %in% "nowcast", 
#                 type %in% "nowcast") %>% 
#   dplyr::mutate(sample = as.numeric(sample)) %>% 
#   dplyr::group_by(region, date, sample) %>% 
#   dplyr::mutate(rt_sample = 1:dplyr::n()) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(timeseries = region, rt = R, date, rt_sample, sample) %>% 
#   dplyr::group_by(timeseries, date, sample) %>% 
#   ## Each cases dataset has multiple Rt samples linked to it
#   ## Take mean here to prevent duplicates
#   dplyr::summarise(rt = mean(rt, na.rm = TRUE)) %>% 
#   dplyr::ungroup()
# 
# ## Extract cases timeseries and format
# case_timeseries <- timeseries$incidence %>% 
#   dplyr::filter(import_status %in% "local") %>% 
#   dplyr::mutate(sample = as.numeric(sample)) %>% 
#   dplyr::select(timeseries = region, cases, date, sample)
# 
# 
# ## Sample timeseries
# samples <- sample(1:max(rt_timeseries$sample), data_samples)
# 
# rt_timeseries <- rt_timeseries %>% 
#   dplyr::filter(sample %in% samples)
# 
# case_timeseries <- case_timeseries %>% 
#   dplyr::filter(sample %in% samples)
# 
# ## Save samples
# saveRDS(rt_timeseries, "data/rt_timeseries.rds")
# 
# saveRDS(case_timeseries, "data/case_timeseries.rds")

rt_timeseries <- readRDS("data/rt_timeseries.rds")
case_timeseries <- readRDS("data/case_timeseries.rds")

rt_timeseries <- rt_timeseries %>%
  dplyr::filter(timeseries == "Afghanistan")
case_timeseries <- case_timeseries %>%
  dplyr::filter(timeseries == "Afghanistan")


# Define a serial interval ------------------------------------------------

## In EpiNow we use a sampled serial interval - this is not yet supported 
## by EpiSoon and so instead here we define the mean serial interval
covid_serial_interval <- rowMeans(EpiNow::covid_serial_intervals)

# Define models for evaluation --------------------------------------------

## Inverse variance weighted
baseline_ensemble <- function(...) {
  EpiSoon::fable_model(model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), fable::NAIVE(y),
                                                             fable::RW(y ~ drift()), cmbn_args = list(weights = "inv_var")), ...)
}

## Mean ensemble
baseline_mean_ensemble <- function(...) {
  EpiSoon::fable_model(model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), fable::NAIVE(y),
                                                             fable::RW(y ~ drift()),
                                                             cmbn_args = list(weights = "equal")), ...)
}

## Last 14 days of data
current_ensemble <- function(y, ...) {
  y <- y[max(1, length(y) - 14):length(y)]
  EpiSoon::fable_model(y = y, model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), 
                                                                    fable::NAIVE(y), fable::RW(y ~ drift()),
                                                                    cmbn_args = list(weights = "inv_var")), ...)
}

## Last 7 days of data
current_7_ensemble <- function(y, ...) {
  y <- y[max(1, length(y) - 7):length(y)]
  EpiSoon::fable_model(y = y, model = fabletools::combination_model(fable::ARIMA(y), fable::ETS(y), 
                                                                    fable::NAIVE(y), fable::RW(y ~ drift()),
                                                                    cmbn_args = list(weights = "inv_var")), ...)
}

## Inverse variance weighted
no_arima_ensemble <- function(...) {
  EpiSoon::fable_model(model = fabletools::combination_model(fable::ETS(y), fable::NAIVE(y),
                                                             fable::RW(y ~ drift()), 
                                                             cmbn_args = list(weights = "inv_var")), ...)
}

saveRDS(no_arima_ensemble, here::here("forecast/model-choice/data/ensemble.rds"))


models <- list( 
  "Naive" = function(...){EpiSoon::fable_model(model = fable::NAIVE(y), ...)},
  "ARIMA" = function(...){EpiSoon::fable_model(model = fable::ARIMA(y), ...)},
  "ETS" = function(...){EpiSoon::fable_model(model = fable::ETS(y), ...)},
  "Drift" = function(...){EpiSoon::fable_model(model = fable::RW(y ~ drift()), ...)}
)

# Set up parallelisation --------------------------------------------------

future::plan("multiprocess")

# Evaluate models ---------------------------------------------------------


## Run iterative evaluation
forecast_eval <- EpiSoon::compare_timeseries(obs_rts = rt_timeseries,
                                             obs_cases = case_timeseries,
                                             models = models,
                                             horizon = 21,
                                             samples = 100,
                                             serial_interval = covid_serial_interval, 
                                             return_raw = TRUE)




# Load in evaluation ------------------------------------------------------

## Forecasts
# summarised
forecast_rts <- forecast_eval$forecast_rts  
saveRDS(forecast_rts, here::here("data/forecast_rts.rds")) 

forecast_cases <- forecast_eval$forecast_cases   
saveRDS(forecast_cases, here::here("data/forecast_cases.rds"))

#raw
forecast_rts_raw <- forecast_eval$raw_rt_forecast
saveRDS(forecast_rts_raw, here::here("data/forecast_rts_raw.rds")) 

forecast_cases_raw <- forecast_eval$raw_case_forecast
saveRDS(forecast_rts_raw, here::here("data/forecast_rts_raw.rds")) 

#scores
forecast_rt_scores <- forecast_eval$rt_scores 
saveRDS(forecast_rt_scores, here::here("data/forecast_rt_scores.rds")) 

forecast_case_scores <- forecast_eval$case_scores
saveRDS(forecast_case_scores, here::here("data/forecast_case_scores.rds")) 


library(data.table)
#raw
fwrite(forecast_cases_raw, "data/forecast_cases_raw.csv")
fwrite(forecast_rts_raw, "data/forecast_rts_raw.csv")

#summarised
fwrite(forecast_rts, "data/forecast_rts.csv")
fwrite(forecast_cases, "data/forecast_cases.csv")

#scores
fwrite(forecast_rt_scores, "data/forecast_rt_scores.csv")
fwrite(forecast_case_scores, "data/forecast_case_scores.csv")


## save all regions
a <- 5
saveRDS(a, here::here("data/test/test.rds"))

regions <- forecast_rts$timeseries %>% unique()

for (region in regions) {
  storepath <- paste("data/region/", region, sep = "")
  dir.create(file.path(storepath), showWarnings = FALSE)
  
  ## raw 
  tmp <- forecast_rts_raw %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_rts.rds", sep = "")
  saveRDS(tmp, here::here(filename))
  
  tmp <- forecast_cases_raw %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_cases.rds", sep = "")
  saveRDS(tmp, here::here(filename))
  
  
  ## summarised   
  tmp <- forecast_rts %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_rts_summarised.rds", sep = "")
  saveRDS(tmp, here::here(filename))
  
  tmp <- forecast_cases %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_cases_summarised.rds", sep = "")
  saveRDS(tmp, here::here(filename))
  
  
  ## scored   
  tmp <- forecast_rt_scores %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_rts_scored.rds", sep = "")
  saveRDS(tmp, here::here(filename))
  
  tmp <- forecast_case_scores %>%
    dplyr::filter(timeseries == region)
  
  filename <- paste(storepath, "/forecast_cases_scored.rds", sep = "")
  saveRDS(tmp, here::here(filename))
}
