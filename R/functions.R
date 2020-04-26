#' @title Make array for weighting by CRPS
#'
#' @description
#' Takes a data.frame as input and ouputs a correctly sized array for the 
#' predictions and true values to be scores with the stan model
#'
#' @param data a data.frame with the following entries: 
#' \itemize {
#'   \item model (the model used to generate the correspondig predictions)
#'   \item geography (the region for which predictions are generated)
#'   \item date (the date of corresponding )
#'   \item y_obs (the true observed value)
#'   \item y_pred (a predicted value)
#'   \item sample_nr (a numeric value indicating the sample number of the corresponding predicted value)
#' }
#' @return list with two elements, an array for the predictions and an array
#' for the true_values. 
#' @examples
#'
#'
#' @export
#' @references Missing
#'

create_arrays <- function (data) {
  
  # number of models
  models <- data$model %>%
    unique()
  K <- length(models)
  
  regions <- data$geography %>%
    unique() 
  R <- length(regions)
  
  S <- data$sample_nr %>% max()
  
  dates <- data$date %>%
    unique()
  
  T <- length(dates)
  
  predict_sample_arr <- array(NA, c(T, R, S, K))
  for (r in 1:R) {
    for (t in 1:T) {
      predict_sample_arr[t, r, , ] <- data %>% 
        dplyr::select(c(model, sample_nr, date, y_pred, geography)) %>%
        dplyr::filter(date == dates[t], geography == regions[r]) %>%
        pivot_wider(names_from = model, values_from = y_pred) %>%
        dplyr::select(all_of(models)) %>%
        as.matrix()
    }
  }
  
  y_arr <- array(NA, c(R, T))
  for (r in 1:R) {
    y_arr[r, ] <- data %>%
      dplyr::filter(geography %in% regions[r], 
                    sample_nr == 1, 
                    model == models[1]) %>%
      .$y_obs
  }
  
  return(list(prediction_array = predict_sample_arr, 
              true_value_array = y_arr, 
              K = K, 
              R = R, 
              T = T, 
              S = S))
}




#' @title Model weighting by CRPS
#'
#' @description
#' Model weights for stacking by CRPS. MISSING.
#'
#' @param 
#' @examples
#'
#' #Missing
#' @export
#' @references Missing
#'

stack_crps <- function(data, 
                       lambda = NULL, 
                       gamma = NULL, 
                       dirichlet_alpha = 1.001,
                       R, K, S, T) {
  
  if (is.null(lambda))
    for (t in 1:T)
      lambda[t] <- 2 - (1 - t / T)^2
    
    if (is.null(gamma))
      gamma <- array(rep(1 / R, R))
  
  arrays <- create_arrays(data)
  
  standata <- list(K = K,
                   R = R,
                   T = T,
                   S = S,
                   predict_sample_mat = arrays$prediction_array,
                   y = arrays$true_value_array,
                   lambda = lambda,
                   gamma = gamma,
                   dirichlet_alpha = dirichlet_alpha)
  
  # model <- stanmodels$stacking_crps     # use inside package
  model <- rstan::stan_model("stan/crps_test.stan")
  opt <- rstan::optimizing(model, data = standata)
  return(opt$par)
}






# ============================================================================ #
load_data_rt <- function(regions, 
                      horizon = 7,
                      split_date = as.Date("2020-04-08")) {
  h <- horizon
  
  #### load data
  ## load rt forecasts
  df <- vector("list", length(regions)) 
  forecast_rts <- lapply(seq_along(df), 
                         FUN = function(i) {
                           filename <- paste("data/", regions[i], 
                                             "/forecast_rts.rds", sep = "")
                           readRDS(here::here(filename))
                         })
  
  if (length(forecast_cases == 1)) {
    forecast_rts <- forecast_rts[[1]] %>%
      dplyr::filter(horizon == h) 
  } else {
    forecast_rts <- do.call(rbind, forecast_rts) %>%
      dplyr::filter(horizon == h) 
  }
  
  
  
  ## load true_data
  rt_timeseries <- readRDS(here::here("data/z_true_data/rt_timeseries.rds")) %>%
    dplyr::filter(timeseries %in% regions, 
                  date <= split_date) %>%
    dplyr::group_by(timeseries, date) %>%
    dplyr::summarise(rt = median(rt)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rt_obs = rt) 
  
  ## merge so we only have dates for which we have observed values and forecasts
  obs_and_pred <- dplyr::inner_join(forecast_rts, 
                                    rt_timeseries, 
                                    by = c("date", 
                                           "timeseries")) %>%
    dplyr::rename(geography = timeseries, 
                  sample_nr = sample, 
                  y_obs = rt_obs, 
                  y_pred = rt) %>%
    dplyr::group_by(obs_sample) %>%
    dplyr::mutate(sample_nr = (as.numeric(obs_sample) - 1) * 100 + sample_nr) %>%
    dplyr::ungroup()
  
  return(list(train_data = obs_and_pred))
  
}










load_data_cases <- function(regions, 
                            horizon = 7,
                            split_date = as.Date("2020-04-08")) {
  h <- horizon
  
  #### load data
  ## load rt forecasts
  df <- vector("list", length(regions)) 
  forecast_cases <- lapply(seq_along(df), 
                         FUN = function(i) {
                           filename <- paste("data/", regions[i], 
                                             "/forecast_cases.rds", sep = "")
                           readRDS(here::here(filename))
                         })
  
  if (length(forecast_cases) == 1) {
    forecast_cases <- forecast_cases[[1]]
  } else {
    forecast_cases <- do.call(rbind, forecast_rts) %>%
      dplyr::filter(horizon == h) 
  }
  
  
  ## load true_data
  case_timeseries <- readRDS(here::here("data/z_true_data/case_timeseries.rds")) %>%
    dplyr::filter(timeseries %in% regions, 
                  date <= split_date) %>%
    dplyr::group_by(timeseries, date) %>%
    dplyr::summarise(cases = median(cases)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(case_obs = cases) 
  
  ## merge so we only have dates for which we have observed values and forecasts
  obs_and_pred <- dplyr::inner_join(forecast_cases, 
                                    case_timeseries, 
                                    by = c("date", 
                                           "timeseries")) %>%
    dplyr::rename(geography = timeseries, 
                  sample_nr = sample, 
                  y_obs = case_obs, 
                  y_pred = cases) %>%
    dplyr::group_by(obs_sample) %>%
    dplyr::mutate(sample_nr = (as.numeric(obs_sample) - 1) * 100 + sample_nr) %>%
    dplyr::ungroup()
  
  return(list(train_data = obs_and_pred))
  
}








