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
  
  if (length(forecast_rts) == 1) {
    forecast_rts <- forecast_rts[[1]] %>%
      dplyr::filter(horizon == h) 
  } else {
    forecast_rts <- do.call(rbind, forecast_rts) %>%
      dplyr::filter(horizon == h) 
  }
  
  
  
  ## load true_data
  rt_timeseries <- readRDS(here::here("data/z_true_data/rt_timeseries.rds")) %>%
    dplyr::filter(timeseries %in% regions) %>%
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
  
  train_data <- obs_and_pred %>%
    dplyr::filter(date <= split_date)
  
  test_data <- obs_and_pred %>%
    dplyr::filter(date > split_date)
  
  return(list(train_data = train_data, test_data = test_data))
  
}



## generate mixture ensembles using Sequential Quasi Monte Carlo 
## return a length-S vector that is weighted col-mixture of individual_draws.
mixture_ensembles <- function(individual_draws,  
                              weight, 
                              random_seed = 1, 
                              permutation = TRUE)
{
	set.seed(random_seed)
  S <- dim(individual_draws)[1]
	K <- dim(individual_draws)[2]
	
	if (permutation == TRUE) {
	  individual_draws <- individual_draws[sample(1:S), ]	 # random permutation of draws
	}
	   
	round_with_preserved_sum <- function(x) {
	  target_sum = sum(x)
	  
	  ints <- as.integer(x)
	  int_sum <- sum(ints)
	  remainder <- target_sum - int_sum
	  
	  decimals <- x - ints
	  order <- order(decimals, decreasing = T)
	  i <- 1
	  while(remainder > 0) {
	    ints[order[i]] <- ints[order[i]] + 1
	    remainder <- remainder - 1
	  }
	  
	  return(ints)
	}
	
	integer_part <- round_with_preserved_sum(S * weight)
	
	integer_part_index <- c(0,cumsum(integer_part))
	existing_draws <- integer_part_index[K+1]
	
	
	if (existing_draws < S){
  	remaining_draws <- S - existing_draws
  	remaining_assignment <- sample(1:K, 
  	                               remaining_draws, 
  	                               prob = weight, 
  	                               replace = F)
  	integer_part[remaining_assignment] <- integer_part[remaining_assignment]+1
	}
	
	mixture_vector <- rep(NA, S)
	for(k in 1:K) {
	  # skip if no draws to make
	  if (integer_part[k] == 0) 
	    next()
	  mixture_vector[(1 + integer_part_index[k]):integer_part_index[k + 1]] <- 
	    individual_draws[1:integer_part[k], k]
	}
	return(mixture_vector)
}
	
#==============================================================================
## generate mixture ensembles without Sequential Quasi Monte Carlo 
## probably what you call bootstrap
mixture_ensembles_simple = function (individual_draws,  
                                     weight, 
                                     random_seed = 1, 
                                     permutation=TRUE)
{
	set.seed(random_seed)
	S <- dim(individual_draws)[1]
	K <- dim(individual_draws)[2]
	
	if (permutation == TRUE) {
	  individual_draws=individual_draws[sample(1:S), ]	 # random permutation of draws
	}
		
	assignment=sample(1:K, S, prob = weight, replace = TRUE)
	return( sapply(c(1:S), function(i){individual_draws[sample(1:S, 1),assignment[i]]}) )
}

## example:
# K=3
# S=1000
# individual_draws=matrix(NA, S, K)
# for (i in 1:K)
# individual_draws[,i]=rnorm(S, i,0)
# weight=c(0.25,0.25,0.5)
# table(mixture_ensembles(individual_draws=individual_draws,  weight=weight, random_seed=1 , permutation=TRUE))
# table(mixture_ensembles_simple(individual_draws=individual_draws,  weight=weight, random_seed=3 , permutation=TRUE))
# weight=c(0.1111,0.1111,0)
# weight[3]=1-sum(weight)
# table(mixture_ensembles(individual_draws=individual_draws,  weight=weight, random_seed=1 , permutation=TRUE))

#==============================================================================
## interval score of a single model  eqn.43 of Gneiting and Raftery: Proper Scoring Rules
## input: observation y; predictive draws pred_draws; desireved coverage: confidence 
##  negatively oriented (the smaller the better)
interval_score <- function(y, pred_draws, confidence=0.95) {
	S <- length(pred_draws)
	alpha <- 1-confidence
	alpha_lower <- alpha/2  # quantile_value
	alpha_upper <- 1-alpha/2 
	l= quantile(pred_draws, alpha_lower, names=FALSE)
	u= quantile(pred_draws, alpha_upper, names=FALSE)
	return(u-l+2/alpha* (l-y)* (l>y) + 2/alpha*(y-u) *(y>u))
}

## example:
interval_score(0,rnorm(1000,0,1),0.95)
interval_score(0,rnorm(1000,0,0.1),0.95)

#==============================================================================
## interval score of a mixture
## has non-discrete nature
## may be optimized using grid approximation in low-D
interval_score_mixture = function(y, 
                                  individual_draws,
                                  weight,  
                                  confidence=0.95) {
  
	pred_draws <- mixture_ensembles(individual_draws=individual_draws,  weight=weight)
	
	return( interval_score(y=y , pred_draws=pred_draws,  confidence=0.95))
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
    forecast_cases <- forecast_cases[[1]] %>%
    dplyr::filter(horizon == h) 
  } else {
    forecast_cases <- do.call(rbind, forecast_rts) %>%
      dplyr::filter(horizon == h) 
  }
  
  
  ## load true_data
  case_timeseries <- readRDS(here::here("data/z_true_data/case_timeseries.rds")) %>%
    dplyr::filter(timeseries %in% regions) %>%
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
  
  
  train_data <- obs_and_pred %>%
    dplyr::filter(date <= split_date)
  
  test_data <- obs_and_pred %>%
    dplyr::filter(date > split_date)
  
  return(list(train_data = train_data, test_data = test_data))
  
}








