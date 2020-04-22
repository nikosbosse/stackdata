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

