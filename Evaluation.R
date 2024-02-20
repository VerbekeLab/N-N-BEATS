library(data.table)
library(openxlsx)
library(magrittr)
library(Mcomp) # load M3 data
library(stringr)
library(DescTools)
library(scoringRules) # CRPS
library(miscTools)
#install.packages("miscTools")

########################## Actuals ##########################
## Load (raw) actuals for M3 and M4
## We collect actuals for both the test sets and training sets
## Training actuals can be used for MASE/RMSSE-type scaling later on

#############################################################
dataset_subset <- 'Monthly' #'Other'
dataset_subset_indicator <- 'M' #'O'
FH <- 18
FL <- 6
FO <- 13
dataset_selection <- 'M4' #'M3'
evaluation_mode <- 'test' #'validation'
M4_path <- 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/M4DataSet/'
results_path <- 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/'
#############################################################

if (dataset_selection == 'M3'){
  
  MS <- subset(M3, dataset_subset)
  # item_id M1385/N2786 --> one observation in test set is -1200 in M3 data via raw source vs 1200 in M3 via R package
  
  MS_actuals_train <- data.frame()
  max_l <- 0
  max_n <- 0
  for (i in c(1:length(MS))) {
    if (length(MS[[i]]$x) > max_l) {max_l <- length(MS[[i]]$x)}
    if (MS[[i]]$n > max_n) {max_n <- MS[[i]]$n}
  }
  if (max_l != max_n) print('TS length error')
  
  MS_actuals_test <- data.frame()
  
  counter <- 1
  for (i in c(1:length(MS))) {
    ts <- MS[[i]]
    
    MS_actuals_train[i,1] <- MS_actuals_test[i,1] <- ts$st
    MS_actuals_train[i,2] <- MS_actuals_test[i,2] <- ts$sn
    MS_actuals_train[i,3:c(max_l+2)] <- c(ts$x, rep(NA, max_l-length(ts$x)))
    MS_actuals_test[i,3:c(FH+2)] <- ts$xx
    
    counter <- counter+1
  }
  
  MS_actuals_train <- data.table(MS_actuals_train)
  MS_actuals_test <- data.table(MS_actuals_test)
  
  setnames(MS_actuals_train, names(MS_actuals_train), c('item_id1', 'item_id2', 1:max_l))
  setnames(MS_actuals_test, names(MS_actuals_test), c('item_id1', 'item_id2', 1:FH))
  MS_actuals_train[, item_id := as.numeric(str_remove(item_id1, dataset_subset_indicator))]
  MS_actuals_test[, item_id := as.numeric(str_remove(item_id1, dataset_subset_indicator))]
  MS_actuals_train[, item_id1 := NULL]
  MS_actuals_train[, item_id2 := NULL]
  MS_actuals_test[, item_id1 := NULL]
  MS_actuals_test[, item_id2 := NULL]
  setcolorder(MS_actuals_train, c('item_id'))
  setorder(MS_actuals_train, item_id)
  setcolorder(MS_actuals_test, c('item_id'))
  setorder(MS_actuals_test, item_id)
  
  rm(MS, ts)
  
} else if (dataset_selection == 'M4') {
  
  MS_actuals_train <- fread(paste0(M4_path, dataset_subset, '-train.csv'))
  MS_actuals_test <- fread(paste0(M4_path, dataset_subset, '-test.csv'))
  
  max_l <- ncol(MS_actuals_train)-1
  
  setnames(MS_actuals_train, names(MS_actuals_train), c('item_id', 1:max_l))
  setnames(MS_actuals_test, names(MS_actuals_test), c('item_id', 1:FH))
  MS_actuals_train[, item_id := as.numeric(str_remove(item_id, dataset_subset_indicator))]
  MS_actuals_test[, item_id := as.numeric(str_remove(item_id, dataset_subset_indicator))]
  
}

MS_actuals_all <- merge.data.table(MS_actuals_train, MS_actuals_test, by = 'item_id')
# MS_actuals_all[,-1] %>% rowSums(na.rm = T) %>% sum() # 47924674261
setnames(MS_actuals_all, names(MS_actuals_all), c('item_id', 1:c(ncol(MS_actuals_train)-1+FH)))
MS_actuals_all <- melt.data.table(MS_actuals_all, id.vars = 'item_id')
# MS_actuals_all$value %>% sum(na.rm = T) # 47924674261
MS_actuals_all <- MS_actuals_all[!is.na(value)]
setorder(MS_actuals_all, variable)
MS_actuals_all[, variable := c(1:.N) , by = list(item_id)]
MS_actuals_all[, variable := as.numeric(variable)]
MS_actuals_all[, diff_sq := (value - shift(value))^2, by = item_id]

MS_actuals_eval <- data.table(MS_actuals_all)
MS_actuals_eval[, max_obs := max(variable), by = list(item_id)]
if (evaluation_mode == 'test'){
  MS_actuals_eval <- MS_actuals_eval[variable <= max_obs]
} else{ # evaluation_mode == 'validation'
  MS_actuals_eval <- MS_actuals_eval[variable < (max_obs-FH+1)]
}
MS_actuals_eval[, max_obs := max(variable), by = list(item_id)]
MS_actuals_eval <- MS_actuals_eval[variable >= (max_obs-FH+1)]
MS_actuals_eval[, min_obs := (min(variable)-1), by = list(item_id)]
MS_actuals_eval[, max_obs := NULL]
MS_actuals_eval[, forecast_horizon := variable - min_obs]
MS_actuals_eval[, variable := NULL]
MS_actuals_eval[, min_obs := NULL]
MS_actuals_eval[, diff_sq := NULL]
MS_actuals_eval[, fc_origin := 1]

MS_A <- data.table()
for (origin in c(1:FO)) {
  MS_A_fc_origin <- MS_actuals_eval[forecast_horizon %in% c(origin:(origin+(FL-1)))]
  MS_A_fc_origin[, fc_origin := origin]
  MS_A_fc_origin[, forecast_horizon := (forecast_horizon - origin + 1)]
  
  n_obs_remove <- FH - (origin-1)
  MS_actuals_all_subset <- data.table(MS_actuals_all)
  MS_actuals_all_subset[, max_obs := (max(variable) - n_obs_remove), by = list(item_id)]
  MS_actuals_all_subset <- MS_actuals_all_subset[variable <= max_obs]
  
  MS_dfs <- MS_actuals_all_subset[, list(discount_factor_sq = mean(diff_sq, na.rm = T)), by = item_id]
  MS_A_fc_origin <- merge.data.table(MS_A_fc_origin, MS_dfs, by = 'item_id')
  
  MS_dfs <- MS_actuals_all_subset[, list(discount_factor_abs = mean(sqrt(diff_sq), na.rm = T)), by = item_id]
  MS_A_fc_origin <- merge.data.table(MS_A_fc_origin, MS_dfs, by = 'item_id')
  
  MS_A <- rbind(MS_A, MS_A_fc_origin)
}
setorder(MS_A, item_id, fc_origin, forecast_horizon)

MS_A_C <- data.table() # cumulative actuals
for (origin in c(1:FO)) {
  MS_A_fc_origin <- MS_actuals_eval[forecast_horizon %in% c(origin:(origin+(FL-1)))]
  MS_A_fc_origin[, fc_origin := origin]
  MS_A_fc_origin[, forecast_horizon := (forecast_horizon - origin + 1)]
  
  MS_A_C_fc_origin <- dcast.data.table(MS_A_fc_origin, item_id + fc_origin ~ forecast_horizon)
  
  MS_A_C_fc_origin <- as.data.frame(MS_A_C_fc_origin)
  for (L in c(2:FL)){
    MS_A_C_fc_origin[toString(L)] <- MS_A_C_fc_origin[toString(L-1)] + MS_A_C_fc_origin[toString(L)]
  }
  MS_A_C_fc_origin <- as.data.table(MS_A_C_fc_origin)
  MS_A_C_fc_origin <- melt.data.table(MS_A_C_fc_origin, id.vars = c('item_id', 'fc_origin'), variable.name = 'forecast_horizon')
  
  n_obs_remove <- FH - (origin-1)
  MS_actuals_all_subset <- data.table(MS_actuals_all)
  MS_actuals_all_subset[, max_obs := (max(variable) - n_obs_remove), by = list(item_id)]
  MS_actuals_all_subset <- MS_actuals_all_subset[variable <= max_obs]
  
  MS_dfs <- MS_actuals_all_subset[, list(discount_factor_sq = mean(diff_sq, na.rm = T)), by = item_id]
  MS_A_C_fc_origin <- merge.data.table(MS_A_C_fc_origin, MS_dfs, by = 'item_id')
  
  MS_dfs <- MS_actuals_all_subset[, list(discount_factor_abs = mean(sqrt(diff_sq), na.rm = T)), by = item_id]
  MS_A_C_fc_origin <- merge.data.table(MS_A_C_fc_origin, MS_dfs, by = 'item_id')
  
  MS_A_C <- rbind(MS_A_C, MS_A_C_fc_origin)
}
setorder(MS_A_C, item_id, fc_origin, forecast_horizon)

########################## Evaluation forecasts DT ##########################
n_methods <- 10
n_items <- unique(MS_A$item_id)
MS_F <- matrix(NA, nrow = length(n_items), ncol = 1 + n_methods) %>% data.table()
setnames(MS_F, c('ID', 
                 'ETS',
                 'DeepAR',
                 'N-N-BEATS', 
                 'N-N-BEATS-C',
                 'N-N-BEATS-S-KL low', 'N-N-BEATS-S-KL high',
                 'N-N-BEATS-S-W2 low', 'N-N-BEATS-S-W2 high',
                 'N-N-BEATS-S-RMSC low', 'N-N-BEATS-S-RMSC high'
                 ))
MS_F[, ID := unique(MS_A$item_id)]
setorder(MS_F, ID)

MS_SCRPS <- data.table(MS_F)
MS_SPL90 <- data.table(MS_F)
MS_RMSSE <- data.table(MS_F)
MS_KL <- data.table(MS_F)
MS_SW2 <- data.table(MS_F)
MS_RMSSC <- data.table(MS_F)

########################## Evaluation cumulative forecasts DT ##########################
n_methods <- 5
n_items <- unique(MS_A_C$item_id)
lead_time <- FL
MS_F_C <- matrix(NA, nrow = length(n_items)*lead_time, ncol = 2 + n_methods) %>% data.table()
setnames(MS_F_C, c('ID', 'L',
                   'ETS-C1', 'ETS-C2', 'ETS-C3',
                   'DeepAR',
                   'N-N-BEATS-C'))

MS_F_C[, ID := rep(n_items, each = lead_time)]
MS_F_C[, L := c(rep(1:lead_time, length(n_items)))]
setorder(MS_F_C, ID, L)

MS_SCRPS_C <- data.table(MS_F_C)
MS_SPL90_C <- data.table(MS_F_C)
MS_RMSSE_C <- data.table(MS_F_C)

########################## Evaluation metrics and functions ##########################
RMSSE <- function(actuals, forecasts, discount_factor){
  sqrt(mean((actuals - forecasts)^2/discount_factor))
}

RMSSC <- function(forecasts, forecasts_previous, discount_factor){
  sqrt(mean((forecasts - forecasts_previous)^2/discount_factor, na.rm = T))
}

SCRPS <- function(actual, mean_forecast, sd_forecast, discount_factor){
  crps_norm(actual, mean_forecast, sd_forecast)/discount_factor
}

KL <- function(mean_forecast, sd_forecast, mean_forecast_previous, sd_forecast_previous){
  var_forecast = sd_forecast^2
  var_forecast_previous = sd_forecast_previous^2
  KL1 = 0.5 * (var_forecast/var_forecast_previous + (mean_forecast-mean_forecast_previous)^2/var_forecast_previous - 1 + log(var_forecast_previous/var_forecast))
  KL2 = 0.5 * (var_forecast_previous/var_forecast + (mean_forecast_previous-mean_forecast)^2/var_forecast - 1 + log(var_forecast/var_forecast_previous))
  min(mean(0.5 * KL1 + 0.5 * KL2, na.rm = T), 30)
}

SPL90 <- function(actual, mean_forecast, sd_forecast, tau_values = c(0.05, 0.95), discount_factor){
  PLosses <- matrix(0, nrow = length(tau_values), ncol = length(mean_forecast))
  counter <- 1
  for (tau in tau_values){
    quantile_predictions <- mean_forecast + qnorm(tau) * sd_forecast
    PLoss <- ifelse(actual < quantile_predictions, 
                    (1-tau) * (quantile_predictions-actual),
                    tau * (actual-quantile_predictions))
    PLosses[counter,] <- PLoss
    counter <- counter+1
  }
  # print(PLosses)
  # print(rowSums(PLosses)/length(mean_forecasts))
  mean(PLosses)/discount_factor
}

SW2 <- function(mean_forecast, sd_forecast, mean_forecast_previous, sd_forecast_previous, discount_factor){
  sqrt(mean(((mean_forecast-mean_forecast_previous)^2 + sd_forecast^2 + sd_forecast_previous^2 - 2*sd_forecast*sd_forecast_previous)/discount_factor, na.rm = T))
}

METHOD_LOAD <- function(datafolder, mean_f, sd_f){
  method_files <- list.files(datafolder)
  method_files <- paste0(datafolder, method_files)
  method_multiple_forecasts <- lapply(method_files, fread) %>% rbindlist()
  method_multiple_forecasts <- method_multiple_forecasts[type %in% c(mean_f, sd_f)]
  return(method_multiple_forecasts)
}

METHOD_ENSEMBLE <- function(method_multiple_forecasts){
  # method_forecasts <- method_multiple_forecasts[, lapply(.SD, mean), by = list(item_id, fc_origin, type), .SDcols = as.character(c(1:FL))]
  method_forecasts_mean <- method_multiple_forecasts[type == 'mean_forecast', 
                                                     lapply(.SD, mean), 
                                                     by = list(item_id, fc_origin), 
                                                     .SDcols = as.character(c(1:FL))]
  method_forecasts_mean[, type := 'mean_forecast']
  method_forecasts_sd <- method_multiple_forecasts[type == 'sd_forecast', 
                                                   lapply(.SD, function(x){sqrt(mean(x^2))}),
                                                   by = list(item_id, fc_origin), 
                                                   .SDcols = as.character(c(1:FL))]
  method_forecasts_sd[, type := 'sd_forecast']
  method_forecasts <- rbind(method_forecasts_mean, method_forecasts_sd)
  return(method_forecasts)
}

METHOD_EVAL_PREP <- function(method_forecasts, mode){
  method_forecasts <- melt.data.table(method_forecasts,
                                      id.vars = c('item_id', 'fc_origin', 'type'),
                                      variable.name = 'forecast_horizon',
                                      value.name = 'forecast')
  method_forecasts[, forecast_horizon := as.numeric(forecast_horizon)]
  setorder(method_forecasts, item_id, fc_origin, forecast_horizon)
  method_forecasts <- dcast.data.table(method_forecasts, 
                                       item_id + fc_origin + forecast_horizon ~ type,
                                       value.var = 'forecast')
  if (mode == 'stability'){
    method_forecasts[mean_forecast < 0, mean_forecast := 0]
    # stability calculation
    method_forecasts[, mean_forecast_previous := shift(mean_forecast, (FL-1))]
    method_forecasts[, sd_forecast_previous := shift(sd_forecast, (FL-1))]
    method_forecasts[fc_origin == 1, mean_forecast_previous := NA]
    method_forecasts[forecast_horizon == FL, mean_forecast_previous := NA]
    method_forecasts[fc_origin == 1, sd_forecast_previous := NA]
    method_forecasts[forecast_horizon == FL, sd_forecast_previous := NA]
    # actuals
    method_forecasts[, actual := MS_A$value]
    method_forecasts[, discount_factor_sq := MS_A$discount_factor_sq]
    method_forecasts[, discount_factor_abs := MS_A$discount_factor_abs]
  } else if(mode == 'cumulative'){
    method_forecasts[cmean_forecast < 0, cmean_forecast := 0]
    # actuals
    method_forecasts[, actual := MS_A_C$value]
    method_forecasts[, discount_factor_sq := MS_A_C$discount_factor_sq]
    method_forecasts[, discount_factor_abs := MS_A_C$discount_factor_abs]
  }
  return(method_forecasts)
}

METHOD_EVAL <- function(method_forecasts_eval_prep, method_name){
  method_forecasts_item_origin <- method_forecasts_eval_prep[, list(SCRPS = SCRPS(actual, mean_forecast, sd_forecast, unique(discount_factor_abs)),
                                                                    SPL90 = SPL90(actual, mean_forecast, sd_forecast, tau_values = c(0.05, 0.95), unique(discount_factor_abs)),
                                                                    RMSSE = RMSSE(actual, mean_forecast, unique(discount_factor_sq)),
                                                                    KL = KL(mean_forecast, sd_forecast, mean_forecast_previous, sd_forecast_previous),
                                                                    SW2 = SW2(mean_forecast, sd_forecast, mean_forecast_previous, sd_forecast_previous, unique(discount_factor_sq)),
                                                                    RMSSC = RMSSC(mean_forecast, mean_forecast_previous, unique(discount_factor_sq))), 
                                                             by = list(fc_origin, item_id)]
  method_forecasts_item <- method_forecasts_item_origin[, list(SCRPS = mean(SCRPS),
                                                               SPL90 = mean(SPL90),
                                                               RMSSE = mean(RMSSE),
                                                               KL = mean(KL, na.rm = T),
                                                               SW2 = mean(SW2, na.rm = T),
                                                               RMSSC = mean(RMSSC, na.rm = T)), 
                                                        by = list(item_id)]
  setorder(method_forecasts_item, item_id)
  MS_SCRPS[, (method_name) := method_forecasts_item$SCRPS]
  MS_SPL90[, (method_name) := method_forecasts_item$SPL90]
  MS_RMSSE[, (method_name) := method_forecasts_item$RMSSE]
  MS_KL[, (method_name) := method_forecasts_item$KL]
  MS_SW2[, (method_name) := method_forecasts_item$SW2]
  MS_RMSSC[, (method_name) := method_forecasts_item$RMSSC]
}

METHOD_EVAL_C <- function(method_forecasts_eval_prep, method_name){
  # option 1
  method_forecasts_item_horizon_origin <- method_forecasts_eval_prep[, list(SCRPS = SCRPS(actual, cmean_forecast, csd_forecast, unique(discount_factor_abs)),
                                                                            SPL90 = SPL90(actual, cmean_forecast, csd_forecast, tau_values = c(0.05, 0.95), unique(discount_factor_abs)),
                                                                            RMSSE = RMSSE(actual, cmean_forecast, unique(discount_factor_sq))),
                                                                     by = list(item_id, forecast_horizon, fc_origin)]
  # calculate average cumulative error per period - correct mean for lead time!
  method_forecasts_item_horizon <- method_forecasts_item_horizon_origin[, list(SCRPS = mean(SCRPS)/forecast_horizon,
                                                                               SPL90 = mean(SPL90)/forecast_horizon,
                                                                               RMSSE = mean(RMSSE)/forecast_horizon),
                                                                        by = list(item_id, forecast_horizon)]
  # # option 2 - same results as option 1
  # # calculate average cumulative error per period - correct scaling factor for lead time!
  # method_forecasts_item_horizon_origin <- method_forecasts_eval_prep[, list(SCRPS = SCRPS(actual, cmean_forecast, csd_forecast, unique(discount_factor_abs)*forecast_horizon),
  #                                                                           SPL90 = SPL90(actual, cmean_forecast, csd_forecast, tau_values = c(0.05, 0.95), unique(discount_factor_abs)*forecast_horizon),
  #                                                                           RMSSE = RMSSE(actual, cmean_forecast, unique(discount_factor_sq)*forecast_horizon^2)),
  #                                                                    by = list(item_id, forecast_horizon, fc_origin)]
  # method_forecasts_item_horizon <- method_forecasts_item_horizon_origin[, list(SCRPS = mean(SCRPS),
  #                                                                              SPL90 = mean(SPL90),
  #                                                                              RMSSE = mean(RMSSE)), 
  #                                                                       by = list(item_id, forecast_horizon)]
  setorder(method_forecasts_item_horizon, item_id, forecast_horizon)
  MS_SCRPS_C[, (method_name) := method_forecasts_item_horizon$SCRPS]
  MS_SPL90_C[, (method_name) := method_forecasts_item_horizon$SPL90]
  MS_RMSSE_C[, (method_name) := method_forecasts_item_horizon$RMSSE]
}

########################## Forecast methods stability ##########################
# 
# ETS #####
MS_ETS <- fread(paste0(results_path,
                       paste0(dataset_selection, dataset_subset_indicator,
                              '_ETS_probabilistic.csv')))
MS_ETS <- MS_ETS[!(type %in% 'sd_insample')]
MS_ETS <- METHOD_EVAL_PREP(MS_ETS, 'stability')
METHOD_EVAL(MS_ETS, 'ETS')

# DEEPAR #####
datafolder <- paste0(results_path, 'DEEPAR_32/')
MS_DEEPAR <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_DEEPAR <- METHOD_ENSEMBLE(MS_DEEPAR)
MS_DEEPAR <- METHOD_EVAL_PREP(MS_DEEPAR, 'stability')
METHOD_EVAL(MS_DEEPAR, 'DeepAR')

# NNBEATS #####
datafolder <- paste0(results_path, 'NNBEATS/')
MS_NNBEATS <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATS <- METHOD_ENSEMBLE(MS_NNBEATS)
MS_NNBEATS <- METHOD_EVAL_PREP(MS_NNBEATS, 'stability')
METHOD_EVAL(MS_NNBEATS, 'N-N-BEATS')

# NNBEATSC #####
datafolder <- paste0(results_path, 'NNBEATSC/')
MS_NNBEATSC <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSC <- METHOD_ENSEMBLE(MS_NNBEATSC)
MS_NNBEATSC <- METHOD_EVAL_PREP(MS_NNBEATSC, 'stability')
METHOD_EVAL(MS_NNBEATSC, 'N-N-BEATS-C')

# NNBEATSS_KL_low #####
datafolder <- paste0(results_path, 'NNBEATSS_KL_low/')
MS_NNBEATSS_KL_low <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_KL_low <- METHOD_ENSEMBLE(MS_NNBEATSS_KL_low)
MS_NNBEATSS_KL_low <- METHOD_EVAL_PREP(MS_NNBEATSS_KL_low, 'stability')
METHOD_EVAL(MS_NNBEATSS_KL_low, 'N-N-BEATS-S-KL low')

# NNBEATSS_KL_high #####
datafolder <- paste0(results_path, 'NNBEATSS_KL_high/')
MS_NNBEATSS_KL_high <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_KL_high <- METHOD_ENSEMBLE(MS_NNBEATSS_KL_high)
MS_NNBEATSS_KL_high <- METHOD_EVAL_PREP(MS_NNBEATSS_KL_high, 'stability')
METHOD_EVAL(MS_NNBEATSS_KL_high, 'N-N-BEATS-S-KL high')

# NNBEATSS_W2_low #####
datafolder <- paste0(results_path, 'NNBEATSS_W2_low/')
MS_NNBEATSS_W2_low <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_W2_low <- METHOD_ENSEMBLE(MS_NNBEATSS_W2_low)
MS_NNBEATSS_W2_low <- METHOD_EVAL_PREP(MS_NNBEATSS_W2_low, 'stability')
METHOD_EVAL(MS_NNBEATSS_W2_low, 'N-N-BEATS-S-W2 low')

# NNBEATSS_W2_high #####
datafolder <- paste0(results_path, 'NNBEATSS_W2_high/')
MS_NNBEATSS_W2_high <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_W2_high <- METHOD_ENSEMBLE(MS_NNBEATSS_W2_high)
MS_NNBEATSS_W2_high <- METHOD_EVAL_PREP(MS_NNBEATSS_W2_high, 'stability')
METHOD_EVAL(MS_NNBEATSS_W2_high, 'N-N-BEATS-S-W2 high')

# NNBEATSS_RMSC_low #####
datafolder <- paste0(results_path, 'NNBEATSS_RMSC_low/')
MS_NNBEATSS_RMSC_low <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_RMSC_low <- METHOD_ENSEMBLE(MS_NNBEATSS_RMSC_low)
MS_NNBEATSS_RMSC_low <- METHOD_EVAL_PREP(MS_NNBEATSS_RMSC_low, 'stability')
METHOD_EVAL(MS_NNBEATSS_RMSC_low, 'N-N-BEATS-S-RMSC low')

# NNBEATSS_RMSC_high #####
datafolder <- paste0(results_path, 'NNBEATSS_RMSC_high/')
MS_NNBEATSS_RMSC_high <- METHOD_LOAD(datafolder, 'mean_forecast', 'sd_forecast')
MS_NNBEATSS_RMSC_high <- METHOD_ENSEMBLE(MS_NNBEATSS_RMSC_high)
MS_NNBEATSS_RMSC_high <- METHOD_EVAL_PREP(MS_NNBEATSS_RMSC_high, 'stability')
METHOD_EVAL(MS_NNBEATSS_RMSC_high, 'N-N-BEATS-S-RMSC high')

# Stability results
MS_SCRPS %>% colMeans() %>% round(3)
MS_SPL90 %>% colMeans() %>% round(3)
MS_RMSSE %>% colMeans() %>% round(3)

MS_KL %>% colMeans() %>% round(3)
MS_SW2 %>% colMeans() %>% round(3)
MS_RMSSC %>% colMeans() %>% round(3)

print("medians")

MS_SCRPS %>% colMedians() %>% round(3)
MS_SPL90 %>% colMedians() %>% round(3)
MS_RMSSE %>% colMedians() %>% round(3)

MS_KL %>% colMedians() %>% round(3)
MS_SW2 %>% colMedians() %>% round(3)
MS_RMSSC %>% colMedians() %>% round(3)


########################## Forecast methods cumulative ##########################

# ETS-C1 #####
MS_ETSC1 <- fread(paste0(results_path, 
                         paste0(dataset_selection, dataset_subset_indicator,
                                '_ETS_probabilistic.csv')))
MS_ETSC1 <- MS_ETSC1[type %in% c('mean_forecast', 'sd_insample')]
MS_ETSC1[type == 'mean_forecast', `2` := `1` + `2`]
MS_ETSC1[type == 'mean_forecast', `3` := `2` + `3`]
MS_ETSC1[type == 'mean_forecast', `4` := `3` + `4`]
MS_ETSC1[type == 'mean_forecast', `5` := `4` + `5`]
MS_ETSC1[type == 'mean_forecast', `6` := `5` + `6`]
MS_ETSC1[type == 'sd_insample', `2` := sqrt(`1`^2 + `2`^2)]
MS_ETSC1[type == 'sd_insample', `3` := sqrt(`2`^2 + `3`^2)]
MS_ETSC1[type == 'sd_insample', `4` := sqrt(`3`^2 + `4`^2)]
MS_ETSC1[type == 'sd_insample', `5` := sqrt(`4`^2 + `5`^2)]
MS_ETSC1[type == 'sd_insample', `6` := sqrt(`5`^2 + `6`^2)]
MS_ETSC1[type == 'mean_forecast', type := 'cmean_forecast']
MS_ETSC1[type == 'sd_insample', type := 'csd_forecast']
MS_ETSC1 <- METHOD_EVAL_PREP(MS_ETSC1, 'cumulative')
METHOD_EVAL_C(MS_ETSC1, 'ETS-C1')

# ETS-C2 #####
MS_ETSC2 <- fread(paste0(results_path, 
                         paste0(dataset_selection, dataset_subset_indicator,
                                '_ETS_probabilistic.csv')))
MS_ETSC2 <- MS_ETSC2[type %in% c('mean_forecast', 'sd_forecast')]
MS_ETSC2[type == 'mean_forecast', `2` := `1` + `2`]
MS_ETSC2[type == 'mean_forecast', `3` := `2` + `3`]
MS_ETSC2[type == 'mean_forecast', `4` := `3` + `4`]
MS_ETSC2[type == 'mean_forecast', `5` := `4` + `5`]
MS_ETSC2[type == 'mean_forecast', `6` := `5` + `6`]
MS_ETSC2[type == 'sd_forecast', `2` := sqrt(`1`^2 + `2`^2)]
MS_ETSC2[type == 'sd_forecast', `3` := sqrt(`2`^2 + `3`^2)]
MS_ETSC2[type == 'sd_forecast', `4` := sqrt(`3`^2 + `4`^2)]
MS_ETSC2[type == 'sd_forecast', `5` := sqrt(`4`^2 + `5`^2)]
MS_ETSC2[type == 'sd_forecast', `6` := sqrt(`5`^2 + `6`^2)]
MS_ETSC2[type == 'mean_forecast', type := 'cmean_forecast']
MS_ETSC2[type == 'sd_forecast', type := 'csd_forecast']
MS_ETSC2 <- METHOD_EVAL_PREP(MS_ETSC2, 'cumulative')
METHOD_EVAL_C(MS_ETSC2, 'ETS-C2')

# ETS-C3 #####
MS_ETSC3 <- fread(paste0(results_path, 
                         paste0(dataset_selection, dataset_subset_indicator,
                                '_ETS_cumul_probabilistic.csv')))
MS_ETSC3 <- METHOD_EVAL_PREP(MS_ETSC3, 'cumulative')
METHOD_EVAL_C(MS_ETSC3, 'ETS-C3')

# DEEPAR #####
datafolder <- paste0(results_path, 'DEEPAR/')
MS_DEEPAR <- METHOD_LOAD(datafolder, 'mean_forecast', 'csd_forecast')
MS_DEEPAR[type == 'mean_forecast', `2` := `1` + `2`]
MS_DEEPAR[type == 'mean_forecast', `3` := `2` + `3`]
MS_DEEPAR[type == 'mean_forecast', `4` := `3` + `4`]
MS_DEEPAR[type == 'mean_forecast', `5` := `4` + `5`]
MS_DEEPAR[type == 'mean_forecast', `6` := `5` + `6`]
MS_DEEPAR[type == 'csd_forecast', type := 'sd_forecast']
MS_DEEPAR <- METHOD_ENSEMBLE(MS_DEEPAR)
MS_DEEPAR[type == 'mean_forecast', type := 'cmean_forecast']
MS_DEEPAR[type == 'sd_forecast', type := 'csd_forecast']
MS_DEEPAR <- METHOD_EVAL_PREP(MS_DEEPAR, 'cumulative')
METHOD_EVAL_C(MS_DEEPAR, 'DeepAR')

# NNBEATSC #####
datafolder <- paste0(results_path, 'NNBEATSC/')
MS_NNBEATSC <- METHOD_LOAD(datafolder, 'mean_forecast', 'csd_forecast')
MS_NNBEATSC[type == 'mean_forecast', `2` := `1` + `2`]
MS_NNBEATSC[type == 'mean_forecast', `3` := `2` + `3`]
MS_NNBEATSC[type == 'mean_forecast', `4` := `3` + `4`]
MS_NNBEATSC[type == 'mean_forecast', `5` := `4` + `5`]
MS_NNBEATSC[type == 'mean_forecast', `6` := `5` + `6`]
MS_NNBEATSC[type == 'csd_forecast', type := 'sd_forecast']
MS_NNBEATSC <- METHOD_ENSEMBLE(MS_NNBEATSC)
MS_NNBEATSC[type == 'mean_forecast', type := 'cmean_forecast']
MS_NNBEATSC[type == 'sd_forecast', type := 'csd_forecast']
MS_NNBEATSC <- METHOD_EVAL_PREP(MS_NNBEATSC, 'cumulative')
METHOD_EVAL_C(MS_NNBEATSC, 'N-N-BEATS-C')

# Cumulative results
for (lead_time_select in 1:FL){
  MS_SCRPS_C[L == lead_time_select] %>% colMeans() %>% round(3) %>% print()
  MS_SPL90_C[L == lead_time_select] %>% colMeans() %>% round(3) %>% print()
  MS_RMSSE_C[L == lead_time_select] %>% colMeans() %>% round(3) %>% print()
}

for (lead_time_select in 1:FL){
  MS_SCRPS_C[L == lead_time_select] %>% colMedians() %>% round(3) %>% print()
  MS_SPL90_C[L == lead_time_select] %>% colMedians() %>% round(3) %>% print()
  MS_RMSSE_C[L == lead_time_select] %>% colMedians() %>% round(3) %>% print()
}

########################## Statistical comparison ##########################
library(tsutils)

# Stability experiment
# Accuracy
MS_SCRPS_plot <- nemenyi(as.matrix(MS_SCRPS[,2:11]), plottype = 'vmcb')
MS_SPL90_plot <- nemenyi(as.matrix(MS_SPL90[,2:11]), plottype = 'vmcb')
MS_RMSSE_plot <- nemenyi(as.matrix(MS_RMSSE[,2:11]), plottype = 'vmcb')
# Stability
MS_KL_plot <- nemenyi(as.matrix(MS_KL[,2:11]), plottype = 'vmcb')
MS_SW2_plot <- nemenyi(as.matrix(MS_SW2[,2:11]), plottype = 'vmcb')
MS_RMSSC_plot <- nemenyi(as.matrix(MS_RMSSC[,2:11]), plottype = 'vmcb')

# Cumulative experiment
for (lead_time_select in 1:FL){
  MS_SCRPS_C_plot <- nemenyi(as.matrix(MS_SCRPS_C[L == lead_time_select ,3:7]), plottype = 'vmcb',
                             main = paste0('SCRPS - lead time ', lead_time_select))
  MS_SPL90_C_plot <- nemenyi(as.matrix(MS_SPL90_C[L == lead_time_select ,3:7]), plottype = 'vmcb',
                             main = paste0('SPL90 - lead time ', lead_time_select))
  MS_RMSSE_C_plot <- nemenyi(as.matrix(MS_RMSSE_C[L == lead_time_select ,3:7]), plottype = 'vmcb',
                             main = paste0('RMSSE - lead time ', lead_time_select))
}


png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_SCRPS_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_SCRPS[,2:11]), plottype = 'vmcb')
dev.off()

png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_SPL90_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_SPL90[,2:11]), plottype = 'vmcb')
dev.off()

png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_RMSSE_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_RMSSE[,2:11]), plottype = 'vmcb')
dev.off()

png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_KL_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_KL[,2:11]), plottype = 'vmcb')
dev.off()

png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_SW2_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_SW2[,2:11]), plottype = 'vmcb')
dev.off()

png(file = 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/MS_RMSSC_plot.png',
    units = 'in', width = 6, height = 4, res = 300)
nemenyi(as.matrix(MS_RMSSC[,2:11]), plottype = 'vmcb')
dev.off()

plots_path= 'C:/Users/u0165132/OneDrive - KU Leuven/1-PhD/NNBEATS project/Results/Plots/'
for (lead_time_select in 1:FL){
  
  png(file = paste0(plots_path,"SCRPS_lead_time_",lead_time_select,".png"),
      units = 'in', width = 6, height = 4, res = 300)
  nemenyi(as.matrix(MS_SCRPS_C[L == lead_time_select ,3:7]), plottype = 'vmcb')
  dev.off()
  
  png(file = paste0(plots_path,"SPL90_lead_time_",lead_time_select,".png"),
      units = 'in', width = 6, height = 4, res = 300)
  nemenyi(as.matrix(MS_SPL90_C[L == lead_time_select ,3:7]), plottype = 'vmcb')
  dev.off()
  png(file = paste0(plots_path,"RMSSE_lead_time_",lead_time_select,".png"),
      units = 'in', width = 6, height = 4, res = 300)
  nemenyi(as.matrix(MS_RMSSE_C[L == lead_time_select ,3:7]), plottype = 'vmcb')
  dev.off()
  
}

##############################################
##############################################
