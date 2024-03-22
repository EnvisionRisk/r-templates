#### DEPENDENCIES ####
#https://www.ise.ufl.edu/uryasev/files/2011/11/CVaR1_JOR.pdf
get_script_path <- function(){
  script_path <- rstudioapi::getSourceEditorContext()$path 
  script_path <- sub('[^/]+$', '', script_path)
  return(script_path)
}

script_path <- get_script_path()

#### Dependencies ####
#source(paste0(script_path, "cVaR optim dependencies.R"))

logo <- magick::image_read("C:/Users/jonas/Dropbox/Envisionrisk/logoicon.png")

usePackage <- function(p)
{
  if (!is.element(p, utils::installed.packages()[,1]))
    utils::install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "parallel", "doParallel", "ggplot2")

# In case one or more of the packages are not installed they will be installed
suppressWarnings(sapply(pckg, usePackage))

library(data.table)
library(parallel)
library(doParallel)
library(ggplot2)
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
#library(ggradar)

options(scipen=999)

#******************************************************************************
#*
#### PERFORMANCE & RISK EVOLUTION OVER TIME - MAIN ANALYSIS! ####
#*
#******************************************************************************
#' Read Data from Clipboard as a Table
#'
#' This function is designed to read data from the clipboard, which is assumed 
#' to have been copied from an Excel file. It facilitates the importing of 
#' tab-separated values into R as a data frame. The primary use case is to 
#' quickly import data that has been copied directly from an Excel spreadsheet.
#'
#' @param header A logical value indicating whether the first row of data should 
#' be treated as column headers. Defaults to TRUE.
#' @param ... Additional arguments passed to `read.table`.
#'
#' @return A data frame with the contents of the clipboard, formatted based on 
#' the specified parameters.
#'
#' @examples
#' # To use this function, first copy data from an Excel spreadsheet.
#' # Then run the function in R:
#' data_from_clipboard <- read.excel()
#'
#' @export
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

#' Write Data to Clipboard in Excel Format
#'
#' This function writes a data frame or matrix to the clipboard in a format 
#' that can be pasted directly into Excel. It is a convenient wrapper around 
#' 'write.table' with tab-separated values, specifically tailored for pasting 
#' into Excel.
#'
#' @param x The data frame or matrix to be written to the clipboard.
#' @param row.names A logical value indicating whether the row names of 'x' 
#' are to be written along with the data. Defaults to FALSE.
#' @param col.names A logical value indicating whether the column names of 'x' 
#' are to be written along with the data. Defaults to TRUE.
#' @param ... Additional arguments to be passed to 'write.table'.
#'
#' @return The function does not return a value; it writes output to the 
#' clipboard.
#'
#' @examples
#' # Example usage:
#' data(iris)
#' write.excel(iris[1:5, 1:3], row.names = TRUE)
#'
#' @export
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

get_performance <- function(dt_prices, instr_universe, base_cur, return_period = "Quarterly", start_date = as.Date("2005-01-01"), end_date = Sys.Date()){
  dt_prices_tmp <- copy(dt_prices)
  dt_prices_tmp <- dt_prices_tmp[data.table::between(date, start_date, end_date)]
  dt_returns <- get_return_data(dt_prices_tmp, 
                                return_period = return_period)
  dt_perf <- dt_returns[,.("performance" = mean(return)), by = symbol]
  setnames(dt_perf, "symbol", "ticker")
  return(dt_perf)
}

get_return_data <- function(dt_prices, return_period = c("Weekly", "Monthly", "Quarterly", "Yearly")){
  dt_tmp_prices <- copy(dt_prices)
  
  if(return_period == "Weekly"){
    dt_tmp_prices[, period := format(date, "%Y-%V")]
    dt_prices_min_date <- setDT(dt_tmp_prices)[, .SD[which.min(date)], by=c("symbol", "period")]
    dt_prices_max_date <- setDT(dt_tmp_prices)[, .SD[which.max(date)], by=c("symbol", "period")]
    dt_prices_out      <- merge(dt_prices_min_date[,.(symbol, period, "start_close" = adj_close)], 
                                dt_prices_max_date[,.(symbol, period, "end_close" = adj_close)], 
                                by = c("symbol", "period"))
    dt_prices_out[, return := (end_close - start_close)/start_close]
    return(dt_prices_out)
  }
  if(return_period == "Monthly"){
    dt_tmp_prices[, period := format(date, "%Y-%m")]
    dt_prices_min_date <- setDT(dt_tmp_prices)[, .SD[which.min(date)], by=c("symbol", "period")]
    dt_prices_max_date <- setDT(dt_tmp_prices)[, .SD[which.max(date)], by=c("symbol", "period")]
    dt_prices_out      <- merge(dt_prices_min_date[,.(symbol, period, "start_close" = adj_close)], 
                                dt_prices_max_date[,.(symbol, period, "end_close" = adj_close)], 
                                by = c("symbol", "period"))
    dt_prices_out[, return := (end_close - start_close)/start_close]
    return(dt_prices_out)
  }
  if(return_period == "Quarterly"){
    dt_tmp_prices[, period := paste(format(date, "%Y"),  # Convert dates to quarterly
                                    sprintf("%02i", (as.POSIXlt(date)$mon) %/% 3L + 1L), 
                                    sep = "-")]
    dt_prices_min_date <- setDT(dt_tmp_prices)[, .SD[which.min(date)], by=c("symbol", "period")]
    dt_prices_max_date <- setDT(dt_tmp_prices)[, .SD[which.max(date)], by=c("symbol", "period")]
    dt_prices_out      <- merge(dt_prices_min_date[,.(symbol, period, "start_close" = adj_close)], 
                                dt_prices_max_date[,.(symbol, period, "end_close" = adj_close)], 
                                by = c("symbol", "period"))
    dt_prices_out[, return := (end_close - start_close)/start_close]
    return(dt_prices_out)
  }
  if(return_period == "Yearly"){
    dt_tmp_prices[, period := year(date)]
    dt_prices_min_date <- setDT(dt_tmp_prices)[, .SD[which.min(date)], by=c("symbol", "period")]
    dt_prices_max_date <- setDT(dt_tmp_prices)[, .SD[which.max(date)], by=c("symbol", "period")]
    dt_prices_out      <- merge(dt_prices_min_date[,.(symbol, period, "start_close" = adj_close)], 
                                dt_prices_max_date[,.(symbol, period, "end_close" = adj_close)], 
                                by = c("symbol", "period"))
    dt_prices_out[, return := (end_close - start_close)/start_close]
    return(dt_prices_out)
  }
  return(dt_prices)
}

get_simulated_pnl <- function(dt_prices, date_of_analysis, tickers, base_cur, risk_horizon, volatility_scenario){
  dt_pnl <- EnvisionRiskRaaS::envrsk_instrument_delta_vector(
    date          = date_of_analysis,
    symbols       = tickers, 
    base_cur      = base_cur,
    horizon       = risk_horizon,
    volatility_id = volatility_scenario)[["Output"]][, .("pnl" = PnL, "symbol" = SYMBOL, "scenario" = SCENARIO)]
  setorder(dt_pnl, "symbol", "scenario")
  
  sim_pnl <- merge(dt_pnl, dt_prices[date == date_of_analysis], by = "symbol", all.x = TRUE)
  sim_pnl[, pnl_pct := pnl / adj_close]
  sim_pnl <- dcast.data.table(sim_pnl, scenario ~ symbol, value.var = "pnl_pct")
  sim_pnl <- sim_pnl[, -1]
}

condition_weight <- function(x, p_min_weight, p_max_weight){
  if(any(x != 0.0 & x < p_min_weight) || any(x != 0.0 & x > p_max_weight)){
    return(1)
  }
  return(0)
}

risk_calc_parallization <- function(p_dt_weigtht, p_sim_pnl, p_rsk_conf_level, n_cores = 1){
  count_port_weights <- nrow(p_dt_weigtht)
  dt_weigtht <- copy(p_dt_weigtht)
  
  message(paste0("The cluster is configured with ", n_cores, " core(s) to compute the Expected Shortfall \nfor each simulated set of portfolio weights."))
  cl <- parallel::makeCluster(n_cores)
  doSNOW::registerDoSNOW(cl)
  
  # progress bar ------------------------------------------------------------
  # token reported in progress bar
  paths <- paste(as.character(1:count_port_weights), 
                 as.character(count_port_weights), sep= "/")  
  
  # allowing progress bar to be used in foreach -----------------------------
  progress <- function(n){
    pb$tick(tokens = list(paths_process = paths[n]))
  } 
  opts <- list(progress = progress)
  
  # foreach loop ------------------------------------------------------------
  # Reset pb for each run
  pb <- progress::progress_bar$new(
    format = "weights_processed = :paths_process [:bar] :elapsed | eta: :eta",
    total = count_port_weights,    
    width = 60)
  
  # Iterate
  m_pnl_sim    <- as.matrix(p_sim_pnl)
  ticker_names <- colnames(p_sim_pnl)
  dt_es <- foreach::foreach(i=1:count_port_weights,
                             .packages=c('data.table', 'Rcpp', 'EnvRskAssetAllocationHelpers'),
                             .combine = rbind,
                             .options.snow = opts,
                             .errorhandling='pass') %dopar% { #
    dt_w         <- as.numeric(dt_weigtht[i,])
    m_w          <- t(as.matrix(dt_w))
    #ticker_names <- colnames(m_w)
    
    es_values <- lapply(1:nrow(m_w), function(x){
      
      exec_id <- digest::digest(paste0(i, "_", x, "_", Sys.time()), algo = "md5")
      w  <- m_w[x, ]

      es <- EnvRskAssetAllocationHelpers::calculate_port_es(
        m_pnl_sim, 
        w, 
        component = TRUE, 
        confidence_level = p_rsk_conf_level)  
      
      data.table("exec_id"            = exec_id,
                 "ticker"             = ticker_names,
                 "weight"             = w,
                 "expected_shortfall" = es[-1],
                 "expected_shortfall_weight" = es[-1]/as.numeric(es[1]))
    })
    dt_values <- rbindlist(es_values)
    dt_values
  }
  
  # Terminate the progressbar
  pb$terminate()
  
  # Stop the cluster
  snow::stopCluster(cl) 
  closeAllConnections()
  
  return(dt_es)
}

#### RISK PROFILING ####
risk_profile_gaussian <- function(dt_pnl_simulations, dt_weights, p_alfa = 0.975){
  n_sim       <- nrow(dt_weights)
  V_Sigma     <- cov(dt_pnl_simulations[, colnames(dt_weights), with = FALSE])
  my_vector   <- round(apply(dt_pnl_simulations[, colnames(dt_weights), with = FALSE], 2, "mean"), 6)
  
  es_component_gaussian <- function(w, Sigma, alfa){
    weight_vec <- as.numeric(w)
    sigma_v   <- Sigma %*% weight_vec
    sigma_l2  <- t(weight_vec) %*% sigma_v
    sigma_l   <- as.numeric(sqrt(sigma_l2))
    
    my_l <- as.numeric(-t(weight_vec) %*% my_vector)
    es_l <- as.numeric(my_l + as.numeric(dnorm(qnorm(alfa)) / (1 - alfa)) * sigma_l)
    
    scaler <- as.numeric(dnorm(qnorm(alfa)) / (1 - alfa)) 
    rb <- - my_vector + scaler * t(sigma_v)/sigma_l
    dt_out <- data.table(
      "position_id" = names(w),
      "weight"      = c(weight_vec), 
      "slope"       = c(rb)/es_l)
    dt_out[, exec_id := EnvRskHelpers::HashedId(1)]
    dt_out[, risk_weight := slope * weight]
    return(dt_out)
  }
  
  lst_es_component <- lapply(1:nrow(dt_weights), function(x){
    es_component_gaussian(dt_weights[x, ], V_Sigma, p_alfa)
  })
  dt_es_component <- rbindlist(lst_es_component)
  
  return(dt_es_component)
}

sim_weights_conditional_weight <- function(cond_weight, dt_weights_limits, scenario_id){
  #stopifnot(c("position_id", "min_weight", "max_weight") %in% colnames(dt_weights_limits))
  
  count_positions <- nrow(dt_weights_limits)
  
  dt_adj_weights <- copy(dt_weights_limits)
  dt_adj_weights[, ":=" (min_weight = min_weight / cond_weight, 
                         max_weight = max_weight / cond_weight)]
  
  sim_new_adj_weights <- gtools::rdirichlet(1, rep(1, count_positions))
  colnames(sim_new_adj_weights) <- dt_adj_weights$position_id
  sim_new_adj_weights <- t(as.matrix(sim_new_adj_weights[, dt_adj_weights$position_id]))
  
  new_adj_weights <- EnvRskAssetAllocationHelpers::portfolio_weights_apply_weight_constraint(
    sim_new_adj_weights,
    dt_adj_weights) * as.numeric(cond_weight)
  new_adj_weights <- cbind(scenario_id, new_adj_weights)
  return(new_adj_weights)
}

risk_budget_weight_adjustment <- function(p_dt_focus, p_dt_risk_budget_group_weight_simulations, p_risk_budget_groups, p_n_sim, n_cores = 1){
  stopifnot(all(colnames(p_dt_focus) %in% c("position_id", "location", "min_weight", "max_weight")))
  stopifnot(all(colnames(p_dt_risk_budget_group_weight_simulations) %in% p_risk_budget_groups))
  
  count_grps <- length(p_risk_budget_groups)
  dt_focus   <- copy(p_dt_focus)
  
  message(paste0("The cluster is configured with ", n_cores, " core(s) to fine-tune the portfolio weight \nfor each risk-budget-group"))
  cl      <- parallel::makeCluster(n_cores)
  doSNOW::registerDoSNOW(cl)
  
  # Iterate
  dt_weight_weight_adjusted_sims <- lapply(1:count_grps, function(i){                                                        
    # progress bar ------------------------------------------------------------
    # token reported in progress bar
    paths <- paste(as.character(1:p_n_sim), 
                   as.character(p_n_sim), sep= "/")  
    
    # allowing progress bar to be used in foreach -----------------------------
    progress <- function(n){
      pb$tick(tokens = list(paths_process = paths[n]))
    } 
    opts <- list(progress = progress)
    
    # foreach loop ------------------------------------------------------------
    # Reset pb for each run
    pb <- progress::progress_bar$new(
      format = "portfolio_weights_processed = :paths_process [:bar] :elapsed | eta: :eta",
      total = p_n_sim,    
      width = 60)
    
     grp <- p_risk_budget_groups[i]
     message(paste("Processing risk-budget-group: ", grp))
     dt_this_focus      <- copy(dt_focus)[grepl(grp, location)]
     count_instr        <- nrow(dt_this_focus)
     
     dt_min_max_weights <- dt_this_focus[,.(position_id, min_weight, max_weight)]
     dt_grp_weight      <- copy(p_dt_risk_budget_group_weight_simulations[, grp, with = FALSE])
     
     dt_weight_weight_adjusted_grp_sims <- foreach::foreach(i=1:p_n_sim,
                                                        .export = c('sim_weights_conditional_weight'),
                                                        .packages=c('data.table', 'Rcpp', 'EnvRskAssetAllocationHelpers'),
                                                        .combine = rbind,
                                                        .options.snow = opts,
                                                        .errorhandling='pass') %dopar% {
     
       sim_weights_conditional_weight(
         dt_grp_weight[i], 
         dt_min_max_weights,
         i)
     }
     
    # Terminate the progressbar
    pb$terminate()
    
    setorder(dt_weight_weight_adjusted_grp_sims, "scenario_id")
    dt_weight_weight_adjusted_grp_sims <- dt_weight_weight_adjusted_grp_sims[, -1]
    dt_weight_weight_adjusted_grp_sims
  })
  dt_weight_adjusted_sims <- do.call(cbind, dt_weight_weight_adjusted_sims)
  
  # Stop the cluster
  snow::stopCluster(cl) 
  closeAllConnections()
  
  return(dt_weight_adjusted_sims)
}

portfolio_distance <- function(optimal_weight, dt_weights_2_compare){
  sum_square_diviance <- function(x, y){
    sum((x - y)^2)
  }
  
  lst_out <- lapply(1:nrow(dt_weights_2_compare), function(x){
    sum_square_diviance(optimal_weight,
                        dt_weights_2_compare[x, ])
  })
  dt_out <- do.call(c, lst_out)
  return(dt_out)
}

# risk_calc_parallization <- function(dt_weigtht, sim_pnl, p_rsk_conf_level, nr_cores = 1){
#   #batches
#   batch_size <- ceiling(nrow(dt_weigtht) / nr_cores)
#   dt_weigtht_residual <- copy(dt_weigtht)
#   dt_weigtht_residual[, index := (0:(.N-1))]
#   dt_weigtht_residual[, batch := index %/% batch_size + 1]
#   dt_weigtht_residual[, index := NULL]
#   dt_weigtht_residual_splt <- split(dt_weigtht_residual, dt_weigtht_residual$batch)
#   dt_weigtht_residual_splt <- lapply(dt_weigtht_residual_splt, function(x){
#     x[, batch := NULL] 
#   })
#   
#   doParallel::registerDoParallel(cores = nr_cores)
#   do_components <- TRUE
#   
#   es_all <- foreach::foreach(i=1:length(dt_weigtht_residual_splt), .export = c('calculate_port_es', 'expected_shortfall', 'expected_shortfall_component'), .packages=c('data.table', 'doParallel'), .errorhandling='pass') %dopar% { #
#     
#     dt_w <- dt_weigtht_residual_splt[[i]]
#     m_w  <- as.matrix(dt_w)
#     ticker_names <- colnames(m_w)
#     es_values <- lapply(1:nrow(m_w), function(x){
#       
#       exec_id <- digest::digest(paste0(i, "_", x, "_", Sys.time()), algo = "md5")
#       w  <- m_w[x, ]
#       
#       es <- calculate_port_es(sim_pnl,
#                               w, 
#                               do_component = do_components, 
#                               confidence_level = p_rsk_conf_level)
#       data.table("exec_id"            = exec_id,
#                  "ticker"             = ticker_names,
#                  "weight"             = w,
#                  "expected_shortfall" = es[-1],
#                  "expected_shortfall_Weight" = es[-1]/as.numeric(es["ES"]))
#     })
#     dt_values <- rbindlist(es_values)
#     
#     return(dt_values)
#   }
#   
#   dt_es <- rbindlist(es_all)
#   closeAllConnections()
#   return(dt_es)
# }

#### HELPER FUNCTIONS #### 
expected_shortfall <- function(vec, p_rsk_conf_level){
  if (length(vec) == 1) {
    return(vec)
  }
  var_scenarie <- (1 - p_rsk_conf_level) * length(vec)
  return(mean(sort(vec)[1:var_scenarie]))
}

expected_shortfall_component <- function (vec, p_rsk_conf_level, sort_order, window = 1) {
  if (length(vec) == 1) {
    return(vec)
  }
  component_vec <- vec[sort_order]
  var_scenarie <- (1 - p_rsk_conf_level) * length(vec)
  return(mean(component_vec[1:var_scenarie]))
}

normalize <- function(v_weight){
  return(v_weight/sum(v_weight))
}

#### Combine risk & performance ####
comb_risk_perf <- function(dt_es, dt_perf){
  dt_es_comb <- merge(dt_es, 
                      dt_perf, 
                      by = "ticker", 
                      all.x = TRUE)
  dt_es_comb[, performance_weighted := performance * weight]
  setorder(dt_es_comb, "exec_id")
  dt_es_comb[, performance := NULL]
  setnames(dt_es_comb, "performance_weighted", "performance")
  
  return(dt_es_comb)
}

slope_analysis <- function(tickers, dt_es_comb){
  # If the slope > 1 then the ticker tend to add risk to the portfolio more than 
  # its weight. While if the slope < 1 the ticker tend to reduce risk.
  calc_es_comp_slope_2_weight <- function(use_ticker){
    data_4_analysis <- dt_es_comb[ticker == use_ticker & weight != 0, .(weight, expected_shortfall_weight)]
    #plot(data_4_analysis)
    my_glm <- glm(expected_shortfall_weight ~ 0 + weight, data = data_4_analysis)
    my_coef <- coef(my_glm)
    return(data.table("ticker" = use_ticker,
                      "slope"  = as.numeric(my_coef)))
  }
  
  lst_slopes <- lapply(tickers, function(x){
    calc_es_comp_slope_2_weight(x)  
  })
  dt_slopes <- rbindlist(lst_slopes)
  setorder(dt_slopes, "slope")
  dt_slopes[, "PortfolioEffect" := dplyr::case_when(slope > 1.2 ~ "Concentrator",
                                                    slope < 0.8 ~ "Diversifier",
                                                    TRUE ~ "Neutral")]
  return(dt_slopes)
}

efficient_frontier <- function(dt_rsk, p_rsk_free_rate){
  min_es <- min(-dt_rsk$ES)
  max_es <- max(-dt_rsk$ES)
  count_es_buckets <- 25
  ex_bucket_length <- (max_es - min_es) / count_es_buckets
  dt_rsk[, silo := cut(-dt_rsk$ES, breaks = c(min_es, min_es+1:count_es_buckets*ex_bucket_length), include.lowest = TRUE)]
  
  dt_tmp_1 <- dt_rsk[, .("avg_es" = mean(-ES), "max_perf" = max(PERF), "max_perf_scenario" = .I[which.max(PERF)]), by = silo]
  setorder(dt_tmp_1, "avg_es")
  
  dt_tmp_rm <- copy(dt_tmp_1)
  dt_tmp_rm[, keep := NA_integer_]
  latest_max <- dt_tmp_rm[1, max_perf]
  n_rows <- nrow(dt_tmp_rm)
  for(i in 1:n_rows){
    #i <- 1
    
    this_max <- dt_tmp_rm[i, max_perf]
    
    if(this_max >= latest_max){
      latest_max <- this_max
      dt_tmp_rm[i]$keep <- 1
    } else {
      dt_tmp_rm[i]$keep <- 0
    }
  }
  setorder(dt_tmp_rm, "avg_es")
  
  #### ####
  
  dt_tmp_rm <- dt_tmp_rm[keep == 1]
  #plot(dt_tmp_rm$avg_es, dt_tmp_rm$max_perf)
  n_rows <- nrow(dt_tmp_rm)
  for(i in 1:(n_rows - 2)){
    #i <- 1
    this_max <- dt_tmp_rm[i, max_perf]
    tanget_1 <- (dt_tmp_rm[i+1, max_perf]-dt_tmp_rm[i, max_perf])/(dt_tmp_rm[i+1, avg_es]-dt_tmp_rm[i, avg_es])
    tanget_2 <- (dt_tmp_rm[i+2, max_perf]-dt_tmp_rm[i, max_perf])/(dt_tmp_rm[i+2, avg_es]-dt_tmp_rm[i, avg_es])  
    
    if(tanget_2 >= tanget_1){
      dt_tmp_rm[i+1]$keep <- 0
    } else {
      dt_tmp_rm[i+1]$keep <- 1
    }
  }
  setorder(dt_tmp_rm, "avg_es")
  
  dt_tmp_rm <- dt_tmp_rm[keep == 1]
  #plot(dt_tmp_rm$avg_es, dt_tmp_rm$max_perf)
  
  n_rows <- nrow(dt_tmp_rm)
  dt_tmp_rm[, tangent := NA_real_]
  for(i in 1:n_rows){
    tanget_rsk_free_rate <- (dt_tmp_rm[i, max_perf]-p_rsk_free_rate)/dt_tmp_rm[i, avg_es]
    dt_tmp_rm[i]$tangent <- tanget_rsk_free_rate
  }
  setorder(dt_tmp_rm, "avg_es")
  
  #### PRUNE ####
  dt_tangents <- copy(dt_tmp_rm)
  dt_tangents[, index := 1:nrow(dt_tangents)]
  
  dt_tan_scenarios <- data.table(expand.grid(1:nrow(dt_tangents), 1:nrow(dt_tangents)))
  setnames(dt_tan_scenarios, "Var1", "Sce1")
  setnames(dt_tan_scenarios, "Var2", "Sce2")
  dt_tan_scenarios <- dt_tan_scenarios[Sce2 > Sce1]
  setorder(dt_tan_scenarios, "Sce1")
  
  lst_tanget_out <- lapply(1:nrow(dt_tan_scenarios), function(x){
    #i <- 1
    dt_out <- dt_tan_scenarios[x,]
    p_1 <- dt_tangents[index == dt_out$Sce1,]
    p_2 <- dt_tangents[index == dt_out$Sce2,]
    tangent <- (p_2$max_perf - p_1$max_perf) / ((p_2$avg_es - p_1$avg_es)) 
    dt_out[, tangent := tangent]
  })
  dt_tangent_out <- rbindlist(lst_tanget_out)
  
  dt_tangent_in <- copy(dt_tangent_out)
  for(i in 1:(nrow(dt_tangent_in) - 1)){
    #i <- 1
    dt_tangent_in_tmp <- dt_tangent_in[Sce1 == i,]
    if(nrow(dt_tangent_in_tmp) != 0){
      select_ind <- dt_tangent_in_tmp[, which.max(tangent)]
      select_sce <- dt_tangent_in_tmp[select_ind, .(Sce1, Sce2)]
      dt_tangent_in <- dt_tangent_in[!(Sce1 == i & Sce2 != select_sce$Sce2)]
      
      select_sce <- select_sce$Sce1:select_sce$Sce2
      select_sce <- select_sce[data.table::between(select_sce, min(select_sce), max(select_sce), incbounds=FALSE)]
      
      dt_tangent_in <- dt_tangent_in[!Sce1 %in% select_sce]
    }
  }
  select_sce <- 1:nrow(dt_tangents)
  select_sce <- select_sce[data.table::between(select_sce, min(select_sce), max(select_sce), incbounds=FALSE)] 
  remove_sce <- select_sce[!select_sce %in% dt_tangent_in$Sce1]
  dt_tangents <- dt_tangents[!index %in% remove_sce]
  return(dt_tangents)
}

plot_efficient_frontier <- function(p_lst_mean_cvar_analysis, lst_input_port = NULL){ 
  #p_lst_mean_cvar_analysis <- lst_mean_cvar_analysis
  p_rsk_free_rate <- p_lst_mean_cvar_analysis[["RiskFreeRate"]]
  dt_eff_frnt <- p_lst_mean_cvar_analysis[["EfficientFrontier"]]
  dt_4_plot <- copy(dt_eff_frnt[,.(avg_es, max_perf, "tangent" = tangent)])
  
  #### TANGET CALC ####
  fix_point_opt_port <- dt_4_plot[which.max(tangent)]
  slope <- (fix_point_opt_port$max_perf - p_rsk_free_rate) / (fix_point_opt_port$avg_es - 0.0)
  
  #### ####
  dt_risk_perf <- p_lst_mean_cvar_analysis[["OptimalWeights"]][,.(ticker, performance, risk)]
  dt_4_plot[, ticker := ""]
  dt_4_plot_risk_perf <- dt_risk_perf[, .("avg_es"   = risk, 
                                          "max_perf" = performance, 
                                          "ticker"   = ticker,
                                          "tangent"  = NA_real_)]
  dt_4_plot <- rbind(dt_4_plot,
                     dt_4_plot_risk_perf)
  dt_4_plot <- rbind(dt_4_plot,
                     data.table("avg_es"   = 0.0,
                                "max_perf" = p_rsk_free_rate,
                                "ticker"   = "RiskfreeRate",
                                "tangent"  = NA_real_))
  
  p1 <- ggplot(dt_4_plot, aes(x = avg_es, y = max_perf)) + 
    geom_point(color = dplyr::case_when(dt_4_plot$ticker == "" ~ "blue", 
                                        dt_4_plot$ticker == "RiskfreeRate" ~ "green",
                                        TRUE ~ "red"), size = 1, alpha = 0.8) + 
    geom_line(data    = dt_4_plot[ticker == ""], 
              mapping = aes(x = avg_es, 
                            y = max_perf), 
              color = "blue") +
    geom_text(aes(label = ifelse(ticker != "", as.character(ticker), '')), 
              hjust = -0.20, 
              vjust = 0.4, 
              size  = 1.5)+
    geom_abline(slope     = slope, 
                intercept = p_rsk_free_rate, 
                linetype = "dashed") +
    geom_text(aes(x = -lst_input_port[["ExpectedShortfall"]], 
                  y = lst_input_port[["PnL"]]), 
                  label  ="★", 
                  vjust  = "center", 
                  hjust  = "center",
                  size   = 5, 
                  colour = "gray")+
    geom_label(
      label      = "Original Portfolio", 
      x          = -lst_input_port[["ExpectedShortfall"]],
      y          = lst_input_port[["PnL"]],
      label.size = NA,
      vjust      = "inward", 
      hjust      = "outward",
      color      = "gray",
      fill       = NA,
      size       = 3) +
    geom_text(aes(x = p_lst_mean_cvar_analysis[["MarketPortfolio"]][["avg_es"]], 
                  y = p_lst_mean_cvar_analysis[["MarketPortfolio"]][["max_perf"]]), 
                  label  = "★", 
                  vjust  = "bottom",
                  size   = 5, 
                  colour = "blue")+
    geom_label(
        label      = "Market Portfolio", 
        x          = p_lst_mean_cvar_analysis[["MarketPortfolio"]][["avg_es"]],
        y          = p_lst_mean_cvar_analysis[["MarketPortfolio"]][["max_perf"]],
        label.size = NA,
        vjust      = "outward", 
        hjust      = "outward",
        color      = "blue",
        fill       = NA,
        size       = 3) +
    theme_bw()+
    xlab("CVaR")+
    ylab("Expected Performance")+
    labs(
      title    = "Strategic Asset Allocation",
      subtitle = paste0(ceiling(p_lst_mean_cvar_analysis[["HorizonYears"]]), " Year Horizon"))
  return(p1)
}

#### INDIVIDUAL RISK ####
calc_individual_risk <- function(sim_pnl, dt_perf, confidence_level){
  es <- apply(sim_pnl, 2, function(x){
    expected_shortfall(x, confidence_level)  
  })
  dt_risk <- data.table("ticker" = names(es),
                        "risk" = -es)
  
  dt_risk_perf <- merge(dt_perf, dt_risk, by = "ticker", all.x = TRUE)
  setorder(dt_risk_perf, "performance")  
  return(dt_risk_perf)
}


#******************************************************************************
#*
#### Aggregate & Combine #### 
#*
#******************************************************************************
pckg <- c("data.tree")
sapply(pckg, usePackage)

library(data.tree)

aggregate_and_combine_port_risk <- function(p_positions, p_date, p_volatility_id, p_signif_level, p_horizon, p_base_cur, rsk_msr = "ES"){
  demo_port_risk_out_reg <- EnvisionRiskRaaS::envrsk_portfolio_risk_regular( 
    positions     = p_positions,
    date          = p_date,
    volatility_id = p_volatility_id,
    signif_level  = p_signif_level,
    horizon       = p_horizon,
    base_cur      = p_base_cur)
  
  # Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
  demo_port_risk_out_cmp <- EnvisionRiskRaaS::envrsk_portfolio_risk_component( 
    positions     = p_positions,
    date          = p_date,
    volatility_id = p_volatility_id,
    signif_level  = p_signif_level,
    horizon       = p_horizon,
    base_cur      = p_base_cur)
  
  # Retain the data we need from API call.
  dt_demo_port_risk_reg <- format_portfolio_risk(demo_port_risk_out_reg)
  dt_demo_port_risk_cmp <- format_portfolio_risk(demo_port_risk_out_cmp)
  
  dt_demo_port_risk <- merge(
    dt_demo_port_risk_reg[,.(pathString, position_type, label, quantity, "Value_at_Risk" = VaR, "Expected_Shortfall" = ES)],
    dt_demo_port_risk_cmp[,.(pathString, "Impact_Value_at_Risk" = VaR, "Impact_Expected_Shortfall" = ES)],
    by = "pathString")
  
  # Calculate Market-value for the portfolio
  demo_port_perf_out <- EnvisionRiskRaaS::envrsk_portfolio_hypothetical_performance(
    positions     = p_positions,
    date          = p_date,
    base_cur      = p_base_cur)
  
  # Retain the data we need from API call.
  dt_demo_port_perf <- format_portfolio_perf(demo_port_perf_out)
  
  # Merge the data from the 3 API calls
  dt_demo_port_risk <- merge(
    dt_demo_port_risk,
    dt_demo_port_perf[,.(pathString, Notational, MarketValue, HypotheticalPnL)],
    by = "pathString")
  
  # Retain the data we need from API call.
  dt_demo_port_perf <- format_portfolio_perf(demo_port_perf_out)
  
  if(rsk_msr == "VaR"){
    dt_demo_port_risk <- 
      dt_demo_port_risk[, .(pathString, position_type, label, quantity, 
                            Value_at_Risk, Impact_Value_at_Risk, 
                            Notational, MarketValue, 
                            HypotheticalPnL)]
    
    setnames(dt_demo_port_risk, "Value_at_Risk", "Risk")
    setnames(dt_demo_port_risk, "Impact_Value_at_Risk", "Impact_Risk")
    
    
  } else {
    dt_demo_port_risk <- 
      dt_demo_port_risk[, .(pathString, position_type, label, quantity, 
                            Expected_Shortfall, 
                            Impact_Expected_Shortfall, Notational, MarketValue, 
                            HypotheticalPnL)]
    setnames(dt_demo_port_risk, "Expected_Shortfall", "Risk")
    setnames(dt_demo_port_risk, "Impact_Expected_Shortfall", "Impact_Risk")
  }
  s_risk_reg   <- sum(dt_demo_port_risk[!is.na(quantity)][["Risk"]])
  s_risk_cmp   <- sum(dt_demo_port_risk[!is.na(quantity)][["Impact_Risk"]])
  s_notational <- sum(dt_demo_port_risk[!is.na(quantity)][["Notational"]])
  
  dt_demo_port_risk[, percent := Notational / s_notational]
  dt_demo_port_risk[, diversification := Risk - Impact_Risk ]
  dt_demo_port_risk[, diversification_pct := diversification / Risk]
  dt_demo_port_risk[, risk_pct := abs(Risk / MarketValue)]
  
  dt_demo_port_risk[1, ":=" (diversification = s_risk_reg - s_risk_cmp, 
                             diversification_pct = (s_risk_reg - s_risk_cmp) / s_risk_reg)]
  dt_demo_port_risk[, Impact_Risk_pct := Impact_Risk / s_risk_cmp]
  return(dt_demo_port_risk)
}

create_tree_output <- function(p_dt_port, p_date){
  # p_dt_port <- dt_demo_port_risk
  # p_date <- demo_date
  dt_port <- copy(p_dt_port)
  lst_path_strings_raw <- strsplit(dt_port$pathString, split = "/")
  lst_path_strings_tmp <- lapply(lst_path_strings_raw, function(x){
    x[1] <- as.character(p_date)
    paste(x, collapse = "/")
  })
  lst_path_strings <- do.call(c, lst_path_strings_tmp)
  dt_port[, pathString := lst_path_strings]
  
  #### FORMAT THE RESPONSE ####
  # Transform the portfolio to a data.tree object (more pleasing for the eye)
  tree_out <- data.tree::as.Node(dt_port)
  
  # Sort the data-tree by Stress-test
  data.tree::Sort(tree_out, "position_type", decreasing = TRUE)
  
  # Format the data-tree column 'quantity' and 'ES'
  data.tree::SetFormat(tree_out, "quantity",            formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})
  data.tree::SetFormat(tree_out, "Risk",                formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "Impact_Risk",         formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "Notational",          formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "MarketValue",         formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "HypotheticalPnL",     formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "percent",             formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "diversification",     formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "diversification_pct", formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "risk_pct",            formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "Impact_Risk_pct",     formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  
  # print(tree_out, 
  #       "Position Type"     = "position_type", 
  #       "Instrument Name"   = "label", 
  #       "Quantity"          = "quantity",
  #       "Percent"           = "percent",
  #       "Exposure"          = "Notational",
  #       "Market Value"      = "MarketValue",
  #       "Impact Risk"       = "Impact_Risk_pct",
  #       "Risk"              = "Risk",
  #       "Risk %"            = "risk_pct",
  #       "Diversification"   = "diversification",
  #       "Diversification %" = "diversification_pct",
  #       "Hypothetical PnL"  = "HypotheticalPnL"
  # )
  return(tree_out)
}

#******************************************************************************
#*
#### HELPERS #### 
#*
#******************************************************************************
do_remove_last <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- head(splt_str, -1)
  if(length(splt_str) > 1){
    splt_str <- paste(splt_str, collapse = "//")  
  }
  
  return(splt_str)
}

do_remove_first <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- tail(splt_str, -1)
  if(length(splt_str) > 1){
    splt_str <- paste(splt_str, collapse = "//")  
  }
  
  return(splt_str)
}

get_last_element <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- tail(splt_str, 1)
  
  return(splt_str)
}

get_first_element <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- head(splt_str, 1)
  
  return(splt_str)
}

DataTreeToJson <- function(data_tree, splitter = "//"){
  stopifnot(c("position_type", "quantity") %in% data_tree$attributesAll)
  x <- data.table(ToDataFrameTable(data_tree, "pathString", "position_type", "quantity"))
  x[, location := gsub("/", "//", pathString)]
  x[, symbol := mapply(get_last_element, location, "//")]
  x[, location := mapply(do_remove_first, location, "//")]
  x[, location := mapply(do_remove_last, location, "//")]
  x[, pathString := NULL]
  return(jsonlite::toJSON(x, pretty = TRUE))
}

format_portfolio_risk <- function(envrsk_portfolio_out){
  # Merge 'Output' and 'Positions_Mapped' returned from the API 
  my_port_risk <- merge(
    envrsk_portfolio_out[["Output"]][,.(UID, Location, VaR, ES)],
    envrsk_portfolio_out[["Positions_Mapped"]][,.(uid, position_type, symbol, "label" = name, quantity)],
    by.x = "UID", by.y = "uid",
    all.x = TRUE)
  
  # Remove unwanted columns and format the remaining columns
  my_port_risk[, UID := NULL]
  my_port_risk[!is.na(symbol), pathString := mapply(do_remove_last, Location, "//")]
  my_port_risk[is.na(symbol), pathString := Location]
  my_port_risk[!is.na(symbol), pathString := paste0(pathString, "//", symbol)]
  my_port_risk[, pathString := gsub("//", "/", pathString)]
  my_port_risk[, ":=" (Location = NULL, symbol = NULL)]
  return(my_port_risk)
}

format_portfolio_perf <- function(envrsk_portfolio_out){
  # Merge 'Output' and 'Positions_Mapped' returned from the API 
  my_port_risk <- merge(
    envrsk_portfolio_out[["Output"]][,.(UID, Location, Notational, MarketValue, HypotheticalPnL)],
    envrsk_portfolio_out[["Positions_Mapped"]][,.(uid, position_type, symbol, "label" = name, quantity)],
    by.x = "UID", by.y = "uid",
    all.x = TRUE)
  
  # Remove unwanted columns and format the remaining columns
  my_port_risk[, UID := NULL]
  my_port_risk[!is.na(symbol), pathString := mapply(do_remove_last, Location, "//")]
  my_port_risk[is.na(symbol), pathString := Location]
  my_port_risk[!is.na(symbol), pathString := paste0(pathString, "//", symbol)]
  my_port_risk[, pathString := gsub("//", "/", pathString)]
  my_port_risk[, ":=" (Location = NULL, symbol = NULL)]
  return(my_port_risk)
}
