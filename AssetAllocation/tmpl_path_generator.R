#******************************************************************************
#*
#### FUNCTIONALITY ####
#*
#******************************************************************************
sim_asset_one_day_pnl_distributions <- function(p_symbols,
                                                p_volatility_scenario,
                                                p_base_cur,
                                                p_date){
  #******************************************************************************
  #*
  #### REVISED PRICE SIMULATION SCENARIO (as of the reference date) #### 
  #*
  #******************************************************************************
  # This serves as the initial framework for projecting future market trends. We 
  # select the reference date strategically to align with a dynamic volatility 
  # level that we consider suitable for forecasting future price movements 
  # and trends
  iter_delta_api_calls <- length(p_symbols) %/% 25 + 1
  dt_symbol_iter       <- data.table(
    "symbol" = p_symbols,
    "batch"  = sort(rep(1:iter_delta_api_calls, 25))[1:length(p_symbols)])
  
  lst_delta_vectors <- lapply(1:iter_delta_api_calls, function(y){
    selected_symbs <- dt_symbol_iter[batch == y]$symbol
    lst_delta_vector <- EnvisionRiskRaaS::envrsk_instrument_delta_vector(
      date          = p_date,
      symbols       = selected_symbs,
      base_cur      = p_base_cur,
      horizon       = 1,
      volatility_id = p_volatility_scenario) 
    dt_delta_vector <- lst_delta_vector[["Output"]][,.(SYMBOL, SCENARIO, PnL)]
  })
  dt_delta_vector <- rbindlist(lst_delta_vectors)
  
  # Rename the columns
  setnames(dt_delta_vector, "SYMBOL", "symbol")
  setnames(dt_delta_vector, "SCENARIO", "scenario")
  setnames(dt_delta_vector, "PnL", "pnl")
  
  return(dt_delta_vector)
} 

get_one_day_pnl_pct <- function(p_symbols, p_base_cur, p_ref_date, p_volatility_scenario){
  dt_asset_prices <- get_asset_prices(p_symbols, 
                                      p_base_cur)
  dt_asset_price  <- dt_asset_prices[date <= p_ref_date, .(symbol, date, close)]
  dt_asset_price  <- setDT(dt_asset_price)[, .SD[which.max(date)], by=symbol]
  
  dt_delta_vector <- sim_asset_one_day_pnl_distributions(
    p_symbols,
    p_volatility_scenario,
    p_base_cur,
    p_ref_date)
  
  dt_delta_vector_pct <- merge(dt_delta_vector,
                               dt_asset_price[, .(symbol, close)],
                               by    = "symbol",
                               all.x = TRUE)
  dt_delta_vector_pct[, pnl_pct := pnl/close]
  dt_delta_vector_pct[, ":=" (pnl = NULL, close = NULL)]
  return(dt_delta_vector_pct)
}

path_simulations <- function(p_dt_delta_pct,
                             p_dt_annual_growth_rate,
                             p_sim_years,
                             p_sim_paths = 10000,
                             p_demean    = TRUE,
                             p_prob      = NULL){
  
  # rm(p_dt_delta_pct) <- dt_one_day_delta_vector_pct[,.(symbol, scenario, pnl_pct)]
  # rm(p_dt_annual_growth_rate) <- dt_input_portfolio[,.(symbol, expected_annual_growth_rate)]
  # rm(p_sim_years) <- sim_years
  # rm(p_sim_paths) <- sim_paths
  
  #******************************************************************************
  #*
  #### FORWARD PATH SIMULATION FUNCTIONALITY ####
  #*
  #******************************************************************************
  # Functionality to simulate each path and extract the end state of each simulation
  SimulatePaths <- function(sim_years, dt_delta_pct, smpl_prob = NULL){
    # Don't change the 10.000 (in 1:10000 below) as it refers to the 
    # pnl-simulations - not to the number of path-simulations.
    if(is.null(smpl_prob) || length(smpl_prob) != 10000){
      smpl_prob <- rep(1/10000, 10000)
    }
    
    dt_path_sampl <- data.table("scenario" = sample(1:10000, sim_years*252, replace = TRUE, prob = smpl_prob))
    
    dt_sim_path <- merge(
      dt_path_sampl,
      dt_delta_pct, 
      by = "scenario",
      all.x = TRUE,
      sort = FALSE)
    # Labeling the path scenarios
    dt_sim_path[, path_scenario := sort(rep(1:(sim_years * 252), length(unique(dt_delta_pct$symbol))))]
    # Change the values so the can be multiplied later on.
    dt_sim_path[, delta_change := 1+pnl_pct]
    setorder(dt_sim_path, "symbol", "path_scenario")
    # Accumulate the simulated PnL
    dt_sim_path[, cum_delta_change := cumprod(delta_change), by = "symbol"]
    return(dt_sim_path)
  }
  
  # For each simulated path, only the outcome from the chosen steps 
  # ('extract_steps') is kept. 
  ExtractEndPath <- function(sim_years, dt_delta_pct, extract_steps = NULL, smpl_prob = NULL){
    dt_sim_path <- SimulatePaths(sim_years, dt_delta_pct, smpl_prob)
    if(is.null(extract_steps)){
      dt_out <- dt_sim_path[path_scenario == sim_years*252, .(symbol, path_scenario, cum_delta_change)]
    } else {
      dt_out <- dt_sim_path[path_scenario %in% extract_steps, .(symbol, path_scenario, cum_delta_change)]
    }
    return(dt_out)
  }
  
  #******************************************************************************
  #*
  #### Adjust the expected annual growth rate for each instrument ####
  #*
  #******************************************************************************
  dt_implied_sim_performance <- p_dt_delta_pct[,list(average = mean(pnl_pct)), by=symbol]
  
  dt_delta_vec_pct <- merge(
    p_dt_delta_pct,
    dt_implied_sim_performance,
    by = "symbol",
    all.x = TRUE)
  dt_delta_vec_pct[, pnl_pct_adj := pnl_pct - average]
  dt_delta_vec_pct[, ":=" (pnl_pct = NULL, average = NULL)]

  dt_delta_vec_pct <- merge(
    dt_delta_vec_pct,
    p_dt_annual_growth_rate[,.(symbol, expected_annual_growth_rate)],
    by = "symbol",
    all.x = TRUE)
  
  # Adjust the annual growth expectation to daily
  dt_delta_vec_pct[, average := ((1+expected_annual_growth_rate)^(1/252) - 1)]
  
  # Do the adjustment by adjusting the daily trend-component
  dt_delta_vec_pct[, pnl_pct_adj := pnl_pct_adj + average] 
  setnames(dt_delta_vec_pct, "pnl_pct_adj", "pnl_pct")
  
  dt_delta_vec_pct[, ":=" (expected_annual_growth_rate = NULL, average = NULL)]
  
  #******************************************************************************
  #*
  #### GO PARALLEL - Greatly speed up the path simulations with multiple CPU's ####
  #*
  #******************************************************************************
  simulations_max   <- p_sim_paths
  simulations_descr <- "path"
  
  # Extract 5 equidistant time points to show evolution simulated risk and 
  # performance evolution over time.
  set_extract_steps <- 1:p_sim_years * 252
  
  message("Setting up the cluster")
  n_cores <- parallel::detectCores()-1
  
  #*** Boiler Plate Code for the Parallel Execution ***
  cl      <- parallel::makeCluster(n_cores)
  doSNOW::registerDoSNOW(cl)
  
  # progress bar ------------------------------------------------------------
  # token reported in progress bar
  paths <- paste(as.character(1:simulations_max), 
                 as.character(simulations_max), sep= "/")  
  
  # allowing progress bar to be used in foreach -----------------------------
  progress <- function(n){
    pb$tick(tokens = list(paths_process = paths[n]))
  } 
  opts <- list(progress = progress)
  
  # foreach loop ------------------------------------------------------------
  # Reset pb for each run
  pb <- progress::progress_bar$new(
    format = paste0(simulations_descr, "_processed = :paths_process [:bar] :elapsed | eta: :eta"),
    total = simulations_max,    
    width = 60)
  
  #*** Boiler Plate Code - END ***
  
  # Iterate
  message("Performing path generating")
  dt_sim_paths <- foreach::foreach(i = 1:simulations_max, .packages = c("data.table"), .combine = rbind, .options.snow = opts) %dopar% {
    dt_out <- ExtractEndPath(p_sim_years, dt_delta_vec_pct, extract_steps = set_extract_steps, smpl_prob = p_prob)
    dt_out[, sim := i]
  }
  
  if(p_demean == TRUE){
    dt_means <- setDT(dt_sim_paths)[, .(mean = mean(cum_delta_change) - 1), by = c("symbol", "path_scenario")]
    dt_sim_paths <- merge(
     dt_sim_paths,
     dt_means,
     by = c("symbol", "path_scenario"),
     all.x = TRUE) 
    dt_sim_paths[, cum_delta_change := cum_delta_change - mean]
    dt_sim_paths[, mean := NULL]
  }
  
  # Terminate the progressbar
  pb$terminate()
  
  # Stop the cluster
  snow::stopCluster(cl) 
  #closeAllConnections()
  
  return(dt_sim_paths)
}

create_portfolio_path <- function(p_dt_portfolio){
  # {symbol, path_scenario, cum_delta_change, sim, weight}
  stopifnot(all(colnames(p_dt_portfolio) %in% c("symbol", "path_scenario", "cum_delta_change", "sim", "weight")))
  dt_sim_paths_portfolio <- copy(p_dt_portfolio)
  
  # Weight the simulated values
  dt_sim_paths_portfolio[, weighted_delta_change := cum_delta_change * weight]
  dt_sim_paths_cast <- dcast.data.table(dt_sim_paths_portfolio, path_scenario + sim ~ symbol, value.var = "weighted_delta_change")
  # Sum the pnl over the individual tickers (but exclude the first two columns (not ticker values))
  dt_sim_paths_cast[, Portfolio := apply(dt_sim_paths_cast[,c(-1, -2)], 1, "sum")]
  # Transform the output back to the same format as it was
  dt_sim_paths_portfolio <- melt.data.table(dt_sim_paths_cast, id.vars = c("path_scenario", "sim"))
  dt_sim_paths_portfolio <- dt_sim_paths_portfolio[variable == "Portfolio"]
  # Rename the columns as before
  setnames(dt_sim_paths_portfolio, "variable", "symbol")
  setnames(dt_sim_paths_portfolio, "value", "cum_delta_change")
  return(dt_sim_paths_portfolio)
}

#******************************************************************************
#*
#### USER INPUT ####
#*
#******************************************************************************
# Symbols/tickers & weight 
# {symbol, weight, location, expected_annual_growth_rate}
# dt_input_portfolio <- data.table(readRDS(url("https://www.dropbox.com/scl/fi/ays28w9vu3piwiifbxnqy/portfolio_by_asset_classes.rds?rlkey=g52i0roki5sln4eplv98w2lpm&raw=true","rb")))[,.(symbol, weight, location, expected_annual_growth_rate)]
# 
# # Base Currency: 
# base_cur <- "DKK"
# 
# # Volatility scenario (alternatives: use "point_in_time", "downturn", 
# # "through_the_cycle", "severe_stress" or "extreme_stress"). In case 
# # "point_in_time" scenario is used the reference date becomes important since 
# # the volatility on the reference date is used. 
# rsk_volatility_scenario <- "through_the_cycle"
# 
# # Reference date: 
# ref_date <- as.Date("2024-01-31")
# 
# # Set simulated paths (where each path-simulation is based on the steps chosen above)
# sim_paths <- 10000
# 
# # Set simulated path-steps (252 steps is equivivalent to one year) 
# sim_years <- 10
# 
# #Confidence level: 
# rsk_conf_level <- 0.975

#******************************************************************************
#*
#### OUTPUT - DELTA VECTORS ####
#*
#******************************************************************************
# dt_one_day_delta_vector_pct <- get_one_day_pnl_pct(
#   unique(dt_input_portfolio$symbol),
#   base_cur,
#   ref_date,
#   rsk_volatility_scenario)
# 
# stopifnot(all(c("symbol", "scenario", "pnl_pct") %in% colnames(dt_one_day_delta_vector_pct)))
# stopifnot(all(c("symbol", "expected_annual_growth_rate") %in% colnames(dt_input_portfolio)))
# 
# dt_horizon_delta_vector_indexed <- path_simulations(
#   dt_one_day_delta_vector_pct[,.(symbol, scenario, pnl_pct)],
#   dt_input_portfolio[,.(symbol, expected_annual_growth_rate)],
#   sim_years,
#   sim_paths)

#******************************************************************************
#*
#### LONG TERM PERFORMANCE AND RISK ASSESMENT (1 - 10 years)
#*
#******************************************************************************
# dt_horizon_delta_vector_indexed_decorated <- merge(
#   dt_horizon_delta_vector_indexed,
#   dt_input_portfolio[,.(symbol, weight)], 
#   by = "symbol",
#   all.x = TRUE)
# 
# dt_sim_paths_portfolio <- create_portfolio_path(dt_horizon_delta_vector_indexed_decorated)
# 
# dt_sim_paths_portfolio_splt   <- split(dt_sim_paths_portfolio, dt_sim_paths_portfolio$path_scenario)
# lst_risk_perf_input_portfolio <- lapply(dt_sim_paths_portfolio_splt, function(tbl){
#   value_at_risk      <- ValueAtRisk(tbl[["cum_delta_change"]], rsk_conf_level)
#   expected_shortfall <- ExpectedShortfall(tbl[["cum_delta_change"]], rsk_conf_level)
#   perf               <- mean(tbl[["cum_delta_change"]])
#   dt_out             <- data.table("ValueAtRisk"       = value_at_risk,
#                                    "ExpectedShortfall" = expected_shortfall,
#                                    "PnL"               = perf)
#   
#   return(dt_out)
# })
# dt_risk_perf_input_portfolio <- rbindlist(lst_risk_perf_input_portfolio)
# dt_risk_perf_input_portfolio[, year := 1:sim_years]
