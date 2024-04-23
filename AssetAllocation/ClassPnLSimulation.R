#------------------------------------------------------------------------------
#-- Retrieve the one-day pnl-simulations for each symbol                     --
#------------------------------------------------------------------------------
#' PnL Simulation Data Fetch
#'
#' This R6 class is designed to fetch and manage profit and loss (PnL) simulation data for given financial symbols over specific reference dates and base currencies.
#'
#' @name PnLSimulationDataFetch
#' @import R6
#' @export
PnLSimulationDataFetch <- R6::R6Class(
  "PnLSimulationDataFetch",
  
  public = list(
    #' Initialize
    #' 
    #' Initializes the PnLSimulationDataFetch object.
    initialize = function(volatility_ids = c("downturn", "through_the_cycle", "point_in_time")) {
      vol_ids            <- unique(volatility_ids[volatility_ids %in% c("downturn", "through_the_cycle", "point_in_time")])
      vol_ids_not_recogn <- unique(volatility_ids[!volatility_ids %in% c("downturn", "through_the_cycle", "point_in_time")])
      
      if(length(vol_ids_not_recogn) > 0){
        warning(paste0("The following volatility_id(s) are not recognized: ", vol_ids_not_recogn))  
      }
      
      if(length(vol_ids) > 0){
        private$volatility_ids <- vol_ids
      } else {
        warning("None of the input volatility_id(s) are recognized. Please choose one or more from {'downturn', 'through_the_cycle', 'point_in_time'}")  
      }
    },
    
    #' Get Volatility_ids
    #' 
    #' Returns the volatility_ids.
    #' @return A character vector of volatility_ids.
    get_volatility_ids = function(){
      return(private$volatility_ids)
    },
    
    #' Get Symbols
    #' 
    #' Returns the financial symbols.
    #' @return A character vector of financial symbols.
    get_symbols = function(){
      return(private$symbs)
    },
    
    #' Set Symbols
    #' 
    #' Sets the financial symbols to fetch data for.
    #' @param vec_symbols A character vector of financial symbols.
    #' @return Invisible.
    set_symbols = function(vec_symbols){
      if(is.null(private$symbs) || !all.equal(private$symbs, vec_symbols)){
        stopifnot(all(is.character(vec_symbols)))
        stopifnot(length(vec_symbols) >= 1)
        private$symbs <- vec_symbols
        invisible(private$in_validate())
      }
    },
    
    #' Get Reference Date
    #' 
    #' Returns the reference date for the data.
    #' @return A Date representing the reference date.
    get_ref_date = function(){
      return(private$ref_date)
    },
    
    #' Set Reference Date
    #' 
    #' Sets the reference date for the data.
    #' @param ref_date A Date object representing the reference date.
    #' @return Invisible.
    set_ref_date = function(ref_date){
      if(!lubridate::is.Date(ref_date)){
        warning()  
      } else {
        if(is.null(private$ref_date) || !all.equal(private$ref_date, ref_date)){
          private$ref_date <- format(ref_date, "%Y-%m-%d")
          invisible(private$in_validate())
        }
      }
    },
    
    #' Get Base Currency
    #' 
    #' Returns the base currency.
    #' @return A character string representing the base currency.
    get_base_cur = function(){
      return(private$base_cur)
    },
    
    #' Set Base Currency
    #' 
    #' Sets the base currency for the data.
    #' @param base_cur A character string representing the base currency.
    #' @return Invisible.
    set_base_cur = function(base_cur){
      if(is.null(private$base_cur) || !all.equal(private$base_cur, base_cur)){
        private$base_cur <- base_cur
        invisible(private$in_validate())
      }
    },
    
    #' Get PnL Simulations
    #' 
    #' Fetches and returns the PnL simulation data.
    #' @return A data.table containing the PnL simulation data.
    get_pnl_simulations = function(){
      stopifnot(!is.null(private$base_cur))
      stopifnot(!is.null(private$ref_date))
      stopifnot(!is.null(private$symbs))
      
      if(is.null(private$dt_pnl_simulations)){
        private$fetch_pnl_sims()
      }
      
      return(private$dt_pnl_simulations)
    }
    
  ),
  private = list(
    symbs              = NULL,
    ref_date           = NULL,
    base_cur           = NULL,
    volatility_ids     = NULL,
    dt_pnl_simulations = NULL,
    
    in_validate = function(){
      private$dt_pnl_simulations = NULL
    },
    
    #' Fetch PnL Simulations
    #' 
    #' Internally fetches the PnL simulation data.
    #' @return Invisible.
    fetch_pnl_sims = function(){
      
      #-------------------------------------------------------------------------------
      #-- Simulated 1-day Profit/Loss Vectors (delta-vectors)                       --
      #-------------------------------------------------------------------------------
      lst_one_day_delta_vector_pct <- pbapply::pblapply(private$volatility_ids, function(x){
        dt_one_day_delta_vector_pct <- get_one_day_pnl_pct(
          unique(private$symbs),
          private$base_cur,
          private$ref_date,
          x)
        dt_one_day_delta_vector_pct[, volatility_id := x]
        dt_one_day_delta_vector_pct
      })
      dt_one_day_delta_vector_pct <- do.call(rbind, lst_one_day_delta_vector_pct)
      stopifnot(all(c("symbol", "scenario", "pnl_pct") %in% colnames(dt_one_day_delta_vector_pct)))
      
      private$dt_pnl_simulations <- dt_one_day_delta_vector_pct
    }
  )
)

LongTermPnLSimulation <- R6::R6Class(
  "LongTermPnLSimulation",
  
  public = list(
    
    initialize = function(tbl_assets_one_day_simulations, tbl_assets_yearly_expected_performance) { # {position_id, scenario_id, volatility_id, pnl_pct}
      
      stopifnot(any(class(tbl_assets_one_day_simulations) %in% c("data.table", "data.frame")))
      stopifnot(any(class(tbl_assets_yearly_expected_performance) %in% c("data.table", "data.frame")))
      stopifnot(check_col_names(tbl_assets_one_day_simulations, c("position_id", "volatility_id", "sim_id", "pnl_pct")))
      stopifnot(check_col_names(tbl_assets_yearly_expected_performance, c("position_id", "volatility_id", "expected_annual_growth_rate")))
      tbl_assets_one_day_simulations_tmp         <- copy(tbl_assets_one_day_simulations[,.(position_id, volatility_id, sim_id, pnl_pct)])
      tbl_assets_yearly_expected_performance_tmp <- copy(tbl_assets_yearly_expected_performance[,.(position_id, volatility_id, expected_annual_growth_rate)])
      
      # Check for consistency - position_id's
      position_ids_asset_sim   <- unique(tbl_assets_one_day_simulations_tmp$position_id)
      position_ids_performance <- unique(tbl_assets_yearly_expected_performance_tmp$position_id)
      stopifnot(all.equal(
        sort(position_ids_asset_sim), 
        sort(position_ids_performance)))
      
      # Check for consistency - volatility_id's
      volatility_ids_asset_sim   <- unique(tbl_assets_one_day_simulations_tmp$volatility_id)
      volatility_ids_performance <- unique(tbl_assets_yearly_expected_performance_tmp$volatility_id)
      stopifnot(all.equal(
        sort(volatility_ids_asset_sim), 
        sort(volatility_ids_performance)))
      
      # Adapt the input dataset to the allowed volatility_ids
      use_volatility_ids                         <- volatility_ids_asset_sim[volatility_ids_asset_sim %in% c("downturn", "through_the_cycle", "point_in_time")]
      stopifnot(length(use_volatility_ids) > 0)
      tbl_assets_one_day_simulations_tmp         <- tbl_assets_one_day_simulations_tmp[volatility_id %in% use_volatility_ids]
      tbl_assets_yearly_expected_performance_tmp <- tbl_assets_yearly_expected_performance_tmp[volatility_id %in% use_volatility_ids]
      
      # Set
      private$dt_assets_one_day_simulations  <- tbl_assets_one_day_simulations_tmp
      private$dt_expected_mean_performance   <- tbl_assets_yearly_expected_performance_tmp
      private$vec_position_ids               <- position_ids_asset_sim
      private$vec_volatility_ids             <- use_volatility_ids
      
      #
      dt_pct_time_spend_in_econ_cycle <- data.table("volatility_id" = private$vec_volatility_ids)
      dt_pct_time_spend_in_econ_cycle[, time_spent_pct := 0.0]
      all_pct_time_spent_allocated <- FALSE
      
      if("through_the_cycle" %in% private$vec_volatility_ids){
        dt_pct_time_spend_in_econ_cycle[
          volatility_id == "through_the_cycle", 
          time_spent_pct := 1.0]  
        all_pct_time_spent_allocated <- TRUE
      } 
      
      if("point_in_time" %in% private$vec_volatility_ids){
        if(all_pct_time_spent_allocated == FALSE){
          dt_pct_time_spend_in_econ_cycle[
            volatility_id == "point_in_time", 
            time_spent_pct := 1.0]  
          all_pct_time_spent_allocated <- TRUE  
        } 
      } 
      
      if("downturn" %in% private$vec_volatility_ids){
        if(all_pct_time_spent_allocated == FALSE){
          dt_pct_time_spend_in_econ_cycle[
            volatility_id == "downturn", 
            time_spent_pct := round(as.numeric(1 * 252 * private$horizon_years, 0))]  
        } 
      }
      
      private$dt_pct_time_spend_in_econ_cycle <- dt_pct_time_spend_in_econ_cycle
      
      dt_days_time_spend_in_econ_cycle <- copy(dt_pct_time_spend_in_econ_cycle)
      setnames(dt_days_time_spend_in_econ_cycle, "time_spent_pct", "time_spent_days")
      dt_days_time_spend_in_econ_cycle[, time_spent_days := NA_integer_]
      private$dt_days_time_spend_in_econ_cycle <- dt_days_time_spend_in_econ_cycle
      
      private$update_days_time_spend_in_econ_cycle()
    },
    
    get_tbl_expected_mean_performance = function(){
      return(private$dt_expected_mean_performance)
    },
    set_tbl_expected_mean_performance = function(tbl_expected_mean_performance){
      stopifnot(check_col_names(tbl_expected_mean_performance, c("position_id", "volatility_id", "performance")))
      dt_expected_mean_performance_tmp   <- copy(tbl_expected_mean_performance[,.(position_id, volatility_id, performance)])
      dt_expected_mean_performance_check <- dt_expected_mean_performance_tmp[,.("count" = length(performance)), by = position_id]
      
      # Check that all position_ids are in the simulation table
      stopifnot(all(dt_expected_mean_performance_check$position_id %in% private$vec_position_ids))
      stopifnot(all(dt_expected_mean_performance_check$volatility_id %in% private$vec_volatility_ids))
      
      # Check that all position_ids has the same number of simulations
      stopifnot(length(unique(dt_expected_mean_performance_check$count)) == 1)
      
      if(!all.equal(dt_expected_mean_performance_tmp, private$dt_expected_mean_performance)){
        private$dt_expected_mean_performance <- dt_expected_mean_performance_tmp
        invisible(private$in_validate())
      }
    },
    
    get_tbl_assets_one_day_simulations = function(){
      return(private$dt_assets_one_day_simulations)
    },
    
    get_tbl_pct_time_spend_in_econ_cycle = function(){
      return(private$dt_pct_time_spend_in_econ_cycle)
    },
    set_tbl_pct_time_spend_in_econ_cycle = function(tbl_pct_time_spend_in_econ_cycle){
      stopifnot(check_col_names(tbl_pct_time_spend_in_econ_cycle, c("volatility_id", "time_spent_pct")))
      dt_pct_time_spend_in_econ_cycle_tmp   <- copy(tbl_pct_time_spend_in_econ_cycle[,.(volatility_id, time_spent_pct)])
      dt_pct_time_spend_in_econ_cycle_tmp   <- dt_pct_time_spend_in_econ_cycle_tmp[volatility_id %in% private$vec_volatility_ids]
      
      # Check that all position_ids are in the simulation table
      stopifnot(all(dt_pct_time_spend_in_econ_cycle_tmp$volatility_id %in% private$vec_volatility_ids))
      if(all.equal(
        dt_pct_time_spend_in_econ_cycle_tmp, 
        private$dt_pct_time_spend_in_econ_cycle,
        ignore.row.order = TRUE,
        check.attributes = FALSE) != TRUE){
          private$dt_pct_time_spend_in_econ_cycle <- dt_pct_time_spend_in_econ_cycle_tmp
          private$update_days_time_spend_in_econ_cycle()
          invisible(private$in_validate())
      }
    },
    
    get_tbl_long_term_pnl = function(){
      if(is.null(private$dt_pnl_simulations)){
        private$do_path_simulation()
      }
      return(private$dt_pnl_simulations)
    },
    get_tbl_expected_annual_growth_rate = function(){
      if(is.null(private$dt_expected_annual_growth_rate)){
        private$do_path_simulation()
      }
      return(private$dt_expected_annual_growth_rate)
    },
    
    get_max_cores = function(){
      return(private$max_use_cpu_cores)
    },
    set_max_cores = function(max_use_cpu_cores){
      stopifnot(is.numeric(max_use_cpu_cores))
      private$max_use_cpu_cores <- ceiling(max_use_cpu_cores)
    },
    
    get_demean = function(){
      return(private$do_demean)
    },
    set_demean = function(boo_demean){
      stopifnot(is.logical(boo_demean))
      if(boo_demean != private$do_demean){
        invisible(private$in_validate())  
      }
      private$do_demean <- boo_demean
    },
    
    get_count_simulations = function(){
      return(private$path_sims)
    },
    set_count_simulations = function(n_simulations){
      stopifnot(is.numeric(n_simulations))
      if(ceiling(n_simulations) != private$path_sims){
        invisible(private$in_validate())  
      }
      private$path_sims <- ceiling(n_simulations)
      
    },
    
    get_horizon_years = function(){
      return(private$horizon_years)
    },
    set_horizon_years = function(dbl_horizon_years){
      stopifnot(is.numeric(dbl_horizon_years))
      stopifnot(dbl_horizon_years > 0)
      if(dbl_horizon_years != private$horizon_years){
        invisible(private$in_validate())  
        private$horizon_years <- dbl_horizon_years
        private$update_days_time_spend_in_econ_cycle()
      }
    }
    
  ),
  private = list(
    dt_assets_one_day_simulations    = NULL,
    dt_expected_mean_performance     = NULL,
    dt_pct_time_spend_in_econ_cycle  = NULL,
    dt_days_time_spend_in_econ_cycle = NULL,
    
    vec_position_ids                 = NULL,
    vec_volatility_ids               = NULL,
    
    max_use_cpu_cores                = 1,
    path_sims                        = 10000,
    do_demean                        = TRUE,
    horizon_years                    = 1,
    
    dt_pnl_simulations               = NULL,
    dt_expected_annual_growth_rate   = NULL,
    
    in_validate                      = function(){
      private$dt_pnl_simulations <- NULL
      private$dt_expected_annual_growth_rate <- NULL
    },
    
    update_days_time_spend_in_econ_cycle = function(){
      dt_days_time_spend_in_econ_cycle <- copy(private$dt_days_time_spend_in_econ_cycle)
      if("through_the_cycle" %in% private$vec_volatility_ids){
        dt_days_time_spend_in_econ_cycle[
          volatility_id == "through_the_cycle", 
          time_spent_days := round(as.numeric(
            private$dt_pct_time_spend_in_econ_cycle[volatility_id == "through_the_cycle"][["time_spent_pct"]] * 252 * private$horizon_years, 0))]
      } 
      
      if("point_in_time" %in% private$vec_volatility_ids){
        dt_days_time_spend_in_econ_cycle[
          volatility_id == "point_in_time", 
          time_spent_days := round(as.numeric(
            private$dt_pct_time_spend_in_econ_cycle[volatility_id == "point_in_time"][["time_spent_pct"]] * 252 * private$horizon_years, 0))]  
      } 
      
      if("downturn" %in% private$vec_volatility_ids){
        dt_days_time_spend_in_econ_cycle[
          volatility_id == "downturn", 
          time_spent_days := round(as.numeric(
            private$dt_pct_time_spend_in_econ_cycle[volatility_id == "downturn"][["time_spent_pct"]] * 252 * private$horizon_years, 0))]  
      }
      
      private$dt_days_time_spend_in_econ_cycle <- dt_days_time_spend_in_econ_cycle
    },
    
    do_path_simulation = function(){
      
      obj_path_sim_test <- long_term_path_simulation(
        tbl_delta_pct                     = private$dt_assets_one_day_simulations,
        tbl_annual_growth_rate            = private$dt_expected_mean_performance,
        tbl_days_time_spend_in_econ_cycle = private$dt_days_time_spend_in_econ_cycle,
        p_sim_paths                       = private$path_sims,
        p_demean                          = private$do_demean,
        n_cores                           = private$max_use_cpu_cores)
      
      private$dt_pnl_simulations             <- obj_path_sim_test[["long_term_pnl_simulations"]]
      private$dt_expected_annual_growth_rate <- obj_path_sim_test[["expected_yearly_performance"]]
    }
  )
)

long_term_path_simulation <- function(
    tbl_delta_pct,
    tbl_annual_growth_rate,
    tbl_days_time_spend_in_econ_cycle,
    p_sim_paths      = 10000,
    p_demean         = TRUE,
    n_cores          = 1){
  
  
  SimulatePath <- function(number_of_days, p_tbl_delta_pct){
    # p_tbl_delta_pct <- dt_delta_vec_pct[volatility_id == "bull_run"]
    # number_of_days <- 756
    dt_path_sampl <- data.table(
      "sim_id" = sample(1:10000, 
                        number_of_days, 
                        replace = TRUE))
    
    dt_path_scenarios <- p_tbl_delta_pct[sim_id %in% dt_path_sampl$sim_id]
    dt_path_scenarios[, pnl_pct := pnl_pct + 1]
    dt_out <- dt_path_scenarios[, .("pnl_pct" = prod(pnl_pct)), by = "position_id"]
    
    return(dt_out)
  }
  
  # tbl_delta_pct                <- tbl_one_day_delta_vector_pct
  # tbl_annual_growth_rate       <- tbl_expected_performance
  # tbl_time_spent_vol_scenarios <- dt_time_spend_in_econ_cycle
  #******************************************************************************
  #*
  #### Adjust the expected annual growth rate for each instrument ####
  #*
  #******************************************************************************
  dt_implied_sim_performance <- tbl_delta_pct[, list(average = mean(pnl_pct)), by=c("volatility_id", "position_id")]
  
  dt_delta_vec_pct <- merge(
    tbl_delta_pct,
    dt_implied_sim_performance,
    by = c("volatility_id", "position_id"),
    all.x = TRUE)
  dt_delta_vec_pct[, pnl_pct_adj := pnl_pct - average]
  dt_delta_vec_pct[, ":=" (pnl_pct = NULL, average = NULL)]
  
  
  dt_delta_vec_pct <- merge(
    dt_delta_vec_pct,
    tbl_annual_growth_rate[,.(position_id, volatility_id, expected_annual_growth_rate)],
    by = c("volatility_id", "position_id"),
    all.x = TRUE)
  
  # Adjust the annual growth expectation to daily
  dt_delta_vec_pct[, daily_growth_rate := ((1+expected_annual_growth_rate)^(1/252) - 1)]
  
  # Do the adjustment by adjusting the daily trend-component
  dt_delta_vec_pct[, pnl_pct_adj := pnl_pct_adj + daily_growth_rate] 
  setnames(dt_delta_vec_pct, "pnl_pct_adj", "pnl_pct")
  
  dt_delta_vec_pct[, ":=" (expected_annual_growth_rate = NULL, daily_growth_rate = NULL)]
  setorder(dt_delta_vec_pct, "volatility_id", "position_id", "sim_id")
  
  #******************************************************************************
  #*
  #### GO PARALLEL - Greatly speed up the path simulations with multiple CPU's ####
  #*
  #******************************************************************************
  simulations_max   <- p_sim_paths
  simulations_descr <- "path"
  
  #n_cores <- parallel::detectCores()-1
  
  #*** Boiler Plate Code for the Parallel Execution ***
  use_n_cores <- max(1, min(parallel::detectCores(), n_cores))
  cl      <- parallel::makeCluster(use_n_cores)
  doSNOW::registerDoSNOW(cl)
  
  #*** Boiler Plate Code - END ***
  
  # Iterate
  lst_sim_paths <- lapply(unique(tbl_days_time_spend_in_econ_cycle$volatility_id), function(vol_id){
    message(paste0("Performing path generating for '", vol_id, "' using ", use_n_cores, " cpu-cores."))
    
    use_days         <- tbl_days_time_spend_in_econ_cycle[volatility_id == vol_id][["time_spent_days"]]
    use_dt_delta_ved <- dt_delta_vec_pct[volatility_id == vol_id]
    
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
    
    
    dt_sim_paths_vol <- foreach::foreach(
      i             = 1:simulations_max, 
      .export       = c("SimulatePath"), 
      .packages     = c("data.table"), 
      .combine      = rbind, 
      .options.snow = opts) %dopar% {
        dt_out <- SimulatePath(
          use_days, 
          use_dt_delta_ved)
        dt_out[, sim := i]
    }
    dt_sim_paths_vol[, volatility_id := vol_id]
    
    # Terminate the progressbar
    pb$terminate()
    
    dt_sim_paths_vol
  })
  dt_sim_paths <- rbindlist(lst_sim_paths)
  setnames(dt_sim_paths, "sim", "path_scenario")
  dt_sim_paths <- dt_sim_paths[, .(pnl_pct = prod(pnl_pct)), by = c("position_id", "path_scenario")]
  
  #### Return expected_yearly_performance based on the chosen volatility scenarios ####
  dt_expected_yearly_performance <- setDT(dt_sim_paths)[, .(mean = mean(pnl_pct)), by = c("position_id")]
  horizon_years <- sum(tbl_days_time_spend_in_econ_cycle[["time_spent_days"]])/252
  dt_expected_yearly_performance[, expected_annual_growth_rate := ((mean)^(1/horizon_years) - 1)]
  dt_expected_yearly_performance[, mean := NULL]
  
  #### ####
  
  if(p_demean == TRUE){
    dt_means <- setDT(dt_sim_paths)[, .(mean = mean(pnl_pct) - 1), by = c("position_id")]
    dt_sim_paths <- merge(
      dt_sim_paths,
      dt_means,
      by = c("position_id"),
      all.x = TRUE) 
    dt_sim_paths[, pnl_pct  := pnl_pct  - mean]
    dt_sim_paths[, mean := NULL]
  }
  
  # Stop the cluster
  snow::stopCluster(cl)
  closeAllConnections()
  
  dt_sim_paths[, pnl_pct := pnl_pct - 1]
  
  return(list(
    "expected_yearly_performance" = dt_expected_yearly_performance,
    "long_term_pnl_simulations"   = dt_sim_paths[,.(position_id, path_scenario, pnl_pct)]))
}
