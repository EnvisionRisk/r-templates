#### Risk Budgeting Portfolios from Simulations (2023) ####
# Sample Average Approximation (SAA). It should be noted that we use SAA 
# to find an approximate solution to optimisation problem, and therefore we 
# only need to approximate the risk measure. 

check_col_names <- function(tbl_to_be_checked, col_names){
  stopifnot(any(class(tbl_to_be_checked) %in% c("data.table", "data.frame", "matrix")))
  return(all(col_names %in% colnames(tbl_to_be_checked)))
}
tbl_upsert <- function(tbl_parent, tbl_child){
  stopifnot(any(class(tbl_parent) %in% c("data.table", "data.frame")))
  stopifnot(any(class(tbl_child) %in% c("data.table", "data.frame")))
  
  tbl_parent <- data.table(tbl_parent)
  tbl_child  <- data.table(tbl_child)
  
  stopifnot(all.equal(colnames(tbl_parent), colnames(tbl_child)))
  
  dt_set_intersect <- data.table::fintersect(
    tbl_parent,
    tbl_child)
  
  dt_set_diff <- data.table::fsetdiff(
    tbl_child,
    tbl_parent)
  
  return(
    rbind(
      dt_set_intersect,
      dt_set_diff))
}

MeanCVaR <- R6::R6Class(
  "MeanCVaR",

  public = list(

    initialize = function(tbl_portfolio_positions) { # {position_id, current_weight}
      stopifnot(any(class(tbl_portfolio_positions) %in% c("data.table", "data.frame")))
      stopifnot(check_col_names(tbl_portfolio_positions, c("position_id", "current_weight")))
      stopifnot(length(tbl_portfolio_positions[["position_id"]]) == length(unique(tbl_portfolio_positions[["position_id"]])))
      stopifnot(nrow(tbl_portfolio_positions[is.na(current_weight), ]) == 0)
      dt_portfolio_positions_tmp   <- copy(tbl_portfolio_positions[,.(position_id, current_weight)])
      dt_portfolio_positions_check <- dt_portfolio_positions_tmp[,.("count" = length(current_weight)), by = position_id]
      
      # Check that all position_ids are unique
      stopifnot(length(unique(dt_portfolio_positions_check$count)) == 1)
      
      private$dt_portfolio_positions <- dt_portfolio_positions_tmp
      private$vec_position_ids       <- tbl_portfolio_positions$position_id
      
      private$dt_expected_mean_performance <- data.table("position_id" = private$vec_position_ids)
      private$dt_expected_mean_performance[, ":=" (performance = NA_real_)]
      
      private$dt_portfolio_location <- data.table("position_id" = private$vec_position_ids)
      private$dt_portfolio_location[, ":=" (location = NA_character_)]
      
      # Initialize Constraints
      private$dt_positions_weight_constraints <- data.table("position_id" = private$vec_position_ids)
      private$dt_positions_weight_constraints[, ":=" (min_weight = 0.0,
                                                      max_weight = 1.0)]
      
      private$cardinality_constraint_lower <- length(private$vec_position_ids)
      private$cardinality_constraint_upper <- length(private$vec_position_ids)
      
      # Settings
      private$max_use_cpu_cores <- max(parallel::detectCores()-1, 1)
    },
    
    get_tbl_pnl_simulation = function(){
      tbl_pnl_simulations_tmp <- copy(private$dt_pnl_simulations)
      tbl_pnl_simulations_tmp[, scenario_id := 1:nrow(tbl_pnl_simulations_tmp)]
      tbl_pnl_simulations_tmp <- melt.data.table(
        tbl_pnl_simulations_tmp, 
        id.vars = "scenario_id")
      setnames(tbl_pnl_simulations_tmp, "variable", "position_id")
      setnames(tbl_pnl_simulations_tmp, "value", "pnl_pct")
      return(tbl_pnl_simulations_tmp[, .(position_id, scenario_id, pnl_pct)])
    },
    set_tbl_pnl_simulation = function(tbl_pnl_simulations){
      # Check that all column names are inthe table
      stopifnot(check_col_names(tbl_pnl_simulations, c("position_id", "scenario_id", "pnl_pct")))
      dt_pnl_simulations_tmp   <- copy(tbl_pnl_simulations[,.(position_id, scenario_id, pnl_pct)])
      dt_pnl_simulations_check <- dt_pnl_simulations_tmp[,.("count" = length(pnl_pct)), by = position_id]
      
      # Check that all position_ids are in the simulation table
      stopifnot(all(private$vec_position_ids %in% dt_pnl_simulations_check$position_id))
      
      # Check that all position_ids has the same number of simulations
      stopifnot(length(unique(dt_pnl_simulations_check$count)) == 1)
      
      # Cast the table to the internal format used
      dt_pnl_simulations_tmp <- data.table::dcast.data.table(tbl_pnl_simulations, 
                                                             scenario_id ~ position_id, 
                                                             value.var = "pnl_pct")
      
      # Check that all position_id have the same scenario_ids
      stopifnot(nrow(dt_pnl_simulations_tmp) == dt_pnl_simulations_check$count[1])
      
      # Set the internal simulation table
      private$dt_pnl_simulations <- dt_pnl_simulations_tmp[, -1]
      invisible(private$in_validate())
    },
    
    get_tbl_expected_mean_performance = function(){
      return(private$dt_expected_mean_performance)
    },
    set_tbl_expected_mean_performance = function(tbl_expected_mean_performance){
      stopifnot(check_col_names(tbl_expected_mean_performance, c("position_id", "performance")))
      stopifnot(nrow(tbl_expected_mean_performance[is.na(performance), ]) == 0)
      dt_expected_mean_performance_tmp   <- copy(tbl_expected_mean_performance[,.(position_id, performance)])
      dt_expected_mean_performance_check <- dt_expected_mean_performance_tmp[,.("count" = length(performance)), by = position_id]
      
      # Check that all position_ids are in the simulation table
      stopifnot(all(dt_expected_mean_performance_check$position_id %in% private$vec_position_ids))
      
      # Check that all position_ids has the same number of simulations
      stopifnot(length(unique(dt_expected_mean_performance_check$count)) == 1)
      
      private$dt_expected_mean_performance <- dt_expected_mean_performance_tmp
      invisible(private$in_validate())
    },
    
    get_tbl_portfolio_location = function(){
      return(private$dt_portfolio_location)
    },
    set_tbl_portfolio_location = function(tbl_portfolio_location){
      stopifnot(check_col_names(tbl_portfolio_location, c("position_id", "location")))
      dt_portfolio_location_tmp   <- copy(tbl_portfolio_location[,.(position_id, location)])
      dt_portfolio_location_check <- dt_portfolio_location_tmp[,.("count" = length(location)), by = position_id]
      
      # Check that all position_ids are in the simulation table
      stopifnot(all(dt_portfolio_location_check$position_id %in% private$vec_position_ids))
      
      # Check that all position_ids has the same number of simulations
      stopifnot(length(unique(dt_portfolio_location_check$count)) == 1)
      
      private$dt_portfolio_location <- dt_portfolio_location_tmp
      
      if(!is.null(private$has_risk_budget_constraint) && private$has_risk_budget_constraint == TRUE){
        invisible(private$in_validate())  
      }
    },
    
    get_tbl_positions_weight_constraints = function(){
      return(private$dt_positions_weight_constraints)
    },
    set_tbl_positions_weight_constraints = function(tbl_positions_weight_constraints){
      stopifnot(check_col_names(tbl_positions_weight_constraints, c("position_id", "min_weight", "max_weight")))
      dt_positions_weight_constraints_tmp   <- copy(tbl_positions_weight_constraints[,.(position_id, min_weight, max_weight)])
      dt_positions_weight_constraints_check <- dt_positions_weight_constraints_tmp[,.("count" = length(min_weight)), by = position_id]
      
      # Check that all position_ids are unique
      stopifnot(all(dt_positions_weight_constraints_check$position_id %in% private$vec_position_ids))
      
      # Check that all position_ids has the same number of simulations
      stopifnot(length(unique(dt_positions_weight_constraints_check$count)) == 1)
      
      private$dt_positions_weight_constraints <- dt_positions_weight_constraints_tmp
      private$dt_positions_weight_constraints[min_weight < 0.0, min_weight := 0.0]
      private$dt_positions_weight_constraints[min_weight > max_weight, min_weight := 0.0]
      private$dt_positions_weight_constraints[max_weight > 1.0, max_weight := 1.0]
      
      if(any(private$dt_positions_weight_constraints[["min_weight"]] > 0.0) | 
         any(private$dt_positions_weight_constraints[["max_weight"]] < 1.0)){
        private$has_position_weight_constraint <- TRUE  
      } else {
        private$has_position_weight_constraint <- FALSE  
      }
      invisible(private$in_validate())
    },
    
    get_tbl_risk_budget_constraints = function(){
      return(
        list(
          "note" = "The implementation of risk-budget constraints is articulated as an inequality condition, based on the premise that the risk attributed to any given group must not surpass the specified risk-budget limit, although it may be less than this threshold.",
          "risk_budget_constraints" = private$dt_risk_budget_constraints))
    },
    set_tbl_risk_budget_constraints = function(tbl_risk_budget_constraints){
      stopifnot(all(!is.na(private$dt_portfolio_location[["location"]])))
      stopifnot(check_col_names(tbl_risk_budget_constraints, c("group", "constraint")))
      tbl_risk_budget_constraints <- unique(data.table(tbl_risk_budget_constraints), by = "group")
      
      dt_risk_budget_constraints <- private$dt_risk_budget_constraints
      if(is.null(dt_risk_budget_constraints)){
        
        risk_budget_groups    <- unique(as.character(
          sapply(
            unique(private$dt_portfolio_location[["location"]]), 
            get_first_element, 
            splitter = "//")))
        
        dt_risk_budget_constraints <- data.table("group"      = risk_budget_groups,
                                                 "constraint" = 1.0)
      }
      
      stopifnot(all(tbl_risk_budget_constraints$group %in% dt_risk_budget_constraints$group))
      
      dt_risk_budget_constraints <- rbind(
        dt_risk_budget_constraints[!(dt_risk_budget_constraints[["group"]] %in% 
                                     tbl_risk_budget_constraints[["group"]])],
        tbl_risk_budget_constraints)
      
      stopifnot(sum(dt_risk_budget_constraints[["constraint"]]) >= 1)  
      private$dt_risk_budget_constraints <- dt_risk_budget_constraints
      
      if(all(private$dt_risk_budget_constraints[["constraint"]] == 1.0)){
        private$has_risk_budget_constraint <- FALSE  
        private$dt_risk_budget_constraints <- NULL
      } else {
        private$has_risk_budget_constraint <- TRUE  
      }
      invisible(private$in_validate())
    },
    
    get_cardinality_constraints = function(){
      return(c("lower" = private$cardinality_constraints_lower,
               "upper" = private$cardinality_constraints_upper))
    },
    set_cardinality_constraints = function(cardinality_lower = NULL, cardinality_upper = NULL){
      if(!all(is.null(c(cardinality_lower, cardinality_upper)))){
        if(!is.null(cardinality_upper)){
          stopifnot(cardinality_upper > 0)
          stopifnot(cardinality_upper <= length(private$vec_position_ids))
          private$cardinality_constraint_upper <- cardinality_upper
        }
        
        if(!is.null(cardinality_lower)){
          stopifnot(cardinality_lower > 0)
          stopifnot(cardinality_lower <= length(private$vec_position_ids))
          private$cardinality_constraint_lower <- cardinality_lower
        }
        
        private$cardinality_constraint_lower <- min(private$cardinality_constraint_lower, 
                                                    private$cardinality_constraint_upper)
        
        if(private$cardinality_constraint_lower == private$cardinality_constraint_upper & 
           private$cardinality_constraint_upper == length(private$vec_position_ids)){
          private$has_cardinality_constraint <- FALSE  
        } else {
          private$has_cardinality_constraint <- TRUE  
        }
        
        invisible(private$in_validate())
      } 
    },
    
    reset_position_weight_constraint = function(){
      private$dt_positions_weight_constraints <- data.table("position_id" = private$vec_position_ids)
      private$dt_positions_weight_constraints[, ":=" (min_weight = 0.0,
                                                      max_weight = 1.0)]
      has_position_weight_constraint           <- FALSE
      
      invisible(private$in_validate())
    },
    reset_cardinality_constraint = function(){
      private$cardinality_constraint_lower <- length(private$vec_position_ids)
      private$cardinality_constraint_upper <- length(private$vec_position_ids)
      has_cardinality_constraint           <- FALSE
      invisible(private$in_validate())
    },
    reset_risk_budget_constraint = function(){
      private$dt_risk_budget_constraints <- NULL
      has_risk_budget_constraint         <- FALSE
      invisible(private$in_validate())
    },
    
    get_max_cores = function(){
      return(private$max_use_cpu_cores)
    },
    set_max_cores = function(max_use_cpu_cores){
      stopifnot(is.numeric(max_use_cpu_cores))
      private$max_use_cpu_cores <- ceiling(max_use_cpu_cores)
    },
    
    get_count_simulations = function(){
      return(private$n_sim)
    },
    set_count_simulations = function(n_simulations){
      stopifnot(is.numeric(n_simulations))
      if(ceiling(n_simulations) != private$n_sim){
        invisible(private$in_validate())  
      }
      private$n_sim <- ceiling(n_simulations)
      
    },
    
    get_risk_free_interest_rate = function(){
      return(private$rsk_free_rate)
    },
    set_risk_free_interest_rate = function(rsk_free_rate){
      stopifnot(is.numeric(rsk_free_rate))
      if(rsk_free_rate != private$rsk_free_rate){
        invisible(private$in_validate())  
      }
      private$rsk_free_rate <- rsk_free_rate
    },
    
    get_risk_confidence_level = function(){
      return(private$rsk_conf_level)
    },
    set_risk_confidence_level = function(risk_confidence_level){
      stopifnot(is.numeric(risk_confidence_level))
      if(risk_confidence_level != private$rsk_conf_level){
        invisible(private$in_validate())  
      }
      
      if(risk_confidence_level < 0.9){
        warning("The risk_confidence_level must meet or exceed a threshold of 0.90. Currently, the risk_confidence_level is configured to 0.9.")
        private$rsk_conf_level <- 0.90
      }
      if(risk_confidence_level > 0.99){
        warning("The risk_confidence_level must be lower or meet a threshold of 0.99. Currently, the risk_confidence_level is configured to 0.99.")
        private$rsk_conf_level <- 0.99
      }
    },
    
    get_pnl_is_log_transformed = function(){
      return(private$pnl_is_log_transformed)
    },
    set_pnl_is_log_transformed = function(bool_pnl_is_log_transformed){
      stopifnot(is.logical(bool_pnl_is_log_transformed))
      if(bool_pnl_is_log_transformed != private$pnl_is_log_transformed){
        invisible(private$in_validate())  
      }
      private$pnl_is_log_transformed <- bool_pnl_is_log_transformed
    },
    
    perform_asset_allocation_analysis = function(){
      private$in_validate()
      private$do_perform_asset_allocation_analysis()
    },
    
    get_market_portfolio = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["market_portfolio"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["market_portfolio"]])
      }
    },
    get_efficient_frontier = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["efficient_frontier"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["efficient_frontier"]])
      }
    },
    get_portfolio_tangent_line = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["portfolio_tangent_line"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["portfolio_tangent_line"]])
      }
    },
    get_summary_market_portfolio_by_risk_budget = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["summary_market_portfolio_by_risk_budget"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["summary_market_portfolio_by_risk_budget"]])
      }
    },
    get_summary_market_portfolio = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["summary_market_portfolio"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["summary_market_portfolio"]])
      }
    },
    get_summary_market_portfolio_by_positions = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$asset_allocation_analysis[["summary_market_portfolio_by_positions"]])  
      } else {
        private$do_perform_asset_allocation_analysis()
        return(private$asset_allocation_analysis[["summary_market_portfolio_by_positions"]])
      }
    },
    get_simulated_portfolio_weights = function(){
      if(private$has_asset_allocation_analysis == TRUE){
        return(private$dt_simulate_portfolio_weights)    
      } else{
        private$do_perform_asset_allocation_analysis()
        return(private$dt_simulate_portfolio_weights)
      }
    }
  ),
  private = list(
    dt_portfolio_positions          = NULL,
    dt_pnl_simulations              = NULL,
    dt_expected_mean_performance    = NULL,
    dt_portfolio_location           = NULL,
    dt_positions_weight_constraints = NULL,
    dt_risk_budget_constraints      = NULL,
    vec_position_ids                = NULL,
    cardinality_constraint_lower    = NULL,
    cardinality_constraint_upper    = NULL,
    has_position_weight_constraint  = FALSE,
    has_cardinality_constraint      = FALSE,
    has_risk_budget_constraint      = FALSE,
    has_asset_allocation_analysis   = FALSE,
    pnl_is_log_transformed          = FALSE,
    max_use_cpu_cores               = FALSE,
    rsk_conf_level                  = 0.975,
    n_sim                           = 10000,
    rsk_free_rate                   = 0.0,
    dt_simulate_portfolio_weights   = NULL,
    asset_allocation_analysis       = NULL,
    
    in_validate                     = function(){
      private$dt_simulate_portfolio_weights   <- NULL 
      
      private$asset_allocation_analysis      <- NULL
      private$has_asset_allocation_analysis  <- FALSE
    },
    
    #-------------------------------------------------------------------------------
    #--                                                                           --
    #-- PERFORM ASSET ALLOCATION ANALYSIS                                         -- 
    #--                                                                           --
    #-------------------------------------------------------------------------------
    do_perform_asset_allocation_analysis = function(){
      execution_time_start <- proc.time()
      private$do_simulate_portfolio_weights()
      
      message("------------------------------------------------------------------------------")
      message("Portfolio Optimal Weights.")
      message("------------------------------------------------------------------------------")
      es_tester <- risk_profile_monte_carlo(
        private$dt_pnl_simulations[, colnames(private$dt_simulate_portfolio_weights), with = FALSE],
        private$dt_simulate_portfolio_weights,
        private$rsk_conf_level,
        private$max_use_cpu_cores)
      
      object_efficient_frontier <- efficient_frontier(
        es_tester,
        private$dt_expected_mean_performance[,.(position_id, performance)],
        private$rsk_free_rate,
        breaks = 25)
      
      summary_market_portfolio_by_risk_budget <- "No risk-budget constraint has been selected."
      if(private$has_risk_budget_constraint){
        optimal_exec_id <- object_efficient_frontier[["MarketPortfolio"]][["exec_id"]]
        summary_market_portfolio_by_risk_budget <- private$output_summary_market_portfolio_by_risk_budget(
          object_efficient_frontier[["TblRiskPerformance"]][exec_id == optimal_exec_id])
        summary_market_portfolio_by_risk_budget[, exec_id := optimal_exec_id]
        summary_market_portfolio_by_risk_budget <- summary_market_portfolio_by_risk_budget[,.("group" = risk_budget_group, 
                                                                                              "weight" = optimal_weights, 
                                                                                              performance, 
                                                                                              performance_weight, 
                                                                                              risk, 
                                                                                              risk_weight)]
        summary_market_portfolio_by_risk_budget[, type := "optimal_weights"]
      }
      
      constraints <- list(
        "weight_constraints"      = private$has_position_weight_constraint,
        "cardinality_constraints" = private$has_cardinality_constraint,
        "risk_budget_constraints" = private$has_risk_budget_constraint)
      execution_time <- proc.time() - execution_time_start
      private$has_asset_allocation_analysis <- TRUE
      #setorder(dt_rsk_perf, exec_id)
      
      message("------------------------------------------------------------------------------")
      message("Portfolio Risk Caclulations with Current Weights.")
      message("------------------------------------------------------------------------------")
      dt_current_weights <- copy(private$dt_portfolio_positions)
      dt_current_weights <- dcast.data.table(dt_current_weights, . ~ position_id, value.var = "current_weight")[, -1]
      dt_summary_current_weights <- risk_profile_monte_carlo(
        private$dt_pnl_simulations[, colnames(dt_current_weights), with = FALSE],
        dt_current_weights,
        private$rsk_conf_level,
        n_cores = 1)
      dt_summary_current_weights[, ":=" (exec_id = NULL, slope = NULL)]
      
      dt_summary_current_weights <- merge(
        dt_summary_current_weights,
        private$dt_expected_mean_performance[,.(position_id, performance)],
        by = "position_id",
        all.x = TRUE)
      dt_summary_current_weights[, performance := performance * weight]
      dt_summary_current_weights[, perf_weight := performance / sum(performance)]
      
      dt_portfolio_current_weights      <- dt_summary_current_weights[, .(PERF = sum(performance), RISK = sum(expected_shortfall))]
      dt_portfolio_current_weights[, slope := (PERF - private$rsk_free_rate) / RISK]
      
      summary_by_risk_budget_current_weights <- "No risk-budget constraint has been selected."
      if(private$has_risk_budget_constraint){
        summary_by_risk_budget_current_weights <- private$output_summary_market_portfolio_by_risk_budget(
          dt_summary_current_weights)  
        summary_by_risk_budget_current_weights <- summary_by_risk_budget_current_weights[,.("group" = risk_budget_group, 
                                                                                            "weight" = optimal_weights, 
                                                                                            performance, 
                                                                                            performance_weight, 
                                                                                            risk, 
                                                                                            risk_weight)]
        summary_by_risk_budget_current_weights[, type := "current_weights"]
        summary_market_portfolio_by_risk_budget <- rbind(
          summary_market_portfolio_by_risk_budget,
          summary_by_risk_budget_current_weights)
      } 
      setnames(dt_summary_current_weights, "perf_weight", "performance_weight")
      setnames(dt_summary_current_weights, "expected_shortfall", "risk")
      
      #### ####
      dt_pnl_scenario_sim_current_weights <- data.table(as.matrix(private$dt_pnl_simulations[, colnames(dt_current_weights), with = FALSE]) %*% as.matrix(as.numeric(dt_current_weights)))
      dt_pnl_scenario_sim_current_weights[, ":=" (scenario_id = 1:nrow(dt_pnl_scenario_sim_current_weights), type = "current_weights")]
      setnames(dt_pnl_scenario_sim_current_weights, "V1", "pnl")
      
      dt_optimal_weights <- copy(object_efficient_frontier[["OptimalWeights"]])
      dt_optimal_weights <- dcast.data.table(dt_optimal_weights, . ~ position_id, value.var = "weight")[, -1]
      dt_pnl_scenario_sim_optimal_weights <- data.table(as.matrix(private$dt_pnl_simulations[, colnames(dt_optimal_weights), with = FALSE]) %*% as.matrix(as.numeric(dt_optimal_weights)))
      dt_pnl_scenario_sim_optimal_weights[, ":=" (scenario_id = 1:nrow(dt_pnl_scenario_sim_optimal_weights), type = "optimal_weights")]
      setnames(dt_pnl_scenario_sim_optimal_weights, "V1", "pnl")
      
      dt_pnl_scenario_sim <- rbind(
        dt_pnl_scenario_sim_optimal_weights,
        dt_pnl_scenario_sim_current_weights)                
      
      #### ####
      private$asset_allocation_analysis <- list(
        "TMSTMP"                            = Sys.time(),
        "EXECUTION_TIME"                         = execution_time,
        "CONSTRAINTS"                            = constraints,
        "SIMULATED_RISK_PERF"                    = object_efficient_frontier[["TblRiskPerformance"]][,.(position_id, 
                                                                                                        exec_id, 
                                                                                                        weight, 
                                                                                                        performance, 
                                                                                                       "performance_weight" = perf_weight,
                                                                                                       "risk" = expected_shortfall, 
                                                                                                       risk_weight)],
        "SUMMARY_OPTIMAL_PORTFOLIO"              = object_efficient_frontier[["MarketPortfolio"]],
        "SUMMARY_CURRENT_PORTFOLIO"              = dt_portfolio_current_weights,
        "OPTIMAL_WEIGHTS_MARKET_PORTFOLIO"       = object_efficient_frontier[["OptimalWeights"]],
        "EFFICIENT_FRONTIER"                     = object_efficient_frontier[["EfficientFrontier"]][,.(exec_id, "performance" = PERF, "risk" = RISK, slope)],
        "SUMMARY_BY_RISK_BUDGET"                 = summary_market_portfolio_by_risk_budget,
        "SUMMARY_CURRENT_WEIGHTS"                = dt_summary_current_weights,
        "PNL_SIMULATION_PORTFOLIOS"              = dt_pnl_scenario_sim)
      
      message("------------------------------------------------------------------------------")
      message("The analysis is complete. ")
      message("------------------------------------------------------------------------------")
    },
    
    #-------------------------------------------------------------------------------
    #--                                                                           --
    #-- OUTPUT                                                                    -- 
    #--                                                                           --
    #-------------------------------------------------------------------------------
    output_portfolio_tangent_line = function(in_efficient_frontier){
      in_efficient_frontier[tangent == max(tangent),.(
        "note"      = "perf := intercept + slope * risk",
        "intercept" = private$rsk_free_rate,
        "slope"     = tangent)]
    },
    output_summary_market_portfolio_by_risk_budget = function(in_rsk_perf){
      dt_risk_per_inkl_risk_group <- copy(in_rsk_perf)
      dt_risk_per_inkl_risk_group <- merge(
        dt_risk_per_inkl_risk_group,
        private$dt_portfolio_location[, .(position_id, location)],
        by = "position_id",
        all.x = TRUE)
      
      lst_risk_budget_group_summary <- lapply(private$dt_risk_budget_constraints[["group"]], function(x){
        dt_tmp <- dt_risk_per_inkl_risk_group[
          grepl(x, location), .("optimal_weights"    = sum(weight), 
                                "performance"        = sum(performance), 
                                "performance_weight" = sum(perf_weight),
                                "risk"               = sum(expected_shortfall),
                                "risk_weight"        = sum(risk_weight))]
        dt_tmp[, risk_budget_group := x]
        dt_tmp
      })
      dt_risk_budget_group_summary <- rbindlist(lst_risk_budget_group_summary)
      return(dt_risk_budget_group_summary)
    },
    
    #-------------------------------------------------------------------------------
    #--                                                                           --
    #-- SIMULATE POSITION WEIGHTS TAKING CONSTRAINT(S) INTO ACCOUNT               -- 
    #--                                                                           --
    #-------------------------------------------------------------------------------
    do_simulate_portfolio_weights = function(){
      message("------------------------------------------------------------------------------")
      message("Portfolio weights are initially simulated without constraints.")
      message("------------------------------------------------------------------------------")
      dt_weigthts <- EnvRskAssetAllocationHelpers::simulate_portfolio_weights(
        private$n_sim, 
        private$vec_position_ids)
      
      if(private$has_cardinality_constraint == TRUE){
        if(private$has_risk_budget_constraint == FALSE){
          message("------------------------------------------------------------------------------")
          message("Adjusting the portfolio weights due to cardinality constraints.")
          message("------------------------------------------------------------------------------")
          dt_weigthts <- EnvRskAssetAllocationHelpers::constraint_cardinality(
            dt_weigthts, 
            private$cardinality_lower, 
            private$cardinality_upper)
        } else {
          warning("At present, it is not permissible to apply both risk-budget and cardinality constraints simultaneously. Consequently, cardinality constraints have been deactivated.")   
        }
      }
      
      if(private$has_position_weight_constraint == TRUE){
        message("------------------------------------------------------------------------------")
        message("Adjusting the portfolio weights due to position weight constraints.")
        message("------------------------------------------------------------------------------")
        do_adjust_zero_weight <- TRUE
        if(private$has_cardinality_constraint){
          do_adjust_zero_weight <- FALSE
        }
        
        dt_weigthts <- EnvRskAssetAllocationHelpers::portfolio_weights_apply_weight_constraint(
          p_dt_weight             = dt_weigthts,
          p_dt_weight_constraints = private$dt_positions_weight_constraints,
          adjust_zero_weight      = do_adjust_zero_weight)
      }
      
      message("------------------------------------------------------------------------------")
      message(paste0("The portfolio undergoes risk profiling. Started at: ", Sys.time()))
      message("------------------------------------------------------------------------------")
      dt_internal_risk_profile      <- private$risk_profiling_with_risk_budget(dt_weigthts, TRUE)
      
      dt_risk_budget_constraints <- data.table(copy(private$dt_risk_budget_constraints))
      if(private$has_risk_budget_constraint == FALSE){
        risk_budget_groups    <- unique(as.character(
          sapply(
            unique(private$dt_portfolio_location[["location"]]), 
            get_first_element, 
            splitter = "//")))
        
        dt_risk_budget_constraints <- data.table("group"      = risk_budget_groups,
                                                 "constraint" = 1.5)
        private$dt_risk_budget_constraints <- dt_risk_budget_constraints
      } 
      dt_internal_suggested_weights <- private$suggested_weight_ranges_with_risk_budget(dt_internal_risk_profile, dt_risk_budget_constraints)
      dt_internal_sim_weights       <- private$optimal_weights_with_risk_budget(dt_internal_suggested_weights)
      
      message("------------------------------------------------------------------------------")
      message(paste0("The simulated portfolio weights are ready. The number of simulated sets is: ", nrow(dt_internal_sim_weights)))
      message("------------------------------------------------------------------------------")
      private$dt_simulate_portfolio_weights   <- dt_internal_sim_weights
    },
    
    #-------------------------------------------------------------------------------
    #--                                                                           --
    #-- RISK-BUDGET FUNCTINALITY                                                  -- 
    #--                                                                           --
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    #-- RISK PROFILING OF THE PORTFOLIO                                           -- 
    #-------------------------------------------------------------------------------
    risk_profiling_with_risk_budget = function(dt_weights, use_gaussian_approx = FALSE){
      dt_risk_profile <- copy(private$dt_portfolio_positions)
      if(use_gaussian_approx == TRUE){
        #colnames(dt_weights) <- colnames(private$dt_pnl_simulations)
        dt_es <- risk_profile_gaussian(private$dt_pnl_simulations[, colnames(dt_weights), with = FALSE], 
                                       dt_weights, 
                                       private$rsk_conf_level)
        dt_es_comp <- comb_risk_perf(dt_es[,.(position_id, weight, risk_weight, exec_id)], 
                                     private$dt_expected_mean_performance[,.(position_id, performance)])
        
        dt_slp <- slope_analysis(unique(private$vec_position_ids), 
                                 dt_es_comp)
        
      } else {
        dt_expected_shortfall <- risk_profile_monte_carlo(
          private$dt_pnl_simulations[, private$vec_position_ids, with = FALSE],
          dt_weights[, private$vec_position_ids, with = FALSE],
          private$rsk_conf_level,
          private$max_use_cpu_cores)
        
        dt_es_comb <- comb_risk_perf(dt_expected_shortfall, 
                                     private$dt_expected_mean_performance[,.(position_id, performance)])
        dt_slp     <- slope_analysis(unique(private$vec_position_ids), 
                                     dt_es_comb)
      }
      
      dt_risk_profile <- merge(
        dt_risk_profile,
        dt_slp[, .(position_id, slope)],
        by = "position_id",
        all.x = TRUE)
      
      dt_risk_profile <- merge(
        dt_risk_profile,
        private$dt_expected_mean_performance,
        by = "position_id",
        all.x = TRUE)
      
      dt_risk_profile[performance > private$rsk_free_rate & slope > 0, risk_adjusted_return := (performance - private$rsk_free_rate) / abs(slope)]
      dt_risk_profile[performance > private$rsk_free_rate & slope < 0, risk_adjusted_return := (performance - private$rsk_free_rate) / abs(slope)]
      dt_risk_profile[performance < private$rsk_free_rate & slope < 0, risk_adjusted_return := (performance - private$rsk_free_rate) / slope]
      dt_risk_profile[performance < private$rsk_free_rate & slope > 0, risk_adjusted_return := (performance - private$rsk_free_rate) / slope]
      setorder(dt_risk_profile, -"risk_adjusted_return")
      
      return(dt_risk_profile)
    },
    
    #-------------------------------------------------------------------------------
    #-- ADJUST SIMULATED WEIGHTS SO THEY ADHERE TO THE RISK-BUDGET                -- 
    #-------------------------------------------------------------------------------
    suggested_weight_ranges_with_risk_budget = function(dt_risk_profile, dt_risk_budget_constraints){
      
      dt_group_focus <- copy(dt_risk_profile)
      dt_group_focus <- merge(
        dt_group_focus,
        private$dt_positions_weight_constraints,
        by = "position_id",
        all.x = TRUE)
      
      dt_group_focus <- merge(
        dt_group_focus,
        private$dt_portfolio_location,
        by = "position_id",
        all.x = TRUE)
      
      min_weight_allocation <- sum(dt_group_focus$min_weight)
      max_weight_allocation <- sum(dt_group_focus$max_weight)
      
      #dt_group_focus[position_id == "f89801ec83de17e47fc433df9964499d"]
      
      #------------------------------------------------------------------------------
      #-- Create the 'subject to' matrix, the constraint values (rhs), 
      #-- the direction (dir) vector and the weight constraints.
      #------------------------------------------------------------------------------
      risk_budget_grps <- unique(dt_risk_budget_constraints[["group"]])
      m <- nrow(dt_group_focus)
      n <- length(risk_budget_grps)
      
      solve_lp_env <- new.env()
      mat_fk <- lapply(risk_budget_grps, function(x){
        as.numeric(grepl(x, dt_group_focus$location)) * dt_group_focus$slope
      })
      mat_fk <- do.call(rbind, mat_fk)
      mat_fk <- rbind(mat_fk,
                      rep(1, m))
      rhs_fk <- c(dt_risk_budget_constraints[["constraint"]], 1.0)
      dir_fk <- c(rep("<=", length(risk_budget_grps)), "==")
      bounds_fk <- list(lower = list(ind = 1:m, val = dt_group_focus$min_weight),
                        upper = list(ind = 1:m, val = dt_group_focus$max_weight))
      
      assign("mat_fk",    mat_fk,    envir = solve_lp_env)
      assign("rhs_fk",    rhs_fk,    envir = solve_lp_env)
      assign("dir_fk",    dir_fk,    envir = solve_lp_env)
      assign("bounds_fk", bounds_fk, envir = solve_lp_env)
      
      solve_lp <- function(obj_fk, set_max = TRUE){
        lp_solution <- Rglpk::Rglpk_solve_LP(
          obj    = obj_fk, 
          mat    = get("mat_fk", env = solve_lp_env),
          dir    = get("dir_fk", env = solve_lp_env),
          rhs    = get("rhs_fk", env = solve_lp_env),
          max    = set_max,
          bounds = get("bounds_fk", env = solve_lp_env))
        
        # Checks
        stopifnot(lp_solution$status == 0)
        stopifnot(round(sum(lp_solution$solution), 6) == 1.0)
        
        return(cbind(dt_group_focus[,.(position_id, location, slope, performance)], 
                     "optimal_weight" = lp_solution$solution))
        
      }
      
      #------------------------------------------------------------------------------
      #-- Solve LP - Maximize the Risk Adjusted Return                             -- 
      #------------------------------------------------------------------------------
      obj_fk_max_return        <- dt_group_focus$performance  
      dt_lp_max_return_weights <- solve_lp(obj_fk_max_return, set_max = TRUE)
      
      #------------------------------------------------------------------------------
      #-- Solve LP - Minimize the Risk                                             -- 
      #------------------------------------------------------------------------------
      obj_fk_min_risk        <- dt_group_focus$slope
      dt_lp_min_risk_weights <- solve_lp(obj_fk_min_risk, set_max = TRUE)
      
      #------------------------------------------------------------------------------
      #--                                                                          --
      #------------------------------------------------------------------------------
      
      #------------------------------------------------------------------------------
      #--                                                                          --
      #------------------------------------------------------------------------------
      lst_max_return_weights_by_grp <- lapply(dt_risk_budget_constraints[["group"]], function(x){
        pos_ids <- dt_lp_max_return_weights[grepl(x, location)]$position_id
        dt_lp_max_return_weights_tmp <- dt_lp_max_return_weights[position_id %in% pos_ids, .(position_id, optimal_weight, slope)] 
        data.table(
          "risk_budget_group" = x,
          "weight"            = sum(dt_lp_max_return_weights_tmp$optimal_weight),
          "risk"              = sum(dt_lp_max_return_weights_tmp$optimal_weight * 
                                    dt_lp_max_return_weights_tmp$slope))
      })
      dt_max_return_weights_by_grp <- rbindlist(lst_max_return_weights_by_grp)
      
      lst_min_risk_weights_min_risk_by_grp <- lapply(dt_risk_budget_constraints[["group"]], function(x){
        pos_ids <- dt_lp_min_risk_weights[grepl(x, location)]$position_id
        dt_lp_min_risk_weights_tmp <- dt_lp_min_risk_weights[position_id %in% pos_ids, .(position_id, optimal_weight, slope)] 
        data.table("risk_budget_group" = x,
                   "weight" = sum(dt_lp_min_risk_weights_tmp$optimal_weight),
                   "risk"   = sum(dt_lp_min_risk_weights_tmp$optimal_weight * 
                                  dt_lp_min_risk_weights_tmp$slope))
      })
      dt_min_risk_weights_min_risk_by_grp <- rbindlist(lst_min_risk_weights_min_risk_by_grp)
      
      #------------------------------------------------------------------------------
      #-- Risk Calc Maximum Performance                                            --
      #------------------------------------------------------------------------------
      es_max_perf <- EnvRskAssetAllocationHelpers::calculate_port_es(
        as.matrix(private$dt_pnl_simulations[, dt_lp_max_return_weights$position_id, with = FALSE]),
        t(as.matrix(dt_lp_max_return_weights$optimal_weight)), 
        component = TRUE, 
        confidence_level = private$rsk_conf_level)
      
      risk_max_perf <- es_max_perf[-1]/as.numeric(es_max_perf[1])
      dt_risk_max_perf <- data.table("position_id" = dt_lp_max_return_weights$position_id,
                                     "risk"   = -1 * es_max_perf[-1],
                                     "risk_attribution"   = risk_max_perf)
      
      dt_risk_max_perf <- merge(
        dt_risk_max_perf,
        dt_lp_max_return_weights[,.(position_id, "weight" = optimal_weight, "perf" = performance, location)],
        by = "position_id",
        all.x = TRUE)
      
      lst_risk_sum_max_perf_by_grp <- lapply(dt_risk_budget_constraints[["group"]], function(x){
        pos_ids <- dt_risk_max_perf[grepl(x, location)]$position_id
        dt_risk_max_perf_tmp <- dt_risk_max_perf[position_id %in% pos_ids, .(position_id, risk, risk_attribution, weight, perf, "slope" = risk_attribution / weight)] 
        dt_risk_max_perf_tmp[, .(
          risk_budget_group = x,
          weight            = sum(weight), 
          perf              = sum(weight * perf), 
          risk              = sum(risk), 
          risk_attribution   = sum(risk_attribution))]
      })
      dt_risk_sum_max_perf_by_grp <- rbindlist(lst_risk_sum_max_perf_by_grp)
      
      dt_risk_sum_max_perf_by_grp <- merge(
        dt_risk_sum_max_perf_by_grp,
        dt_risk_budget_constraints[,.("risk_budget_group" = group, constraint)],
        by = "risk_budget_group",
        all.x = TRUE)
      dt_risk_sum_max_perf_by_grp[, in_valid := risk_attribution > constraint]
      
      #------------------------------------------------------------------------------
      #-- Risk Calc Minimum Risk                                                   --
      #------------------------------------------------------------------------------
      es_min_risk <- EnvRskAssetAllocationHelpers::calculate_port_es(
        as.matrix(private$dt_pnl_simulations[, dt_lp_min_risk_weights$position_id, with = FALSE]),
        t(as.matrix(dt_lp_min_risk_weights$optimal_weight)), 
        component = TRUE, 
        confidence_level = private$rsk_conf_level)
      
      risk_min_risk <- es_min_risk[-1]/as.numeric(es_min_risk[1])
      dt_risk_min_risk <- data.table("position_id" = dt_lp_min_risk_weights$position_id,
                                     "risk"   = -1 * es_min_risk[-1],
                                     "risk_attribution"   = risk_min_risk)
      
      dt_risk_min_risk <- merge(
        dt_risk_min_risk,
        dt_lp_min_risk_weights[,.(position_id, "weight" = optimal_weight, "perf" = performance, location)],
        by = "position_id",
        all.x = TRUE)
      
      lst_risk_sum_min_risk_by_grp <- lapply(dt_risk_budget_constraints[["group"]], function(x){
        pos_ids <- dt_risk_min_risk[grepl(x, location)]$position_id
        dt_risk_min_risk_tmp <- dt_risk_min_risk[position_id %in% pos_ids, .(position_id, risk, risk_attribution, weight, perf, "slope" = risk_attribution / weight)] 
        dt_risk_min_risk_tmp[, .(
          risk_budget_group = x,
          weight            = sum(weight), 
          perf              = sum(weight * perf), 
          risk              = sum(risk), 
          risk_attribution   = sum(risk_attribution))]
      })
      dt_risk_sum_min_risk_by_grp <- rbindlist(lst_risk_sum_min_risk_by_grp)
      
      dt_risk_sum_min_risk_by_grp <- merge(
        dt_risk_sum_min_risk_by_grp,
        dt_risk_budget_constraints[,.("risk_budget_group" = group, constraint)],
        by = "risk_budget_group",
        all.x = TRUE)
      dt_risk_sum_min_risk_by_grp[, in_valid := risk_attribution > constraint]
      
      #------------------------------------------------------------------------------
      #-- Combine upper and lower to find the limits                               --
      #------------------------------------------------------------------------------
      dt_risk_sum_max_perf_by_grp[in_valid == TRUE, range_tmp_1 := weight / (risk_attribution / constraint)]
      dt_risk_sum_min_risk_by_grp[in_valid == TRUE, range_tmp_2 := weight / (risk_attribution / constraint)]
      dt_risk_sum_max_perf_by_grp[in_valid == FALSE, range_tmp_1 := weight]
      dt_risk_sum_min_risk_by_grp[in_valid == FALSE, range_tmp_2 := weight]
      
      dt_suggested_limits <- merge(
        dt_risk_sum_max_perf_by_grp[,.(risk_budget_group, range_tmp_1)],
        dt_risk_sum_min_risk_by_grp[,.(risk_budget_group, range_tmp_2)],
        by = "risk_budget_group")
      dt_suggested_limits[, ":=" (lower_weight_lim = mapply(min, range_tmp_1, range_tmp_2), 
                                  upper_weight_lim = mapply(max, range_tmp_1, range_tmp_2))]
      dt_suggested_limits[, ":=" (range_tmp_1 = NULL, 
                                  range_tmp_2 = NULL)]
      
      dt_suggested_limits[, ":=" (lower_weight_lim_tmp = lower_weight_lim - mapply(max, 0.5 * (upper_weight_lim - lower_weight_lim), 0.025),
                                  upper_weight_lim_tmp = upper_weight_lim + mapply(max, 0.5 * (upper_weight_lim - lower_weight_lim), 0.025))]
      
      dt_suggested_limits[, ":=" (lower_weight_lim = lower_weight_lim_tmp,
                                  upper_weight_lim = upper_weight_lim_tmp)]
      dt_suggested_limits[, ":=" (lower_weight_lim_tmp = NULL,
                                  upper_weight_lim_tmp = NULL)]
      
      dt_suggested_limits[, lower_weight_lim := mapply(max, lower_weight_lim, 0)]
      dt_suggested_limits[, upper_weight_lim := mapply(min, upper_weight_lim, 1)]
      
      dt_suggested_limits <- merge(
        dt_suggested_limits,
        dt_max_return_weights_by_grp[,.(risk_budget_group, "suggested_optimal_weight" = weight)],
        by = "risk_budget_group",
        all.x = TRUE)
      
      return(dt_suggested_limits)
      
    },
    optimal_weights_with_risk_budget = function(dt_suggested_limits){
     
      lst_unif_sim <- lapply(1:nrow(dt_suggested_limits), function(x){
        use_min <- dt_suggested_limits[x, lower_weight_lim]
        use_max <- dt_suggested_limits[x, upper_weight_lim]
        runif(ceiling(private$n_sim/2),
              min = use_min,
              max = use_max)
      })
      dt_unif_sim            <- do.call(cbind, lst_unif_sim)
      colnames(dt_unif_sim)  <- dt_suggested_limits$risk_budget_group
      dt_multi_simz_method_1 <- data.table(t(apply(dt_unif_sim, 1, normalize)))
      
      dt_multi_simz_method_2 <- t(rmultinom(ceiling(private$n_sim/2), 100, dt_suggested_limits$suggested_optimal_weight)/100)
      colnames(dt_multi_simz_method_2) <- dt_suggested_limits$risk_budget_group
      
      dt_multi_simz <- rbind(
        dt_multi_simz_method_1,
        dt_multi_simz_method_2)
      
      #------------------------------------------------------------------------------
      #-- Apply Weight Constraints                                                 --
      #------------------------------------------------------------------------------
      select_grps <- dt_suggested_limits$risk_budget_group
      
      dt_weight_range <- merge(
        private$dt_positions_weight_constraints,
        private$dt_portfolio_location,
        by = "position_id",
        all.x = TRUE)
      
      lst_risk_budget_max_min <- lapply(select_grps, function(x){
        dt_out <- dt_weight_range[grepl(x, location), .("min_weight" = sum(min_weight), "max_weight" = sum(max_weight))]  
        dt_out[, risk_budget_group := x]
        dt_out
      })
      dt_risk_budget_max_min <- rbindlist(lst_risk_budget_max_min)
      
      dt_multi_simz <- EnvRskAssetAllocationHelpers::portfolio_weights_apply_weight_constraint(
        dt_multi_simz,
        dt_risk_budget_max_min[,.("position_id" = risk_budget_group, min_weight, max_weight)],
        adjust_zero_weight = TRUE)
      
      #------------------------------------------------------------------------------
      #-- Adjust the portfolio weight to account for the risk-budget constraints   --
      #------------------------------------------------------------------------------
      dt_weight_weight_adjusted_sims <- risk_budget_weight_adjustment(
        dt_weight_range, 
        dt_multi_simz, 
        select_grps,
        nrow(dt_multi_simz),
        private$max_use_cpu_cores)
      #apply(dt_weight_weight_adjusted_sims, 1, "sum")
      
      #------------------------------------------------------------------------------
      #-- Calculate Risk and check Risk-Budget Constraints                         --
      #------------------------------------------------------------------------------
      message("------------------------------------------------------------------------------")
      message("Identifying the simulated portfolio weights that comply with the risk-budget constraints.")
      message("------------------------------------------------------------------------------")
      es_tester <- risk_profile_monte_carlo(
        private$dt_pnl_simulations[, colnames(dt_weight_weight_adjusted_sims), with = FALSE],
        as.matrix(dt_weight_weight_adjusted_sims),
        private$rsk_conf_level,
        private$max_use_cpu_cores)
      
      dt_portfolio_info <- copy(private$dt_portfolio_location)
      invisible(lapply(private$dt_risk_budget_constraints[["group"]], function(x){
        dt_portfolio_info[grepl(x, location), risk_budget_group := x]  
      }))
      
      dt_check <- merge(
        es_tester[, .(exec_id, position_id, weight, expected_shortfall, risk_weight)],
        dt_portfolio_info[, .(position_id, risk_budget_group)],
        by = "position_id",
        all.x = TRUE)
      
      dt_check_agg <- dt_check[, .(weight = sum(weight),
                                   es_attribution = sum(risk_weight)),
                               by = c("exec_id", "risk_budget_group")]
      dt_check_agg <- merge(
        dt_check_agg,
        private$dt_risk_budget_constraints[,.("risk_budget_group" = group, constraint)],
        by = "risk_budget_group",
        all.x = TRUE)
      dt_check_agg[, is_valid := es_attribution <= constraint]
      dt_check_agg_by_exec_id <- dt_check_agg[, .(is_risk_budget_valid = all(is_valid)), by = exec_id]
      
      use_exec_ids <- dt_check_agg_by_exec_id[is_risk_budget_valid == TRUE]$exec_id
      dt_rsk_perf <- es_tester[exec_id %in% use_exec_ids]
      #setnames(dt_rsk_perf, "ticker", "position_id")
      
      dt_portfolio_info <- merge(
        dt_portfolio_info,
        private$dt_expected_mean_performance,
        by = "position_id",
        all.x = TRUE)
      
      dt_portfolio_info <- merge(
        dt_portfolio_info,
        private$dt_positions_weight_constraints,
        by = "position_id",
        all.x = TRUE)
      
      dt_rsk_perf <- merge(
        dt_rsk_perf,
        dt_portfolio_info[,.(position_id, performance, min_weight, max_weight)],
        by = "position_id",
        all.x = TRUE)
      
      # Check that the weight constraints are full filled
      dt_rsk_perf_weight_constraints <- dt_rsk_perf[, .(is_weight_valid = all(between(weight, min_weight, max_weight))), by = "exec_id"]
      use_exec_ids <- dt_rsk_perf_weight_constraints[is_weight_valid == TRUE]$exec_id
      
      dt_rsk_perf_by_exec_id <- dt_rsk_perf[exec_id %in% use_exec_ids, .(perf = sum(performance * weight), 
                                                                         risk = sum(-1 * expected_shortfall)), 
                                            by = exec_id]
      
      #-------------------------------------------------------------------------------
      #-- PORTFOLIO WEIGHT FULL FILLING THE RISK BUGET                              --
      #-------------------------------------------------------------------------------
      dt_weight_constraint_fullfilled <- dcast.data.table(dt_rsk_perf[exec_id %in% use_exec_ids,.(exec_id, position_id, weight)], exec_id ~ position_id, value.var = "weight") 
      dt_weight_constraint_fullfilled <- dt_weight_constraint_fullfilled[, -1]
      dt_weight_constraint_fullfilled
      
      return(dt_weight_constraint_fullfilled)
    }
  )
)

