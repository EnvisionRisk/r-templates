
#-------------------------------------------------------------------------------
#### DEPENDENCIES ####
#-------------------------------------------------------------------------------
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_strategic_asset_allocation_dependencies.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_path_generator.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_helpers.R")

#-------------------------------------------------------------------------------
#--
#### USER INPUT ####
#--
#-------------------------------------------------------------------------------
# Symbols/tickers & weight 
# {position_id, symbol, weight, location, expected_annual_growth_rate}
dt_input_portfolio <- data.table(readRDS(url("https://www.dropbox.com/scl/fi/ays28w9vu3piwiifbxnqy/portfolio_by_asset_classes.rds?rlkey=g52i0roki5sln4eplv98w2lpm&raw=true","rb")))[,.(position_id, symbol, weight, location, expected_annual_growth_rate)]

# Reference date: 
ref_date <- as.Date("2024-01-31")

# Base Currency: 
base_cur <- "DKK"

#### Risk Settings ####
rsk_free_rate  <- 0.035

# Volatility scenario (alternatives: use "point_in_time", "downturn", 
# "through_the_cycle", "severe_stress" or "extreme_stress"). In case 
# "point_in_time" scenario is used the reference date becomes important since 
# the volatility on the reference date is used. 
rsk_volatility_scenario <- "through_the_cycle"

#### Simulation Settings ####
sim_paths <- 10000 # Set simulated paths (where each path-simulation is based on the steps chosen above)
sim_years <- 10    # Set simulated path-steps (252 steps is equivivalent to one year) 

#Confidence level: 
rsk_conf_level <- 0.975

#Demean the delta vectors?
do_demean_delta_vectors <- TRUE

#
target_risk_level <- 0.06 #ES / MV := 6000000 / 100000000 = 6%

#-------------------------------------------------------------------------------
#--
#### FUNCTIONALITY ####
#--
#-------------------------------------------------------------------------------
normalize <- function(v_weight){
  return(v_weight/sum(v_weight))
}

# # Constraint Weight
# constraint_weight_check <- function(x, p_min_weight, p_max_weight){
  # if(any(x != 0.0 & x < p_min_weight) || any(x != 0.0 & x > p_max_weight)){
    # return(1)
  # }
  # return(0)
# }

constraint_weight <- function(p_dt_weights, p_weight_lower, p_weight_upper){
  
  dt_weight_with_weight_constr <- copy(p_dt_weights)
  dt_weight_with_weight_constr[, exclude_weight := apply(
    dt_weight_with_weight_constr, 
    1, 
    constraint_weight_check, 
    p_min_weight = p_weight_lower, 
    p_max_weight = p_weight_upper)]

  dt_weight_with_weight_constr <- dt_weight_with_weight_constr[exclude_weight == 0]
  dt_weight_with_weight_constr[, exclude_weight := NULL]
  
  colnames(dt_weight_with_weight_constr) <- colnames(p_dt_weights)
  return(dt_weight_with_weight_constr)
}

# constraint_cardinality <- function(p_dt_weights, p_cardinality_lower, p_cardinality_upper){
  # #p_dt_weights <- dt_weights
  # n        <- nrow(p_dt_weights)
  # n_assets <- ncol(p_dt_weights)
  # lst_cardinality <- lapply(1:n, function(x){
    # #x <- 1
    # n_cardinality_sim <- round(runif(1, p_cardinality_lower, p_cardinality_upper), 0)
    # n_cardinality_sample <- c(rep(1, n_cardinality_sim), rep(0, n_assets - n_cardinality_sim))
    # sample(n_cardinality_sample, n_assets)
  # })
  # m_cardinality <- do.call(rbind, lst_cardinality)
  
  # #Recalculate weights
  # m_test_weigtht <- as.matrix(p_dt_weights)
  # m_weight_1 <- m_test_weigtht * m_cardinality
  # lst_weight_1_norm <- lapply(1:nrow(m_weight_1), function(x){
    # normalize(m_weight_1[x, ])
  # })
  # m_weight_1_norm <- do.call(rbind, lst_weight_1_norm)
  # dt_weight_1_norm <- data.table(m_weight_1_norm)
  # #colnames(dt_weight_1_norm) <- colnames(p_dt_weights)
  # return(dt_weight_1_norm)
# } 

#******************************************************************************
#*
#### GENERATE PORTFOLIO WEIGHTS - NO CONSTRAINTS ####
#*
#******************************************************************************
# simulate_portfolio_weights <- function(p_n, 
                                       # p_symbols,
                                       # p_factor_loadings = NULL){
  
  # n_assets <- length(p_symbols)
  # if(is.null(p_factor_loadings) || length(p_factor_loadings) != length(p_symbols)){
    # factor_loadings <- rep(1, n_assets)
  # } else {
    # factor_loadings <- p_factor_loadings
  # }
  
  # dt_port_weigthts        <- data.table(gtools::rdirichlet(p_n, factor_loadings))
  # colnames(dt_port_weigthts) <- p_symbols
  
  # return(dt_port_weigthts)
# }

#-------------------------------------------------------------------------------
#-- Apply Weight Constraint                                                   --
#-------------------------------------------------------------------------------
# portfolio_weights_apply_weight_constraint <- function(p_dt_weight, p_dt_weight_constraints, adjust_zero_weight = TRUE, step_size = 0.5, delta = 0.0005, iter_max = 100){
#   # p_dt_weight            <- dt_weights_cardinality_adjusted
#   # p_dt_weight_constrains <- dt_weight_constraints
#   
#   pos_ids <- colnames(p_dt_weight) 
#   n_pos   <- ncol(p_dt_weight)
#   
#   dt_internal_weight_constraints <- merge(
#     data.table("position_id" = pos_ids),
#     p_dt_weight_constraints,
#     by = "position_id",
#     all.x = TRUE)
#   dt_internal_weight_constraints[is.na(min_weight), min_weight := 0.0]
#   dt_internal_weight_constraints[is.na(max_weight), max_weight := 1.0]
#   
#   dt_internal_weight <- data.table(copy(p_dt_weight))
#   dt_internal_weight <- dt_internal_weight[, dt_internal_weight_constraints$position_id, with = FALSE]
#   
#   min_weight     <- dt_internal_weight_constraints$min_weight
#   max_weight     <- dt_internal_weight_constraints$max_weight
#   dt_weight_splt <- split(dt_internal_weight, 1:nrow(dt_internal_weight))
#   
#   lst_weights_adjusted <- lapply(dt_weight_splt, function(p_row){
#     p_row <- do.call(c, p_row)
#     iter <- 1
#     if(adjust_zero_weight){
#       while((any(p_row < min_weight) | any(p_row > max_weight)) & iter <= iter_max){
#         if(any(p_row < min_weight)){
#           p_row[p_row < min_weight] <- p_row[p_row < min_weight] + mapply(max, (min_weight[p_row < min_weight] - p_row[p_row < min_weight]) / (1 / step_size), delta)
#         }
#         if(any(p_row > max_weight)){
#           p_row[p_row > max_weight] <- p_row[p_row > max_weight] - mapply(max, (p_row[p_row > max_weight] - max_weight[p_row > max_weight]) / (1 / step_size), delta)
#         }
#         p_row <- normalize(p_row)  
#         iter <- iter + 1
#       }
#       p_row
#     } else {
#       p_row[p_row == 0] <- NA_real_
#       bool_not_na <- which(!is.na(p_row))
#       iter <- 1
#       while((any(p_row[bool_not_na] < min_weight[bool_not_na]) | any(p_row[bool_not_na] > max_weight[bool_not_na])) & iter <= iter_max){
#         if(any(p_row[bool_not_na] < min_weight[bool_not_na])){
#           p_row[!is.na(p_row) & p_row < min_weight] <- p_row[!is.na(p_row) & p_row < min_weight] + mapply(max, (min_weight[!is.na(p_row) & p_row < min_weight] - p_row[!is.na(p_row) & p_row < min_weight]) / (1 / step_size), delta)
#         }
#         if(any(p_row[bool_not_na] > max_weight[bool_not_na])){
#           p_row[!is.na(p_row) & p_row > max_weight] <- p_row[!is.na(p_row) & p_row > max_weight] - mapply(max, (p_row[!is.na(p_row) & p_row > max_weight] - max_weight[!is.na(p_row) & p_row > max_weight]) / (1 / step_size), delta)
#         }
#         p_row[is.na(p_row)] <- 0
#         p_row <- normalize(p_row)  
#         p_row[p_row == 0] <- NA_real_
#         iter <- iter + 1
#       }
#       p_row[is.na(p_row)] <- 0
#       p_row
#     }
#   })
#   dt_weights_adjusted <- data.table(do.call(rbind, lst_weights_adjusted))
#   colnames(dt_weights_adjusted) <- pos_ids
#   dt_weights_adjusted
# }

#-------------------------------------------------------------------------------
#-- Apply Weight Constraint                                                   --
#-------------------------------------------------------------------------------
# portfolio_weights_apply_weight_constraint <- function(p_dt_weight, p_dt_weight_constraints, adjust_zero_weight = TRUE, step_size = 0.5, delta = 0.0005, iter_max = 100){
  # # p_dt_weight            <- dt_weights_cardinality_adjusted
  # # p_dt_weight_constrains <- dt_weight_constraints
  
  # pos_ids <- colnames(p_dt_weight) 
  # n_pos   <- ncol(p_dt_weight)
  
  # dt_internal_weight_constraints <- merge(
    # data.table("position_id" = pos_ids),
    # p_dt_weight_constraints,
    # by = "position_id",
    # all.x = TRUE)
  # dt_internal_weight_constraints[is.na(min_weight), min_weight := 0.0]
  # dt_internal_weight_constraints[is.na(max_weight), max_weight := 1.0]
  
  # dt_internal_weight <- data.table(copy(p_dt_weight))
  # dt_internal_weight <- dt_internal_weight[, dt_internal_weight_constraints$position_id, with = FALSE]
  
  # min_weight     <- dt_internal_weight_constraints$min_weight
  # max_weight     <- dt_internal_weight_constraints$max_weight
  
  # m_weights_adjusted <- adjust_weights(
    # as.matrix(dt_internal_weight), 
    # t(as.matrix(min_weight)), 
    # t(as.matrix(max_weight)),
    # adjust_zero_weight,
    # step_size, 
    # delta, 
    # iter_max)
  # dt_weights_adjusted <- data.table(m_weights_adjusted)
  # colnames(dt_weights_adjusted) <- pos_ids
  # return(dt_weights_adjusted)
# }

#-------------------------------------------------------------------------------
#--                                                                           --
#-- Simulated Profit/Loss Vectors (delta-vectors)                             --
#--                                                                           --
#-------------------------------------------------------------------------------
volatility_ids <- c("downturn", "through_the_cycle", "point_in_time")
tbl_expected_performance <- copy(dt_input_portfolio[,.(position_id, expected_annual_growth_rate)])
lst_expected_performance <- lapply(volatility_ids, function(x){
  dt_out <- copy(tbl_expected_performance)
  dt_out[, volatility_id := x]
})
tbl_expected_performance <- rbindlist(lst_expected_performance)

#-------------------------------------------------------------------------------
#-- Simulated 1-day Profit/Loss Vectors (delta-vectors)                       --
#-------------------------------------------------------------------------------
lst_one_day_delta_vector_pct <- pbapply::pblapply(volatility_ids, function(x){
  dt_one_day_delta_vector_pct <- get_one_day_pnl_pct(
    unique(dt_input_portfolio$symbol),
    base_cur,
    ref_date,
    x)
  dt_one_day_delta_vector_pct[, volatility_id := x]
  dt_one_day_delta_vector_pct
})
dt_one_day_delta_vector_pct <- do.call(rbind, lst_one_day_delta_vector_pct)
stopifnot(all(c("symbol", "scenario", "pnl_pct") %in% colnames(dt_one_day_delta_vector_pct)))
#stopifnot(all(c("symbol", "expected_annual_growth_rate") %in% colnames(dt_input_portfolio)))

# To accomodate derivatives, same symbol used more than once etc, make sure 
# that the simulations are identified by their position_id
tbl_one_day_delta_vector_pct <- merge(
  dt_one_day_delta_vector_pct, 
  dt_input_portfolio[,.(symbol, position_id)],
  by    = "symbol",
  all.x = TRUE)
tbl_one_day_delta_vector_pct[, symbol := NULL]

setnames(tbl_one_day_delta_vector_pct, "scenario", "sim_id")

#-------------------------------------------------------------------------------
#-- In Case the Portfolio Contains Derivatives Create the PnL simulations for --
#-- the Derivatives Here (based on the PnL simulation of the underlying plus  --
#-- details if needed)                                                        --
#-------------------------------------------------------------------------------
# Manual Adjustment

#-------------------------------------------------------------------------------
#-- Overlay the simulated one-day pnl-distributions with subject views for    -- 
#-- example via Entropy Pooling                                               --
#-------------------------------------------------------------------------------
# Manual Adjustment

#-------------------------------------------------------------------------------
#-- Generate Horizon Path Simulations                                         --
#-------------------------------------------------------------------------------
tbl_expected_performance[volatility_id == "point_in_time", volatility_id := "bull_run"]
tbl_one_day_delta_vector_pct[volatility_id == "point_in_time", volatility_id := "bull_run"]

demean                      <- TRUE
volatility_ids              <- c("downturn", "through_the_cycle", "bull_run")
dt_time_spend_in_econ_cycle <- data.table("volatility_id" = volatility_ids)
dt_time_spend_in_econ_cycle[volatility_id == "bull_run", time_in_days := round(3.0 * 252, 0)]
dt_time_spend_in_econ_cycle[volatility_id == "through_the_cycle", time_in_days := round(4.5 * 252, 0)]
dt_time_spend_in_econ_cycle[volatility_id == "downturn", time_in_days := round(2.5*252, 0)]

#### USE PATH_SIMULATION CLASS ####
lst_horizon_delta_vector_indexed <- pbapply::pblapply(volatility_ids, function(vol_id){
  message(paste0("Now processing volatility_id: ", vol_id))
  #vol_id <- "bull_run"
  dt_horizon_delta_vector_indexed <- path_simulations(
    tbl_one_day_delta_vector_pct[volatility_id == vol_id,.(position_id, sim_id, pnl_pct)],
    tbl_expected_performance[volatility_id == vol_id,.(position_id, expected_annual_growth_rate)],
    dt_time_spend_in_econ_cycle[volatility_id == vol_id][["time_in_days"]],
    sim_paths)
  dt_horizon_delta_vector_indexed[, volatility_id := vol_id]
})
dt_horizon_delta_vector_indexed <- rbindlist(lst_horizon_delta_vector_indexed)
setorder(dt_horizon_delta_vector_indexed, "position_id", "sim_id")
dt_horizon_delta_vector_indexed <- dt_horizon_delta_vector_indexed[, .(pnl = prod(pnl)), by = .(sim_id, position_id)]

if(demean == TRUE){
  dt_means <- setDT(dt_horizon_delta_vector_indexed)[, .(mean = mean(pnl) - 1), by = "position_id"]
  dt_sim_paths <- merge(
    dt_horizon_delta_vector_indexed,
    dt_means,
    by = "position_id",
    all.x = TRUE)
  dt_sim_paths[, pnl := pnl - mean]
  dt_sim_paths[, mean := NULL]
}

#-------------------------------------------------------------------------------
#-- Extract Long Horizon Delta-Vectors                                        --
#-------------------------------------------------------------------------------
tbl_pnl_simulations <- copy(dt_sim_paths)


#-------------------------------------------------------------------------------
#-- Overlay the long-run simulated pnl-distributions with or without 'demean' --
#-- with subject views for example via Entropy Pooling                        --
#-------------------------------------------------------------------------------
#simulated_delta_vectors

use_max_cpu_cores <- parallel::detectCores() - 1
#-------------------------------------------------------------------------------
#--                                                                           --
#-- GENERATE PORTFOLIO WEIGHTS - WITH RISK BUDGET CONSTRAINTS                 --
#--                                                                           --
#-------------------------------------------------------------------------------
dt_portfolio <- copy(dt_input_portfolio[,.(position_id, weight)])

#-------------------------------------------------------------------------------
#-- The Portfolio Structure                                                   --
#-------------------------------------------------------------------------------
dt_portfolio_organization <- copy(dt_input_portfolio[,.(position_id, location)])
dt_portfolio_organization[, top_sub_portfolios := as.vector(sapply(location, get_first_element, splitter = "//"))]

#-------------------------------------------------------------------------------
#-- The Portfolio Future Growth Expectations                                  --
#-------------------------------------------------------------------------------
dt_positions_expected_growth <- copy(dt_input_portfolio[,.(position_id, expected_annual_growth_rate)])
dt_positions_expected_growth[, performance := (1 + expected_annual_growth_rate)^(set_horizon_days/252) - 1]
#-------------------------------------------------------------------------------
#-- Weight Constraints                                                        --
#-------------------------------------------------------------------------------
dt_weight_constraints <- copy(dt_portfolio[, .(position_id)])
dt_weight_constraints <- dt_weight_constraints[, ":=" (min_weight = 0.005, max_weight = 0.05)]
dt_weight_constraints <- merge(
  dt_weight_constraints,
  dt_portfolio_organization[,.(position_id, top_sub_portfolios)],
  by = "position_id",
  all.x = TRUE)
dt_weight_constraints[top_sub_portfolios == "FixedIncome", ":=" (min_weight = 0.005, max_weight = 0.1)]
dt_weight_constraints[, top_sub_portfolios := NULL]

#-------------------------------------------------------------------------------
#-- Cardinality Constraints                                                   --
#-------------------------------------------------------------------------------
constr_cardinality_lower <- length(dt_portfolio$position_id)
constr_cardinality_upper <- length(dt_portfolio$position_id)

#-------------------------------------------------------------------------------
#-- RISK BUDGET SPECIFICATION                                                 --
#-------------------------------------------------------------------------------
risk_budget_grps            <- c("Equities", "FixedIncome")
risk_budget_constraint      <- c(0.45, 0.6)

dt_risk_budget_constraints <- data.table("group"           = risk_budget_grps,
                                         "constraint"      = risk_budget_constraint)

# Check for embedded risk-budget. Embedded risk-budget is not supported yet
# defaults to the lowest level from the risk-budget list 
# c("Equities", "FixedIncome", "FixedIncome//CorporateBond") -> c("Equities", "FixedIncome")
#-------------------------------------------------------------------------------
#-- GENERATE WEIGHTS that adhere to the condition set out                     --
#-------------------------------------------------------------------------------
message("Generating Portfolio Weights")
dt_weigthts                 <- simulate_portfolio_weights(10000, dt_portfolio$position_id)

dt_weights_cardinality_adjusted <- constraint_cardinality(
  dt_weigthts, 
  constr_cardinality_lower, 
  constr_cardinality_upper)

message("Adjusting the weights so they are within the min-max weights specification")
dt_weigthts_weight_adjusted <- portfolio_weights_apply_weight_constraint(
  p_dt_weight             = dt_weights_cardinality_adjusted,
  p_dt_weight_constraints = dt_weight_constraints)

#-------------------------------------------------------------------------------
#-- RISK PROFILING OF THE PORTFOLIO                                           -- 
#-------------------------------------------------------------------------------
message("Calculating Expected Shortfall for the simulated portfolio weights")
if(TRUE){
  dt_es <- risk_profile_gaussian(simulated_delta_vectors[, dt_portfolio$position_id, with = FALSE], 
                                 dt_weigthts_weight_adjusted[, dt_portfolio$position_id, with = FALSE], 
                                 rsk_conf_level)
  dt_es_comp <- comb_risk_perf(dt_es[,.("ticker" = position_id, weight, "expected_shortfall_Weight" = risk_weight, exec_id)], 
                               dt_positions_expected_growth[,.("ticker" = position_id, performance)])
  
  dt_slp <- slope_analysis(tickers = unique(dt_positions_expected_growth$position_id), 
                           dt_es_comp)
} else {
  dt_expected_shortfall <- risk_calc_parallization(
    dt_weigthts_weight_adjusted[, dt_portfolio$position_id, with = FALSE],
    simulated_delta_vectors[, dt_portfolio$position_id, with = FALSE],
    rsk_conf_level,
    use_max_cpu_cores)
  
  dt_es_comb <- comb_risk_perf(dt_expected_shortfall, 
                               dt_positions_expected_growth[,.("ticker" = position_id, performance)])
  dt_slp     <- slope_analysis(tickers = unique(dt_portfolio$position_id), dt_es_comb)
}

dt_portfolio[, slope := NULL]
dt_portfolio <- merge(
  dt_portfolio,
  dt_slp[, .("position_id" = ticker, slope)],
  by = "position_id",
  all.x = TRUE)

dt_risk_adjusted_return <- copy(dt_portfolio)
dt_risk_adjusted_return <- merge(
  dt_risk_adjusted_return,
  dt_positions_expected_growth,
  by = "position_id",
  all.x = TRUE)

dt_risk_adjusted_return[expected_annual_growth_rate > rsk_free_rate & slope > 0, risk_adjusted_return := (expected_annual_growth_rate - rsk_free_rate) / abs(slope)]
dt_risk_adjusted_return[expected_annual_growth_rate > rsk_free_rate & slope < 0, risk_adjusted_return := (expected_annual_growth_rate - rsk_free_rate) / abs(slope)]
dt_risk_adjusted_return[expected_annual_growth_rate < rsk_free_rate & slope < 0, risk_adjusted_return := (expected_annual_growth_rate - rsk_free_rate) / slope]
dt_risk_adjusted_return[expected_annual_growth_rate < rsk_free_rate & slope > 0, risk_adjusted_return := (expected_annual_growth_rate - rsk_free_rate) / slope]
setorder(dt_risk_adjusted_return, -"risk_adjusted_return")

#dt_risk_adjusted_return[position_id == "f89801ec83de17e47fc433df9964499d"]
#-------------------------------------------------------------------------------
#--                                                                           --
#-- INITIAL GUESS RANGE FOR THE OPTIMAL WEIGHTS                               --
#--                                                                           --
#-------------------------------------------------------------------------------
dt_group_focus <- copy(dt_risk_adjusted_return)
dt_group_focus <- merge(
  dt_group_focus,
  dt_weight_constraints,
  by = "position_id",
  all.x = TRUE)

dt_group_focus <- merge(
  dt_group_focus,
  dt_portfolio_organization[,.(position_id, location)],
  by = "position_id",
  all.x = TRUE)

min_weight_allocation <- sum(dt_group_focus$min_weight)
max_weight_allocation <- sum(dt_group_focus$max_weight)

#dt_group_focus[position_id == "f89801ec83de17e47fc433df9964499d"]
#------------------------------------------------------------------------------
#-- Risk Budget Constraints                                                  --
#------------------------------------------------------------------------------
lst_all_obs_included <- lapply(dt_risk_budget_constraints$group, function(x){
  grepl(x, dt_group_focus$location)
})

make_complete_risk_budget_adj <- apply(
  do.call(rbind, 
          lst_all_obs_included), 2, "any")

min_risk_grps_to_append    <- unique(as.character(
  sapply(
    unique(dt_group_focus[!make_complete_risk_budget_adj]$location), 
    get_first_element, 
    splitter = "//")))

risk_budget_grps_adj       <- c(
  dt_risk_budget_constraints$group,
  min_risk_grps_to_append)

risk_budget_constraint_adj <- c(
  dt_risk_budget_constraints$constraint,
  rep(1, length(min_risk_grps_to_append)))

risk_budget_constraint_type_adj <- rep("<=", length(risk_budget_grps_adj))

stopifnot(sum(risk_budget_constraint_adj) >= 1)

dt_risk_budget_constraints_adj <- data.table(
  "group"           = risk_budget_grps_adj,
  "constraint_type" = risk_budget_constraint_type_adj,
  "constraint"      = risk_budget_constraint_adj)

#------------------------------------------------------------------------------
#-- Create the 'subject to' matrix, the constraint values (rhs), 
#-- the direction (dir) vector and the weight constraints.
#------------------------------------------------------------------------------
m <- nrow(dt_group_focus)
n <- length(risk_budget_grps_adj)

mat_fk <- lapply(risk_budget_grps_adj, function(x){
  as.numeric(grepl(x, dt_group_focus$location)) * dt_group_focus$slope
})
mat_fk <- do.call(rbind, mat_fk)
mat_fk <- rbind(mat_fk,
                rep(1, m))
rhs_fk <- c(dt_risk_budget_constraints_adj$constraint, 1.0)
dir_fk <- c(dt_risk_budget_constraints_adj$constraint_type, "==")
bounds_fk <- list(lower = list(ind = 1:m, val = dt_group_focus$min_weight),
                  upper = list(ind = 1:m, val = dt_group_focus$max_weight))

#------------------------------------------------------------------------------
#-- Solve LP - Maximize the Risk Adjusted Return                         -- 
#------------------------------------------------------------------------------
obj_fk <- dt_group_focus$risk_adjusted_return
lp_solution <- Rglpk::Rglpk_solve_LP(
  obj = obj_fk, 
  mat = mat_fk,
  dir = dir_fk,
  rhs = rhs_fk,
  max = TRUE,
  bounds = bounds_fk)

lp_solution$status == 0
sum(lp_solution$solution)

dt_suggested_weights <- cbind(dt_group_focus, "optimal_weight" = lp_solution$solution)
#------------------------------------------------------------------------------
#-- Solve LP - Minimize the Risk                                             -- 
#------------------------------------------------------------------------------
obj_fk <- dt_group_focus$slope
lp_solution <- Rglpk::Rglpk_solve_LP(
  obj = obj_fk, 
  mat = mat_fk,
  dir = dir_fk,
  rhs = rhs_fk,
  max = TRUE,
  bounds = bounds_fk)

lp_solution$status == 0
sum(lp_solution$solution)

dt_suggested_weights_min_risk <- cbind(dt_group_focus, "optimal_weight" = lp_solution$solution)

#------------------------------------------------------------------------------
#--                                                                          --
#------------------------------------------------------------------------------
lst_suggested_weights_by_grp <- lapply(dt_risk_budget_constraints_adj$group, function(x){
  pos_ids <- dt_suggested_weights[grepl(x, location)]$position_id
  dt_suggested_weights_tmp <- dt_suggested_weights[position_id %in% pos_ids, .(position_id, optimal_weight, slope)] 
  data.table("risk_budget_group" = x,
             "weight" = sum(dt_suggested_weights_tmp$optimal_weight),
             "risk"   = sum(dt_suggested_weights_tmp$optimal_weight * 
                            dt_suggested_weights_tmp$slope))
})
dt_suggested_weights_by_grp <- rbindlist(lst_suggested_weights_by_grp)

lst_suggested_weights_min_risk_by_grp <- lapply(dt_risk_budget_constraints_adj$group, function(x){
  pos_ids <- dt_suggested_weights[grepl(x, location)]$position_id
  dt_suggested_weights_tmp <- dt_suggested_weights_min_risk[position_id %in% pos_ids, .(position_id, optimal_weight, slope)] 
  data.table("risk_budget_group" = x,
             "weight" = sum(dt_suggested_weights_tmp$optimal_weight),
             "risk"   = sum(dt_suggested_weights_tmp$optimal_weight * 
                            dt_suggested_weights_tmp$slope))
})
dt_suggested_weights_min_risk_by_grp <- rbindlist(lst_suggested_weights_min_risk_by_grp)
dt_suggested_weights_min_risk_by_grp

#------------------------------------------------------------------------------
#-- Risk Calc Maximum Performance                                            --
#------------------------------------------------------------------------------
es_max_perf <- calculate_port_es(simulated_delta_vectors[, dt_suggested_weights$position_id, with = FALSE],
                                 dt_suggested_weights$optimal_weight, 
                                 do_component = TRUE, 
                                 confidence_level = rsk_conf_level)

risk_max_perf <- es_max_perf[-1]/as.numeric(es_max_perf["ES"])
dt_risk_max_perf <- data.table("position_id" = names(risk_max_perf),
                               "risk"   = -1 * es_max_perf[-1],
                               "risk_attribution"   = risk_max_perf)

dt_risk_max_perf <- merge(
  dt_risk_max_perf,
  dt_suggested_weights[,.(position_id, "weight" = optimal_weight, "perf" = expected_annual_growth_rate, location)],
  by = "position_id",
  all.x = TRUE)

lst_risk_sum_max_perf_by_grp <- lapply(dt_risk_budget_constraints_adj$group, function(x){
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
  dt_risk_budget_constraints_adj[,.("risk_budget_group" = group, constraint)],
  by = "risk_budget_group",
  all.x = TRUE)
dt_risk_sum_max_perf_by_grp[, in_valid := risk_attribution > constraint]

#------------------------------------------------------------------------------
#-- Risk Calc Minimum Risk                                                   --
#------------------------------------------------------------------------------
es_min_risk <- calculate_port_es(simulated_delta_vectors[, dt_suggested_weights_min_risk$position_id, with = FALSE],
                                 dt_suggested_weights_min_risk$optimal_weight, 
                                 do_component = TRUE, 
                                 confidence_level = rsk_conf_level)

risk_min_risk <- es_min_risk[-1]/as.numeric(es_min_risk["ES"])
dt_risk_min_risk <- data.table("position_id" = names(risk_min_risk),
                               "risk"   = -1 * es_min_risk[-1],
                               "risk_attribution"   = risk_min_risk)

dt_risk_min_risk <- merge(
  dt_risk_min_risk,
  dt_suggested_weights_min_risk[,.(position_id, "weight" = optimal_weight, "perf" = expected_annual_growth_rate, location)],
  by = "position_id",
  all.x = TRUE)

lst_risk_sum_min_risk_by_grp <- lapply(dt_risk_budget_constraints_adj$group, function(x){
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
  dt_risk_budget_constraints_adj[,.("risk_budget_group" = group, constraint)],
  by = "risk_budget_group",
  all.x = TRUE)
dt_risk_sum_min_risk_by_grp[, in_valid := risk_attribution > constraint]

#------------------------------------------------------------------------------
#-- Setup the portfolio Weight Variation                                     --
#------------------------------------------------------------------------------
dt_risk_sum_max_perf_by_grp[in_valid == TRUE, range_tmp_1 := weight / (risk_attribution / constraint)]
dt_risk_sum_min_risk_by_grp[in_valid == TRUE, range_tmp_2 := weight / (risk_attribution / constraint)]
dt_risk_sum_max_perf_by_grp[in_valid == FALSE, range_tmp_1 := weight]
dt_risk_sum_min_risk_by_grp[in_valid == FALSE, range_tmp_2 := weight]

dt_suggested_limits <- merge(
  dt_risk_sum_max_perf_by_grp[,.(risk_budget_group, range_tmp_1)],
  dt_risk_sum_min_risk_by_grp[,.(risk_budget_group, range_tmp_2)],
  by = "risk_budget_group")
dt_suggested_limits[, ":=" (lower_weight_lim = mapply(min, range_tmp_1, range_tmp_2), upper_weight_lim = mapply(max, range_tmp_1, range_tmp_2))]
dt_suggested_limits[, ":=" (range_tmp_1 = NULL, range_tmp_2 = NULL)]

dt_suggested_limits[, ":=" (lower_weight_lim_tmp = lower_weight_lim - mapply(max, 0.5 * (upper_weight_lim - lower_weight_lim), 0.025),
                            upper_weight_lim_tmp = upper_weight_lim + mapply(max, 0.5 * (upper_weight_lim - lower_weight_lim), 0.025))]

dt_suggested_limits[, ":=" (lower_weight_lim = lower_weight_lim_tmp,
                            upper_weight_lim = upper_weight_lim_tmp)]
dt_suggested_limits[, ":=" (lower_weight_lim_tmp = NULL,
                            upper_weight_lim_tmp = NULL)]

dt_suggested_limits[, lower_weight_lim := mapply(max, lower_weight_lim, 0)]
dt_suggested_limits[, upper_weight_lim := mapply(min, upper_weight_lim, 1)]

dt_risk_sum_max_perf_by_grp[, range_tmp_1 := NULL]
dt_risk_sum_min_risk_by_grp[, range_tmp_2 := NULL]

#------------------------------------------------------------------------------
#-- Number of Portfolio Weight Simulations                                   --
#------------------------------------------------------------------------------
n_sim <- 10000

#------------------------------------------------------------------------------
#-- Portfolio Weight Simulations - Risk-Budget-Groups                        --
#------------------------------------------------------------------------------
lst_unif_sim <- lapply(1:nrow(dt_suggested_limits), function(x){
  runif(n_sim, 
       min = dt_suggested_limits[x, lower_weight_lim],
       max = dt_suggested_limits[x, upper_weight_lim])
})
dt_unif_sim <- do.call(cbind, lst_unif_sim)
colnames(dt_unif_sim) <- dt_suggested_limits$risk_budget_group
dt_multi_simz <- data.table(t(apply(dt_unif_sim, 1, normalize)))

#------------------------------------------------------------------------------
#-- Apply Weight Constraints                                                 --
#------------------------------------------------------------------------------
select_grps <- dt_risk_sum_max_perf_by_grp$risk_budget_group
message("Setting up the portfolio weights with weight-constraints")
dt_weight_weight_adjusted_sims <- risk_budget_weight_adjustment(
  dt_suggested_weights[, .(position_id, location, min_weight, max_weight)], 
  dt_multi_simz, 
  select_grps,
  n_sim)
#apply(dt_weight_weight_adjusted_sims, 1, "sum")

#------------------------------------------------------------------------------
#-- Calculate Risk and check Risk-Budget Constraints                         --
#------------------------------------------------------------------------------
es_tester <- risk_calc_parallization(
  dt_weight_weight_adjusted_sims,
  simulated_delta_vectors[, colnames(dt_weight_weight_adjusted_sims), with = FALSE],
  rsk_conf_level,
  use_max_cpu_cores)

invisible(lapply(dt_risk_budget_constraints_adj$group, function(x){
  dt_suggested_weights[grepl(x, location), risk_budget_group := x]  
}))

dt_check <- merge(
  es_tester[, .(exec_id, "position_id" = ticker, weight, expected_shortfall, expected_shortfall_Weight)],
  dt_suggested_weights[, .(position_id, risk_budget_group)],
  by = "position_id",
  all.x = TRUE)

dt_check_agg <- dt_check[, .(weight = sum(weight),
             es_attribution = sum(expected_shortfall_Weight)),
         by = c("exec_id", "risk_budget_group")]

dt_check_agg <- merge(
  dt_check_agg,
  dt_risk_budget_constraints_adj[,.("risk_budget_group" = group, constraint)],
  by = "risk_budget_group",
  all.x = TRUE)
dt_check_agg[, is_valid := es_attribution <= constraint]
dt_check_agg_by_exec_id <- dt_check_agg[, .(is_risk_budget_valid = all(is_valid)), by = exec_id]

use_exec_ids <- dt_check_agg_by_exec_id[is_risk_budget_valid == TRUE]$exec_id
dt_rsk_perf <- es_tester[exec_id %in% use_exec_ids]
setnames(dt_rsk_perf, "ticker", "position_id")
dt_rsk_perf <- merge(
  dt_rsk_perf,
  dt_suggested_weights[,.(position_id, expected_annual_growth_rate, min_weight, max_weight)],
  by = "position_id",
  all.x = TRUE)

# Check that the weight constraints are full filled
dt_rsk_perf_weight_constraints <- dt_rsk_perf[, .(is_weight_valid = all(between(weight, min_weight, max_weight))), by = "exec_id"]
use_exec_ids <- dt_rsk_perf_weight_constraints[is_weight_valid == TRUE]$exec_id

dt_rsk_perf_by_exec_id <- dt_rsk_perf[exec_id %in% use_exec_ids, .(perf = sum(expected_annual_growth_rate * weight), 
                risk = sum(-1 * expected_shortfall)), 
            by = exec_id]

#-------------------------------------------------------------------------------
#-- PORTFOLIO WEIGHT FULL FILLING THE RISK BUGET                              --
#-------------------------------------------------------------------------------
dt_weight_constraint_fullfilled <- dcast.data.table(dt_rsk_perf[exec_id %in% use_exec_ids,.(exec_id, position_id, weight)], exec_id ~ position_id, value.var = "weight") 
dt_weight_constraint_fullfilled <- dt_weight_constraint_fullfilled[, -1]
dt_weight_constraint_fullfilled

stopifnot(nrow(dt_weight_constraint_fullfilled) > 100)

#-------------------------------------------------------------------------------
#-- PORTFOLIO WEIGHT FULL FILLING THE RISK BUGET                              --
#-------------------------------------------------------------------------------
rsk_free_rate_horizon <- (1+rsk_free_rate)^(set_horizon_days/252) - 1
dt_rsk <- dt_rsk_perf[exec_id %in% use_exec_ids, .(ES = sum(expected_shortfall), PERF = sum(expected_annual_growth_rate * weight)), by = exec_id]
dt_efficient_frontier <- efficient_frontier(dt_rsk, rsk_free_rate_horizon)

#### MIN RISK PORTFOLIO ####
min_risk_scenarie <- dt_rsk[ES == max(ES)]

#### OPTIMAL PORTFOLIO ####
optimal_scenarie <- dt_efficient_frontier[which.max(tangent), max_perf_scenario]
optimal_exec_id <- dt_rsk[optimal_scenarie, exec_id]

#### Equivalent Portfolios - by perf > rsk_free_rate + tangent * 0.95 * risk ####
tangent_variation_level <- 0.05
max_tangent <- max(dt_efficient_frontier$tangent)
dt_eq_port <- copy(dt_rsk)
dt_eq_port[, aa := rsk_free_rate + max_tangent * (1 - tangent_variation_level) * -1 * ES]
dt_eq_port[, is_perf_above := PERF >= aa]
dt_search_set <- dt_eq_port[is_perf_above == TRUE]$exec_id

dt_equivalent_portfolios <- dt_rsk_perf[exec_id %in% dt_search_set, .(exec_id, position_id, weight)]
setorder(dt_equivalent_portfolios, exec_id)
dt_equivalent_portfolios_cast <- dcast.data.table(dt_equivalent_portfolios, exec_id ~ position_id, value.var = "weight")

#undebug(portfolio_distance)
exec_id_index <- portfolio_distance(dt_portfolio$weight, dt_equivalent_portfolios_cast[, dt_portfolio$position_id, with = FALSE][, -1])
dt_equivalent_port_res <- data.table(
  "exec_id" = dt_equivalent_portfolios_cast$exec_id,
  "SSD"     = exec_id_index)

equivalent_exec_id <- dt_equivalent_port_res[SSD == min(SSD)]$exec_id
dt_equivalent_portfolios_out <- merge(
  dt_portfolio[,.(position_id, "current_weight" = weight)],
  dt_equivalent_portfolios[exec_id == equivalent_exec_id, .(position_id, "near_optimal_weight" = round(weight, 3))],
  by = "position_id",
  all.x = TRUE)

dt_equivalent_portfolios_out <- merge(
  dt_equivalent_portfolios_out,
  dt_equivalent_portfolios[exec_id == optimal_exec_id, .(position_id, "optimal_weight" = round(weight, 3))],
  by = "position_id",
  all.x = TRUE)

sum((dt_equivalent_portfolios_out$current_weight - dt_equivalent_portfolios_out$optimal_weight)**2)
sum((dt_equivalent_portfolios_out$current_weight - dt_equivalent_portfolios_out$near_optimal_weight)**2)

#### PLOT ####
plot(dt_rsk_perf_by_exec_id$risk, 
     dt_rsk_perf_by_exec_id$perf, 
     xlim = c(0.0, max(dt_rsk_perf_by_exec_id$risk)), 
     ylim = c(0.0, max(dt_rsk_perf_by_exec_id$perf)),
     xlab = "Risk",
     ylab = "Performance")
#points(dt_efficient_frontier[tangent != max(tangent), .(avg_es, max_perf)], col = "red", pch = 19)
points(-min_risk_scenarie$ES, 
       min_risk_scenarie$PERF, 
       col = "#FBDE44FF", 
       pch = 19)
points(dt_efficient_frontier[tangent == max(tangent), .(avg_es, max_perf)], 
       col = "#F65058FF", 
       pch = 19)
points(0, rsk_free_rate_horizon, 
       col = "#28334AFF", 
       pch = 19)
abline(a   = rsk_free_rate_horizon, 
       b   = max(dt_efficient_frontier$tangent),
       lty = 2)


# Market Portfolio
dt_efficient_frontier[tangent == max(tangent),.("performance"    = max_perf,
                                                "risk"           = avg_es,
                                                "tangent"        = tangent,
                                                "risk_free_rate" = rsk_free_rate_horizon)]

# By Positions
dt_rsk_perf[exec_id == optimal_exec_id, .(position_id, 
                                          "optimal_weights"           = weight, 
                                          "performance"               = expected_annual_growth_rate, 
                                          expected_shortfall, 
                                          "expected_shortfall_weight" = expected_shortfall_Weight)]

# By Risk-Budget
dt_risk_per_inkl_risk_group <- copy(dt_rsk_perf[exec_id == optimal_exec_id])
dt_risk_per_inkl_risk_group <- merge(
  dt_risk_per_inkl_risk_group,
  dt_group_focus[, .(position_id, location)],
  by = "position_id",
  all.x = TRUE)

lst_risk_budget_group_summary <- lapply(dt_risk_budget_constraints_adj$group, function(x){
  #x <- dt_risk_budget_constraints_adj$group[1]
  dt_tmp <- dt_risk_per_inkl_risk_group[
    grepl(x, location), .("optimal_weights"           = sum(weight), 
                          "performance"               = sum(weight * expected_annual_growth_rate), 
                          "expected_shortfall"        = sum(expected_shortfall),
                          "expected_shortfall_weight" = sum(expected_shortfall_Weight))]
  dt_tmp[, risk_budget_group := x]
  dt_tmp
})
dt_risk_budget_group_summary <- rbindlist(lst_risk_budget_group_summary)

#apply(dt_weight_weight_adjusted_sims, 1, "sum")

