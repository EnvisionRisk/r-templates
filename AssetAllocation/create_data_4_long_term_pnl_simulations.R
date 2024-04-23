options(digits = 8)
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_strategic_asset_allocation_dependencies.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_path_generator.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_helpers.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/ClassPnLSimulation.R")

#-------------------------------------------------------------------------------
#--                                                                           --
#-- CREATE THE INPUT-DATA NECESSARY TO CONDUCT THE ASSET-ALLOCATION ANALYSIS  --
#-- AND SAVE IT TO A FILE FOR EASY RETRIVAL                                   --
#--                                                                           --
#-------------------------------------------------------------------------------
# Access all instruments currently available in the EnvisionRisk system.
tbl_instruments_name   <- EnvisionRiskRaaS::envrsk_instrument_search()[valid_to >= Sys.Date(),.(symbol, name)]

# In portfolio analytics, the base currency is the currency against which the 
# value of an investment portfolio is measured, enabling consistent evaluation 
# and comparison of international assets regardless of their local currency 
# denomination.
base_cur               <- "DKK"

# In simple terms, the risk-free interest rate is like a baseline or guaranteed 
# rate you can earn without taking any risk, often represented by government 
# bonds. In asset allocation analysis, it's used as a starting point to compare 
# other investments. If an investment doesn't offer a higher return than this 
# risk-free rate, considering the extra risk involved, it might not be worth 
# including in a portfolio. Essentially, it helps investors decide how to spread 
# their investments across different assets by weighing the extra returns 
# against the extra risks.
risk_free_interest_rate <- 0.035

# The table below provides a detailed view of the portfolio being analyzed. It 
# features a unique position_id for each entry, an instrument-ticker symbol for 
# easy identification, the anticipated yearly performance for each position, its 
# current portfolio weight, and a location column. This column is designed to 
# categorize the portfolio into tailor-made buckets, thereby streamlining 
# analysis. The use of "//" delineates the main portfolio from its 
# sub-portfolios. For instance, in the example given, the portfolio is 
# segmented into four main sub-portfolios: Equities, FixedIncome, Alternatives, 
# and Cash. Each category is further broken down into more specific 
# sub-portfolios, creating a hierarchical, tree-like organizational structure 
# for comprehensive portfolio management.
tbl_input_portfolio    <- readRDS("C:/Users/jonas/Dropbox/Public/Data/portfolio_by_asset_classes.rds")
if(round(sum(tbl_input_portfolio[["weight"]]), getOption("digits") - 1) != 1.0){
  sum_weight <- round(sum(tbl_input_portfolio[["weight"]]), getOption("digits"))
  tbl_input_portfolio[, weight := round(weight / sum_weight, getOption("digits"))]
}

tbl_input_portfolio <- merge(
  tbl_input_portfolio,
  tbl_instruments_name,
  by = "symbol",
  all.x = TRUE)

# The tables 'tbl_weight_constraints' and 'tbl_risk_budget_constraints' 
# outlined below specify the constraints applied in the asset allocation 
# analysis.
#
# The 'tbl_weight_constraints' table sets the permissible variation range for 
# each weight. To indicate the absence of constraints, set min_weight to 0 and 
# max_weight to 1.
tbl_weight_constraints      <- readRDS("C:/Users/jonas/Dropbox/Public/Data/portfolio_weight_constraints.rds")

# The table 'tbl_risk_budget_constraints' defines the risk-budget constraints. 
# A risk budget in asset allocation is essentially an allocation of how much 
# risk, usually measured in terms of volatility or potential for loss, each 
# part of an investment portfolio is allowed to take on. It's used as a 
# constraint to ensure that the overall risk level of the portfolio stays 
# within acceptable bounds and aligns with the investor's risk tolerance, 
# investment objectives, and time horizon. By setting a risk budget, investors 
# can strategically distribute risk across various assets, optimizing the 
# potential for returns while keeping the risk at a manageable level. This 
# approach helps in creating a balanced and diversified portfolio that aims for 
# the best possible outcomes within the defined risk parameters.
#
# The constraint must be established at the highest levels of the portfolio, 
# although it's not mandatory for all top-tier portfolios to be constrained. In 
# the example provided, only two of the top-level categories, Equities and 
# FixedIncome, have specified constraints.
tbl_risk_budget_constraints <- readRDS("C:/Users/jonas/Dropbox/Public/Data/portfolio_risk_budget_constraints.rds")

#------------------------------------------------------------------------------
#--                         --
#------------------------------------------------------------------------------
# The table 'tbl_pct_time_spend_pct_in_econ_cycle' outlines your projections on 
# the amount of time allocated to different economic scenarios, significantly 
# influencing the long-term profit and loss (PnL) simulations. We currently 
# provide three distinct volatility scenarios for analysis: "downturn," 
# "through_the_cycle," and "point_in_time." This table indicates the proportion 
# of time (as a percentage of horizon_years) spent in each scenario. The table 
# displays the duration (as a percentage of the total horizon years) for each 
# economic scenario. It's important to note that the exact timing of these 
# economic conditions is less critical, as our primary focus lies in the 
# simulated PnL values at the conclusion of the horizon_years.
#
# If you lack specific forecasts for future economic conditions, a preliminary 
# approach might be to anticipate that future volatility will mirror the 
# historical average (the “through_the_cycle” scenario). While accurately 
# predicting future economic fluctuations is notoriously challenging, 
# delineating broad scenarios and their expected durations can add substantial 
# value to various applications. 
#
# In the following example, we project 10 years into the future, with a horizon 
# of 10 years. We estimate that out of these 10 years, 5.5 years (equivalent to 
# 1386 days, calculated as 5.5 years * 252 trading days per year * 0.55) will 
# correspond to an average economic scenario. Meanwhile, 1.5 years are 
# anticipated to experience a downturn volatility scenario, and 3 years are 
# expected to witness a "bull-run" market condition. Although we don't have a 
# specific scenario designated for a bull-run, we utilize the point-in-time 
# scenario in conjunction with a selected reference date to serve as a stand-in 
# for our anticipated bull-run scenario. The reference-date in this example 
# reflects a day with low volatility-levels.
#
# The point-in-time (PIT) scenario offers the flexibility to adapt a volatility 
# scenario to meet your specific requirements. It captures the volatility level 
# on a chosen reference date, allowing you to modify the volatility scenario by 
# selecting a different date. To effectively utilize this feature, identify a 
# reference date when the volatility levels of the key assets in your portfolio 
# align with your projections for future volatility in your tailored scenario. 
# This customization enables you to precisely align the volatility assumptions 
# with your expectations, ensuring the model closely mirrors your anticipated 
# market conditions.
horizon_years               <- 1
ref_date_pit                <- as.Date("2023-11-30")

tbl_pct_time_spend_pct_in_econ_cycle <- readRDS("C:/Users/jonas/Dropbox/Public/Data/portfolio_pct_time_spend_pct_in_econ_cycle.rds")
#tbl_pct_time_spend_pct_in_econ_cycle[, time_spent_pct := 0]
#tbl_pct_time_spend_pct_in_econ_cycle[volatility_id == "through_the_cycle", time_spent_pct := 1.0]
stopifnot(round(sum(tbl_pct_time_spend_pct_in_econ_cycle$time_spent_pct), 4) == 1.0)

volatility_ids              <- tbl_pct_time_spend_pct_in_econ_cycle[time_spent_pct > 0][["volatility_id"]]

#------------------------------------------------------------------------------
#-- SETUP EXPECTED PERFORMANCE FOR EACH VOLATILITY_ID                        --
#------------------------------------------------------------------------------
# The selection of volatility scenarios must also be mirrored in the expected
# performance projections. This means that for each included scenario, there
# should be a clearly defined performance expectation for every position. While
# the expectations across different volatility scenarios may not vary, it's
# essential to explicitly outline these anticipations. Even if the performance
# expectation for each position remains consistent across scenarios, it must be
# distinctly specified for each one, ensuring no assumptions are made about
# uniformity without explicit confirmation.
tbl_expected_performance <- tbl_input_portfolio[,.(position_id, expected_annual_growth_rate)]

lst_performance_by_vol_id <- lapply(volatility_ids, function(x){
  tbl_out <- copy(tbl_expected_performance)
  tbl_out[, volatility_id := x]
  tbl_out
})
tbl_expected_performance <- rbindlist(lst_performance_by_vol_id)

#------------------------------------------------------------------------------
#-- FETCH ONE-DAY PNL SIMULATIONS FOR EACH SYMBOL AND VOLATILITY_ID          --
#------------------------------------------------------------------------------
obj_pnl_simulations <- PnLSimulationDataFetch$new(volatility_ids)  
obj_pnl_simulations$set_base_cur(base_cur)
obj_pnl_simulations$set_ref_date(ref_date_pit)
obj_pnl_simulations$set_symbols(unique(tbl_input_portfolio$symbol))

tbl_one_day_delta_vector_pct  <- copy(obj_pnl_simulations$get_pnl_simulations())

#------------------------------------------------------------------------------
#-- FETCH ONE-DAY PNL SIMULATIONS FOR EACH SYMBOL AND VOLATILITY_ID          --
#------------------------------------------------------------------------------
tbl_one_day_delta_vector_pct <- merge(
  tbl_one_day_delta_vector_pct, 
  tbl_input_portfolio[,.(symbol, position_id)],
  by    = "symbol",
  all.x = TRUE)
tbl_one_day_delta_vector_pct[, symbol := NULL]
setnames(tbl_one_day_delta_vector_pct, "scenario", "sim_id")

#------------------------------------------------------------------------------
#-- PERFORM LONG TERM PNL SIMULATION                                         --
#------------------------------------------------------------------------------
# Initialize the object
obj_long_pnl_simulations <- LongTermPnLSimulation$new(
  tbl_one_day_delta_vector_pct,
  tbl_expected_performance)  

# Set
obj_long_pnl_simulations$set_max_cores(4)
obj_long_pnl_simulations$set_horizon_years(horizon_years)
obj_long_pnl_simulations$set_demean(FALSE)
obj_long_pnl_simulations$set_tbl_pct_time_spend_in_econ_cycle(tbl_pct_time_spend_pct_in_econ_cycle)

# Get the long term simulated PnL and expected_yearly_performance
tbl_profit_loss_simulations     <- obj_long_pnl_simulations$get_tbl_long_term_pnl()
tbl_expected_yearly_performance <- obj_long_pnl_simulations$get_tbl_expected_annual_growth_rate()

#------------------------------------------------------------------------------
#--                         --
#------------------------------------------------------------------------------
# Save the Data for asset allocation analysis
load_data <- list(
  "tbl_master_portfolio_information" = tbl_input_portfolio[,.(position_id, symbol, name, location, weight)],
  "tbl_profit_loss_simulations"      = tbl_profit_loss_simulations,
  "tbl_expected_yearly_performance"  = tbl_expected_yearly_performance,
  "tbl_weight_constraints"           = tbl_weight_constraints,
  "tbl_risk_budget_constraints"      = tbl_risk_budget_constraints,
  "base_cur"                         = base_cur,
  "risk_free_interest_rate"          = risk_free_interest_rate,
  "horizon_years"                    = horizon_years)
#saveRDS(load_data, "C:/Users/jonas/Dropbox/Public/Data/load_asset_allocation_input_data_example.rds")
