options(digits = 8)
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/tmpl_strategic_asset_allocation_dependencies.R")
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/AssetAllocation/ClassMeanCVaR.R")

#-------------------------------------------------------------------------------
#--                                                                           --
#-- ASSET ALLOCATION ANALYSIS                                                 --
#--                                                                           --
#-------------------------------------------------------------------------------
# Asset allocation analysis is a critical process in investment management, 
# aiming to optimize the distribution of assets within a portfolio to balance 
# risk against return based on an investor's goals, risk tolerance, and 
# investment horizon. It's important because it determines how an investor's 
# capital is spread across different asset classes—such as stocks, bonds, and 
# cash—to maximize returns while minimizing risk. This strategic diversification 
# is key to navigating market volatility and achieving long-term 
# financial objectives.
#
# The output of asset allocation analysis is a tailored investment strategy that 
# outlines the ideal percentage of each asset class in the portfolio. This 
# strategy guides investment decisions, ensuring that the portfolio's 
# composition aligns with the investor's risk appetite and return expectations. 
# Over time, the analysis may also include recommendations for rebalancing the 
# portfolio to maintain the desired asset allocation, adapting to changes in 
# market conditions, financial goals, or risk tolerance. Ultimately, asset 
# allocation analysis provides a foundation for informed investment choices, 
# aiming to enhance portfolio performance and support the achievement of 
# financial milestones.
#
#To conduct asset allocation analysis, several key data elements are necessary:
#  
# * Base Currency: Essential for uniform valuation and comparison of international investments.
# * Risk-Free Interest Rate: Acts as a benchmark for comparing investment returns against a no-risk option.
# * Portfolio Details: Including unique position identifiers, expected yearly performance, current weights, and customizable location categories for organization.
# * Weight and Risk Budget Constraints: Specified in 'tbl_weight_constraints' and 'tbl_risk_budget_constraints' to manage allocation and risk levels.
# * Economic Scenario Duration: Projected time spent in different economic conditions, affecting long-term PnL simulations.
# * Performance Expectations: Defined for each position across all scenarios, ensuring explicit anticipation of returns.
# These components enable a comprehensive and nuanced analysis, allowing for the 
# strategic allocation of assets in line with volatility expectations, risk 
# tolerance, and investment objectives.
#-------------------------------------------------------------------------------
#--                                                                           --
#-- Load the Example Data                                                     --
#--                                                                           --
#-------------------------------------------------------------------------------
load_data <- readRDS(url("https://www.dropbox.com/scl/fi/lvhwcw55sjorcbucantqo/load_asset_allocation_input_data_example.rds?rlkey=vu6i7wrf4w9oben2hq87js29e&raw=true","rb"))

tbl_portfolio                 <- copy(load_data[["tbl_master_portfolio_information"]][, .(position_id, "current_weight" = weight)])
tbl_portfolio_organization    <- copy(load_data[["tbl_master_portfolio_information"]][, .(position_id, location)])
tbl_expected_mean_performance <- copy(load_data[["tbl_expected_yearly_performance"]][, .(position_id, "performance" = expected_annual_growth_rate)])
tbl_weight_constraints        <- copy(load_data[["tbl_weight_constraints"]][, .(position_id, min_weight, max_weight)])
tbl_pnl_simulations           <- copy(load_data[["tbl_profit_loss_simulations"]][, .(position_id, "scenario_id" = path_scenario, pnl_pct)])
tbl_risk_budget_constraints   <- copy(load_data[["tbl_risk_budget_constraints"]])

risk_free_interest_rate <- copy(load_data[["risk_free_interest_rate"]])
horizon_years           <- copy(load_data[["horizon_years"]])

#tbl_risk_budget_constraints[group == "Equities", constraint := 0.10]
#-------------------------------------------------------------------------------
#--                                                                           --
#-- Initialize the MeanCVaR Object - Required                                 --
#--                                                                           --
#-------------------------------------------------------------------------------
my_mean_cvar <- MeanCVaR$new(tbl_portfolio)
my_mean_cvar$set_risk_free_interest_rate(risk_free_interest_rate)
my_mean_cvar$set_max_cores(4)
my_mean_cvar$set_count_simulations(10000)

#-------------------------------------------------------------------------------
#-- Required
#-------------------------------------------------------------------------------
my_mean_cvar$set_tbl_pnl_simulation(tbl_pnl_simulations)
my_mean_cvar$set_tbl_expected_mean_performance(tbl_expected_mean_performance[,.(position_id, performance)])

#-------------------------------------------------------------------------------
#-- Optional (Required): Portfolio Organization                               --
#-- If a risk-budget constraint is specified, it becomes mandatory/required.  --
#-------------------------------------------------------------------------------
my_mean_cvar$set_tbl_portfolio_location(tbl_portfolio_organization)

#-------------------------------------------------------------------------------
#-- Optional: 
#-------------------------------------------------------------------------------
my_mean_cvar$set_tbl_positions_weight_constraints(tbl_weight_constraints)
my_mean_cvar$set_tbl_risk_budget_constraints(tbl_risk_budget_constraints)

#-------------------------------------------------------------------------------
#--                                                                           --
#-- Asset Allocation Analysis                                                 --
#--                                                                           --
#-------------------------------------------------------------------------------
#undebug(my_mean_cvar$.__enclos_env__$private$do_perform_asset_allocation_analysis)
my_mean_cvar$.__enclos_env__$private$do_perform_asset_allocation_analysis()
my_mean_cvar$.__enclos_env__$private$asset_allocation_analysis



#-------------------------------------------------------------------------------
#--                                                                           --
#-- SCRIPLES                                                                  --
#--                                                                           --
#-------------------------------------------------------------------------------
dt_aa <- copy(my_mean_cvar$.__enclos_env__$private$asset_allocation_analysis[["SUMMARY_MARKET_PORTFOLIO_BY_RISK_BUDGET"]])
portfolio_performance_pr_unit_of_risk <- sum(dt_aa$performance)/sum(dt_aa$risk)
dt_aa[, performance_risk_ratio := round(performance_weight / risk_weight, 2)] #performance - portfolio_performance_pr_unit_of_risk * risk
dt_agg_aa <- rbind(
  dt_aa,
  dt_aa[, .(
  "group"                  = "Portfolio",
  "weight"                 = sum(weight),
  "performance"            = sum(performance),
  "performance_weight"     = sum(performance_weight),
  "risk"                   = sum(risk),
  "risk_weight"            = sum(risk_weight),
  "exec_id"                = "",
  "performance_risk_ratio" = 1.00)])

#-------------------------------------------------------------------------------
#--                                                                           --
#-- Do 10 calculations with the same inputs                                   --
#--                                                                           --
#-------------------------------------------------------------------------------
lst_asset_allocation_analysis <- pbapply::pblapply(1:10, function(x){
  my_mean_cvar$.__enclos_env__$private$do_perform_asset_allocation_analysis()
  my_mean_cvar$.__enclos_env__$private$asset_allocation_analysis
})

lst_slopes <- lapply(lst_asset_allocation_analysis, function(x){
  cbind(
    x[["SUMMARY_OPTIMAL_PORTFOLIO"]])
})
dt_slopes <- rbindlist(lst_slopes)

lst_rsk_budget <- lapply(lst_asset_allocation_analysis, function(x){
  dt_out <- copy(x[["SUMMARY_BY_RISK_BUDGET"]])
  exec_id <- x[["SUMMARY_OPTIMAL_PORTFOLIO"]][["exec_id"]]
  dt_out[, exec_id := exec_id]
})
dt_rsk_budget <- rbindlist(lst_rsk_budget)
dt_rsk_budget_melt <- melt.data.table(dt_rsk_budget, id.vars = c("group", "type", "exec_id")) 
setnames(dt_rsk_budget_melt, "variable", "measure")

dt_rsk_budget <- merge(
  dt_rsk_budget[type == "optimal_weights"],
  dt_slopes[,.(exec_id, slope)],
  by = "exec_id",
  all.x = TRUE)

summary_assat_allocation_analysis_weights <- merge(
  dt_rsk_budget[,.("mean_iteration" = mean(weight)), by = c("group")],
  dt_rsk_budget[slope == max(slope),.("best_iteration" = weight), by = c("group")],
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_weights <- merge(
  dt_rsk_budget[slope == min(slope),.("worst_iteration" = weight), by = c("group")],
  summary_assat_allocation_analysis_weights,
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_weights[, type := "weight"]

summary_assat_allocation_analysis_performance <- merge(
  dt_rsk_budget[,.("mean_iteration" = mean(performance)), by = c("group")],
  dt_rsk_budget[slope == max(slope),.("best_iteration" = performance), by = c("group")],
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_performance <- merge(
  dt_rsk_budget[slope == min(slope),.("worst_iteration" = performance), by = c("group")],
  summary_assat_allocation_analysis_performance,
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_performance[, type := "performance"]

summary_assat_allocation_analysis_risk_weight <- merge(
  dt_rsk_budget[,.("mean_iteration" = mean(risk_weight)), by = c("group")],
  dt_rsk_budget[slope == max(slope),.("best_iteration" = risk_weight), by = c("group")],
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_risk_weight <- merge(
  dt_rsk_budget[slope == min(slope),.("worst_iteration" = risk_weight), by = c("group")],
  summary_assat_allocation_analysis_risk_weight,
  by = "group",
  all.x = TRUE)
summary_assat_allocation_analysis_risk_weight[, type := "risk_weight"]

summary_assat_allocation_analysis <- rbind(
  summary_assat_allocation_analysis_weights,
  summary_assat_allocation_analysis_performance,
  summary_assat_allocation_analysis_risk_weight)

dt_rsk_budget_melt$exec_id <- factor(dt_rsk_budget_melt$exec_id, levels = unique(dt_rsk_budget_melt$exec_id), labels = paste0("Sc_", 1:length(unique(dt_rsk_budget_melt$exec_id))))
ggplot(dt_rsk_budget_melt[type == "optimal_weights" & measure == "weight"], aes(x = exec_id, y = value, fill = group)) +
  geom_bar(stat = "identity") 


lst_optimal_weight <- lapply(lst_asset_allocation_analysis, function(x){
  dt_out <- copy(x[["OPTIMAL_WEIGHTS_MARKET_PORTFOLIO"]])
  exec_id <- x[["SUMMARY_OPTIMAL_PORTFOLIO"]][["exec_id"]]
  dt_out[, exec_id := exec_id]
})
dt_optimal_weight      <- rbindlist(lst_optimal_weight)
dt_optimal_weight_melt <- melt.data.table(dt_optimal_weight, id.vars = c("exec_id", "position_id")) 
setnames(dt_optimal_weight_melt, "variable", "measure")

dt_optimal_weight_melt$exec_id <- factor(dt_optimal_weight_melt$exec_id, 
                                         levels = unique(dt_optimal_weight_melt$exec_id), 
                                         labels = paste0("Sc_", 1:length(unique(dt_optimal_weight_melt$exec_id))))

  



