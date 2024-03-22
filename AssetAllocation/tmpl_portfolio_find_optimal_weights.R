find_optimal_portfolio_weights <- function(p_dt_sim_paths,
                                           p_dt_annual_growth_rate,
                                           p_rsk_free_rate,
                                           p_rsk_conf_level,
                                           p_asset_weight_max_lower_limit = NULL,
                                           p_asset_weight_min_upper_limit = NULL,
                                           p_cardinality_lower = NULL,
                                           p_cardinality_upper = NULL,
                                           p_horizon_years = 1){
  # p_dt_sim_paths                 <- dt_sim_paths
  # p_dt_annual_growth_rate        <- dt_port_weights[,.(symbol, expected_annual_growth_rate)]
  # p_rsk_free_rate                <- 0.035 
  # p_rsk_conf_level               <- 0.975
  # p_asset_weight_max_lower_limit <- NULL
  # p_asset_weight_min_upper_limit <- NULL
  # p_cardinality_lower            <- 20
  # p_cardinality_upper            <- 33
  # p_horizon_years                <- 1
  
  #### DEFINE The Portfolio ####
  instr_universe <- unique(p_dt_sim_paths$symbol)
  
  #### Conditions ####
  target_sims       <- 20000
  init_port_sims    <- 10000
  set_horizon_days  <- p_horizon_years*252
  
  if(is.null(p_cardinality_lower) || p_cardinality_lower < 1){
    p_cardinality_lower <- length(instr_universe)
  }
  if(is.null(p_cardinality_upper) || p_cardinality_upper < 1){
    p_cardinality_upper <- length(instr_universe)
  }
  if(p_cardinality_upper < p_cardinality_lower){
    p_cardinality_upper <- p_cardinality_lower
  }
  
  #******************************************************************************
  #*
  #### OPTIMAL PORTFOLIO WEIGHTS ####
  #*
  #******************************************************************************
  #### DEFINE EXPECTED PERFORMANCE FOR EACH INSTRUMENT ####
  # User defined
  dt_perf            <- p_dt_sim_paths[path_scenario == set_horizon_days, list(average = mean(cum_delta_change-1)), by=c("symbol", "path_scenario")]
  dt_perf            <- dt_perf[,.(symbol, average)]
  setnames(dt_perf, "symbol", "ticker")
  setnames(dt_perf, "average", "performance")
  
  #### Minimum expected performance ####
  perf_expectation <- dcast.data.table(dt_perf, 
                                       . ~ ticker, 
                                       value.var = "performance")[, -1]
  perf_expectation <- as.matrix(perf_expectation)
  use_perf_min     <- p_rsk_free_rate
  
  #### IMPORT SIMULATED DELTA VECTORS ####
  #sim_pnl <- get_simulated_pnl(dt_prices, date_of_analysis, instr_universe, base_cur, risk_horizon, volatility_scenario)
  sim_pnl <- copy(p_dt_sim_paths)[path_scenario == set_horizon_days, .(symbol, cum_delta_change, sim)]
  sim_pnl[, cum_delta_change := cum_delta_change-1]
  setnames(sim_pnl, "sim", "scenario")
  setnames(sim_pnl, "cum_delta_change", "pnl_pct")
  sim_pnl <- dcast.data.table(sim_pnl, scenario ~ symbol, value.var = "pnl_pct")
  sim_pnl <- sim_pnl[, -1]
  
  #### INDIVIDUAL RISK ####
  dt_risk_perf <- calc_individual_risk(sim_pnl          = sim_pnl,
                                       dt_perf          = dt_perf,
                                       confidence_level = p_rsk_conf_level)
  
  dt_instruments <- copy(dt_port_weights[,.("ticker" = symbol, "group" = location)])
  dt_instruments <- merge(dt_instruments, 
                          dt_risk_perf, 
                          by = "ticker", 
                          all.x = TRUE)
  
  #### GENERATE WEIGHTS that adhere to the condition set out ####
  message("Generating Portfolio Weights")
  dt_weigtht_red <- generate_target_sims(target_sim_n       = target_sims,
                                         cardinality_lower  = p_cardinality_lower, 
                                         cardinality_upper  = p_cardinality_upper, 
                                         perf_expectation   = perf_expectation,
                                         perf_min           = use_perf_min,
                                         weight_lower_limit = p_asset_weight_max_lower_limit,
                                         weight_upper_limit = p_asset_weight_min_upper_limit,
                                         max_iter           = 105,
                                         max_sims_pr_iter   = init_port_sims)
  
  #### Calculate expected shortfall (parallelization can greatly speed up the calculations) ####
  message("Calculating Expected Shortfall for the simulated portfolio weights")
  dt_expected_shortfall <- risk_calc_parallization(
    dt_weigtht_red,
    sim_pnl,
    p_rsk_conf_level)
  
  #### Combine risk & performance ####
  dt_es_comb <- comb_risk_perf(dt_expected_shortfall, dt_perf)
  dt_slp     <- slope_analysis(tickers = instr_universe, dt_es_comb)
  dt_slp[, Contribution := paste0(PortfolioEffect, " (", round(slope, 1), ")")]
  dt_instruments <- merge(dt_instruments, 
                          dt_slp[,.(ticker, PortfolioEffect, Contribution, slope)], 
                          by = "ticker", 
                          all.x = TRUE)
  
  # Unique Portfolio calculation ID's where the risk impact > 0.25
  # rsk_imp_ids <- unique(dt_es_comb[expected_shortfall_Weight > 0.25]$exec_id)
  # dt_es_comb <- dt_es_comb[!exec_id %in% rsk_imp_ids]
  
  #### FIND THE OPTIMAL PORTFOLIO ####
  dt_rsk <- dt_es_comb[ , .(ES = sum(expected_shortfall), PERF = sum(performance)), by = exec_id]
  
  use_rsk_free_rate     <- (1+p_rsk_free_rate)^p_horizon_years-1
  dt_efficient_frontier <- efficient_frontier(dt_rsk, use_rsk_free_rate)
  
  #### OPTIMAL PORTFOLIO ####
  optimal_scenarie <- dt_efficient_frontier[which.max(tangent), max_perf_scenario]
  optimal_exec_id <- dt_rsk[optimal_scenarie, exec_id]
  
  dt_instruments <- merge(dt_instruments, 
                          dt_es_comb[exec_id == optimal_exec_id, .(ticker, weight, expected_shortfall_Weight)], 
                          by = "ticker", 
                          all.x = TRUE)
  
  market_portfolio <- dt_efficient_frontier[max_perf_scenario  == optimal_scenarie]
  return(list(
    "HorizonYears"      = p_horizon_years,
    "RiskFreeRate"      = use_rsk_free_rate,
    "MarketPortfolio"   = market_portfolio,
    "EfficientFrontier" = dt_efficient_frontier,
    "OptimalWeights"    = dt_instruments,
    "TangentLine"       = paste0("E(perf) = ", round(market_portfolio[["tangent"]], 2)," * CVaR + ", round(use_rsk_free_rate, 4))))
}

#### Tables and Figures ####
#### Optimal Portfolio Weights ####
dt_4_point_plot <- rbindlist(lst_out)
#check that portfolio effect is consistant over the 5 runs
#dt_4_point_plot_splt <- split(dt_4_point_plot, dt_4_point_plot$ticker)

dt_4_risk_plot <- dt_4_point_plot[tangent == max(tangent)]
setorder(dt_4_risk_plot, "PortfolioEffect", "expected_shortfall_Weight")
dt_4_risk_plot$ticker <- factor(dt_4_risk_plot$ticker, levels = dt_4_risk_plot$ticker[order(dt_4_risk_plot$PortfolioEffect, dt_4_risk_plot$expected_shortfall_Weight)])
dt_4_risk_plot_cast <- melt.data.table(dt_4_risk_plot, id.vars = c("ticker", "PortfolioEffect", "tangent"))
dt_4_risk_plot_cast$variable <- factor(dt_4_risk_plot_cast$variable, 
                                       levels = levels(dt_4_risk_plot_cast$variable),
                                       labels = c("Portfolio", "RiskImpact"))
dt_4_risk_plot_cast_segment <- copy(dt_4_risk_plot_cast)
dt_4_risk_plot_cast_segment <- dt_4_risk_plot_cast_segment[,.(ticker, PortfolioEffect, variable, value)]
dt_4_risk_plot_cast_max <- dt_4_risk_plot_cast_segment[, .("max" = max(value)), by = c("ticker", "PortfolioEffect")]
dt_4_risk_plot_cast_min <- dt_4_risk_plot_cast_segment[, .("min" = min(value)), by = c("ticker", "PortfolioEffect")]
dt_4_risk_plot_segment <- merge(
  dt_4_risk_plot_cast_min,
  dt_4_risk_plot_cast_max,
  by = c("ticker", "PortfolioEffect"))

dt_4_risk_plot_cast <- merge(dt_4_risk_plot_cast,
                             dt_4_risk_plot_segment,
                             by = c("ticker", "PortfolioEffect"),
                             all.x = TRUE)

plot_risk_weight <- ggplot(dt_4_risk_plot_cast, aes(x = value, y = ticker, colour = variable, shape = variable)) +
  geom_point()+
  geom_segment(aes(yend = ticker,
                   x    = min,
                   xend = max),
               colour = "grey50", linetype = "dashed") +
  facet_grid(PortfolioEffect ~ ., scales='free_y', space = "free_y")+
  theme_bw()+
  theme(legend.position="top")+
  xlab("Weight")+
  ylab("Symbol")+
  scale_colour_brewer("Weight: ", palette = "Set1")+
  scale_shape("Weight: ")

grid::grid.newpage()
plot_risk_weight 
grid::grid.raster(logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inch'))

#### Plot weight-riskimpact by sub portfolio ####

#### Different Weights for 5 different (close to) Optimal Portfolios ####
dt_4_point_plot[, max_diff := max(weight) - min(weight), by = "ticker"]
setorder(dt_4_point_plot, "tangent", "PortfolioEffect", -"max_diff")
dt_4_point_plot$ticker <- factor(dt_4_point_plot$ticker, 
                                 levels = unique(
                                   dt_4_point_plot$ticker[
                                     order(dt_4_point_plot$PortfolioEffect, 
                                           dt_4_point_plot$max_diff)]))
plot_weight <- ggplot(dt_4_point_plot, aes(x = weight, y = ticker, colour = as.factor(round(tangent, 3)), shape = as.factor(round(tangent, 3)))) +
  geom_point()+
  facet_grid(PortfolioEffect ~ ., scales='free_y', space = "free_y")+
  theme_bw()+
  theme(legend.position="top")+
  scale_colour_brewer("Slope:", palette = "Set1")+
  scale_shape("Slope:")+
  xlab("Weight")+
  ylab("Symbol")

grid::grid.newpage()
plot_weight 
grid::grid.raster(logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inch'))

#write.excel(dt_instruments, dec = ",")

#### Mean-CVaR Optimal Portfolio Plot ####
optim_port_index <- pbapply::pblapply(1:5, function(x){
  lst_mean_cvar_analysises[[x]][["MarketPortfolio"]][["tangent"]]  
})
lst_mean_cvar_analysis <- lst_mean_cvar_analysises[[which.max(optim_port_index)]]
#lst_mean_cvar_analysis <- lst_mean_cvar_analysises[[which.min(optim_port_index)]]

p_ef_frontier         <- plot_efficient_frontier(lst_mean_cvar_analysis, input_port_perf_risk)
grid::grid.newpage()
p_ef_frontier 
grid::grid.raster(logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inch'))
