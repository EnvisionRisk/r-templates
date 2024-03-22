#******************************************************************************
#*
#*
#### STRATEGIC ASSET ALLOCATION ####
#*
#*
#******************************************************************************

#******************************************************************************
#*
#### USER INPUT ####
#*
#******************************************************************************
# Symbols/tickers & weight 
# {symbol, weight, location, expected_annual_growth_rate}
dt_input_portfolio <- data.table(readRDS(url("https://www.dropbox.com/scl/fi/ays28w9vu3piwiifbxnqy/portfolio_by_asset_classes.rds?rlkey=g52i0roki5sln4eplv98w2lpm&raw=true","rb")))[,.(symbol, weight, location, expected_annual_growth_rate)]

# Reference date: 
ref_date <- as.Date("2024-01-31")

# Base Currency: 
base_cur <- "DKK"

#### Risk Settings ####
rsk_conf_level <- 0.975
rsk_free_rate  <- 0.035

# Volatility scenario (alternatives: use "point_in_time", "downturn", 
# "through_the_cycle", "severe_stress" or "extreme_stress"). In case 
# "point_in_time" scenario is used the reference date becomes important since 
# the volatility on the reference date is used. 
rsk_volatility_scenario <- "through_the_cycle"

#### Simulation Settings ####
sim_forward_years     <- 10 # How many years forward should we use?
sim_forward_base_year <- 1 #Which year to use for the optimal weight calculation?

# Set simulated paths (where each path-simulation is based on the steps chosen above)
sim_paths <- 10000

#### Set Date of Analysis ####
#date_of_analysis    <- as.Date("2024-01-10")

#### Conditions ####
# Asset Weight Constraints
constr_min_weight <- 0.003 
constr_max_weight <- 0.12

# Cardinality
constr_cardinality_lower <- 33
constr_cardinality_upper <- length(dt_positions$symbol)

# Risk Budgeting
constr_max_risk <- 0.1

#******************************************************************************
#*
#### Strategic Asset Allocation Report ####
#*
#******************************************************************************
# Gather price and delta vectors for the analysis and make the data ready for analysis
dt_positions <- copy(dt_input_portfolio)[,.(symbol, weight)]

dt_input_port <- merge(
  dt_input_portfolio, 
  dt_instrument_search[, .(symbol, name, instrument_type)],
  by = "symbol",
  all.x = TRUE)

dt_delta_vector <- sim_asset_one_day_pnl_distributions(
  dt_positions$symbol,
  rsk_volatility_scenario,
  base_cur,
  ref_date)

#******************************************************************************
#*
#### Convert the absolute price changes into percentage price changes ####
#*
#******************************************************************************
dt_all_prices <- asset_prices(dt_positions$symbol,
                              base_cur)
dt_prices     <- dt_all_prices[date == ref_date, .(symbol, date, close)]

dt_delta_vector_pct <- merge(dt_delta_vector,
                             dt_prices[, .(symbol, close)],
                             by    = "symbol",
                             all.x = TRUE)
dt_delta_vector_pct[, pnl_pct := pnl/close]
dt_delta_vector_pct[, ":=" (pnl = NULL, close = NULL)]

#### ####
dt_observed_perf_saa <- get_performance(
  dt_all_prices, 
  unique(dt_prices$symbol), 
  base_cur, 
  return_period = "Yearly",
  start_date = ref_date - 365*set_strategic_year_4_optimal_weights,
  end_date   = ref_date)
setnames(dt_observed_perf_saa, "ticker", "symbol")

dt_input_port <- merge(
  dt_input_port,
  dt_observed_perf_saa,
  by = "symbol",
  all.x = TRUE)

dt_input_port <- dt_input_port[, .(name, symbol, instrument_type, location, weight, 
                                   "observed_performance" = performance, 
                                   "expected_performance" = expected_annual_growth_rate)]

stopifnot(all(c("symbol", "scenario", "pnl_pct") %in% colnames(dt_delta_vector_pct)))
stopifnot(all(c("symbol", "expected_annual_growth_rate") %in% colnames(dt_port_weights)))
dt_sim_paths <- path_simulations(dt_delta_vector_pct[,.(symbol, scenario, pnl_pct)],
                                 dt_port_weights[,.(symbol, expected_annual_growth_rate)],
                                 set_sim_years,
                                 set_sim_paths)

#### Simulate Portfolio Weights ####

#### Mean-CVaR analysis - Find Optimal Portfolio Weights ####
# Do 5 Optimal Weights Simulations ####
lst_mean_cvar_analysises <- pbapply::pblapply(1:5, function(y){
  lst_mean_cvar_analysis <- find_optimal_portfolio_weights(
    dt_sim_paths,
    dt_input_portfolio[,.(symbol, expected_annual_growth_rate)],
    rsk_free_rate,
    rsk_conf_level,
    min_weight,
    max_weight,
    cardinality_lower, 
    cardinality_upper,
    1)
})

lst_out <- lapply(lst_mean_cvar_analysises, function(x){
  dt_out <- x[["OptimalWeights"]][, .(ticker, weight, PortfolioEffect, expected_shortfall_Weight)]
  tangent <- x[["MarketPortfolio"]][["tangent"]]
  dt_out[, tangent := tangent]
  dt_out
})

#### Backtest - Portfolio based Upon Optimal Weights ####

#### Risk Monitoring - Portfolio based Upon Optimal Weights ####


#### ####


#### USER INPUT END ####


#### ####



#******************************************************************************
#*
#### LONG TERM PERFORMANCE AND RISK ASSESMENT (1 - 10 years)
#*
#******************************************************************************
dt_sim_paths_portfolio <- merge(
  dt_sim_paths,
  dt_port_weights[,.(symbol, weight)], 
  by = "symbol",
  all.x = TRUE)
dt_sim_paths_portfolio <- portfolio_path(dt_sim_paths_portfolio)



#### ####
n_sim           <- 10000

rsk_free_r        <- 0.035 
max_risk_constr   <- 0.99
max_weight_constr <- 0.99
min_weight_constr <- 0.0

expected_perf     <- c(0.1, 0.05, 0.04, 0.01)

alpha_param     <- 0.15 

#### ####
w_contribution    <- c(2, 1.3, 0.7, -0.3)

fkt <- function(p_dirichlet_vec){
  #p_dirichlet_vec <- factor_loadings_no_constr
  my_matrix <- data.table(gtools::rdirichlet(n_sim, p_dirichlet_vec))
  lower_lim <- sort(apply(my_matrix, 1, min))[alpha_param * n_sim]
  upper_lim <- sort(apply(my_matrix, 1, max))[(1 - alpha_param) * n_sim]
  data.table(
    "lower" = round(lower_lim, 4), 
    "upper" = round(upper_lim, 4))
}

factor_loadings_no_rsk_budgeting <- expected_perf/(rsk_free_r*abs(w_contribution))
factor_loadings_no_constr        <- factor_loadings_no_rsk_budgeting^2
factor_loadings_rsk_budgeting    <- factor_loadings_no_constr * (1/abs(w_contribution))
factor_loadings_risk_parity <- sum(abs(w_contribution))/abs(w_contribution)
# risk_parity_portfolio_guess <- factor_loadings_risk_parity/sum(factor_loadings_risk_parity)
# risk_parity_portfolio_guess * w_contribution  

k <- 100
e1 <- rep(1/k, k)
x1 <- gtools::rdirichlet(10000, rep(1, k))
optimal.weights <- x1[which.min(apply((x1 - e1)^2, 1, "sum")), ]
hist(optimal.weights, 25)

#s_p_w <- gtools::rdirichlet(n_sim, factor_loadings_rsk_budgeting)
#s_p_w <- gtools::rdirichlet(n_sim, factor_loadings_no_constr)
s_p_w <- gtools::rdirichlet(n_sim, factor_loadings_risk_parity)

#### Maximum Weight Constr? ####
find_inf_max_weight <- function(p_dt, p_max_weight){
  out_bool_max_weight_constr <- p_dt <= p_max_weight
  dt_out <- as.data.table(cbind(p_dt, "V5" = apply(out_bool_max_weight_constr, 1, "all")))
  nrow(dt_out[V5 == 1])
}
my_seq <- seq(max_weight_constr, 1, (1-max_weight_constr)/50)
out_inf_max_weight <- sapply(my_seq, find_inf_max_weight, p_dt = s_p_w)
inf_max_weight <- max(max_weight_constr, my_seq[which(out_inf_max_weight >= 1000)[1]])

out_bool_weight_constr <- s_p_w < inf_max_weight
dt_aa <- as.data.table(cbind(s_p_w, apply(out_bool_weight_constr, 1, "all")))
nrow(dt_aa[V5 == 1])

par(mfrow=c(2, 2))
hist(dt_aa[V5 == 1, V1], 50)
hist(dt_aa[V5 == 1, V2], 50)
hist(dt_aa[V5 == 1, V3], 50)
hist(dt_aa[V5 == 1, V4], 50)
par(mfrow=c(1, 1))

#### Maximum Risk Constribution? ####
select_aa_index <- as.vector(dt_aa[, ncol(dt_aa), with = FALSE] == 1)
dt_bb <- copy(dt_aa[select_aa_index, 1:(ncol(dt_aa) - 1), with = FALSE])
dt_bb_splt <- split(dt_bb, 1:nrow(dt_bb))
lst_spw_contrb <- lapply(dt_bb_splt, function(x){
  x * abs(w_contribution)
})
dt_spw_contrb <- rbindlist(lst_spw_contrb)
out_bool_risk_constr <- dt_spw_contrb < max_weight_constr * 1.15
dt_cc <- as.data.table(cbind(dt_spw_contrb, "V5" = apply(out_bool_risk_constr, 1, "all")))
dt_dd <- dt_bb[which(dt_cc$V5 == 1), ]
nrow(dt_dd)

par(mfrow=c(2, 2))
hist(dt_dd[,V1], 50)
hist(dt_dd[,V2], 50)
hist(dt_dd[,V3], 50)
hist(dt_dd[,V4], 50)
par(mfrow=c(1, 1))

#### Minimum Weight? ####
find_sup_min_weight <- function(p_dt, p_min_weight){
  out_bool_min_weight_constr <- p_dt >= p_min_weight
  dt_out <- as.data.table(cbind(p_dt, "V5" = apply(out_bool_min_weight_constr, 1, "all")))
  nrow(dt_out[V5 == 1])
}
my_seq <- seq(min_weight_constr, 0, -min_weight_constr/50)
out_sup_min_weight <- sapply(my_seq, find_sup_min_weight, p_dt = dt_dd)
sup_min_weight <- min(min_weight_constr, my_seq[which(out_sup_min_weight >= 250)[1]])
  
out_bool_min_weight_constr <- dt_dd >= sup_min_weight
dt_ee <- as.data.table(cbind(dt_dd, "V5" = apply(out_bool_min_weight_constr, 1, "all")))
dt_ee[V5 == 1]

par(mfrow=c(2, 2))
hist(dt_ee[V5 == 1, V1], 50)
hist(dt_ee[V5 == 1, V2], 50)
hist(dt_ee[V5 == 1, V3], 50)
hist(dt_ee[V5 == 1, V4], 50)
par(mfrow=c(1, 1))


25000/nrow(dt_ee[V5 == 1])
25000/250
#******************************************************************************
#*
#### Create Table ####
#*
#******************************************************************************
lst_mean_cvar_analysis[["OptimalWeights"]][,-"Contribution"]

mean_cvar_tbl_format <- function(tbl_input){
  #tbl_input <- lst_mean_cvar_analysis[["OptimalWeights"]][,-"Contribution"]
  setnames(tbl_input, "ticker", "symbol")
  setnames(tbl_input, "PortfolioEffect", "portfolio risk effect")
  setnames(tbl_input, "expected_shortfall_Weight", "risk impact")
  setorder(tbl_input, "portfolio risk effect", -"risk impact")
  return(
    tbl_input %>%
      gt()  %>%
      fmt_percent(columns = c("performance", "risk", "weight", "risk impact"), decimals = 1) %>%
      tab_options(
        heading.subtitle.font.size = 12,
        heading.align = "left",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width= px(3),
        row_group.background.color = "grey",
        table.background.color = "#d5e4eb"
      ) 
  )
}
#### ####

#******************************************************************************
#*
#### LONG TERM PERFORMANCE AND RISK ASSESMENT (1 - 10 years)
#*
#******************************************************************************
# Optimal Weights
dt_port_optimal_weights <- merge(
  lst_mean_cvar_analysis[["OptimalWeights"]][,.("symbol" = ticker, "location" = group, weight)],
  dt_input_port[,.(symbol, "position_type" = instrument_type)],
  by = "symbol",
  all.x = TRUE)

dt_sim_paths_portfolio <- merge(
  dt_sim_paths,
  dt_port_optimal_weights[,.(symbol, weight)], #dt_port_optimal_weights
  by = "symbol",
  all.x = TRUE)
dt_sim_paths_portfolio <- portfolio_path(dt_sim_paths_portfolio)

# Combine the simulated values for the individual tickers with that of the portfolio
dt_sim_paths_ext <- rbind(
  dt_sim_paths,
  dt_sim_paths_portfolio)

# The result of the simulation based on the extracted time periods. The result 
# is unadjusted for trend. The trend in performance and risk reflects the 
# historical performance since 2005. 
dt_perf_risk_evo      <- copy(dt_sim_paths_portfolio)
dt_perf_risk_evo_splt <- split(dt_perf_risk_evo, dt_perf_risk_evo$path_scenario)
lst_risk_perf <- lapply(dt_perf_risk_evo_splt, function(tbl){
  value_at_risk      <- ValueAtRisk(tbl[["cum_delta_change"]], rsk_conf_level)
  expected_shortfall <- ExpectedShortfall(tbl[["cum_delta_change"]], rsk_conf_level)
  perf               <- mean(tbl[["cum_delta_change"]])
  dt_out             <- data.table("ValueAtRisk"       = value_at_risk,
                                   "ExpectedShortfall" = expected_shortfall,
                                   "PnL"               = perf)
  
  return(dt_out)
})
dt_risk_perf <- rbindlist(lst_risk_perf)
dt_risk_perf[, day_points := seq(252, set_sim_years*252, 252)]

# Transform days into years for the extraction points
dt_risk_perf_melt <- melt.data.table(dt_risk_perf, id.vars = "day_points")
y_lim            <- dt_risk_perf_melt[day_points == max(day_points) & variable == "PnL"]$value*1.1
dt_risk_perf_melt[, year := day_points/252]

# Average performance pr year
max_year            <- max(dt_risk_perf_melt$year)
yearly_avg_perf_tmp <- dt_risk_perf_melt[variable == "PnL" & year == max_year]$value
yearly_avg_perf     <- yearly_avg_perf_tmp^(1/max_year)

p_perf_risk <- ggplot(dt_risk_perf_melt, aes(x = year, y = value, colour = variable))+
  geom_line()+
  geom_point()+
  ylim(c(0, y_lim))+
  xlab("Year")+
  ylab("Expected Portfolio Performance (index t0 = 1)")+
  ggthemes::scale_colour_economist()+
  labs(title    = "Expected Performance & Risk",
       subtitle = paste0("Based on forward looking simulations (", max(dt_risk_perf_melt$year)," years)"))+
  theme_bw()+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  annotate("text", 
           x = 5,
           y = y_lim/1.1, 
           label= paste0("Average performance: ", 
                         round((yearly_avg_perf-1) * 100, 1), 
                         "% p.a."))

grid::grid.newpage()
p_perf_risk
grid::grid.raster(logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inch'))