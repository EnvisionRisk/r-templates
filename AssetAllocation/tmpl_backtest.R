#******************************************************************************
#*
#### BACKTEST THE OPTIMAL PORTFOLIO 2018-2023
#*
#******************************************************************************
#// Run Backtest Flow 2018 - 2023 (2022 and forward are out-of-time predictions)

#Define confidence level - for the risk calculatens in step 2
rsk_setting_confidence_level <- rsk_conf_level

# Specification of the portfolio weights, initial investement and 
# base currency. These 3 elements are used in step 1 to construct 
# the daily portfolio quantities used to calculate the performance 
# and risk prediction. The portfolio in this example invest 
# 100 mill. DKK on the 5th of January 2018 and it is rebalance 
# every year at the end of year.  
port_init_exposure_amount <- 1000000
port_base_cur             <- base_cur

#### Table of Weights ####
# Portfolio and sub-portfolio structures are defined using the 
# 'location' field, with '//' designating a sub-portfolio level. 
# This flexible system allows for unlimited levels, enabling 
# intricate specifications such as 'Equity//Index//EU//Energy//...' 
# by repeating '//' as needed. The portfolio structure plays a 
# crucial role in risk and performance analytics, enabling the 
# calculation of risk and performance metrics across various 
# layers of the portfolio. 
init_date <- as.Date("2018-01-05")
rebalance_dates <- as.Date(paste0(2019:2024, "-01-01"))
portfolio_action_dates <- c(init_date, rebalance_dates)

lst_port_weight <- lapply(portfolio_action_dates, function(x){
  dt_port_weights_tmp <- copy(dt_port_optimal_weights)
  dt_port_weights_tmp[, date := as.Date(x)]
  dt_port_weights_tmp
})
dt_port_weight_4_backtesting <- rbindlist(lst_port_weight)

#### *** User input End *** ####

#### Validate that the Portfolio weights sum to 1 for each date ####
dt_port_weight_4_validation <- dt_port_weight_4_backtesting[, .("sum_weight" = sum(weight)), by=list(date)]
dt_port_weight_4_validation[, sum_weight := round(sum_weight, 2)]
stopifnot(all(dt_port_weight_4_validation$sum_weight == 1))

#### Step One - Portfolio Construction ####
#******************************************************************************
#*
#### PORTFOLIO CONSTRUCTION - from portfolio weights to (daily) portfolio quantities ####
#*
#******************************************************************************
#* Calculated from the designated weights of each portfolio position, the 
#* portfolio position quantities are vital for assessing risk and performance. 
#* The EnvisionRisk API simplifies this process, ensuring that these essential 
#* quantities are readily available for in-depth risk and performance analyses.

#### Extract the Portfolio Structure based upon the above portfolio specification ('location') ####
dt_port_structure <- copy(dt_port_weight_4_backtesting[, .(date, symbol, position_type, location)])

#### Call the API to do the heavy lifting ####
# Transform the portfolio weights to daily portfolio quantities.
dt_obj_quantities <- EnvisionRiskRaaS::envrsk_workflow_weight_2_quantities(
  dt_snapshot_weight     = dt_port_weight_4_backtesting,
  init_port_market_value = port_init_exposure_amount,
  is_wide                = FALSE,
  base_cur               = port_base_cur,
  to_date                = Sys.Date())

#### Attach the portfolio structure to the portfolio quantities ####
# A helper function to select the relevant portfolio structure 
# based on a specific date.
fetch_port_structure <- function(iter_date){
  dt_tmp    <- copy(dt_port_structure[date <= iter_date])
  return(dt_tmp[date == max(date)])
}

# List of dates for iteration
iter_dates <- unique(dt_obj_quantities[["Output"]][["PortfolioQuantites"]][["date"]])

# Loop through each date in the 'iter_dates' array, selecting the 
# corresponding portfolio structure for every individual date.
lst_port_structure <- lapply(iter_dates, function(x){
  dt_port_struc_tmp <- fetch_port_structure(x)
  dt_port_struc_tmp[, date := as.Date(x)]
})

# Collapse the result into one table
dt_expanded_port_structure <- rbindlist(lst_port_structure)

# Merge the portfolio structure to the portforlio quantities
dt_obj_quantities[["Output"]][["PortfolioQuantites"]][, date := as.Date(date)]
dt_obj_quantities[["Output"]][["PortfolioQuantites"]] <- merge(
  dt_obj_quantities[["Output"]][["PortfolioQuantites"]],
  dt_expanded_port_structure,
  by = c("symbol", "date"),
  all.x = TRUE)

#### Extract the portfolio positions (to be used for the risk calculations later in the script) ####
dt_port_quantities <- dt_obj_quantities[["Output"]][["PortfolioQuantites"]]
setkey(dt_port_quantities, symbol, date)
setorder(dt_port_quantities, "date", "symbol")

# The Portfolio Positions
dt_port_positions <- copy(dt_port_quantities[,.(symbol, date, position_type, quantity, location)])

#### Add a unique ID to each position so we can merge the performance and risk-metrics consistently ####
dt_port_positions <- EnvisionRiskRaaS::envrsk_decorate_portfolio_with_uid(dt_port_positions)
setnames(dt_port_positions, "POSITION_ID", "position_id")

#### Step Two - Performance & Risk Calculations ####
#******************************************************************************
#*
#### PORTFOLIO PERFORMANCE & RISK CALCULATIONS ####
#*
#******************************************************************************
#### Calculate risk and performance metrics for each date in iter_port_dates ####
# Dates to iterate over in portfolio dataset
iter_port_dates <- unique(dt_port_positions$date)

message(paste0("Iterate over each date in dates: ", Sys.time()))
#### Call the API to do the heavy lifting for each date in dates ####
# Enhancing performance substantially is achievable by parallelizing 
# this process. For a practical example, we invite you to contact 
# EnvisionRisk.

simulations_max   <- length(iter_port_dates)
simulations_descr <- "dates"

message("Setting up the cluster")
n_cores <- 5 #parallel::detectCores()-1

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

obj_risk_perf <- foreach::foreach(i = 1:simulations_max, .packages = c("data.table", "EnvisionRiskRaaS"), .options.snow = opts) %dopar% {
  #obj_risk_perf <- pbapply::pblapply(iter_port_dates, function(x){
  #i <- 1
  x <- as.Date(iter_port_dates[i])
  dt_port_quantities_date <- dt_port_positions[date == x]
  
  # Call the API to get the portfolio performance metrics (hypothetical PnL) for 
  # each date of the iterations. Profit and Loss (P/L) in financial instruments 
  # stem from the dynamic nature of market prices. Two critical types are actual 
  # P/L and hypothetical P/L.
  #
  # - Actual P/L: This measure represents the real-world gains or losses 
  #   incurred due to market value changes in a position or portfolio. It's 
  #   calculated by comparing the closing values of a business day with those of 
  #   the subsequent day, taking into account any intraday trades conducted. It 
  #   reflects the actual financial impact of your trading decisions and market 
  #   movements.
  #
  # - Hypothetical P/L: Contrarily, hypothetical P/L estimates the potential 
  #   profit or loss if a portfolio or position remains static for one business 
  #   day, with no intraday trades included. This figure is calculated within the 
  #   model framework and is used to gauge the performance of your positions 
  #   under the day's market conditions. It's an invaluable tool for evaluating 
  #   portfolio strategy and potential risk exposure.
  
  # In EnvisionRisk, hypothetical performance is projected forward to align with 
  # risk predictions like Value-at-Risk (VaR) and Expected-Shortfall (ES). 
  # Specifically, the hypothetical performance for a given date, 't0', is 
  # calculated as: 
  # 	PnL_t0 := PortfolioValue_t1 - PortfolioValue_t0. 
  # This means that the hypothetical performance for any date 'tn' isn't accessible 
  # in the system until the end of the following day, 'tn+1', though risk predictions 
  # for 'tn' are available as of that date.
  obj_performance <- EnvisionRiskRaaS::envrsk_portfolio_hypothetical_performance(
    date      = x,
    positions = dt_port_quantities_date,
    base_cur  = port_base_cur)
  dt_static      <- obj_performance[["Positions_Mapped"]][, .(location, symbol, name)]
  setnames(dt_static, "location", "Location")
  dt_performance <- obj_performance[["Output"]][, .(Location, Notational, MarketValue, HypotheticalPnL)]
  
  # Value-at-Risk and Expected Shortfall is calculated for the portfolio. For the 
  # purpose of backtests the 'risk horizon' and 'volatility scenario" is pre-set 
  # respectivly to a 1 day horizon and 'point_in_time' volatility scenario.
  
  # The endpoint returns two risk estimates: Value-at-Risk (VaR) and 
  # Expected-Shortfall (ES). Except for the overarching portfolio level, these 
  # risk estimates are presented unconditionally - or regular - measuring the 
  # stand-alone risk of each portfolio levels.
  
  # The assessment of risk can take two approaches: conditional risk (component risk) 
  # and unconditional risk (regular risk). In the context of a multi-position portfolio, 
  # the conditional risk refers to the contribution of each individual position to 
  # the overall portfolio risk. It's the aggregation of these individual risks that 
  # constitute the portfolio's total risk. 
  
  # This is fundamentally different from unconditional risk, where each position's 
  # risk s assessed independently, with no consideration of its impact on the 
  # overall portfolio risk. It should be noted that conditional and unconditional 
  # risk are equivalent at the portfolio's top level.  
  obj_risk <- EnvisionRiskRaaS::envrsk_portfolio_risk_regular(
    date          = x,
    positions     = dt_port_quantities_date,
    base_cur      = port_base_cur,
    horizon       = 1,
    signif_level  = rsk_setting_confidence_level,
    volatility_id = "point_in_time")
  dt_risk <- obj_risk[["Output"]][, .(Location, PortfolioTreeDepthLevel, PortfolioType, VaR, ES)]
  
  # Un-mapped symbols (they dont exist in EnvisionRisk database)
  dt_unmmapped <- obj_risk[["Positions_UnMapped"]]
  
  # Collect the performance and risk predictions into one table
  dt_output <- merge(
    dt_performance,
    dt_risk,
    by = "Location",
    all.x = TRUE)
  
  # Add static information for each symbol to the performance- and risk predictions
  dt_output <- merge(
    dt_output,
    dt_static,
    by = "Location",
    all.x = TRUE)
  
  # Static information is only available for the symbols and not for the 
  # portfolio- and sub-portfolio levels. Adjust the row-values for the 
  # columns 'symbol' and 'name' for these levels (called 'book' in 
  # EnvisionRisk terminology).
  dt_output[is.na(symbol), ":=" (symbol = Location, name = Location)]
  
  # Add the date to the output table
  dt_output[, date := x]
  
  # return the output-tables - performance & risk and un-mapped positions (if any).
  list("Output"   = dt_output,
       "Unmapped" = dt_unmmapped)
}
# Terminate the progressbar
pb$terminate()

# Stop the cluster
snow::stopCluster(cl) 
#closeAllConnections()


# Aggregate the output-tables for each date into one large table
lst_risk_perf <- lapply(obj_risk_perf, function(tbl){
  tbl[["Output"]] 
})
dt_risk_perf <- data.table::rbindlist(lst_risk_perf)

# Aggregate the un-mapped symbols for each date into one large table
lst_unmapped <- lapply(obj_risk_perf, function(tbl){
  tbl[["Unmapped"]] 
})
dt_unmapped <- data.table::rbindlist(lst_unmapped)

# If there are any un-mapped symbols - print a warning and print the un-mapped table!
if(nrow(dt_unmapped) > 0){
  count_symbols <- nrow(unique(dt_unmapped$symbol))
  warning(paste0("There are ", count_symbols, " positions with un-mapped symbols. Those positions are excluded from the performance and risk predictions"))
  dt_unmapped
}

#### Step Three - Backtest ####
#******************************************************************************
#*
#### PERFORM THE BACKTEST ####
#*
#******************************************************************************
dt_risk_perf[, group := symbol]
dt_risk_perf[PortfolioType == "position", group := paste0(sapply(Location, do_remove_last, splitter = "//"), "//", symbol)]

# In this process, we conduct a comprehensive backtest for the entire portfolio. 
# The evaluation of overall performance and risk is tailored based on the 
# 'Location' attribute. Specifically, we utilize the condition 
# dt_risk_perf[Location == "EGO"], where 'EGO' refers to the name of the 
# top-level portfolio to define the specific segment of the portfolio for 
# which the backtest is conducted. 
#
# The backtest dataset incorporates key columns from the position table, 
# including a renaming of the 'HypotheticalPnL' column to 'pnl'. This 
# modification aligns with the backtest workflow's requirement for the dataset 
# to contain exactly four named columns {date, VaR, ES, pnl}. For more infomation
# and interpretation of the output see https://envisionrisk.stoplight.io/docs/api-aleadomus-documentation/lexas553hryhq-backtesting
backtest_all_portfolio <- dt_risk_perf[group == "EGO", .(date, VaR, ES, "pnl" = HypotheticalPnL)]

backtest_all_portfolio[between(date, as.Date("2022-01-01"), as.Date("2022-12-31")) & pnl < VaR]
dt_4_graph <- melt.data.table(backtest_all_portfolio, id.vars = "date")

#### MAIN GRAPH ####
envrsk_backtest_dashboard_ts <- function(dt_backtest, base_cur){
  color_classic_trustworthy <- c("#CA3542", "#27647B", "#849FAD", "#AEC0C9", "#57575F")
  
  dt_out_graph <- data.table::copy(dt_backtest)
  dt_out_graph[, IS_VaR_EVENT := pnl < VaR]
  dt_out_graph[IS_VaR_EVENT != TRUE, VaR_EVENT := NA_real_]
  dt_out_graph[IS_VaR_EVENT == TRUE, VaR_EVENT := pnl]
  dt_out_graph[, IS_VaR_EVENT := NULL]
  dt_out_graph[, date := as.Date(date)]
  
  dt_out_melt <- melt.data.table(dt_out_graph, id.vars = "date")
  dt_out_melt[variable == "VaR", variable := "Value-at-Risk"]
  dt_out_melt[variable == "ES", variable := "Expected-Shortfall"]
  dt_out_melt[variable == "pnl", variable := "Profit/Loss"]
  p_backtest_ts <- ggplot(dt_out_melt[variable %in% c("Value-at-Risk", "Expected-Shortfall", "Profit/Loss")], aes(x = date, y = value, colour = variable))+
    geom_line()+
    geom_point(data = dt_out_melt[variable %in% c("VaR_EVENT")], aes(x = date, y = value), colour = color_classic_trustworthy[1])+
    ggthemes::scale_color_economist("")+
    theme_bw()+
    theme(legend.position="top")+
    scale_y_continuous(name=paste0("Profit/Loss (in ", base_cur, ")"), labels = scales::comma_format(big.mark = ".",
                                                                                                     decimal.mark = ","))
  return(p_backtest_ts)
}
envrsk_backtest_dashboard_ts(backtest_all_portfolio, "DKK")

#backtest_all_portfolio <- backtest_all_portfolio[!between(date, as.Date("2020-02-20"), as.Date("2020-03-10"))]
obj_backtest <- EnvisionRiskRaaS::envrsk_workflow_backtest(
  backtestdata = backtest_all_portfolio, 
  base_cur     = port_base_cur,
  signif_level = rsk_setting_confidence_level)

#### Sub-portfolio Backtests ####
# We can use the same template to perform backtesting for each of the 'book'-levels 
# in the portfolio. 
#iter_over_port_books <- unique(dt_risk_perf[PortfolioType == "book" & Notational > 0 & PortfolioTreeDepthLevel <= 2]$symbol)
iter_over_ports <- unique(dt_risk_perf$group)

obj_sub_backtest <- lapply(iter_over_ports, function(x){
  #x <- iter_over_port_books[3]
  #backtest_specific_portfolio <- dt_risk_perf[group == x & !between(date, as.Date("2020-02-20"), as.Date("2020-03-10")), .(date, VaR, ES, "pnl" = HypotheticalPnL)]
  backtest_specific_portfolio <- dt_risk_perf[group == x, .(date, VaR, ES, "pnl" = HypotheticalPnL)]
  obj_backtest <- EnvisionRiskRaaS::envrsk_workflow_backtest(
    backtestdata = backtest_specific_portfolio, 
    base_cur     = port_base_cur,
    signif_level = rsk_setting_confidence_level)
  dt_backtest <- obj_backtest[["Output"]]
  dt_backtest[, group := x]
  dt_backtest
})

# Show the results
dt_sub_backtest <- rbindlist(obj_sub_backtest)
setnames(dt_sub_backtest, "#VaR-Events", "var_events")
setnames(dt_sub_backtest, "#Obs", "obs")
setnames(dt_sub_backtest, "Value-at-Risk-p-value", "var_p_value")
setnames(dt_sub_backtest, "Expected-Shortfall-p-value", "es_p_value")
dt_sub_backtest <- dt_sub_backtest[Year == "", .(group, obs, var_events, var_p_value, es_p_value)]

#' The summary of the backtest is created based on the daily input-values 
#' for Profit/Loss, Value-at-Risk and Expected-Shortfall. The p-value can 
#' be interpretated as a likelihood of observing an even worse outcome 
#' than observed (i.e. more frequent loss-events for Value-at-Risk and 
#' more severe losses for Expected-Shortfall) under the assumption that the 
#' model is accurate (the null hypothesis). In case where the p-value is low 
#' this is used as evidence against the hypothesis and hence conclude that 
#' the model show signs of not being accurate.

#### ####
max_levels_included <- 3 #2

# Add top/bottom lines to all levels < max_levels_included and make the rows bold
dt_tbl_backtest <- copy(dt_sub_backtest)
splt_str <- stringr::str_split(dt_tbl_backtest$group, "//")
level_backtest_tmp <- lapply(splt_str, function(x){
  (length(x) - 1)
})
level_backtest <- do.call(c, level_backtest_tmp)
dt_tbl_backtest <- cbind(dt_tbl_backtest, level_backtest)

# Format 
p_value_factor <- factor(c("p-value >= 0.1",
                           "p-value < 0.1",
                           "p-value < 0.05",
                           "p-value < 0.02",
                           "p-value < 0.01",
                           "p-value < 0.005",
                           "p-value < 0.001"), ordered = TRUE)
dt_p_value_colour_schema <- data.table(
  "p_value"   = rev(p_value_factor),
  "hex_value" = c("red3", "#FF0000", "#FF5500", "#FFAA00", "#FFFF00", "#FFFF80", "#378805"))
p_value_col_schema <- function(p_vf){
  return(dt_p_value_colour_schema[p_value == p_vf][["hex_value"]])
}
dt_tbl_backtest[, var_colour := sapply(var_p_value, p_value_col_schema, simplify = TRUE)]
dt_tbl_backtest[, es_colour := sapply(es_p_value, p_value_col_schema, simplify = TRUE)]

dt_tbl_backtest[, port_name_short := as.character(sapply(group, get_last_element, splitter = "//", simplify = TRUE))]
chr_backtest_names <- ifelse(level_backtest > 0, paste0("$\\hookrightarrow$ ", dt_tbl_backtest$port_name_short), dt_tbl_backtest$port_name_short)
dt_tbl_backtest[, port_name_short_formated := chr_backtest_names]

dt_backtest_4_print     <- dt_tbl_backtest[level_backtest < max_levels_included]
dt_backtest_4_print_red <- dt_backtest_4_print[,.("Portfolio"     = port_name_short_formated,
                                                  "#Obs"          = scales::number(
                                                    obs,
                                                    accuracy = 1,
                                                    big.mark = ","),
                                                  "#Events"            = var_events,    
                                                  "Value-at-Risk"      = var_p_value,
                                                  "Expected-Shortfall" = es_p_value)]

kw0 <- paste0("p-value", footnote_marker_symbol(1, double_escape = T))
names(dt_backtest_4_print_red)[4] <- paste0(names(dt_backtest_4_print_red)[4], footnote_marker_symbol(2))
names(dt_backtest_4_print_red)[5] <- paste0(names(dt_backtest_4_print_red)[5], footnote_marker_symbol(2))

kableExtra::kbl(
  caption = paste0("Backtest (2018-2024)"),
  dt_backtest_4_print_red, 
  booktabs = T,
  format = "html",
  longtable = TRUE,
  align=c('l', rep('r', times=4)),
  escape = FALSE) %>%
  #column_spec(1, width = "10cm") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_indent(positions = which(dt_backtest_4_print$level_backtest == 1), level_of_indent = 1) %>%
  add_indent(positions = which(dt_backtest_4_print$level_backtest == 2), level_of_indent = 2, ) %>%
  add_header_above(c(" " = 3, setNames(2, kw0)), 
                   color = "black", 
                   background = "white", 
                   font_size = 10,
                   line = TRUE,
                   bold = TRUE,
                   escape = FALSE) %>%
  collapse_rows(columns = c(4,5), valign = 'middle') %>%
  row_spec(row = 0, 
           color = "black", 
           background = "white", 
           bold = TRUE, 
           font_size = 10,
           extra_css = "border-bottom: 2px solid;") %>%
  row_spec(row = 1, color = "black", bold = TRUE, extra_css = "border-bottom: 1px solid; border-top: 1px solid") %>%
  row_spec(row = which(dt_backtest_4_print$level_backtest == 1), color = "black", bold = TRUE, extra_css = "border-bottom: 1px solid; border-top: 1px solid") %>%
  footnote(general = "Reference: Carlo Acerbi & Balazs Szekely (2014) BACKTESTING EXPECTED SHORTFALL, MSCI. The unconditional test statistic (test 2) is used here",
           symbol = c("The p-value tells you how likely it is to observe a worse outcome under the null hypothesis of an accurate model.",                       
                      "The risk measures are based on a 97.5% confidence level and one day horizon with pointt-in-time volatility scenario")) %>% 
  column_spec(4, color = "black", background = dt_backtest_4_print$var_colour) %>% 
  column_spec(5, color = "black", background = dt_backtest_4_print$es_colour) %>% 
  kable_styling(full_width = F, font_size = 10, latex_options = c("HOLD_position"))

#' The interpretation of 'p-value < 10%' is that the p-value is higher
#' than 5% but lower than 10%. The same rationale for the rest of the p-values.
#' We dont calculate an exact p-value, but only calculte it for certain cutoff 
#' values.
envrsk_backtest_dashboard_p_value_interpretation_table <- function(){
  colour_tmplt <- c("red3", "#FF0000", "#FF5500", "#FFAA00", "#FFFF00", "#FFFF80", "#378805")
  p_value_factor <- factor(c("p-value >= 0.1",
                             "p-value < 0.1",
                             "p-value < 0.05",
                             "p-value < 0.02",
                             "p-value < 0.01",
                             "p-value < 0.005",
                             "p-value < 0.001"), ordered = TRUE)
  
  dt_p_value_factor <- data.table("Interpretation" = p_value_factor)
  
  return(
    dt_p_value_factor %>%
      gt()  %>%
      tab_options(
        heading.subtitle.font.size = 12,
        heading.align = "left",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width= px(3),
        row_group.background.color = "grey",
        table.background.color = "#d5e4eb"
      ) %>%
      tab_header(
        title = md("**Colour Schema**"),
        subtitle = html("<em>p-value</em>")
      ) %>%
      data_color( # Update cell colors...
        columns = c("Interpretation"),
        colors = scales::col_factor(
          domain = p_value_factor,
          palette = colour_tmplt,
          na.color = "#808080")
      ) %>%
      tab_source_note(
        source_note = md("Reference: Basel Committee on Banking Supervision (2019)
                         *Minimum capital requirements for market risk*,
                         'MAR99 Guidance on use of the internal models approach', table 2 (p. 128).
                         The colour schema is an **EnvisionRisk** adoptation
                         of the chosen cutoff values in the BIS paper.")
      )
  )
}
envrsk_backtest_dashboard_p_value_interpretation_table()


obj_sub_performance <- lapply(iter_over_port_books, function(x){
  #x <- iter_over_port_books[2]
  backtest_specific_portfolio <- dt_risk_perf[Location == x, .(date, PortfolioTreeDepthLevel, PortfolioType, MarketValue, HypotheticalPnL)]
  mv_0 <- backtest_specific_portfolio[1][["MarketValue"]]
  backtest_specific_portfolio[, cumsum := cumsum(HypotheticalPnL)]
  backtest_specific_portfolio[, Location := x]
  backtest_specific_portfolio[, cumsum := cumsum / mv_0]
  backtest_specific_portfolio
})
dt_4_perf_graph <- rbindlist(obj_sub_performance)
dt_4_perf_graph[,":=" (MarketValue = NULL, HypotheticalPnL = NULL)]
dt_4_perf_graph_melt <- melt.data.table(dt_4_perf_graph, id.vars = c("date", "PortfolioTreeDepthLevel", "PortfolioType", "Location"))
dt_4_perf_graph_melt$PortfolioTreeDepthLevel <- factor(dt_4_perf_graph_melt$PortfolioTreeDepthLevel, 
                                                       levels = unique(dt_4_perf_graph_melt$PortfolioTreeDepthLevel),
                                                       labels = c("Portfolio", "Sub-Portfolios"))
p_performance_backtest <- ggplot(dt_4_perf_graph_melt[,.(date, "performance" = value, Location, PortfolioTreeDepthLevel)], aes(x = as.Date(date), y = round(performance*100, 2), colour = Location))+
  geom_line()+
  theme_bw()+
  facet_grid(PortfolioTreeDepthLevel ~ .) +
  xlab("Date")+
  ylab("Accumulated Performance (%)")+
  theme(legend.position="bottom")+
  scale_colour_brewer("", palette = "Set1")+
  grid::grid.newpage()
p_performance_backtest 
grid::grid.raster(logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1, 'inch'))