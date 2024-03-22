get_asset_prices <- function(p_symbols,
                             p_base_cur){
  iter_delta_api_calls <- length(p_symbols) %/% 25 + 1
  dt_symbol_iter       <- data.table(
    "symbol" = p_symbols,
    "batch"  = sort(rep(1:iter_delta_api_calls, 25))[1:length(p_symbols)])
  #******************************************************************************
  #*
  #### PRICE (for the ref date) ####
  #*
  #******************************************************************************
  # The simulated price changes are calculated in the base currency. To convert 
  # these into percentage changes, the price on the reference date is required.
  lst_prices <- lapply(1:iter_delta_api_calls, function(y){
    selected_symbs <- dt_symbol_iter[batch == y]$symbol
    lst_prices <- EnvisionRiskRaaS::envrsk_market_price(
      symbols       = selected_symbs,
      base_cur      = p_base_cur)
    # Extract the prices for the reference date
    dt_prices <- lst_prices[["Output"]][,.(symbol, date, close, adj_close)]
  })
  dt_prices <- rbindlist(lst_prices)
  dt_prices[, date := as.Date(date)]
  return(dt_prices)
}

input_tbl_format <- function(tbl_input){
  #tbl_input <- dt_input_port
  #tbl_input_in     <- copy(tbl_input)
  #setnames(tbl_input, "ticker", "symbol")
  #setnames(tbl_input, "PortfolioEffect", "portfolio risk effect")
  #setnames(tbl_input, "expected_shortfall_Weight", "risk impact")
  #setorder(tbl_input, "portfolio risk effect", -"risk impact")
  color_palette <- c("#CC79A7", "#009E73")
  
  tbl_input_in[, sub_portfolio := do.call(c, purrr::map(strsplit(tbl_input$location, "//"), 1))]
  tbl_input_in[, ":=" (instrument_type = NULL, location = NULL)]
  setorder(tbl_input_in, "sub_portfolio")
  return(
    tbl_input_in[,.(sub_portfolio, name, symbol, weight, observed_performance, expected_performance)] %>%
      gt()  %>%
      tab_style(style = cell_fill(color = "grey"),
                locations = cells_body(rows = seq(1, nrow(tbl_input), 2))) %>%
      fmt_percent(columns = c("weight", "observed_performance", "expected_performance"), decimals = 1) %>%
      tab_options(
        heading.subtitle.font.size = 12,
        heading.align = "left",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width= px(3)
        #row_group.background.color = "grey",
        #table.background.color = "#d5e4eb"
      ) 
  )
}

observed_annual_growth_rate <- function(p_period        = c("Weekly", "Monthly", "Quarterly", "Yearly"),
                                        p_base_cur,
                                        p_end_date      = Sys.Date(),
                                        p_horizon_years = 1){
  
  start_date         <- p_end_date - 365*p_horizon_years
  
  dt_perf            <- get_performance(dt_all_prices[,.(symbol, date, adj_close)],
                                        instr_universe,
                                        p_base_cur,
                                        p_period,
                                        start_date,
                                        p_end_date)
}

#x <- c("EGO//EGO//Test", "EGO//test")
replace_ego <- function(p_str, replacement = ""){
  stringr::str_replace(p_str, "EGO//\\b", replacement) 
}
#replace_ego(x, "MyCompany//")

explore_vol_reference_assets <- function(p_symbol){
  #p_symbol <- "SPY.US"
  #******************************************************************************
  #### Explore Historical Levels of Dynamic Volatilities ####
  #******************************************************************************
  # We conduct an exploration to identify an appropriate date that can serve as our 
  # reference point in simulating future price paths. These estimates are solely 
  # for informational purposes and assist in choosing the reference data. The 
  # volatility estimates are utilized indirectly - through the profit 
  # and loss simulation via the delta vector.
  lst_pit_volatility <- EnvisionRiskRaaS::envrsk_market_volatility(
    symbols = p_symbol)
  dt_pit_volatility_ts <- lst_pit_volatility[["Output"]]
  dt_pit_volatility_ts[, date := as.Date(date)]
  
  sigma_quantiles  <- quantile(dt_pit_volatility_ts$sigma, c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1.0)) 
  sigma_mean_ttc   <- mean(dt_pit_volatility_ts$sigma)
  
  dt_quantiles_vol_tmp <- data.table(
    "symbol"                 = p_symbol,
    "volatility_environment" = paste0("quantile_", names(sigma_quantiles)),
    "sigma"                  = sigma_quantiles)
  
  dt_quantiles_vol_tmp <- rbind(dt_quantiles_vol_tmp,
                                data.table(
                                  "symbol"                 = p_symbol,
                                  "volatility_environment" = "through_the_cycle",
                                  "sigma"                  = sigma_mean_ttc))
  
  # Point-in-Time Volatility Estimates
  dt_pit_volatility <- setDT(dt_pit_volatility_ts)[, .SD[which.max(date)], by=symbol]
  
  # Stressed Volatility Estimates
  lst_stress_volatility <- EnvisionRiskRaaS::envrsk_market_stress_volatility(
    symbols = p_symbol)
  dt_stress_volatility <- lst_stress_volatility[["Output"]]
  dt_stress_volatility <- setDT(dt_stress_volatility)[, .SD[which.max(as.Date(valid_to))], by=c("symbol", "volatility_environment")]
  setorder(dt_stress_volatility, "volatility_environment", "symbol")
  
  # Combine volatilities from the 4 the volatility-scenarios ("point_in_time", 
  # "downturn", "severe_stress", "extreme_stress").
  dt_pit_vol_tmp <- copy(dt_pit_volatility)
  dt_pit_vol_tmp[, date := NULL]
  dt_pit_vol_tmp[, volatility_environment := "point_in_time"]
  
  dt_volatility <- rbind(dt_pit_vol_tmp,
                         dt_stress_volatility[, -c("valid_from", "valid_to")])
  
  dt_volatility <- rbind(dt_volatility,
                         dt_quantiles_vol_tmp)
  setorder(dt_volatility, sigma)
  
  fkt_last_observed <- function(p_sigma){
    dt_tmp <- dt_pit_volatility_ts[between(sigma, 0.98 * p_sigma, 1.02 * p_sigma)]
    select_date <- dt_tmp[, .SD[which.max(date)]]$date
    return(as.character(select_date))
  }
  
  n_count <- length(dt_pit_volatility_ts$sigma)
  fkt_time_spend_above <- function(p_sigma){
    time_spend_above <- nrow(dt_pit_volatility_ts[sigma >= p_sigma])/n_count
    return(time_spend_above)
  }
  
  dt_volatility[, last_observed := sapply(sigma, fkt_last_observed, simplify = TRUE)]
  dt_volatility[, time_spend_above := sapply(sigma, fkt_time_spend_above, simplify = TRUE)]
  
  return(dt_volatility)
}


#### Tables and Figures ####
# symbol <- "TSLA.US"
# dyn_vol_reference <- explore_vol_reference_assets(symbol)
# dyn_vol_reference <- dyn_vol_reference[,.(volatility_environment, 
#                                           sigma, 
#                                           last_observed, 
#                                           time_spend_above)]
# 
# dyn_vol_reference_4_tbl <- dyn_vol_reference[,.("Vol. Environment" = volatility_environment, 
#                                 "Vol. Estimate" = scales::percent(
#                                   sigma*sqrt(252),
#                                   accuracy = 0.1),
#                                 "Last Observed" = last_observed,
#                                 "Time Spend Above" = scales::percent(
#                                   time_spend_above,
#                                   accuracy = 0.1))]
# 
# names(dyn_vol_reference_4_tbl)[2] <- paste0(names(dyn_vol_reference_4_tbl)[2], footnote_marker_symbol(1))
# names(dyn_vol_reference_4_tbl)[3] <- paste0(names(dyn_vol_reference_4_tbl)[3], footnote_marker_symbol(2))
# names(dyn_vol_reference_4_tbl)[4] <- paste0(names(dyn_vol_reference_4_tbl)[4], footnote_marker_symbol(3))
# 
# pit_index <- which(dyn_vol_reference_4_tbl$`Vol. Environment` == "point_in_time")
# average_index <- which(dyn_vol_reference_4_tbl$`Vol. Environment` == "through_the_cycle")
# downturn_index <- which(dyn_vol_reference_4_tbl$`Vol. Environment` == "downturn")
# severe_index <- which(dyn_vol_reference_4_tbl$`Vol. Environment` == "severe_stress")
# extreme_index <- which(dyn_vol_reference_4_tbl$`Vol. Environment` == "extreme_stress")
# 
# dyn_vol_reference_4_tbl[pit_index, 'Vol. Environment'] <- paste0(dyn_vol_reference_4_tbl[pit_index, 'Vol. Environment'], footnote_marker_alphabet(1))
# dyn_vol_reference_4_tbl[average_index, 'Vol. Environment'] <- paste0(dyn_vol_reference_4_tbl[average_index, 'Vol. Environment'], footnote_marker_alphabet(2))
# dyn_vol_reference_4_tbl[downturn_index, 'Vol. Environment'] <- paste0(dyn_vol_reference_4_tbl[downturn_index, 'Vol. Environment'], footnote_marker_alphabet(3))
# dyn_vol_reference_4_tbl[severe_index, 'Vol. Environment'] <- paste0(dyn_vol_reference_4_tbl[severe_index, 'Vol. Environment'], footnote_marker_alphabet(4))
# dyn_vol_reference_4_tbl[extreme_index, 'Vol. Environment'] <- paste0(dyn_vol_reference_4_tbl[extreme_index, 'Vol. Environment'], footnote_marker_alphabet(5))
# 
# kableExtra::kbl(
#   caption = paste0("Dynamic Volatility Estimates - ", symbol),
#   dyn_vol_reference_4_tbl, 
#   booktabs = T,
#   format = "html",
#   longtable = TRUE,
#   align=c('l', rep('r', times=4)),
#   escape = FALSE) %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   row_spec(row = 0, 
#            color = "black", 
#            background = "white", 
#            bold = TRUE, 
#            font_size = 10,
#            extra_css = "border-bottom: 2px solid;") %>%
#   row_spec(row = pit_index, color = "white", background = "#27647B", bold = TRUE) %>%
#   row_spec(row = downturn_index, color = "white", background = "#CA3542", bold = TRUE) %>%
#   row_spec(row = average_index, color = "white", background =  "#57575F", bold = TRUE) %>%
#   row_spec(row = severe_index, color = "white", background = "#849FAD", bold = TRUE) %>%
#   row_spec(row = extreme_index, color = "white", background = "#AEC0C9", bold = TRUE) %>%
#   footnote(general = paste0("Volatility scenarios play an important role in calculating Value at Risk (VaR) and Expected Shortfall (ES), as they allow us to model potential changes in the value of a portfolio in response to various market conditions. The table highlight the 5 different volatility scenarios available: point-in-time, through-the-cycle and 3 different stress scenarios (downturn, severe-stress, and extreme-stress). The stress volatility scenarios (downturn, severe-stress and extreme-stress) are all reflection of historical periods with high volatility. Stress scenarios are defined by their respective timeframes and durations, each reflecting different market conditions. The table is updated at: ", format(Sys.Date(), format="%B %d %Y")),
#            symbol = c(
#              "Annualized volatility estimates, as calculated using the GARCH model",
#              "'Last Observed' denotes the most recent date on which the volatility estimate was recorded",
#              "'Time Spend Above' refers to the proportion of daily estimates where the volatility estimate was equal to or exceeded the threshold"),
#            alphabet = c(
#              "The point_in_time scenario reflects the most recent volatility estimate.",                       
#              "The ’downturn’ scenario derives its average volatility from the period around Lehman Brothers’ collapse (September 2008 - August 2009), a time of significant market upheaval." ,
#              "The through_the_cycle scenario reflects the (average) volatility.",
#              "The ’severe_stress’ scenario, calculated similarly to the ’downturn’ but using a period where the individual risk factor exhibited its maximum observed average yearly volatility",
#              "The ’extreme_stress’ scenario, akin to ’severe-stress’ but is based on a one-month duration, capturing short-term, intense market fluctuations.")
#            ) %>% 
#   kable_styling(full_width = F, font_size = 10, latex_options = c("HOLD_position")) 