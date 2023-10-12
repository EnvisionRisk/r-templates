options(scipen=999)

usePackage <- function(p)
{
  if (!is.element(p, utils::installed.packages()[,1]))
    utils::install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "parallel", "doParallel", "ggplot2", "wesanderson")
logo <- magick::image_read("C:/Users/jonas/Dropbox/Projects/templates/r-templates/logoicon.png")

# In case one or more of the packages are not installed they will be installed
suppressWarnings(sapply(pckg, usePackage))

library(data.table)
library(parallel)
library(doParallel)
library(ggplot2)
library(wesanderson)

#### PROCESS ####
fkt_process <- function(p_date, p_base_cur, p_symbs, p_symbs_labels = NULL,
                        p_horizon_pit = 1, p_horizon_dt = 10, 
                        p_signif_level_pit = 0.975, p_signif_level_dt = 0.99){
  dt_price_symbs <- EnvisionRiskRaaS::envrsk_market_price(p_symbs, base_cur = p_base_cur)[["Output"]][date <= p_date, .(symbol, date, close)]
  dt_price_symbs <- dt_price_symbs[, .SD[which.max(as.Date(date))], by = symbol]
  
  response_risk_symbs <- EnvisionRiskRaaS::envrsk_instrument_expected_shortfall(
    p_date,
    p_symbs, 
    base_cur      = p_base_cur, 
    horizon       = p_horizon_pit,
    signif_level  = p_signif_level_pit,
    volatility_id = "point_in_time")
  dt_risk <- merge(response_risk_symbs[["Output"]][, .(symbol, "Risk" = expected_shortfall)],
                   response_risk_symbs[["symbols_mapped"]][, .(symbol, name, qc)],
                   by = "symbol")
  
  response_stress_symbs <- EnvisionRiskRaaS::envrsk_instrument_expected_shortfall(
    p_date,
    p_symbs, 
    base_cur      = p_base_cur, 
    horizon       = p_horizon_dt,
    signif_level  = p_signif_level_dt,
    volatility_id = "downturn")
  
  dt_risk_table <- merge(
    merge(dt_risk,
          response_stress_symbs[["Output"]][, .(symbol, "Stress" = expected_shortfall)],
          by = "symbol"),
    dt_price_symbs[,.(symbol, close)],
    by = "symbol")
  dt_risk_table <- dt_risk_table[,.("Portfolio" = symbol, "MarketValue" = close, "Risk" = Risk, "Stress" = Stress)]
  dt_risk_table[, ":=" (RiskPct = Risk / MarketValue, StressPct = Stress / MarketValue)]
  
  if(is.null(p_symbs_labels)){
    p_symbs_labels <- p_symbs
  }
  dt_risk_table$Portfolio <- factor(
    dt_risk_table$Portfolio,
    levels = p_symbs,
    labels = p_symbs_labels)
  
  return(dt_risk_table)
}

#### ####
fkt_est <- function(dt_prices, use_date, horizon, symbols, base_cur, signif_level = 0.99, vol_sce = "point_in_time"){
  #horizon <- 1
  #symbols <- c("TRYDKK", "SEKDKK")
  #vol_sce <- "point_in_time"
  #use_date <- as.Date("2023-05-02")
  dt_price_basis <- dt_prices[symbol %in% symbols & date == use_date, .(symbol, close)]
  dt_es <- EnvisionRiskRaaS::envrsk_instrument_expected_shortfall(
    date          = use_date,
    symbols       = symbols, 
    base_cur      = base_cur,
    horizon       = horizon,
    signif_level  = signif_level,
    volatility_id = vol_sce)[["Output"]]
  dt_es <- merge(dt_es, dt_price_basis, by = "symbol", all.x = TRUE)
  dt_es[, expected_shortfall_pct := expected_shortfall / close ]
  
  dt_delta <- EnvisionRiskRaaS::envrsk_instrument_delta_vector(
    date          = use_date,
    symbols       = symbols, 
    base_cur      = base_cur,
    horizon       = horizon,
    volatility_id = vol_sce)[["Output"]]
  dt_delta <- merge(dt_delta, dt_price_basis, by.x = "SYMBOL", by.y = "symbol", all.x = TRUE)
  dt_delta[, PnL_pct := PnL / close ]
  
  lst_min_sim <- lapply(symbols, function(x){
    es <- dt_es[symbol == x]$expected_shortfall
    data.table("symbol"    = x,
               "worst_chg" = min(dt_delta[SYMBOL == x & PnL < es]$PnL))
  })
  dt_min_sim <- rbindlist(lst_min_sim)
  dt_es <- dt_es[, .(symbol, expected_shortfall)]
  dt_es[, horizon := horizon]
  dt_out <- merge(dt_es, dt_min_sim, by = "symbol", all.x = TRUE)
  dt_out <- merge(dt_out, dt_price_basis, by = "symbol", all.x = TRUE)
  return(dt_out)
}

#### ####
get_risk_assesment_daily <- function(p_date, p_base_cur, p_symbs, p_symbs_labels, 
                                     p_signif_level_pit = 0.975, p_signif_level_dt = 0.99){
  #use_date <- as.Date("2023-05-02")
  #symbols <- c("USDDKK")
  #base_cur <- "DKK"
  #signif_level <- 0.99
  message("Retrieving Price Information")
  dt_prs <- EnvisionRiskRaaS::envrsk_market_price(
    symbols = p_symbs,
    base_cur = p_base_cur)[["Output"]][,.(symbol, date, close)]
  dt_prs[, date := as.Date(date)]
  setorder(dt_prs, "symbol", "date")
  
  message("Retrieving Point-in-Time Risk Estimates")
  forward_points_days <- c(1:5, 10, 15, 20, 30)
  dt_out_pit <- lapply(forward_points_days, function(x){
    fkt_est(dt_prs, p_date, x, p_symbs, p_base_cur, p_signif_level_pit, "point_in_time")
  })
  dt_out_pit <- rbindlist(dt_out_pit)
  dt_out_pit[, worst_chg := NULL]
  setnames(dt_out_pit, "expected_shortfall", "lower_pit_mv")
  dt_out_pit[, lower_pit_mv := lower_pit_mv + close]
  dt_out_pit[, close := NULL]
  dt_out_pit_melt <- melt.data.table(dt_out_pit, id.vars = c("symbol", "horizon"))
  
  message("Retrieving Downturn Risk Estimates")
  dt_out_dt <- lapply(forward_points_days, function(x){
    fkt_est(dt_prs, p_date, x, p_symbs, p_base_cur, p_signif_level_dt, "downturn")
  })
  dt_out_dt <- rbindlist(dt_out_dt)
  dt_out_dt[, worst_chg := NULL]
  setnames(dt_out_dt, "expected_shortfall", "lower_dt_mv")
  dt_out_dt[, lower_dt_mv := lower_dt_mv + close]
  dt_out_dt[, close := NULL]
  dt_out_dt_melt <- melt.data.table(dt_out_dt, id.vars = c("symbol", "horizon"))
  
  dt_out_lower_mv <- rbind(dt_out_pit_melt,
                           dt_out_dt_melt)
  
  #### ####
  max_forward <- max(forward_points_days)
  dt_forward_dates <- data.table("date" = seq(p_date + 1, by = "day", length.out = max_forward+100))
  dt_forward_dates[, weekday := weekdays(date, abbreviate = TRUE)]
  dt_forward_dates <- dt_forward_dates[!weekday %in% c("Sat", "Sun")]
  dt_forward_dates[, horizon := .I]
  
  #### ####
  dt_forward_dates      <- merge(dt_forward_dates, dt_out_lower_mv, by = "horizon", all.y = TRUE)
  dt_forward_dates[, ":=" (horizon = NULL, weekday = NULL)]
  dt_forward_dates[, date := as.Date(date)]
  
  dt_out <- rbind(dt_prs[,.(date, symbol, "variable"  = "market_value", "value" = close)],
                  dt_forward_dates)
  dt_out[, date := as.Date(date)]
  
  if(is.null(p_symbs_labels)){
    p_symbs_labels <- p_symbs
  }
  dt_out$symbol <- factor(
    dt_out$symbol,
    levels = p_symbs,
    labels = p_symbs_labels)
  return(dt_out)
}

create_market_cockpit <- function(dt_market_cockpit, base_cur, prediction_date = Sys.Date() - 1){
  use_year         <- lubridate::year(prediction_date) 
  prices_from_date <- as.Date(paste0(use_year, "-01-01"))
  prices_to_date   <- min(max(dt_market_cockpit$date), 
                          as.Date(paste0(use_year, "-12-31"))) 
  dt_out_plot <- copy(dt_market_cockpit[between(date, prices_from_date, prices_to_date)])
  
  dt_out_cast            <- dcast.data.table(dt_out_plot, date + symbol ~ variable, value.var = "value")
  dt_out_cast_init_index <- dt_out_cast[, .SD[1], by = "symbol"][,.(symbol, market_value)]
  setnames(dt_out_cast_init_index, "market_value", "init_mv")
  dt_out_cast <- merge(dt_out_cast, dt_out_cast_init_index, by = "symbol", all.x = TRUE)
  dt_out_cast[, ":=" (mv_index           = market_value / init_mv,
                      lower_pit_mv_index = lower_pit_mv / init_mv,
                      lower_dt_mv_index  = lower_dt_mv / init_mv)]
  dt_out_cast[, init_mv := NULL]
  dt_out_plot <- melt.data.table(dt_out_cast, id.vars = c("symbol", "date"))
  dt_out_plot <- dt_out_plot[!is.na(value)]
  
  dt_out_plot <- dt_out_plot[variable %in% c("mv_index", "lower_pit_mv_index", "lower_dt_mv_index")]
  dt_out_plot$variable <- factor(dt_out_plot$variable, 
                                 levels = c("mv_index", "lower_pit_mv_index", "lower_dt_mv_index"), 
                                 labels = c("Price (index)", paste("Point-in-Time Risk Prediction (", as.character(prediction_date), ")"), "Stress Risk Prediction"))
  
  width <- 3
  p1 <- ggplot(dt_out_plot, aes(x = date, y = value, colour = variable))+
    geom_line()+
    facet_wrap(symbol ~ .)+ #, scale = "free_y"
    scale_colour_manual(values = wes_palette("Darjeeling1", n = 3))+
    theme_bw() +
    theme(legend.position = "top",
          plot.title = element_text(color = "#EC0108", face = 'bold'),
          plot.subtitle = element_text(color = "black"))+
    labs(title    = paste0("Market Cockpit (", use_year,")"),
         subtitle = 'Price Evolution & Risk Monitoring (YTD +30 days)',
         y        = paste("Index (in ", base_cur, ")"),
         colour   = "") +
    coord_cartesian(clip = "off") +
    theme(plot.margin = unit(c(1, 1, width, 1), "lines"))
  return(p1)
}

get_dynamic_volatility <- function(use_date, use_symbs){
  dt_vol <- EnvisionRiskRaaS::envrsk_market_volatility(
    use_symbs)[["Output"]]
  dt_vol[, date := as.Date(date)]
  
  use_year         <- lubridate::year(use_date) 
  prices_from_date <- as.Date(paste0(use_year, "-01-01"))
  prices_to_date   <- min(max(dt_vol$date), 
                          as.Date(paste0(use_year, "-12-31"))) 
  dt_out_plot <- copy(dt_vol[between(date, prices_from_date, prices_to_date)])
  
  dt_out_init_index <- dt_out_plot[, .SD[1], by = "symbol"][,.(symbol, sigma)]
  setnames(dt_out_init_index, "sigma", "init_sigma")
  dt_out_plot <- merge(dt_out_plot, dt_out_init_index, by = "symbol", all.x = TRUE)
  dt_out_plot[, ":=" (sigma_index = sigma / init_sigma)]
  dt_out_plot[, init_sigma := NULL]
  return(dt_out_plot)
}  

create_market_cockpit_dynamic_volatility <- function(dt_market_cockpit_dyn_vol){
  p1 <- ggplot(dt_market_cockpit_dyn_vol, aes(x = date, y = sigma_index))+
    geom_line()+
    facet_wrap(symbol ~ .)+ #, scale = "free_y"
    theme_bw()+
    theme(legend.position = "top",
          plot.title = element_text(color = "#EC0108", face = 'bold'),
          plot.subtitle = element_text(color = "black"))+
    labs(title    = paste0("Market Cockpit (", use_year,")"),
         subtitle = 'Dynamic Volatility Monitoring (YTD)',
         y        = paste("Index"),
         colour   = "")
  return(p1)
}