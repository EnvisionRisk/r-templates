options(scipen = 999,
        digits = 12)

#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Demo/PortfolioWeights2Quantities/Dependencies/portfolio_weights_2_quantities_dependencies.R")

#******************************************************************************
#*
#* DEMONSTRATION
#*
#******************************************************************************
#### Context ####
# Import portfolio weights (in wide format)
dt_port_w <- readRDS("Data/pension_port_weights_wide.rds")

# Call the API to transform the portfolio weights to daily portfolio quantities
dt_obj_quantities <- EnvisionRiskRaaS::envrsk_workflow_weight_2_quantities(
  dt_snapshot_weight     = dt_port_w,
  init_port_market_value = 125000000000,
  is_wide                = TRUE,
  base_cur               = "DKK")

# 
dt_port_quantities <- dt_obj_quantities[["Output"]][["PortfolioQuantites"]]

#### DECORATE THE POSITION DATA WITH ID AND POSITION_TYPE ####
dt_port_quantities <- EnvisionRiskRaaS::envrsk_decorate_portfolio_with_uid(positions = dt_port_quantities)
dt_port_quantities <- EnvisionRiskRaaS::envrsk_decorate_portfolio_with_product_type(positions = dt_port_quantities)
setkey(dt_port_quantities, symbol, date)
setorder(dt_port_quantities, "date", "symbol")

#saveRDS(dt_port_quantities[,.(position_id, symbol, date, position_type, quantity)], "Data/pension_port_temporal_positions.rds")
#dt_port_quantities <- readRDS("Data/pension_port_temporal_positions.rds")

#### SHOW the positions over time ####
dt_4_graph <- dt_port_quantities[, .(symbol, date, quantity, mv_bc, weight)]
dt_4_graph_melt <- melt.data.table(dt_4_graph,
                                   id.vars = c("symbol", "date"))
dt_4_graph_melt[, date := as.Date(date)]
ggplot(dt_4_graph_melt, aes(x = date, y = value, colour = symbol))+
  geom_line()+
  facet_wrap(. ~ variable, nrow = 3, scales = "free_y")+
  ggthemes::theme_economist()+
  scale_colour_discrete("")+
  labs(title    = "Portfolio Weights to Quantities",
       subtitle = paste0("Market-value (in ", "DKK", "), quantities & weights - 2018-2022"),
       caption = "EnvisionRisk")



#dt_port_quantities <- readRDS("Data/pension_port_temporal_positions.rds")
dt_port_quantities <- readRDS("Data/pension_port_temporal_positions.rds")
max_date <- max(dt_port_quantities$date)
dt_port_quantities[date == max_date]
dt_port_market_weights <- EnvisionRiskRaaS::envrsk_portfolio_hypothetical_performance(
  max_date, 
  dt_port_quantities[date == max_date],
  base_cur = "DKK")

dt_port_weights <- merge(
  dt_port_market_weights[["Positions_Mapped"]][,.(uid, symbol, position_type, quantity)],
  dt_port_market_weights[["Output"]][,.(UID, MarketValue)],
  by.x = "uid",
  by.y = "UID",
  all.x = TRUE)

port_mv <- sum(dt_port_weights$MarketValue)
dt_port_weights[, weight := MarketValue / port_mv]
dt_port_weights[, date := max_date]

lst_input <- list(
  "MarketValue" = port_mv,
  "Weights"     = dt_port_weights[, .(date, symbol, weight)])


#### ####
my_port_raw      <- data.table("date" = c("2023-06-30"),
                               "TLT.US" = c(0.4),
                               "SPY.US" = c(0.6), stringsAsFactors = FALSE)

dt_obj_quantities_test <- EnvisionRiskRaaS::envrsk_workflow_weight_2_quantities(
  dt_snapshot_weight     = my_port_raw,
  init_port_market_value = 1000000000,
  is_wide                = TRUE,
  base_cur               = "DKK")

mv         <- sum(dt_obj_quantities_test$Output$PortfolioQuantites[date == as.Date("2023-10-20")]$mv_bc)
dt_weights <- dt_obj_quantities_test$Output$PortfolioQuantites[date == as.Date("2023-10-20")][,.(date, symbol, weight)]
dt_obj_quantities_test_2 <- EnvisionRiskRaaS::envrsk_workflow_weight_2_quantities(
  dt_snapshot_weight     = dt_weights,
  init_port_market_value = mv,
  is_wide                = FALSE,
  base_cur               = "DKK")

sum(dt_obj_quantities_test_2$Output$PortfolioQuantites[date == as.Date("2023-10-20")]$mv_bc)

merge(dt_obj_quantities_test$Output$PortfolioQuantites[date == as.Date("2023-10-20")],
      dt_obj_quantities_test_2$Output$PortfolioQuantites[date == as.Date("2023-10-20")],
      by = "symbol")
