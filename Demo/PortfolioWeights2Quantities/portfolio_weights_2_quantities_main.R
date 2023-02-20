options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/PortfolioWeights2Quantities/Dependencies/portfolio_weights_2_quantities_dependencies.R")

#### AUTHENTICATE ####
envrsk_auth_renew_access_token()
access_token <- my_access_token[["access-token"]]

#******************************************************************************
#*
#* DEMONSTRATION
#*
#******************************************************************************
#### Context ####
# Import portfolio weights (in wide format)
dt_port_w <- readRDS("Data/portfolio_weights_wide.rds")

# Call the API to transform the portfolio weights to daily portfolio quantities
dt_obj_quantities <- envrsk_workflow_weight_2_quantities(access_token           = access_token,
                                                         dt_snapshot_weight     = dt_port_w,
                                                         init_port_market_value = 1000000,
                                                         is_wide                = TRUE,
                                                         base_cur               = "DKK")

# 
dt_port_quantities <- dt_obj_quantities[["Output"]][["PortfolioQuantites"]]

#### DECORATE THE POSITION DATA WITH ID AND POSITION_TYPE ####
dt_port_quantities <- envrsk_decorate_portfolio_id(access_token = access_token,
                                                   positions    = dt_port_quantities)
setnames(dt_port_quantities, "POSITION_ID", "position_id")
dt_port_quantities[, position_type := "etf"]
setkey(dt_port_quantities, symbol, date)
setorder(dt_port_quantities, "date", "symbol")

#saveRDS(dt_port_quantities[,.(position_id, symbol, date, position_type, quantity)], "Data/temporal_positions.rds")

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
