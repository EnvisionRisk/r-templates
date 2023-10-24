#### DEMONSTRATION ####
#******************************************************************************
# The purpose of the demonstration is to illustrate how to decorate a 
# portfolio of financial positions with risk predictions using EnvisionRisk’s 
# API. The demonstration here uses RStudio as the client side tool.
# 
# The 'Risk'-column we add to the portfolio is defined as the 1-day 
# Expected-Shortfall(97.5%) and the risk is denominated in DKK. Other choices 
# are possible and can be customized by the user. See the API documentation 
# (https://envisionrisk.stoplight.io/) to see the full list of customizations.
#******************************************************************************
#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Demo/RiskAggregation/Dependencies/risk_aggregation_dependencies.R")

#### SETTINGS ####
demo_cur  <- "DKK"
demo_date <- as.Date("2023-10-04")

#### IMPORT PORTFOLIO ####
demo_port_data_all_dates <- rbindlist(jsonlite::read_json("Data/pension_port_temporal_positions_short.json"))
demo_port_data <- demo_port_data_all_dates[date == demo_date]

#### API REQUEST ####
# Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
dt_demo_port_risk <- aggregate_and_combine_port_risk(
  p_positions     = demo_port_data,
  p_date          = demo_date,
  p_volatility_id = "point_in_time",
  p_signif_level  = 0.975,
  p_horizon       = 1,
  p_base_cur      = demo_cur)

# Create Tree Structure and Format the Tree
demo_port_risk_tree   <- create_tree_output(dt_demo_port_risk, demo_date)

#### Define the Output Elements ####
do_print_tree <- function(){
  print(demo_port_risk_tree, 
        #"Position Type"     = "position_type", 
        #"Instrument Name"   = "label", 
        #"Quantity"          = "quantity",
        "Exposure"          = "Notational",
        "Exposure %"        = "percent",
        "Market Value"      = "MarketValue",
        "Impact Risk"       = "Impact_Risk_pct",
        "Risk"              = "Risk",
        "Risk %"            = "risk_pct",
        "Diversification %" = "diversification_pct",
        "Hypothetical PnL"  = "HypotheticalPnL", 
        filterFun = function(x) !x$isLeaf)
}

#### PRESENT THE RESULT ####
do_print_tree()

