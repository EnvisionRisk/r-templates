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
demo_date <- as.Date("2023-10-10")

#### CALL THE API ####
api_response <- EnvisionRiskRaaS::envrsk_portfolio_risk_regular( 
  positions     = demo_port_data,
  date          = demo_date,
  volatility_id = "point_in_time",
  signif_level  = 0.975,
  horizon       = 1,
  base_cur      = demo_cur)

#### THE Input ####
print(demo_port_risk_tree, 
      #"Position Type"     = "position_type", 
      #"Instrument Name"   = "label", 
      "Exposure"          = "Notational",
      "Exposure %"        = "percent",
      "Market Value"      = "MarketValue",
      filterFun = function(x) !x$isLeaf)


#### IMPORT PORTFOLIO ####
# Portfolio positions (predefined example portfolio is available in the '/Data' folder)
# demo_port_data  <- base::readRDS("Data/pension_port_temporal_positions.rds")
# dt_pension_port <- readRDS(paste0(getwd(), "/Data/pension_port_example.rds"))
# demo_port_data  <- merge(demo_port_data,
#                         dt_pension_port[,.(symbol, location)],
#                         by = "symbol",
#                         all.x = TRUE)
# demo_port_data <- demo_port_data[date == demo_date]
#demo_port_data[symbol == "TIP.US", location := "Alternatives//InflationProtected"]

#### API REQUEST ####
# Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
dt_demo_port_risk <- aggregate_and_combine_port_risk(
  p_positions     = demo_port_data,
  p_date          = demo_date,
  p_volatility_id = "point_in_time",
  p_signif_level  = 0.975,
  p_horizon       = 1,
  p_base_cur      = demo_cur)

# Calculate Stress as downturn, 10-day 99.0% Expected-shortfall
dt_demo_port_stress <- aggregate_and_combine_port_risk(
  p_positions     = demo_port_data,
  p_date          = demo_date,
  p_volatility_id = "downturn",
  p_signif_level  = 0.975,
  p_horizon       = 1,
  p_base_cur      = demo_cur)

# Create Tree Structure and Format the Tree
demo_port_risk_tree   <- create_tree_output(dt_demo_port_risk, demo_date)
demo_port_stress_tree <- create_tree_output(dt_demo_port_stress, demo_date)

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

