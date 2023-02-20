#### DEMONSTRATION ####
#******************************************************************************
# The purpose of the demonstration is to illustrate how to decorate a 
# portfolio of financial positions with risk predictions using EnvisionRisk’s 
# API. The demonstration here uses RStudio as the client side tool.
# 
# The 'Risk'-column we add to the portfolio is defined as the 1-day 
# Expected-Shortfall(97.5%) and the risk is denominated in EUR. Other choices 
# are possible and can be customized by the user. See the API documentation 
# (https://envisionrisk.stoplight.io/) to see the full list of customizations.
#
# The risk is calculated using the endpoint:
# 'https://api.envisionrisk.com/v1/themis/portfolio-risk-component' 
#
# Call the EnvisionRisk API. The API endpoint are called through the R-specific 
# function 'envrsk_portfolio_risk_regular'. This function takes care of making 
# the input data ready for the API request and formatting the response from the 
# API into R-specific data structures. The output from the API is saved into the
# variable 'demo_port_risk_out'.
#******************************************************************************
options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/RiskAggregation/Dependencies/risk_aggregation_dependencies.R")

#### AUTHENTICATE ####
envrsk_auth_renew_access_token()

#### IMPORT PORTFOLIO ####
# Portfolio positions (predefined example portfolio is available in the '/Data' folder)
demo_port_data <- base::readRDS("Data/treasury_example_port.rds")

#### SETTINGS ####
demo_cur  <- "DKK"
demo_date <- as.Date("2023-01-01")

#### API REQUEST ####
# Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
demo_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token  = access_token, 
    positions     = demo_port_data,
    date          = demo_date,
    volatility_id = "point_in_time",
    signif_level  = 0.975,
    horizon       = 1,
    base_cur      = demo_cur)

# Retain the data we need from API call.
dt_demo_port_risk <- format_portfolio_risk(demo_port_risk_out)
dt_demo_port_risk[is.na(VaR)]

#dt_demo_port_risk[is.na(VaR)]

# Calculate Stress as downturn, 10-day 99.0% Expected-shortfall
demo_port_stress_out <- envrsk_portfolio_risk_regular(
  access_token  = access_token, 
  positions     = demo_port_data,
  date          = demo_date,
  volatility_id = "downturn",
  signif_level  = 0.99,
  horizon       = 10,
  base_cur      = demo_cur)

# Retain the data we need from API call.
dt_demo_port_stress <- format_portfolio_risk(demo_port_stress_out)

# Calculate Market-value for the portfolio
demo_port_perf_out <- envrsk_portfolio_hypothetical_performance(
  access_token  = access_token, 
  positions     = demo_port_data,
  date          = demo_date,
  base_cur      = demo_cur)


# Retain the data we need from API call.
dt_demo_port_perf <- format_portfolio_perf(demo_port_perf_out)

#### CONSOLIDATE THE API RESPONSE ####
# Merge the data from the 3 API calls
dt_demo_port <- merge(
  merge(dt_demo_port_risk[,.(pathString, position_type, label, quantity, "Value-at-Risk" = VaR, "Expected-Shortfall" = ES)],
        dt_demo_port_stress[,.("Stress-Test" = ES, pathString)],
        by = "pathString"),
  dt_demo_port_perf[,.(pathString, Notational, MarketValue, HypotheticalPnL)],
  by = "pathString")

#### FORMAT THE RESPONSE ####
# Transform the portfolio to a data.tree object (more pleasing for the eye)
demo_port <- data.tree::as.Node(dt_demo_port)

# Sort the data-tree by Stress-test
data.tree::Sort(demo_port, "position_type", "Stress-Test", decreasing = TRUE)

# Format the data-tree column 'quantity' and 'ES'
data.tree::SetFormat(demo_port, "quantity", formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})
data.tree::SetFormat(demo_port, "Notational",  formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port, "MarketValue",  formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port, "HypotheticalPnL",  formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port, "Value-at-Risk",  formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port, "Expected-Shortfall",  formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port, "Stress-Test",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))


#### PRESENT THE RESULT ####
print(demo_port, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity",
      "Market Value"    = "MarketValue",
      "Risk"            = "Expected-Shortfall",
      "Stress"          = "Stress-Test")




