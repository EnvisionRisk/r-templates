options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/0.RiskAggregation/Dependencies/risk_aggregation_dependencies.R")

#### CREDENTIALS ####
# Provide credentials - email and password. In case you have not yet received 
# your personal credentials, contact EnvisionRisk at info@envisionrisk.com
Sys.setenv("USR_ID"  = getPass::getPass(msg = "Please provide email: ", noblank = TRUE, forcemask = FALSE))
Sys.setenv("USR_PWD" = getPass::getPass(msg = "Please provide password: ", noblank = TRUE, forcemask = FALSE))

#******************************************************************************
#*
#* AUTHENTICATIO WITH THE RISK SERVER 
#*
#******************************************************************************
# Retrieve the access-token from the Auth-server.
my_access_token <- get_access_token(Sys.getenv("USR_ID"), 
                                    Sys.getenv("USR_PWD"))

# Extract the access-token. The access-token is valid for 24 hours after
# it has been requested. 
access_token    <- my_access_token[["access-token"]]

#The access-token is valid until;
print(paste0("The access-token is valid until: ", my_access_token[["access-token-expiry"]]))

#******************************************************************************
#*
#* DEMONSTRATION
#*
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
#******************************************************************************

#******************************************************************************
#*
#### INPUT for the demonstration - portfolio positions, date and volatility_id
#*
##******************************************************************************
# Portfolio positions (predefined example portfolio is available in the '/Data' folder)
demo_port_data <- base::readRDS("Data/example_port_structure.rds")

#### Alternatively download the portfolio-file used for the demonstration from an external location ####
#demo_port <- base::readRDS(base::url("https://www.dropbox.com/s/h54j08xb5envg4p/example_port_structure.rds?raw=1"))
#base::closeAllConnections()

# In case you want to get a list of available instruments (symbols) to use for your own portfolio construction:
# dt_available_instrumements <- envrsk_instrument_search(access_token = access_token, valid_at = Sys.Date())
# dt_available_instrumements

#******************************************************************************
#*
#### DEMONSTRATION - user defined portfolio structure - unconditional risk ####
#*
#******************************************************************************
# Call the EnvisionRisk API. The API endpoint are called through the R-specific 
# function 'envrsk_portfolio_risk_regular'. This function takes care of making 
# the input data ready for the API request and formatting the response from the 
# API into R-specific data structures. The output from the API is saved into the
# variable 'demo_port_risk_out'.

#******************************************************************************
#*
#### Value-at-Risk (VaR) & Expected-Shortfall (ES) ####
#*
#******************************************************************************
demo_cur  <- "EUR"
demo_date <- as.Date("2022-12-13")

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

# Merge the data from the 3 API calls
dt_demo_port <- merge(
  merge(dt_demo_port_risk[,.(pathString, position_type, label, quantity, "Value-at-Risk" = VaR, "Expected-Shortfall" = ES)],
        dt_demo_port_stress[,.("Stress-Test" = ES, pathString)],
        by = "pathString"),
  dt_demo_port_perf[,.(pathString, Notational, MarketValue, HypotheticalPnL)],
  by = "pathString")

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

print(demo_port, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity",
      "Market Value" = "MarketValue",
      "Risk" = "Expected-Shortfall",
      "Stress" = "Stress-Test")

print(demo_port, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity",
      "Value-at-Risk",
      "Expected-Shortfall",
      "Stress-Test")



