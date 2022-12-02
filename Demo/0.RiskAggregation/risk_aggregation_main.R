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
# The purpose of the demonstration is to illustrate how we can decorate a 
# portfolio of financial positions with risk predictions using EnvisionRisk’s 
# API. The demonstration here uses RStudio as the client side tool.
# 
# The 'risk'-column we add to the portfolio is defined as the 1-day 
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
demo_port <- base::readRDS("Data/example_port_structure.rds")

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
demo_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token  = access_token, 
    positions     = demo_port,
    date          = "2022-11-15",
    volatility_id = "point_in_time",
    signif_level  = 0.975,
    horizon       = 1,
    base_cur      = "EUR")

# Retain the data we need from API call.
demo_port_risk <- format_portfolio_risk(demo_port_risk_out)

# Transform the portfolio to a data.tree object (more pleasing for the eye)
demo_port_tree <- data.tree::as.Node(demo_port_risk)

# Sort the data-tree by ES
data.tree::Sort(demo_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree column 'quantity' and 'ES'
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})
data.tree::SetFormat(demo_port_tree, "VaR",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port_tree, "ES",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(demo_port_tree, 
#       "Position Type" = "position_type", 
#       "Quantity"      = "quantity")

# OUTPUT ES
print(demo_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity",
      "Expected-Shortfall" = "ES")

# OUTPUT VaR and ES
print(demo_port_tree, 
      "Position Type"      = "position_type", 
      "Instrument Name"    = "label", 
      "Quantity"           = "quantity", 
      "Value-at-Risk"      = "VaR",
      "Expected-Shortfall" = "ES")

#******************************************************************************
#*
#### ONLY SHOW THE AGGERGATED ROWS ####
#*
#******************************************************************************
# In case we only want to see the aggregated levels of the portfolio
demo_port_tree <- data.tree::as.Node(demo_port_risk[is.na(quantity)])

# Sort the data-tree
data.tree::Sort(demo_port_tree, "ES")
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})

# OUTPUT
print(demo_port_tree,
      "Value-at-Risk"      = "VaR",
      "Expected-Shortfall" = "ES")

#******************************************************************************
#*
#### STRESS TEST ####
#*
#******************************************************************************
demo_port_risk_out <- envrsk_portfolio_risk_regular(
  access_token  = access_token, 
  positions     = demo_port,
  date          = "2022-11-15",
  volatility_id = "downturn",
  signif_level  = 0.975,
  horizon       = 10,
  base_cur      = "EUR")

# Retain the data we need from API call.
demo_port_risk <- format_portfolio_risk(demo_port_risk_out)

# Transform the portfolio to a data.tree object (more pleasing for the eye)
demo_port_tree <- data.tree::as.Node(demo_port_risk)

# Sort the data-tree by ES
data.tree::Sort(demo_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree column 'quantity' and 'ES'
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})
data.tree::SetFormat(demo_port_tree, "VaR",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port_tree, "ES",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(demo_port_tree, 
#       "Position Type" = "position_type", 
#       "Quantity"      = "quantity")

# OUTPUT ES
print(demo_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity",
      "Stress Test"     = "ES")

#******************************************************************************
#*
#### ONLY SHOW THE AGGERGATED ROWS ####
#*
#******************************************************************************
# In case we only want to see the aggregated levels of the portfolio
demo_port_tree <- data.tree::as.Node(demo_port_risk[is.na(quantity)])

# Sort the data-tree
data.tree::Sort(demo_port_tree, "ES")
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})

# OUTPUT
print(demo_port_tree, 
      "Risk" = "ES")

