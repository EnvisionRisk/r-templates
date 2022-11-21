options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/RiskAggregation/risk_aggregation_dependencies.R")

library(data.tree)
library(data.table)

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
#### INPUT for the demonstration - date, volatility_id and portfolio positions
#*
##******************************************************************************
demo_date          <- "2022-11-15"    # A date in ISO8601 format ('yyyy-mm-dd'). The date has to be after '2018-01-01'
demo_volatility_id <- "point_in_time" # choices: {point_in_time, through_the_cycle, downturn, severe_stress, extreme_stress}

# Portfolio positions (predefined example portfolio is available in the 
# '/Data' folder)
demo_port <- base::readRDS("Data/example_port_structure.rds")

#### Alternatively download the portfolio-file used for the demonstration from an external location ####
#demo_port <- base::readRDS(base::url("https://www.dropbox.com/s/h54j08xb5envg4p/example_port_structure.rds?raw=1"))
#base::closeAllConnections()

# To see a list of available instruments (symbols):
# dt_available_instrumements <- envrsk_instrument_search(access_token = access_token, valid_at = demo_date)
# dt_available_instrumements

#******************************************************************************
#*
#### AGGREGATION, simple portfolio structure ####
#*
#******************************************************************************
# Call the EnvisionRisk API. 
system.time(
  demo_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token  = access_token, 
    date          = demo_date, 
    positions     = demo_port[,.(symbol, position_type, quantity)],
    volatility_id = demo_volatility_id)
)

# Retain the data that we need from API request (demo_port_risk_out)
demo_port_risk     <- format_portfolio_risk(demo_port_risk_out)

# Transform the portfolio to a data.tree object and rename the ES column to Risk (more pleasing for the eye)
demo_port_tree <- data.tree::as.Node(demo_port_risk)

# Sort the data-tree
data.tree::Sort(demo_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port_tree, "ES",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(demo_port_tree, 
#       "Position Type"   = "position_type", 
#       "Instrument Name" = "label", 
#       "Quantity"        = "quantity")

# OUTPUT
print(demo_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity", 
      "Risk"            = "ES")

#******************************************************************************
#*
#### DEMONSTRATION - AGGREGATION, user defined portfolio structure ####
#*
#******************************************************************************
# Call the EnvisionRisk API.
system.time(
  demo_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token  = access_token, 
    date          = demo_date, 
    positions     = demo_port,
    volatility_id = demo_volatility_id)
)

# Retain the data we need from API call (demo_port_risk_out)
demo_port_risk     <- format_portfolio_risk(demo_port_risk_out)

# Transform the portfolio to a data.tree object and rename the ES column to Risk (more pleasing for the eye)
demo_port_tree <- data.tree::as.Node(demo_port_risk)

# Sort the data-tree
data.tree::Sort(demo_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree
data.tree::SetFormat(demo_port_tree, "quantity", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(demo_port_tree, "ES",       formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(demo_port_tree, 
#       "Position Type"   = "position_type", 
#       "Instrument Name" = "label", 
#       "Quantity"        = "quantity")

# OUTPUT
print(demo_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity", 
      "Risk"            = "ES")

#### Only show the books (remove the positions)
# Transform and show only the portfolio levels (Only books)
demo_port_tree <- data.tree::as.Node(demo_port_risk[is.na(quantity)])

# Sort the data-tree
data.tree::Sort(demo_port_tree, "ES")
data.tree::SetFormat(demo_port_tree, "ES", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# OUTPUT
print(demo_port_tree, 
      "Risk" = "ES")

