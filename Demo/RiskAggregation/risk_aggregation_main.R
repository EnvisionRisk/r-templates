options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/RiskAggregation/risk_aggregation_dependencies.R")

library(data.tree)
library(data.table)

#### CREDENTIALS ####
# Provide credentials - email and password. In case you have not yet recieved your personal credentials
# contact EnvisionRisk at info@envisionrisk.com
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
# The purpose of the demonstration is to illustrate how we can decorate the 
# portfolio with risk predictions using EnvisionRisk’s API. The demonstration 
# uses RStudio as the client side tool.
#
# The 'risk'-column will be defined as the 1-day Expected-Shortfall(97.5%). 
# The risk is denominated in EUR and the risk prediction is made for the 
# 15th November 2022. Other choices are possible and can be customized by 
# the user
#
# The only data used for the risk calculations are the portfolio specification - which 
# is available from 
# 'https://www.dropbox.com/s/h54j08xb5envg4p/example_port_structure.rds' - and 
# the date. The risk is calculated by using the endpoint:
# 'https://api.envisionrisk.com/v1/themis/portfolio-risk-component' 
#******************************************************************************
# Use the file from the r-template-main repository (if it has been downloaded)
my_port <- base::readRDS("Data/example_port_structure.rds")

#### Download the portfolio-file used for the demonstration from an external location ####
#my_port <- base::readRDS(base::url("https://www.dropbox.com/s/h54j08xb5envg4p/example_port_structure.rds?raw=1"))
#base::closeAllConnections()

#******************************************************************************
#*
#### AGGREGATION, simple portfolio structure ####
#*
#******************************************************************************
# Call the EnvisionRisk API with the portfolio 'my_port' and date '2022-11-15' as input. 
system.time(
  my_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token = access_token, 
    date         = "2022-11-15", 
    positions    = my_port[,.(symbol, position_type, quantity)])
)

# Retain the data that we need to use from API request (my_port_risk_out)
my_port_risk     <- format_portfolio_risk(my_port_risk_out)

# Transform the portfolio to a data.tree object and rename the ES column to Risk (more pleasing for the eye)
my_port_tree <- data.tree::as.Node(my_port_risk)

# Sort the data-tree
data.tree::Sort(my_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree
data.tree::SetFormat(my_port_tree, "quantity", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(my_port_tree, "ES", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(my_port_tree, 
#       "Position Type"   = "position_type", 
#       "Instrument Name" = "label", 
#       "Quantity"        = "quantity")

# OUTPUT
print(my_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity", 
      "Risk"            = "ES")

#******************************************************************************
#*
#### DEMONSTRATION - AGGREGATION, user defined portfolio structure ####
#*
#******************************************************************************
# Call the EnvisionRisk API with the portfolio 'my_port' and date '2022-11-15' as input.
system.time(
  my_port_risk_out <- envrsk_portfolio_risk_regular(
    access_token = access_token, 
    date         = "2022-11-15", 
    positions    = my_port)
)

# Retain the data that we want to use from API call (my_port_risk_out)
my_port_risk     <- format_portfolio_risk(my_port_risk_out)

# Transform the portfolio to a data.tree object and rename the ES column to Risk (more pleasing for the eye)
my_port_tree <- data.tree::as.Node(my_port_risk)

# Sort the data-tree
data.tree::Sort(my_port_tree, "position_type", "ES", decreasing = TRUE)

# Format the data-tree
data.tree::SetFormat(my_port_tree, "quantity", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
data.tree::SetFormat(my_port_tree, "ES", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))

# SHOW THE INPUT AS A DATA-TREE
# print(my_port_tree, 
#       "Position Type"   = "position_type", 
#       "Instrument Name" = "label", 
#       "Quantity"        = "quantity")

# OUTPUT
print(my_port_tree, 
      "Position Type"   = "position_type", 
      "Instrument Name" = "label", 
      "Quantity"        = "quantity", 
      "Risk"            = "ES")

# Transform and show only the portfolio levels (Only books)
my_port_tree <- data.tree::as.Node(my_port_risk[is.na(quantity)])
data.tree::Sort(my_port_tree, "ES")
data.tree::SetFormat(my_port_tree, "ES", formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
print(my_port_tree, 
      "Risk" = "ES")

