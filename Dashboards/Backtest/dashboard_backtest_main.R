options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Dashboards/Backtest/dashboard_backtest_dependencies.R")

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
#### Load the backtest data from the working directory ####
#******************************************************************************
# Portfolio backtest (Th example backtest data is available in the '/Data' folder)
demo_backtestdata <- data.table::data.table(base::readRDS("Data/backtestdata.rds"))

# Create the backtest dashboard. 
dashboard_backtest(
    access_token,
    backtestdata = demo_backtestdata,
    base_cur     = "DKK",
    signif_level = 0.975,
    title        = "60/40 ETF")
