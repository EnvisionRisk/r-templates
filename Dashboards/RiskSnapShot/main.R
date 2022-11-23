options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Dashboards/RiskSnapShot/dashboard_risk_snapshot_dependencies.R")
source("Dashboards/RiskSnapShot/dashboard_risk_snapshot_plot.R")

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
#### INPUT for the demonstration - date, volatility_id and portfolio positions
#*
##******************************************************************************
demo_date          <- "2022-11-15"    # A date in ISO8601 format ('yyyy-mm-dd'). The date has to be after '2018-01-01'
demo_volatility_id <- "point_in_time" # choices: {point_in_time, through_the_cycle, downturn, severe_stress, extreme_stress}

# Portfolio positions (predefined example portfolio is available in the 
# '/Data' folder)
demo_port <- base::readRDS("Data/example_port_structure.rds")

#******************************************************************************
#### Create the risk-snapshot dashboard ####
# The dashboard will be written to the folder '/Output'
#******************************************************************************
# In case the access-token has expired - request a new one.
if(Sys.time() < my_access_token[["access-token-expiry"]]){
  dashboard_risk_snapshot(
    access_token,
    date          = demo_date,
    positions     = demo_port,
    risk_measure  = "ES",
    base_cur      = "EUR",
    horizon       = 1,
    signif_level  = 0.975,
    volatility_id = demo_volatility_id)
}
