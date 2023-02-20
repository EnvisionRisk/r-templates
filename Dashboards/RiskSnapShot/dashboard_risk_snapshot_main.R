options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Dashboards/RiskSnapShot/Dependencies/dashboard_risk_snapshot_dependencies.R")

#### AUTHENTICATE ####
envrsk_auth_renew_access_token()
access_token <- my_access_token[["access-token"]]

# Portfolio positions (predefined example portfolio is available in the 
# '/Data' folder)
demo_port <- base::readRDS("Data/example_port_structure.rds")

#******************************************************************************
#### Create the risk-snapshot dashboard ####
# The dashboard will be written to the folder '/Output'
#******************************************************************************
# In case the access-token has expired - request a new one.
dashboard_risk_snapshot(
    access_token,
    positions     = demo_port,
    date          = "2022-11-15",
    volatility_id = "point_in_time",
    base_cur      = "EUR",
    risk_measure  = "ES",
    signif_level  = 0.975,
    horizon       = 1)
