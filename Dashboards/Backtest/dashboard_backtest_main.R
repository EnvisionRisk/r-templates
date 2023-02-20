options(scipen=999)

#### DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Dashboards/Backtest/Dependencies/dashboard_backtest_dependencies.R")

#### AUTHENTICATE ####
envrsk_auth_renew_access_token()
access_token <- my_access_token[["access-token"]]

#******************************************************************************
# Portfolio backtest (Th example backtest data is available in the '/Data' folder)
#******************************************************************************
demo_backtestdata <- data.table::data.table(base::readRDS("Data/backtestdata.rds"))

# Create the backtest dashboard. 
dashboard_backtest(
    access_token,
    backtestdata = demo_backtestdata,
    base_cur     = "DKK",
    signif_level = 0.975,
    title        = "60/40 ETF")
