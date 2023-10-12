#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Dashboards/Backtest/Dependencies/dashboard_backtest_dependencies.R")

#******************************************************************************
# Portfolio backtest (Th example backtest data is available in the '/Data' folder)
#******************************************************************************
demo_backtestdata <- data.table::data.table(base::readRDS("Data/backtestdata.rds"))

# Create the backtest dashboard. 
dashboard_backtest(
    backtestdata = demo_backtestdata,
    base_cur     = "DKK",
    signif_level = 0.975,
    title        = "Pension Example")
