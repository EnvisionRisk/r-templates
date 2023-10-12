#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Dashboards/RiskSnapShot/Dependencies/dashboard_risk_snapshot_dependencies.R")

#### IMPORT POSITIONS FROM FILE ####
dt_positions <- readRDS("Data/treasury_example_port.rds")

#******************************************************************************
#### Create the risk-snapshot dashboard ####
#******************************************************************************
dashboard_risk_snapshot(
    positions     = dt_positions,
    date          = as.Date("2023-09-18"),
    volatility_id = "point_in_time",
    base_cur      = "DKK",
    risk_measure  = "ES",
    signif_level  = 0.975,
    horizon       = 1)
