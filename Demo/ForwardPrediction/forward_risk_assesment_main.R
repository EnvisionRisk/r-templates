#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Demo/ForwardPrediction/Dependencies/forward_risk_assesment_dependencies.R")


#### SETTINGS ####
base_cur             <- "DKK"
confidence_level_pit <- 0.975
confidence_level_dt  <- 0.990
horizon_pit          <- 1
horizon_dt           <- 10

#### Define Portfolio & Date of Analysis ####
# use_symbs        <- c("EEM.US", "VNQ.US", "MDY.US", "XLP.US", "SPY.US", "EFA.US", "TIP.US", "AGG.US", "DJP.US", "SHV.US", "USDEUR")
# use_symbs_labels <- c("Emerging Markets", "REITs", "Mid Cap", "Consumer Staples", "Large Cap", "Int'l Stocks", "TIPS", "Bonds", "Comdty", "Cash", "FX_USD")
use_symbs        <- c("SPY.US")
use_symbs_labels <- c("Large Cap")
use_date         <- Sys.Date()-1

#### Risk Overview table ####
dt_out_risk_table <- fkt_process(
  use_date, base_cur, use_symbs, use_symbs_labels, 
  horizon_pit, horizon_dt, 
  confidence_level_pit, confidence_level_dt)

#### Risk Monitoring Graph ####
dt_out_forward_pred <- get_risk_assesment_daily(
  use_date, base_cur, use_symbs, use_symbs_labels,
  confidence_level_pit, confidence_level_dt)

#### OUTPUT OVERVIEW TABLE ####
dt_out_risk_table

#### OUTPUT MONITORING PLOT ####
market_cockpit <- create_market_cockpit(dt_out_forward_pred, base_cur, prediction_date = use_date)
print(market_cockpit)
grid::grid.raster(logo, x = 0.97, y = 0.03, just = c('right', 'bottom'), width = unit(1, 'inch'))

