library(kableExtra)
library(data.table)
library(ggplot2)
library(ggthemes)
library(foreach)

#******************************************************************************
#*
#### SIMULATE PRICE PATHS FOR LONG HORIZONS ####
#*
#******************************************************************************
# Objective: To project the price movements of a portfolio comprising various 
# instruments over a period of 1 to 5 years (encompassing 252 to 1250 trading 
# days). Utilizing EnvisionRisk's service ensures the maintenance of the 
# interdependence among these instruments.
source("C:/Users/jonas/Dropbox/Kunder/Udvikling/Examples/strategic_asset_allocation_dependencies.R")

dt_instrument_search <- EnvisionRiskRaaS::envrsk_instrument_search()