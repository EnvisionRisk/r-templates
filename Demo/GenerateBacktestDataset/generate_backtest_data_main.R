options(scipen=999)

#### LOAD DEPENDENCIES ####
source("envrsk_api_bridge_2_R.R")
source("Demo/GenerateBacktestDataset/Dependencies/generate_backtest_dataset_dependencies.R")

#### AUTHENTICATE ####
envrsk_auth_renew_access_token()
access_token <- my_access_token[["access-token"]]

#******************************************************************************
#*
#### Providing Context ####
#*
#******************************************************************************
my_base_cur     <- "DKK"
my_volatility   <- "point_in_time"
my_signif_level <- 0.975

# Import the daily portfolio positions 'temporal_positions.rds'. The 
# 'temporal_positions.rds' is generated in the demo 
# '/Demo/PortfolioWeights2Quantities'.
dt_port_quantities           <- readRDS(paste0(getwd(), "/Data/temporal_positions.rds"))
dt_port_quantities           <- dt_port_quantities[date < Sys.Date()]
colnames(dt_port_quantities) <- toupper(colnames(dt_port_quantities))
dt_port_quantities_splt      <- base::split(dt_port_quantities, dt_port_quantities$DATE)


#### GO PARALLEL for all dates ####
# The number of cores should be lower than the number
# of threads available on the cloud server. The number of threads available is
# listed in the license agreement (usually one thread is available for a
# standard license). Get in touch in case you have a need for more threads. In
# general the execution time can be halved when the number of threads are
# tripled. 

# Set the number of cores. In case you set the number of cores higher than 1 
# please make sure that your license agreement support the number of threads 
# chosen. If you select a higher number of cores that supported (per your 
# license agreement) your excess thread-requests will be denied which might 
# result is some unexpected behavior.


n.cores <- 1 #envrsk_get_max_threads(access_token = access_token)
cl      <- parallel::makeCluster(n.cores)

invisible(parallel::clusterEvalQ(cl, library("data.table")))

# Export needed functions and datasets to the parallel-cluster
parallel::clusterExport(cl, c(
  "access_token",
  "my_base_cur",
  "my_volatility",
  "my_signif_level",
  "dt_port_quantities_splt",
  "envrsk_portfolio_risk_regular",
  "envrsk_portfolio_hypothetical_performance",
  "portfolio_temporal_output",
  "base_url",
  "base_path",
  "envrsk_post",
  "process_portfolio_return_values"))

# The dates to iterate over
iter_dates   <- names(dt_port_quantities_splt)

# Iterate over dates - for each data calculate portfolio risk measures.
# The speed is around 1-3 calender dates pr thread pr second (depending on the size
# of the portfolio). Since for backtest there typically is a need to calculate 
# risk for a large number of dates the below API calls can take a long time to 
# complete (several minutes). 

#### Parallel Execution - be patient :) ####
expected_avg_calc_time_pr_day <- nrow(dt_port_quantities)/length(iter_dates)/20
time_2_complete               <- length(iter_dates) * expected_avg_calc_time_pr_day / max(1, (2/3)*n.cores)
print(paste0("Expect the calculation time to take more than ", round(time_2_complete, 0), "seconds"))
print(paste0("Expect the calculation to complete around ", Sys.time() + time_2_complete))

tail(iter_dates)
t1           <- proc.time()
backtest_out <- parLapply(cl, iter_dates, function(x){
  my_positions    <- dt_port_quantities_splt[[x]][,.(POSITION_ID, POSITION_TYPE, SYMBOL, QUANTITY)]

  out <- portfolio_temporal_output(
    access_token  = access_token,
    date          = x,
    positions     = my_positions,
    base_cur      = my_base_cur,
    signif_level  = my_signif_level,
    volatility_id = my_volatility)

  out
})
closeAllConnections()
proc.time() - t1

#### Collate the results ####
backtest_data <- rbindlist(backtest_out)
backtestdata <- backtest_data[, .("Date" = date, VaR, ES, "PnL" = HypotheticalPnL)]

#### Save the dataset to '/Data' folder ####
saveRDS(backtestdata, "Data/backtestdata.rds")
write(jsonlite::toJSON(backtestdata), "Data/backtestdata.json")

#### Plot the risk estimates together with the observed P&L ####
backtestdata_melt <- melt.data.table(backtestdata, id.vars = "Date")
backtestdata_melt[, Date := as.Date(Date)]
ggplot(backtestdata_melt, aes(x = Date, y = value, colour = variable))+
    geom_line()


