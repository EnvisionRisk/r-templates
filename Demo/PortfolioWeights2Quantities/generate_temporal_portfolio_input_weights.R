# From portfolio weights to portfolio quantities

#### SELECT SYMBOLS 4 BACKTESTING ####
my_port_weight <- data.table::data.table(
  jsonlite::fromJSON('[{"symbol":"EEM.US", "weight":0.15},
                       {"symbol":"EFA.US", "weight":0.15},
                       {"symbol":"SPY.US", "weight":0.30},
                       {"symbol":"IEF.US", "weight":0.10},
                       {"symbol":"SHY.US", "weight":0.10},
                       {"symbol":"TLT.US", "weight":0.20}]'))

#### Create re-weighting dates - the first date is the initialization date ####
dates <- c(paste0(2018, c("-01-02" , "-03-30", "-06-30", "-09-30", "-12-31")),
           paste0(2019, c("-03-31" , "-06-30", "-09-30", "-12-31")),
           paste0(2020, c("-03-31" , "-06-30", "-09-30", "-12-31")),
           paste0(2021, c("-03-31" , "-06-30", "-09-30", "-12-31")),
           paste0(2022, c("-03-31" , "-06-30", "-09-30", "-12-31")),
           paste0(2023, c("-03-31")))

#### CREATE REWEIGHTING DATES ####
lst_port <- lapply(dates, function(x){
  dt_tmp <- data.table::copy(my_port_weight)
  dt_tmp[, date := x]
  dt_tmp
})
dt_port <- rbindlist(lst_port)
#saveRDS(dt_port, paste0(getwd(), "/Data/portfolio_weights.rds"))

#### TRANSFORM THE DATA INTO WIDE FORMAT ####
dt_port_w <- dcast.data.table(dt_port[, .(date, symbol, weight)],
                              date ~ symbol,
                              value.var = "weight")

#saveRDS(dt_port_w, paste0(getwd(), "/Data/portfolio_weights_wide.rds"))
