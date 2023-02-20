usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "jsonlite", "httr", "ggthemes", "parallel")

# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)
library(ggplot2)
library(parallel)

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#******************************************************************************
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#******************************************************************************
portfolio_temporal_output <- function(access_token,
                                      date,
                                      positions,
                                      base_cur      = NULL,
                                      signif_level  = NULL,
                                      volatility_id = NULL){

  envrsk_obj_risk_regular <- envrsk_portfolio_risk_regular(
    access_token  = access_token,
    date          = date,
    positions     = positions,
    base_cur      = base_cur,
    volatility_id = volatility_id,
    horizon       = 1,
    signif_level  = signif_level,
    report_depth  = 0)
  dt_rsk_out <- envrsk_obj_risk_regular[["Output"]]
  dt_rsk_out[, date := date]

  envrsk_obj_hypothetical_perf <- envrsk_portfolio_hypothetical_performance(
    access_token  = access_token,
    date          = date,
    positions     = positions,
    base_cur      = base_cur,
    report_depth  = 0)
  dt_pnl_out <- envrsk_obj_hypothetical_perf[["Output"]]
  dt_pnl_out[, date := date]

  dt_out <- merge(dt_rsk_out[,.(UID, date, Location, VaR, ES)],
                  dt_pnl_out[, .(date, Notational, MarketValue, HypotheticalPnL)],
                  by = "date",
                  all.x = TRUE)

  return(dt_out)
}
