#******************************************************************************
#* Denependencies 
#******************************************************************************
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "jsonlite", "httr", "keyring", 
          "flexdashboard", "ggplot2", 
          "knitr", "kableExtra")
# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)
library(flexdashboard)
library(ggplot2)
library(knitr)
library(kableExtra)

source("Dashboards/Backtest/dashboard_backtest_plot.R")

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


#******************************************************************************
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#******************************************************************************
#### workflow-backtest ####
envrsk_workflow_backtest <- function(access_token,
                                     backtestdata,
                                     base_cur      = NULL,
                                     signif_level  = NULL){
  
  end_point <- "workflow-backtest"
  api_url <- paste0(base_url, base_path, end_point)
  
  .params <- list("base_cur"      = base_cur,
                  "signif_level"  = signif_level)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = backtestdata)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list("Title"  = out_raw[["Title"]],
                "Input"  = list("backtestdata" = data.table::rbindlist(out_raw[["Input"]][["BacktestData"]]),
                                "base_cur"     = out_raw[["Input"]][["BaseCur"]],
                                "signif_level" = out_raw[["Input"]][["SignifLevel"]]),
                "TechOpr"  = out_raw[["TechOpr"]],
                "Output"   = data.table::rbindlist(out_raw[["Output"]]))
  } else {
    return(res_out)
  }
  
  return(out)
}

dashboard_backtest <- function(access_token, backtestdata, base_cur, signif_level, title = "Backtest"){
  
  out_workflow_backtest <- envrsk_workflow_backtest(access_token  = access_token, 
                                                    backtestdata  = backtestdata,
                                                    base_cur      = base_cur,
                                                    signif_level  = signif_level)
  
  if(!is.null(out_workflow_risk_snapshot[["status_code"]])){
    if(out_workflow_risk_snapshot[["status_code"]] != 200){
      return(out_workflow_risk_snapshot)
    }
  }
  
  random_id <- IdGenerator(1)
  output_file_name <- paste0("Report_Backtest_", out_workflow_backtest$Input$date, "_id_", random_id)
  suppressWarnings(rmarkdown::render(input       = "/Dashboards/Backtest/dashboard_backtest.Rmd",
                                     output_file = paste0(getwd(), "/Output/", output_file_name, ".html"), 
                                     params      = list(calc_backtest  = out_workflow_backtest,
                                                        set_title      = title),
                                     quiet       = TRUE))
  
  return(paste0("Report generated: ", getwd(), "/Dashboards/Backtest/Output/", output_file_name, ".html"))
}
