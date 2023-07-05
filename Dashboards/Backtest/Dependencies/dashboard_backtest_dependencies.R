# In case one or more of the packages are not installed they will be installed
pckg <- c("flexdashboard", "ggplot2", 
          "knitr", "kableExtra")
sapply(pckg, usePackage)

library(data.table)
library(flexdashboard)
library(ggplot2)
library(knitr)
library(kableExtra)

source("Dashboards/Backtest/Dependencies/dashboard_backtest_plot.R")

#******************************************************************************
#* 
#******************************************************************************
dashboard_backtest <- function(backtestdata, base_cur, signif_level, title = "Backtest"){
  
  out_workflow_backtest <- EnvisionRiskRaaS::envrsk_workflow_backtest(
    backtestdata  = backtestdata,
    base_cur      = base_cur,
    signif_level  = signif_level)
  
  if(!is.null(out_workflow_backtest[["status_code"]])){
    if(out_workflow_backtest[["status_code"]] != 200){
      return(out_workflow_backtest)
    }
  }
  
  random_id <- IdGenerator(1)
  output_file_name <- paste0("Report_Backtest_id_", random_id)
  suppressWarnings(rmarkdown::render(input       = paste0(getwd(), "/Dashboards/Backtest/Dependencies/dashboard_backtest.Rmd"),
                                     output_file = paste0(getwd(), "/Dashboards/Backtest/Output/", output_file_name, ".html"), 
                                     params      = list(calc_backtest  = out_workflow_backtest,
                                                        set_title      = title),
                                     quiet       = TRUE))
  
  return(paste0("Report generated: ", getwd(), "/Dashboards/Backtest/Output/", output_file_name, ".html"))
}
