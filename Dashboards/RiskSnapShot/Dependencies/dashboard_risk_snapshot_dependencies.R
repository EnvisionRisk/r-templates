#******************************************************************************
#* Denependencies
#******************************************************************************
pckg <- c("flexdashboard", "treemapify","ggplot2",
          "knitr", "kableExtra")

# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)
library(flexdashboard)
library(treemapify)
library(ggplot2)
library(knitr)
library(kableExtra)

source("Dashboards/RsikSnapShot/Dependencies/dashboard_risk_snapshot_plot.R")

# Create the risk dashboard with knitr
dashboard_risk_snapshot <- function(date, positions, risk_measure,
                                    base_cur, horizon, signif_level,
                                    volatility_id){

  if(Sys.Date() == as.Date(date)){
    date <- Sys.Date() - 1
  }
  
  out_workflow_risk_snapshot <- EnvisionRiskRaaS::envrsk_workflow_risk_snapshot(date = date,
                                                              positions     = positions,
                                                              risk_measure  = risk_measure,
                                                              base_cur      = base_cur,
                                                              horizon       = horizon,
                                                              signif_level  = signif_level,
                                                              volatility_id = volatility_id)
  
  if(!is.null(out_workflow_risk_snapshot[["status_code"]])){
    if(out_workflow_risk_snapshot[["status_code"]] != 200){
      return(out_workflow_risk_snapshot)
    }
  }
  
  if(nrow(out_workflow_risk_snapshot[["positions_unmapped"]]) > 0){
    warning(paste("Some position was not recognised and have been left out: ", paste(out_workflow_risk_snapshot[["positions_unmapped"]]$SYMBOL, collapse = ", ")))
  }
  random_id <- IdGenerator(1)
  output_file_name <- paste0("Report_Risk_Snapshot_", out_workflow_risk_snapshot$Input$date, "_id_", random_id)
  suppressWarnings(rmarkdown::render(input       = paste0(getwd(), "/Dashboards/RiskSnapShot/Dependencies/dashboard_risk_snapshot.Rmd"),
                                     #output_file = paste0("/home/rstudio/r-templates-main/Dashboards/RiskSnapShot/Output/", output_file_name, ".html"),
                                     output_file = paste0(getwd(), "/Dashboards/RiskSnapShot/Output/", output_file_name, ".html"),
                                     params      = list(workflow_object = out_workflow_risk_snapshot),
                                     quiet       = TRUE))

  #return(paste0("Report generated: ", getwd(), "/Dashboards/RiskSnapShot/Output/", output_file_name, ".html"))
  return(paste0("Report generated: ", getwd(), "/Dashboards/RiskSnapShot/Output/", output_file_name, ".html"))
}

