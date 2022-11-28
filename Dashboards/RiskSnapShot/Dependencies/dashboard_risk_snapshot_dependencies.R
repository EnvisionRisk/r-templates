#******************************************************************************
#* Denependencies
#******************************************************************************
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "jsonlite", "httr",
          "flexdashboard", "treemapify","ggplot2",
          "knitr", "kableExtra")
# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)
library(flexdashboard)
library(treemapify)
library(ggplot2)
library(knitr)
library(kableExtra)

source("Dashboards/RiskSnapShot/Dependencies/dashboard_risk_snapshot_plot.R")

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#******************************************************************************
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#******************************************************************************
#### workflow-risk-snapshot ####
envrsk_workflow_risk_snapshot <- function(access_token,
                                          date,
                                          positions,
                                          risk_measure = "ES",
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL){
  end_point <- "workflow-risk-snapshot"
  api_url <- paste0(base_url, base_path, end_point)

  .params <- list("date"          = date,
                  "risk_measure"  = risk_measure,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"                  = out_raw[["Input"]],
      "tech_opr"               = out_raw[["tech_opr"]],
      "positions"              = data.table::rbindlist(out_raw[["positions"]]),
      "portfolio_delta_vector" = data.table::rbindlist(out_raw[["portfolio_delta_vector"]]),
      "portfolio_risk"         = data.table::rbindlist(out_raw[["portfolio_risk"]]),
      "positions_mapped"       = data.table::rbindlist(out_raw[["Positions_Mapped"]], fill = TRUE),
      "positions_unmapped"     = data.table::rbindlist(out_raw[["Positions_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }

  return(out)
}

# Create the risk dashboard with knitr
dashboard_risk_snapshot <- function(access_token, date, positions, risk_measure,
                                    base_cur, horizon, signif_level,
                                    volatility_id){

  if(Sys.Date() == as.Date(date)){
    date <- Sys.Date() - 1
  }
  
  out_workflow_risk_snapshot <- envrsk_workflow_risk_snapshot(access_token  = access_token,
                                                              date          = date,
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
  suppressWarnings(rmarkdown::render(input       = paste0(getwd(), "/Dashboards/RiskSnapShot/dashboard_risk_snapshot.Rmd"),
                                     output_file = paste0(getwd(), "/Output/", output_file_name, ".html"),
                                     params      = list(workflow_object = out_workflow_risk_snapshot),
                                     quiet       = TRUE))

  return(paste0("Report generated: ", getwd(), "/Dashboards/RiskSnapShot/Output/", output_file_name, ".html"))
}

