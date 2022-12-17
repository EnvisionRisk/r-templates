usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.tree")

# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.tree)
library(data.table)

#******************************************************************************
#*
#### HELPERS #### 
#*
#******************************************************************************
do_remove_last <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- head(splt_str, -1)
  if(length(splt_str) > 1){
    splt_str <- paste(splt_str, collapse = "//")  
  }
  
  return(splt_str)
}

do_remove_first <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- tail(splt_str, -1)
  if(length(splt_str) > 1){
    splt_str <- paste(splt_str, collapse = "//")  
  }
  
  return(splt_str)
}

get_last_element <- function(str_in, splitter = "//"){
  splt_str <- stringr::str_split(str_in, splitter)[[1]]  
  splt_str <- tail(splt_str, 1)
  
  return(splt_str)
}

DataTreeToJson <- function(data_tree, splitter = "//"){
  stopifnot(c("position_type", "quantity") %in% data_tree$attributesAll)
  x <- data.table(ToDataFrameTable(data_tree, "pathString", "position_type", "quantity"))
  x[, location := gsub("/", "//", pathString)]
  x[, symbol := mapply(get_last_element, location, "//")]
  x[, location := mapply(do_remove_first, location, "//")]
  x[, location := mapply(do_remove_last, location, "//")]
  x[, pathString := NULL]
  return(jsonlite::toJSON(x, pretty = TRUE))
}

format_portfolio_risk <- function(envrsk_portfolio_out){
  # Merge 'Output' and 'Positions_Mapped' returned from the API 
  my_port_risk <- merge(
    envrsk_portfolio_out[["Output"]][,.(UID, Location, VaR, ES)],
    envrsk_portfolio_out[["Positions_Mapped"]][,.(uid, position_type, symbol, "label" = name, quantity)],
    by.x = "UID", by.y = "uid",
    all.x = TRUE)
  
  # Remove unwanted columns and format the remaining columns
  my_port_risk[, UID := NULL]
  my_port_risk[!is.na(symbol), pathString := mapply(do_remove_last, Location, "//")]
  my_port_risk[is.na(symbol), pathString := Location]
  my_port_risk[!is.na(symbol), pathString := paste0(pathString, "//", symbol)]
  my_port_risk[, pathString := gsub("//", "/", pathString)]
  my_port_risk[, ":=" (Location = NULL, symbol = NULL)]
  return(my_port_risk)
}

format_portfolio_perf <- function(envrsk_portfolio_out){
  # Merge 'Output' and 'Positions_Mapped' returned from the API 
  my_port_risk <- merge(
    envrsk_portfolio_out[["Output"]][,.(UID, Location, Notational, MarketValue, HypotheticalPnL)],
    envrsk_portfolio_out[["Positions_Mapped"]][,.(uid, position_type, symbol, "label" = name, quantity)],
    by.x = "UID", by.y = "uid",
    all.x = TRUE)
  
  # Remove unwanted columns and format the remaining columns
  my_port_risk[, UID := NULL]
  my_port_risk[!is.na(symbol), pathString := mapply(do_remove_last, Location, "//")]
  my_port_risk[is.na(symbol), pathString := Location]
  my_port_risk[!is.na(symbol), pathString := paste0(pathString, "//", symbol)]
  my_port_risk[, pathString := gsub("//", "/", pathString)]
  my_port_risk[, ":=" (Location = NULL, symbol = NULL)]
  return(my_port_risk)
}
