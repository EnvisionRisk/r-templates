# In case one or more of the packages are not installed they will be installed
pckg <- c("data.tree")
sapply(pckg, usePackage)

library(data.tree)
library(data.table)

#******************************************************************************
#*
#### Aggregate & Combine #### 
#*
#******************************************************************************
aggregate_and_combine_port_risk <- function(p_positions, p_date, p_volatility_id, p_signif_level, p_horizon, p_base_cur, rsk_msr = "ES"){
  demo_port_risk_out_reg <- EnvisionRiskRaaS::envrsk_portfolio_risk_regular( 
    positions     = p_positions,
    date          = p_date,
    volatility_id = p_volatility_id,
    signif_level  = p_signif_level,
    horizon       = p_horizon,
    base_cur      = p_base_cur)
  
  # Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
  demo_port_risk_out_cmp <- EnvisionRiskRaaS::envrsk_portfolio_risk_component( 
    positions     = p_positions,
    date          = p_date,
    volatility_id = p_volatility_id,
    signif_level  = p_signif_level,
    horizon       = p_horizon,
    base_cur      = p_base_cur)
  
  # Retain the data we need from API call.
  dt_demo_port_risk_reg <- format_portfolio_risk(demo_port_risk_out_reg)
  dt_demo_port_risk_cmp <- format_portfolio_risk(demo_port_risk_out_cmp)
  
  dt_demo_port_risk <- merge(
    dt_demo_port_risk_reg[,.(pathString, position_type, label, quantity, "Value_at_Risk" = VaR, "Expected_Shortfall" = ES)],
    dt_demo_port_risk_cmp[,.(pathString, "Impact_Value_at_Risk" = VaR, "Impact_Expected_Shortfall" = ES)],
    by = "pathString")
  
  # Calculate Market-value for the portfolio
  demo_port_perf_out <- EnvisionRiskRaaS::envrsk_portfolio_hypothetical_performance(
    positions     = p_positions,
    date          = p_date,
    base_cur      = p_base_cur)
  
  # Retain the data we need from API call.
  dt_demo_port_perf <- format_portfolio_perf(demo_port_perf_out)
  
  # Merge the data from the 3 API calls
  dt_demo_port_risk <- merge(
    dt_demo_port_risk,
    dt_demo_port_perf[,.(pathString, Notational, MarketValue, HypotheticalPnL)],
    by = "pathString")
  
  # Retain the data we need from API call.
  dt_demo_port_perf <- format_portfolio_perf(demo_port_perf_out)
  
  if(rsk_msr == "VaR"){
    dt_demo_port_risk <- 
      dt_demo_port_risk[, .(pathString, position_type, label, quantity, 
                            Value_at_Risk, Impact_Value_at_Risk, 
                            Notational, MarketValue, 
                            HypotheticalPnL)]
    
    setnames(dt_demo_port_risk, "Value_at_Risk", "Risk")
    setnames(dt_demo_port_risk, "Impact_Value_at_Risk", "Impact_Risk")
    
    
  } else {
    dt_demo_port_risk <- 
      dt_demo_port_risk[, .(pathString, position_type, label, quantity, 
                            Expected_Shortfall, 
                            Impact_Expected_Shortfall, Notational, MarketValue, 
                            HypotheticalPnL)]
    setnames(dt_demo_port_risk, "Expected_Shortfall", "Risk")
    setnames(dt_demo_port_risk, "Impact_Expected_Shortfall", "Impact_Risk")
  }
  s_risk_reg   <- sum(dt_demo_port_risk[!is.na(quantity)][["Risk"]])
  s_risk_cmp   <- sum(dt_demo_port_risk[!is.na(quantity)][["Impact_Risk"]])
  s_notational <- sum(dt_demo_port_risk[!is.na(quantity)][["Notational"]])
  
  dt_demo_port_risk[, percent := Notational / s_notational]
  dt_demo_port_risk[, diversification := Risk - Impact_Risk ]
  dt_demo_port_risk[, diversification_pct := diversification / Risk]
  dt_demo_port_risk[, risk_pct := abs(Risk / MarketValue)]
  
  dt_demo_port_risk[1, ":=" (diversification = s_risk_reg - s_risk_cmp, 
                             diversification_pct = (s_risk_reg - s_risk_cmp) / s_risk_reg)]
  dt_demo_port_risk[, Impact_Risk_pct := Impact_Risk / s_risk_cmp]
  return(dt_demo_port_risk)
}

create_tree_output <- function(p_dt_port, p_date){
  # p_dt_port <- dt_demo_port_risk
  # p_date <- demo_date
  dt_port <- copy(p_dt_port)
  lst_path_strings_raw <- strsplit(dt_port$pathString, split = "/")
  lst_path_strings_tmp <- lapply(lst_path_strings_raw, function(x){
    x[1] <- as.character(p_date)
    paste(x, collapse = "/")
  })
  lst_path_strings <- do.call(c, lst_path_strings_tmp)
  dt_port[, pathString := lst_path_strings]
  
  #### FORMAT THE RESPONSE ####
  # Transform the portfolio to a data.tree object (more pleasing for the eye)
  tree_out <- data.tree::as.Node(dt_port)
  
  # Sort the data-tree by Stress-test
  data.tree::Sort(tree_out, "position_type", decreasing = TRUE)
  
  # Format the data-tree column 'quantity' and 'ES'
  data.tree::SetFormat(tree_out, "quantity",            formatFun = function(x) {if(is.na(x)){""} else {format(round(x, 0), nsmall=0, big.mark=",")}})
  data.tree::SetFormat(tree_out, "Risk",                formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "Impact_Risk",         formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "Notational",          formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "MarketValue",         formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "HypotheticalPnL",     formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "percent",             formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "diversification",     formatFun = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))
  data.tree::SetFormat(tree_out, "diversification_pct", formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "risk_pct",            formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  data.tree::SetFormat(tree_out, "Impact_Risk_pct",     formatFun = function(x) FormatPercent(as.numeric(x), digits = 1))
  
  # print(tree_out, 
  #       "Position Type"     = "position_type", 
  #       "Instrument Name"   = "label", 
  #       "Quantity"          = "quantity",
  #       "Percent"           = "percent",
  #       "Exposure"          = "Notational",
  #       "Market Value"      = "MarketValue",
  #       "Impact Risk"       = "Impact_Risk_pct",
  #       "Risk"              = "Risk",
  #       "Risk %"            = "risk_pct",
  #       "Diversification"   = "diversification",
  #       "Diversification %" = "diversification_pct",
  #       "Hypothetical PnL"  = "HypotheticalPnL"
  # )
  return(tree_out)
}

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
