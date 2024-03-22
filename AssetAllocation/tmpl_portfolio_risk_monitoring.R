#******************************************************************************
#*
#### MONITORING EXAMPLE - THE OPTIMAL PORTFOLIO 2023
#*
#******************************************************************************

#### API REQUEST ####
use_date <- Sys.Date() - 4
dt_input_weights <- dt_port_optimal_weights[,.(symbol, weight)]
dt_input_weights[, date := use_date]

lst_port_quantities <- EnvisionRiskRaaS::envrsk_workflow_weight_2_quantities(
  dt_input_weights,
  100000000,
  base_cur,
  FALSE,
  use_date)
dt_port_optimal_quantities <- lst_port_quantities[["Output"]][["PortfolioQuantites"]]

dt_port_quantities <- merge(
  dt_port_optimal_weights,
  dt_port_optimal_quantities[,.(symbol, quantity)],
  by = "symbol",
  all.x = TRUE)

# Calculate Risk as point-in-time, 1-day 97.5% Expected-shortfall
dt_demo_port_risk <- aggregate_and_combine_port_risk(
  p_positions     = dt_port_quantities,
  p_date          = use_date,
  p_volatility_id = "point_in_time",
  p_signif_level  = rsk_conf_level,
  p_horizon       = 1,
  p_base_cur      = base_cur)

# Create Tree Structure and Format the Tree
demo_port_risk_tree   <- create_tree_output(dt_demo_port_risk, use_date)

#### Define the Output Elements ####
do_print_tree <- function(){
  print(demo_port_risk_tree, 
        #"Position Type"     = "position_type", 
        #"Instrument Name"   = "label", 
        #"Quantity"          = "quantity",
        "Exposure"          = "Notational",
        "Exposure %"        = "percent",
        "Market Value"      = "MarketValue",
        "Impact Risk"       = "Impact_Risk_pct",
        "Risk"              = "Risk",
        "Risk %"            = "risk_pct",
        "Diversification %" = "diversification_pct",
        "Hypothetical PnL"  = "HypotheticalPnL", 
        filterFun = function(x) !x$isLeaf)
}

#### PRESENT THE RESULT ####
do_print_tree()

dt_demo_port_risk
max_levels_included <- 3 #2

# Add top/bottom lines to all levels < max_levels_included and make the rows bold
dt_tbl_port <- copy(dt_demo_port_risk)
splt_str <- stringr::str_split(dt_tbl_port$pathString, "/")
level_port_tmp <- lapply(splt_str, function(x){
  (length(x) - 1)
})
level_port <- do.call(c, level_port_tmp)
dt_tbl_port <- cbind(dt_tbl_port, level_port)

# Format 
dt_tbl_port[, port_name_short := as.character(sapply(pathString, get_last_element, splitter = "/", simplify = TRUE))]
dt_tbl_port[, indent_port_name := level_port * 1]
dt_tbl_port[, has_lines := ifelse(level_port < (max_levels_included - 1), TRUE, FALSE)]
dt_tbl_port[, is_bold := ifelse(has_lines, TRUE, FALSE)]

chr_port_names <- ifelse(level_port > 0, paste0("$\\hookrightarrow$ ", dt_tbl_port$port_name_short), dt_tbl_port$port_name_short)
dt_tbl_port[, port_name_short_formated := chr_port_names]

dt_4_print     <- dt_tbl_port[level_port < max_levels_included]
dt_4_print_red <- dt_4_print[,.("Portfolio" = port_name_short_formated, 
                                "Exposure" = scales::number(
                                  Notational, 
                                  accuracy = 1,
                                  big.mark = ","), 
                                "Exposure (%)" = scales::percent(
                                  percent,
                                  accuracy = 0.1),
                                "Market Value" = scales::number(
                                  MarketValue,
                                  accuracy = 1,
                                  big.mark = ","),
                                "Impact Risk (%)" = scales::percent(
                                  Impact_Risk_pct,
                                  accuracy = 0.1), 
                                "Risk" = scales::number(
                                  Risk,
                                  accuracy = 1,
                                  big.mark = ","), 
                                "Risk (%)" = scales::percent(
                                  risk_pct,
                                  accuracy = 0.1), 
                                "Diversification (%)" = scales::percent(
                                  diversification_pct,
                                  accuracy = 1), 
                                "Hypothetical PnL" = scales::number(
                                  HypotheticalPnL,
                                  accuracy = 1,
                                  big.mark = ","))]


names(dt_4_print_red)[5] <- paste0(names(dt_4_print_red)[5], footnote_marker_symbol(1))
#names(dt_4_print_red)[8] <- paste0(names(dt_4_print_red)[8], footnote_marker_symbol(3))
names(dt_4_print_red)[9] <- paste0(names(dt_4_print_red)[9], footnote_marker_symbol(2))

tbl_risk_performance <- kableExtra::kbl(
  caption = paste0("Risk & Performance Monitoring (", use_date, " EoD)"),
  dt_4_print_red, 
  booktabs = T,
  format = "html",
  align=c('l', rep('r', times=8)),
  escape = FALSE) %>%
  #column_spec(1, width = "10cm") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_indent(positions = which(dt_4_print$level_port == 1), level_of_indent = 1) %>%
  add_indent(positions = which(dt_4_print$level_port == 2), level_of_indent = 2, ) %>%
  row_spec(row = 0, color = "white", "background" = "#27647B", bold = TRUE, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(row = 1, color = "white", "background" = "black", bold = TRUE, extra_css = "border-bottom: 1px solid; border-top: 1px solid") %>%
  row_spec(row = which(dt_4_print$level_port == 1), color = "black", bold = TRUE, extra_css = "border-bottom: 1px solid; border-top: 1px solid") %>%
  footnote(general = paste0("Every amount is denominated in ", base_cur, ", inclusive of currency risks (for assets not denominated in ", base_cur, "), and pertains to the 'point_in_time' volatility scenario. The metrics 'Risk', 'Risk (%)', and 'Risk Impact (%)' are all derived from a ", round(rsk_conf_level*100, 1), "% Expected Shortfall over a 1-day horizon."),
           symbol = c("In the context of a multi-position portfolio, the 'Impact Risk (%)' refers to the risk-contribution of each sub-portfolio to the overall portfolio risk.",                       
                      "'Hypothetical PnL' represent the profit or loss if a portfolio or position remains static for one business day, with no intraday trades included.")) %>% 
  kable_styling(full_width = F, font_size = 10) #%>%
#save_kable(file = "table_2.png", zoom = 1.5)

