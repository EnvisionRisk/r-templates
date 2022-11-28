envrsk_backtest_dashboard_p_value_interpretation_table <- function(){
  colour_tmplt <- c("red3", "#FF0000", "#FF5500", "#FFAA00", "#FFFF00", "#FFFF80", "#378805")
  p_value_factor <- factor(c("p-value >= 0.1",
                             "p-value < 0.1",
                             "p-value < 0.05",
                             "p-value < 0.02",
                             "p-value < 0.01",
                             "p-value < 0.005",
                             "p-value < 0.001"), ordered = TRUE)

  dt_p_value_factor <- data.table("Interpretation" = p_value_factor)

  return(
    dt_p_value_factor %>%
      gt()  %>%
      tab_options(
        heading.subtitle.font.size = 12,
        heading.align = "left",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width= px(3),
        row_group.background.color = "grey",
        table.background.color = "#d5e4eb"
      ) %>%
      tab_header(
        title = md("**Colour Schema**"),
        subtitle = html("<em>p-value</em>")
      ) %>%
      data_color( # Update cell colors...
        columns = c("Interpretation"),
        colors = scales::col_factor(
          domain = p_value_factor,
          palette = colour_tmplt,
          na.color = "#808080")
      ) %>%
      tab_source_note(
        source_note = md("Reference: Basel Committee on Banking Supervision (2019)
                         *Minimum capital requirements for market risk*,
                         'MAR99 Guidance on use of the internal models approach', table 2 (p. 128).
                         The colour schema is an **ENVISIONRISK** adoptation
                         of the chosen cutoff values in the BIS paper.")
      )
  )
}

envrsk_backtest_dashboard_summary_risk <- function(backtest_output){
  p_value_factor <- factor(c("p-value >= 0.1",
                             "p-value < 0.1",
                             "p-value < 0.05",
                             "p-value < 0.02",
                             "p-value < 0.01",
                             "p-value < 0.005",
                             "p-value < 0.001"), ordered = TRUE)
  colour_tmplt <- c("red3", "#FF0000", "#FF5500", "#FFAA00", "#FFFF00", "#FFFF80", "#378805")
  
  setnames(backtest_output, "Value-at-Risk-p-value", "Value-at-Risk")
  setnames(backtest_output, "Expected-Shortfall-p-value", "Expected-Shortfall")
  setnames(backtest_output, "Expected-Shortfall-Test-Statistic", "ES-Test-Stat")
  
  return(
    backtest_output %>%
      gt(groupname_col = "Type")  %>%
      fmt_number(columns = c("#Obs"), decimals = 0, sep_mark = ",") %>%
      cols_align(
        align = "right",
        columns = c("Value-at-Risk", "Expected-Shortfall")
      ) %>%
      tab_spanner(
        label = "Test for Accuracy (p-value)",
        columns = c("Value-at-Risk", "Expected-Shortfall")
      ) %>%
      tab_options(
        heading.subtitle.font.size = 12,
        heading.align = "left",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width= px(3),
        row_group.background.color = "grey",
        table.background.color = "#d5e4eb"
      ) %>%
      tab_header(
        title = md("**Backtest Results**"),
        subtitle = html("<em>Value-at-Risk & Expected Shortfall</em>")
      ) %>%
      tab_footnote(
        footnote = "The p-value tells you how likely it is to observe a worse outcome under the null hypothesis of an accurate model.",
        locations = cells_column_labels(
          columns = 5:6 # note
        )
      ) %>%
      data_color( # Update cell colors...
        columns = c("Value-at-Risk"), # ...for Mass column
        colors = scales::col_factor(
          domain = p_value_factor,
          palette = colour_tmplt,
          na.color = "#808080")
      ) %>%
      data_color( # Update cell colors...
        columns = c("Expected-Shortfall"), # ...for Mass column
        colors = scales::col_factor(
          domain = p_value_factor,
          palette = colour_tmplt,
          na.color = "#808080")
      ) %>%
      tab_source_note(
        source_note = md("Reference: Carlo Acerbi & Balazs Szekely (2014) *BACKTESTING EXPECTED SHORTFALL*, MSCI. The unconditional test statistic (test 2) is used here")
      )
  )
}

#envrsk_backtest_dashboard_p_value_interpretation_table()
#envrsk_backtest_dashboard_summary_risk(backtest_out)

#### MAIN GRAPH ####
envrsk_backtest_dashboard_ts <- function(dt_backtest, base_cur){
  color_classic_trustworthy <- c("#CA3542", "#27647B", "#849FAD", "#AEC0C9", "#57575F")

  dt_out_graph <- data.table::copy(dt_backtest)
  dt_out_graph[, IS_VaR_EVENT := pnl < var]
  dt_out_graph[IS_VaR_EVENT != TRUE, VaR_EVENT := NA_real_]
  dt_out_graph[IS_VaR_EVENT == TRUE, VaR_EVENT := pnl]
  dt_out_graph[, IS_VaR_EVENT := NULL]
  dt_out_graph[, date := as.Date(date)]
  
  dt_out_melt <- melt.data.table(dt_out_graph, id.vars = "date")
  dt_out_melt[variable == "var", variable := "Value-at-Risk"]
  dt_out_melt[variable == "es", variable := "Expected-Shortfall"]
  dt_out_melt[variable == "pnl", variable := "Profit/Loss"]
  p_backtest_ts <- ggplot(dt_out_melt[variable %in% c("Value-at-Risk", "Expected-Shortfall", "Profit/Loss")], aes(x = date, y = value, colour = variable))+
    geom_line()+
    geom_point(data = dt_out_melt[variable %in% c("VaR_EVENT")], aes(x = date, y = value), colour = color_classic_trustworthy[1])+
    ggthemes::scale_color_economist("")+
    #scale_color_manual("", values = color_classic_trustworthy[c(2, 3, 4)])+
    ggthemes::theme_economist()+
    #theme_bw()+
    scale_y_continuous(name=paste0("Profit/Loss (in ", base_cur, ")"), labels = scales::comma_format(big.mark = ".",
                                                                                                     decimal.mark = ","))
  return(p_backtest_ts)
}
