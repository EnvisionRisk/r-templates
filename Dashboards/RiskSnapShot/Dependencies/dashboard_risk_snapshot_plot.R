choose_number_format <- function(x)
{
  intl     <- c(0, 1e3, 1e6, 1e9, 1e12)
  suffixes <- c(NA_character_, 'K', 'M', 'B', 'T')
  index   <- findInterval(abs(x), intl)
  return(suffixes[index])
}

get_color_classic_trustworthy <- function(){
  color_classic_trustworthy <- c("#CA3542", "#27647B", "#849FAD", "#AEC0C9", "#57575F")
  return(color_classic_trustworthy)
}
# scales::show_col(color_classic_trustworthy)

createValueBoxes <- function(df, h = 4, w = 6, padding=0.5, rows = 2){
  # verify our inputs
  if (!is.data.frame(df)) {
    stop(paste("Argument", deparse(substitute(df)), "must be a data.frame."))
  }
  if(!all(i <- rlang::has_name(df,c("values", "infos", "icons")))){
    stop(sprintf(
      "%s does not contain: %s",
      deparse(substitute(df)),
      paste(columns[!i], collapse=", ")))
  }

  boxes = nrow(df) # number of items passed
  # calculate the grid
  cols = boxes/rows
  plotdf <- data.frame(
    x = rep(seq(0, (w+padding)*cols-1, w+padding), times=rows),
    y = rep(seq(0, (h+padding)*rows-1, h+padding), each=cols),
    h = rep(h, boxes),
    w = rep(w, boxes),
    value = df$values,
    info = df$infos,
    icon = emojifont::fontawesome(df$icons),
    font_family = c(rep("fontawesome-webfont", boxes)),
    color = factor(get_color_classic_trustworthy()[1:boxes])
  )

  ggplot(plotdf, aes(x, y, height = h, width = w, label = info)) +
    ## Create the tiles using the `color` column
    geom_tile(aes(fill = color)) +
    ## Add the numeric values as text in `value` column
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - w/2.2, y = y + h/4), hjust = 0) +
    ## Add the labels for each box stored in the `info` column
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - w/2.2, y = y-h/4), hjust = 0) +
    coord_fixed() +
    scale_fill_manual(values = color_classic_trustworthy[1:boxes],
                      breaks = as.character(get_color_classic_trustworthy()[1:boxes])) +
    ## Use `geom_text()` to add the icons by specifying the unicode symbol.
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + w/4, y = y + h/8), alpha = 0.25) +
    theme_void() +
    guides(fill = "none")

}

####***********************************************************************************************************************************
####*
####* PnL HISTOGRAM ####
####*
####***********************************************************************************************************************************
envrsk_plot_pnl <- function(dt_pnl_dist,
                            rsk_measure,
                            base_cur){
  
  #### SCALING ####
  bin_width <- abs(2*rsk_measure)*3/100
  d <- data.table::data.table(PnL   = rsk_measure,
                              event = c(paste0("Risk (", 
                                               scales::number(rsk_measure, 
                                                              decimal.mark = ",", 
                                                              big.mark = ".", 
                                                              accuracy = 1),
                                               " ", 
                                               base_cur,
                                               ")"
                                              )
                                        )
  )
  
  p_hist <- ggplot(dt_pnl_dist, aes(x = PnL)) +
    geom_histogram(aes(y=..density..), 
                   color="black", 
                   fill = get_color_classic_trustworthy()[4], 
                   binwidth = bin_width, 
                   alpha = 0.75) +
    theme_bw()+
    geom_vline(data = d, 
               mapping=aes(xintercept = PnL), 
               color=get_color_classic_trustworthy()[1], 
               linetype = "dashed") +
    geom_text(data = d, 
              mapping=aes(x = PnL, y=0, label = event), 
              size=4, 
              angle=90, 
              vjust=-0.4, 
              hjust=-0.5, 
              colour = get_color_classic_trustworthy()[5]) +
    ylab("Freq")+
    xlab(paste0("P&L (in ", base_cur, ")")) +
    scale_x_continuous(labels = scales::number_format(decimal.mark = ",", 
                                                      big.mark = ".", 
                                                      accuracy = 1),
                       limits = c(rsk_measure * 2, -1 * rsk_measure * 2))+
    theme(axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  return(p_hist)
}

aggr_dt_risk <- function(dt_risk, top_symbols = 10){ 
  dt_risk[, quantity := scales::number(as.numeric(quantity), decimal.mark = ",", big.mark = ".", accuracy = 1)]

  top_symbols <- min(nrow(dt_risk), top_symbols)
  data.table::setorder(dt_risk, "component")
  n_rows        <- nrow(dt_risk)
  total_row     <- apply(dt_risk[, -1:-4], 2, sum)
  the_rest_row  <- NULL
  dt_the_rest   <- NULL
  if(n_rows > top_symbols){
    the_rest_row  <- apply(dt_risk[(top_symbols + 1):n_rows, -1:-4], 2, sum)
    dt_the_rest   <- cbind(data.table::data.table("uid"      = "",
                                      "symbol"   = "other",
                                      "name"     = "",
                                      "quantity" = ""), data.table(t(the_rest_row)))
  }

  dt_total      <- cbind(data.table::data.table("uid"    = "",
                                    "symbol" = "all",
                                    "name"   = "",
                                    "quantity" = ""), data.table::data.table(t(total_row)))

  dt_table_risk <- rbind(dt_risk[1:min(top_symbols, n_rows)],
                         dt_the_rest,
                         dt_total)
  dt_table_risk[, weight := notational / dt_total$notational]
  dt_table_risk[, diversification := regular - component]
  dt_table_risk[, diff_pct := diversification / regular]

  return(dt_table_risk)
}

envrsk_summary_risk <- function(dt_risk, base_cur){ 
  my_font_size <- 15
  dt_risk_decorated <- aggr_dt_risk(dt_risk)
  dt_risk_decorated <- dt_risk_decorated[,.(uid, symbol, name, quantity, notational, weight, market_value, regular, component, diversification, diff_pct)]

  dt_risk_decorated[, ":=" (notational      = scales::number(notational, decimal.mark = ",", big.mark = ".", accuracy = 1),
                            market_value    = scales::number(market_value, decimal.mark = ",", big.mark = ".", accuracy = 1),
                            regular         = scales::number(regular, decimal.mark = ",", big.mark = ".", accuracy = 1),
                            component       = scales::number(component, decimal.mark = ",", big.mark = ".", accuracy = 1),
                            diversification = scales::number(diversification, decimal.mark = ",", big.mark = ".", accuracy = 1),
                            diff_pct        = scales::percent(diff_pct, accuracy = 1.0),
                            weight          = scales::percent(weight, accuracy = 1.0))]

  my_col_names <- c("uid", "symbol", "name", "#",
                    toupper(base_cur), "%", #notational
                    toupper(base_cur), #market_value
                    "regular", "component",
                    toupper(base_cur), "%")
  out_table <- kableExtra::kbl(dt_risk_decorated, align = c('c', 'c', 'c', 'r', 'r', 'r', 'r', 'r', 'r', 'r'), col.names = my_col_names, "html") %>%
    kable_classic(full_width = F, html_font = "Cambria", font_size = my_font_size) %>%
    add_header_above(c("position" = 4, "notational" = 2, "market value" = 1, "risk" = 2, "diversification" = 2),
                     background = get_color_classic_trustworthy()[5], color = "white", include_empty = F,
                     bold = TRUE, font_size = my_font_size) %>%
    row_spec(0, background = get_color_classic_trustworthy()[5], color = "white", bold = TRUE, font_size = my_font_size) %>%
    kableExtra::row_spec(seq(1, nrow(dt_risk_decorated), 2), background=get_color_classic_trustworthy()[4]) %>%
    kableExtra::row_spec(nrow(dt_risk_decorated) - 1, extra_css = "border-bottom: 1px solid") %>%
    kableExtra::row_spec(nrow(dt_risk_decorated), bold = TRUE, font_size = my_font_size)
  return(out_table)
}

#***********************************************************************************************************************************
#*
#### Portfolio Currency Denominated Risk ####
#*
#***********************************************************************************************************************************
envrsk_plot_currency_denominated_risk <- function(dt_risk, base_cur){
  dt_treemap <- dt_risk[,.(qc, notational, component)]
  dt_treemap <- dt_treemap[, .(notational = sum(notational), component = sum(component)), by = qc]

  p_cur_risk <- ggplot(dt_treemap, aes(area = notational, fill = component,
                                       label = qc)) +
    geom_treemap() +
    geom_treemap_text(colour = "black", place = "center",
                      size = 15, grow = FALSE)+
    scale_fill_gradient2("Risk",
                         low      = get_color_classic_trustworthy()[3],
                         mid      = "white",
                         high     = get_color_classic_trustworthy()[1],
                         midpoint = 0.0,
                         label    = scales::number_format(decimal.mark = ",", big.mark = ".", accuracy = 1))+
    theme_bw()

  return(p_cur_risk)
}
#***********************************************************************************************************************************
#*
#### DIVERSIFICATION FIGURE ####
#*
#***********************************************************************************************************************************
envrsk_plot_port_asset_diversification <- function(dt_risk, base_cur){
  
  dt_diversification <- aggr_dt_risk(dt_risk)
  dt_diversification <- dt_diversification[symbol != "all"]
  dt_diversification <- dt_diversification[symbol != "other"]

  dt_uid_symbol <- dt_diversification[, .(uid, symbol)]
  dt_diversification[, symbol := NULL]

  dt_diversification <- dt_diversification[,.(uid, component, regular, diversification)]
  dt_diversification_melt <- data.table::melt.data.table(dt_diversification, id.vars = c("uid", "regular"))
  dt_diversification_melt[, variable := factor(variable, levels = c("diversification", "component"), labels = c(c("Diversification", "Component Risk")))]
  dt_diversification_melt <- merge(dt_diversification_melt, dt_uid_symbol, by = "uid", all.x = TRUE)
  dt_diversification_melt[, uid := NULL]

  p_diversification <- ggplot(data = dt_diversification_melt,
                              aes(x = reorder(symbol, -regular),
                                  y = value,
                                  fill = variable,
                                  order = variable)) +
    geom_bar(stat="identity")+
    scale_fill_manual("",
                      values = c(get_color_classic_trustworthy()[2],
                                 get_color_classic_trustworthy()[4])) +
    scale_y_continuous(labels = scales::number_format(decimal.mark = ",", 
                                                      big.mark = ".", 
                                                      accuracy = 1))+
    coord_flip()+
    theme_bw()+
    theme(legend.position = "top") +
    ylab(paste0("Risk (in ", toupper(base_cur), ")"))+
    xlab('Symbol')  
    
    #labs(title = "Diversification Effects (top 10)")
  return(p_diversification)
}
