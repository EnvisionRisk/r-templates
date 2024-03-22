#### DEPENDENCIES ####
# *** Common dependencies ***
source("init_r_templates.R")

# *** Specific dependencies ***
source("Demo/ForwardPrediction/Dependencies/forward_risk_assesment_dependencies.R")

#### SETTINGS ####
base_cur             <- "USD"
confidence_level_pit <- 0.95
confidence_level_dt  <- 0.95
horizon_pit          <- 1
horizon_dt           <- 1

#### Define Portfolio & Date of Analysis ####
# use_symbs        <- c("EEM.US", "VNQ.US", "MDY.US", "XLP.US", "SPY.US", "EFA.US", "TIP.US", "AGG.US", "DJP.US", "SHV.US", "USDEUR")
# use_symbs_labels <- c("Emerging Markets", "REITs", "Mid Cap", "Consumer Staples", "Large Cap", "Int'l Stocks", "TIPS", "Bonds", "Comdty", "Cash", "FX_USD")
use_symbs        <- c("EURUSD", "CHFUSD", "CADUSD", "EURPLN", "SGDUSD", "SEKUSD")
use_symbs_labels <- c("EUR/USD", "CHF/USD", "CAD/USD", "EUR/PLN", "SGD/USD", "SEK/USD")
#use_symbs        <- c("SPY.US")
#use_symbs_labels <- c("Large Cap")
use_date         <- Sys.Date()-2

#### Risk Overview table ####
dt_out_risk_table <- fkt_process(
  use_date, base_cur, use_symbs, use_symbs_labels, 
  horizon_pit, horizon_dt, 
  confidence_level_pit, confidence_level_dt)

#### Risk Monitoring Graph ####
dt_out_forward_pred <- get_risk_assesment_daily(
  use_date, base_cur, use_symbs, use_symbs_labels,
  confidence_level_pit, confidence_level_dt)

#### OUTPUT OVERVIEW TABLE ####
dt_out_risk_table

#### OUTPUT MONITORING PLOT ####
market_cockpit <- create_market_cockpit(dt_out_forward_pred, base_cur, prediction_date = use_date)
print(market_cockpit)
grid::grid.raster(logo, x = 0.97, y = 0.03, just = c('right', 'bottom'), width = unit(1, 'inch'))

#### Animate ####
use_dates <- unique(dt_out_forward_pred$date[between(dt_out_forward_pred$date, as.Date("2024-01-01"), as.Date("2024-02-28"))])
lst_risk_pred_animate <- pbapply::pblapply(use_dates, function(x){
  dt_out_forward_pred_animate <- get_risk_assesment_daily(
    x, base_cur, use_symbs, use_symbs_labels,
    confidence_level_pit, confidence_level_dt)
  dt_out_forward_pred_animate[, pred_date := x]
})
#saveRDS(lst_risk_pred_animate, "risk4_animation.rds")
lst_risk_pred_animate <- readRDS("risk4_animation.rds")

dt_risk_pred_animate <- rbindlist(lst_risk_pred_animate)
market_cockpit <- create_market_cockpit(dt_risk_pred_animate, base_cur, prediction_date = "")
print(market_cockpit)
##---------------------------------------------------------------
##  Transition through distinct states in time                 --
##---------------------------------------------------------------
graph1_animation = market_cockpit +
  transition_time(pred_date) +
  labs(subtitle = "Date: {frame_time}")
graph1_animation
grid::grid.raster(logo, x = 0.97, y = 0.03, just = c('right', 'bottom'), width = unit(1, 'inch'))


#### TESTER ####
##----------------------------------------------------------------
##  Load the library                                            --
##----------------------------------------------------------------
library(tidyverse)    # data wrangling    
library(gganimate)    # data visualization
library(ggthemes)     # set plot theme
library(gapminder)    # the dataset

##---------------------------------------------------------------
##  EDA                                                        --
##---------------------------------------------------------------
data <- gapminder
glimpse(data)

##---------------------------------------------------------------
##  Creating basic static plot                                 --
##---------------------------------------------------------------
# Make a ggplot, but add frame=year: one image per year
graph1 <- gapminder %>% 
  # plot the data with scatterplots
  ggplot(aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  # axis transformations 
  scale_x_log10() +
  theme_bw()
graph1

##---------------------------------------------------------------
##  Transition through distinct states in time                 --
##---------------------------------------------------------------
graph1_animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}")
graph1_animation

##----------------------------------------------------------------
##  Display preceding frames with a gradual decrease in size    --
##----------------------------------------------------------------
graph1_animation <- graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
graph1_animation

##---------------------------------------------------------------
##  Make the label more visible                                --
##---------------------------------------------------------------
graph1_animation_2 <- graph1 +
  geom_text(aes(x = min(gdpPercap), y = min(lifeExp), label = as.factor(year)) , 
            hjust=-1.5, vjust = -0.2, alpha = 0.2,  col = "gray", size = 20) +
  transition_states(as.factor(year), state_length = 0)
#display the graph
graph1_animation_2

##---------------------------------------------------------------
##                                                             --
##---------------------------------------------------------------
use_dates <- unique(dt_out_forward_pred$date[between(dt_out_forward_pred$date, as.Date("2024-01-01"), as.Date("2024-02-29"))])
price  <- EnvisionRiskRaaS::envrsk_market_price_raw(use_symbs)[["Output"]]
lst_es <- pbapply::pblapply(use_dates, function(x){
  price_date <- price[date <= x]
  price_date <- price_date[date == max(date), .(close), by = symbol]
  es_out <- EnvisionRiskRaaS::envrsk_instrument_expected_shortfall_raw(x, use_symbs, horizon = 30, signif_level = 0.95, volatility_id = "point_in_time")[["Output"]]
  dt_es <- merge(
    es_out,
    price_date,
    by = "symbol",
    all.x = TRUE)
  dt_es[, es_pct := expected_shortfall / close]
  dt_es[, ":=" (expected_shortfall = NULL, volatility_id = NULL)]
  dt_es[, date := x]
  dt_es
})
dt_expected_shortfall <- rbindlist(lst_es)

dt_expected_shortfall$symbol <- factor(dt_expected_shortfall$symbol, levels = use_symbs, labels = use_symbs_labels)
plot_es_temporal <- ggplot(dt_expected_shortfall, aes(x = date, y = es_pct*100, colour = symbol))+
  geom_line()+
  facet_wrap("symbol", ncol = 3)+
  theme_bw()+
  theme(legend.position = "top",
        plot.title = element_text(color = "#EC0108", face = 'bold'),
        plot.subtitle = element_text(color = "black"))+
  labs(title    = paste0("Expected Shortfall 95%, 30 days"),
       subtitle = '2020 - 2024',
       y        = paste("Expected Shortfall (in %)"),
       colour   = "") +
  coord_cartesian(clip = "off") 

print(plot_es_temporal)
grid::grid.raster(logo, x = 0.97, y = 0.03, just = c('right', 'bottom'), width = unit(1, 'inch'))

dt_vol_eurpln <- EnvisionRiskRaaS::envrsk_market_volatility("EURPLN")[["Output"]]
tail(dt_vol_eurpln, 50)