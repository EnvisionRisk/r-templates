#*****************************************************************************
#* 
#* EnvisionRisk, Market Risk-as-a-Service API, R-interface, 2023
#* 
#*****************************************************************************
#* Leverage your risk management with EnvisionRisk’s Risk-as-a-Service API to:
#*  - Be up and running with portfolio market risk analytics quickly – Value-at-Risk, 
#*    Expected-Shortfall, risk adjusted returns, stress tests, economic-capital, 
#*    cost-of-risk, delta-vectors.
#*  - Customize workflows for advanced risk assessments.
#*  - Back test your risk predictions to ensure that they are accurate.
#*  - Obtain historical time-series for all risk factors – prices and volatilities.
#*  
#* EnvisionRisk removes the pain – and reduces the costs – of managing a market 
#* risk prediction framework. Innovative, secure and easy-to-use REST API 
#* delivering risk analytics for portfolios of financial instruments.
#* 
#* 
#* 
#* Value at Risk (VaR) and Expected Shortfall (ES) are pivotal in market risk 
#* management as they estimate potential losses under different market conditions.
#* Corporations use Value at Risk (VaR) and Expected Shortfall (ES) to manage 
#* their market risk. Value at Risk (VaR) and Expected Shortfall (ES) are 
#* forward-looking predictions as they estimate potential future losses based 
#* on current portfolio compositions and statistical analysis of historical 
#* market data. 
#* 
#* They allow firms to quantify, monitor, and manage their risk 
#* exposure, thus reducing the likelihood of unexpected losses. These tools 
#* allow corporations to assess their risk exposure, informing strategic 
#* decision-making, and ensuring they hold enough capital to cover potential 
#* losses. They help in maintaining financial stability, managing investor 
#* expectations, and complying with regulatory requirements.
#*  
#* VaR provides a maximum potential loss over a set period at a given confidence 
#* level, offering an easily understandable risk measure. ES takes it further, 
#* offering insight into the potential losses in the most severe scenarios, 
#* capturing tail risk. These metrics allow firms to quantify, monitor, and 
#* manage their risk exposure, ensuring they maintain sufficient capital reserves 
#* and remain resilient during market volatility, thereby safeguarding their 
#* financial stability.
#*****************************************************************************
#### DEPENDENCIES ####
#*****************************************************************************
#*
#* DEPENDENCIES
#* 
#*****************************************************************************
source("C:/Users/jonas/Dropbox/Projects/templates/r-templates/envrsk_api_bridge_2_R.R")

#### AUTHENTICATE ####
#*****************************************************************************
#*
#* AUTHENTICATE with the cloud server
#* 
#*****************************************************************************
envrsk_auth_renew_access_token(force_renew = FALSE)
access_token <- my_access_token[["access-token"]]

#### AVAILABILITY OF INSTRUMENTS ####
#*****************************************************************************
#*
#* Get list of available instruments
#*
#*****************************************************************************
# Invoke the API 
dt_all_available_instr <- envrsk_instrument_search(access_token = access_token)

# Print
dt_all_available_instr

#### INSTRUMENT SERVICES ####
#*****************************************************************************
#*
#* INSTRUMENT LEVEL REQUESTS
#* 
#*****************************************************************************
# Examples: 
instr_symbs <- c("AAPL.US", "DANSKE.CO", "USDDKK")

#*****************************************************************************
#### Instrument Raw Prices - No currency conversion has been applied ####
#*****************************************************************************
# Invoke the API and request prices for the symbols you are interested in. There
# is a limit of 25 symbols pr request due to the amount of data. 
dt_raw_prices <- envrsk_market_price_raw(access_token = access_token, 
                                         symbols      = instr_symbs)
# Print 
dt_raw_prices[["Output"]]

# In case one or more of the requested instrument symbols are not available
dt_raw_prices[["symbols_unmapped"]]

#*****************************************************************************
#### Instrument Prices - Currency conversion has been applied ####
#*****************************************************************************
base_cur  <- "DKK"

# Invoke the API
dt_prices <- envrsk_market_price(access_token = access_token, 
                                 symbols      = instr_symbs,
                                 base_cur     = base_cur)
# Print 
dt_prices[["Output"]]

# In case one or more of the requested instrument symbols are not available
dt_prices[["symbols_unmapped"]]

#*****************************************************************************
#### Instruments Volatilities ####
#*****************************************************************************
# Invoke the API
dt_dynamic_volatilities <- envrsk_market_volatility(access_token = access_token, 
                                                    symbols      = instr_symbs)
dt_stress_volatilities  <- envrsk_market_stress_volatility(access_token = access_token, 
                                                           symbols      = instr_symbs)
# Print 
dt_dynamic_volatilities[["Output"]]
dt_stress_volatilities[["Output"]]

# In case one or more of the requested instrument symbols are not available
dt_dynamic_volatilities[["symbols_unmapped"]]
dt_stress_volatilities[["symbols_unmapped"]]

#******************************************************************************
#### Instruments Risk ####
#******************************************************************************
#* Value at Risk (VaR):
#* Value at Risk (VaR) is a statistical measure that quantifies the level of 
#* financial risk within a firm, portfolio, or position over a specific time 
#* frame. This metric is most commonly used by investment and commercial banks 
#* to determine the extent and occurrence rate of potential losses in their 
#* institutional portfolios.
#* 
#* VaR is denoted in either percentage or monetary terms and is defined under 
#* three variables: a time period (horizon), a confidence level (signif_level), 
#* and a loss amount or loss percentage. For example, if a portfolio of investments 
#* has a one-day VaR of 1 million at a 5% level, there is a 5% chance that the 
#* portfolio will fall in value by over 1 million over a one day period, assuming 
#* markets are normal and there is no trading.
#* 
#* Expected Shortfall (ES):
#* Expected shortfall (ES), also known as Conditional Value at Risk (CVaR), 
#* is a risk measure that looks at the tail risk of an investment. It is used 
#* to estimate the expected return of the portfolio in the worst-case scenario.
#* 
#* Unlike VaR, which provides a cut-off point and fails to describe the potential 
#* losses beyond that point, expected shortfall is an attempt to estimate what 
#* the average loss will be in the worst-case scenario.
#* 
#* Essentially, while VaR asks the question, "Given a certain confidence level, 
#* how bad can losses be?", ES asks, "Given that we are in the worst x% of cases, 
#* what is our average loss?"
#* 
#* Both VaR and ES are used in financial risk management for risk assessment, 
#* but ES is considered to provide more comprehensive information about the 
#* potential risk because it considers the tail risk.
#******************************************************************************
run_date             <- as.Date("2023-05-17")
base_cur             <- "DKK"
horizon              <- 1
volatility_id        <- "point_in_time"
signif_level         <- 0.975

#Value at Risk
dt_instr_var <- envrsk_instrument_value_at_risk(access_token  = access_token, 
                                                date          = run_date,
                                                symbols       = instr_symbs,
                                                base_cur      = base_cur,
                                                horizon       = horizon,
                                                signif_level  = signif_level, 
                                                volatility_id = volatility_id)
# Print 
dt_instr_var[["Output"]]

#Expected Shortfall
dt_instr_es <- envrsk_instrument_expected_shortfall(access_token  = access_token, 
                                                    date          = run_date,
                                                    symbols       = instr_symbs,
                                                    base_cur      = base_cur,
                                                    horizon       = horizon,
                                                    signif_level  = signif_level, 
                                                    volatility_id = volatility_id)
# Print
dt_instr_es[["Output"]]

#******************************************************************************
#### Instruments Delta Vectors ####
#******************************************************************************
#* Simulated stock price changes can be used to calculate Value at Risk (VaR) 
#* and Expected Shortfall (ES), two key measures in market risk management. Monte 
#* Carlo simulations are extensively used in the financial industry to calculate 
#* Value at Risk (VaR) and Expected Shortfall (ES), key metrics for market risk 
#* management.
#* 
#* Monte Carlo simulations involve generating a large number of scenarios for 
#* future asset or portfolio returns based on the statistical characteristics of 
#* the assets and their co-dependence. Each generated scenario represents a 
#* potential state of the world in the future.
#* 
#* In the context of VaR, the generated scenarios are sorted from worst to best 
#* in terms of the portfolio return. The VaR at a certain confidence level 
#* (e.g., 5%) is then the return at the corresponding point in this sorted list 
#* (e.g., the 5th percentile). This represents a threshold such that, in the 
#* worst 5% of scenarios, the portfolio return is less than or equal to the VaR.
#*
#* Expected Shortfall (ES) takes this a step further and calculates the average 
#* return in the worst scenarios beyond the VaR threshold. This is typically 
#* calculated as the average of the returns in the worst 5% of scenarios, in the 
#* example above. ES is especially useful because it gives an estimate of what 
#* to expect when the worst-case scenarios actually happen, whereas VaR only 
#* gives the threshold separating the worst cases from the rest.
#******************************************************************************
run_date             <- as.Date("2023-05-17")
base_cur             <- "DKK"
horizon              <- 10
volatility_id        <- "point_in_time"

# Invoke the API
dt_instr_deltavector <- envrsk_instrument_delta_vector(access_token  = access_token, 
                                                       date          = run_date,
                                                       symbols       = instr_symbs,
                                                       base_cur      = base_cur,
                                                       horizon       = horizon,
                                                       volatility_id = volatility_id)
# Print
dt_instr_deltavector[["Output"]]

#### PORTFOLIO SERVICES ####
#*****************************************************************************
#*
#* PORTFOLIO LEVEL REQUESTS 
#* 
#*****************************************************************************
#### Portfolio Construction ####
#*****************************************************************************
#* A portfolio of financial positions refers to the collection of financial 
#* investments held by an individual, a corporation, or an investment fund. 
#* These investments can include a diverse array of financial assets such as 
#* stocks, bonds, commodities, mutual funds, exchange-traded funds (ETFs), real 
#* estate, and more. Each individual investment within the portfolio is known 
#* as a position.
#* 
#* A position, in investment terms, represents the amount of a particular security, 
#* commodity, or other asset that is owned (long position) or borrowed and sold 
#* (short position) by an individual or institution. It reflects an investor's 
#* exposure to that particular asset.
#* 
#* The composition of a portfolio is usually tailored to match the investor's risk 
#* tolerance, investment goals, and time horizon. An investor's portfolio can be 
#* diversified across different types of assets, industries, and geographical 
#* regions to reduce risk through diversification. This is based on the principle 
#* that not all investments are likely to move in the same direction at the same 
#* time, so losses in one investment may be offset by gains in another.
#* 
#* Portfolio management is the process of selecting and overseeing a collection 
#* of investments that meet long-term financial objectives and risk tolerance 
#* with a reasonable level of return. It may involve periodic adjustments as market 
#* conditions, investor's life circumstances, or investment goals change.
#*****************************************************************************
demo_port_data <- data.table::data.table("symbol"        = c("AAPL.US", "DANSKE.CO", "CashUSD", "AGG.US"),
                                         "position_type" = c("single_stock", "single_stock", "cash", "etf"),
                                         "quantity"      = c(129, 768, 69000, 89))

# Invoke the API - Add a unique ID to each position in the portfolio
demo_port_data <- envrsk_decorate_portfolio_id(access_token = access_token,
                                               positions    = demo_port_data)

#*****************************************************************************
#### Portfolio Regular Risk - VaR & ES ####
#*****************************************************************************
#* Portfolio risk management involves the use of several metrics and risk measures 
#* to identify and control potential losses that could occur due to changes in 
#* the financial market. Two of these key measures are Value at Risk (VaR) and 
#* Expected Shortfall (ES).
#* 
#* Value at Risk (VaR): This is a statistical measure that quantifies the level 
#* of financial risk within a firm or investment portfolio over a specific time 
#* frame. VaR provides an estimate of the maximum loss that a portfolio could 
#* incur within a certain confidence level. For instance, if the VaR of a 
#* portfolio is stated to be $1 million at the 1% level for one day, this means 
#* there is a 1% chance that the portfolio could decrease in value by $1 million 
#* or more over a day.
#* 
#* Although VaR is a widely used risk measure, VaR has its limitations, 
#* particularly its inability to measure the size of extreme losses beyond the 
#* VaR threshold.
#* 
#* Expected Shortfall (ES): Also known as Conditional Value at Risk (CVaR), ES is 
#* another risk measure that aims to address some of the shortcomings of VaR. It 
#* is used to estimate the expected loss that could occur in a worst-case scenario. 
#* Unlike VaR, which just gives the maximum loss at a specific confidence level, 
#* ES provides an expected value or the average of those losses that could occur 
#* beyond the VaR cut-off point.
#* 
#* For example, if we have a portfolio with a 1% 1-day VaR of $1 million, the 
#* Expected Shortfall might tell us that, on the 1% of days when the loss exceeds 
#* $1 million, the average loss is say, $1.5 million.
#* 
#* This measure gives investors a more comprehensive view of the potential 
#* downside because it takes into account the severity of losses when adverse 
#* market conditions occur.
#* 
#* Both VaR and ES are critical for managing market risk, understanding the 
#* potential for losses, and setting capital reserves against potential future 
#* losses. But it's important to note that while these risk measures provide a 
#* guide, they are based on historical data and statistical assumptions and are 
#* not definitive predictors of future risk.
#*****************************************************************************
run_date             <- as.Date("2023-05-17")
base_cur             <- "DKK"
horizon              <- 1
volatility_id        <- "point_in_time"
signif_level         <- 0.975

out_portf_reg_risk <- envrsk_portfolio_risk_regular(access_token  = access_token, 
                                                    date          = run_date,
                                                    positions     = dt_port,
                                                    base_cur      = base_cur,
                                                    horizon       = horizon,
                                                    signif_level  = signif_level, 
                                                    volatility_id = volatility_id)

dt_portf_reg_risk <- merge(out_portf_reg_risk[["Output"]], 
                           out_portf_reg_risk[["Positions_Mapped"]][,.(uid, symbol, position_type, quantity, exchange, name, qc)],
                           by.x = "UID",
                           by.y = "uid",
                           all.x = TRUE)

# Print 
dt_portf_reg_risk

# In case one or more of the requested instrument symbols are not available on 
# the platform
out_portf_reg_risk[["Positions_UnMapped"]]

#*****************************************************************************
#### Portfolio Component Risk - VaR & ES ####
#*****************************************************************************
#* Component risk measures are used in market risk management to quantify the 
#* risk contribution of each individual asset (or group of assets) to the total 
#* risk of a portfolio. Two commonly used component risk measures are Component 
#* Value at Risk (CVaR) and Component Expected Shortfall (CES).
#* Component Value at Risk (CVaR): Also known as Incremental VaR, this measure 
#* quantifies the amount of risk that a particular asset or group of assets 
#* contributes to the overall portfolio risk. This measure can help determine 
#* which assets contribute most to portfolio risk, thus allowing for targeted 
#* risk management strategies.
#* 
#* Component Expected Shortfall (CES): This is an extension of the CVaR concept 
#* and measures the contribution of a given asset to the Expected Shortfall (ES) 
#* of the portfolio. The Expected Shortfall, also known as Conditional VaR, 
#* provides a more comprehensive measure of risk because it takes into account 
#* not just the probability of a large loss (as VaR does) but also the average 
#* size of that loss. CES calculates the marginal or incremental risk of a 
#* single asset by measuring the effect on the overall portfolio's ES if the 
#* position in a particular asset is slightly increased.
#* 
#* By applying these component risk measures, risk managers can better understand 
#* how individual assets or asset classes are contributing to overall portfolio 
#* risk, and thus make more informed decisions about risk mitigation strategies. 
#* However, as with any quantitative model, it's important to remember that these 
#* measures are based on a number of assumptions and simplifications, so they 
#* should be used as part of a broader risk management framework, rather than as 
#* a single definitive measure of risk.
#*****************************************************************************
out_portf_comp_risk <- envrsk_portfolio_risk_component(access_token  = access_token, 
                                                       date          = run_date,
                                                       positions     = demo_port_data,
                                                       base_cur      = base_cur,
                                                       horizon       = horizon,
                                                       signif_level  = signif_level, 
                                                       volatility_id = volatility_id)

dt_portf_comp_risk <- merge(out_portf_comp_risk[["Output"]], 
                           out_portf_comp_risk[["Positions_Mapped"]][,.(uid, symbol, position_type, quantity, exchange, name, qc)],
                           by.x = "UID",
                           by.y = "uid",
                           all.x = TRUE)

# Print
dt_portf_comp_risk

# In case one or more of the requested instrument symbols are not available
out_portf_comp_risk[["Positions_UnMapped"]]

#*****************************************************************************
#### Portfolio Delta Vector ####
#*****************************************************************************
#* Delta vectors are the key components in calculating Value at Risk (VaR) and 
#* Expected Shortfall (ES) for market risk management.
#* 
#* Delta Vector: In a financial context, a delta vector refers to the 
#* sensitivities of financial instrument prices to small changes in underlying 
#* parameters, such as prices, volatilities, and interest rates. These deltas 
#* represent the rate of change of a financial instrument's value concerning 
#* these factors.
#* 
#* Calculating Value at Risk (VaR): VaR estimates the potential loss in value of 
#* a risky asset or portfolio over a defined period for a given confidence interval. 
#* To calculate VaR using delta vectors, one first calculates the portfolio's 
#* delta-normal VaR. This calculation is based on the delta vector, the covariance 
#* matrix of the factors, and a percentile of the standard normal distribution 
#* corresponding to the desired confidence level. The delta-normal VaR is 
#* appropriate when the changes in portfolio value can be approximated as a linear 
#* function of the changes in risk factors.
#* 
#* Calculating Expected Shortfall (ES): ES, also known as Conditional Value at 
#* Risk (CVaR), is another risk measure that quantifies the expected loss in the 
#* worst-case scenario. ES gives a more comprehensive view of risk because, unlike 
#* VaR, it takes into account the severity of the losses beyond the VaR cut-off 
#* point. To calculate ES using delta vectors, you need to determine the expected 
#* loss beyond the VaR, given that the portfolio's loss exceeds the VaR. In a 
#* delta-normal framework, this calculation can be performed using the portfolio's 
#* delta vector and the normal distribution.
#* 
#* In summary, delta vectors are used in the context of linear approximations to 
#* portfolio changes in calculating VaR and ES. These risk measures provide crucial 
#* information to market risk managers about the potential losses their portfolio 
#* could face within a certain confidence level over a defined period.
#*****************************************************************************
out_portf_deltavector <- envrsk_portfolio_delta_vector(access_token  = access_token, 
                                                       date          = run_date,
                                                       positions     = demo_port_data,
                                                       base_cur      = base_cur,
                                                       horizon       = horizon, 
                                                       volatility_id = volatility_id)

# Print 
out_portf_deltavector[["Output"]]

# In case one or more of the requested instrument symbols are not available
out_portf_deltavector[["Positions_UnMapped"]]

#*****************************************************************************
#### Portfolio Market Value, Notational & Hypothetical PnL ####
#*****************************************************************************
out_portf_mv <- envrsk_portfolio_hypothetical_performance(access_token  = access_token, 
                                                          date          = run_date,
                                                          positions     = demo_port_data,
                                                          base_cur      = base_cur)

dt_portf_mv <- merge(out_portf_mv[["Output"]], 
                     out_portf_mv[["Positions_Mapped"]][,.(uid, symbol, position_type, quantity, exchange, name, qc)],
                     by.x = "UID",
                     by.y = "uid",
                     all.x = TRUE)

# Print
dt_portf_mv

# In case one or more of the requested instrument symbols are not available
out_portf_mv[["Positions_UnMapped"]]

