#******************************************************************************
#* Denependencies
#******************************************************************************
usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
pckg <- c("data.table", "jsonlite", "httr", "parallel", "getPass")

# In case one or more of the packages are not installed they will be installed
sapply(pckg, usePackage)

library(data.table)

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#******************************************************************************
#*
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#*
#******************************************************************************
#*
#* The functionality wraps the EnvisionRisk market Risk-as-a-Service API into R
#* style functions. The functions takes familiar R data object as input, takes
#* care of communicating with the cloud server and transform the JSON output
#* from the API into R data structures.
#*
base_url  <- "https://api.envisionrisk.com/"
base_path <- "v1/themis/"

envrsk_post <- function(access_token,
                        url,
                        body,
                        params){

  # Query parameters
  .query <- c(params)

  #' headers
  .headers <- httr::add_headers('ACCESS-TOKEN' = access_token)

  # Body
  .body <- jsonlite::toJSON(body)

  #' post call to the endpoint
  res <- httr::POST(
    url    = url,
    query  = .query,
    config = .headers,
    body   = .body,
    httr::accept_json()
  )

  return(list("status_code" = httr::status_code(res),
              "content"     = httr::content(res)))
}

envrsk_get <- function(access_token,
                       url,
                       params){
  
  # Query parameters
  .query <- c(params)
  
  #' headers
  .headers <- httr::add_headers('ACCESS-TOKEN' = access_token)
  
  
  #' post call to the endpoint
  res <- httr::GET(
    url    = url,
    query  = .query,
    config = .headers,
    httr::accept_json()
  )
  
  return(list("status_code" = httr::status_code(res),
              "content"     = httr::content(res)))
}

#******************************************************************************
#### Auth ####
#******************************************************************************
get_access_token <- function(usr_id, usr_pwd){
  # Query parameters
  .query <- list("usr_id"  = usr_id,
                 "usr_pwd" = usr_pwd)

  access_token_expiry <- Sys.time() + 24*60*60
  get_access_token <- httr::GET(
    url    = "https://api.envisionrisk.com/auth/get-access-token",
    query  = .query
  )

  if(httr::status_code(get_access_token) != 200){
    return(httr::content(get_access_token))
  }
  access_token <- httr::content(get_access_token)
  return(list("access-token"        = access_token,
              "access-token-expiry" = Sys.time() + (24*60*60 - 60)))

}

#******************************************************************************
#### Portfolio ####
#******************************************************************************
envrsk_portfolio_risk_regular <- function(access_token,
                                          date,
                                          positions,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL,
                                          report_depth  = NULL,
                                          simplify      = FALSE){
  end_point <- "portfolio-risk-regular"
  api_url <- paste0(base_url, base_path, end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_risk_component <- function(access_token,
                                            date,
                                            positions,
                                            base_cur      = NULL,
                                            horizon       = NULL,
                                            signif_level  = NULL,
                                            volatility_id = NULL,
                                            report_depth  = NULL,
                                            simplify      = FALSE){
  end_point <- "portfolio-risk-component"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_delta_vector <- function(access_token,
                                          date,
                                          positions,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL,
                                          report_depth  = NULL,
                                          simplify      = FALSE){
  end_point <- "portfolio-delta-vector"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_economic_capital_regular <- function(access_token,
                                                      date,
                                                      positions,
                                                      base_cur      = NULL,
                                                      horizon       = NULL,
                                                      signif_level  = NULL,
                                                      volatility_id = NULL,
                                                      expected_roe  = NULL,
                                                      report_depth  = NULL,
                                                      simplify      = FALSE){
  end_point <- "portfolio-economic-capital-regular"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_economic_capital_component <- function(access_token,
                                                        date,
                                                        positions,
                                                        base_cur      = NULL,
                                                        horizon       = NULL,
                                                        signif_level  = NULL,
                                                        volatility_id = NULL,
                                                        expected_roe  = NULL,
                                                        report_depth  = NULL,
                                                        simplify      = FALSE){
  end_point <- "portfolio-economic-capital-component"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_portfolio_hyp_rskadj_perf_regular <- function(access_token,
                                                               date,
                                                               positions,
                                                               base_cur      = NULL,
                                                               horizon       = NULL,
                                                               signif_level  = NULL,
                                                               volatility_id = NULL,
                                                               expected_roe  = NULL,
                                                               report_depth  = NULL,
                                                               simplify      = FALSE){
  end_point <- "portfolio-hyp-rskadj-perf-regular"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_portfolio_hyp_rskadj_perf_component <- function(access_token,
                                                                 date,
                                                                 positions,
                                                                 base_cur      = NULL,
                                                                 horizon       = NULL,
                                                                 signif_level  = NULL,
                                                                 volatility_id = NULL,
                                                                 expected_roe  = NULL,
                                                                 report_depth  = NULL,
                                                                 simplify      = FALSE){
  end_point <- "portfolio-hyp-rskadj-perf-component"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

envrsk_portfolio_hypothetical_performance <- function(access_token,
                                                      date,
                                                      positions,
                                                      base_cur     = NULL,
                                                      report_depth = NULL,
                                                      simplify     = FALSE){
  end_point <- "portfolio-hyp-perf"
  api_url <- paste0(base_url, base_path, end_point)

  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = positions)
  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

process_portfolio_return_values <- function(res_out, simplify){
  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(!is.null(out[["Positions_Mapped"]])){
      out[["Positions_Mapped"]] <- data.table::rbindlist(out[["Positions_Mapped"]], fill = TRUE)
    }

    if(!is.null(out[["Positions_UnMapped"]])){
      out[["Positions_UnMapped"]]     <- data.table::rbindlist(out[["Positions_UnMapped"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
}

#******************************************************************************
#### Instrument ####
#******************************************************************************
envrsk_instrument_search <- function(access_token,
                                     partial_name        = NULL,
                                     partial_symbol      = NULL,
                                     partial_exchange_id = NULL,
                                     position_type       = NULL,
                                     valid_at            = NULL){
  end_point <- "search-instrument"
  api_url <- paste0(base_url, base_path, end_point)

  .params <- list("partial_name"        = partial_name,
                  "partial_symbol"      = partial_symbol,
                  "partial_exchange_id" = partial_exchange_id,
                  "position_type"       = position_type,
                  "valid_at"            = valid_at)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = list())

  if(res_out[["status_code"]] == 200){
    out <- data.table::rbindlist(res_out[["content"]][["Output"]], fill = TRUE)
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_performance <- function(access_token,
                                          symbols,
                                          base_cur  = NULL,
                                          from      = NULL,
                                          to        = NULL,
                                          days      = 1,
                                          direction = "lead",
                                          overlap   = TRUE){
  end_point <- "instrument-performance"
  api_url <- paste0(base_url, base_path, end_point)
  
  .params <- list("base_cur"  = base_cur,
                  "from"      = from,
                  "to"        = to,
                  "days"      = days,
                  "direction" = direction,
                  "overlap"   = overlap)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_performance_raw <- function(access_token,
                                              symbols,
                                              from      = NULL,
                                              to        = NULL,
                                              days      = 1,
                                              direction = "lead",
                                              overlap   = TRUE){
  end_point <- "instrument-performance-raw"
  api_url <- paste0(base_url, base_path, end_point)
  
  .params <- list("from"      = from,
                  "to"        = to,
                  "days"      = days,
                  "direction" = direction,
                  "overlap"   = overlap)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_value_at_risk <- function(access_token,
                                     date,
                                     symbols,
                                     base_cur      = NULL,
                                     horizon       = NULL,
                                     signif_level  = NULL,
                                     volatility_id = NULL){
  end_point <- "instrument-value-at-risk"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_value_at_risk_raw <- function(access_token,
                                         date,
                                         symbols,
                                         horizon       = NULL,
                                         signif_level  = NULL,
                                         volatility_id = NULL){
  end_point <- "instrument-value-at-risk-raw"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_expected_shortfall <- function(access_token,
                                          date,
                                          symbols,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL){
  end_point <- "instrument-expected-shortfall"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_expected_shortfall_raw <- function(access_token,
                                              date,
                                              symbols,
                                              horizon       = NULL,
                                              signif_level  = NULL,
                                              volatility_id = NULL){
  end_point <- "instrument-expected-shortfall-raw"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_delta_vector <- function(access_token,
                                           date,
                                           symbols,
                                           base_cur      = NULL,
                                           horizon       = NULL,
                                           volatility_id = NULL){
  end_point <- "instrument-delta-vector"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_instrument_delta_vector_raw <- function(access_token,
                                               date,
                                               symbols,
                                               horizon       = NULL,
                                               volatility_id = NULL){
  end_point <- "instrument-delta-vector-raw"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}
#******************************************************************************
#### Time Serie ####
#******************************************************************************
envrsk_market_price <- function(access_token,
                                symbols,
                                base_cur = NULL){
  end_point <- "market-price"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list("base_cur" = base_cur)
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_market_price_raw <- function(access_token,
                                    symbols){
  end_point <- "market-price-raw"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_market_volatility <- function(access_token,
                                     symbols){
  end_point <- "market-volatility"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

envrsk_market_stress_volatility <- function(access_token,
                                            symbols){
  end_point <- "market-stress-volatility"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]
  
  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = symbols)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    
    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#******************************************************************************
#### Workflow ####
#******************************************************************************

# workflow-risk-snapshot #
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

# workflow-backtest #
envrsk_workflow_backtest <- function(access_token,
                                     backtestdata,
                                     base_cur      = NULL,
                                     signif_level  = NULL){

  end_point <- "workflow-backtest"
  api_url <- paste0(base_url, base_path, end_point)

  .params <- list("base_cur"      = base_cur,
                  "signif_level"  = signif_level)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = backtestdata)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list("Title"  = out_raw[["Title"]],
                "Input"  = list("backtestdata" = data.table::rbindlist(out_raw[["Input"]][["BacktestData"]]),
                                "base_cur"     = out_raw[["Input"]][["BaseCur"]],
                                "signif_level" = out_raw[["Input"]][["SignifLevel"]]),
                "TechOpr"  = out_raw[["TechOpr"]],
                "Output"   = data.table::rbindlist(out_raw[["Output"]]))
  } else {
    return(res_out)
  }

  return(out)
}

envrsk_workflow_weight_2_quantities <- function(access_token, dt_snapshot_weight,
                                                init_port_market_value, base_cur,
                                                is_wide = FALSE, to_date = NULL){
  if(is.null(to_date)){
    to_date <- Sys.Date()
  }

  end_point <- "workflow-weight-2-quantities"
  api_url <- paste0(base_url, base_path, end_point)

  .params <- list("init_port_market_value" = init_port_market_value,
                  "is_wide"                = is_wide,
                  "base_cur"               = base_cur,
                  "to_date"                = to_date)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = .params,
                         body         = dt_snapshot_weight)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list("Title"  = out_raw[["Title"]],
                "Input"  = list("PortfolioWeights" = data.table::rbindlist(out_raw$Input$PortfolioWeights),
                                "base_cur"         = out_raw[["Input"]][["BaseCur"]],
                                "signif_level"     = out_raw[["Input"]][["SignifLevel"]]),
                "TechOpr"  = out_raw[["TechOpr"]],
                "Output"  = list("PortfolioEvents"    = data.table::rbindlist(out_raw$Output$Events, fill = TRUE),
                                 "PortfolioQuantites" = data.table::rbindlist(out_raw$Output$Positions)),
                "UnMappedSymbols" = data.table::rbindlist(out_raw$Output$UnmappedSymbols, fill = TRUE))
  } else {
    return(res_out)
  }
}

#******************************************************************************
#### Manifest ####
#******************************************************************************
envrsk_get_manifest <- function(access_token){
  end_point <- "get-manifest"
  api_url <- paste0(base_url, base_path, end_point)
  
  # Query parameters
  .params <- list()
  
  res_out <- envrsk_get(url          = api_url,
                        access_token = access_token,
                        params       = .params)
  
  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    out_raw[["PORT_CONSTITUENTS"]] <- do.call(c, out_raw[["PORT_CONSTITUENTS"]])
  } else {
    return(res_out)
  }
  return(out_raw)
}

#******************************************************************************
#### Miscellaneous ####
#******************************************************************************
# common-decorate-table-id #
envrsk_decorate_portfolio_id <- function(access_token, positions, simplify = TRUE){
  end_point <- "decorate-table-id"
  api_url <- paste0(base_url, base_path, end_point)

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = list(),
                         body         = positions)
  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
  return(out)
}

# common-decorate-position-id #
envrsk_decorate_position_id <- function(access_token, positions, simplify = TRUE){
  end_point <- "decorate-position-id"
  api_url <- paste0(base_url, base_path, end_point)

  res_out <- envrsk_post(url          = api_url,
                         access_token = access_token,
                         params       = list(),
                         body         = positions)

  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
  return(out)
}

#******************************************************************************
#### R Functionality ####
#******************************************************************************



