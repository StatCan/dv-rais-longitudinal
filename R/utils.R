format_number <- function(number, locale = "en") {
  # format number: add thousand separator and decimal points based on the locale
  
  if (locale == "fr") {
    k_delim <- " "
    d_delim <- ","
  } else {
    k_delim <- ","
    d_delim <- "."
  }
  
  ifelse(is.na(number), 
         out <- NA, 
         out <- prettyNum(number, big.mark=k_delim, decimal.mark=d_delim, 
                          scientific=FALSE))
  
  return(out)
}

format_pct <- function(number, locale = "en") {
  # format number in percentage:
  # add thousand separator, decimal points, and the percentage symbol (%)
  # based on the locale
  
  if (locale == "fr") {
    pct <- " %"
  } else {
    pct <- "%"
  }
  
  fmt_num <- format_number(number, locale)
  
  out <- ifelse(is.na(fmt_num),
                NA, 
                paste0(fmt_num, pct))
  
  return(out)
}

format_dollar <- function(number, locale = "en") {
  # format number in dollar amount:
  # add thousand separator, decimal points, and the dollar symbol ($)
  # based on the locale

  if (locale == "fr") {
    before <- ""
    after <- " $"
  } else {
    before <- "$"
    after <- ""
  }
  
  fmt_num <- format_number(number, locale)
  
  out <- ifelse(is.na(fmt_num),
                NA,
                paste0(before, fmt_num, after))
  
  return(out)
}

format_colon <- function(locale = "en") {
  # add an extra space before the colon (:) depending on the locale.
  
  out <- ifelse(locale == "fr",
                " : ", ": ")
  return(out)
}

value_status_flag <- function(value, status, flag, is_percent = FALSE, locale = "en") {
  if (is.na(value)) {
    status
  } else {
    HTML(
      paste0(
        ifelse(is_percent,
               format_pct(value, locale = locale),
               format_number(value, locale = locale)),
        "<sup>", str_trim(flag), "</sup>", collapse = NULL))
  }
}