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
  
  ifelse(is.na(fmt_num),
         out <- NA,
         out <- paste0(fmt_num, pct))
  
  return(out)
}

format_dollar <- function(number, locale = "en") {
  # format number in dollar amount:
  # add thousand separator, decimal points, and the dollar symbol ($)
  # based on the locale

  fmt_num <- format_number(number, locale)
  
  ifelse(is.na(fmt_num),
         out <- NA,
         out <- ifelse(locale == "fr",
                       paste0(fmt_num, " $"),
                       paste0("$", fmt_num)))
  
  return(out)
}