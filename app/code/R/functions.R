print_message_with_time <- function(printed_message) {
  # print(paste0(Sys.time(), ": ", printed_message))
  message(paste0(Sys.time(), ": ", printed_message))
}

determine_maturity<- function(maturity_text, frequency) {
  maturity = NA
  if (frequency=="daily") {
    maturity = maturities_daily[maturities_choices_daily==maturity_text]
  } else {
    if (frequency=="weekly") {
      maturity = maturities_weekly[maturities_choices_weekly==maturity_text]
    } else {
      if (frequency=="monthly") {
        maturity = maturities_monthly[maturities_choices_monthly==maturity_text]
      } else {
        if (frequency=="quarterly") {
          maturity = maturities_quarterly[maturities_choices_quarterly==maturity_text]
        }
      }
    }
  }
  return(maturity)
}