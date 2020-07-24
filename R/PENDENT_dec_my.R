dec_ym <- function(x) {
  year <- floor(x);
  month <- floor((x - year) * 12);
  day <- ((x - year) * 12 - month) * 30.42
  ym <- paste0(ifelse(year == 0, "", paste(year,"y")),month,"m")
  # return(list(year = year, month = month, day = day))
  return(ym)
}


dec_ym(1.11)
