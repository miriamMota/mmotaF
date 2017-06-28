#' A date_create Function
#'
#' Convierte tres vectores dia, mes, año en un fechas. En el caso de que falte el mes y o el día se indica el 6 y 15 respectivamente.
#' @param d vector numérico que incluya los dias.
#' @param m vector numérico que incluya los meses. 
#' @param y vector numérico que incluya los años.
#' @export date_create
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples 
#' day <- c(NA, NA, NA, 12, 20, NA, NA, NA, 19, NA, 25)
#' month <- c(NA, NA, 12, 7, 4, NA, 1, NA, 11, NA, 2)
#' year <- c(2000, NA, 2003, 2012, 2012, 1991, 2013, 2008, 2007, 1994, 2011)
#' date_create(d = day, m = month, y = year)
#' @keywords date day month year

date_create <- function(dy,mth,yr) {
  data <- NA
  data[is.na(yr) & is.na(mth) & is.na(dy)] <- NA
  data[!is.na(yr) & is.na(mth) & is.na(dy)] <- paste0(yr[!is.na(yr) & is.na(mth) & is.na(dy)],"/06/15")
  data[!is.na(yr) & !is.na(mth) &is.na(dy)] <- paste0(yr[!is.na(yr) & !is.na(mth) &is.na(dy)], "/", 
                                                      mth[!is.na(yr) & !is.na(mth) &is.na(dy)],"/15")
  data[!is.na(yr) & !is.na(mth) & !is.na(dy)] <- paste0(yr[!is.na(yr) & !is.na(mth) & !is.na(dy)],"/", 
                                                        mth[!is.na(yr) & !is.na(mth) & !is.na(dy)],"/",
                                                        dy[!is.na(yr) & !is.na(mth) & !is.na(dy)])
  
  data <- as.Date(data, format = "%Y/%m/%d")
  return(data)
}

