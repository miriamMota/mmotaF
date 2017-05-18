#' A NAperc Function
#'
#' Percentatge de dades faltants per variable (na$perc) i variables amb més de "x" percentatge(na$var). 
#' @param dat data frame que contiene las variables a evaluar 
#' @param maxNA Porcentafe de NA permitido 
#' @export NAperc
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @keywords na missing percentage 

NAperc <- function(df, maxNA = 80){
na <- list()
na$perc <- sort( apply(df,2,function(x) round((sum(is.na(x))/length(x) )*100,2)) )
na$var <- names(na$perc )[na$perc  > maxNA]
return(na)
}