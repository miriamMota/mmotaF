#' A recode_factor Function
#'
#' Crea un diccionario para los niveles de una variable factor
#' @param dict data.frame con dos columnas, columna 1,rawtext, niveles posibles; columna 2,recoded, niveles correctos(an object of type create_dictionary.)
#' @param var object to be coerced or tested. (factor)
#' @export recode_factor
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' \dontrun{
#' dc_fumador <- create_dictionary(factorDat$Fumador)
#' table(factorDat$Fumador)
#' factorDat$Fumador <- recode_factor(dict = dc_fumador, var = factorDat$Fumador)
#' table(factorDat$Fumador)
#' }
#' @keywords dictionary variable factor matching clean


recode_factor <- function(dict, var, na.char = "NA") {
    var <- as.character(var)

    for (i in 1:nrow(dict)) {
        var[as.character(var) == dict[i, 1]] <- dict[i, 2]
    }
    var[var == na.char] <- NA
    return(variable = factor(var))
}
