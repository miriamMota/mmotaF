#' A create_dictionary Function
#'
#' Crea un diccionario para los niveles de una variable factor
#' @param var object to be coerced or tested. (factor)
#' @param na.char a character vector of strings which are to be interpreted as NA values. Blank fields are also considered to be missing values in logical, integer, numeric and complex fields. Note that the test happens after white space is stripped from the input, so na.strings values may need their own white space stripped in advance.
#' @param name.var name of variable
#' @export create_dictionary
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' \dontrun{
#' dc_fumador <- create_dictionary(var = factorDat$Fumador)
#' }
#' @keywords dictionary variable factor


create_dictionary <- function(var, name.var = NULL) {

    var <- as.factor(var)
    dc <- data.frame(rawtext = levels(var),
                     recoded = rep("", length(levels(var))))
    dc$rawtext <- as.character(dc$rawtext)
    dc$recoded <- as.character(dc$recoded)
    names(dc) <- c(paste0("rawtext.", name.var), paste0("recoded.", name.var))
    dict <- edit(dc)
    names(dict) <- c("rawtext", "recoded")

    return(dictionary = dict)
}
