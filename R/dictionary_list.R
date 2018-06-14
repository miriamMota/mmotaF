#' A dictionary_list Function
#'
#' Crea una lista de diccionarios para los niveles de una seria de variables factor
#' @param var nombre de las variables factor a recodificar
#' @param dat data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables
#' @export dictionary_list
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' \dontrun{
#' if (!file.exists('dades/dictionaries.rda')) {
#' dictionaries <- dictionary_list(variables = names(factorDat), data = factorDat)
#' save(dictionaries, file = 'dades/dictionaries.rda')
#' }else{
#'    load('dades/dictionaries.rda')
#'   }
#' Asignar nuevos niveles a todas las variables
#' for (i in 1:length(dictionaries)) {
#' factorDat[,gsub('^dc_','',names(dictionaries)[i])] <- recode_factor(dict = dictionaries[[i]],
#' var = factorDat[,gsub('^dc_','',names(dictionaries)[i])] )
#'  }
#' }
#' @keywords dictionary variable factor

dictionary_list <- function(variables, data) {
    dictionary_all <- list()
    for (i in 1:length(variables)) {
        dictionary_all[[paste0("dc_", variables[i])]] <-
          create_dictionary(data[, variables[i]],
                            name.var = variables[i])
    }
    return(dictionary_all)
}
