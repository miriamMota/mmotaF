#' A dictionary_list Function
#'
#' Crea una lista de diccionarios para los niveles de una seria de variables factor
#' @param var nombre de las variables factor a recodificar
#' @param dat data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables
#' @export dictionary_list
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' # dictionary_all <- dictionary_list(names(factorDat), factorDat)
#' # Asignar nuevos niveles a todas las variables
#' # for (i in 1:length(dictionary_all)) {
#' # factorDat[,gsub("^dc_","",names(dictionary_all)[i])] <- recode_factor(dict = dictionary_all[[i]],
#' #                                                                        var = factorDat[,gsub("^dc_","",names(dictionary_all)[i])] )
#' #                                                                        }
#' @keywords dictionary variable factor

dictionary_list <- function(variables, data){
  dictionary_all <- list()
  for (i in 1:length(variables)) {
    dictionary_all[[paste0("dc_", variables[i])]] <- create_dictionary(
      data[,variables[i]])
  }
  return(dictionary_all)
}



