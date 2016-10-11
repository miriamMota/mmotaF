#' A postTexCG Function
#'
#' Corrige problema con funcion export2latex del paquete compareGroups, donde por defecto indica: "2*p.overall" y "2*N"
#' @param nameFile nombre del archivo con ruta incluida
#' @keywords compareGroups export2latex error tex 
#' @export postTexCG
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @return Reescribe el fichero eliminando "2*"


############################
## Miriam Mota Foix
## 2016.10.11
############################

postTexCG <- function(nameFile){
  FileInput = readLines(nameFile) 
  gs_pattern = "{2}{*}"
  fileoutput <- gsub(gs_pattern,"",FileInput, fixed = T)
  write(fileoutput, file = nameFile)
}