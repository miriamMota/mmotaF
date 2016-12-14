#' A set.var.labels Function
#'
#' Preparar etiquetas para las variables
#' @param dataframe en el cual queremos añadir etiquetas
#' @param label.vector vector string con el nombre de las etiquetas de las variables
#' @keywords labels
#' @export set.var.labels
#' @import Hmisc
#' @examples
#' #dat <- data.frame(a=c(0,1,2),b=c(1,3,4),d=c(3,4,5))
#' #labels <- c("Letter A","Letter B", "Letter C") ## vector con etiquetes
#' #var.labels <- set.var.labels (dat,as.character(labels))
#' #label(dat) <- lapply(names(var.labels), function(x) label(dat[,x]) = var.labels[x])


set.var.labels <- function(dataframe, label.vector){
  column.names <- names(dataframe)
  dataframe <- mapply(label, column.names, label.vector)
  return(dataframe)
}

