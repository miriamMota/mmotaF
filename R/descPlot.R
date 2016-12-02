#' A descPlot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es "data.frame"
#' @param dat data frame que contiene las variables a graficar. 
#' @param topdf valor lógico indicando si se quieren guardar los gráficos en pdf. Por defecto FALSE.
#' @param nameFile nombre del fichero (tipo caracter) donde guardar los gráficos. Por defecto "descriptive_plots.pdf". 
#' @export descPlot
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(y = as.factor(c(0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1)),X = c(20,2,1,2,23,3,4,5,15,-5,2,1,3,10,4,87,5,3,7,0,1,-2))
#' descPlot(dat = df)
#' @keywords plots descriptive 


descPlot <- function(dat, topdf = FALSE, nameFile = "descriptive_plots.pdf"){
  if (topdf) pdf(nameFile)
  par(mfrow = c(4,2))
  for (i in 2:dim(dat)[2]) {
    if (class(dat[,i]) == "factor") {
      try(plot(dat[,i], xlab = names(dat)[i], main = "Diagrama de barras"),TRUE)
    }else {
      try(hist(dat[,i], xlab = names(dat)[i], main = "Histograma"),TRUE)
    }
  }
  if (topdf) dev.off()
}


