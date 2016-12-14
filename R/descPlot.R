#' A descPlot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es "data.frame"
#' @param dat data frame que contiene las variables a graficar. 
#' @param topdf valor lógico indicando si se quieren guardar los gráficos en pdf. Por defecto FALSE.
#' @param nameFile nombre del fichero (tipo caracter) donde guardar los gráficos. Por defecto "descriptive_plots.pdf". 
#' @param color nombre del color para pintar el gráfico
#' @param nrow.par número de filas que mostrar en la interficie gráfica 
#' @param ncol.par número de columnas que mostrar en la interficie gráfica 
#' @export descPlot
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),
#' X = rnorm(50,10,1))
#' descPlot(dat = df, color = "red", nrow.par = 1, ncol.par = 2)
#' @keywords plots descriptive 


descPlot <- function(dat, topdf = FALSE, nameFile = "descriptive_plots.pdf", color = "gray48", nrow.par = 4, ncol.par = 2) 
{
  if (topdf)   pdf(nameFile)
  par(mfrow = c(nrow.par, ncol.par))
  for (i in 1:dim(dat)[2]) {
    if (class(dat[, i]) == "factor") {
      try(plot(dat[, i], xlab = names(dat)[i], main = "Diagrama de barras", col = makeTransparent(color)), 
          TRUE)
    }
    else {
      # par(mar = c(3.1, 3.1, 1.1, 2.1))
      try(hist(dat[, i], xlab = names(dat)[i], main = "Histograma", col = makeTransparent(color)), TRUE)
      try(rug(dat[,i]))
      # try(boxplot(dat[,i], horizontal = TRUE, outline = TRUE, frame = FALSE,  col = "green1", add = TRUE),TRUE)
    }
  }
  if (topdf)    dev.off()
}



