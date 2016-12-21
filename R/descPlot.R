#' A descPlot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es "data.frame"
#' @param dat data frame que contiene las variables a graficar. 
#' @param topdf valor lógico indicando si se quieren guardar los gráficos en pdf. Por defecto FALSE.
#' @param nameFile nombre del fichero (tipo caracter) donde guardar los gráficos. Por defecto "descriptive_plots.pdf". 
#' @param color nombre del color para pintar el gráfico
#' @param nrow.par número de filas que mostrar en la interficie gráfica 
#' @param ncol.par número de columnas que mostrar en la interficie gráfica 
#' @param show.lg TRUE o FALSE indica si se muestra la leyenda. Por defecto FALSE.
#' @export descPlot
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),
#' X = rnorm(50,10,1))
#' descPlot(dat = df, color = "red", nrow.par = 1, ncol.par = 2)
#' @keywords plots descriptive 


descPlot <- function(dat, topdf = FALSE, nameFile = "descriptive_plots.pdf", 
                      color = "gray48", nrow.par = 4, ncol.par = 2, show.lg = FALSE) 
{
  if (topdf) 
    pdf(nameFile)
  par(mfrow = c(nrow.par, ncol.par))
  for (i in 1:dim(dat)[2]) {
    if (class(dat[, i]) == "factor") {
      col.lev <- makeTransparent(color, "#3498db", "#95a5a6", "#e74c3c", "olivedrab4", "#2ecc71")[1:length(levels(dat[,i])) ]
      #col.lev <-  makeTransparent(rainbow(length(levels(dat[,i]))))
      tab2bar <- prop.table(table(dat[, i])) * 100
      try(barplot(tab2bar, 
                  xlab = names(dat)[i], 
                  ylab = "%",
                  main = "Diagrama de barras", 
                  col = col.lev ,#legend.text = T,  
                  ylim = c(0, max(tab2bar) ) ), TRUE)
      if (show.lg) {
        legend("topleft", levels(dat[,i]), bty = "n", fill = col.lev, cex = 0.75 )
      }
    }
    else {
      try(hist(dat[, i], xlab = names(dat)[i], main = "Histograma", 
               col = makeTransparent(color)), TRUE)
      try(rug(dat[, i]))
    }
  }
  if (topdf) 
    dev.off()
}





