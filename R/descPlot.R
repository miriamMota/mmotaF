#' A descPlot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es "data.frame"
#' @param dat data frame que contiene las variables a graficar.
#' @param topdf valor lógico indicando si se quieren guardar los gráficos en pdf. Por defecto FALSE.
#' @param nameFile nombre del fichero (tipo caracter) donde guardar los gráficos. Por defecto "descriptive_plots.pdf".
#' @param subtitle subtitol
#' @param color nombre del color para pintar el gráfico
#' @param rowcol c(nrows, ncols) to create a matrix of nrows x ncols plots that are filled in by row
#' @param show.lg TRUE o FALSE indica si se muestra la leyenda. Por defecto FALSE.
#' @param show.freq TRUE o FALSE indica si se muestran las frecuencias. Por defecto TRUE
#' @export descPlot
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),X = rnorm(50,10,1))
#' descPlot(dat = df, color = "red", rowcol = c(1,2))
#' descPlot(dat = df, y = "Y", color = "red", rowcol = c(1,1))
#' descPlot(mtc_bis)
#' descPlot(dat = mtc_bis, y ="gear", rowcol = c(2,2))
#' @keywords plots descriptive


descPlot <- function(dat, y = NULL,
                     nameFile = "descriptive_plots.pdf",
                     topdf = FALSE,
                     subtitle = NULL,
                     color = "#8D4ABA",
                     rowcol = c(3,2),
                     show.lg = FALSE,
                     show.freq = TRUE )
{

  if (is.null(y)) {
    parmar = c(5.1, 4.1, 4.1, 2.1)
  }else{
    parmar = c(5.1, 4.1, 4.1, 7.1)
    dat[,y] <- as.factor(as.character(dat[,y]))
  }

  par(mar = parmar , mfrow  = rowcol)

  if (topdf) {
    pdf(nameFile)
    par(mar = parmar, xpd = TRUE , mfrow = rowcol)
  }
  if (sum(label(dat) == "") != 0) {
    namevar <- names(dat)
  }else{
    namevar <- label(dat)
  }

  for (i in 1:dim(dat)[2]) {
    ##### variables factor
    if ( ifelse(!is.null(y), names(dat)[i] != y, TRUE)) {
      if (class(dat[, i])[length(class(dat[, i]))] == "factor") {
        ## descriptiu univariat
        if (is.null(y)) {
          col.lev <-  gg_color(length(levels(dat[,i])))
          tab2bar <- prop.table(table(dat[, i])) * 100
          try(aa <- barplot(tab2bar,
                            xlab = namevar[i],
                            ylab = "%",
                            main = "Diagrama de barras",
                            sub = ifelse(is.null(subtitle),"", subtitle),
                            col = col.lev ,#legend.text = T,
                            ylim = c(0, max(tab2bar) + 6.5 ) ), TRUE)
          if (show.freq) try(text(aa,tab2bar + 4,labels = table(dat[, i]), cex = 0.8))
          if (show.lg) {
            legend("topleft", levels(dat[,i]), bty = "n", fill = col.lev, cex = 0.75 )
          }
          ## descriptiu bivariat
        }else{
          col.lev <-  gg_color(length(levels(dat[,i])))
          tab2bar <- prop.table(table(dat[, i],dat[,y]),2) * 100
          try(aa <- barplot(tab2bar,
                            xlab = y,
                            ylab = "%",
                            main = names(dat)[i],
                            sub = ifelse(is.null(subtitle),"", subtitle),
                            col = col.lev), TRUE)

          legend("topright",inset=c(-0.25,0), legend = levels(dat[,i]), bty = "n", fill = col.lev, cex = 0.75 )
        }
        ##### variables numeriques
      }else {
        ## descriptiu univariat
        if (is.null(y)) {
          try(hist(dat[, i], xlab = namevar[i], main = "Histograma",
                   sub = ifelse(is.null(subtitle),"",subtitle),
                   col = makeTransparent(color,alpha = 0.8)), TRUE)
          try(rug(dat[, i]))
          # try(rug(jitter(dat[, i],amount = 0)))
          ## descriptiu bivariat
        }else{
          beeswarm(dat[,i] ~ dat[, y],ylab = "", xlab = "",
                   main = names(dat)[i], axes = F,
                   pch = 20, col = gg_color(3))
          boxplot(dat[,i] ~ dat[, y], add = T,
                  col = makeTransparent("grey",alpha = 0.3))
        }
      }
    }
  }
  if (topdf) {dev.off()}
}
