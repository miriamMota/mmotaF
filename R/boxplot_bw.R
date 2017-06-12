#' A boxplot_bw Function
#'
#' Boxplot incluyendo puntos individuales
#' @param dat data frame que contiene las variables a graficar.
#' @param x nombre de la variable numerica
#' @param y nombre de la variable factor. Default value is NULL.
#' @param ylim_plot is vector which contains lower and upper limits which are to appear on the y axes.
#' @param title_plot a main title for the plot
#' @title titulo del grafico
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @export boxplot_bw
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' boxplot_bw(dat = mtc_bis, x = "qsec" ,y ="gear")
#' boxplot_bw(dat = mtc_bis, x = "qsec" )
#' @keywords plots descriptive boxplot




boxplot_bw <- function(x, y = NULL, dat,
                       las = 0,
                       title_plot = NULL,
                       ylim_plot = NULL ){

  if (is.null(title_plot)) title_plot <- x
  if (is.null(ylim_plot)) ylim_plot <- c(min(dat[,x], na.rm = T), max(dat[,x] + 0.2, na.rm = T))

  if (is.null(y)) {
    beeswarm(dat[,x] ,ylab = "",
             main = title_plot, ylim = ylim_plot,
             axes = F,
             pch = 20,
             col = gg_color(1))

    boxplot(dat[,x], add = T, col = makeTransparent("grey",alpha = 0.3), las = las)
  }else{
    beeswarm(dat[,x] ~ dat[, y],ylab = "", xlab = y,
             main = title_plot, ylim = ylim_plot,
             axes = F,
             pch = 20,
             col = gg_color(length(levels(dat[, y]))))
    boxplot(dat[,x] ~ dat[, y], add = T, col = makeTransparent("grey",alpha = 0.3), las = las)
  }
}
