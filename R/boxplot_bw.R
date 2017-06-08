#' A boxplot_bw Function
#'
#' Boxplot incluyendo puntos individuales
#' @param dat data frame que contiene las variables a graficar.
#' @param x nombre de la variable numerica
#' @param y nombre de la variable factor
#' @title titulo del grafico
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @export boxplot_bw
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' boxplot_bw(dat = mtc_bis, x = "qsec" ,y ="gear")
#' @keywords plots descriptive boxplot




boxplot_bw <- function(y, x, dat, las = 0, title = NULL, x){
  if(is.null(title)) title <- x
  beeswarm(dat[,x] ~ dat[, y],ylab = "", xlab = y,
           main = title, xlim = ifelse(is.null(xlim), c(min(x), max(x)), xlim),
           axes = F,
           pch = 20,
           col = gg_color(length(levels(dat[, y]))))

  boxplot(dat[,x] ~ dat[, y], add = T, col = makeTransparent("grey",alpha = 0.3), las = las)
}

