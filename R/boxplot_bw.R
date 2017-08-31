#' A boxplot_bw Function
#'
#' Boxplot incluyendo puntos individuales
#' @param dat data frame que contiene las variables a graficar.
#' @param x nombre de la variable numerica
#' @param y nombre de la variable factor. Default value is NULL.
#' @param ylim.plot is vector which contains lower and upper limits which are to appear on the y axes.
#' @param title.plot a main title for the plot
#' @param title titulo del grafico
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @export boxplot_bw
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' boxplot_bw(dat = mtc_bis, x = 'qsec', y = 'gear')
#' boxplot_bw(dat = mtc_bis, x = 'qsec' )
#' @keywords plots descriptive boxplot


boxplot_bw <- function(x, y = NULL, dat,
                       las = 0,
                       title.plot = NULL,
                       ylim.plot = NULL,
                       cex.lab = 1) {

    if (is.null(ylim.plot))
        ylim.plot <- c(min(dat[, x], na.rm = T), max(dat[, x] + 0.2, na.rm = T))
    op <- par(cex.axis = cex.lab)

    ## univariant
    if (is.null(y)) {
        if (is.null(title.plot))
            title.plot <- x
        beeswarm(dat[, x],
                 ylab = "", main = title.plot,
                 ylim = ylim.plot,
                 axes = F,
                 pch = 20,
                 col = gg_color(1))

        boxplot(dat[, x],
                add = T,
                col = makeTransparent("grey", alpha = 0.3),
                las = las)

        ## bivariant
    } else {
        if (is.null(title.plot))
            title.plot <- ""
        beeswarm(dat[, x] ~ dat[, y],
                 ylab = "", xlab = y, main = title.plot,
                 ylim = ylim.plot,
                 axes = F,
            pch = 20, col = gg_color(length(levels(dat[, y]))))
        boxplot(dat[, x] ~ dat[, y],
                add = T,
                col = makeTransparent("grey", alpha = 0.3),
                las = las,
                cex.lab = cex.lab,
                ylab = x)
    }
    par(op)
}
