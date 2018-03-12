#' A boxplot_bw Function
#'
#' Boxplot incluyendo puntos individuales
#' @param dat matrix or data frame containing the variables in the formula.
#' @param y name numeric vector of data values.
#' @param group name factor vector. Default value is NULL.
#' @param ylim.plot is vector which contains lower and upper limits which are to appear on the y axes.
#' @param title.plot a main title for the plot
#' @param sub.plot a sub title for the plot
#' @param do.test logical value si se quiere realizar test kruskall Wallis.
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @param at.text if do.test TRUE, give location of each string in user coordinates. If the component of at corresponding to a particular text item is not a finite value (the default), the location will be determined by adj.
#' @param ylab a title for the y axis
#' @param xlab a title for the x axis
#' @export boxplot_bw
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(runif = c(runif(100, min = -3, max = 3),rep(0,25)), rnorm = c(rnorm(100),rep(0,25)) )
#' boxplot_bw(dat = df, y = 'rnorm' )
#' boxplot_bw(dat = mtc_bis, y = 'qsec' )
#' boxplot_bw(dat = mtc_bis, y = 'qsec', group = 'gear',
#' title.plot = "Boxplot per grup", do.test = TRUE, las = 2)
#' @keywords plots descriptive boxplot


boxplot_bw <- function(y, group = NULL, dat,
                       las = 0,
                       title.plot = NULL,
                       sub.plot = NULL,
                       ylim.plot = NULL,
                       cex.lab = 1,
                       ylab = "",
                       xlab = NULL,
                       do.test = FALSE, at.text = 1) {

    if (is.null(ylim.plot))
        ylim.plot <- c(min(dat[, y], na.rm = T), max(dat[, y] + 0.2, na.rm = T))
    op <- par(cex.axis = cex.lab)

    ## univariant
    if (is.null(group)) {
        if (is.null(title.plot))
            title.plot <- y
        beeswarm(dat[, y],
                 ylab = "",
                 main = title.plot,
                 sub = sub.plot,
                 cex.sub = .7,
                 ylim = ylim.plot,
                 axes = F,
                 pch = 20,
                 col = gg_color(1), corral = "gutter")

        boxplot(dat[, y],
                add = T,
                col = makeTransparent("grey", alpha = 0.3),
                las = las)

        ## bivariant
    } else {
      if (is.null(title.plot))  {title.plot <- ""}
      if (is.null(xlab))  {xlab <- group}

      dat[,group] <- as.factor(as.character(dat[,group]))

        beeswarm(dat[, y] ~ dat[, group],
                 ylab = "", xlab = xlab,
                 main = title.plot,
                 ylim = ylim.plot,
                 axes = F,
            pch = 20, col = gg_color(length(levels(dat[, group]))))
        boxplot(dat[, y] ~ dat[, group],
                add = T,
                col = makeTransparent("grey", alpha = 0.3),
                las = las,
                cex.lab = cex.lab,
                sub = sub.plot,
                cex.sub = .7,
                ylab = ylab)
        if(do.test){
          KWpval <- kruskal.test(dat[, y] ~ dat[, group])$p.val
          mtext(paste("KW p-value: ", ifelse( round(KWpval,3) < 0.001, "<0.001", round(KWpval,3) )) ,
                at = at.text, side = 3,cex = 0.6)
        }

    }
    par(op)
}
