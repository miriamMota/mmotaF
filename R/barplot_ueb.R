#' A barplot_ueb Function
#'
#' Boxplot incluyendo puntos individuales
#' @param y name factor vector of data values.
#' @param group name factor vector. Default value is NULL.
#' @param dat matrix or data frame containing the variables in the formula.
#' @param title.plot a main title for the plot
#' @param sub.plot a sub title for the plot
#' @param cex.lab expansion factor for axis names (bar labels). (size x labels)
#' @param cex.lg expansion factor for legend names (size legend)
#' @param cex.main expansion factor for main names (size main)
#' @param show.lg TRUE o FALSE indica si se muestra la leyenda. Por defecto FALSE.
#' @param show.freq TRUE o FALSE indica si se muestran las frecuencias. Por defecto TRUE
#' @param do.test logical value si se quiere realizar test kruskall Wallis.
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @param ylab a title for the y axis
#' @export barplot_ueb
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(sex = c(sample(c('Male', 'Female'), 500, replace = TRUE, prob = c(.2,.8) ),
#' sample(c('Male', 'Female'), 500, replace = TRUE, prob = c(.4,.6) )),
#' grup =  c( rep('Casos', 500),rep('Control', 500)  ))
#' barplot_ueb(y = "sex", dat = df)
#' barplot_ueb(y = "sex",group = "grup", dat = df, cex.lab = 0.8, do.test = T)
#' @keywords plots descriptive barplot


barplot_ueb <- function(y, group = NULL, dat,
                       las = 0,
                       title.plot = NULL,
                       sub.plot = NULL,
                       cex.lab = 1,
                       cex.main = 1,
                       cex.lg = 1,
                       ylab = "",
                       do.test = FALSE,
                       show.lg = FALSE,
                       show.freq = TRUE ) {
  ## descriptiu univariat
  if (is.null(group)) {
    parmar <- c(5.1, 4.1, 4.1, 2.1)
    } else {
      parmar <- c(5.1, 4.1, 4.1, 7.1)
      dat[, group] <- as.factor(as.character(dat[, group]))
    }

  if (is.null(group)) {
    if (show.lg) {
      parmar <- c(5.1, 4.1, 4.1, 7.1)
    }
    op <- par(mar = parmar, xpd = TRUE)
    col.lev <- gg_color(length(levels(dat[, y])))
    tab2bar <- prop.table(table(dat[, y])) * 100
    aa <- barplot(tab2bar, xlab = "", ylab = "%",
                  main = ifelse(is.null(title.plot), y, title.plot) ,
                  sub = ifelse(is.null(sub.plot), "", sub.plot),
                  col = col.lev, ylim = c(0, max(tab2bar) + 6.5),
                  las = las, cex.names = cex.lab,
                  cex.main = cex.main)
    if (show.freq)
      try(text(aa, tab2bar + 4, labels = table(dat[, y]), cex = 0.8))
    if (show.lg) {
      legend(length(levels(dat[, y])) + 0.7, (max(tab2bar, na.rm = T) * 0.4),
             inset = c(-0.25, 0),
             legend = levels(dat[, y]),
             bg = "white",
             fill = col.lev, cex = cex.lg,
             yjust = 0, title = y)
      par(op)
    }
    par(op)

    ## descriptiu bivariat
  } else {
    op <- par(mar = parmar, xpd = TRUE)

    col.lev <- gg_color(length(levels(dat[, y])))
    tab2bar <- prop.table(table(dat[, y], dat[, group]), 2) * 100
    aa <- barplot(tab2bar,
                  xlab = group, ylab = "%", main = ifelse(is.null(title.plot), "", title.plot),
                  sub = ifelse(is.null(sub.plot), "", sub.plot),
                  col = col.lev, las = las,
                  cex.names = cex.lab, cex.main = cex.main)

    legend(length(levels(dat[, group])) + 0.7, 50, inset = c(-0.25, 0),
           legend = levels(dat[, y]),
           bg = "white",
           fill = col.lev,
           cex = cex.lg, yjust = 0.5, title = y)  ## ajustar llegenda y
    if (do.test) {
      Chipval <- chisq.test(dat[, y], dat[, group])$p.val
      mtext(paste("Chi p-value: ", ifelse( round(Chipval,3) < 0.001, "<0.001", round(Chipval,3) ) ) , at = 1, side = 3,cex = 0.6)
    }


    par(op)
  }
}
