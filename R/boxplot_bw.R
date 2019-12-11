
#' A boxplot_bw Function
#'
#' Boxplot incluyendo puntos individuales
#' @param dat matrix or data frame containing the variables in the formula.
#' @param y name numeric vector of data values.
#' @param frml Right side of ~ must have the terms in an additive way, and left side of ~ must contain the name of the grouping variable or can be left in blank (in this latter case descriptives for whole sample are calculated and no test is performed).
#' @param group name factor vector. Default value is NULL.
#' @param ylim.plot is vector which contains lower and upper limits which are to appear on the y axes.
#' @param title.plot a main title for the plot
#' @param sub.plot a sub title for the plot
#' @param do.test logical value si se quiere realizar test kruskall Wallis.
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @param ylab a title for the y axis
#' @param xlab a title for the x axis.
#' @param cex.lab size of the axis label text with a numeric value.
#' @param cex.pval size of the p-value text with a numeric value.
#' @param cex.main expansion factor for main names (size main)
#' @param cex.n expansion factor for size n
#' @export boxplot_bw
#' @import beeswarm
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(runif = c(runif(100, min = -3, max = 3),
#' rep(0,25)), rnorm = c(rnorm(100),rep(0,25)) )
#' boxplot_bw(dat = df, y = 'rnorm' )
#' boxplot_bw(dat = mtc_bis, y = 'qsec', title.plot = "Title" )
#' boxplot_bw(dat = mtc_bis, y = 'qsec', group = 'gear',
#' title.plot = "Boxplot per grup", do.test = TRUE, las = 2)
#' boxplot_bw(dat = df, frml =   ~ rnorm )
#' boxplot_bw(dat = mtc_bis, , frml =  gear ~ qsec ,
#' title.plot = "Boxplot per grup", do.test = TRUE, las = 2)
#' @keywords plots descriptive boxplot

boxplot_bw <- function(y, group = NULL,
                       frml = NULL,
                       dat,
                       las = 0,
                       title.plot = NULL,
                       sub.plot = NULL,
                       ylim.plot = NULL,
                       cex.lab = 1,
                       cex.main = 1,
                       cex.pval = 0.6,
                       cex.n = 0.5,
                       ylab = "",
                       xlab = NULL,
                       do.test = FALSE, color = NULL ) {


  ## en el cas de que hi hagi formula seleccionem el grup i la y
  if(!is.null(frml)){
    y <- rhs.vars(frml)
    if(!is.null(lhs.vars(frml))) {group <- lhs.vars(frml)}
  }


  if (is.null(ylim.plot))
    ylim.plot <- c(min(dat[, y], na.rm = T), max(dat[, y] + 0.2, na.rm = T))
  op <- par(cex.axis = cex.lab)

  ## univariant
  if (is.null(group)) {
    if (is.null(title.plot))
      title.plot <- y
    beeswarm(dat[, y],
             ylab = "",
             main = strwrap(title.plot,width = 40),
             sub = sub.plot,
             cex.main = cex.main,
             cex.sub = .7,
             ylim = ylim.plot,
             axes = F,
             pch = 20,
             col = gg_color(1), corral = "gutter")

    boxplot(dat[, y],
            add = T,
            col = makeTransparent("grey", alpha = 0.3),
            las = las)

    mtext(paste0("n = ", sum(complete.cases(dat[,y]))),side = 3, adj = 1,
          cex = cex.n)

    ## bivariant
  } else {
    if (is.null(title.plot))  {title.plot <- ""
    }
    label_group <- Hmisc::label(dat[,group])
    xlab <- ifelse(is.null(xlab), ifelse(label_group == "", group, label_group), xlab)
    xlab <- strwrap(xlab,width = 40)
    if (any(class(dat[,group]) == "character"))  dat[,group] <- as.factor(as.character(dat[,group]))

    if (is.null(color))  color <- gg_color(length(levels(dat[, group])))

    dat[,group] <- droplevels(dat[,group])
    beeswarm(dat[, y] ~ dat[, group],
             ylab = "", xlab = wrap.it(xlab,30),
             main = strwrap(title.plot,width = 40),
             ylim = ylim.plot,
             axes = F,
             cex.main = cex.main,
             cex.lab = 1,
             cex.axis = cex.lab,
             pch = 20,
             col = color )
    boxplot(dat[, y] ~ dat[, group],
            add = T,
            col = makeTransparent("grey", alpha = 0.3),
            las = las,
            cex.lab = 1,
            cex.axis = cex.lab,
            sub = sub.plot,
            cex.sub = .7,
            ylab = ylab,
            names = wrap.it(levels(dat[,group]),10))
    if (do.test) {
      KWpval <- kruskal.test(dat[, y] ~ dat[, group])$p.val
      mtext(paste("KW p-value: ", ifelse( round(KWpval,3) < 0.001, "<0.001", round(KWpval,3) )) ,
            adj = 0, side = 3,cex = cex.pval)
    }
    mtext(paste0("n = ",nrow(na.omit(dat[,c(group,y)]))),side = 3, adj = 1,
          cex = cex.n)
  }
  par(op)
}

