#' A multGGplot Function
#'
#' Similar a par(mfrow=c(row,col)) para "ggplot" (Obtenida de cookbook for R )
#' @param plotslist plots separados por coma
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @keywords multiGGplot
#' @export multGGplot
#' @import grid
#' @import ggplot2
#' @examples
#' p1 <- qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"),fill=gear)
#' p2 <- p3 <- p4 <- p1
#' multGGplot(p1,p2,p3,p4, cols=2)


multGGplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {

      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

