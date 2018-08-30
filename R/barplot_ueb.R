#' A barplot_ueb Function
#'
#' Barplot univariante o bivariante mostrando frecuencias relativas. En el caso
#' del univariante es posible añadir las frecuencias absolutas. Para el análisis
#' bivariante es posible realizar test Chi cuadrado.
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
#' @param do.test logical value si se quiere realizar test Chi cuadrado SIN corrección de yates.
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @param at.text if do.test TRUE, give location of each string in user coordinates. If the component of at corresponding to a particular text item is not a finite value (the default), the location will be determined by adj.
#' @param ylab a title for the y axis
#' @export barplot_ueb
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(sex = c(sample(c('Male', 'Female'), 500, replace = TRUE, prob = c(.2,.8) ),
#' sample(c('Male', 'Female'), 500, replace = TRUE, prob = c(.4,.6) )),
#' grup =  c( rep('Casos', 500),rep('Control', 500)  ))
#' barplot_ueb(y = "sex", dat = df)
#' barplot_ueb(y = "sex",group = "grup", dat = df, cex.lab = 0.8, do.test = TRUE)
#' @keywords plots descriptive barplot


barplot_ueb <- function(y, group = NULL, dat,
                       las = 0,
                       title.plot = NULL,
                       sub.plot = NULL,
                       cex.lab = 1,
                       cex.main = 1,
                       cex.lg = 1,
                       ylab = "",
                       title.lg = FALSE,
                       do.test = FALSE,
                       show.lg = FALSE,
                       show.freq = TRUE,
                       at.text = 1)  {
  ## descriptiu univariat
  if (is.null(group)) {
    parmar <- c(5.1, 4.1, 4.1, 2.1)
    } else {
      parmar <- c(5.1, 4.1, 4.1, 7.1)
      label_group <- Hmisc::label(dat[,group])
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
                  main = ifelse(is.null(title.plot),
                                ifelse(Hmisc::label(dat[,y]) == "", y, Hmisc::label(dat[,y])) ,
                                title.plot) ,
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
                  xlab = ifelse(label_group == "", group, label_group),
                  ylab = "%", main = ifelse(is.null(title.plot),
                                            ifelse(Hmisc::label(dat[,y]) == "", "", Hmisc::label(dat[,y])) , title.plot),
                  sub = ifelse(is.null(sub.plot), "", sub.plot),
                  col = col.lev, las = las,
                  cex.names = cex.lab, cex.main = cex.main)

    legend(length(levels(dat[, group])) + 0.7, 50, inset = c(-0.25, 0),
           legend = levels(dat[, y]),
           bg = "white",
           fill = col.lev,
           cex = cex.lg, yjust = 0.5,
           title = if.else(title.lg,ifelse(Hmisc::label(dat[,y]) == "", "", Hmisc::label(dat[,y]))), "")  ## ajustar llegenda y
    if (do.test) {
      if (any(table(dat[, y], dat[, group]) < 5)) {
        fishpval <- fisher.test(dat[, y], dat[, group])$p.val
        mtext(paste("Fisher p-value: ", ifelse( round(fishpval,3) < 0.001, "<0.001", round(fishpval,3) ) ) ,
              at = at.text, side = 3,cex = 0.6)
      }else{
        Chipval <- chisq.test(dat[, y], dat[, group], correct = F)$p.val
        mtext(paste("Chi p-value: ", ifelse( round(Chipval,3) < 0.001, "<0.001", round(Chipval,3) ) ) ,
              at = at.text, side = 3,cex = 0.6)
      }

    }


    par(op)
  }
}
