#' A descPlot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es 'data.frame'
#' @param dat data frame que contiene las variables a graficar.
#' @param topdf valor lógico indicando si se quieren guardar los gráficos en pdf. Por defecto FALSE.
#' @param nameFile nombre del fichero (tipo caracter) donde guardar los gráficos. Por defecto 'descriptive_plots.pdf'.
#' @param subtitle subtitol
#' @param color nombre del color para pintar el gráfico
#' @param rowcol c(nrows, ncols) to create a matrix of nrows x ncols plots that are filled in by row
#' @param show.lg TRUE o FALSE indica si se muestra la leyenda. Por defecto FALSE.
#' @param show.freq TRUE o FALSE indica si se muestran las frecuencias. Por defecto TRUE
#' @param cex.lab expansion factor for axis names (bar labels). (size x labels)
#' @param cex.lg character expansion factor relative to current par('cex'). Used for text, and provides the default for pt.cex and title.cex.
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],
#' 1: always horizontal, 2:always perpendicular to the axis, 3: always vertical.
#' @param do.test logical value si se quiere realizar test kruskall Wallis.
#' @export descPlot
#' @import beeswarm Hmisc
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),Y1=as.factor(rbinom(50,1,.40)),X = rnorm(50,10,1))
#' descPlot(dat = df, color = 'red', rowcol = c(1,2))
#' descPlot(dat = df, y = 'Y', color = 'red', rowcol = c(1,1))
#' descPlot(mtc_bis)
#' descPlot(dat = mtc_bis, y ='gear', rowcol = c(2,2), do.test = TRUE )
#' @keywords plots descriptive


descPlot <- function(dat, y = NULL,
                     nameFile = "descriptive_plots.pdf",
                     topdf = FALSE,
                     subtitle = NULL,
                     color = "#8D4ABA",
                     rowcol = c(3, 2),
                     show.lg = FALSE,
                     show.freq = TRUE,
                     cex.lab = 1,
                     cex.lg = 0.65,
                     cex.main = 0.8,
                     las = 0,
                     do.test = FALSE) {

    if (is.null(y)) {
        parmar <- c(5.1, 4.1, 4.1, 2.1)
    } else {
        parmar <- c(5.1, 4.1, 4.1, 7.1)
        dat[, y] <- as.factor(as.character(dat[, y]))
    }

    par(mar = parmar, mfrow = rowcol)

    if (topdf) {
        pdf(nameFile)
        par(mfrow = rowcol)
    }
    if (sum(label(dat[!names(dat) %in% y]) == "") != 0) {
        namevar <- names(dat)
    } else {
        namevar <- label(dat)
    }

    for (i in 1:dim(dat)[2]) {

        ##### variables factor
        if (ifelse(!is.null(y), names(dat)[i] != y, TRUE)) {
            if (class(dat[, i])[length(class(dat[, i]))] == "factor") {

                ## descriptiu univariat
                if (is.null(y)) {
                  ###########################
                  # if (show.lg) {
                  #   parmar <- c(5.1, 4.1, 4.1, 7.1)
                  # }
                  # op <- par(mar = parmar, xpd = TRUE)
                  # col.lev <- gg_color(length(levels(dat[, i])))
                  # tab2bar <- prop.table(table(dat[, i])) * 100
                  # try(aa <- barplot(tab2bar, xlab = "", ylab = "%",
                  #                   main = namevar[i],
                  #                   sub = ifelse(is.null(subtitle), "", subtitle),
                  #                   col = col.lev, ylim = c(0, max(tab2bar) + 6.5),
                  #   las = las, cex.names = cex.lab,
                  #   cex.main = cex.main), TRUE)
                  # if (show.freq)
                  #   try(text(aa, tab2bar + 4, labels = table(dat[, i]), cex = 0.8))
                  # if (show.lg) {
                  #   legend(length(levels(dat[, i])) + 0.7, (max(tab2bar, na.rm = T) * 0.4),
                  #          inset = c(-0.25, 0),
                  #          legend = levels(dat[, i]),
                  #          bg = "white",
                  #          fill = col.lev, cex = cex.lg,
                  #          yjust = 0, title = names(dat)[i])
                  ###########################

                    barplot_ueb(y = names(dat)[i], dat = dat,
                                title.plot = namevar[i],
                                sub.plot = ifelse(is.null(subtitle), "", subtitle),
                                las = las,
                                cex.lab = cex.lab,
                                cex.main = cex.main,
                                show.freq = show.freq,
                                show.lg = show.lg,
                                cex.lg = cex.lg)


                  #   par(op)
                  # }
                  # par(op)

                  ## descriptiu bivariat
                } else {
                  ###########################
                  # op <- par(mar = parmar, xpd = TRUE)
                  # col.lev <- gg_color(length(levels(dat[, i])))
                  # tab2bar <- prop.table(table(dat[, i], dat[, y]), 2) * 100
                  # try(aa <- barplot(tab2bar,
                  #                   xlab = y, ylab = "%", main = namevar[i],
                  #                   sub = ifelse(is.null(subtitle), "", subtitle),
                  #                   col = col.lev, las = las,
                  #                   cex.names = cex.lab, cex.main = cex.main),
                  #   TRUE)
                  #
                  # legend(length(levels(dat[, y])) + 0.7, 50, inset = c(-0.25, 0),
                  #        legend = levels(dat[, i]),
                  #        bg = "white",
                  #        fill = col.lev,
                  #        cex = cex.lg, yjust = 0.5, title = names(dat)[i])  ## ajustar llegenda y
                  # par(op)
                  ###########################
                  barplot_ueb(y = names(dat)[i], group = y, dat = dat,
                              sub.plot = ifelse(is.null(subtitle), "", subtitle),
                              las = las,
                              cex.lab = cex.lab,
                              cex.main = cex.main,
                              show.freq = show.freq,
                              show.lg = show.lg,
                              cex.lg = cex.lg,
                              do.test = do.test)
                }


                ##### variables numeriques
            } else {

                ## descriptiu univariat
                if (is.null(y)) {
                  try(hist(dat[, i], xlab = "",
                           main = namevar[i],
                           sub = ifelse(is.null(subtitle), "", subtitle),
                           col = makeTransparent(color, alpha = 0.8)), TRUE)
                  try(rug(dat[, i]))

                  ## descriptiu bivariat
                } else {
                  boxplot_bw(y = i, group = y, dat = dat,
                             title.plot = namevar[i],
                             cex.lab = cex.lab, do.test = do.test)
                }
            }


        }
    }
    if (topdf) {
        dev.off()
    }
}
