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
#' @param at.text if do.test TRUE, give location of each string in user coordinates. If the component of at corresponding to a particular text item is not a finite value (the default), the location will be determined by adj.
#' @export descPlot
#' @import beeswarm Hmisc
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),Y1=as.factor(rbinom(50,1,.40)),X = rnorm(50,10,1))
#' descPlot(dat = df, color = 'red', rowcol = c(1,2))
#' descPlot(dat = df, y = 'Y', color = 'red', rowcol = c(1,1), las = 2)
#' descPlot(mtc_bis)
#' descPlot(dat = mtc_bis, y ='gear', rowcol = c(2,2), do.test = TRUE, las = 2 )
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
                     do.test = FALSE,
                     at.text = 1) {


     par(mfrow = rowcol)

    if (topdf) {
        pdf(nameFile)
        par(mfrow = rowcol)
    }
    if (sum(Hmisc::label(dat[!names(dat) %in% y]) == "") != 0) {
        namevar <- names(dat)
    } else {
        namevar <- Hmisc::label(dat)
    }

    for (i in 1:dim(dat)[2]) {

        ##### variables factor
        if (ifelse(!is.null(y), names(dat)[i] != y, TRUE)) { ## comprovem que la variable que testem no es la resposta
            if (class(dat[, i])[length(class(dat[, i]))] == "factor") {

                ## descriptiu univariat
                if (is.null(y)) {

                    barplot_ueb(y = names(dat)[i], dat = dat,
                                title.plot = namevar[i],
                                sub.plot = ifelse(is.null(subtitle), "", subtitle),
                                las = las,
                                cex.lab = cex.lab,
                                cex.main = cex.main,
                                show.freq = show.freq,
                                show.lg = show.lg,
                                cex.lg = cex.lg)


                  ## descriptiu bivariat
                } else {

                  barplot_ueb(y = names(dat)[i], group = y, dat = dat,
                              sub.plot = ifelse(is.null(subtitle), "", subtitle),
                              las = las,
                              cex.lab = cex.lab,
                              cex.main = cex.main,
                              show.freq = show.freq,
                              show.lg = show.lg,
                              cex.lg = cex.lg,
                              do.test = do.test,
                              at.text = at.text)
                }


                ##### variables numeriques
            } else if (class(dat[, i])[length(class(dat[, i]))] == "character") {
              message(paste("La variable",names(dat)[i], "es tipo caracter y no se ha realizado gráfico"))
              }else {

                ## descriptiu univariat
                if (is.null(y)) {
                  if (class(dat[,i])[length(class(dat[,i]))] == "Date" |
                      class(dat[,i])[length(class(dat[,i]))] == "POSIXt") {
                    breaks.units <- ifelse(length(unique(format(dat[,i],"%Y"))) >= 4 , "years", "months"  )
                    try(hist(dat[, i], xlab = "", breaks = breaks.units,
                             main = namevar[i], freq = T, las = las, cex.axis = cex.lab,
                             sub = ifelse(is.null(subtitle), "", subtitle),
                             col = makeTransparent("#57ADC2", alpha = 0.8)), TRUE)

                  }else{
                  try(hist(dat[, i], xlab = "",
                           main = namevar[i],
                           sub = ifelse(is.null(subtitle), "", subtitle),
                           col = makeTransparent(color, alpha = 0.8)), TRUE)
                  try(rug(dat[, i]))
                  }
                  ## descriptiu bivariat
                } else {
                  boxplot_bw(y = i, group = y, dat = dat, las = las,
                             title.plot = namevar[i],
                             cex.lab = cex.lab, do.test = do.test,
                             at.text = at.text, cex.main = cex.main)
                }
            }


        }
    }
    if (topdf) {
        dev.off()
    }
}
