#' A desc_plot Function
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
#' @export desc_plot
#' @import beeswarm Hmisc janitor
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),Y1=as.factor(rbinom(50,1,.40)),X = rnorm(50,10,1))
#' desc_plot(dat = df, color = 'red', rowcol = c(1,2))
#' desc_plot(dat = df, y = 'Y', color = 'red', rowcol = c(1,1), las = 2)
#' desc_plot(mtc_bis)
#' # desc_plot(dat = mtc_bis, y ='am', rowcol = c(2,2), do.test = TRUE, las = 2 )
#' @keywords plots descriptive


descPlot <- function(...) {
    .Deprecated("desc_plot") #include a package argument, too
    desc_plot(...)
}


desc_plot <- function(dat, y = NULL,
                      nameFile = "descriptive_plots.pdf",
                      topdf = FALSE,
                      subtitle = NULL,
                      color = "#8D4ABA",
                      rowcol = c(3, 2),
                      show.lg = FALSE,
                      show.freq = TRUE,
                      cex.lab = 1,
                      cex.lg = 0.65,
                      cex.main = 0.9,
                      cex.n = 0.5,
                      las = 0,
                      do.test = FALSE, ...) {


    par(mfrow = rowcol)

    ## eliminem columnes buides
    dat <- remove_empty(dat, which = c("cols"))

    if (topdf) {
        pdf(nameFile)
        par(mfrow = rowcol)
    }

    lbls<- Hmisc::label(dat[!names(dat) %in% y])
    namevar <- lbls
    namevar[lbls == ""] <- names(dat)[!names(dat) %in% y][lbls == ""]




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
                                cex.n = cex.n,
                                show.freq = show.freq,
                                show.lg = show.lg,
                                cex.lg = cex.lg, ...)


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
                                cex.n = cex.n,
                                do.test = do.test,
                                title.plot = namevar[i] , ...)
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
                        try(hist(dat[, i], xlab = "", breaks = breaks.units, cex.main = cex.main,
                                 main = namevar[i], freq = T, las = las, cex.axis = cex.lab,
                                 sub = ifelse(is.null(subtitle), "", subtitle),
                                 col = makeTransparent("#57ADC2", alpha = 0.8), ...), TRUE)

                    }else{
                        try(hist(dat[, i], xlab = "",
                                 main = namevar[i], cex.main = cex.main,
                                 sub = ifelse(is.null(subtitle), "", subtitle),
                                 col = makeTransparent(color, alpha = 0.8), ...), TRUE)
                        try(rug(dat[, i]))
                        try(mtext(paste0("n = ", sum(complete.cases(dat[,names(dat)[i]]))),side = 3, adj = 1,
                                      cex = cex.n))
                    }
                    ## descriptiu bivariat
                } else {
                    boxplot_bw(y = i, group = y, dat = dat, las = las,
                               title.plot = namevar[i],
                               cex.lab = cex.lab, do.test = do.test,
                               cex.main = cex.main, ...)
                    mtext(paste0("n = ",nrow(na.omit(dat[,c(names(dat)[i],y)]))),side = 3, adj = 1,
                          cex = cex.n)
                }
            }


        }
    }
    if (topdf) {
        dev.off()
    }
}
