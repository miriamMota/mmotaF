#' A desc_plot Function
#'
#' Genera graficos univariantes para todas las variables que se indiquen, el formato de entrada es 'data.frame'
#' @param dat data frame que contiene las variables a graficar.
#' @param covariates a character string with names of variables.
#' @param frml Right side of ~ must have the terms in an additive way, and left side of ~ must contain the name of the grouping variable or can be left in blank (in this latter case descriptives for whole sample are calculated and no test is performed).
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
#' @param bw logical value for beeswarm
#' @export descPlot
#' @export desc_plot
#' @import beeswarm Hmisc janitor
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame(Y=as.factor(rbinom(50,1,.40)),Y1=as.factor(rbinom(50,1,.40)),X = rnorm(50,10,1))
#' desc_plot(dat = df, color = 'red', rowcol = c(1,2))
#' desc_plot(dat = df, y = 'Y', color = 'red', rowcol = c(1,1), las = 2)
#' desc_plot(mtc_bis)
#' desc_plot(dat = mtc_bis, y ='am', rowcol = c(2,2), do.test = FALSE, las = 2 )
#'
#' ## Indicant variables a evaluar
#' desc_plot(covariates = c("cyl","wt"),dat = mtc_bis, rowcol = c(2,2), do.test = FALSE, las = 2 )
#' desc_plot(covariates = c("cyl","wt"),dat = mtc_bis, y ='am', rowcol = c(2,2), do.test = FALSE, las = 2 )
#' desc_plot(covariates = c("cyl","wt"),dat = mtc_bis, y ='am', rowcol = c(2,2), do.test = FALSE, las = 2,bw = FALSE )
#'
#' # Indicanta variables a evaluar mitjançant formula
#' desc_plot(frml =  ~ cyl + wt ,dat = mtc_bis, rowcol = c(2,2), do.test = FALSE, las = 2 )
#' desc_plot(frml = am ~ cyl + wt ,dat = mtc_bis, rowcol = c(2,2), do.test = FALSE, las = 2 )

#' @keywords plots descriptive






descPlot <- function(...) {
    .Deprecated("desc_plot") #include a package argument, too
    desc_plot(...)
}


desc_plot <- function(dat,
                      covariates = NULL,
                      frml = NULL,
                      y = NULL,
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
                      do.test = FALSE,
                      bw = TRUE,
                      bw.n.max = 999,
                      show.n=T, ...) {
  par(mfrow = rowcol)

  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  ## en el cas de que seleccionem variables a analitzar reduim bbdd a variables necesaies
  if (!is.null(covariates)) {
    if (!is.null(y)) {
      dat <-  dat[,c(covariates,y)]
    }else{
      dat <- dat[,c(covariates)]
    }
  }


  ## eliminem columnes buides
  dat <- remove_empty(dat, which = c("cols"))

  if (topdf) {
    pdf(nameFile)
    par(mfrow = rowcol)
  }

  ## Labels i names  de les a gràficar excepte la que crea grup.
  lbls <- Hmisc::label(dat[!names(dat) %in% y])
  lbls[lbls == ""] <- names(dat)[!names(dat) %in% y][lbls == ""]
  namevar <- names(lbls)


  for (i in seq_along(namevar)) {

    ##### variables factor
    # if (ifelse(!is.null(y), names(dat)[i] != y, TRUE)) { ## comprovem que la variable que testem no es la resposta
    if (class(dat[, namevar[i]])[length(class(dat[, namevar[i]]))] == "factor") {

      ## descriptiu univariat
      if (is.null(y)) {

        barplot_ueb(y = namevar[i], dat = dat,
                    title.plot = lbls[i] ,
                    sub.plot = ifelse(is.null(subtitle), "", subtitle),
                    las = las,
                    cex.lab = cex.lab,
                    cex.main = cex.main,
                    cex.n = cex.n,
                    show.freq = show.freq,
                    show.lg = show.lg,
                    cex.lg = cex.lg)


        ## descriptiu bivariat
      } else {

        barplot_ueb(y = namevar[i], group = y, dat = dat,
                    sub.plot = ifelse(is.null(subtitle), "", subtitle),
                    las = las,
                    cex.lab = cex.lab,
                    cex.main = cex.main,
                    show.freq = show.freq,
                    show.lg = show.lg,
                    cex.lg = cex.lg,
                    cex.n = cex.n,
                    do.test = do.test,
                    title.plot = lbls[i],... )
      }


      ##### variables numeriques
    } else if (class(dat[, namevar[i]])[length(class(dat[, namevar[i]]))] == "character") {
      message(paste("La variable",namevar[i], "es tipo caracter y no se ha realizado gráfico"))
    }else {

      ## descriptiu univariat
      if (is.null(y)) {
        if (class(dat[,namevar[i]])[length(class(dat[,namevar[i]]))] == "Date" |
            class(dat[,namevar[i]])[length(class(dat[,namevar[i]]))] == "POSIXt") {
          breaks.units <- ifelse(length(unique(format(dat[,namevar[i]],"%Y"))) >= 4 , "years", "months"  )
          try(hist(dat[, namevar[i]], xlab = "", breaks = breaks.units, cex.main = cex.main,
                   main = strwrap(lbls[i],width = 40), freq = T, las = las, cex.axis = cex.lab,
                   sub = ifelse(is.null(subtitle), "", subtitle),
                   col = makeTransparent("#57ADC2", alpha = 0.8)), TRUE)

        }else{
          try(hist(dat[, namevar[i]], xlab = "",
                   main = strwrap(lbls[i],width = 40), cex.main = cex.main,
                   sub = ifelse(is.null(subtitle), "", subtitle),
                   col = makeTransparent(color, alpha = 0.8)), TRUE)
          # try(rug(dat[, i]))
          try(mtext(paste0("n = ", sum(complete.cases(dat[,namevar[i]]))),side = 3, adj = 1,
                    cex = cex.n))
        }
        ## descriptiu bivariat
      } else {
        boxplot_bw(y = namevar[i],
                   group = y,
                   dat = dat, las = las,
                   title.plot = strwrap(lbls[i],width = 40),
                   cex.lab = cex.lab,
                   do.test = do.test,
                   cex.main = cex.main,bw = bw, show.n = show.n, bw.n.max = bw.n.max)
        # mtext(paste0("n = ",nrow(na.omit(dat[,c(names(dat)[i],y)]))),side = 3, adj = 1,
        #       cex = cex.n)
      }
    }


    # }
  }
  if (topdf) {
    dev.off()
  }
}
