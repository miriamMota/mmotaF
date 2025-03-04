#' Genera gráficos descriptivos con ggplot2
#'
#' Esta función crea gráficos descriptivos univariados y bivariados en función de las variables proporcionadas.
#' Se pueden generar histogramas, gráficos de barras, diagramas de caja y otras representaciones gráficas.
#'
#' @param dat Data frame con los datos a analizar.
#' @param covariates Vector de nombres de variables a analizar. Si es NULL, se analizan todas las variables.
#' @param frml Fórmula opcional para especificar la variable dependiente y las covariables.
#' @param y Nombre de la variable dependiente (opcional para análisis bivariado).
#' @param nameFile Nombre del archivo PDF donde se guardarán los gráficos (si `topdf = TRUE`).
#' @param topdf Lógico. Si es TRUE, guarda los gráficos en un archivo PDF.
#' @param list.plots Lógico. Si es TRUE, devuelve una lista con los gráficos generados.
#' @param color Color para los gráficos de variables numéricas.
#' @param rowcol Vector de longitud 2 especificando el número de filas y columnas en la salida de gráficos.
#' @param show.freq Lógico. Si es TRUE, muestra frecuencias en gráficos de barras.
#' @param bw Lógico. Si es TRUE, agrega dispersión en gráficos de cajas.
#' @param size.n Tamaño del texto para `n` en los gráficos.
#' @param size.freq Tamaño del texto para frecuencias en gráficos de barras.
#' @param size.title Tamaño del título de los gráficos.
#' @param size.pval Tamaño del texto del p-valor en gráficos bivariantes.
#' @param show.pval Lógico. Si es TRUE, muestra el p-valor en gráficos bivariantes.
#' @param show.n Lógico. Si es TRUE, muestra el número de observaciones en los gráficos.
#' @param show.na Lógico. Si es TRUE, incluye valores NA en los gráficos.
#' @param legend.position Posición de la leyenda en gráficos bivariantes.
#'
#' @return Si `list.plots` es TRUE, devuelve una lista con los gráficos generados. Si `topdf` es TRUE, guarda los gráficos en un archivo PDF.
#'         Si ambos son FALSE, imprime los gráficos en la consola.
#'
#' @import ggplot2 dplyr Hmisc purrr tidyr scales
#' @export
#'
#' @examples
#' # Generar gráficos descriptivos para todas las variables
#' #desc_ggplot(mtcars)
#'
#' # Especificar variables de análisis
#' #desc_ggplot(mtcars, covariates = c("mpg", "hp"))
#'
#' # Guardar gráficos en un PDF
#' #desc_ggplot(mtcars, topdf = TRUE, nameFile = "graficos.pdf")


desc_ggplot <- function(dat,
                        covariates = NULL,
                        frml = NULL,
                        y = NULL,
                        nameFile = "descriptive_plots.pdf",
                        topdf = FALSE,
                        list.plots = FALSE,
                        color = "#8D4ABA",
                        rowcol = c(1, 1),
                        show.freq = TRUE,
                        bw = TRUE,
                        size.n = 3,
                        size.freq = 2.5,
                        size.title = 10,
                        size.pval = 3,
                        show.pval = FALSE,
                        show.n = TRUE,
                        show.na = FALSE,
                        legend.position = "right",
                        ...) {

  graficos <- list()

  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  ## en el cas de que seleccionem variables a analitzar reduim bbdd a variables necesaies
  if (!is.null(covariates)) {
    dat %<>% select(any_of(c(covariates,y)))
  }

  # if (sum(!is.na(dat[, y])) > bw.n.max) {
  # bw <- FALSE
  # }

  ## eliminem columnes buides
  dat <- remove_empty(dat, which = c("cols"))

  ## Labels i names  de les a gràficar excepte la que crea grup.
  lbls <- Hmisc::label(dat[!names(dat) %in% y])
  lbls[lbls == ""] <- names(dat)[!names(dat) %in% y][lbls == ""]
  namevar <- names(lbls)

  lbl_y <- ifelse(Hmisc::label(dat[,y]) == "",y, Hmisc::label(dat[,y]))



  common_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = size.title),
      panel.grid.major = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )



  for (i in seq_along(namevar)) {

    if (class(dat[, namevar[i]])[length(class(dat[, namevar[i]]))] == "factor") {



      ## descriptiu univariat
      if (is.null(y)) {

        dd <- if (show.na) dat else dat %>% select(any_of(c(namevar[i]))) %>% na.omit()

        # Crear el gráfico de barras
        graficos[[i]] <- ggplot(dd, aes_string(x = namevar[i])) +
          geom_bar(aes(y = (..count..)/sum(..count..) * 100, fill = !!sym(namevar[i])), color = "black", linewidth = 0.3) +  # Barras en porcentaje
          labs(title = lbls[namevar[i]], x = NULL, y = "%") +  # Etiquetas
          common_theme +
          theme(legend.position = "none") +
          scale_x_discrete(labels = wrap_format(10))

        if(show.freq) graficos[[i]] <- graficos[[i]] + geom_text(stat = "count", aes(y = (..count..)/sum(..count..) * 100, label = ..count..), vjust = -0.5,size = size.freq)   # Agregar valores sobre las barras

        if(show.n) graficos[[i]] <- graficos[[i]] + annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dd[,namevar[i]]))), hjust = 1.2, vjust = 1.5, size = size.n)

        ## descriptiu bivariat
      } else {

        dd <- if (show.na) dat else dat %>% select(any_of(c(namevar[i], y))) %>% na.omit()



        # Crear gráfico de barras apiladas con porcentajes
        graficos[[i]] <- ggplot(dd, aes_string(x = y, fill = namevar[i])) +
          geom_bar(position = "fill", linewidth = 0.3) +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +  # Cortar etiquetas largas
          labs(title = lbls[namevar[i]], x = lbl_y, y = "%", fill = "") +
          common_theme +
          theme(legend.position = legend.position,  # Mueve la leyenda abajo
                legend.spacing.y = unit(0.5, "cm")  # Espaciado entre elementos de la leyenda
          ) +
          ggtitle(label = str_wrap(lbls[namevar[i]], width = 40))

        # Agregar etiquetas con el porcentaje dentro de cada región
        if (show.freq) {
          graficos[[i]] <- graficos[[i]] +
            geom_text(
              aes(label = scales::percent(..count.. / tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
              stat = "count",
              position = position_fill(vjust = 0.5),
              color = "black",
              size = size.freq
            )
        }

        if (show.n) {
          df_counts <- dd %>%
            count(!!sym(y))  # Contar observaciones por grupo en el eje x

          graficos[[i]] <- graficos[[i]] +
            geom_text(data = df_counts,
                      aes(x = !!sym(y),
                          y = 1.01,  # Ubicación encima de la barra (ajusta según el gráfico)
                          label = paste0("n = ", n)),
                      inherit.aes = FALSE,
                      size = (size.n-0.5),
                      vjust = 0,  # Coloca el texto arriba
                      color = "black")

          total_n <- sum(df_counts$n)  # Calcula la n total
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = length(unique(dd[[y]])) + 0.5, y = 1.1,
                     label = paste0("n = ", total_n), hjust = 1, size = (size.n + 0.5))
        }

        # Agregar p-valor si es necesario
        if (show.pval) {
          info_test <- test_categoricas(data = dd, factor1 = y, factor2 = namevar[i])
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = 1, y = 1.06,
                     label = paste(info_test$test, "p:", format.pval(info_test$pvalor, digits = 3, eps = 0.001)),
                     hjust = 0, size = size.pval)
        }
      }
      ##### variables caracter
    } else if (class(dat[, namevar[i]])[length(class(dat[, namevar[i]]))] == "character") {
      message(paste("La variable",namevar[i], "es tipo caracter y no se ha realizado gráfico"))

      ##### variables dates
    } else if (class(dat[,namevar[i]])[length(class(dat[,namevar[i]]))] == "Date" |
               class(dat[,namevar[i]])[length(class(dat[,namevar[i]]))] == "POSIXt"){
      library(ggplot2)
      # Gráfico de histograma para visualizar la frecuencia de fechas
      ggplot(dat, aes_string(x = namevar[i])) +
        geom_histogram(binwidth = 86400, fill = "steelblue", color = "black", linewidth = 0.3) +  # binwidth = 86400 (1 día en segundos)
        labs(title = lbls[namevar[i]], x = "Fecha", y = "Frecuencia") +
        # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "5 days") +
        theme_minimal() +
        common_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dat %>% select(namevar[i])))), hjust = 1.2, vjust = 1.5, size = size.n)

      ##### variables numeriques
    }else {

      if (is.null(y)) {
        ################# HISTOGRAMA
        ######### UNI
        graficos[[i]] <- ggplot(dat, aes_string(x = namevar[i])) +
          geom_histogram( fill = color, color = "black", alpha = 0.6, linewidth = 0.3) + # Histograma
          geom_rug(sides = "b") +  # Agregar rayitas (rug plot) en la base
          labs(title = lbls[namevar[i]], x = NULL, y = "Frequency") +
          annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dat %>% select(namevar[i])))),
                   hjust = 1.2, vjust = 1.5, size = size.n) +
          common_theme

      } else {

        ######## BIVARIANT
        # Gráfico con ggplot2
        if(show.pval)       info_test <- test_numericas(factor_col = y,numerica_col = namevar[i] ,data = dat,parametrico = FALSE)

        graficos[[i]] <- ggplot(dat, aes_string(x = y, y = namevar[i], color = y)) +
          geom_boxplot(fill = "gray80", alpha = 0.5, outlier.shape = NA) +  # Caja gris sin outliers
          labs(title = ifelse(show.pval, paste(lbls[namevar[i]],".",info_test$test,  "p:", format.pval(info_test$pvalor, digits = 3, eps = 0.001)),
                              lbls[namevar[i]]),  # Título con etiqueta de Hmisc
               x = lbl_y,
               y = "") +
          common_theme +
          theme(legend.position = "none")

        if(bw) graficos[[i]] <- graficos[[i]] + geom_jitter(width = 0.2, height = 0, alpha = 0.7)   # Puntos dispersos

        if(show.n){
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = Inf, y = Inf,
                     label = paste("n =", nrow(dat %>% select(any_of(c(namevar[i],y))) %>% na.omit())), hjust = 1.2, vjust = 1.5, size = size.n)
        }

      }

    }

    if(!is.null(options.ggplot)) graficos[[i]] <- graficos[[i]] + options.ggplot
  }
  graficos <- graficos[!sapply(graficos, is.null)]

  if (topdf) {
    ggplot_to_pdf(graficos, row = rowcol[1], col = rowcol[2], name.file = nameFile)
  } else if (list.plots){
    return(graficos)
  } else {
    return(walk(graficos, print))
  }
}

