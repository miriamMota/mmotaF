#' A desc_unimods Function
#'
#' Genera tabla con los coeficientes  , intervalos de confianza y p-valores de un modelo de regresión logística o lineal
#' @param y response variable.
#' @param var2test nombre de las variables a testar mediante regresión logítica
#' @param format a character string; possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format; if format is a function, it must return a character string
#' @param size A numeric input for table font size
#' @param caption Character vector containing the table's caption or title. Default value is "Univariate logistic regression"
#' @param show.n TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".
#' @param type regression type. "logistic" or "linealE
#' @param group TRUE o FALSE mostrar variables agrupadas en la tabla
#' @keywords OR summary regresion logistic
#' @export desc_unimods
#' @import kableExtra
#' @examples
#' # resglm <- desc_unimods(y = "am",
#' # var2test = c("mpg","cyl","disp","hp","drat","wt","qsec","vs" ) ,
#' # data = mtc_bis, format = "html", size = 10, type = "logistic")
#'


desc_unimods <- function(y, var2test, data, type = NULL,
                         size = 8.5,
                         format = "html",
                         caption = NULL,
                         show.n = TRUE,
                         group = TRUE){

  if (!is.factor(data[,y]) & type == "logistic" ){
    stop("variable 'y' must be factor")
  }

  if (is.null(type)) stop("model 'type' is needed ")
  if (length(levels(data[,y])) != 2 & type == "logistic") stop("variable 'y' must have two levels")
  if (is.null(caption)) caption <- paste("Univariate logistic regression (", y, ").",
                                         ifelse(type == "logistic", paste( "Reference level:", levels(data[,y])[1]), ""))

  unimod_df <- NULL
  for(i in seq_along(var2test)){
    # print(var_mod[i])
    frml <- as.formula(paste0(y," ~", var2test[i]))

    mod <- switch (type,
                   "logistic" = glm(frml,data =  data, family = "binomial"),
                   "linear" = lm(frml, data)
    )
    unimod_df <- rbind(unimod_df, desc_mod(mod,show.pretty = T))
  }
  xtab <- kable_ueb(unimod_df[, !names(unimod_df) %in% "vars_label"], row.names = F, digits = 3,font_size = size,
                    caption = )
  if(group){
    xtab <- xtab %>% kableExtra::group_rows(index = table(unimod_df$vars_label)[unique(as.character(unimod_df$vars_label))])
  }
  return(list( unimod_ci_df = unimod_df, xtab = xtab))
}
