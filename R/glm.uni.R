#' A glm.uni Function
#'
#' Genera tabla con los coeficientes OR , intervalos de confianza y p-valores de un modelo de regresión logística
#' @param y response variable. Variable factor con 2 categorias
#' @param var2test nombre de las variables a testar mediante regresión logítica
#' @param format a character string; possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format; if format is a function, it must return a character string
#' @param size A numeric input for table font size
#' @param caption Character vector containing the table's caption or title. Default value is "Univariate logistic regression"
#' @param show.n TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".
#' @param show.aov.pval TRUE o FALSE muestra el p-valor del modelo global. Default value is "TRUE".
#' @param group TRUE o FALSE mostrar variables agrupadas en la tabla
#' @keywords OR summary regresion logistic
#' @export glm.uni
#' @import kableExtra knitr magrittr survival
#' @examples
#' df <- data.frame( score = rnorm(50,10,1), hores = rnorm(50,10,1), mort = as.factor(rbinom(50,1,.40)) )
#' glm.uni(y = "mort", var2test = c("score", "hores"), data = df, format = "latex", size = 10)


# y = "MORT"
# var2test <- names(dat)[!names(dat) %in% c("MORT", "NUMPACIE")]
# data <- dat
# require(kableExtra)
# require(knitr)
#
# aa <- glm.uni(y = "MORT", var2test = c("TABAC", "EDAT", "SBP"), data = dat, format = "latex", size = 10)




glm.uni <- function(y, var2test, var2match = NULL, data,
                    size = 8.5,
                    format = "latex",
                    caption= "Univariate logistic regression",
                    show.n = TRUE,
                    show.aov.pval = TRUE,
                    group = TRUE){

  if (class(data[,y]) != "factor") stop("variable 'y' must be factor")
  if (length(levels(data[,y])) != 2) stop("variable 'y' must have two levels")

  for (i in seq_along(var2test)){
    if(class(data[,var2test[i]])[1] == "factor" ) data[,var2test[i]] <- factor(data[,var2test[i]])
  }


  unimod <- lapply(var2test,
                   function(var) {

                     if (is.null(var2match)) {
                       formula <- as.formula(paste(y," ~", var))
                       res.logist <- glm(formula, data = data[complete.cases(data[,y]),], family = binomial)
                     }else{
                       formula <- as.formula(paste( "as.numeric(",y,")~", var," + strata(",var2match,")"))
                       res.logist <-  survival::clogit(formula,  data[complete.cases(data[,y]),])
                     }
                     res_lm <- round(tabOR_lr(mod = res.logist,xtab = FALSE, title = var, show.intcp = FALSE, show.n = show.n, show.aov.pval = show.aov.pval),2)
                     rownames(res_lm) <- gsub(var,"", rownames(res_lm))
                     res_lm[is.na(res_lm)] <- ""
                     res_lm <- cbind(varlev = paste0(var,".",rownames(res_lm)), res_lm)
                   })
  names(unimod) <- var2test
  unimod_df <- do.call(rbind, unimod)
  unimod_df <- cbind(Variable  = gsub("^.*\\.","",unimod_df$varlev), unimod_df)
  # rownames(unimod_df) <- gsub("^.*\\.","",rownames(unimod_df))
  ## unimod_df$`P-value (Global)` <- as.numeric(unimod_df$`P-value (Global)`)
  ## unimod_df$`P-value (Global)` <- ifelse(unimod_df$`P-value (Global)` < 0.0001,"0.0001", unimod_df$`P-value (Global)`)

  if (group) {
    xtab <- kable(unimod_df[,!names(unimod_df) %in% c("varlev")], format = format, booktabs = T,caption = caption,  row.names = FALSE, longtable = TRUE) %>%
      kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size = size) %>%
      column_spec(which(names(unimod_df) == "Global P-value") - 1, bold = T)  %>%
      group_rows(index = eval(parse(text =   paste0("c(",paste0("'",names(unimod), "'" , " = ",unlist(lapply(unimod,nrow)), collapse = ", " ), ")")   )),
                 latex_gap_space = '1em')
    # group_rows(index = c('TEMPSVIU' = 1, 'Edata' = 1, 'BMI' = 1, 'EdataDIAG' = 1, 'TABAC' = 2, 'SBP' = 1, 'DBP' = 1, 'ECG' = 2, 'CHD' = 1))
    # row_spec(which(unimod_df$`P-value (Global)` < 0.05), bold = T, color = "black", background = "#C0B2CF") %>%

  }else{
    xtab <- kable(unimod_df[,!names(unimod_df) %in% c("varlev")], format = format, booktabs = T,caption = caption, longtable = TRUE) %>%
      kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size = size) %>%
      column_spec(which(names(unimod_df) == "P-value (Global)") , bold = T)
  }

  return(list(unimod_list = unimod, xtab = xtab))
}