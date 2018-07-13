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
#' resglm <- glm.uni(y = "am",
#' var2test = c("mpg","cyl","disp","hp","drat","wt","qsec","vs" ) ,
#' data = mtc_bis, format = "latex", size = 10)
#'
#' ## plots
#'
# # coef one variable
# cp <- coefplot(resglm$unimod_list$wt, intercept = F)
# cp
#
# # OR one variable
# bk <- seq(-10,10, by = 2)
# cp + scale_x_continuous(breaks = bk, labels = round(exp(bk),2), name = "OR")
#'
# # coef all variables
# mp <- multiplot(resglm$unimod_list, intercept = F, innerCI = 1, outerCI = 1)
# mp
#
# # coef all variables
# mp + scale_x_continuous(breaks = bk, labels = round(exp(bk),2), name = "OR") +


coefplot.data.frame(model = modelCI, title = title,
                    xlab = xlab, ylab = ylab, lwdInner = lwdInner, lwdOuter = lwdOuter,
                    pointSize = pointSize, color = color, cex = cex, textAngle = textAngle,
                    numberAngle = numberAngle, zeroColor = zeroColor, zeroLWD = zeroLWD,
                    outerCI = outerCI, innerCI = innerCI, multi = FALSE,
                    zeroType = zeroType, numeric = numeric, fillColor = fillColor,
                    alpha = alpha, horizontal = horizontal, facet = facet,
                    scales = scales)



glm.uni <- function(y, var2test, var2match = NULL, data,
                    size = 8.5,
                    format = "latex",
                    caption= "Univariate logistic regression",
                    show.n = TRUE,
                    show.aov.pval = TRUE,
                    group = TRUE){

  if (class(data[,y]) != "factor") stop("variable 'y' must be factor")
  if (length(levels(data[,y])) != 2) stop("variable 'y' must have two levels")

  for (i in seq_along(var2test)) {
    if (class(data[,var2test[i]])[length(class(data[,var2test[i]]))] == "factor" ) data[,var2test[i]] <- factor(data[,var2test[i]])
  }


  mods <- lapply(var2test,
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
                     return(list(mod = res.logist, mod.res = res_lm))
                   })
  unimod <- lapply(mods,function(x)x[[2]])
  names(unimod) <- var2test

  glmmod <- lapply(mods,function(x)x[[1]])
  names(glmmod) <- var2test

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

  return(list(unimod_list = glmmod, unimod_ci_list = unimod, unimod_ci_df = unimod_df, xtab = xtab))
}
