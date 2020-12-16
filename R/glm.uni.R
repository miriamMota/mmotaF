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
#' @import kableExtra knitr magrittr survival dplyr
#' @examples
#' # resglm <- glm.uni(y = "am",
#' # var2test = c("mpg","cyl","disp","hp","drat","wt","qsec","vs" ) ,
#' # data = mtc_bis, format = "html", size = 10)
#'

glm.uni <- function(y, var2test, var2match = NULL, data,
                    size = 8.5,
                    format = "html",
                    caption = NULL,
                    show.n = TRUE,
                    show.aov.pval = FALSE,
                    group = TRUE,
                    nround = 3){

  if (!is.factor(data[,y]) ){
    stop("variable 'y' must be factor")
  }else{
      data[,y] <- factor_ueb(data[,y])
    }

  if (length(levels(data[,y])) != 2) stop("variable 'y' must have two levels")
  if (is.null(caption)) caption <- paste("Univariate logistic regression (", y, "). Reference level:", levels(data[,y])[1])

  # for (i in seq_along(var2test)) {
  #   if (class(data[,var2test[i]])[length(class(data[,var2test[i]]))] == "factor" ) data[,var2test[i]] <- factor(data[,var2test[i]])
  # }
  mods <- lapply(var2test,
                 function(var) {

                   if (is.null(var2match)) {
                     formula <- as.formula(paste(y," ~", var))
                     res.logist <- glm(formula, data = data[complete.cases(data[,y]),], family = binomial)
                   }else{
                     formula <- as.formula(paste( "as.numeric(",y,")~", var," + strata(",var2match,")"))
                     res.logist <-  survival::clogit(formula,  data[complete.cases(data[,y]),])
                   }
                   res_lm <- desc_mod(mod = res.logist,xtab = FALSE, title = var, show.intcp = FALSE, show.n = show.n,
                                      show.aov.pval = show.aov.pval)
                   rownames(res_lm) <- gsub(var,"", rownames(res_lm))
                   res_lm[is.na(res_lm)] <- ""
                   res_lm <- cbind(varlev = paste0(var,".",rownames(res_lm)), res_lm)
                   return(list(mod = res.logist, mod.res = res_lm))
                 })
  unimod <- lapply(mods,function(x)x[[2]])
  names(unimod) <- var2test
  for (i in seq_along(unimod))   unimod[[i]] <- cbind(unimod[[i]], Variable = names(unimod)[[i]] )

  glmmod <- lapply(mods,function(x)x[[1]])
  names(glmmod) <- var2test

  unimod_df <- do.call(rbind, unimod)
  # unimod_df <- cbind(Level  = gsub("^.*\\.","",unimod_df$varlev), unimod_df)
  # unimod_df <- cbind(Variable = rownames(unimod_df), unimod_df[,!names(unimod_df) %in% "Variable"])


  # rownames(unimod_df) <- as.character(rownames(unimod_df))
  # rownames(unimod_df)[Hmisc::label(data[,rownames(unimod_df)]) != ""] <-  Hmisc::label(data[,rownames(unimod_df)])[Hmisc::label(data[,rownames(unimod_df)]) != ""]

  unimod_df <- unimod_df %>%  mutate_if(is.numeric, round,nround)

  if (group) {
    tab_group <- table(unimod_df$Variable)[unique(as.character(unimod_df$Variable))]
    xtab <- kable_ueb(unimod_df[,!names(unimod_df) %in% c("varlev", "Variable")], format = format, booktabs = T,caption = caption,  row.names = FALSE,
                      longtable = TRUE, position = "left")  %>%
      column_spec(which(names(unimod_df) == "Global P-value") - 1, bold = T)  %>%
      kableExtra::group_rows(index = tab_group,latex_gap_space = '1em')
      # kableExtra::group_rows(index = eval(parse(text = paste0("c(",paste0("'",names(unimod), "'" , " = ",unlist(lapply(unimod,nrow)),
      #                                                                     collapse = ", " ), ")")   )),latex_gap_space = '1em')
  }else{
    xtab <- kable_ueb(unimod_df[,!names(unimod_df) %in% c("varlev")],
                      format = format, caption = caption, longtable = TRUE,
                      position = "left", row.names = FALSE) %>%
      column_spec(which(names(unimod_df) == "P-value (Global)") ,
                  bold = T)
  }

  return(list(unimod_list = glmmod, unimod_ci_list = unimod, unimod_ci_df = unimod_df, xtab = xtab))
}

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

