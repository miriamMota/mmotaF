#' A desc_mod Function
#'
#' Genera tabla resumen para modelos lineales,logisticos y cox
#' @param mod a fitted object of class inheriting from "glm", "clogit","cox" or "lm".
#' @param xtab TRUE o FALSE, para obtener tabla en formato .tex
#' @param title if xtab = T, Character vector containing the table's caption or title.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param sz.latex A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param label Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @param show.intcp TRUE o FALSE, indica si se muestra o no el intercept del modelo. En ambos casos el modelo se ha calcula con intercept. Default value is "FALSE".
#' @param show.n TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".
#' @param show.aov.pval TRUE o FALSE muestra el p-valor del modelo global. Default value is "TRUE".
#' @param show.pretty TRUE o FALSE muestra las 'labels' de las variables. Solo funciona para lm y glm . Default value is "FALSE".
#' @param group_rw TRUE o FALSE  agrupa las filas por variables. Default value is "FALSE".
#' @param row.names TRUE or FALSE. Show or not rownames
#' @keywords OR regresion logistica
#' @export tabOR_lr
#' @export desc_mod
#' @import xtable dplyr papeR
#' @examples
#'

tabOR_lr <- function(...) {
  .Deprecated("desc_mod") #include a package argument, too
  desc_mod(...)
}

desc_mod <- function(mod,
                     xtab = FALSE,
                     title = "Model summary",
                     xtab.type = "latex",
                     sz.latex = "small",
                     font_size = 13,
                     show.pretty = FALSE,
                     group_rw = FALSE,
                     show.intcp = FALSE,
                     show.n = TRUE,
                     show.aov.pval = TRUE,
                     row.names = TRUE) {


  type_mod <-  switch(class(mod)[1],
                      glm= 'Odds Ratio',
                      clogit='Hazard Ratio',
                      lm = "Estimate",
                      coxph = "Hazard Ratio")
  pret_mod <- papeR::prettify(summary(mod))
  names(pret_mod)[names(pret_mod) == " "] <- "Variable"
  res <- pret_mod[, c("Variable", type_mod, "CI (lower)", "CI (upper)", grep("Pr", names(pret_mod), value = T) ) ]
  rownames(res) <- res$Variable




  if(!show.intcp){
    res <- res %>% dplyr::filter(Variable != "(Intercept)" )
  }

  #P.valueGlobal
  res[1,"P-value (Global)"] <- na.omit(anova(mod,test = "Chisq")$Pr)[1]
  #N total
  res[1,"N"] <- nobs(mod)
  # colnames(tauORcoef) <- c("OR", "LowerIC", "UpperIC", "P-value", "P-value (Global)", "N")




  if (!show.n) {  res <- res[,!names(res) %in% ("N")]  }

  if (!show.aov.pval) {    res <- res[,!names(res) %in% ("P-value (Global)")]  }


  if (show.pretty){
    if(class(mod)[1] == "glm" | class(mod)[1] == "lm") {
      vars_mod <- get.vars(alias(mod)$Model)[-1]
      Hmisc::label(mod$model, self = F)[Hmisc::label(mod$model) == ""] <- names(mod$model)[Hmisc::label(mod$model)==""]
      label_var <- Hmisc::label(mod$model)[-1]
    }else{
      vars_mod <- attr(terms(mod),"term.labels")
    }

    matches <- stringr::str_c(vars_mod, collapse ="|")
    vars_name <- stringr::str_extract_all(res$Variable, matches, simplify = T)[,1]
    res <- tibble::add_column(res,vars_name,.before = "Variable")

    if(class(mod)[1] == "glm" | class(mod)[1] == "lm") {
      vars_label <- c(if(show.intcp) "Intercept",label_var)
      res <- tibble::add_column(res,vars_label,.before = "Variable")
    }
    levs <- stringr::str_replace_all(res$Variable,vars_name,"")
    res <- tibble::add_column(res,levs,.before = "Variable")

    res <- res %>% select(- Variable,-vars_name)

  }

  if (xtab) {
    if(group_rw) {
      kable_ueb(res[,!names(res)%in% c("var_name", "vars_label")],
                caption = title, row.names = row.names) %>%
        kableExtra::group_rows(index = table(res$vars_label)[unique(as.character(res$vars_label))])
    }else{
      kable_ueb(res, caption = title, row.names = row.names)
    }

  } else {
    return(res)
  }
}
