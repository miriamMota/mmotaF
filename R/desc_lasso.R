#' A desc_lasso Function
#'
#' Genera resumen para los coeficientes de laso distintos de 0 ( o todos)
#' @param glmmod a fitted object of class inheriting from lasso
#' @param s Value(s) of the penalty parameter lambda at which predictions are required. Default is the entire sequence used to create the model.
#' @param y character variable. Outcome variable name
#' @param show.all TRUE or FALSE. show all coefficients. Default value is dalse
#' @param data  data frame containing the outcome variable
#' @keywords lasso
#' @export desc_lasso
#' @import Hmisc stringr
#' @examples



desc_lasso <- function(glmmod, s, y = NULL, data, show.all = FALSE){

  #nous objectes
  results <- list()

  #comprovacions
  if(!any( class(glmmod) == "glmnet")) stop("Class of glmmod should be 'glmnet'")


  ### summary lasso
  lasso_coef <- coef(glmmod,s=s)
  df_lasso <- data.frame(
    feature=rownames(lasso_coef),
    coeficient=lasso_coef[1:length(rownames(lasso_coef))])

  vars_mod <- all.vars(glmmod$terms) #totes les variables del model

  if(any(Hmisc::label(dat[,vars_mod])== "")){
    Hmisc::label(dat[,vars_mod], self = F)[  Hmisc::label(dat[,vars_mod])== ""] <- vars_mod[  Hmisc::label(dat[,vars_mod]) == ""]
  }
  if(any(table(Hmisc::label(dat[,vars_mod])) >1) ) stop("There are more than one variable with same label")

  # Extraccio dels noms originals de les variables
  matches <- stringr::str_c(vars_mod, collapse ="|")
  vars_name <- stringr::str_extract_all(df_lasso$feature, matches, simplify = T)[,1]
  var_label <- c("Intercept", Hmisc::label(dat[,vars_name[!vars_name %in% ""]]))
  levs <- stringr::str_replace_all(df_lasso$feature,vars_name,"")
  df_lasso <- cbind(vars_name, var_label, levs, df_lasso)
  results$df_lasso <- df_lasso

  df_lasso_selected <- df_lasso[which(df_lasso$coeficient != 0), ]
  results$df_lasso_selected <- df_lasso_selected


  if(show.all) {
    results$all_lasso <- df_lasso
    results$all_lasso_ht <- kable_ueb(df_lasso[,!names(df_lasso) %in% c("vars_name", "feature", "var_label")], row.names = FALSE, digits = 3) %>%
      kableExtra:: group_rows( index =  table(df_lasso$var_label)[unique(as.character(df_lasso$var_label))])
  }



  results$lasso_ht <- kable_ueb(df_lasso_selected[,!names(df_lasso_selected) %in% c("vars_name", "feature", "var_label")], row.names = FALSE, digits = 3) %>%
    kableExtra:: group_rows( index =  table(df_lasso_selected$var_label)[unique(as.character(df_lasso_selected$var_label))])



  results$vars_selected <- unique(df_lasso_selected$vars_name[!df_lasso_selected$vars_name%in% ""])
  ## creem la formula amb els coeficients diferents de 0
  if(!is.null(y)){results$formul <- as.formula(paste0(y," ~", paste0(results$vars_selected, collapse = "+"))) }



  return(results)

}

