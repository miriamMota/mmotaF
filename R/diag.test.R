#' A diag.test Function
#'
#' mesures de clasificació
#' @param x either a character string with the name of the diagnostic test variable. (Potser una variable numerica o per exemple, una probabilitat de un model de regressio logistica)
#' @param y a  character string with the name of the variable that distinguishes healthy from diseased individuals
#' @param frml an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. Es necesario usar este parametro cuando tengamos mas de una variable explicativa.
#' @export diag.test
#' @import epiR
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' data(aSAH)
#' y <- aSAH$outcome
#' p1 <- factor(ifelse(aSAH$s100b >= 0.22, levels(aSAH$outcome)[2], levels(aSAH$outcome)[1]  ))
#' diag.test(y = y, pred = p1)
#'
#' df_pred <- data.frame(p1 = factor(ifelse(aSAH$s100b >= 0.22, levels(aSAH$outcome)[2], levels(aSAH$outcome)[1]  )),
#' p2 = factor(ifelse(aSAH$s100b >= 0.3, levels(aSAH$outcome)[2], levels(aSAH$outcome)[1]  )))
#' diag.test(y = y, pred = df_pred)
#' @return variable:  taula detallada amb totes les mesures de classificació
#' @return summary: taula detallada amb totes les mesures de classificació per a cada una de les variables
#' @keywords roc glm test


diag.test <- function(pred, y, tag.healthy = levels(y)[1] , nround = 2){

  n_var <- ifelse(is.null(ncol(pred)), 1, ncol(pred))

  sum_ac_l <- list()
  classification <- list()
  pos.class <- NA

  for (i in 1:n_var) {

    if (n_var == 1) {
      name_var <- deparse(substitute(pred))
      pred_i <- pred
    }else{
      name_var <- names(pred)[i]
      pred_i <- pred[,i]
    }
    classification[["variable"]][[name_var]] <- list()


    if (!all(levels(pred_i) %in% levels(y)) ) stop("\n ERROR: los niveles de las variables",name_var," e 'y' deben ser los mismos \n ")


    pred_i <- factor(pred_i, c(levels(pred_i)[levels(pred_i) != tag.healthy], tag.healthy  ))
    y <- factor(y, c(levels(y)[levels(y) != tag.healthy], tag.healthy  ))

    tab2test <- table(pred_i,y)
    classification[["variable"]][[name_var]][["table"]] <- tab2test

    epiRes <- epi.tests(tab2test)

    if (epiRes$elements$se == confusionMatrix(table(pred_i,y))$byClass[["Sensitivity"]] ) {
      positive <- confusionMatrix(table(pred_i,y))$positive
      classification[["variable"]][[name_var]][["positive.class"]] <- positive
      pos.class[i] <- positive
    }else{
      stop("Error: Problemas calculo diag test! ")
    }

    if (!identical(rownames(tab2test), colnames(tab2test))) stop("Error en l'ordre de les variables")


    ll <- list()
    ll[["Accuracy"]] <- unlist(c(epiRes$elements$diag.acc)) * 100
    ll[["Sensitivity"]] <- unlist(c(epiRes$elements$sensitivity)) * 100
    ll[["Specificity"]] <- unlist(c(epiRes$elements$specificity)) * 100
    ll[["PPV"]] <-  c(epiRes$elements$ppv, epiRes$elements$ppv.low, epiRes$elements$ppv.up) * 100
    ll[["NPV"]] <-  c(epiRes$elements$npv, epiRes$elements$npv.low, epiRes$elements$npv.up) * 100
    ll[["LRpositive"]] <-  unlist(c(epiRes$elements$lr.positive))
    ll[["LRnegative"]] <-  unlist(c(epiRes$elements$lr.negative))
    ll[["Prevalence"]] <-  unlist(c(epiRes$elements$tprev)) * 100


    res <- data.frame(matrix(unlist(ll), nrow = length(ll), byrow = T),stringsAsFactors = FALSE)
    rownames(res) <- names(ll)
    colnames(res) <- c("Value", "IC low 95%", "IC up 95% ")



    classification[["variable"]][[name_var]][[name_var]] <- res
    sum_ac_l[[name_var]] <- apply(res, 1, function(x) paste0(round(x[1],nround), " (", round(x[2],nround), "; ", round(x[3],nround),")"))
  }

  classification[["summary"]] <- list()
  ## comprovem que per a totes les variables la positive.class (es a dir, els 'casos') sigui sempre el mateix
  if (length(unique(pos.class)) == 1) {
    classification[["summary"]][["positive.class.all"]] <- unique(pos.class)
  }else{
      stop("Existen distintas 'positive.class'")
  }

  sum_ac <- data.frame(matrix(unlist(sum_ac_l), nrow = length(sum_ac_l), byrow = T),stringsAsFactors = FALSE)
  rownames(sum_ac) <- names(sum_ac_l)
  colnames(sum_ac) <- names(sum_ac_l[[1]])
  sum_ac <- t(sum_ac)
  classification[["summary"]][["table"]] <- sum_ac


  # print("Reference class is:" )
  # print(classification$summary)
  return(classification)

}

