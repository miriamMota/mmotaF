#' A tabOR_lr Function
#'
#' Genera tabla con los coeficientes OR , intervalos de confianza y p-valores de un modelo de regresión logística
#' @param mod a fitted object of class inheriting from "glm".
#' @param xtab TRUE o FALSE, para obtener tabla en formato .tex
#' @param title if xtab = T, Character vector containing the table's caption or title.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param sz.latex A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param label Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @param show.intcp TRUE o FALSE, indica si se muestra o no el intercept del modelo. En ambos casos el modelo se ha calcula con intercept. Default value is "FALSE".
#' @param show.n TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".
#' @param show.aov.pval TRUE o FALSE muestra el p-valor del modelo global. Default value is "TRUE".
#' @keywords OR regresion logistica
#' @export tabOR_lr
#' @import xtable
#' @examples
#' df <- data.frame( x = rnorm(50,10,1), y = as.factor(rbinom(50,1,.40)) )
#' mod <- glm(y ~ x, data = df, family = binomial)
#' tabOR_lr(mod, xtab = FALSE,title='OR de los coeficientes')


tabOR_lr <- function(mod,
                     xtab = FALSE,
                     title = "title",
                     xtab.type = "latex",
                     sz.latex = "small",
                     label = NULL,
                     show.intcp = FALSE,
                     show.n = TRUE,
                     show.aov.pval = TRUE) {

    ORcoef <- exp(mod$coeff)
    ic <- exp(confint(mod))
    if (length(mod$coefficients) == 1) {
        infORcoef <- ic[1]
        supORcoef <- ic[2]
    } else {
        infORcoef <- ic[, 1]
        supORcoef <- ic[, 2]
    }
    p.val <- summary(mod)$coef[, which(colnames(summary(mod)$coef) == "Pr(>|z|)")]

    if(show.intcp){
      pval_glob <- as.numeric(c(anova(mod,test = "Chisq")$Pr[2], rep("", length(p.val)-1)))
      # pval_glob <- ifelse(pval_glob == 0, "$<$ 0.01", pval_glob )
      n_mod <- as.numeric(c(length(mod$y) , rep("", length(p.val)-1)))
      tauORcoef <- data.frame(ORcoef, infORcoef, supORcoef, p.val, pval_glob, n_mod)
      colnames(tauORcoef) <- c("OR", "LowerIC", "UpperIC", "P-value", "Global P-value", "N")
    }else{
      pval_glob <- as.numeric(c(anova(mod,test = "Chisq")$Pr[2], rep("", length(p.val)-1)))
      # pval_glob <- ifelse(pval_glob == 0, "$<$ 0.01", pval_glob )
      n_mod <- as.numeric(c(length(mod$y) , rep("", length(p.val)-1)))
      tauORcoef <- data.frame(ORcoef, infORcoef, supORcoef, p.val, pval_glob, n_mod)
      tauORcoef <- tauORcoef[!rownames(tauORcoef) %in% "(Intercept)", ]
      colnames(tauORcoef) <- c("OR", "LowerIC", "UpperIC", "P-value", "P-value (Global)", "N")
    }

    if(!show.n){
      tauORcoef <- tauORcoef[,!names(tauORcoef)%in%("N")]
    }

    if(!show.aov.pval){
      tauORcoef <- tauORcoef[,!names(tauORcoef)%in%("P-value (Global)")]
    }


    if (xtab) {
        print(xtable(tauORcoef, caption = title, digits = 2, label = label), type = xtab.type, size = sz.latex)
    } else {
        return(tauORcoef)
    }
}
