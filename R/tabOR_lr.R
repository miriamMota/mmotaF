#' A tabOR_lr Function
#'
#' Genera tabla con los coeficientes OR , intervalos de confianza y p-valores de un modelo de regresión logística
#' @param mod a fitted object of class inheriting from "glm".
#' @param xtab TRUE o FALSE, para obtener tabla en formato .tex
#' @param title if xtab = T, Character vector containing the table's caption or title.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param sz.latex A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param label Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @keywords OR regresion logistica
#' @export tabOR_lr
#' @import xtable
#' @examples
#' #tabOR_lr(glm(rnorm(50,10,1)~ as.factor(rbinom(50,1,.40))),
#' #xtab=TRUE,title='OR de los coeficientes')


tabOR_lr <- function(mod, xtab = FALSE, title = "title", xtab.type = "latex", sz.latex = "small", label = NULL) {
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
    tauORcoef <- data.frame(ORcoef, infORcoef, supORcoef, p.val)
    colnames(tauORcoef) <- c("OR", "IC inferior", "IC superior", "P.valor")
    if (xtab) {
        print(xtable(tauORcoef, caption = title, digits = 2, label = label), type = xtab.type, size = sz.latex)
    } else {
        return(tauORcoef)
    }
}
