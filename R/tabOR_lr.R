#' A tabOR_lr Function
#'
#' Genera tabla con los coeficientes OR , intervalos de confianza y p-valores de un modelo de regresión logística
#' @param mod Modelo de regresión logística ("glm")
#' @param xtab TRUE o FALSE, para obtener tabla en formato .tex
#' @param title Solo en el caso de xtab = TRUE. Cabecera de la tabla.
#' @keywords OR regresion logisitica
#' @export tabOR_lr
#' @import xtable
#' @examples
#' #tabOR_lr(glm(rnorm(50,10,1)~ as.factor(rbinom(50,1,.40))),
#' #xtab=TRUE,title="OR de los coeficientes")


tabOR_lr <- function(mod, xtab = FALSE, title = "title"){
  ORcoef <- exp(mod$coeff) ## OR 
  ic <- exp(confint(mod))
  infORcoef <- ifelse(  length(mod$coefficients) == 1, ic[1], ic[,1]) ## IC dels OR 
  supORcoef <- ifelse(  length(mod$coefficients) == 1, ic[2], ic[,2])
  p.val <- summary(mod)$coef[,4]
  tauORcoef <- data.frame(ORcoef, infORcoef,supORcoef,p.val)
  colnames(tauORcoef) <- c("OR", "IC inferior", "IC superior","P.valor")
  if (xtab)   {
    xtable(tauORcoef,caption = title,digits = 2)
  }else {return(tauORcoef)}
  
}
