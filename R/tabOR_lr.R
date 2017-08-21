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
  if(length(mod$coefficients) == 1) {
    infORcoef <- ic[1]
    supORcoef <-ic[2]
  }else{
    infORcoef <- ic[,1]
    supORcoef <- ic[,2]
  } 
  p.val <- summary(mod)$coef[,which(colnames(summary(mod)$coef) == "Pr(>|z|)")]
  tauORcoef <- data.frame(ORcoef, infORcoef,supORcoef,p.val)
  colnames(tauORcoef) <- c("OR", "IC inferior", "IC superior","P.valor")
  if (xtab)   {
    xtable(tauORcoef,caption = title,digits = 2)
  }else {return(tauORcoef)}
  
}
