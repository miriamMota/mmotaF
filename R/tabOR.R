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
#' # tabOR_lr(glm(c(0,0,0,1,1)~ c(1,2,3,20,12)))
#' # tabOR_lr(glm(c(0,0,0,1,1)~ c(1,2,3,20,12)),xtab=TRUE,title="OR de los coeficientes")

############################
## Miriam Mota Foix
## 24.11.2015
############################

############################
## tabOR
############################
###
### mod = model de regressió logística (sense el summary)
###
### Generar taula amb els coeficients OR , intervals de confiança i p-valors
###
############################

tabOR_lr <- function(mod, xtab=FALSE, title = "title"){
  ORcoef <- exp(mod$coeff) ## OR
  infORcoef <- exp(confint(mod))[,1] ## IC dels OR
  supORcoef <- exp(confint(mod))[,2]
  p.val <- summary(mod)$coef[,4]
  tauORcoef <- data.frame(ORcoef, infORcoef,supORcoef,p.val)
  colnames(tauORcoef) <- c("OR", "IC inferior", "IC superior","P.valor")
  if (xtab)   {
    xtable(tauORcoef,caption = title,digits = 2)
  }else{ return(tauORcoef)}

}


