#' A doROC Function
#'
#' Creación de curvas ROC con regresión logística mediante el paquete pROC y validación con datos externos.
#' Para realizar las curvas ROC, en primer lugar, se modeliza - para el conjunto de datos “training set”- la variable respuesta grupo 
#' mediante regresión logística teniendo en cuenta la variable explicativa. Una vez ajustado el modelo se realiza las curva
#' ROC(representación gráfica de la sensibilidad en frente a la especificidad).
#' @param frml objeto tipo formula indicando como variable respuesta una variable binaria y como explicativa una contínua
#' @param titol tipo caracter. Título para el gráfico
#' @param validation valor lógico indicando si se entreran datos externos para testar el punto de corte elegido en el grafico
#' @param test data.frame indicando los nuevos valores de la variable explicativa
#' @param test_y vector factor indicando grupo de los nuevos individuos
#' @param col.thres color de la cruz que indica el punto de corte óptimo en el gráfico
#' @param col.ic color para el intervalo de confianza. Debe ser translucido, por lo que se puede usar la función makeTransparent para cualquier color R. 
#' @export doROC
#' @import pROC
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' y <- as.factor(c(0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1))
#' X <- c(20,2,1,2,23,3,4,5,15,-5,2,1,3,10,4,87,5,3,7,0,1,-2)
#' resROC <- doROC (frml = y ~ X, titol = "prova",
#'                  validation = TRUE,
#'                  test = data.frame(X=c(1,2,3)),
#'                  test_y = as.factor(c(0,1,1)))
#' @return auc: Area bajo la curva y correspondiente intervalo de confianza
#' @return pvalue: p-valor de la variable explicativa en el modelo de regresion logística
#' @return thres.best: punto de corte  óptimo calculado con el estad ́ıstico de Youden
#' @return misc.test: en el caso de tener datos de validación, la tasa de error de clasificacion utilizando como punto de corte el thres.best.
#' @keywords roc glm test validation


doROC <- function(frml, titol,validation = FALSE, test, test_y,col.thres = "blue", col.ic = "#aaddddAA")
{
  mod <- glm(frml , family = binomial, na.action = "na.omit")
  
  pred <- predict(mod, type = "response")  
  
  rocobj <- plot.roc(mod$y, pred,  main = titol, ci = TRUE, 
                     percent = TRUE, print.thres = "best") 
  thres <- rocobj$sensitivities - (1 - rocobj$specificities)
  thres.best <- rocobj$thresholds[which(thres == max(thres))] # threshold  de Youden
  ciobj <- ci.se(rocobj, boot.n = 100,progress = "none")
  plot(ciobj, type = "shape", col = col.ic) # plot as a blue shape
  plot(ci(rocobj, of = "thresholds", thresholds = "best",progress = "none"), col = col.thres,lwd=2)
  ic <- rocobj$ci
  auc_text <- paste(round(ic[2],2), "% (", 
                    round(ic[1],1), " - ", 
                    round(ic[3],1), "%)", sep = "")
  p.val <- summary(mod)$coef[,4][2]
  text(40, 5, 
       paste("AUC:", auc_text), #"\n p-val:", round(p.val,2)),
       #paste("AUC:", round(ic[2],1), "%", "(",round(ic[1],1), 
       #      "%", "-", round(ic[3],1), "%", ")\n p-val:", round(p.val,5)),
       cex = .8)
  
  
  if (validation) {
    pred <- predict(mod, newdata = test, type = "response")  
    tab.pred <- table(ifelse(pred >  thres.best, levels(test_y)[2],levels(test_y)[1]), test_y)
    misc.test <- 1  - (sum(diag(tab.pred))/sum(tab.pred))
    return(list(auc = auc_text, pvalue = p.val, thres.best = thres.best, misc.test = misc.test))
  }
  if (!validation) return(list(auc = auc_text, pvalue = p.val, thres.best = thres.best, levels = rocobj[15], cases = rocobj[6], controls = rocobj[7] ))
}


