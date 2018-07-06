#' A doROC Function
#'
#' Creación de curvas ROC (representación gráfica de la sensibilidad en frente a la especificidad) con o sin regresión logística mediante el paquete pROC y optimalCutpoints.
#' Se calcula el punto de corte optimo mediante youden y se obtienen las medidas de clasificacion.
#' @param frml an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. Es necesario usar este parametro cuando tengamos mas de una variable explicativa.
#' @param x either a character string with the name of the diagnostic test variable. (Potser una variable numerica o per exemple, una probabilitat de un model de regressio logistica)
#' @param group a  character string with the name of the variable that distinguishes healthy from diseased individuals
#' @param tag.healthy IMPORTANTE! the value codifying healthy individuals in the status variable. Por defecto nivel de referencia levels(dat[,group])[1]
#' @param dat data frame containing the variables in the formula.
#' @param modGLM OBLIGATORIO! Valor logico que indica si se realiza regresion logistica. En el caso de indicar TRUE, el 'thres.best' indicara el punto de corte como probabilidad de predicción. En el caso de indicar FALSE, 'cutoff' nos indicara el punto de corte real en la variable. Es necesario indicar TRUE cuando querramos evaluar mas de una variable.
#' @param title a main title for the plot
#' @param doPlot A logical value indicating whether show a plot. Default value is TRUE
#' @param show.lg A logical value indicating whether show a legend Default value is TRUE
#' @param show.cascon A logical value indicating whether show number cases/controls. Default value is TRUE
#' @param show.detail A logical value indicating whether show detail output. Default value is TRUE
#' @param xtab A logical value indicating whether the output is a xtable. Default value is FALSE.
#' @param direction character string specifying the direction to compute the ROC curve. By default individuals with a test value lower than the cutoff are classified as healthy (negative test), whereas patients with a test value greater than (or equal to) the cutoff are classified as diseased (positive test). If this is not the case, however, and the high values are related to health, this argument should be established at ">".
#' @param cex.main expansion factor for main names (size main)
#' @export doROC
#' @import pROC OptimalCutpoints xtable
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#'
#' # univariate
#' doROC(x = "mpg", group = "am", dat = mtc_bis, modGLM = FALSE)
#' doROC(frml = am~mpg, dat = mtc_bis, modGLM = FALSE)
#'
#' #univariate model (mateix resultat que univariate)
#' doROC(x = "mpg", group = "am", dat = mtc_bis, modGLM = TRUE)
#' doROC(frml = am~mpg, dat = mtc_bis, modGLM = TRUE)
#'
#' #multivariate model
#' doROC(x = c("mpg", "drat"), group = "am", dat = mtc_bis, modGLM = TRUE)
#' doROC(frml = am~mpg+drat, dat = mtc_bis, modGLM = TRUE)
#'

#'
#' @return auc: Area bajo la curva y correspondiente intervalo de confianza
#' @return cutoff.probability: en el caso de haber realizado modGLM, punto de corte óptimo de la probabilidad de predicción calculado con el indice de Youden
#' @return cutoff.variable: punto de corte de la variable cuantitativa. SOLO CUANDO EVALUAMOS UNA UNICA VARIABLE CUANTITATIVA.
#' @return youden: the optimal value of the method considered for selecting the optimal cutpoint, i.e., the value of the criterion at the optimal cutpoint.
#' @return res_detail: taula detallada amb totes les sensibilitats i especificitats
#' @return res_sum:  the optimal cutpoint(s) obtained with the method(s) selected; its/their accuracy measures and the area under ROC curve (AUC)
#' @return dat: base de datos original incluyendo predicción de la variable respuesta teniendo en cuenta como punto de corte indice de Youden y predicción en forma de probabilidad en el caso de modGLM = TRUE.
#' @return table: the results of table on data and reference
#' @return positive.class: the positive result level
#' @keywords roc glm test



doROC <- function(frml, x , group  , dat,
                  tag.healthy = NULL,
                  title = NULL,
                  modGLM = NULL,
                  doPlot = TRUE,
                  cex.main = 0.9,
                  show.lg = TRUE,
                  show.cascon = TRUE,
                  show.detail = TRUE,
                  xtab = FALSE,

                  direction = c("<", ">"), ...)
{

  ## comprovacions varies, warnings i errors
  if (exists(deparse(substitute(show.ci)))) message("\n UEBmessage: Argument 'show.ci' is deprecated \n")
  if (exists(deparse(substitute(validation)))) message("\n UEBmessage: Argument 'validation' is deprecated \n")
  if (exists(deparse(substitute(test_y)))) message("\n UEBmessage: Arguments 'test' and 'test_y' are deprecated \n")
  if (exists(deparse(substitute(col.thres)))) message("\n UEBmessage: Argument 'col.thres' is deprecated \n")
  if (exists(deparse(substitute(col.ic)))) message("\n UEBmessage: Argument 'col.ic' is deprecated \n")
  if (exists(deparse(substitute(x.axes)))) message("\n UEBmessage: Argument 'x.axes' is deprecated \n")
  if (exists(deparse(substitute(show.thr)))) message("\n UEBmessage: Argument 'show.thr' is deprecated \n")
  if (is.null(modGLM)) stop("Es necesario indicar, TRUE o FALSE para el parametro modGLM.")
  if ((missing(x) | missing(group)) & missing(frml))  stop("'x' and 'group' argument required, or 'frml' argument required", call. = FALSE)

  if (missing(x)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  if (missing(frml)) frml <- as.formula(paste(group, "~", paste0(x, collapse = " + ")))
  if (missing(group)) group <- strsplit(as.character(frml), "~", fixed = T)[[2]]
  if (is.null(title)) title <- paste(group, "-",paste0(x, collapse = "+"))
  if (is.null(tag.healthy)) tag.healthy <- levels(dat[,group])[1]

  dat[,group] <- relevel(dat[,group], ref = tag.healthy)

  results <- list()

  ## assignació variable resposta (group) i variable cuantitativa (x) o formula (frml). També titol y nivell de referencia (tag.healthy)
  if (modGLM) {
    mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
    results$mod <- mod
    pred <- predict(mod, type = "response")
    dat$pred <- NA
    dat[names(pred),]$pred <- pred
    x <- "pred"
  # }else{
  #   if (!missing(frml)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  }
  results$dat <- dat



  # calcul corba ROC, punt optim amb index de youden i mesures de clasificacio
  meth.cutoff <- "Youden"
  positive.class <- levels(dat[,group])[levels(dat[,group]) != tag.healthy]
  clasRes <- optimal.cutpoints(X = x, status = group, methods = meth.cutoff,
                               data = dat,tag.healthy = tag.healthy, ci.fit = TRUE,
                               direction = direction)
  results$res_sum <- summary(clasRes)

  if (doPlot) {
    plot(clasRes, which = 1, legend = show.lg, cex.main = cex.main, ylim = c(0,1))
    mtext(title, side = 3)
    if (show.cascon) {
      text(.85, .25,
           paste0("controls: ", clasRes$Youden$Global$measures.acc$n$h,
                  "\n cases: ", clasRes$Youden$Global$measures.acc$n$d),
           cex = 0.8)
    }

  }

  if (show.detail) {
    results$res_detail <- cbind(results$res_sum$Youden$Global$measures.acc$cutoffs,
                                results$res_sum$Youden$Global$measures.acc$Se[,1],
                                results$res_sum$Youden$Global$measures.acc$Sp[,1],
                                results$res_sum$Youden$Global$measures.acc$DLR.Positive[,1],
                                results$res_sum$Youden$Global$measures.acc$DLR.Negative[,1])
    colnames(results$res_detail) <- c("Cutpoint", "Sensitivity", "Specificity", "LR+", "LR-")
  }

  ## es mostren els resultats general com xtable per a latex
  if (xtab) {
    print(xtable(results$res_sum$p.table$Global$Youden[[1]],
           caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI )))
  }

  ## punts de talls
  if (modGLM) {
    results$cutoff.probability <- clasRes$Youden$Global$optimal.cutoff$cutoff # threshold  de Youden probability
    name_var_cuanti <-  strsplit(as.character(frml), "~", fixed = T)[[3]]

    if (length(unlist(strsplit(name_var_cuanti, "+", fixed = T))) == 1) {
      results$cutoff.variable <- results$dat[,name_var_cuanti][which(results$dat$pred == results$cutoff.probability)]
    }else{
      results$cutoff.variable <- "No se puede calcular debido a que existe más de una variable explicativa."
    }
    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, positive.class, tag.healthy ))
    }

  }else{
    results$cutoff.variable <- clasRes$Youden$Global$optimal.cutoff$cutoff # punto de corte optimo, segun Youden para variable numerica

    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, positive.class, tag.healthy ))
    }
  }
  results$dat$outcome.predict <- factor(results$dat$outcome.predict, c(tag.healthy,positive.class))

  results$youden <- clasRes$Youden$Global$optimal.criterion
  results$auc <- results$res_sum$Youden$Global$measures.acc$AUC
  results$table <- table(Group = results$dat[,group], predict = results$dat$outcome.predict)
  results$tag.healthy <- tag.healthy

  # missatge canvi de nom a output
  message(" !!!!!!!!! \n UEBmessage: Output 'thres.best' are deprecated, new same output is 'cutoff.probability' \n !!!!!!!!!")

  message(" !!!!!!!!! \n UEBmessage: tag.healthy: ", tag.healthy , " \n !!!!!!!!!")
  return(results)
}

# set.seed(81)
# df <- data.frame(y = as.factor(rbinom(50,1,.40)),x = rnorm(50,10,1))
# resROC <- doROC (frml = y ~ x, title = 'prova1', tag.healthy= "1",
# cex.main = 0.6, dat = df, modGLM = FALSE, direction = ">")
# resROC$cutoff.variable
# resROC <- doROC (frml = y ~ x, title = 'prova1',
# cex.main = 0.6, dat = df, modGLM = TRUE)
# # si usamos el parametro modGLM = TRUE y queremos obtener el punto
# #de corte real en la variable.
# # Esto SOLO funciona si tenemos unicamente UNA variable explicativa.
# resROC$cutoff.variable
# (pt <- resROC$dat$x[which(resROC$dat$pred == resROC$cutoff.probability)])
