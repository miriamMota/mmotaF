#' A doROC Function
#'
#' Creación de curvas ROC con regresión logística mediante el paquete pROC y validación con datos externos.
#' Para realizar las curvas ROC, en primer lugar, se modeliza - para el conjunto de datos “training set”- la variable respuesta grupo
#' mediante regresión logística teniendo en cuenta la variable explicativa. Una vez ajustado el modelo se realiza las curva
#' ROC(representación gráfica de la sensibilidad en frente a la especificidad).
#' @param frml an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param dat data frame containing the variables in the formula.
#' @param title a main title for the plot
#' @param validation valor lógico indicando si se entreran datos externos para testar el punto de corte elegido en el grafico
#' @param test data.frame indicando los nuevos valores de la variable explicativa
#' @param test_y vector factor indicando grupo de los nuevos individuos
#' @param col.thres color de la cruz que indica el punto de corte óptimo en el gráfico
#' @param col.ic color para el intervalo de confianza. Debe ser translucido, por lo que se puede usar la función makeTransparent para cualquier color R.
#' @param x.axes a logical indicating if the specificity axis (x axis) must be plotted as as decreasing “specificity” (FALSE, the default) or increasing “1 - specificity” (TRUE) as in most legacy software. This affects only the axis, not the plot coordinates.
#' @param cex.main expansion factor for main names (size main)
#' @export doROC
#' @import pROC
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' set.seed(1)
#' df <- data.frame(y = as.factor(rbinom(50,1,.40)),x = rnorm(50,10,1))
#' resROC <- doROC (frml = y ~ x, title = 'prova1', cex.main = 0.6, dat = df)
#' resROC <- doROC (frml = y ~ x, title = 'prova', dat = df,
#'                  validation = TRUE,
#'                  test = data.frame(x=c(1,2,3)),
#'                  test_y = as.factor(c(0,1,1)))
#' @return auc: Area bajo la curva y correspondiente intervalo de confianza
#' @return pvalue: p-valor de la variable explicativa en el modelo de regresion logística
#' @return thres.best: punto de corte  óptimo calculado con el estad ́ıstico de Youden
#' @return misc.test: en el caso de tener datos de validación, la tasa de error de clasificacion utilizando como punto de corte el thres.best.
#' @keywords roc glm test validation



doROC <- function(x , group , frml , dat,
                  tag.healthy = NULL,
                  title = NULL,
                  meth.cutoff = "Youden",
                  doPlot = TRUE,
                  cex.main = 0.9,
                  show.lg = TRUE,
                  show.cascon = TRUE,
                  show.detail = TRUE,
                  xtab = TRUE,
                  modGLM = NULL,
                  direction = c("<", ">"), ...)
# show.ci = NULL
# validation = FALSE,
# test = NULL,
# test_y = NULL,
# col.thres = "blue",
# col.ic = "#aaddddAA",
# x.axes = FALSE,
# show.thr = TRUE,
{
  message("Arguments 'show.ci', 'validation', 'test', 'test_y',
          'col.thres', 'col.ic', 'x.axes' and 'show.thr' are deprecated")
  results <- list()
  if (modGLM) {
    mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
    results$mod <- mod
    pred <- predict(mod, type = "response")
    dat <- dat[names(pred),]
    dat$pred <- pred
    x <- "pred"
  }else{
    if (!missing(frml)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  }

  if (missing(x)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  if ((missing(x) | missing(group)) & missing(frml))  stop("'x' and 'group' argument required, or 'frml' argument required", call. = FALSE)

  if (missing(group)) group <- strsplit(as.character(frml), "~", fixed = T)[[2]]

  if (is.null(title)) title <- paste(group, "-",x)
  if (is.null(tag.healthy)) tag.healthy <- levels(dat[,group])[1]
  # if (!is.null(show.ci)) warning("argument 'show.ci' is deprecated")


  clasRes <- optimal.cutpoints(X = x, status = group, methods = meth.cutoff,
                               data = dat,tag.healthy = tag.healthy, ci.fit = TRUE,
                               direction = direction)
  results$res_sum <- summary(clasRes)

  if (doPlot) {
    plot(clasRes, which = 1, legend = show.lg, cex.main = cex.main)
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

  if (xtab) {
    xtable(results$res_sum$p.table$Global$Youden[[1]],
           caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI ))
  }

  if (modGLM) {
    results$thres.best  <- clasRes$Youden$Global$optimal.cutoff$cutoff # threshold  de Youden probability
  }else{
    results$cutoff <- clasRes$Youden$Global$optimal.cutoff$cutoff # punto de corte optimo, segun Youden para variable numerica
  }
  results$youden <- clasRes$Youden$Global$optimal.criterion

  return(results)
}





## prev 2018.03.21
# doROC <- function(frml, dat,
#                   title = NULL,
#                   validation = FALSE,
#                   test = NULL,
#                   test_y = NULL,
#                   col.thres = "blue",
#                   col.ic = "#aaddddAA",
#                   x.axes = FALSE,
#                   show.cascon = TRUE,
#                   show.ci = TRUE,
#                   show.thr = TRUE,
#                   cex.main = 0.8) {
#
#   if (is.null(title)) title <- strsplit(as.character(frml), "~", fixed = T)[[2]]
#
#   mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
#   pred <- predict(mod, type = "response")
#   rocobj <- plot.roc(mod$y, pred, main = title,
#                      ci = TRUE, percent = TRUE,
#                      print.thres = ifelse(show.thr, "best", FALSE),
#                      legacy.axes = x.axes, cex.main = cex.main)
#   if (show.cascon) {
#     text(15, 20,
#          paste0("cases: ", length(rocobj[6]$cases), "\n controls: ", length(rocobj[7]$controls)),
#          cex = 0.8)
#   }
#   thres <- rocobj$sensitivities - (100 - rocobj$specificities)
#   thres.best <- rocobj$thresholds[which(thres == max(thres))]  # threshold  de Youden
#   ciobj <- ci.se(rocobj, boot.n = 200, progress = "none")
#
#   if (show.ci)    plot(ciobj, type = "s", col = col.ic, cex.main = cex.main)  # plot as a blue shape
#
#   if (show.thr) {
#     plot(ci(rocobj, of = "thresholds", thresholds = "best", progress = "none"),
#          col = col.thres,
#          lwd = 2, cex.main = cex.main)
#   }
#
#
#   ic <- rocobj$ci
#   auc_text <- paste(round(ic[2], 2), "% (", round(ic[1], 1), " - ", round(ic[3], 1), "%)", sep = "")
#   p.val <- summary(mod)$coef[, 4][2]
#   text(40, 5, paste("AUC:", auc_text), cex = 0.8)
#
#
#   if (validation) {
#     pred <- predict(mod, newdata = test, type = "response")
#     tab.pred <- table(ifelse(pred > thres.best, levels(test_y)[2], levels(test_y)[1]), test_y)
#     misc.test <- 1 - (sum(diag(tab.pred)) / sum(tab.pred))
#     return(list(auc = auc_text, pvalue = p.val, thres.best = thres.best, misc.test = misc.test))
#   }
#   if (!validation)
#     return(list(auc = auc_text,
#                 pvalue = p.val,
#                 mod = mod,
#                 thres.best = thres.best,
#                 cases = paste("El total de casos es:", length(rocobj[6]$cases)),
#                 rocobj = rocobj,
#                 controls = paste("El total de controls es:", length(rocobj[7]$controls))))
# }
