#' Create a nice data frame from a survival model for automatically generating a table.
#'
#' @param survobj \code{\link[survival]{survfit}} object
#' @param surv TRUE/FALSE. Per defecte supervivencia TRUE, decreixent. FALSE indica cumulative events (f(y) = 1-y)
#' @param nround integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @param event antic parametre
#' @export make_surv_table
#' @import survival stargazer
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' data(aml)
#' surv_fit <-survfit(Surv(time = aml$time, event = aml$status) ~ 1)
#' stargazer(make_surv_table(surv_fit),
#'            summary = FALSE, type = c('latex'), title = 'Summary of status survival fit',
#'           rownames = FALSE)
#' @return a data frame with labeled columns
#' @keywords survival table latex

make_surv_table <- function(fit_surv,
                            nround = 2,
                            surv = TRUE,
                            event = NULL ) {

  if(!is.null(event))  warning("old event parameter")
    res <- summary(fit_surv, conf.int = T)

    if (surv) {
        tab_surv <- data.frame(Time = res$time,
                               n.risk = res$n.risk,
                               n.event = res$n.event,
                               Survival = res$surv,
                               `Cumulative Events` = cumsum(res$n.event),
                               `L.Inf95%IC` = res$lower,
                               `L.Sup95%IC` = res$upper)
    } else {
        tab_surv <- data.frame(Time = res$time,
                               n.risk = res$n.risk,
                               n.event = res$n.event,
                               Survival = 1 - res$surv,
                               `Cumulative Events` = cumsum(res$n.event),
                               `L.Inf95%IC` = 1 - res$upper,
                               `L.Sup95%IC` = 1 - res$lower)
    }

    round(tab_surv, nround)
    return(tab_surv)

}
