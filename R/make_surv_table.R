#' Create a nice data frame from a survival model for automatically generating a table.
#'
#' @param survobj \code{\link[survival]{survfit}} object
#' @param timevec a vector of times for survival summary
#' @param scale a number that scales the timevec, e.g., if your data is recorded in days and you want months displayed, use scale = 30
#' @export make_surv_table
#' @import survival stargazer
#' @examples
#' data(aml)
#' surv_fit <-survfit(Surv(time = aml$time, event = aml$status) ~ 1)
#' stargazer(make_surv_table(surv_fit, summary(surv_fit)$time),
#'            summary = FALSE, type = c('latex'), title = 'Summary of HMOHIV survival fit',
#'           rownames = FALSE)
#' @return a data frame with labeled columns
#' @keywords survival table latex

make_surv_table <- function(survobj,
                            timevec,
                            scale = 1,
                            col_names = NULL){
  # vector of times for summary points, defaults to six month intervals
  if(is.null(timevec)){
    print('timevec invalid; defaulting to six-month intervals by days')
    timevec <- c(0, 180, 360, 540, 720)
  }
  # here, scale converts the numbers into months
  survsum <- summary(survobj, timevec, scale)
  # make a data frame out of the summary object
  df <- data.frame(survsum$time,
                   survsum$surv,
                   1 - survsum$surv,
                   survsum$std.err,
                   cumsum(survsum$n.event),
                   survsum$n.risk
  )
  # give that data frame names
  if(is.null(col_names)){
    colnames(df) <- c('Time',
                      'Survival',
                      'Deaths',
                      'Survival SE',
                      'Cummulative Events',
                      'Number remaining')
  } else{
    colnames(df) <- col_names
  }
  # return the frame
  return(df)
}
