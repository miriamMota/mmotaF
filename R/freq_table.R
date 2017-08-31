#' A freq_table Function
#'
#' Tabla de frecuencias relativas, por fila columnas o global.
#' @param data data frame que contiene las variables a analizar
#' @param var.cat vector con el nombre de las variables categoricas a resumir
#' @param y nombre de la variable factor principal.
#' @param margin index, or vector of indices to generate margin for
#' @param nround integer indicating the number of decimal places (round).
#' @param latex TRUE o FALSE, para obtener tabla en formato .tex
#' @param sz.tab A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is 'footnotesize'
#' @param title titles for the table
#' @export freq_table
#' @import stargazer
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' freq_table(var.cat = c('cyl','vs','gear', 'carb'), y = 'am', data = mtc_bis, latex = TRUE)
#' @keywords freq tables compare qualitative

freq_table <- function(var.cat, y,
                       data, margin = NULL,
                       nround = 2,
                       latex = TRUE,
                       sz.tab = "footnotesize",
                       title = "titles for the table") {

    data[, y] <- factor(data[, y])
    res_freq_all <- NULL
    for (i in 1:length(var.cat)) {
        data[, var.cat[i]] <- factor(data[, var.cat[i]])
        (freq_abs <- table(data[, var.cat[i]], data[, y]))
        (freq_rel <- prop.table(freq_abs, margin = margin) * 100)
        res_freq <- matrix(paste0(freq_abs, " (", round(freq_rel, nround), "%)"),
                           ncol = ncol(freq_abs))
        res_freq <- cbind(c(var.cat[i], rep("", nrow(res_freq) - 1)),
                          rownames(freq_abs),
                          res_freq)
        colnames(res_freq) <- c("Variable", "Levels", colnames(freq_abs))
        res_freq_all <- rbind(res_freq_all, rep("", ncol(res_freq)), res_freq)
    }
    if (latex) {
        stargazer(res_freq_all,
                  summary = F, type = "latex", add.lines = c(1, 2, 3),
                  font.size = sz.tab, title = title)
    } else {
        return(res_freq_all)
    }
}
