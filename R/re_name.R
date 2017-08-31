
#' A re_name Function
#'
#' Renombra los identificadores de columna, substituyendo '.' por '_', eliminando acentos, ec.
#' @param dat data frame que contiene las variables a evaluar
#' @export re_name
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @keywords names dataframe correct

re_name <- function(data) {
    names(data) <- gsub(".", "_",
                        gsub("..", ".",
                             gsub("..", ".",
                                  gsub("..", ".", names(data), fixed = T),
        fixed = T), fixed = T), fixed = T)
    names(data) <- gsub(" ", "_", names(data), fixed = T)
    names(data) <- gsub("__", "_", names(data), fixed = T)
    names(data) <- gsub("_$", "", names(data))
    names(data) <- gsub("^X_", "", names(data))
    names(data) <- chartr("áéóíúÁÉÍÓÚ", "aeoiuAEIOU", names(data))
    return(data)
}
