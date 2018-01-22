
#' A re_name Function
#'
#' Renombra los identificadores de columna, substituyendo '.' por '_', eliminando acentos, ec.
#' @param dat data frame que contiene las variables a evaluar
#' @param char a character string specifying the characters to be translated. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @param newchar a character string specifying the translations. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @export re_name
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame('nombre+a-apellido'=as.factor(rbinom(50,1,.40)), 'dni...NIF' = rnorm(50,10,1),
#' 'Y/X'=as.factor(rbinom(50,1,.40)),'ibm 2' = rnorm(50,10,1))
#' names(df) <- c('nombre+a-apellido',  'dni...NIF', 'Y/X', 'ibm 2') ## modifiquem noms per a que siguin "incorrectes"
#' df <- re_name(data = df)
#' df <- re_name(data = df, char = "+-", newchar = "__")
#' @keywords names dataframe correct

re_name <- function(data,char = NULL, newchar = NULL) {
    names(data) <- gsub(".", "_",
                        gsub("..", ".",
                             gsub("..", ".",
                                  gsub("..", ".", names(data), fixed = T),
        fixed = T), fixed = T), fixed = T)
    names(data) <- gsub(" ", "_", names(data), fixed = T)
    names(data) <- gsub("/", "_", names(data), fixed = T)
    names(data) <- gsub("__", "_", names(data), fixed = T)
    names(data) <- gsub("_$", "", names(data))
    names(data) <- gsub("^X_", "", names(data))
    names(data) <- chartr("áéóíúÁÉÍÓÚ", "aeoiuAEIOU", names(data))
    if (!is.null(char))
    names(data) <-  chartr(char, newchar, names(data))
    return(data)
}
