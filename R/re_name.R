
#' A re_name Function
#'
#' Renombra los identificadores de columna, substituyendo '.' por '_', eliminando acentos, ec.
#' @param dat data frame que contiene las variables a evaluar or character vector with variable names.
#' @param char a character string specifying the characters to be translated. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @param newchar a character string specifying the translations. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @export re_name
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' df <- data.frame('nombre+a-apellido' = as.factor(rbinom(50,1,.40))
#' , 'dni...NIF' = rnorm(50,10,1),
#' 'Y/X'=as.factor(rbinom(50,1,.40)),'ibm 2' = rnorm(50,10,1))
#' names(df) <- c('nombre+a-apellido',
#' 'dni...NIF', 'Y/X', 'ibm 2') ## modifiquem noms per a que siguin "incorrectes"
#' df <- re_name(data = df)
#' df <- re_name(data = df, char = "+-", newchar = "__")
#' @keywords names dataframe correct

re_name <- function(data,char = NULL, newchar = NULL) {
  if (is.data.frame(data)) {
    nms <- names(data)
  } else {
    nms <- data
  }

  if (!is.null(char))
    nms <-  chartr(char, newchar, nms)
  nms <- gsub(".", "_",
              gsub("..", ".",
                   gsub("..", ".",
                        gsub("..", ".", nms, fixed = T),
                        fixed = T), fixed = T), fixed = T)
  nms <- gsub(" ", "_", nms, fixed = T)
  nms <- gsub("/", "_", nms, fixed = T)
  nms <- gsub("__", "_", nms, fixed = T)
  nms <- gsub("_$", "", nms)
  nms <- gsub("^X_", "", nms)
  nms <- chartr("áéóíúÁÉÍÓÚ", "aeoiuAEIOU", nms)
  # words <- words[-grep("http|@|#|ü|ä|ö|'", words)] # remove urls, usernames, hashtags and umlauts (the latter can not be displayed by all fonts)
  # clean_words <- words[-grep("[^A-Za-z0-9]", words)]

  if (is.data.frame(data)) {
    names(data) <- nms
    return(data)
  } else {
    return(nms)
  }
}
