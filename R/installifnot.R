#' installifnot Function
#'
#' Comprueba si un paquete de Bioconductor esta instalado y si no lo instala y carga.
#' @param pkg Nombre del paquete en formato caracter
#' @keywords install
#' @export
#' @examples
#' # installBiocifnot("CMA")



installBiocifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    source("http://Bioconductor.org/biocLite.R")
    biocLite(pckgName)
    require(pckgName)
  }
}
# Example
# installBiocifnot("CMA")



#' installifnot Function
#'
#' Comprueba si un paquete de R cran esta instalado y si no lo instala y carga.
#' @param pkg Nombre del paquete en formato caracter
#' @keywords install
#' @export
#' @examples
#' # installifnot("rpart")

installifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    install.packages(pckgName, dep = TRUE)
    require(pckgName)
  }
}
# Example
# installifnot("xlsx")



#' installifnot Function
#'
#' Comprueba si un paquete de Github esta instalado y si no lo instala y carga.
#' @param pkg Nombre del paquete en formato caracter
#' @param pathGit carpeta on esta el paquet dins de git amb "/" final
#' @keywords install
#' @export
#' @examples
#' # installGitifnot("miriammota/","mmotaF")

installGitifnot <- function(pathGit, pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    installifnot("devtools")
    install_github(file.path(pathGit,pckgName))
    require(pckgName)
  }
}
# Example
# installGitifnot("miriammota/","mmotaF")
