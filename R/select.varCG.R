#' A select.varCG Function
#'
#' Selección de variables según pvalor para objeto clase comparegroups.
#' @param restab an object of class 'compareGroups'
#' @param p.value valor de selección para el p.valor. Por defecto 0.05.
#' @keywords compareGroups selection pvalue
#' @export select.varCG
#' @examples
#' \dontrun{
#' restab <- createTable(compareGroups(vs~., data = mtc_bis))
#' select.varCG(restab)
#' }

select.varCG <- function(...) {
  .Defunct(msg = "Esta función ha sido eliminada.")
}


# select.varCG <- function(restab, p.value = 0.05){
#   pval <- getResults(restab, "p.overall")
#   varSel <- names(which(pval < p.value))
#   nameVarSel <- NULL
#   for (i in 1:length(varSel)) nameVarSel[i] <- unlist(strsplit(varSel[i], ":"))[1]
#   return(nameVarSel)
# }

