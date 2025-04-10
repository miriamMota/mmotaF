% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select.varCG.R
\name{select.varCG}
\alias{select.varCG}
\title{A select.varCG Function}
\usage{
\method{select}{varCG}(...)
}
\arguments{
\item{restab}{an object of class 'compareGroups'}

\item{p.value}{valor de selección para el p.valor. Por defecto 0.05.}
}
\description{
Selección de variables según pvalor para objeto clase comparegroups.
}
\examples{
\dontrun{
restab <- createTable(compareGroups(vs~., data = mtc_bis))
select.varCG(restab)
}
}
\keyword{compareGroups}
\keyword{pvalue}
\keyword{selection}
