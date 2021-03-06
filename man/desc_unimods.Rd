% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc_unimods.R
\name{desc_unimods}
\alias{desc_unimods}
\title{A desc_unimods Function}
\usage{
desc_unimods(
  y,
  var2test,
  data,
  type = NULL,
  size = 8.5,
  format = "html",
  caption = NULL,
  show.n = TRUE,
  group = TRUE
)
}
\arguments{
\item{y}{response variable.}

\item{var2test}{nombre de las variables a testar mediante regresión logítica}

\item{type}{regression type. "logistic" or "linealE}

\item{size}{A numeric input for table font size}

\item{format}{a character string; possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format; if format is a function, it must return a character string}

\item{caption}{Character vector containing the table's caption or title. Default value is "Univariate logistic regression"}

\item{show.n}{TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".}

\item{group}{TRUE o FALSE mostrar variables agrupadas en la tabla}
}
\description{
Genera tabla con los coeficientes  , intervalos de confianza y p-valores de un modelo de regresión logística o lineal
}
\examples{
# resglm <- desc_unimods(y = "am",
# var2test = c("mpg","cyl","disp","hp","drat","wt","qsec","vs" ) ,
# data = mtc_bis, format = "html", size = 10, type = "logistic")

}
\keyword{OR}
\keyword{logistic}
\keyword{regresion}
\keyword{summary}
