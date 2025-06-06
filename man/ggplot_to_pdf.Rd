% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_to_pdf.R
\name{ggplot_to_pdf}
\alias{ggplot_to_pdf}
\title{Guarda una lista de gráficos ggplot en un archivo PDF}
\usage{
ggplot_to_pdf(lista_ggplots, row = 2, col = 2, name.file = "graficos.pdf")
}
\arguments{
\item{lista_ggplots}{Lista de gráficos en formato ggplot2 que se desean exportar.}

\item{row}{Número de filas en la disposición de gráficos por página. Por defecto, 2.}

\item{col}{Número de columnas en la disposición de gráficos por página. Por defecto, 2.}

\item{name.file}{Nombre del archivo PDF de salida. Por defecto, "graficos.pdf".}
}
\value{
No devuelve un objeto en R, sino que guarda los gráficos en un archivo PDF en el directorio de trabajo.
}
\description{
Esta función toma una lista de objetos ggplot y los guarda en un archivo PDF, organizándolos en una cuadrícula con un número especificado de filas y columnas.
}
\details{
La función calcula el número total de gráficos y determina cuántas páginas son necesarias para acomodarlos en el archivo PDF. Luego, usa `grid.arrange()` de `gridExtra` para organizar los gráficos en la cuadrícula definida por los parámetros `row` y `col`.
}
\examples{
\dontrun{
library(ggplot2)
library(gridExtra)

# Crear una lista de gráficos de ejemplo
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p2 <- ggplot(mtcars, aes(hp, drat)) + geom_point()
p3 <- ggplot(mtcars, aes(qsec, disp)) + geom_point()
p4 <- ggplot(mtcars, aes(cyl, gear)) + geom_point()

lista <- list(p1, p2, p3, p4)

# Guardar los gráficos en un PDF
ggplot_to_pdf(lista, row = 2, col = 2, name.file = "mis_graficos.pdf")
}


}
