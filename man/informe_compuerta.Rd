% Escrito a mano: el NAMESPACE y los .Rd de este paquete se mantienen asi
% (roxygenise reescribe el NAMESPACE y se pierden exports).
\name{informe_compuerta}
\alias{informe_compuerta}
\alias{guardar_informe_compuerta}
\title{Informe de la compuerta, el mismo que muestra la aplicacion}
\usage{
informe_compuerta(escala, local = NULL, origen = NULL)

guardar_informe_compuerta(lineas, ruta)
}
\arguments{
\item{escala}{Objeto \code{semilla} con \code{$compuerta} ya calculada.}
\item{local}{Resultados del analisis local (\code{$sim}, \code{$estres}), si los
hay. Anaden las secciones de estructura completa y resistencia.}
\item{origen}{Texto libre sobre de donde salio el resultado.}
\item{lineas}{Salida de \code{informe_compuerta}.}
\item{ruta}{Fichero de destino.}
}
\value{
Un vector de caracteres, una linea por elemento.
}
\description{
Devuelve en texto el volcado COMPLETO de la compuerta: los tres controles con
sus numeros, el escenario, la estructura, la prueba de resistencia y los items
con su deseabilidad. Es exactamente el informe que descarga el Paso 5b de la
aplicacion Shiny, con las mismas etiquetas y los mismos veredictos.
}
\details{
Vive en el paquete, y no en la aplicacion, precisamente para que no puedan
divergir: quien trabaje desde R y quien use la aplicacion tienen que leer lo
mismo. La aplicacion llama a esta funcion; no guarda una copia propia.

Sobre las etiquetas: el campo que describe el resultado es \code{$escenario}
(ROBUSTA / VULNERABLE / FRAGIL / SOLO PUNTAJE TOTAL / SE ESPERAN k FACTOR(ES),
NO K). El antiguo \code{$veredicto} se conserva por compatibilidad, pero daba
falsos negativos a instrumentos publicados que funcionan y no debe presentarse
como el resultado.
}
\examples{
\dontrun{
cat(informe_compuerta(escala), sep = "\n")
guardar_informe_compuerta(informe_compuerta(escala), "compuerta.txt")
}
}
