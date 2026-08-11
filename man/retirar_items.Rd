% Generated manually (el NAMESPACE y los .Rd de este paquete se mantienen a
% mano: roxygenise() reescribe el NAMESPACE y se pierden exports).
\name{retirar_items}
\alias{retirar_items}
\title{Retirar items de una escala}
\usage{
retirar_items(x, items, minimo_por_dimension = 2, estricto = FALSE,
  verbose = TRUE)
}
\arguments{
\item{x}{Objeto \code{semilla} (o lista con \code{$items}).}

\item{items}{Identificadores de los items a retirar: codigos
(\code{"Item_4"}), textos completos o posiciones enteras. Vector.}

\item{minimo_por_dimension}{Minimo de items que debe conservar cada dimension.
Por defecto 2.}

\item{estricto}{Si \code{TRUE}, se detiene cuando algun identificador no casa
con ninguna fila. Por defecto \code{FALSE}: avisa y sigue con los que si.}

\item{verbose}{Si \code{TRUE} (defecto), informa de lo retirado y de lo que
queda.}
}
\value{
El objeto de entrada con menos items, con \code{$embeddings} y
\code{$similitud} recortados en el mismo orden, \code{$efa} a \code{NULL} y la
compuerta marcada como caduca. El atributo \code{"items_retirados"} acumula los
identificadores retirados.
}
\description{
Saca uno o varios items de una escala y deja el objeto coherente: recorta
\code{$items}, \code{$embeddings} y \code{$similitud} con el MISMO indice -son
las tres piezas indexadas por item- e invalida lo que deja de describirla (la
estructura medida y la compuerta). Es la operacion que hace el boton "Retirar
los items seleccionados" de la aplicacion Shiny, disponible aqui para quien
trabaje desde R: el flujo es identico en los dos sitios.
}
\details{
\strong{Como se identifica un item.} La tabla de consenso que devuelve
\code{\link{estructura_por_consenso}} nombra los items \code{Item_1},
\code{Item_2}... mientras que \code{$items} puede traer una columna
\code{codigo}, solo un \code{numero}, o ninguna de las dos. Por eso el argumento
\code{items} admite varias formas y se resuelven en este orden: la columna
\code{codigo}, si existe; \code{"Item_<numero>"} contra la columna
\code{numero}; el texto literal del item; y la posicion.

\strong{Por que hay un minimo por dimension.} La afinidad de un item "con los
demas de su dimension" se calcula excluyendolo a el; con un solo item la
dimension queda indefinida y ni el consenso ni la compuerta pueden correr.

\strong{Que se invalida.} \code{$efa} se borra. La compuerta NO se borra -de
ella heredan el umbral de redundancia, los vetos lexicos y la exposicion por
dimension- pero se marca con \code{$compuerta$caduca = TRUE} y
\code{$compuerta$caduca_motivo = "retiro_manual"}.
}
\examples{
\dontrun{
est <- estructura_por_consenso(escala, api_key = Sys.getenv("OPENAI_API_KEY"))
bajos <- est$consenso$Codigo[est$consenso$Consenso < 0.667]
escala2 <- retirar_items(escala, bajos)
est2 <- estructura_por_consenso(escala2, api_key = Sys.getenv("OPENAI_API_KEY"))
}
}
\seealso{
\code{\link{estructura_por_consenso}}, \code{\link{forma_corta}}
}
