#' Escala de demostracion con embeddings sinteticos
#'
#' Objeto \code{semilla} pequeno para probar las funciones que no llaman a la
#' API (redundancia, separabilidad, graficos). Los 15 items, en tres
#' dimensiones, estan redactados a mano para la demostracion. Los embeddings
#' \strong{no} vienen de ningun modelo: cada item es el centroide de su
#' dimension mas ruido, normalizado, en 64 dimensiones. Los items 1 y 2 se
#' construyeron casi identicos para que la auditoria de redundancia tenga un
#' par que senalar. No sirve para sacar conclusiones sobre ninguna escala real.
#'
#' @format Lista de clase \code{c("semilla", "list")} con:
#' \describe{
#'   \item{concepto}{Lista con \code{concepto}, \code{definicion},
#'     \code{dimensiones} (definicion de cada dimension) y \code{fuente}.}
#'   \item{items}{\code{data.frame} de 15 filas con \code{numero},
#'     \code{dimension}, \code{caracteristica} e \code{item}.}
#'   \item{embeddings}{Matriz 15 x 64 de embeddings sinteticos normalizados.}
#'   \item{similitud}{Matriz 15 x 15 de similitud coseno entre items.}
#'   \item{metadata}{Lista con idioma, poblacion, modelos y
#'     \code{sintetico = TRUE}.}
#' }
#' @source Construido con \code{data-raw/semilla_demo.R} (semilla 20261008).
#' @examples
#' semilla_demo$items[, c("dimension", "item")]
#' round(semilla_demo$similitud[1:3, 1:3], 2)
"semilla_demo"
