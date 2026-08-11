#' @title Retirar items de una escala
#'
#' @description
#' Saca uno o varios items de una escala y deja el objeto coherente: recorta
#' \code{$items}, \code{$embeddings} y \code{$similitud} con el MISMO indice
#' -son las tres piezas indexadas por item- e invalida lo que deja de
#' describirla (la estructura medida y la compuerta). Es la operacion que hace
#' el boton "Retirar los items seleccionados" de la aplicacion Shiny, disponible
#' aqui para quien trabaje desde R: el flujo es identico en los dos sitios.
#'
#' @details
#' \strong{Como se identifica un item.} La tabla de consenso que devuelve
#' \code{\link{estructura_por_consenso}} nombra los items \code{Item_1},
#' \code{Item_2}... mientras que \code{$items} puede traer una columna
#' \code{codigo}, solo un \code{numero}, o ninguna de las dos. Por eso el
#' argumento \code{items} admite varias formas y se resuelven en este orden:
#' \enumerate{
#'   \item la columna \code{codigo}, si existe;
#'   \item \code{"Item_<numero>"} contra la columna \code{numero};
#'   \item el texto literal del item (columna \code{item});
#'   \item la posicion (un entero es la fila n).
#' }
#' Si un identificador no casa con ninguna fila se avisa y se ignora, salvo que
#' \code{estricto = TRUE}, en cuyo caso se detiene.
#'
#' \strong{Por que hay un minimo por dimension.} La afinidad de un item "con los
#' demas de su dimension" se calcula excluyendolo a el; con un solo item la
#' dimension queda indefinida y ni el consenso ni la compuerta pueden correr.
#' Por eso no se permite dejar una dimension por debajo de
#' \code{minimo_por_dimension}.
#'
#' \strong{Que se invalida.} \code{$efa} (la estructura medida) se borra, porque
#' describe a una escala que ya no existe. La compuerta NO se borra -de ella
#' heredan el umbral de redundancia, los vetos lexicos y la exposicion por
#' dimension- pero se marca con \code{$compuerta$caduca = TRUE} y
#' \code{$compuerta$caduca_motivo = "retiro_manual"}, para que quien la lea
#' sepa que sus numeros son de antes y por que.
#'
#' @param x Objeto \code{semilla} (o lista con \code{$items}).
#' @param items Identificadores de los items a retirar: codigos
#'   (\code{"Item_4"}), textos completos o posiciones enteras. Vector.
#' @param minimo_por_dimension Minimo de items que debe conservar cada
#'   dimension. Por defecto 2.
#' @param estricto Si \code{TRUE}, se detiene cuando algun identificador no casa
#'   con ninguna fila. Por defecto \code{FALSE}: avisa y sigue con los que si.
#' @param verbose Si \code{TRUE} (defecto), informa de lo retirado y de lo que
#'   queda.
#'
#' @return El objeto de entrada con menos items, con \code{$embeddings} y
#'   \code{$similitud} recortados en el mismo orden, \code{$efa} a \code{NULL} y
#'   la compuerta marcada como caduca. Ademas, el atributo
#'   \code{"items_retirados"} acumula los identificadores retirados.
#'
#' @examples
#' \dontrun{
#' est <- estructura_por_consenso(escala, api_key = Sys.getenv("OPENAI_API_KEY"))
#' bajos <- est$consenso$Codigo[est$consenso$Consenso < 0.667]
#' escala2 <- retirar_items(escala, bajos)
#' # y se vuelve a medir, porque lo anterior ya no la describe:
#' est2 <- estructura_por_consenso(escala2, api_key = Sys.getenv("OPENAI_API_KEY"))
#' }
#'
#' @seealso \code{\link{estructura_por_consenso}}, \code{\link{forma_corta}}
#' @export
retirar_items <- function(x, items, minimo_por_dimension = 2,
                          estricto = FALSE, verbose = TRUE) {

  if (is.null(x) || is.null(x$items) || !nrow(x$items))
    stop("La escala no tiene items.")
  if (missing(items) || !length(items))
    stop("Indica al menos un item a retirar.")

  it   <- x$items
  ids  <- items
  idx  <- integer(0)
  sin_casar <- character(0)

  for (id in ids) {
    fila <- .idx_un_item(it, id)
    if (length(fila)) idx <- c(idx, fila) else sin_casar <- c(sin_casar, as.character(id))
  }
  idx <- sort(unique(idx))

  if (length(sin_casar)) {
    msg <- paste0("No se pudo identificar: ", paste(sin_casar, collapse = ", "),
                  ". Columnas disponibles: ", paste(names(it), collapse = ", "), ".")
    if (estricto) stop(msg) else warning(msg, call. = FALSE)
  }
  if (!length(idx)) stop("Ningun identificador caso con una fila de la escala.")

  keep <- rep(TRUE, nrow(it)); keep[idx] <- FALSE
  if (!any(keep)) stop("No se puede retirar: la escala quedaria vacia.")

  # El minimo por dimension se comprueba ANTES de tocar nada
  if ("dimension" %in% names(it)) {
    tras <- table(as.character(it$dimension[keep]))
    flacas <- names(tras)[tras < minimo_por_dimension]
    perdidas <- setdiff(unique(as.character(it$dimension)), names(tras))
    if (length(flacas) || length(perdidas))
      stop("No se puede retirar: ",
           paste(c(flacas, perdidas), collapse = ", "),
           " quedaria con menos de ", minimo_por_dimension, " items. ",
           "Una dimension asi no permite calcular el consenso ni la compuerta.")
  }

  retirados <- if ("codigo" %in% names(it)) as.character(it$codigo)[idx] else
    if ("numero" %in% names(it)) paste0("Item_", it$numero[idx]) else
      paste0("fila_", idx)

  # Las tres piezas van indexadas por item y en el mismo orden: se recortan con
  # el MISMO 'keep' o cada item queda con el vector de otro.
  x$items <- it[keep, , drop = FALSE]
  if (!is.null(x$embeddings) && nrow(x$embeddings) == length(keep))
    x$embeddings <- x$embeddings[keep, , drop = FALSE]
  if (!is.null(x$similitud) && nrow(x$similitud) == length(keep))
    x$similitud <- x$similitud[keep, keep, drop = FALSE]

  # Lo medido describia la escala CON esos items
  x$efa <- NULL
  if (!is.null(x$compuerta)) {
    x$compuerta$caduca <- TRUE
    x$compuerta$caduca_motivo <- "retiro_manual"
  }
  attr(x, "items_retirados") <- c(attr(x, "items_retirados"), retirados)

  if (isTRUE(verbose)) {
    message("Retirados ", length(idx), " item(s): ", paste(retirados, collapse = ", "))
    message("Quedan ", nrow(x$items), " items.")
    if (!is.null(x$compuerta))
      message("La compuerta queda marcada como caduca: vuelve a ejecutarla ",
              "para que sus numeros describan esta escala.")
  }
  x
}

# Resuelve UN identificador a la fila de items_df. Devuelve integer(0) si no
# casa. Mismo orden de tanteo que usa la aplicacion, para que retirar el mismo
# item desde R y desde Shiny signifique exactamente lo mismo.
.idx_un_item <- function(it, id) {
  id_chr <- as.character(id)

  if ("codigo" %in% names(it)) {
    i <- which(as.character(it$codigo) == id_chr)
    if (length(i)) return(i[1])
  }
  if ("numero" %in% names(it)) {
    i <- which(paste0("Item_", it$numero) == id_chr)
    if (length(i)) return(i[1])
  }
  if ("item" %in% names(it)) {
    i <- which(trimws(as.character(it$item)) == trimws(id_chr))
    if (length(i)) return(i[1])
  }
  # Posicion: solo si el identificador es un entero dentro de rango
  n <- suppressWarnings(as.integer(id_chr))
  if (!is.na(n) && n >= 1 && n <= nrow(it)) return(n)

  integer(0)
}
