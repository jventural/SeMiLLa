# =============================================================================
# cargas_por_cohesion(): expone de forma independiente las cargas del GENERADOR
# que usa carga_propia = "semantica" en simular_estructura() y estres_escala().
# v2.9.1
# =============================================================================

#' Cargas del generador ordenadas por cohesion semantica (uso independiente)
#'
#' Calcula, sin necesidad de un objeto \code{semilla} ni de llamadas a la API,
#' las cargas que el motor de simulacion usa cuando se pide
#' \code{carga_propia = "semantica"} en \code{\link{simular_estructura}} y
#' \code{\link{estres_escala}}. Cada item recibe una carga propia dentro de
#' \code{rango} segun su cohesion (similitud media de embeddings) con los demas
#' items de su dimension, y se inyectan cargas cruzadas de \code{carga_cruzada}
#' cuando la afinidad del item con OTRA dimension alcanza \code{umbral_cruce}
#' de su afinidad propia.
#'
#' @section Advertencia de interpretacion:
#' Estas cargas son un INSUMO DEL SIMULADOR, no una prediccion de las cargas
#' factoriales empiricas. En la calibracion con datos reales la correlacion
#' entre similitud semantica y correlaciones policoricas fue de .21; por eso
#' el motor usa la similitud SOLO para ordenar los items dentro de
#' \code{rango} (por defecto .45-.75), nunca como estimacion puntual de
#' lambda. Para comparar clusters semanticos con una estructura EFA empirica
#' use \code{\link{cargas_semanticas}} (indice de Jaccard), que es una funcion
#' DISTINTA.
#'
#' @param similitud Matriz p x p de similitud coseno entre items (p. ej. la
#'   que devuelve \code{\link{obtener_embeddings}}), o una matriz de
#'   embeddings p x d (filas = items), en cuyo caso la similitud coseno se
#'   calcula internamente.
#' @param dimension Vector de longitud p con la dimension teorica de cada
#'   item (character o factor), en el MISMO orden que las filas de
#'   \code{similitud}.
#' @param rango Vector de dos valores con el minimo y maximo de la carga
#'   propia (por defecto \code{c(0.45, 0.75)}).
#' @param umbral_cruce Proporcion de la afinidad propia a partir de la cual
#'   la afinidad con otra dimension inyecta una carga cruzada (por defecto
#'   \code{0.90}).
#' @param carga_cruzada Valor de la carga cruzada inyectada (por defecto
#'   \code{0.20}).
#'
#' @return Una lista de clase \code{semilla_cargas_cohesion} con:
#'   \item{tabla}{data.frame con \code{item} (indice), \code{dimension} y
#'     \code{carga_propia} de cada item.}
#'   \item{LAMBDA}{Matriz p x K de cargas del generador (columnas = dimensiones).}
#'   \item{afinidad}{Matriz p x K de afinidad media item-dimension.}
#'   \item{n_cruces}{Numero de cargas cruzadas inyectadas.}
#'   \item{params}{Parametros usados.}
#'
#' @examples
#' \dontrun{
#' emb <- obtener_embeddings(data.frame(item = textos), api_key)
#' cc  <- cargas_por_cohesion(emb$similitud, dimensiones)
#' cc$tabla
#' # Uso posterior en la prueba de estres (equivale a carga_propia = "semantica"):
#' # estres_escala(x, similitud = emb$similitud, carga_propia = "semantica")
#' }
#'
#' @seealso \code{\link{simular_estructura}}, \code{\link{estres_escala}},
#'   \code{\link{cargas_semanticas}}
#' @export
cargas_por_cohesion <- function(similitud, dimension,
                                rango = c(0.45, 0.75),
                                umbral_cruce = 0.90,
                                carga_cruzada = 0.20) {
  S <- as.matrix(similitud)
  p <- length(dimension)
  if (nrow(S) != ncol(S)) {
    # Se recibio una matriz de embeddings p x d: calcular similitud coseno
    if (nrow(S) != p) stop("Las filas de 'similitud' deben corresponder a 'dimension'.")
    En <- S / sqrt(rowSums(S^2))
    S  <- En %*% t(En)
  }
  if (nrow(S) != p) stop("dim(similitud) no coincide con length(dimension).")
  dims <- unique(as.character(dimension))
  memb <- match(as.character(dimension), dims)
  K <- length(dims)
  ls <- .lambda_semantica(S, memb, K, rango = rango,
                          umbral_cruce = umbral_cruce,
                          carga_cruzada = carga_cruzada)
  colnames(ls$LAMBDA) <- dims
  colnames(ls$afinidad) <- dims
  out <- list(
    tabla = data.frame(item = seq_len(p),
                       dimension = as.character(dimension),
                       carga_propia = ls$lambda_propia),
    LAMBDA = ls$LAMBDA,
    afinidad = ls$afinidad,
    n_cruces = ls$n_cruces,
    params = list(rango = rango, umbral_cruce = umbral_cruce,
                  carga_cruzada = carga_cruzada)
  )
  class(out) <- c("semilla_cargas_cohesion", "list")
  out
}

#' @export
print.semilla_cargas_cohesion <- function(x, ...) {
  cat("Cargas del generador por cohesion semantica (insumo de simulacion,\n")
  cat("NO prediccion de cargas empiricas; ver ?cargas_por_cohesion)\n")
  cat(sprintf("  Items: %d | Dimensiones: %d | Rango: %.2f-%.2f | Cruces: %d\n\n",
              nrow(x$tabla), ncol(x$LAMBDA),
              x$params$rango[1], x$params$rango[2], x$n_cruces))
  print(x$tabla, row.names = FALSE)
  invisible(x)
}
