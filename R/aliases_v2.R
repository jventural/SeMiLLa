# =============================================================================
# SeMiLLa - Alias de nomenclatura (v2.1)
# =============================================================================
# Nombres descriptivos que dejan claro que estos indices son proxies semanticos
# PRE-EMPIRICOS (geometria del espacio de embeddings), no estimaciones de
# fiabilidad poblacional en sentido CTT/IRT. Son alias de las funciones
# existentes; se conservan los nombres originales por compatibilidad.
# =============================================================================

#' @title Cohesion dimensional semantica (alias de omega_semantico)
#'
#' @description
#' Alias descriptivo de \code{\link{omega_semantico}}. Calcula la cohesion
#' semantica tipo omega de cada dimension sobre la matriz de similitud coseno.
#' Es un proxy pre-empirico de consistencia (geometria del texto), no una
#' estimacion de fiabilidad poblacional.
#'
#' @inheritParams omega_semantico
#' @return Igual que \code{\link{omega_semantico}}.
#' @seealso \code{\link{omega_semantico}}, \code{\link{auditar_redundancia}}
#' @export
coherencia_dimensional <- function(x, verbose = TRUE) {
  omega_semantico(x, verbose = verbose)
}

#' @title Homogeneidad semantica (alias de fiabilidad_semantica)
#'
#' @description
#' Alias descriptivo de \code{\link{fiabilidad_semantica}}. Resume la
#' homogeneidad tipo alfa de cada dimension a partir de la similitud media. Es
#' un proxy pre-empirico de consistencia, no fiabilidad poblacional.
#'
#' @inheritParams fiabilidad_semantica
#' @return Igual que \code{\link{fiabilidad_semantica}}.
#' @seealso \code{\link{fiabilidad_semantica}}, \code{\link{auditar_redundancia}}
#' @export
homogeneidad_semantica <- function(x, metodo = "spearman_brown", verbose = TRUE) {
  fiabilidad_semantica(x, metodo = metodo, verbose = verbose)
}
