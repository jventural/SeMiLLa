# =============================================================================
# SISTEMA DE CACHE REPRODUCIBLE PARA SeMiLLa
# =============================================================================
# El cache permite 100% de reproducibilidad: la primera corrida guarda la
# respuesta del LLM/embedding indexada por un hash (prompt + modelo + seed +
# temperature + top_p + idioma). Las corridas siguientes leen del disco sin
# llamar a la API, garantizando los mismos items.
#
# Flujo tipico:
#   habilitar_cache("D:/mi_proyecto/cache_llm")
#   escala <- semilla("resiliencia", api_key, seed = 2026)  # 1a vez: llama API
#   escala <- semilla("resiliencia", api_key, seed = 2026)  # 2a vez: del cache
#
# El directorio de cache puede incluirse como material suplementario del
# articulo para que cualquier lector reproduzca los mismos items sin
# depender de que OpenAI no cambie el modelo.
# =============================================================================


#' @title Habilitar Cache de Respuestas LLM
#'
#' @description
#' Activa el cache de disco para TODAS las llamadas a OpenAI (chat y embeddings)
#' realizadas desde SeMiLLa. La primera corrida guarda la respuesta indexada
#' por hash. Las corridas siguientes la leen sin llamar a la API.
#'
#' Esto garantiza reproducibilidad 100% aun cuando OpenAI actualice el modelo
#' silenciosamente.
#'
#' @param dir Directorio donde se guardara el cache (default: "SeMiLLa_cache"
#'   en el working directory)
#' @param verbose Mostrar mensajes (default: TRUE)
#'
#' @return Directorio de cache (invisible)
#'
#' @examples
#' \dontrun{
#' # Habilitar cache en el directorio del proyecto
#' habilitar_cache("D:/mi_proyecto/cache_llm")
#'
#' # Primera corrida: llama a la API y guarda
#' escala <- semilla("resiliencia infantil", api_key, seed = 2026)
#'
#' # Segunda corrida: lee del cache (items IDENTICOS)
#' escala2 <- semilla("resiliencia infantil", api_key, seed = 2026)
#'
#' # Desactivar
#' deshabilitar_cache()
#' }
#'
#' @export
habilitar_cache <- function(dir = "SeMiLLa_cache", verbose = TRUE) {
  dir <- normalizePath(dir, mustWork = FALSE)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  options(SeMiLLa.cache_dir = dir)
  options(SeMiLLa.cache_enabled = TRUE)

  if (verbose) {
    n_files <- length(list.files(dir, pattern = "\\.rds$"))
    cat("\n  [cache] Habilitado en: ", dir, "\n", sep = "")
    cat("  [cache] Entradas existentes: ", n_files, "\n\n", sep = "")
  }
  invisible(dir)
}


#' @title Deshabilitar Cache de Respuestas LLM
#'
#' @description Desactiva el cache. Las llamadas siguientes iran a la API.
#'
#' @param verbose Mostrar mensaje (default: TRUE)
#' @export
deshabilitar_cache <- function(verbose = TRUE) {
  options(SeMiLLa.cache_enabled = FALSE)
  if (verbose) cat("\n  [cache] Deshabilitado\n\n")
  invisible(NULL)
}


#' @title Informacion del Cache
#'
#' @description Muestra estado y estadisticas del cache actual.
#'
#' @return Lista invisible con: habilitado, dir, n_entradas, tamano_mb
#' @export
info_cache <- function() {
  habilitado <- isTRUE(getOption("SeMiLLa.cache_enabled", FALSE))
  dir <- getOption("SeMiLLa.cache_dir", NULL)

  cat("\n  [cache] Estado: ", ifelse(habilitado, "HABILITADO", "deshabilitado"), "\n", sep = "")
  cat("  [cache] Directorio: ", ifelse(is.null(dir), "(ninguno)", dir), "\n", sep = "")

  n_entradas <- 0L
  tamano_mb <- 0
  if (!is.null(dir) && dir.exists(dir)) {
    archivos <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    n_entradas <- length(archivos)
    if (n_entradas > 0) {
      tamano_mb <- sum(file.info(archivos)$size) / (1024^2)
    }
  }
  cat("  [cache] Entradas: ", n_entradas, "\n", sep = "")
  cat("  [cache] Tamano: ", sprintf("%.2f MB", tamano_mb), "\n\n", sep = "")

  invisible(list(
    habilitado = habilitado,
    dir = dir,
    n_entradas = n_entradas,
    tamano_mb = tamano_mb
  ))
}


#' @title Limpiar Cache
#'
#' @description Elimina todas las entradas del cache actual.
#'
#' @param confirmar Si TRUE (default), pide confirmacion
#' @export
limpiar_cache <- function(confirmar = TRUE) {
  dir <- getOption("SeMiLLa.cache_dir", NULL)
  if (is.null(dir) || !dir.exists(dir)) {
    cat("  [cache] No hay directorio de cache configurado.\n")
    return(invisible(NULL))
  }
  archivos <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  n <- length(archivos)
  if (n == 0) {
    cat("  [cache] Ya esta vacio.\n")
    return(invisible(NULL))
  }
  if (confirmar) {
    cat("  [cache] Se eliminaran ", n, " entradas de: ", dir, "\n", sep = "")
    cat("  Presiona Enter para continuar, Ctrl+C para cancelar...")
    readline()
  }
  unlink(archivos)
  cat("  [cache] ", n, " entradas eliminadas\n", sep = "")
  invisible(n)
}


# =============================================================================
# DISPATCHER UNIFICADO (SeMiLLa v2.0)
# =============================================================================
# Las cuatro funciones anteriores (habilitar_cache, deshabilitar_cache,
# info_cache, limpiar_cache) siguen disponibles, pero ahora son alias de un
# dispatcher unico mas ergonomico: cache(action, path).

#' @title Gestionar el cache de respuestas LLM (interfaz v2.0)
#'
#' @description
#' Funcion unificada para gestionar el cache de disco de SeMiLLa. Sustituye
#' las cuatro funciones \code{habilitar_cache()}, \code{deshabilitar_cache()},
#' \code{info_cache()} y \code{limpiar_cache()} (que se mantienen como alias
#' por retrocompatibilidad).
#'
#' @param action Una de: \code{"enable"} (activar cache), \code{"disable"}
#'   (desactivar), \code{"info"} (mostrar estado), \code{"clear"} (vaciar).
#' @param path Directorio donde guardar el cache. Solo se usa con
#'   \code{action = "enable"}. Default: \code{"SeMiLLa_cache"} en el WD.
#' @param verbose Mostrar mensajes en consola.
#' @param confirmar Solo aplica con \code{action = "clear"}: pedir
#'   confirmacion antes de borrar.
#'
#' @return Depende de la accion:
#' \itemize{
#'   \item \code{"enable"}: directorio de cache (invisible).
#'   \item \code{"disable"}: \code{NULL} invisible.
#'   \item \code{"info"}: lista con estado del cache.
#'   \item \code{"clear"}: numero de entradas eliminadas (invisible).
#' }
#'
#' @examples
#' \dontrun{
#' cache("enable", path = "D:/mi_proyecto/cache_llm")
#' cache("info")
#' cache("clear", confirmar = FALSE)
#' cache("disable")
#' }
#'
#' @export
cache <- function(action = c("enable", "disable", "info", "clear"),
                  path = "SeMiLLa_cache",
                  verbose = TRUE,
                  confirmar = TRUE) {
  action <- match.arg(action)
  switch(action,
    "enable"  = habilitar_cache(path, verbose = verbose),
    "disable" = deshabilitar_cache(verbose = verbose),
    "info"    = info_cache(),
    "clear"   = limpiar_cache(confirmar = confirmar)
  )
}


# -----------------------------------------------------------------------------
# INTERNAS
# -----------------------------------------------------------------------------

#' @keywords internal
.cache_enabled <- function() {
  isTRUE(getOption("SeMiLLa.cache_enabled", FALSE)) &&
    !is.null(getOption("SeMiLLa.cache_dir", NULL))
}


#' @keywords internal
.cache_key <- function(tipo, payload) {
  # Hash md5 determinista del payload serializado.
  # payload: lista con todos los parametros que determinan la salida
  #   (prompt, modelo, temperature, seed, top_p, modelo_embedding, input, etc.)
  #
  # Usamos serialize() + digest() para hash estable.
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("El paquete 'digest' es requerido para el cache. Instala con: install.packages('digest')")
  }
  hash <- digest::digest(payload, algo = "md5", serialize = TRUE)
  file.path(getOption("SeMiLLa.cache_dir"), paste0(tipo, "_", hash, ".rds"))
}


#' @keywords internal
.cache_get <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readRDS(path),
    error = function(e) NULL
  )
}


#' @keywords internal
.cache_set <- function(path, value) {
  dir_cache <- dirname(path)
  if (!dir.exists(dir_cache)) dir.create(dir_cache, recursive = TRUE)
  saveRDS(value, path, compress = TRUE)
  invisible(path)
}


#' @keywords internal
.cache_msg_hit <- function(tipo, verbose = TRUE) {
  if (verbose && isTRUE(getOption("SeMiLLa.cache_verbose", TRUE))) {
    cat("    [cache HIT] ", tipo, "\n", sep = "")
  }
}


#' @keywords internal
.cache_msg_miss <- function(tipo, verbose = TRUE) {
  if (verbose && isTRUE(getOption("SeMiLLa.cache_verbose", FALSE))) {
    cat("    [cache MISS] ", tipo, " (llamando API)\n", sep = "")
  }
}
