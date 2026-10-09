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
#' El proposito de esta funcion es fijar las opciones de sesion
#' \code{SeMiLLa.cache_dir} y \code{SeMiLLa.cache_enabled}; por eso no las
#' restaura al salir. Use \code{deshabilitar_cache()} para desactivarlo.
#'
#' @param dir Directorio donde se guardara el cache. Por defecto, el
#'   directorio de cache de usuario que devuelve
#'   \code{tools::R_user_dir("SeMiLLa", "cache")} (nunca el directorio de
#'   trabajo). Para un proyecto concreto, indique una ruta propia.
#' @param verbose Logico. Si \code{TRUE} (por defecto), muestra el directorio
#'   y el numero de entradas existentes.
#'
#' @return Cadena de caracteres con la ruta normalizada del directorio de
#'   cache, de forma invisible. Se llama sobre todo por su efecto: crea el
#'   directorio si no existe y fija las opciones \code{SeMiLLa.cache_dir} y
#'   \code{SeMiLLa.cache_enabled = TRUE}.
#'
#' @seealso \code{\link{cache}}, \code{\link{deshabilitar_cache}},
#'   \code{\link{info_cache}}, \code{\link{limpiar_cache}}
#'
#' @examples
#' old <- options(SeMiLLa.cache_dir = NULL, SeMiLLa.cache_enabled = NULL)
#' d <- file.path(tempdir(), "semilla_cache_demo")
#' habilitar_cache(d)
#' info_cache()
#' deshabilitar_cache()
#' options(old)
#' unlink(d, recursive = TRUE)
#'
#' @export
habilitar_cache <- function(dir = tools::R_user_dir("SeMiLLa", "cache"),
                            verbose = TRUE) {
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
#'   Su proposito es fijar la opcion de sesion \code{SeMiLLa.cache_enabled =
#'   FALSE}; no borra el directorio ni las entradas guardadas.
#'
#' @param verbose Logico. Si \code{TRUE} (por defecto), muestra un mensaje.
#'
#' @return \code{NULL} de forma invisible; se llama por su efecto.
#'
#' @examples
#' old <- options(SeMiLLa.cache_enabled = NULL)
#' deshabilitar_cache()
#' getOption("SeMiLLa.cache_enabled")
#' options(old)
#'
#' @export
deshabilitar_cache <- function(verbose = TRUE) {
  options(SeMiLLa.cache_enabled = FALSE)
  if (verbose) cat("\n  [cache] Deshabilitado\n\n")
  invisible(NULL)
}


#' @title Informacion del Cache
#'
#' @description Devuelve el estado y las estadisticas del cache actual. Al
#'   imprimirse (en consola, o con \code{capture.output()}) los muestra en
#'   cuatro lineas.
#'
#' @return Lista de clase \code{semilla_cache_info} con cuatro elementos:
#'   \code{habilitado} (logico, si el cache esta activo), \code{dir} (ruta del
#'   directorio de cache o \code{NULL} si no hay ninguno configurado),
#'   \code{n_entradas} (entero, numero de archivos \code{.rds} guardados) y
#'   \code{tamano_mb} (numerico, tamano total de esas entradas en megabytes).
#'
#' @examples
#' estado <- info_cache()
#' estado$habilitado
#' estado
#'
#' @export
info_cache <- function() {
  habilitado <- isTRUE(getOption("SeMiLLa.cache_enabled", FALSE))
  dir <- getOption("SeMiLLa.cache_dir", NULL)

  n_entradas <- 0L
  tamano_mb <- 0
  if (!is.null(dir) && dir.exists(dir)) {
    archivos <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    n_entradas <- length(archivos)
    if (n_entradas > 0) {
      tamano_mb <- sum(file.info(archivos)$size) / (1024^2)
    }
  }
  # Visible y con metodo print: la app lee el estado con
  # capture.output(cache("info")), que no captura message().
  structure(list(
    habilitado = habilitado,
    dir = dir,
    n_entradas = n_entradas,
    tamano_mb = tamano_mb
  ), class = c("semilla_cache_info", "list"))
}


#' @rdname info_cache
#' @param x Objeto \code{semilla_cache_info}.
#' @param ... No se usa.
#' @export
print.semilla_cache_info <- function(x, ...) {
  cat("  [cache] Estado: ", if (isTRUE(x$habilitado)) "HABILITADO" else "deshabilitado",
      "\n", sep = "")
  cat("  [cache] Directorio: ", if (is.null(x$dir)) "(ninguno)" else x$dir, "\n", sep = "")
  cat("  [cache] Entradas: ", x$n_entradas, "\n", sep = "")
  cat("  [cache] Tamano: ", sprintf("%.2f MB", x$tamano_mb), "\n", sep = "")
  invisible(x)
}


#' @title Limpiar Cache
#'
#' @description Elimina todas las entradas (\code{.rds}) del directorio de
#'   cache configurado en la opcion \code{SeMiLLa.cache_dir}.
#'
#' @param confirmar Logico. Si \code{TRUE} (por defecto) y la sesion es
#'   interactiva, pide confirmacion (Enter) antes de borrar. En sesiones no
#'   interactivas se borra sin preguntar.
#'
#' @return Numero entero de entradas eliminadas, de forma invisible, o
#'   \code{NULL} invisible si no hay directorio configurado o ya esta vacio.
#'   Se llama por su efecto.
#'
#' @examples
#' old <- options(SeMiLLa.cache_dir = NULL, SeMiLLa.cache_enabled = NULL)
#' d <- file.path(tempdir(), "semilla_cache_demo")
#' habilitar_cache(d, verbose = FALSE)
#' saveRDS(1:3, file.path(d, "demo.rds"))
#' limpiar_cache(confirmar = FALSE)
#' options(old)
#' unlink(d, recursive = TRUE)
#'
#' @export
limpiar_cache <- function(confirmar = TRUE) {
  dir <- getOption("SeMiLLa.cache_dir", NULL)
  if (is.null(dir) || !dir.exists(dir)) {
    message("  [cache] No hay directorio de cache configurado.")
    return(invisible(NULL))
  }
  archivos <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  n <- length(archivos)
  if (n == 0) {
    message("  [cache] Ya esta vacio.")
    return(invisible(NULL))
  }
  if (confirmar && interactive()) {
    message("  [cache] Se eliminaran ", n, " entradas de: ", dir)
    message("  Presiona Enter para continuar, Ctrl+C para cancelar...")
    readline()
  }
  unlink(archivos)
  message("  [cache] ", n, " entradas eliminadas")
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
#' por retrocompatibilidad). Con \code{"enable"} y \code{"disable"} su
#' proposito es fijar opciones de sesion, que por eso no se restauran.
#'
#' @param action Una de: \code{"enable"} (activar cache), \code{"disable"}
#'   (desactivar), \code{"info"} (mostrar estado), \code{"clear"} (vaciar).
#' @param path Directorio donde guardar el cache. Solo se usa con
#'   \code{action = "enable"}. Por defecto,
#'   \code{tools::R_user_dir("SeMiLLa", "cache")} (nunca el directorio de
#'   trabajo).
#' @param verbose Logico. Mostrar mensajes en consola (acciones
#'   \code{"enable"} y \code{"disable"}).
#' @param confirmar Solo aplica con \code{action = "clear"}: pedir
#'   confirmacion antes de borrar (solo en sesiones interactivas).
#'
#' @return Depende de la accion:
#' \itemize{
#'   \item \code{"enable"}: ruta del directorio de cache (invisible).
#'   \item \code{"disable"}: \code{NULL} invisible.
#'   \item \code{"info"}: objeto \code{semilla_cache_info} (visible) con \code{habilitado},
#'     \code{dir}, \code{n_entradas} y \code{tamano_mb} (ver
#'     \code{\link{info_cache}}).
#'   \item \code{"clear"}: numero de entradas eliminadas (invisible), o
#'     \code{NULL} si no habia nada que borrar.
#' }
#'
#' @examples
#' old <- options(SeMiLLa.cache_dir = NULL, SeMiLLa.cache_enabled = NULL)
#' d <- file.path(tempdir(), "semilla_cache_demo")
#' cache("enable", path = d)
#' cache("info")
#' cache("clear", confirmar = FALSE)
#' cache("disable")
#' options(old)
#' unlink(d, recursive = TRUE)
#'
#' @export
cache <- function(action = c("enable", "disable", "info", "clear"),
                  path = tools::R_user_dir("SeMiLLa", "cache"),
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
