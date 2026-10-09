# =============================================================================
# ALIASES DEPRECADOS (compatibilidad con SeMiLLa v1.x)
# =============================================================================
# En v2.0 algunos nombres cambiaron para reflejar mejor su funcion. Los
# nombres viejos siguen disponibles como aliases que muestran un warning
# una vez por sesion. Esto permite que codigo escrito contra v1.x siga
# funcionando sin modificaciones inmediatas.
#
# Calendario de retirada: los aliases se mantendran hasta v3.0 (>= 2027).
# =============================================================================


# Registro de los avisos ya emitidos en la sesion. Vive en un entorno interno
# del paquete (no en options()) para no alterar las opciones globales.
.semilla_deprec_env <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.deprec_warn_once <- function(viejo, nuevo) {
  if (!isTRUE(.semilla_deprec_env[[viejo]])) {
    warning(sprintf(
      "'%s' esta depreciado en SeMiLLa v2.0. Use '%s' en su lugar.",
      viejo, nuevo
    ), call. = FALSE)
    assign(viejo, TRUE, envir = .semilla_deprec_env)
  }
  invisible(NULL)
}


# -----------------------------------------------------------------------------
# Renombrado: evaluar_calidad_items -> auditar_redaccion_items
# -----------------------------------------------------------------------------

#' @title Alias depreciado: ver \code{auditar_redaccion_items()}
#' @description Esta funcion fue renombrada en SeMiLLa v2.0 para distinguirla
#'   claramente de \code{validez_contenido()} (que evalua criterios psicometricos
#'   formales con V de Aiken). \code{auditar_redaccion_items()} se enfoca en la
#'   redaccion de los items.
#' @param ... Argumentos de \code{\link{auditar_redaccion_items}}
#'   (\code{x}, \code{api_key}, \code{criterios}, \code{modelo},
#'   \code{verbose}, etc.).
#' @return El mismo objeto que devuelve \code{\link{auditar_redaccion_items}}:
#'   la auditoria de la redaccion de cada item. La primera llamada de la sesion
#'   emite ademas un \code{warning} de depreciacion.
#' @seealso \code{\link{auditar_redaccion_items}}
#' @examples
#' # Requiere una clave de API de OpenAI (llama a un LLM).
#' \dontrun{
#' calidad <- evaluar_calidad_items(semilla_demo,
#'                                  api_key = Sys.getenv("OPENAI_API_KEY"))
#' }
#' @export
evaluar_calidad_items <- function(...) {
  .deprec_warn_once("evaluar_calidad_items", "auditar_redaccion_items")
  auditar_redaccion_items(...)
}


# -----------------------------------------------------------------------------
# Cache: 4 funciones -> 1 dispatcher cache()
# Las 4 funciones siguen exportadas (no son alias, son las implementaciones
# subyacentes), pero la documentacion recomienda usar cache(action = ...).
# -----------------------------------------------------------------------------

# (sin alias adicional necesario; cache() ya existe como dispatcher)


# -----------------------------------------------------------------------------
# Generar por formato: 6 funciones -> 1 dispatcher generar_items()
# Idem: las 6 funciones especificas siguen exportadas. generar_items() es
# la nueva interfaz unificada.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Ensamblar por formato: 6 funciones -> 1 dispatcher ensamblar()
# Idem.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Plots de coherencia: 2 funciones -> 1 dispatcher plot_coherencia()
# -----------------------------------------------------------------------------

#' @title Plot unificado de coherencia (interfaz v2.0)
#' @description Despacha a \code{plot_coherencia_boxplot()} o
#'   \code{plot_coherencia_violin()} segun el argumento \code{tipo}.
#' @param x Resultado de \code{\link{analizar_coherencia}} (objeto
#'   \code{semilla_coherencia}).
#' @param tipo \code{"boxplot"} (por defecto) o \code{"violin"}.
#' @param ... Argumentos adicionales pasados a
#'   \code{\link{plot_coherencia_boxplot}} o
#'   \code{\link{plot_coherencia_violin}} (p. ej. \code{titulo}).
#' @return Objeto \code{ggplot} con la distribucion de la similitud intra- e
#'   inter-dimension de los items, tal como lo devuelve la funcion subyacente.
#' @examples
#' coh <- analizar_coherencia(semilla_demo, verbose = FALSE)
#' plot_coherencia(coh)
#' plot_coherencia(coh, tipo = "violin")
#' @export
plot_coherencia <- function(x, tipo = c("boxplot", "violin"), ...) {
  tipo <- match.arg(tipo)
  if (tipo == "boxplot") plot_coherencia_boxplot(x, ...)
  else                   plot_coherencia_violin(x, ...)
}


# -----------------------------------------------------------------------------
# Funciones archivadas en v2.0 (siguen accesibles via SeMiLLa:::nombre):
#   - efa_embeddings()        : reemplazada por precision_clasificacion() y efa_regularizado()
#   - comparar_estructura()   : pareja de la anterior
#   - predecir_irt()          : heuristica sin respaldo empirico, integrada en banco_cat()
#   - plot_irt()              : visualiza predecir_irt
#   - plot_jaccard()          : solapa con plot_sankey() (mas informativo)
#   - validar_escala()        : ahora interna (uso desde generar_escala)
#   - ayuda()                 : redundante con help() y ?function
# -----------------------------------------------------------------------------
