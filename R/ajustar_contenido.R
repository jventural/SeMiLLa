# Ajuste dirigido por validez de contenido.
#
# Por que existe (caso v4 de bienestar psicologico academico, 2026-10-15):
# converger_escala() decide con un puntaje que domina la estructura esperada
# por la compuerta. En constructos cuyas dimensiones correlacionan alto (Ryff),
# la compuerta espera 1 factor por mas que se reescriba, y converger descarta
# tambien las rondas que SI mejoran la V de Aiken: la escala final conservaba
# items con V = .38. Este paso actua solo sobre el contenido: reescribe los
# items con V baja o con negacion encubierta, vuelve a medir y revierte los
# reemplazos que no mejoran.

#' Reescribir los items con validez de contenido baja
#'
#' Reescribe con el LLM los items cuya V de Aiken promedio queda bajo
#' \code{umbral} (y, en una escala sin inversos, los que traen una negacion
#' encubierta), vuelve a medir la validez de contenido con los jueces
#' simulados y revierte cada reemplazo cuya V no mejora. Complementa a
#' \code{\link{converger_escala}}, que optimiza sobre todo la estructura.
#'
#' @param x Objeto \code{semilla}.
#' @param api_key Clave del proveedor activo.
#' @param validez Resultado previo de \code{\link{validez_contenido}}. Si es
#'   \code{NULL}, se calcula.
#' @param umbral V de Aiken bajo la cual un item se reescribe. Se aplica a cada
#'   criterio por separado: basta que uno quede bajo el umbral. En la v5 de
#'   bienestar psicologico academico, 12 de los 13 items con V < .80 recibieron
#'   notas bajas de un consejo de jueces externo; con .70 pasaban.
#' @param criterios Criterios que califican los jueces. Por defecto incluye la
#'   claridad, que fue lo que mas empeoro en la v5 sin que los jueces internos
#'   la midieran.
#' @param max_rondas Numero maximo de rondas de reescritura.
#' @param n_jueces Numero de jueces simulados.
#' @param modelo Modelo de generacion (\code{usar_proveedor()} lo redirige).
#' @param umbral_redundancia Similitud maxima aceptada con otro item.
#' @param verbose Si \code{TRUE}, informa el progreso.
#' @return Lista de clase \code{semilla_ajuste_contenido} con \code{escala}
#'   (objeto \code{semilla} ajustado), \code{validez_contenido} (la ultima
#'   medicion) y \code{registro} (un \code{data.frame} con cada reemplazo, su V
#'   antes y despues, y si se revirtio).
#' @seealso \code{\link{converger_escala}}, \code{\link{validez_contenido}}
#' @export
ajustar_validez_contenido <- function(x, api_key, validez = NULL, umbral = 0.80,
                                      criterios = c("relevancia", "representatividad", "claridad"),
                                      max_rondas = 2L, n_jueces = 10,
                                      modelo = "gpt-4.1-mini",
                                      umbral_redundancia = 0.70, verbose = TRUE) {
  if (!inherits(x, "semilla")) stop("'x' debe ser un objeto semilla.")
  # Una validez previa sin todos los criterios (p. ej., sin claridad) se recalcula.
  if (is.null(validez) || !all(paste0("V_", criterios) %in% names(validez$v_aiken)))
    validez <- validez_contenido(x, api_key = api_key, criterios = criterios, n_jueces = n_jueces)
  # El cliente se configura solo si hay items que reescribir: una escala sin
  # nada que corregir no necesita Python ni el modulo 'openai'.
  openai <- NULL
  rango  <- .rango_palabras_escala(x$items$item)
  sin_inversos <- isFALSE(x$metadata$incluir_inversos)
  registro <- data.frame(ronda = integer(0), item = integer(0), V_antes = numeric(0),
                         V_despues = numeric(0), revertido = logical(0),
                         item_viejo = character(0), item_nuevo = character(0),
                         stringsAsFactors = FALSE)

  for (ronda in seq_len(max_rondas)) {
    v <- .v_minima(validez$v_aiken, criterios)
    jerga <- .jerga_abstracta(x$items$item)
    malos <- which(v < umbral | (sin_inversos & .negacion_encubierta(x$items$item)) |
                   .supuesto_temporal(x$items$item) | jerga)
    if (verbose) cat(sprintf("\n[Ronda %d] %d items a reescribir\n", ronda, length(malos)))
    if (length(malos) == 0) break
    if (is.null(openai)) openai <- .configurar_openai(api_key, modelo = modelo)
    previos <- list(items = x$items$item, emb = x$embeddings)
    for (j in malos) {
      conservados <- setdiff(x$items$item[x$items$dimension == x$items$dimension[j]],
                             x$items$item[j])
      nuevo <- tryCatch(.reemplazar_item_dirigido(
        x = x, openai = openai, modelo = modelo, idx_item = j,
        motivo = .motivo_contenido(validez$v_aiken[j, ], criterios, umbral, jerga[j]),
        anti_halo = FALSE, poblacion = x$metadata$poblacion, rango_palabras = rango,
        umbral_redundancia = umbral_redundancia, embeddings_existentes = x$embeddings,
        items_dimension_conservados = conservados, no_empeorar = TRUE,
        faceta_objetivo = if ("caracteristica" %in% names(x$items)) x$items$caracteristica[j]),
        error = function(e) {
          if (verbose) cat(sprintf("    I%-3d error: %s\n", j, conditionMessage(e)))
          NULL
        })
      if (!is.list(nuevo)) {
        if (verbose) cat(sprintf("    I%-3d sin reemplazo\n", j)); next
      }
      registro[nrow(registro) + 1L, ] <- list(ronda, j, v[j], NA_real_, NA,
                                              x$items$item[j], nuevo$item)
      x$items$item[j] <- nuevo$item
      if (!is.null(nuevo$emb) && !is.null(x$embeddings) &&
          length(nuevo$emb) == ncol(x$embeddings))
        x$embeddings[j, ] <- nuevo$emb
      if (verbose) cat(sprintf("    I%-3d -> %s\n", j, nuevo$item))
    }
    filas <- which(registro$ronda == ronda)
    if (length(filas) == 0) break
    # Solo se vuelven a juzgar los items reescritos: juzgar los 36 en cada
    # ronda triplicaba el tiempo (v4: ~6 min por ronda solo en jueces).
    idx <- registro$item[filas]
    v_sub <- .validez_subconjunto(x, idx, api_key, n_jueces, criterios)
    # Un reemplazo que no mejora la V vuelve al item anterior.
    for (k in seq_along(filas)) {
      f <- filas[k]; j <- idx[k]
      registro$V_despues[f] <- .v_minima(v_sub[k, , drop = FALSE], criterios)
      registro$revertido[f] <- isTRUE(registro$V_despues[f] < registro$V_antes[f])
      if (registro$revertido[f]) {
        x$items$item[j] <- previos$items[j]
        if (!is.null(previos$emb)) x$embeddings[j, ] <- previos$emb[j, ]
      } else {
        comunes <- intersect(names(v_sub), names(validez$v_aiken))
        comunes <- setdiff(comunes, "numero")
        validez$v_aiken[j, comunes] <- v_sub[k, comunes]
      }
    }
    if (verbose)
      cat(sprintf("  %d aceptados, %d revertidos\n",
                  sum(!registro$revertido[filas]), sum(registro$revertido[filas])))
  }
  # Medicion completa final, para informar la escala tal como queda.
  if (nrow(registro) > 0 && any(!registro$revertido))
    validez <- validez_contenido(x, api_key = api_key, criterios = criterios, n_jueces = n_jueces)
  if (verbose)
    cat(sprintf("\nV promedio de la escala: %.3f\n",
                mean(validez$v_aiken$V_promedio, na.rm = TRUE)))
  structure(list(escala = x, validez_contenido = validez, registro = registro),
            class = c("semilla_ajuste_contenido", "list"))
}


# V de Aiken solo de los items idx: se juzga una copia de la escala con esos
# items. Devuelve las filas de v_aiken en el orden de idx.
#' @keywords internal
.validez_subconjunto <- function(x, idx, api_key, n_jueces,
                                criterios = c("relevancia", "representatividad")) {
  sub <- x
  sub$items <- x$items[idx, , drop = FALSE]
  if (!is.null(x$embeddings)) sub$embeddings <- x$embeddings[idx, , drop = FALSE]
  sub$similitud <- NULL
  vc <- validez_contenido(sub, api_key = api_key, criterios = criterios, n_jueces = n_jueces)
  vc$v_aiken
}


# V del criterio mas bajo de cada item: un juez externo objeta el item por su
# peor criterio, no por el promedio.
#' @keywords internal
.v_minima <- function(v_aiken, criterios) {
  cols <- intersect(paste0("V_", criterios), names(v_aiken))
  if (!length(cols)) return(v_aiken$V_promedio)
  apply(as.matrix(v_aiken[, cols, drop = FALSE]), 1, min, na.rm = TRUE)
}

# Motivo que recibe el generador: que criterio fallo y, si aplica, la jerga.
#' @keywords internal
.motivo_contenido <- function(fila, criterios, umbral, jerga = FALSE) {
  bajos <- character(0)
  for (cr in criterios) {
    val <- fila[[paste0("V_", cr)]]
    if (!is.null(val) && isTRUE(val < umbral))
      bajos <- c(bajos, sprintf("%s baja (V = %.2f)", cr, val))
  }
  m <- if (length(bajos)) paste(bajos, collapse = ", ") else "validez de contenido baja"
  if (isTRUE(jerga))
    m <- paste0(m, "; registro abstracto o tecnico: usa palabras cotidianas de la poblacion")
  if (any(grepl("^claridad", bajos)))
    m <- paste0(m, "; redactalo con palabras simples y un sentido que todos entiendan igual")
  m
}
