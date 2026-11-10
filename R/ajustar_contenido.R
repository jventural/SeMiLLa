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
#' @param motivos Razones por las que un item se reescribe: \code{"validez"}
#'   (V bajo \code{umbral}), \code{"negacion"} (negacion encubierta en una
#'   escala sin inversos), \code{"temporal"} (supone tiempo en la carrera, p. ej.
#'   "cada ciclo") y \code{"jerga"} (registro abstracto). Por defecto, todas.
#'   Con \code{c("negacion", "temporal")} se corrigen solo defectos de
#'   redaccion sin tocar el resto de una escala ya elegida.
#' @param verificar_nivel Si \code{TRUE} (por defecto), en las dimensiones que
#'   no son conductuales cada candidato pasa por un control de nivel con el LLM:
#'   se descarta si describe una conducta, habito, tarea o tramite, o si
#'   depende de la institucion o de otras personas, y se pide otro. Sin este
#'   control, el ajuste repuso en Dominio del entorno los items de tareas que
#'   se habian retirado a proposito (2026-11-01).
#' @param verbose Si \code{TRUE}, informa el progreso.
#' @details El item viejo y el reescrito se juzgan en la misma tanda de jueces.
#'   Si el item se reescribio por validez, el reemplazo se acepta solo si su V
#'   minima supera a la del item viejo; si se reescribio por redaccion, basta
#'   con que no la empeore. Las facetas de \code{x$concepto$caracteristicas}
#'   con forma de tarea en una dimension no conductual generan un aviso y no se
#'   usan como guia.
#' @return Lista de clase \code{semilla_ajuste_contenido} con \code{escala}
#'   (objeto \code{semilla} ajustado), \code{validez_contenido} (la ultima
#'   medicion) y \code{registro} (un \code{data.frame} con cada reemplazo, su V
#'   antes y despues medidas en la misma tanda, si se revirtio y una
#'   \code{nota}: el motivo, o el candidato descartado por quedar fuera de
#'   nivel).
#' @seealso \code{\link{converger_escala}}, \code{\link{validez_contenido}}
#' @export
ajustar_validez_contenido <- function(x, api_key, validez = NULL, umbral = 0.80,
                                      criterios = c("relevancia", "representatividad", "claridad"),
                                      max_rondas = 2L, n_jueces = 10,
                                      modelo = "gpt-4.1-mini",
                                      umbral_redundancia = 0.70,
                                      motivos = c("validez", "negacion", "temporal", "jerga"),
                                      verificar_nivel = TRUE,
                                      verbose = TRUE) {
  if (!inherits(x, "semilla")) stop("'x' debe ser un objeto semilla.")
  motivos <- match.arg(motivos, several.ok = TRUE)
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
                         nota = character(0), stringsAsFactors = FALSE)
  # Facetas con forma de tarea en una dimension que no es conductual: tiran
  # los reemplazos hacia conductas (caso Dominio del entorno, 2026-11-01). Se
  # avisa y esas facetas no se pasan al generador.
  facetas_tarea <- .facetas_con_forma_de_tarea(x)
  if (length(facetas_tarea) && verbose)
    warning("Facetas con forma de tarea en dimensiones que no son conductuales (",
            paste(facetas_tarea, collapse = ", "), "): no se usan como guia de los ",
            "reemplazos. Revise x$concepto$caracteristicas.", call. = FALSE)

  for (ronda in seq_len(max_rondas)) {
    v <- .v_minima(validez$v_aiken, criterios)
    jerga <- "jerga" %in% motivos & .jerga_abstracta(x$items$item)
    negacion <- "negacion" %in% motivos & sin_inversos & .negacion_encubierta(x$items$item)
    temporal <- "temporal" %in% motivos & .supuesto_temporal(x$items$item)
    por_validez <- "validez" %in% motivos & v < umbral
    malos <- which(por_validez | negacion | temporal | jerga)
    if (verbose) cat(sprintf("\n[Ronda %d] %d items a reescribir\n", ronda, length(malos)))
    if (length(malos) == 0) break
    if (is.null(openai)) openai <- .configurar_openai(api_key, modelo = modelo)
    previos <- list(items = x$items$item, emb = x$embeddings)
    for (j in malos) {
      dim_j <- x$items$dimension[j]
      conservados <- setdiff(x$items$item[x$items$dimension == dim_j], x$items$item[j])
      x_gen <- x
      faceta <- if ("caracteristica" %in% names(x$items)) x$items$caracteristica[j]
      if (dim_j %in% facetas_tarea) {
        x_gen$concepto$caracteristicas[[dim_j]] <- NULL
        faceta <- NULL
      }
      def_j <- .definicion_dimension(x, dim_j)
      controlar_nivel <- isTRUE(verificar_nivel) && .tipo_dimension(dim_j, def_j) != "conductual"
      motivo_j <- .motivo_contenido(validez$v_aiken[j, ], criterios, umbral, jerga[j],
                                    negacion = negacion[j], temporal = temporal[j])
      # El candidato fuera de nivel se descarta y se pide otro (hasta 2 intentos).
      nuevo <- NULL; nota <- ""
      for (intento in 1:2) {
        cand <- tryCatch(.reemplazar_item_dirigido(
          x = x_gen, openai = openai, modelo = modelo, idx_item = j, motivo = motivo_j,
          anti_halo = FALSE, poblacion = x$metadata$poblacion, rango_palabras = rango,
          umbral_redundancia = umbral_redundancia, embeddings_existentes = x$embeddings,
          items_dimension_conservados = conservados, no_empeorar = TRUE,
          faceta_objetivo = faceta),
          error = function(e) {
            if (verbose) cat(sprintf("    I%-3d error: %s\n", j, conditionMessage(e)))
            NULL
          })
        if (!is.list(cand)) break
        if (!controlar_nivel) { nuevo <- cand; break }
        nivel <- .nivel_item_llm(openai, modelo, cand$item, dim_j, def_j,
                                 otras_dimensiones = .definiciones_dimensiones(x))
        if (!isFALSE(nivel$ok)) { nuevo <- cand; break }
        nota <- paste0("fuera de nivel: ", cand$item, " (", nivel$motivo, ")")
        if (verbose) cat(sprintf("    I%-3d candidato fuera de nivel: %s\n", j, cand$item))
        motivo_j <- paste0(motivo_j, "; el intento anterior (\"", cand$item, "\") quedo fuera ",
                           "del nivel de la dimension: ", nivel$motivo, ". Expresa la vivencia ",
                           "o la capacidad que define la dimension, no una conducta, tarea o ",
                           "tramite, ni algo que dependa de la institucion o de otras personas")
      }
      if (!is.list(nuevo)) {
        if (verbose) cat(sprintf("    I%-3d sin reemplazo\n", j))
        if (nzchar(nota))
          registro[nrow(registro) + 1L, ] <- list(ronda, j, v[j], NA_real_, TRUE,
                                                  x$items$item[j], NA_character_, nota)
        next
      }
      registro[nrow(registro) + 1L, ] <- list(ronda, j, v[j], NA_real_, NA,
                                              x$items$item[j], nuevo$item,
                                              if (por_validez[j]) "validez" else "redaccion")
      x$items$item[j] <- nuevo$item
      if (!is.null(nuevo$emb) && !is.null(x$embeddings) &&
          length(nuevo$emb) == ncol(x$embeddings))
        x$embeddings[j, ] <- nuevo$emb
      if (verbose) cat(sprintf("    I%-3d -> %s\n", j, nuevo$item))
    }
    filas <- which(registro$ronda == ronda & !is.na(registro$item_nuevo))
    if (length(filas) == 0) break
    # Solo se vuelven a juzgar los items reescritos: juzgar los 36 en cada
    # ronda triplicaba el tiempo (v4: ~6 min por ronda solo en jueces).
    # El item viejo y el nuevo se juzgan en la MISMA tanda: comparar la V
    # nueva con la de otra corrida mezclaba el cambio con el ruido de los
    # jueces, y un reemplazo peor pasaba por un empate (Dominio del entorno,
    # 2026-11-01: volvio "Ajusto mi espacio de estudio...").
    idx <- registro$item[filas]
    par <- .validez_pareada(x, idx, registro$item_viejo[filas], api_key, n_jueces, criterios)
    v_sub <- par$nuevo
    for (k in seq_along(filas)) {
      f <- filas[k]; j <- idx[k]
      registro$V_antes[f] <- .v_minima(par$viejo[k, , drop = FALSE], criterios)
      registro$V_despues[f] <- .v_minima(v_sub[k, , drop = FALSE], criterios)
      # Por validez, el nuevo debe superar al viejo; por redaccion (negacion,
      # tiempo, jerga) basta con que no lo empeore.
      registro$revertido[f] <- if (identical(registro$nota[f], "validez"))
        !isTRUE(registro$V_despues[f] > registro$V_antes[f])
      else isTRUE(registro$V_despues[f] < registro$V_antes[f])
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
  # Los reemplazos solo actualizaron filas de $embeddings: la similitud se
  # recalcula para que la compuerta y la redundancia vean los items finales.
  x <- .sincronizar_similitud(x)
  # Medicion completa final, para informar la escala tal como queda.
  if (nrow(registro) > 0 && any(!registro$revertido, na.rm = TRUE))
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


# V de los items nuevos y de los que reemplazan, juzgados en la MISMA tanda.
# Devuelve list(nuevo, viejo), cada una con las filas de v_aiken en el orden
# de idx.
#' @keywords internal
.validez_pareada <- function(x, idx, viejos, api_key, n_jueces,
                             criterios = c("relevancia", "representatividad")) {
  sub <- x
  nuevos_df <- x$items[idx, , drop = FALSE]
  viejos_df <- nuevos_df
  viejos_df$item <- viejos
  sub$items <- rbind(nuevos_df, viejos_df)
  if ("numero" %in% names(sub$items)) sub$items$numero <- seq_len(nrow(sub$items))
  rownames(sub$items) <- NULL
  sub$embeddings <- NULL
  sub$similitud <- NULL
  vc <- validez_contenido(sub, api_key = api_key, criterios = criterios, n_jueces = n_jueces)
  n <- length(idx)
  list(nuevo = vc$v_aiken[seq_len(n), , drop = FALSE],
       viejo = vc$v_aiken[n + seq_len(n), , drop = FALSE])
}


# Definicion de una dimension tal como la ve el generador.
#' @keywords internal
.definicion_dimension <- function(x, dim) {
  d <- if (is.list(x$concepto$dimensiones)) x$concepto$dimensiones[[dim]] else NULL
  if (is.null(d)) dim else paste(d, collapse = " ")
}


# Dimensiones no conductuales cuyas facetas (x$concepto$caracteristicas o la
# columna caracteristica de los items) describen tareas: "Organiza el
# tiempo...", "Gestiona los tramites...". Esas facetas venian de una
# definicion anterior y empujaban los reemplazos de Dominio del entorno hacia
# conductas (2026-11-01).
#' @keywords internal
.forma_de_tarea <- function(texto) {
  t <- tolower(chartr("\u00c1\u00c9\u00cd\u00d3\u00da\u00e1\u00e9\u00ed\u00f3\u00fa",
                      "AEIOUaeiou", texto))
  grepl(paste0("^\\s*(organiza|gestiona|planifica|administra|programa|utiliza|usa|",
               "identifica y utiliza|realiza|cumple|asiste|acude|tramita|consulta|",
               "busca|acomoda|ajusta|distribuye|prepara|entrega)\\b|",
               "tramite|horario|servicios? (universitari|de la universidad)|",
               "espacio de estudio|biblioteca"), t)
}

#' @keywords internal
.facetas_con_forma_de_tarea <- function(x) {
  dims <- unique(x$items$dimension)
  dims[vapply(dims, function(d) {
    if (.tipo_dimension(d, .definicion_dimension(x, d)) == "conductual") return(FALSE)
    fac <- c(if (!is.null(x$concepto$caracteristicas)) unlist(x$concepto$caracteristicas[[d]]),
             if ("caracteristica" %in% names(x$items))
               x$items$caracteristica[x$items$dimension == d])
    fac <- fac[!is.na(fac) & nzchar(fac)]
    length(fac) > 0 && mean(.forma_de_tarea(fac)) >= 0.5
  }, logical(1))]
}


# Control de nivel: el item expresa lo que define la dimension (vivencia,
# percepcion o capacidad) o baja a una conducta, tarea o tramite, o depende de
# la institucion o de terceros. Devuelve list(ok = TRUE/FALSE/NA, motivo,
# criterio).
# 2.12.4: tres criterios mas, por el caso de bienestar academico (02-11-2026):
#   (c) el item cae en lo que la dimension "No incluye" (se lee de def_dim o
#       se pasa aparte en no_incluye);
#   (d) describe un acto cognitivo ("reconozco", "se", "entiendo") donde la
#       dimension pide una actitud o vivencia ("Reconozco mis fortalezas" no
#       es aceptarse), o valora la tarea ("mis cursos son utiles") en vez de
#       la vivencia de la persona;
#   (e) encaja mejor en otra dimension de la escala (otras_dimensiones: vector
#       con nombre = dimension y valor = definicion).
#' @keywords internal
.nivel_item_llm <- function(openai, modelo, item, dimension, def_dim = "",
                            no_incluye = "", otras_dimensiones = NULL) {
  def_dim <- paste(def_dim %||% "", collapse = " ")
  if (!nzchar(no_incluye %||% "")) no_incluye <- .no_incluye_de(def_dim)
  otras <- otras_dimensiones[names(otras_dimensiones) != dimension]
  prompt <- paste0(
    "Dimension: ", dimension, "\n",
    if (nzchar(def_dim)) paste0("Definicion: ", def_dim, "\n") else "",
    if (nzchar(no_incluye)) paste0("La dimension NO incluye: ", no_incluye, "\n") else "",
    if (length(otras)) paste0("Otras dimensiones de la escala:\n",
                              paste0("- ", names(otras), ": ", otras, collapse = "\n"),
                              "\n") else "",
    "Item: \"", item, "\"\n\n",
    "La dimension se define como una vivencia, percepcion o capacidad de la persona. ",
    "Decide si el item se mantiene en ese nivel. Esta FUERA DE NIVEL si (a) describe ",
    "una conducta, habito, tecnica, tarea o tramite concreto (organizar el horario, ",
    "acomodar el lugar de estudio, hacer gestiones, usar servicios), que es un correlato ",
    "y no la dimension; o (b) su respuesta depende de como funcionen la institucion, ",
    "sus servicios o otras personas y no de la persona que responde",
    if (nzchar(no_incluye)) "; o (c) trata algo que la dimension NO incluye" else "",
    "; o (d) describe un acto cognitivo (reconocer, saber, entender, identificar) donde ",
    "la dimension pide una actitud o vivencia, o valora la tarea o el curso (que sea ",
    "util o interesante) en vez de la vivencia de la persona",
    if (length(otras)) "; o (e) lo que pregunta encaja mejor en una de las otras dimensiones"
    else "",
    ". Responde solo con un JSON: {\"nivel\": \"ok\"} o {\"nivel\": \"fuera\", ",
    "\"criterio\": \"<a|b|c|d|e>\", \"motivo\": \"<10 palabras>\"}.")
  r <- tryCatch(.llamar_openai(openai, list(list(role = "user", content = prompt)),
                               modelo = modelo, temperature = 0, max_tokens = 300L,
                               razonamiento = "low"),
                error = function(e) NULL)
  if (is.null(r)) return(list(ok = NA, motivo = "", criterio = NA_character_))
  j <- tryCatch(jsonlite::fromJSON(.extraer_json_llm(r)), error = function(e) NULL)
  nivel <- tolower(if (is.list(j) && !is.null(j$nivel)) j$nivel else r)
  motivo <- if (is.list(j) && !is.null(j$motivo)) as.character(j$motivo) else ""
  criterio <- if (is.list(j) && !is.null(j$criterio)) as.character(j$criterio)[1]
              else NA_character_
  ok <- if (grepl("fuera", nivel)) FALSE else if (grepl("ok", nivel)) TRUE else NA
  list(ok = ok, motivo = motivo, criterio = if (isFALSE(ok)) criterio else NA_character_)
}

# Lo que una definicion dice que "No incluye" (hasta el punto siguiente).
#' @keywords internal
.no_incluye_de <- function(def) {
  def <- paste(def %||% "", collapse = " ")
  m <- regmatches(def, regexpr("(?i)no incluye\\s*:[^.]*", def, perl = TRUE))
  if (!length(m)) return("")
  trimws(sub("(?i)^no incluye\\s*:", "", m, perl = TRUE))
}


# V del criterio mas bajo de cada item: un juez externo objeta el item por su
# peor criterio, no por el promedio.
#' @keywords internal
.v_minima <- function(v_aiken, criterios) {
  cols <- intersect(paste0("V_", criterios), names(v_aiken))
  if (!length(cols)) return(v_aiken$V_promedio)
  apply(as.matrix(v_aiken[, cols, drop = FALSE]), 1, min, na.rm = TRUE)
}

# Motivo que recibe el generador: que criterio fallo y, si aplica, la jerga,
# la negacion o el supuesto de tiempo. Antes la negacion y el supuesto de
# tiempo marcaban el item pero no llegaban al motivo: el generador leia
# "validez de contenido baja" y podia repetir el mismo defecto.
#' @keywords internal
.motivo_contenido <- function(fila, criterios, umbral, jerga = FALSE,
                              negacion = FALSE, temporal = FALSE) {
  bajos <- character(0)
  for (cr in criterios) {
    val <- fila[[paste0("V_", cr)]]
    if (!is.null(val) && isTRUE(val < umbral))
      bajos <- c(bajos, sprintf("%s baja (V = %.2f)", cr, val))
  }
  redaccion <- c(if (isTRUE(negacion))
                   "trae una negacion o contraste ('no', 'sin', 'aunque'): redactalo en sentido directo y afirmativo",
                 if (isTRUE(temporal))
                   "supone tiempo en la carrera ('cada ciclo', 'desde que inicie'): que lo pueda responder igual un estudiante de primer ciclo")
  m <- if (length(bajos)) paste(bajos, collapse = ", ")
       else if (length(redaccion)) "defecto de redaccion"
       else "validez de contenido baja"
  if (length(redaccion)) m <- paste(c(m, redaccion), collapse = "; ")
  if (isTRUE(jerga))
    m <- paste0(m, "; registro abstracto o tecnico: usa palabras cotidianas de la poblacion")
  if (any(grepl("^claridad", bajos)))
    m <- paste0(m, "; redactalo con palabras simples y un sentido que todos entiendan igual")
  m
}
