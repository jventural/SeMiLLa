#' @title Verificar la clave de una prueba objetiva con un LLM independiente
#'
#' @description
#' Somete cada item de un objeto \code{semilla_prueba_objetiva} a un LLM que
#' actua como EXAMINADO: recibe el enunciado y las opciones SIN ninguna marca
#' de cual es la correcta, y responde usando solo su conocimiento del dominio.
#' Luego se compara su respuesta con la clave declarada al generar la prueba.
#'
#' La logica es de auditoria regresiva: si un experto independiente no llega
#' a la clave, el item puede tener (a) una clave equivocada, (b) mas de una
#' opcion defendible, o (c) un enunciado ambiguo. Los tres casos violan la
#' directriz de "una sola opcion correcta inequivocamente" (Moreno, Martinez
#' & Muniz, 2004) y ameritan revision humana antes de aplicar la prueba.
#'
#' Con \code{n_resoluciones > 1} el item se resuelve varias veces (la primera
#' pasada con temperatura 0, las siguientes con temperatura moderada) y la
#' respuesta final se decide por voto mayoritario, lo que reduce falsos
#' desacuerdos por variabilidad del LLM.
#'
#' @param escala_o Objeto \code{semilla_prueba_objetiva} devuelto por
#'   \code{\link{generar_prueba_objetiva}}.
#' @param api_key Clave de OpenAI (o del proveedor activo, ver
#'   \code{\link{usar_proveedor}}).
#' @param modelo Modelo LLM que actua como examinado. Se recomienda un modelo
#'   DISTINTO (o al menos una instancia independiente) del que genero los
#'   items, para que la verificacion no herede los mismos sesgos.
#' @param n_resoluciones Numero de veces que se resuelve cada item
#'   (default 1). Con valores > 1 se aplica voto mayoritario.
#' @param seed Semilla para reproducibilidad.
#' @param verbose Mostrar progreso.
#'
#' @return El mismo objeto \code{semilla_prueba_objetiva} con dos agregados:
#' \itemize{
#'   \item \code{verificacion}: data.frame de clase
#'         \code{semilla_verificacion_clave} con columnas \code{n_item},
#'         \code{tema}, \code{formato}, \code{clave_declarada},
#'         \code{respuesta_llm}, \code{coincide}, \code{seguridad} y
#'         \code{observacion} (razonamiento del LLM cuando hay desacuerdo).
#'   \item \code{metadata$verificacion}: lista con \code{modelo},
#'         \code{fecha}, \code{n_verificados}, \code{n_coincidencias},
#'         \code{tasa_coincidencia} y \code{n_discrepancias}.
#' }
#'
#' @examples
#' \dontrun{
#' p <- generar_prueba_objetiva(
#'   dominio = "psicometria introductoria",
#'   api_key = api_key,
#'   tabla_especificacion = tabla
#' )
#' p <- verificar_clave(p, api_key = api_key)
#' p$verificacion                      # detalle item por item
#' subset(p$verificacion, !coincide)   # solo discrepancias
#' }
#'
#' @seealso \code{\link{generar_prueba_objetiva}},
#'   \code{\link{ensamblar_prueba_objetiva}}
#'
#' @export
verificar_clave <- function(
  escala_o,
  api_key,
  modelo         = "gpt-4.1-mini-2025-04-14",
  n_resoluciones = 1L,
  seed           = 2026,
  verbose        = TRUE
) {

  if (!inherits(escala_o, "semilla_prueba_objetiva"))
    stop("'escala_o' debe ser un objeto semilla_prueba_objetiva.")
  n_resoluciones <- max(1L, as.integer(n_resoluciones))

  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  if (verbose) {
    cat("\n[verificar_clave] Configurando cliente LLM...\n")
    cat("  Modelo examinado: ", modelo, "\n", sep = "")
    cat("  Items a verificar: ", nrow(escala_o$items), "\n", sep = "")
    if (n_resoluciones > 1)
      cat("  Resoluciones por item: ", n_resoluciones,
          " (voto mayoritario)\n", sep = "")
  }
  openai <- .configurar_openai(api_key, modelo = modelo)

  items     <- escala_o$items
  opciones  <- escala_o$opciones
  empar     <- escala_o$emparejamientos
  contextos <- escala_o$contextos

  filas <- vector("list", nrow(items))

  for (i in seq_len(nrow(items))) {
    n   <- items$n_item[i]
    fmt <- items$formato[i]

    res <- .resolver_item_examinado(
      openai = openai, modelo = modelo,
      dominio = escala_o$dominio,
      item_fila = items[i, , drop = FALSE],
      opciones_i = opciones[opciones$n_item == n, , drop = FALSE],
      empar_i    = if (nrow(empar) > 0)
                     empar[empar$n_item == n, , drop = FALSE]
                   else empar,
      contexto_i = if (nrow(contextos) > 0)
                     contextos[contextos$n_item == n, , drop = FALSE]
                   else contextos,
      n_resoluciones = n_resoluciones
    )

    filas[[i]] <- data.frame(
      n_item          = n,
      tema            = items$tema[i],
      formato         = fmt,
      clave_declarada = res$clave_declarada,
      respuesta_llm   = res$respuesta_llm,
      coincide        = res$coincide,
      seguridad       = res$seguridad,
      observacion     = res$observacion,
      stringsAsFactors = FALSE
    )

    if (verbose) {
      marca <- if (is.na(res$coincide)) "?" else if (res$coincide)
                 .color_check() else .color_warning()
      cat("  ", marca, " ", sprintf("%2d.", n),
          " [", fmt, "] clave=", res$clave_declarada,
          " | LLM=", res$respuesta_llm, "\n", sep = "")
    }
  }

  verificacion <- do.call(rbind, filas)
  class(verificacion) <- c("semilla_verificacion_clave", "data.frame")

  n_eval  <- sum(!is.na(verificacion$coincide))
  n_coin  <- sum(verificacion$coincide, na.rm = TRUE)
  tasa    <- if (n_eval > 0) n_coin / n_eval else NA_real_

  escala_o$verificacion <- verificacion
  escala_o$metadata$verificacion <- list(
    modelo            = modelo,
    fecha             = format(Sys.Date()),
    n_resoluciones    = n_resoluciones,
    n_verificados     = n_eval,
    n_coincidencias   = n_coin,
    tasa_coincidencia = tasa,
    n_discrepancias   = n_eval - n_coin
  )

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat("  Coincidencia clave-examinado: ", n_coin, "/", n_eval,
        if (!is.na(tasa)) paste0(" (", round(100 * tasa, 1), "%)") else "",
        "\n", sep = "")
    if (n_eval - n_coin > 0) {
      cat("  ", .color_warning(),
          " Revise las discrepancias con: subset(x$verificacion, !coincide)\n",
          sep = "")
    }
    cat(.linea("-"), "\n\n", sep = "")
  }

  escala_o
}


# =============================================================================
# Helpers internos
# =============================================================================

# Resuelve UN item en modo examinado (sin acceso a la clave) y lo compara
# con la clave declarada. Devuelve lista con clave_declarada, respuesta_llm,
# coincide, seguridad y observacion.

#' @keywords internal
.resolver_item_examinado <- function(openai, modelo, dominio, item_fila,
                                     opciones_i, empar_i, contexto_i,
                                     n_resoluciones) {

  fmt       <- item_fila$formato[1]
  n         <- item_fila$n_item[1]
  enunciado <- item_fila$enunciado[1]

  sin_datos <- function(motivo) {
    list(clave_declarada = "-", respuesta_llm = "-", coincide = NA,
         seguridad = NA_character_, observacion = motivo)
  }

  sys_msg <- paste0(
    "Eres una persona experta en '", dominio, "' que rinde una prueba",
    " objetiva. Responde cada item usando UNICAMENTE tu conocimiento del",
    " tema. No tienes acceso a la clave de respuestas ni a pistas del",
    " material. Si dudas entre opciones, elige la mas defendible y",
    " reporta tu seguridad como 'media' o 'baja'.",
    " Tu salida debe ser un objeto JSON valido, sin texto adicional."
  )

  # ---------------------------------------------------------------------------
  # Construir la vista del examinado + clave declarada, segun formato
  # ---------------------------------------------------------------------------
  if (fmt == "emparejamiento") {
    if (is.null(empar_i) || nrow(empar_i) == 0)
      return(sin_datos("Item sin pares de emparejamiento; no se verifico."))

    # Mismo barajado que la version aplicable (.construir_md_objetivas usa
    # set.seed(n)), de modo que el examinado ve lo que veria un respondiente.
    set.seed(n)
    orden <- sample(seq_len(nrow(empar_i)))
    premisas_txt <- paste0(seq_len(nrow(empar_i)), ") ", empar_i$premisa,
                           collapse = "\n")
    respuestas_txt <- paste0(letters[seq_along(orden)], ") ",
                             empar_i$respuesta[orden], collapse = "\n")
    # Para la premisa k, la letra correcta es la posicion donde quedo su
    # respuesta original tras el barajado.
    letras_correctas <- letters[match(seq_len(nrow(empar_i)), orden)]
    clave_txt <- paste(seq_len(nrow(empar_i)), "->", letras_correctas,
                       collapse = " · ")

    user_msg <- paste0(
      "Item de emparejamiento.\n",
      "Consigna: ", enunciado, "\n\n",
      "Premisas:\n", premisas_txt, "\n\n",
      "Respuestas:\n", respuestas_txt, "\n\n",
      "Asocia cada premisa (numero) con UNA respuesta (letra). La",
      " correspondencia es uno a uno.\n",
      "Devuelve JSON: {\"pares\": {\"1\": \"<letra>\", \"2\": \"<letra>\", ...},",
      " \"seguridad\": \"alta|media|baja\", \"razon\": \"<max 25 palabras>\"}"
    )

    votos <- .votar_resoluciones(openai, modelo, sys_msg, user_msg,
                                 n_resoluciones, function(parsed) {
      pares <- parsed$pares
      if (is.null(pares)) return(NULL)
      k <- seq_len(nrow(empar_i))
      letras <- tolower(vapply(as.character(k), function(kk) {
        v <- pares[[kk]]
        if (is.null(v)) "" else substr(trimws(as.character(v)), 1, 1)
      }, character(1)))
      if (any(!nzchar(letras))) return(NULL)
      paste(k, "->", letras, collapse = " · ")
    })
    if (is.null(votos$valor))
      return(sin_datos("El LLM no devolvio un emparejamiento parseable."))

    coincide <- identical(votos$valor, clave_txt)
    return(list(
      clave_declarada = clave_txt,
      respuesta_llm   = votos$valor,
      coincide        = coincide,
      seguridad       = votos$seguridad,
      observacion     = if (coincide) NA_character_ else votos$razon
    ))
  }

  # Resto de formatos: requieren opciones
  if (is.null(opciones_i) || nrow(opciones_i) == 0)
    return(sin_datos("Item sin opciones (posible error de generacion); no se verifico."))

  opciones_txt <- paste0(seq_len(nrow(opciones_i)), ") ",
                         opciones_i$texto_opcion, collapse = "\n")

  bloque_contexto <- if (!is.null(contexto_i) && nrow(contexto_i) > 0) {
    paste0("Texto base:\n", contexto_i$contexto[1], "\n\n")
  } else ""

  if (fmt == "vf_multiple") {
    idx_verdaderas <- sort(which(opciones_i$es_correcta))
    clave_txt <- paste(letters[seq_len(nrow(opciones_i))], "=",
                       ifelse(opciones_i$es_correcta, "V", "F"),
                       collapse = " · ")

    user_msg <- paste0(
      "Item de verdadero/falso multiple.\n",
      bloque_contexto,
      "Enunciado raiz: ", enunciado, "\n\n",
      "Afirmaciones:\n", opciones_txt, "\n\n",
      "Indica cuales afirmaciones son VERDADERAS.\n",
      "Devuelve JSON: {\"verdaderas\": [<numeros de las afirmaciones",
      " verdaderas>], \"seguridad\": \"alta|media|baja\",",
      " \"razon\": \"<max 25 palabras>\"}"
    )

    votos <- .votar_resoluciones(openai, modelo, sys_msg, user_msg,
                                 n_resoluciones, function(parsed) {
      v <- suppressWarnings(as.integer(unlist(parsed$verdaderas)))
      v <- sort(unique(v[!is.na(v) & v >= 1 & v <= nrow(opciones_i)]))
      paste(v, collapse = ",")
    })
    if (is.null(votos$valor))
      return(sin_datos("El LLM no devolvio un JSON parseable."))

    idx_llm <- suppressWarnings(
      as.integer(strsplit(votos$valor, ",", fixed = TRUE)[[1]]))
    idx_llm <- idx_llm[!is.na(idx_llm)]
    resp_txt <- paste(letters[seq_len(nrow(opciones_i))], "=",
                      ifelse(seq_len(nrow(opciones_i)) %in% idx_llm, "V", "F"),
                      collapse = " · ")
    coincide <- identical(sort(idx_llm), idx_verdaderas)
    return(list(
      clave_declarada = clave_txt,
      respuesta_llm   = resp_txt,
      coincide        = coincide,
      seguridad       = votos$seguridad,
      observacion     = if (coincide) NA_character_ else votos$razon
    ))
  }

  # usual / alternativa / verdadero_falso / contexto_dependiente
  idx_correcta <- which(opciones_i$es_correcta)[1]
  if (is.na(idx_correcta))
    return(sin_datos("Item sin opcion marcada como correcta; no se verifico."))
  clave_txt <- letters[idx_correcta]

  user_msg <- paste0(
    "Item de opcion multiple (una sola respuesta correcta).\n",
    bloque_contexto,
    "Enunciado: ", enunciado, "\n\n",
    "Opciones:\n", opciones_txt, "\n\n",
    "Devuelve JSON: {\"eleccion\": <numero de la opcion elegida>,",
    " \"seguridad\": \"alta|media|baja\", \"razon\": \"<max 25 palabras>\"}"
  )

  votos <- .votar_resoluciones(openai, modelo, sys_msg, user_msg,
                               n_resoluciones, function(parsed) {
    k <- suppressWarnings(as.integer(parsed$eleccion))
    if (is.na(k) || k < 1 || k > nrow(opciones_i)) return(NULL)
    as.character(k)
  })
  if (is.null(votos$valor))
    return(sin_datos("El LLM no devolvio una eleccion parseable."))

  k_llm <- as.integer(votos$valor)
  coincide <- identical(k_llm, as.integer(idx_correcta))
  list(
    clave_declarada = clave_txt,
    respuesta_llm   = letters[k_llm],
    coincide        = coincide,
    seguridad       = votos$seguridad,
    observacion     = if (coincide) NA_character_ else votos$razon
  )
}


# Ejecuta n_resoluciones pasadas y decide por voto mayoritario. La primera
# pasada usa temperatura 0 (determinista); las adicionales, 0.7, con un
# marcador de pasada en el mensaje para que el cache de SeMiLLa no las
# colapse en una sola llamada. `extraer` recibe el JSON parseado y devuelve
# una representacion canonica (string) de la respuesta, o NULL si es invalida.

#' @keywords internal
.votar_resoluciones <- function(openai, modelo, sys_msg, user_msg,
                                n_resoluciones, extraer) {
  valores    <- character(0)
  seguridades <- character(0)
  razones    <- character(0)

  for (p in seq_len(n_resoluciones)) {
    msg_p <- if (p == 1) user_msg else
      paste0(user_msg, "\n[pasada independiente ", p, "]")
    raw <- tryCatch(
      .llamar_openai(
        openai = openai,
        messages = list(
          list(role = "system", content = sys_msg),
          list(role = "user",   content = msg_p)
        ),
        modelo = modelo, max_tokens = 400L,
        temperature = if (p == 1) 0 else 0.7,
        razonamiento = "low"   # resolver un item es un JUICIO, no generacion
      ),
      error = function(e) NULL
    )
    if (is.null(raw)) next

    parsed <- tryCatch(
      jsonlite::fromJSON(.limpiar_json(raw), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    val <- tryCatch(extraer(parsed), error = function(e) NULL)
    if (is.null(val) || !nzchar(val)) next

    valores     <- c(valores, val)
    seguridades <- c(seguridades,
                     tolower(as.character(parsed$seguridad %||% NA)))
    razones     <- c(razones, as.character(parsed$razon %||% NA))
  }

  if (length(valores) == 0)
    return(list(valor = NULL, seguridad = NA_character_,
                razon = NA_character_))

  tab <- sort(table(valores), decreasing = TRUE)
  ganador <- names(tab)[1]
  # En empate gana la pasada determinista (la primera aparicion)
  if (length(tab) > 1 && tab[1] == tab[2]) ganador <- valores[1]
  idx <- which(valores == ganador)[1]

  list(valor = ganador,
       seguridad = seguridades[idx],
       razon = razones[idx])
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_verificacion_clave <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Verificacion de clave con examinado LLM (SeMiLLa)\n")
  cat("===========================================================\n")
  n_eval <- sum(!is.na(x$coincide))
  n_coin <- sum(x$coincide, na.rm = TRUE)
  cat("  Items verificados : ", n_eval, " de ", nrow(x), "\n", sep = "")
  cat("  Coincidencias     : ", n_coin,
      if (n_eval > 0) paste0(" (", round(100 * n_coin / n_eval, 1), "%)")
      else "", "\n", sep = "")
  disc <- x[!is.na(x$coincide) & !x$coincide, , drop = FALSE]
  if (nrow(disc) > 0) {
    cat("-----------------------------------------------------------\n")
    cat("  DISCREPANCIAS (revisar manualmente):\n")
    for (i in seq_len(nrow(disc))) {
      cat("    Item ", disc$n_item[i], " [", disc$formato[i], "] clave=",
          disc$clave_declarada[i], " vs LLM=", disc$respuesta_llm[i],
          " (seguridad ", disc$seguridad[i], ")\n", sep = "")
      if (!is.na(disc$observacion[i]))
        cat("      Razon LLM: ", disc$observacion[i], "\n", sep = "")
    }
  } else if (n_eval > 0) {
    cat("  Sin discrepancias: la clave resistio la resolucion independiente.\n")
  }
  no_eval <- x[is.na(x$coincide), , drop = FALSE]
  if (nrow(no_eval) > 0) {
    cat("-----------------------------------------------------------\n")
    cat("  NO VERIFICADOS:\n")
    for (i in seq_len(nrow(no_eval))) {
      cat("    Item ", no_eval$n_item[i], ": ", no_eval$observacion[i],
          "\n", sep = "")
    }
  }
  cat("===========================================================\n\n")
  invisible(x)
}
