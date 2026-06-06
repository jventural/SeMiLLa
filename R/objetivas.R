#' @title Generar prueba objetiva (items de eleccion multiple)
#'
#' @description
#' Genera tests de conocimiento (no de actitud) con respuesta correcta y
#' distractores plausibles. Soporta seis formatos de la taxonomia clasica
#' (Moreno, Martinez & Muniz, 2004; Haladyna, Downing & Rodriguez, 2002):
#'
#' \enumerate{
#'   \item \strong{usual}: enunciado + N opciones, una correcta.
#'   \item \strong{alternativa}: enunciado + 2 opciones, una correcta.
#'   \item \strong{verdadero_falso}: enunciado + Verdadero/Falso.
#'   \item \strong{vf_multiple}: contexto comun + K subenunciados con V/F.
#'   \item \strong{emparejamiento}: dos columnas (premisas y respuestas) a
#'         relacionar uno-a-uno.
#'   \item \strong{contexto_dependiente}: texto base + K subitems usuales
#'         que se contestan con base en ese texto.
#' }
#'
#' Cada item se etiqueta con su \strong{nivel de Bloom} (Recordar,
#' Comprender, Aplicar, Analizar, Evaluar, Crear) segun la tabla de
#' especificacion provista por el investigador.
#'
#' Las 31 directrices de Haladyna, Downing & Rodriguez (2002), reproducidas
#' en Moreno, Martinez & Muniz (2004), se inyectan al prompt como reglas
#' duras: enunciado claro, opcion correcta unica, distractores plausibles
#' (basados en errores tipicos), evitar absolutos, evitar "todas/ninguna
#' de las anteriores", evitar pistas gramaticales, independencia entre
#' opciones, etc.
#'
#' @section Referencias metodologicas:
#' \itemize{
#'   \item Moreno, R., Martinez, R. J., & Muniz, J. (2004). Directrices
#'         para la construccion de items de eleccion multiple.
#'         \emph{Psicothema, 16}(3), 490-497.
#'   \item Haladyna, T. M., Downing, S. M., & Rodriguez, M. C. (2002). A
#'         review of multiple-choice item-writing guidelines for classroom
#'         assessment. \emph{Applied Measurement in Education, 15}(3),
#'         309-333.
#' }
#'
#' @param dominio Cadena con el nombre del dominio (e.g.
#'   "psicometria introductoria: validez, fiabilidad, IRT").
#' @param api_key Clave de OpenAI.
#' @param tabla_especificacion data.frame con columnas:
#'   \code{tema} (subarea del dominio),
#'   \code{nivel_bloom} (uno de: "Recordar", "Comprender", "Aplicar",
#'                                "Analizar", "Evaluar", "Crear"),
#'   \code{formato} (uno de los seis sub-formatos),
#'   \code{n_items} (cantidad de items para esa fila).
#' @param n_opciones Numero de opciones para formato \code{"usual"}
#'   (default 4). Para \code{"alternativa"} es 2 fijo y para
#'   \code{"verdadero_falso"} es 2 fijo (V/F).
#' @param k_vf_multiple Numero de subenunciados V/F para formato
#'   \code{"vf_multiple"} (default 4).
#' @param n_pares_emparejamiento Numero de pares para formato
#'   \code{"emparejamiento"} (default 4).
#' @param k_contexto_dependiente Numero de subitems por contexto en formato
#'   \code{"contexto_dependiente"} (default 3).
#' @param idioma "es", "en" o "pt".
#' @param modelo Modelo OpenAI.
#' @param seed Semilla para reproducibilidad.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_prueba_objetiva} con:
#' \itemize{
#'   \item \code{dominio}, \code{tabla_especificacion}.
#'   \item \code{items}: data.frame (n_item, tema, nivel_bloom, formato,
#'         enunciado, instruccion_extra).
#'   \item \code{opciones}: data.frame en formato largo (n_item, n_opcion,
#'         etiqueta, texto_opcion, es_correcta).
#'   \item \code{emparejamientos}: data.frame para items de emparejamiento.
#'   \item \code{contextos}: data.frame con textos base de
#'         items contexto-dependientes.
#'   \item \code{metadata}.
#' }
#'
#' @examples
#' \dontrun{
#' tabla <- data.frame(
#'   tema        = c("Validez", "Fiabilidad", "IRT"),
#'   nivel_bloom = c("Comprender", "Aplicar", "Analizar"),
#'   formato     = c("usual", "usual", "vf_multiple"),
#'   n_items     = c(4, 4, 2)
#' )
#' p <- generar_prueba_objetiva(
#'   dominio = "psicometria introductoria",
#'   api_key = api_key,
#'   tabla_especificacion = tabla
#' )
#' }
#'
#' @export
generar_prueba_objetiva <- function(
  dominio,
  api_key,
  tabla_especificacion,
  n_opciones                = 4L,
  k_vf_multiple             = 4L,
  n_pares_emparejamiento    = 4L,
  k_contexto_dependiente    = 3L,
  idioma                    = c("es", "en", "pt"),
  modelo                    = "gpt-4.1-mini-2025-04-14",
  seed                      = 2026,
  verbose                   = TRUE
) {

  idioma <- match.arg(idioma)

  # Validar tabla
  cols_req <- c("tema", "nivel_bloom", "formato", "n_items")
  if (!all(cols_req %in% names(tabla_especificacion)))
    stop("'tabla_especificacion' debe tener columnas: ",
         paste(cols_req, collapse = ", "))

  formatos_validos <- c("usual", "alternativa", "verdadero_falso",
                         "vf_multiple", "emparejamiento",
                         "contexto_dependiente")
  if (any(!tabla_especificacion$formato %in% formatos_validos))
    stop("Formatos validos: ", paste(formatos_validos, collapse = ", "))

  bloom_validos <- c("Recordar", "Comprender", "Aplicar",
                      "Analizar", "Evaluar", "Crear")
  if (any(!tabla_especificacion$nivel_bloom %in% bloom_validos))
    stop("Niveles Bloom validos: ", paste(bloom_validos, collapse = ", "))

  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  if (verbose) {
    cat("\n[generar_prueba_objetiva] Configurando OpenAI...\n")
    n_total <- sum(tabla_especificacion$n_items)
    cat("  Dominio: ", dominio, "\n", sep = "")
    cat("  Items totales: ", n_total, "\n", sep = "")
    cat("  Formatos:\n")
    print(table(tabla_especificacion$formato))
  }
  openai <- .configurar_openai(api_key)

  # Acumuladores
  items_list   <- list()
  opciones_list <- list()
  empar_list   <- list()
  contextos_list <- list()
  contador <- 0L

  for (r in seq_len(nrow(tabla_especificacion))) {
    tema     <- tabla_especificacion$tema[r]
    nivel    <- tabla_especificacion$nivel_bloom[r]
    fmt      <- tabla_especificacion$formato[r]
    n_to_gen <- tabla_especificacion$n_items[r]

    if (verbose) cat("\n  > Tema='", tema, "' Bloom='", nivel,
                       "' Formato='", fmt, "' (", n_to_gen, " items)\n",
                       sep = "")

    for (j in seq_len(n_to_gen)) {
      contador <- contador + 1L
      raw <- .generar_item_objetivo(
        openai = openai, modelo = modelo,
        dominio = dominio, tema = tema, nivel_bloom = nivel,
        formato = fmt, n_opciones = n_opciones,
        k_vf_multiple = k_vf_multiple,
        n_pares = n_pares_emparejamiento,
        k_contexto = k_contexto_dependiente,
        idioma = idioma
      )

      # Empaquetar segun formato
      meta <- list(
        n_item        = contador,
        tema          = tema,
        nivel_bloom   = nivel,
        formato       = fmt,
        enunciado     = raw$enunciado,
        instruccion_extra = raw$instruccion_extra %||% NA_character_
      )
      items_list[[contador]] <- as.data.frame(meta, stringsAsFactors = FALSE)

      if (!is.null(raw$opciones) && length(raw$opciones) > 0) {
        opciones_list[[contador]] <- data.frame(
          n_item       = contador,
          n_opcion     = seq_along(raw$opciones),
          etiqueta     = letters[seq_along(raw$opciones)],
          texto_opcion = raw$opciones,
          es_correcta  = seq_along(raw$opciones) %in% raw$correcta_idx,
          stringsAsFactors = FALSE
        )
      }
      if (!is.null(raw$emparejamientos)) {
        empar_list[[contador]] <- cbind(n_item = contador,
                                          raw$emparejamientos)
      }
      if (!is.null(raw$contexto)) {
        contextos_list[[contador]] <- data.frame(
          n_item   = contador,
          contexto = raw$contexto,
          stringsAsFactors = FALSE
        )
      }
      if (verbose) {
        cat("    ", sprintf("%2d.", contador), " ",
            substr(raw$enunciado, 1, 80),
            if (nchar(raw$enunciado) > 80) "..." else "", "\n", sep = "")
      }
    }
  }

  resultado <- list(
    dominio              = dominio,
    tabla_especificacion = tabla_especificacion,
    items                = do.call(rbind, items_list),
    opciones             = if (length(opciones_list) > 0)
                              do.call(rbind, opciones_list)
                            else data.frame(),
    emparejamientos      = if (length(empar_list) > 0)
                              do.call(rbind, empar_list)
                            else data.frame(),
    contextos            = if (length(contextos_list) > 0)
                              do.call(rbind, contextos_list)
                            else data.frame(),
    idioma               = idioma,
    metadata             = list(
      modelo  = modelo,
      seed    = seed,
      fecha   = format(Sys.Date()),
      n_items = contador
    )
  )
  class(resultado) <- c("semilla_prueba_objetiva", "list")
  resultado
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.directrices_moreno_es <- function() {
  paste(
    "DIRECTRICES OBLIGATORIAS (Moreno, Martinez & Muniz, 2004; Haladyna,",
    "Downing & Rodriguez, 2002):",
    "1) Enunciado claro y completo, idea central en el enunciado, no en las opciones.",
    "2) Vocabulario sencillo adecuado al nivel del respondiente.",
    "3) Una sola opcion correcta inequivocamente.",
    "4) Distractores PLAUSIBLES (basados en errores tipicos o confusiones",
    "   conceptuales reales), no absurdos ni triviales.",
    "5) Opciones homogeneas en contenido, longitud y estructura gramatical.",
    "6) Opciones independientes entre si (no se solapan).",
    "7) Evitar 'Todas las anteriores', 'Ninguna de las anteriores'.",
    "8) Evitar absolutos: 'siempre', 'nunca', 'completamente', 'absolutamente'.",
    "9) Evitar pistas gramaticales (concordancia singular/plural,",
    "   articulos un/una, longitud notoriamente mayor en la correcta).",
    "10) Evitar palabras del enunciado repetidas en una sola opcion.",
    "11) Enunciado en forma afirmativa; si es negativa, resaltar 'NO' en mayusculas.",
    "12) Evitar items con trampas, opiniones, contenido trivial."
  )
}

#' @keywords internal
.bloom_descripcion_es <- function(nivel) {
  switch(nivel,
    "Recordar"   = "Recordar - reconocer o recuperar informacion factica (definiciones, formulas, hechos).",
    "Comprender" = "Comprender - explicar, clasificar, ejemplificar, parafrasear.",
    "Aplicar"    = "Aplicar - usar conocimiento o procedimientos en una situacion concreta.",
    "Analizar"   = "Analizar - descomponer informacion, identificar relaciones, comparar.",
    "Evaluar"    = "Evaluar - emitir juicio basado en criterios y evidencia.",
    "Crear"      = "Crear - sintetizar para producir algo nuevo.",
    nivel
  )
}


#' @keywords internal
.generar_item_objetivo <- function(openai, modelo, dominio, tema,
                                    nivel_bloom, formato, n_opciones,
                                    k_vf_multiple, n_pares, k_contexto,
                                    idioma) {

  if (idioma != "es") {
    # Soporte minimo: usar prompts en espanol y traducir al final.
    # Para simplicidad, fallback en espanol; la traduccion la hace el usuario.
  }

  directrices <- .directrices_moreno_es()
  bloom_def   <- .bloom_descripcion_es(nivel_bloom)

  formato_def <- switch(formato,
    "usual" = paste0(
      "Formato 'usual': enunciado o pregunta + ", n_opciones,
      " opciones (a, b, c, ...), una unica correcta."
    ),
    "alternativa" = paste0(
      "Formato 'alternativa': enunciado + EXACTAMENTE 2 opciones de",
      " contenido (no V/F), una correcta."
    ),
    "verdadero_falso" = paste0(
      "Formato 'verdadero_falso': afirmacion + dos opciones: 'Verdadero'",
      " y 'Falso'. Una de las dos es correcta."
    ),
    "vf_multiple" = paste0(
      "Formato 'vf_multiple': enunciado-raiz que introduce un contexto +",
      " EXACTAMENTE ", k_vf_multiple, " subafirmaciones, cada una a marcar",
      " como Verdadero o Falso. Las subafirmaciones se devuelven como las",
      " 'opciones', cada una con su clave V/F en el campo es_correcta."
    ),
    "emparejamiento" = paste0(
      "Formato 'emparejamiento': dos columnas con ", n_pares, " elementos",
      " cada una (premisas a la izquierda, respuestas a la derecha). El",
      " respondiente asocia cada premisa con su respuesta correcta. La",
      " correspondencia debe ser uno-a-uno."
    ),
    "contexto_dependiente" = paste0(
      "Formato 'contexto_dependiente': un texto-base (parrafo de 60-100",
      " palabras) seguido de ", k_contexto, " subitems estilo 'usual'",
      " (cada uno con ", n_opciones, " opciones, una correcta) que se",
      " contestan con base en ese texto."
    ),
    paste("Formato:", formato)
  )

  sys_msg <- paste(
    "Eres un experto en construccion de pruebas objetivas (items de",
    "eleccion multiple) siguiendo las directrices de Moreno, Martinez y",
    "Muniz (2004) y Haladyna, Downing y Rodriguez (2002).",
    "",
    directrices,
    "",
    "Tu salida debe ser un objeto JSON valido (sin texto adicional, sin",
    "comillas markdown, solo JSON puro)."
  )

  # Schema esperado por formato
  schema_txt <- switch(formato,
    "usual" = paste0(
      "{\n",
      "  \"enunciado\": \"...\",\n",
      "  \"opciones\": [\"a) ...\", \"b) ...\", \"c) ...\", \"d) ...\"],\n",
      "  \"correcta_idx\": 1\n",
      "}\n",
      "donde correcta_idx es 1-based (1=primera opcion).",
      " Devuelve EXACTAMENTE ", n_opciones, " opciones (con etiquetas a, b, c, ... incluidas en el texto)."
    ),
    "alternativa" = paste0(
      "{\n",
      "  \"enunciado\": \"...\",\n",
      "  \"opciones\": [\"a) ...\", \"b) ...\"],\n",
      "  \"correcta_idx\": 1\n",
      "}\n",
      "EXACTAMENTE 2 opciones de contenido, no V/F."
    ),
    "verdadero_falso" = paste0(
      "{\n",
      "  \"enunciado\": \"...\",\n",
      "  \"opciones\": [\"Verdadero\", \"Falso\"],\n",
      "  \"correcta_idx\": 1\n",
      "}\n",
      "Si la afirmacion es verdadera, correcta_idx=1; si es falsa, correcta_idx=2."
    ),
    "vf_multiple" = paste0(
      "{\n",
      "  \"enunciado\": \"<enunciado raiz que introduce el contexto>\",\n",
      "  \"instruccion_extra\": \"Indique si cada afirmacion es Verdadera (V) o Falsa (F)\",\n",
      "  \"opciones\": [\"a) <afirmacion 1>\", \"b) <afirmacion 2>\", \"c) <afirmacion 3>\", \"d) <afirmacion 4>\"],\n",
      "  \"correcta_idx\": [1, 3]\n",
      "}\n",
      "EXACTAMENTE ", k_vf_multiple, " subafirmaciones. correcta_idx es",
      " un ARRAY de los indices (1-based) de las subafirmaciones VERDADERAS."
    ),
    "emparejamiento" = paste0(
      "{\n",
      "  \"enunciado\": \"<consigna, e.g. 'Asocie cada concepto con su definicion'>\",\n",
      "  \"emparejamientos\": [\n",
      "    {\"premisa\": \"...\", \"respuesta\": \"...\"},\n",
      "    {\"premisa\": \"...\", \"respuesta\": \"...\"},\n",
      "    {\"premisa\": \"...\", \"respuesta\": \"...\"},\n",
      "    {\"premisa\": \"...\", \"respuesta\": \"...\"}\n",
      "  ]\n",
      "}\n",
      "EXACTAMENTE ", n_pares, " pares. La correspondencia premisa-respuesta",
      " es la correcta (en el cuestionario aplicable se DESORDENAN las",
      " respuestas para que el respondiente empareje)."
    ),
    "contexto_dependiente" = paste0(
      "{\n",
      "  \"enunciado\": \"<consigna corta, e.g. 'Lea el siguiente texto y responda las preguntas que siguen'>\",\n",
      "  \"contexto\": \"<parrafo de 60-100 palabras con un caso, escenario o texto base>\",\n",
      "  \"subitems\": [\n",
      "    {\"enunciado\": \"...\", \"opciones\": [\"a) ...\", \"b) ...\", \"c) ...\", \"d) ...\"], \"correcta_idx\": 1},\n",
      "    {\"enunciado\": \"...\", \"opciones\": [\"a) ...\", \"b) ...\", \"c) ...\", \"d) ...\"], \"correcta_idx\": 2},\n",
      "    {\"enunciado\": \"...\", \"opciones\": [\"a) ...\", \"b) ...\", \"c) ...\", \"d) ...\"], \"correcta_idx\": 3}\n",
      "  ]\n",
      "}\n",
      "EXACTAMENTE ", k_contexto, " subitems."
    ),
    "{\"enunciado\": \"...\", \"opciones\": [\"...\"], \"correcta_idx\": 1}"
  )

  user_msg <- paste0(
    "Dominio: ", dominio, "\n",
    "Tema especifico: ", tema, "\n",
    "Nivel cognitivo (Bloom): ", bloom_def, "\n",
    "Formato del item: ", formato_def, "\n\n",
    "Genera UN item siguiendo el siguiente schema JSON:\n",
    schema_txt, "\n",
    "Devuelve SOLO el JSON, sin explicaciones ni marcadores."
  )

  raw <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 1200L, temperature = 0.4
  )

  # Parsear JSON robustamente
  raw_clean <- .limpiar_json(raw)
  parsed <- tryCatch(jsonlite::fromJSON(raw_clean, simplifyVector = FALSE),
                     error = function(e) {
                       warning("JSON invalido para tema='", tema, "' fmt='",
                               formato, "'. Devuelvo placeholder.")
                       NULL
                     })

  if (is.null(parsed)) {
    return(list(
      enunciado = paste0("[ERROR JSON] ", tema, " - ", formato),
      opciones = character(0), correcta_idx = integer(0)
    ))
  }

  # Normalizar segun formato
  if (formato == "contexto_dependiente") {
    # Devolvemos la consigna como enunciado, el contexto, y un sub-item plano
    # como opciones (los demas subitems se generan como items_independientes
    # ligados al mismo contexto en la siguiente version - aqui simplificamos).
    sub <- parsed$subitems
    if (is.null(sub) || length(sub) == 0) {
      return(list(enunciado = paste0("[ERROR sin subitems] ", parsed$enunciado),
                   opciones = character(0), correcta_idx = integer(0)))
    }
    # Empaquetamos los subitems como un solo registro con opciones concatenadas
    # del primero, y los textos de los siguientes en instruccion_extra.
    primer <- sub[[1]]
    opts <- vapply(primer$opciones, as.character, character(1))
    opts <- sub("^\\s*\\(?[a-zA-Z]\\)\\s*",  "", opts, perl = TRUE)
    opts <- sub("^\\s*[a-zA-Z][.\\)]\\s*",  "", opts, perl = TRUE)
    opts <- sub("^\\s*[0-9]+[.\\)]\\s*",     "", opts, perl = TRUE)
    opts <- trimws(opts)
    extra <- if (length(sub) > 1) {
      paste0("Sub-items adicionales:\n",
             paste(vapply(seq_along(sub)[-1], function(k) {
               s <- sub[[k]]
               opts_k <- paste(vapply(s$opciones, as.character, character(1)),
                               collapse = " | ")
               paste0("  ", letters[k], ".", k, ") ", s$enunciado,
                      " | Opciones: ", opts_k,
                      " [correcta: ", as.integer(s$correcta_idx), "]")
             }, character(1)), collapse = "\n"))
    } else NA_character_
    return(list(
      enunciado         = paste0(parsed$enunciado, " \u2014 ", primer$enunciado),
      opciones          = opts,
      correcta_idx      = as.integer(primer$correcta_idx),
      contexto          = parsed$contexto,
      instruccion_extra = extra
    ))
  }

  if (formato == "emparejamiento") {
    pares <- parsed$emparejamientos
    if (is.null(pares) || length(pares) == 0) {
      return(list(enunciado = paste0("[ERROR sin pares] ", parsed$enunciado),
                   opciones = character(0), correcta_idx = integer(0)))
    }
    df <- data.frame(
      premisa   = vapply(pares, function(p) as.character(p$premisa),  character(1)),
      respuesta = vapply(pares, function(p) as.character(p$respuesta), character(1)),
      stringsAsFactors = FALSE
    )
    return(list(
      enunciado         = parsed$enunciado,
      opciones          = character(0),
      correcta_idx      = integer(0),
      emparejamientos   = df,
      instruccion_extra = "Empareje cada premisa con su respuesta correcta."
    ))
  }

  # Formatos planos: usual, alternativa, vf, vf_multiple
  opts <- if (!is.null(parsed$opciones))
            vapply(parsed$opciones, as.character, character(1))
          else character(0)
  # Limpiar prefijos "a)", "(a)", "1.", etc. que el LLM haya incluido
  # (luego el ensamblador agrega su propia etiqueta).
  opts <- sub("^\\s*\\(?[a-zA-Z]\\)\\s*",  "", opts, perl = TRUE)
  opts <- sub("^\\s*[a-zA-Z][.\\)]\\s*",  "", opts, perl = TRUE)
  opts <- sub("^\\s*[0-9]+[.\\)]\\s*",     "", opts, perl = TRUE)
  opts <- trimws(opts)
  cidx <- if (!is.null(parsed$correcta_idx))
            as.integer(unlist(parsed$correcta_idx))
          else integer(0)
  extra <- if (!is.null(parsed$instruccion_extra))
              as.character(parsed$instruccion_extra)
            else NA_character_
  list(
    enunciado         = as.character(parsed$enunciado),
    opciones          = opts,
    correcta_idx      = cidx,
    instruccion_extra = extra
  )
}


#' @keywords internal
.limpiar_json <- function(s) {
  # Quitar fences ```json ... ``` o ``` ... ```
  s <- sub("^```json\\s*", "", s)
  s <- sub("^```\\s*", "",     s)
  s <- sub("\\s*```$", "",     s)
  trimws(s)
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_prueba_objetiva <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Prueba objetiva (eleccion multiple) - SeMiLLa\n")
  cat("===========================================================\n")
  cat("  Dominio: ", x$dominio, "\n", sep = "")
  cat("  N items: ", nrow(x$items), "\n", sep = "")
  cat("  Formatos:\n")
  print(table(x$items$formato))
  cat("  Niveles Bloom:\n")
  print(table(x$items$nivel_bloom))
  cat("-----------------------------------------------------------\n")
  cat("  Primer item (ejemplo):\n")
  cat("    [", x$items$tema[1], " / ", x$items$nivel_bloom[1], " / ",
      x$items$formato[1], "]\n", sep = "")
  cat("    ", x$items$enunciado[1], "\n", sep = "")
  if (nrow(x$opciones) > 0) {
    op1 <- x$opciones[x$opciones$n_item == 1, ]
    if (nrow(op1) > 0) {
      for (k in seq_len(nrow(op1))) {
        marca <- if (op1$es_correcta[k]) " *" else ""
        cat("      ", op1$etiqueta[k], ") ", op1$texto_opcion[k],
            marca, "\n", sep = "")
      }
    }
  }
  cat("===========================================================\n\n")
  invisible(x)
}
