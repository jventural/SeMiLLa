# =============================================================================
# extraer_psicometria_pdf() - Parsear valores psicometricos de un paper PDF
# =============================================================================
# Lee un PDF de validacion psicometrica con pdftools (R puro) y le pide a un
# LLM (OpenAI) que devuelva un JSON estructurado con: escala, n, dimensiones,
# items, alpha, cargas factoriales, congruencias, correlaciones latentes.
#
# Pensada para alimentar comparar_con_semilla(): el usuario solo necesita el
# PDF y este parser arma automaticamente el "ground truth empirico".
#
# Autor: Dr. Jose Ventura-Leon
# =============================================================================

#' Extraer valores psicometricos de un paper PDF
#'
#' @param archivo_pdf Ruta al PDF
#' @param api_key API key de OpenAI
#' @param modelo Modelo a usar (default: gpt-4.1-mini)
#' @param paginas Vector con paginas a extraer (default: todas).
#'        Util para PDFs largos: pasar c(2:8) restringe el procesamiento
#'        a la seccion metodos+resultados+tablas+items.
#' @param max_chars Truncar a este numero de caracteres (default: 60000).
#'        Evita superar el context window. Si el PDF es mas largo, pasa paginas.
#' @param guardar_json Ruta opcional para guardar el JSON crudo del LLM.
#' @param verbose Mostrar progreso
#'
#' @return Objeto S3 'psicometria_extraida' con:
#'   - escala         : list(nombre, autores, ano, n_participantes, idioma, poblacion)
#'   - constructo     : list(nombre, definicion)
#'   - dimensiones    : data.frame (codigo, nombre, definicion, n_items, alpha,
#'                                  alpha_ic_inf, alpha_ic_sup, tucker, omega)
#'   - items          : data.frame (codigo, dimension, texto, carga_convergente,
#'                                  congruencia_tucker, problematico)
#'   - cor_latentes   : matrix (correlaciones interfactoriales latentes)
#'   - cor_observadas : matrix (correlaciones entre puntajes observados)
#'   - meta           : list (metodo_estimacion, software, criterio_problematico)
#'   - raw_json       : caracter (JSON crudo devuelto por el LLM)
#'
#' @examples
#' \dontrun{
#' emp <- extraer_psicometria_pdf("paper.pdf", api_key = "sk-...")
#' print(emp$dimensiones)
#' print(emp$items)
#' }
extraer_psicometria_pdf <- function(archivo_pdf,
                                    api_key,
                                    modelo = "gpt-4.1-mini",
                                    paginas = NULL,
                                    max_chars = 60000,
                                    guardar_json = NULL,
                                    n_extracciones = 1,
                                    tol_consenso = 0.01,
                                    verbose = TRUE) {
  # n_extracciones >= 2 corre la extraccion varias veces y reporta consenso.
  #   - Valores numericos (alpha, carga, tucker, omega) se aceptan si las
  #     extracciones difieren en <= tol_consenso. Si discrepan, se marca NA y
  #     se anota en attr(resultado, "discrepancias").
  #   - Items textuales se aceptan si ambos textos son identicos.
  if (n_extracciones >= 2) {
    if (verbose) cat("\n=== Doble extraccion con consenso (n_extracciones=",
                     n_extracciones, ") ===\n", sep = "")
    extracciones <- lapply(seq_len(n_extracciones), function(i) {
      if (verbose) cat("\n--- Extraccion ", i, " ---\n", sep = "")
      .extraer_psicometria_pdf_simple(
        archivo_pdf  = archivo_pdf,
        api_key      = api_key,
        modelo       = modelo,
        paginas      = paginas,
        max_chars    = max_chars,
        guardar_json = if (!is.null(guardar_json))
          paste0(tools::file_path_sans_ext(guardar_json), "_run", i, ".json") else NULL,
        verbose      = verbose
      )
    })
    return(.consensuar_extracciones(extracciones, tol = tol_consenso, verbose = verbose))
  }
  # Caso normal: una sola extraccion
  .extraer_psicometria_pdf_simple(
    archivo_pdf = archivo_pdf, api_key = api_key, modelo = modelo,
    paginas = paginas, max_chars = max_chars,
    guardar_json = guardar_json, verbose = verbose
  )
}


# La logica anterior se encapsula en este helper interno
.extraer_psicometria_pdf_simple <- function(archivo_pdf,
                                            api_key,
                                            modelo = "gpt-4.1-mini",
                                            paginas = NULL,
                                            max_chars = 60000,
                                            guardar_json = NULL,
                                            verbose = TRUE) {

  # ---- Validar inputs ----
  if (!file.exists(archivo_pdf)) stop("PDF no encontrado: ", archivo_pdf)
  if (nchar(api_key) < 20) stop("API key invalida")
  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("Instala pdftools: install.packages('pdftools')")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Instala jsonlite: install.packages('jsonlite')")
  if (!requireNamespace("httr",    quietly = TRUE))
    stop("Instala httr: install.packages('httr')")

  if (verbose) cat("\n[1/3] Leyendo PDF con pdftools...\n")
  paginas_txt <- pdftools::pdf_text(archivo_pdf)
  if (!is.null(paginas)) paginas_txt <- paginas_txt[paginas]
  texto <- paste(paginas_txt, collapse = "\n\n---PAGE_BREAK---\n\n")

  if (nchar(texto) > max_chars) {
    if (verbose) cat("  PDF tiene ", nchar(texto),
                     " chars - truncando a ", max_chars, "\n", sep = "")
    texto <- substr(texto, 1, max_chars)
  }

  if (verbose) cat("  Texto extraido: ", nchar(texto), " caracteres\n", sep = "")

  # ---- Prompt sistema con schema estricto ----
  system_prompt <- paste(
    "Eres un asistente experto en psicometria. Extraes valores numericos exactos de papers de validacion de escalas psicometricas.",
    "Devuelves UNICAMENTE un JSON valido segun el schema proporcionado.",
    "Si un VALOR NUMERICO (alpha, carga, tucker) no aparece en el paper, usa null. NUNCA inventes numeros.",
    "Las cargas factoriales deben ser las CONVERGENTES (la dimension teorica del item).",
    "",
    "REGLA CRITICA - ITEMS INICIALES vs FINALES:",
    "Si el paper menciona que la escala ORIGINAL tenia N items y la version FINAL/REDUCIDA quedo con N-k items (k items eliminados por baja carga, redundancia, cross-loading, items invertidos, etc.):",
    "  -> DEBES incluir los N items ORIGINALES (no solo los finales).",
    "  -> Los k items ELIMINADOS deben marcarse con problematico = true y, si el paper especifica la razon, anotarla en el campo 'razon_problema'.",
    "  -> Los items finales (los que permanecen en la version validada) deben marcarse con problematico = false.",
    "Esto es CRUCIAL para evaluar correctamente la deteccion de items debiles.",
    "",
    "REGLA ESPECIAL PARA ITEMS NO PUBLICADOS:",
    "Si el paper NO incluye los textos literales de los items (solo nombra la escala, ej: 'se aplico el MBI-HSS') pero la escala es ESTANDARIZADA y publica (ej: MBI, DASS-21, PANAS, PSS-10, WLEIS, RSES, SWLS, UWES, EAPESA), entonces COMPLETA los items con los textos canonicos en el idioma del paper, manteniendo la asignacion a dimensiones segun el manual de la escala.",
    "Si haces esto, indica en meta.items_completados_por_llm = true.",
    "Si la escala no es estandar y el paper omite los items, deja el campo 'texto' como null."
  )

  schema <- '{
  "escala": {
    "nombre": "string (ej: WLEIS, DASS-21)",
    "autores": "string (autores del paper)",
    "ano": "integer",
    "n_participantes": "integer",
    "idioma": "string (es/en/pt)",
    "poblacion": "string"
  },
  "constructo": {
    "nombre": "string",
    "definicion": "string"
  },
  "dimensiones": [
    {
      "codigo": "string corto (ej: SEA, ANS)",
      "nombre": "string completo",
      "definicion": "string",
      "n_items": "integer",
      "alpha": "number (Cronbach alpha o omega si solo reportan ese)",
      "alpha_ic_inf": "number or null",
      "alpha_ic_sup": "number or null",
      "tucker": "number or null (coef congruencia Tucker)",
      "omega": "number or null"
    }
  ],
  "items": [
    {
      "codigo": "string (ej: WLEIS1, DASS3)",
      "dimension": "string (codigo de la dimension)",
      "texto": "string (texto del item en el idioma del paper, null si no aparece)",
      "carga_convergente": "number or null (carga factorial en su dimension teorica)",
      "congruencia_tucker": "number or null (item-level si reportada)",
      "problematico": "boolean (true si el paper lo marca como problematico: eliminado en version final, baja carga, redundancia, cross-loading, item invertido descartado, etc.)",
      "razon_problema": "string or null (ej: eliminado por baja carga, cross-loading, item invertido, redundancia)"
    }
  ],
  "cor_latentes": "matrix or null (correlaciones interfactoriales latentes)",
  "cor_observadas": "matrix or null (correlaciones entre puntajes)",
  "meta": {
    "metodo_estimacion": "string (ej: WLSMV, ULS, ML, MLR)",
    "software": "string (ej: Mplus, lavaan, Factor)",
    "criterio_problematico": "string (criterio que el paper usa para marcar items debiles)"
  }
}'

  user_prompt <- paste0(
    "Extrae los valores psicometricos del siguiente paper. Devuelve UNICAMENTE el JSON siguiendo este schema exacto:\n\n",
    schema,
    "\n\n---PAPER---\n\n",
    texto
  )

  # ---- Llamar al LLM ----
  if (verbose) cat("[2/3] Enviando al LLM (", modelo, ")...\n", sep = "")

  body <- list(
    model = modelo,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    ),
    temperature = 0,
    response_format = list(type = "json_object")
  )

  resp <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(180)
  )

  if (httr::status_code(resp) != 200) {
    stop("Error API OpenAI: ", httr::status_code(resp), "\n",
         httr::content(resp, as = "text", encoding = "UTF-8"))
  }

  contenido <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  raw_json  <- contenido$choices[[1]]$message$content

  if (!is.null(guardar_json)) writeLines(raw_json, guardar_json)

  # ---- Parsear JSON ----
  if (verbose) cat("[3/3] Parseando JSON...\n")
  parsed <- tryCatch(
    jsonlite::fromJSON(raw_json, simplifyVector = TRUE, simplifyDataFrame = TRUE),
    error = function(e) {
      stop("JSON invalido devuelto por el LLM:\n", conditionMessage(e),
           "\n\n---RAW---\n", raw_json)
    }
  )

  # ---- Normalizar a data.frames ----
  if (!is.null(parsed$dimensiones) && !is.data.frame(parsed$dimensiones)) {
    parsed$dimensiones <- do.call(rbind, lapply(parsed$dimensiones, as.data.frame))
  }
  if (!is.null(parsed$items) && !is.data.frame(parsed$items)) {
    parsed$items <- do.call(rbind, lapply(parsed$items, as.data.frame))
  }

  # ---- Convertir correlaciones a matrices si vienen como list-of-lists ----
  to_matrix <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.matrix(x)) return(x)
    if (is.data.frame(x)) return(as.matrix(x))
    if (is.list(x)) return(do.call(rbind, lapply(x, unlist)))
    NULL
  }
  parsed$cor_latentes   <- to_matrix(parsed$cor_latentes)
  parsed$cor_observadas <- to_matrix(parsed$cor_observadas)

  parsed$raw_json <- raw_json
  class(parsed) <- c("psicometria_extraida", "list")

  if (verbose) {
    cat("\n--- EXTRACCION COMPLETA ---\n")
    cat("Escala:        ", parsed$escala$nombre,         "\n", sep = "")
    cat("Autores:       ", parsed$escala$autores,        "\n", sep = "")
    cat("N:             ", parsed$escala$n_participantes,"\n", sep = "")
    cat("Dimensiones:   ", nrow(parsed$dimensiones),     "\n", sep = "")
    cat("Items:         ", nrow(parsed$items),           "\n", sep = "")
    if (!is.null(parsed$dimensiones$alpha)) {
      cat("Alphas:        ",
          paste(round(parsed$dimensiones$alpha, 3), collapse = ", "), "\n", sep = "")
    }
  }

  parsed
}


# =============================================================================
# Helper: consensuar extracciones multiples
# =============================================================================
.consensuar_extracciones <- function(extracciones, tol = 0.01, verbose = TRUE) {
  k <- length(extracciones)
  if (k < 2) return(extracciones[[1]])

  consenso <- extracciones[[1]]
  discrepancias <- list()

  # Numericos en dimensiones
  campos_dim <- c("alpha", "alpha_ic_inf", "alpha_ic_sup", "tucker", "omega", "n_items")
  if (!is.null(consenso$dimensiones) && nrow(consenso$dimensiones) > 0) {
    for (i in seq_len(nrow(consenso$dimensiones))) {
      cod <- consenso$dimensiones$codigo[i]
      for (f in intersect(campos_dim, names(consenso$dimensiones))) {
        vals <- sapply(extracciones, function(e) {
          if (is.null(e$dimensiones) || !cod %in% e$dimensiones$codigo) return(NA_real_)
          v <- e$dimensiones[e$dimensiones$codigo == cod, f]
          if (length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[1]))
        })
        if (all(is.na(vals))) next
        # rango entre extracciones
        rng <- diff(range(vals, na.rm = TRUE))
        if (!is.na(rng) && rng > tol) {
          discrepancias[[length(discrepancias)+1]] <- list(
            tabla = "dimensiones", codigo = cod, campo = f, valores = vals
          )
          consenso$dimensiones[i, f] <- NA  # marcar como inconsistente
        }
      }
    }
  }

  # Numericos en items
  campos_item <- c("carga_convergente", "congruencia_tucker")
  if (!is.null(consenso$items) && nrow(consenso$items) > 0) {
    for (i in seq_len(nrow(consenso$items))) {
      cod <- consenso$items$codigo[i]
      for (f in intersect(campos_item, names(consenso$items))) {
        vals <- sapply(extracciones, function(e) {
          if (is.null(e$items) || !cod %in% e$items$codigo) return(NA_real_)
          v <- e$items[e$items$codigo == cod, f]
          if (length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[1]))
        })
        if (all(is.na(vals))) next
        rng <- diff(range(vals, na.rm = TRUE))
        if (!is.na(rng) && rng > tol) {
          discrepancias[[length(discrepancias)+1]] <- list(
            tabla = "items", codigo = cod, campo = f, valores = vals
          )
          consenso$items[i, f] <- NA
        }
      }
      # Texto de item: consenso por igualdad exacta
      if ("texto" %in% names(consenso$items)) {
        textos <- sapply(extracciones, function(e) {
          if (is.null(e$items) || !cod %in% e$items$codigo) return(NA_character_)
          t <- e$items$texto[e$items$codigo == cod][1]
          if (is.null(t) || is.na(t)) NA_character_ else as.character(t)
        })
        textos <- textos[!is.na(textos)]
        if (length(unique(textos)) > 1) {
          discrepancias[[length(discrepancias)+1]] <- list(
            tabla = "items", codigo = cod, campo = "texto", valores = textos
          )
          # Mantener el texto mas frecuente
          tab <- table(textos)
          consenso$items$texto[i] <- names(tab)[which.max(tab)]
        }
      }
    }
  }

  if (verbose) {
    cat("\n--- CONSENSO ---\n")
    cat("Extracciones combinadas: ", k, "\n", sep = "")
    cat("Discrepancias detectadas: ", length(discrepancias),
        " (tol = ", tol, ")\n", sep = "")
    if (length(discrepancias) > 0 && length(discrepancias) <= 10) {
      for (d in discrepancias) {
        cat("  - [", d$tabla, "] ", d$codigo, " $ ", d$campo,
            ": ", paste(round(d$valores, 3), collapse = " vs "), "\n", sep = "")
      }
    }
  }

  attr(consenso, "discrepancias")     <- discrepancias
  attr(consenso, "n_extracciones")    <- k
  attr(consenso, "tol_consenso")      <- tol
  consenso$meta$n_extracciones        <- k
  consenso$meta$n_discrepancias       <- length(discrepancias)
  consenso
}


#' @export
print.psicometria_extraida <- function(x, ...) {
  cat("\n=== PSICOMETRIA EXTRAIDA ===\n\n")
  cat("Escala:    ", x$escala$nombre, " (", x$escala$ano, ")\n", sep = "")
  cat("Autores:   ", x$escala$autores, "\n", sep = "")
  cat("N:         ", x$escala$n_participantes, "\n", sep = "")
  cat("Idioma:    ", x$escala$idioma, "\n", sep = "")
  cat("Poblacion: ", x$escala$poblacion, "\n\n", sep = "")
  cat("--- DIMENSIONES ---\n")
  print(x$dimensiones)
  cat("\n--- ITEMS (primeros 5) ---\n")
  print(utils::head(x$items, 5))
  if (!is.null(x$meta$metodo_estimacion))
    cat("\nMetodo:    ", x$meta$metodo_estimacion, "\n", sep = "")
  invisible(x)
}


#' Convertir objeto psicometria_extraida a Excel listo para semilla(fuente='usuario')
#'
#' @param x Objeto psicometria_extraida
#' @param archivo Ruta xlsx de salida
#' @return Invisible: data.frame escrito
#'
#' @details
#' Genera el formato esperado por leer_escala()/semilla(fuente='usuario'):
#' columnas constructo, definicion_constructo, dimension, definicion_dimension,
#' codigo, item. Si algun item no tiene texto extraido, intentara reusar el
#' codigo como placeholder (el usuario tendra que rellenarlo).
psicometria_a_excel <- function(x, archivo) {
  if (!inherits(x, "psicometria_extraida"))
    stop("x debe ser psicometria_extraida")
  if (!requireNamespace("writexl", quietly = TRUE))
    stop("Instala writexl: install.packages('writexl')")

  items <- x$items
  dims  <- x$dimensiones

  # Mapear dimension de items -> nombre completo + definicion (desde dims)
  items$dim_nombre <- dims$nombre[match(items$dimension, dims$codigo)]
  items$dim_def    <- dims$definicion[match(items$dimension, dims$codigo)]

  # Ordenar por dimension
  items <- items[order(match(items$dimension, dims$codigo)), ]

  # Si falta texto, usar placeholder
  txt_faltante <- is.na(items$texto) | items$texto == ""
  if (any(txt_faltante)) {
    warning(sum(txt_faltante), " items sin texto extraido. Edita el Excel manualmente.")
    items$texto[txt_faltante] <- paste0("[PENDIENTE: ", items$codigo[txt_faltante], "]")
  }

  datos <- data.frame(
    constructo            = c(x$constructo$nombre, rep("", nrow(items) - 1)),
    definicion_constructo = c(x$constructo$definicion, rep("", nrow(items) - 1)),
    dimension             = ifelse(is.na(items$dim_nombre), items$dimension, items$dim_nombre),
    definicion_dimension  = ifelse(is.na(items$dim_def), "", items$dim_def),
    codigo                = items$codigo,
    item                  = items$texto,
    stringsAsFactors      = FALSE
  )

  writexl::write_xlsx(datos, archivo)
  cat("Excel generado: ", archivo, " (", nrow(datos), " items)\n", sep = "")
  invisible(datos)
}
