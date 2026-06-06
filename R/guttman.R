#' @title Generar escala con formato de respuesta Guttman
#'
#' @description
#' Genera un instrumento donde cada item tiene un stem (raiz) y un conjunto
#' de alternativas que representan niveles ORDENADOS del constructo. A
#' diferencia del formato Likert (opciones genericas) o del formato basado
#' en historias (juicio sobre vinetas), las alternativas Guttman son
#' afirmaciones especificas y monotonicamente progresivas anclas a un
#' "construct map" (mapa del constructo) provisto por el investigador.
#'
#' Referencia metodologica:
#'   Wilson, M., Bathia, S., Morell, L., Gochyyev, P., Koo, B. W., & Smith,
#'   R. (2023). Seeking a better balance between efficiency and
#'   interpretability: Comparing the Likert response format with the
#'   Guttman response format. \emph{Psychological Methods, 28}(6),
#'   1358-1373.
#'
#' Estructura del item Guttman:
#' \preformatted{
#'   <Stem-pregunta o stem-afirmacion raiz>
#'   (a) Alternativa - Nivel 0 (mas bajo)
#'   (b) Alternativa - Nivel 1
#'   (c) Alternativa - Nivel 2
#'   (d) Alternativa - Nivel 3
#'   (e) Alternativa - Nivel 4 (mas alto)
#' }
#'
#' Cada respondiente elige UNA alternativa. El analisis posterior se hace
#' con un Partial Credit Model (PCM, Rasch politomico).
#'
#' @param concepto Cadena con la definicion del constructo.
#' @param api_key Clave de OpenAI.
#' @param construct_map Lista NOMBRADA con los niveles del constructo,
#'   ordenados de mas bajo a mas alto. Cada elemento es una descripcion
#'   cualitativa del nivel. Ejemplo:
#'   \code{list(
#'     "Nivel 0" = "Desconocimiento del rol del investigador",
#'     "Nivel 1" = "Curiosidad inicial sobre que es la investigacion",
#'     ...
#'   )}.
#' @param n_items Numero de stems a generar (default 12).
#' @param facetas Vector character con las facetas/strands del constructo
#'   (opcional). Si se proveen, los items se distribuiran entre ellas.
#' @param items_por_faceta Vector con la cantidad de items por faceta
#'   (debe sumar \code{n_items}). Si NULL y \code{facetas} esta dado,
#'   se distribuyen uniformemente.
#' @param idioma "es" o "en".
#' @param modelo Modelo OpenAI.
#' @param seed Semilla para reproducibilidad.
#' @param max_palabras_stem Limite de palabras del stem.
#' @param max_palabras_alternativa Limite por alternativa.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_guttman} con:
#' \itemize{
#'   \item \code{construct_map}: la lista de niveles
#'   \item \code{items}: data.frame (n_item, faceta, stem)
#'   \item \code{alternativas}: data.frame en formato largo
#'         (n_item, nivel_idx, nivel_nombre, alternativa)
#'   \item \code{concepto}, \code{idioma}, \code{metadata}
#' }
#'
#' @examples
#' \dontrun{
#' construct_map <- list(
#'   "Nivel 0" = "Desconocimiento del rol del investigador",
#'   "Nivel 1" = "Curiosidad inicial",
#'   "Nivel 2" = "Participacion incipiente",
#'   "Nivel 3" = "Comodidad con la identidad de investigador",
#'   "Nivel 4" = "Integracion plena de la identidad"
#' )
#' g <- generar_escala_guttman(
#'   concepto = "identidad investigadora...",
#'   api_key  = api_key,
#'   construct_map = construct_map,
#'   n_items = 12L
#' )
#' print(g)
#' }
#'
#' @export
generar_escala_guttman <- function(
  concepto,
  api_key,
  construct_map,
  n_items                  = 12L,
  facetas                  = NULL,
  items_por_faceta         = NULL,
  idioma                   = c("es", "en"),
  modelo                   = "gpt-4.1-mini-2025-04-14",
  seed                     = 2026,
  max_palabras_stem        = 18L,
  max_palabras_alternativa = 22L,
  verbose                  = TRUE
) {

  idioma <- match.arg(idioma)

  if (!is.list(construct_map) || length(construct_map) < 3L)
    stop("'construct_map' debe ser una lista nombrada con al menos 3 niveles.")
  if (is.null(names(construct_map)) || any(!nzchar(names(construct_map))))
    stop("Cada elemento de 'construct_map' debe tener nombre (Nivel 0, ...).")

  K <- length(construct_map)
  niveles_nombres   <- names(construct_map)
  niveles_descripts <- unlist(construct_map)

  # Distribucion de items entre facetas
  if (!is.null(facetas)) {
    if (is.null(items_por_faceta)) {
      base_n <- n_items %/% length(facetas)
      resto  <- n_items - base_n * length(facetas)
      items_por_faceta <- rep(base_n, length(facetas)) +
                          c(rep(1L, resto),
                            rep(0L, length(facetas) - resto))
    }
    if (sum(items_por_faceta) != n_items)
      stop("'items_por_faceta' debe sumar 'n_items'.")
    asignacion_facetas <- unlist(mapply(rep, facetas, items_por_faceta,
                                          SIMPLIFY = FALSE))
  } else {
    asignacion_facetas <- rep(NA_character_, n_items)
  }

  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  if (verbose) {
    cat("\n[generar_escala_guttman] Configurando OpenAI...\n")
    cat("  Niveles del construct map: ", K, "\n", sep = "")
    cat("  Items a generar          : ", n_items, "\n", sep = "")
    if (!is.null(facetas)) {
      cat("  Facetas: ",
          paste(paste0(facetas, "(", items_por_faceta, ")"),
                collapse = ", "), "\n", sep = "")
    }
  }
  openai <- .configurar_openai(api_key)

  # ---------- 1. Generar stems ----------
  if (verbose) cat("\n[1/2] Generando ", n_items, " stems...\n", sep = "")
  stems <- .generar_stems_guttman(
    openai = openai, modelo = modelo,
    concepto = concepto, n_items = n_items,
    facetas_asignadas = asignacion_facetas,
    construct_map = construct_map,
    max_palabras = max_palabras_stem,
    idioma = idioma, verbose = verbose
  )

  # ---------- 2. Generar alternativas por item ----------
  if (verbose) cat("\n[2/2] Generando alternativas (", K,
                    " por item, monotonicamente progresivas)...\n", sep = "")
  alts_list <- vector("list", n_items)
  for (i in seq_len(n_items)) {
    alts_list[[i]] <- .generar_alternativas_guttman(
      openai = openai, modelo = modelo,
      stem = stems[i], faceta = asignacion_facetas[i],
      construct_map = construct_map,
      concepto = concepto,
      max_palabras = max_palabras_alternativa,
      idioma = idioma
    )
    if (verbose) {
      cat("  ", sprintf("%2d/%d", i, n_items), " stem: ",
          substr(stems[i], 1, 60),
          if (nchar(stems[i]) > 60) "..." else "", "\n", sep = "")
      for (k in seq_len(K)) {
        cat("       (", letters[k], ") ", niveles_nombres[k], ": ",
            substr(alts_list[[i]][k], 1, 70),
            if (nchar(alts_list[[i]][k]) > 70) "..." else "", "\n", sep = "")
      }
    }
  }

  # Construir data.frames
  items_df <- data.frame(
    n_item = seq_len(n_items),
    faceta = asignacion_facetas,
    stem   = stems,
    stringsAsFactors = FALSE
  )

  alternativas_df <- do.call(rbind, lapply(seq_len(n_items), function(i) {
    data.frame(
      n_item        = i,
      nivel_idx     = seq_len(K) - 1L,    # 0, 1, 2, ..., K-1
      nivel_nombre  = niveles_nombres,
      nivel_descrip = niveles_descripts,
      alternativa   = alts_list[[i]],
      stringsAsFactors = FALSE
    )
  }))

  resultado <- list(
    construct_map = construct_map,
    items         = items_df,
    alternativas  = alternativas_df,
    concepto      = concepto,
    idioma        = idioma,
    facetas       = facetas,
    metadata      = list(
      modelo  = modelo,
      seed    = seed,
      fecha   = format(Sys.Date()),
      n_items = n_items,
      K       = K
    )
  )
  class(resultado) <- c("semilla_guttman", "list")
  resultado
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.generar_stems_guttman <- function(openai, modelo, concepto, n_items,
                                    facetas_asignadas, construct_map,
                                    max_palabras, idioma, verbose) {

  niveles_descritos <- paste0(
    "  - ", names(construct_map), ": ", unlist(construct_map),
    collapse = "\n"
  )

  if (idioma == "es") {
    sys_msg <- paste(
      "Eres un experto en construccion de instrumentos psicometricos con",
      "formato de respuesta GUTTMAN (Wilson et al., 2023). Tu tarea es",
      "generar STEMS (raices de items) cuya respuesta sera elegida entre",
      "alternativas que representan niveles cualitativos del construct map.",
      "",
      "Caracteristicas del stem en formato Guttman:",
      "- Es una pregunta o afirmacion raiz, NEUTRA, que invita al",
      "  respondiente a ubicar su nivel actual del constructo.",
      "- NO sugiere ningun nivel particular del construct map.",
      "- Su contenido tematico debe permitir que las alternativas posteriores",
      "  abarquen TODOS los niveles del construct map.",
      "- En espanol claro, una sola oracion, sin signos de exclamacion."
    )
  } else {
    sys_msg <- paste(
      "You are an expert in Guttman-format psychometric items (Wilson et",
      "al., 2023). Generate STEMS that introduce a topic; the response",
      "options (generated separately) will represent the levels of the",
      "construct map."
    )
  }

  facetas_txt <- if (any(!is.na(facetas_asignadas))) {
    paste0("\nDistribucion por faceta:\n",
           paste0("  ", seq_len(n_items), ". ", facetas_asignadas,
                  collapse = "\n"))
  } else ""

  if (idioma == "es") {
    user_msg <- paste0(
      "Constructo: ", concepto, "\n\n",
      "Construct map (niveles cualitativos del constructo):\n",
      niveles_descritos, "\n",
      facetas_txt, "\n\n",
      "Genera EXACTAMENTE ", n_items, " stems distintos. Cada stem debe ser",
      " una afirmacion o pregunta neutra que pueda ser respondida con",
      " alternativas que representen los niveles del construct map.\n",
      "Limite por stem: ", max_palabras, " palabras.\n",
      "Devuelve los stems en lineas numeradas (1., 2., ...). Sin titulos",
      " ni comillas. Sin etiquetar el nivel."
    )
  } else {
    user_msg <- paste0(
      "Construct: ", concepto, "\n\n",
      "Construct map:\n", niveles_descritos, "\n", facetas_txt, "\n\n",
      "Generate EXACTLY ", n_items, " distinct stems. Word limit: ",
      max_palabras, ". Numbered lines. No quotes."
    )
  }

  raw <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 1200L, temperature = 0.45
  )

  lineas <- unlist(strsplit(raw, "\n", fixed = TRUE))
  lineas <- trimws(lineas)
  lineas <- lineas[nzchar(lineas)]
  lineas <- sub("^[0-9]+[.\\)]\\s*", "", lineas, perl = TRUE)
  lineas <- sub("^[-*]\\s*", "", lineas)
  lineas <- lineas[nzchar(lineas)]

  if (length(lineas) < n_items) {
    warning("Solo se obtuvieron ", length(lineas), "/", n_items,
            " stems. Rellenando con generico.")
    lineas <- c(lineas, rep("Stem por completar", n_items - length(lineas)))
  }
  lineas[seq_len(n_items)]
}


#' @keywords internal
.generar_alternativas_guttman <- function(openai, modelo, stem, faceta,
                                            construct_map, concepto,
                                            max_palabras, idioma) {

  K <- length(construct_map)
  niveles_nombres   <- names(construct_map)
  niveles_descritos <- paste0(
    "  ", niveles_nombres, ": ", unlist(construct_map),
    collapse = "\n"
  )

  if (idioma == "es") {
    sys_msg <- paste(
      "Eres un experto en construccion de items en formato Guttman.",
      "Para un STEM dado y un construct map, generas EXACTAMENTE", K,
      "alternativas de respuesta, una por nivel del construct map,",
      "ordenadas de mas BAJO a mas ALTO.",
      "",
      "Reglas estrictas:",
      "1) Cada alternativa debe ser una afirmacion en primera persona,",
      "   especifica al stem y anclada al nivel correspondiente del map.",
      "2) Las alternativas son MONOTONICAMENTE PROGRESIVAS: la alternativa",
      "   k+1 implica un nivel cualitativamente MAYOR que la k.",
      "3) Mutuamente EXCLUYENTES y EXHAUSTIVAS.",
      "4) NO incluyas etiquetas (Nivel 0, Nivel 1, etc.) en el texto.",
      "5) Vocabulario claro, espanol natural.",
      "6) NO uses gradadores genericos (mucho/poco, siempre/nunca).",
      "   Cada alternativa debe describir una situacion concreta del nivel.",
      "7) Una sola oracion por alternativa, max", max_palabras, "palabras."
    )
  } else {
    sys_msg <- paste(
      "Generate", K, "Guttman-style alternatives (one per construct map",
      "level), monotonically progressive, mutually exclusive and exhaustive."
    )
  }

  faceta_txt <- if (!is.na(faceta)) paste0("Faceta del item: ", faceta, "\n") else ""

  if (idioma == "es") {
    user_msg <- paste0(
      "Constructo: ", concepto, "\n",
      faceta_txt,
      "Stem: \"", stem, "\"\n\n",
      "Construct map (niveles ordenados de mas BAJO a mas ALTO):\n",
      niveles_descritos, "\n\n",
      "Genera EXACTAMENTE ", K, " alternativas, una por nivel, en el",
      " mismo orden que el construct map (de mas baja a mas alta).\n",
      "Devuelve cada alternativa en una linea numerada con la letra:",
      "\n  a) ...\n  b) ...\n  c) ...\n  d) ...\n  e) ...\n",
      "Sin titulos, sin comillas, sin etiquetar el nivel."
    )
  } else {
    user_msg <- paste0(
      "Construct: ", concepto, "\n", faceta_txt,
      "Stem: \"", stem, "\"\n\nConstruct map:\n", niveles_descritos, "\n\n",
      "Generate EXACTLY ", K, " alternatives in order. Lines a) b) c) ..."
    )
  }

  raw <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 800L, temperature = 0.4
  )

  lineas <- unlist(strsplit(raw, "\n", fixed = TRUE))
  lineas <- trimws(lineas)
  lineas <- lineas[nzchar(lineas)]
  # Quitar prefijos a) b) o 1) 2) o variantes
  lineas <- sub("^[a-zA-Z][.\\)]\\s*", "", lineas, perl = TRUE)
  lineas <- sub("^\\([a-zA-Z]\\)\\s*", "", lineas, perl = TRUE)
  lineas <- sub("^[0-9]+[.\\)]\\s*", "", lineas, perl = TRUE)
  lineas <- sub("^[-*]\\s*", "", lineas)
  lineas <- lineas[nzchar(lineas)]

  if (length(lineas) < K) {
    warning("Solo se obtuvieron ", length(lineas), "/", K,
            " alternativas para stem '", substr(stem, 1, 30),
            "...'. Rellenando con generico.")
    lineas <- c(lineas, rep("Alternativa por completar", K - length(lineas)))
  }
  lineas[seq_len(K)]
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_guttman <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Escala formato GUTTMAN (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Idioma         : ", x$idioma, "\n", sep = "")
  cat("  Niveles (K)    : ", x$metadata$K, "\n", sep = "")
  cat("  N items        : ", nrow(x$items), "\n", sep = "")
  if (!is.null(x$facetas)) {
    cat("  Facetas        : ", paste(x$facetas, collapse = ", "), "\n",
        sep = "")
  }
  cat("-----------------------------------------------------------\n")
  cat("  Construct map:\n")
  for (lv in names(x$construct_map)) {
    cat("    ", lv, ": ",
        substr(x$construct_map[[lv]], 1, 70),
        if (nchar(x$construct_map[[lv]]) > 70) "..." else "", "\n", sep = "")
  }
  cat("\n  Primer item (ejemplo):\n")
  cat("    Stem: ", x$items$stem[1], "\n", sep = "")
  alts <- x$alternativas[x$alternativas$n_item == 1, ]
  for (j in seq_len(nrow(alts))) {
    cat("    (", letters[j], ") ", alts$alternativa[j], "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}
