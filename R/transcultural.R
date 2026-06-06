# =============================================================================
# ADAPTACION TRANSCULTURAL ASISTIDA POR LLM
# Inspirado en Grobelny, Szymanski & Strozyk (2025)
# =============================================================================

#' @title Adaptacion Transcultural de una Escala con LLM
#'
#' @description
#' Genera una version traducida y culturalmente adaptada de una escala
#' usando un LLM, siguiendo lineamientos de adaptacion transcultural
#' (ITC, Beaton et al., 2000) y la propuesta de Grobelny, Szymanski y
#' Strozyk (2025) para usar prompts estructurados con guias psicometricas.
#'
#' Devuelve la escala adaptada con embeddings nuevos en el idioma objetivo
#' y un reporte de equivalencia semantica (distancia coseno y z_drift)
#' utilizable directamente con \code{detectar_dif_semantico()}.
#'
#' @param x Objeto semilla original (con o sin embeddings).
#' @param idioma_destino Codigo de idioma destino: "es", "en", "pt", "fr",
#'   "de", "it", "pl", etc.
#' @param api_key API key de OpenAI.
#' @param cultura Descripcion breve de la cultura/poblacion destino
#'   (ej. "adultos jovenes peruanos", "estudiantes universitarios alemanes").
#' @param modelo Modelo de OpenAI (default: \code{"gpt-4.1-mini"}).
#' @param verificar_equivalencia Calcular distancia semantica entre
#'   versiones (default TRUE; requiere embeddings en \code{x}).
#' @param verbose Mostrar progreso.
#'
#' @return Lista de clase \code{semilla_transcultural} con:
#' \itemize{
#'   \item \code{escala_origen}: objeto semilla original.
#'   \item \code{escala_destino}: objeto semilla adaptado.
#'   \item \code{equivalencia}: tabla con distancia coseno por item.
#'   \item \code{items_problematicos}: items con baja equivalencia semantica.
#'   \item \code{idioma_destino}, \code{cultura}.
#' }
#'
#' @details
#' El prompt instruye al LLM a (1) traducir manteniendo la dimension
#' teorica, (2) sustituir referentes culturales (alimentos, instituciones,
#' modismos) por equivalentes locales, (3) mantener registro de lectura
#' apropiado para la poblacion. SeMiLLa NO sustituye al panel de expertos
#' bilingues exigido por las International Test Commission Guidelines;
#' produce un \emph{borrador rapido} que un panel humano debe auditar
#' antes del back-translation.
#'
#' @examples
#' \dontrun{
#' escala_es <- semilla("autoeficacia academica", idioma = "es")
#' escala_pl <- adaptar_transcultural(escala_es,
#'                                    idioma_destino = "pl",
#'                                    cultura = "estudiantes universitarios polacos",
#'                                    api_key = Sys.getenv("OPENAI_API_KEY"))
#' escala_pl$items_problematicos
#' }
#'
#' @references
#' Grobelny, J., Szymanski, K., & Strozyk, Z. (2025). Act as an expert in
#' psychometry. The evaluation of large language models utility in
#' psychological tests cross-cultural adaptations. \emph{Acta Psychologica}.
#'
#' Beaton, D. E., Bombardier, C., Guillemin, F., & Ferraz, M. B. (2000).
#' Guidelines for the process of cross-cultural adaptation of self-report
#' measures. \emph{Spine}, 25(24), 3186-3191.
#'
#' @export
adaptar_transcultural <- function(x,
                                  idioma_destino,
                                  api_key,
                                  cultura = NULL,
                                  modelo = "gpt-4.1-mini",
                                  verificar_equivalencia = TRUE,
                                  verbose = TRUE) {

  if (!inherits(x, "semilla")) stop("x debe ser un objeto 'semilla'.")
  .validar_api_key(api_key)
  openai <- .configurar_openai(api_key)

  items_df <- x$items
  concepto <- x$concepto
  idioma_origen <- x$metadata$idioma %||% "es"

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("ADAPTACION TRANSCULTURAL ASISTIDA POR LLM"), "\n")
    cat(.linea("-"), "\n\n", sep = "")
    cat("  Origen:   ", .nombre_idioma(idioma_origen), "\n", sep = "")
    cat("  Destino:  ", .nombre_idioma(idioma_destino), "\n", sep = "")
    if (!is.null(cultura)) cat("  Cultura:  ", cultura, "\n", sep = "")
    cat("  Items:    ", nrow(items_df), "\n\n", sep = "")
  }

  # Prompt estructurado siguiendo la pauta Grobelny et al. (2025)
  cultura_txt <- if (!is.null(cultura)) paste0("Poblacion destino: ", cultura, ". ") else ""
  items_json <- jsonlite::toJSON(
    data.frame(numero = items_df$numero,
               dimension = items_df$dimension,
               item = items_df$item,
               stringsAsFactors = FALSE),
    auto_unbox = TRUE
  )

  prompt <- paste0(
    "Act as an expert in psychometry specialized in cross-cultural ",
    "adaptation of psychological tests (ITC Guidelines, Beaton et al., 2000). ",
    "Your task is to adapt the following items from ", idioma_origen,
    " to ", idioma_destino, ". ", cultura_txt,
    "Rules: (1) preserve the theoretical dimension, (2) replace culturally ",
    "specific referents (food, institutions, idioms, holidays) with local ",
    "equivalents, (3) keep reading level appropriate for the target population, ",
    "(4) keep first-person voice if the original is first-person. ",
    "Return STRICTLY a JSON array with objects {numero, dimension, item}. ",
    "Items: ", items_json
  )

  messages <- list(
    list(role = "system",
         content = "You are a psychometric expert in cross-cultural test adaptation. Respond ONLY with valid JSON."),
    list(role = "user", content = prompt)
  )

  resp <- .llamar_openai(openai, messages, modelo, max_tokens = 4000L, temperature = 0)
  resp <- gsub("```json|```", "", resp)
  resp <- trimws(resp)

  # Parseo robusto
  parsed <- tryCatch(jsonlite::fromJSON(resp, simplifyDataFrame = TRUE),
                     error = function(e) NULL)
  if (is.list(parsed) && !is.data.frame(parsed)) {
    candidato <- parsed[vapply(parsed, is.data.frame, logical(1))]
    if (length(candidato) > 0) parsed <- candidato[[1]]
  }
  if (is.null(parsed) || !is.data.frame(parsed) || !"item" %in% names(parsed)) {
    stop("La respuesta del LLM no se pudo parsear como tabla de items.")
  }

  items_dest <- items_df
  items_dest$item <- parsed$item[match(items_df$numero, parsed$numero)]

  # Reconstruir objeto semilla destino
  escala_destino <- x
  escala_destino$items <- items_dest
  escala_destino$metadata$idioma <- idioma_destino
  escala_destino$metadata$adaptado_de <- idioma_origen
  escala_destino$metadata$cultura_destino <- cultura
  escala_destino$embeddings <- NULL
  escala_destino$similitud  <- NULL

  equivalencia <- NULL
  items_problematicos <- NULL

  if (verificar_equivalencia && !is.null(x$embeddings)) {
    if (verbose) cat("  Calculando embeddings de la version adaptada...\n")
    escala_destino <- obtener_embeddings(escala_destino,
                                         api_key = api_key,
                                         verbose = FALSE)
    dif_res <- detectar_dif_semantico(x, escala_destino,
                                      emparejamiento = "orden",
                                      umbral_z = 2.0,
                                      verbose = FALSE)
    equivalencia <- dif_res$distancias
    items_problematicos <- dif_res$items_dif

    if (verbose) {
      cat("  Equivalencia semantica:\n")
      cat("    Drift mediano: ", sprintf("%.3f", dif_res$drift_global), "\n", sep = "")
      cat("    Items con baja equivalencia: ", nrow(items_problematicos), "\n\n", sep = "")
    }
  }

  resultado <- list(
    escala_origen = x,
    escala_destino = escala_destino,
    equivalencia = equivalencia,
    items_problematicos = items_problematicos,
    idioma_destino = idioma_destino,
    cultura = cultura,
    modelo = modelo
  )
  class(resultado) <- c("semilla_transcultural", "list")
  resultado
}

#' @export
print.semilla_transcultural <- function(x, ...) {
  cat("Adaptacion Transcultural SeMiLLa\n")
  cat("  Origen -> Destino: ", x$escala_origen$metadata$idioma,
      " -> ", x$idioma_destino, "\n", sep = "")
  if (!is.null(x$cultura)) cat("  Cultura: ", x$cultura, "\n", sep = "")
  cat("  Items adaptados: ", nrow(x$escala_destino$items), "\n", sep = "")
  if (!is.null(x$items_problematicos)) {
    cat("  Items con baja equivalencia: ", nrow(x$items_problematicos), "\n", sep = "")
  }
  invisible(x)
}
