# =============================================================================
# BANCO DE ITEMS PARA CAT (Computerized Adaptive Testing)
# Inspirado en Gao, Ma, Qi & Liu (2026), AIG basado en LLMs para Big Five CAT
# =============================================================================

#' @title Generar Banco de Items para CAT
#'
#' @description
#' Construye un banco amplio de items por dimension usando generacion
#' automatica con LLM, los pre-filtra por similitud semantica
#' (eliminacion de redundancia) y por discriminacion predicha desde
#' embeddings, y exporta el banco en formato compatible con paquetes de
#' Computerized Adaptive Testing (\code{mirt}, \code{catR}).
#'
#' Replica el pipeline de Gao, Ma, Qi y Liu (2026) para AIG en bancos
#' CAT: genera N candidatos por dimension, calcula propiedades semanticas,
#' filtra los mejores B por dimension.
#'
#' @param x Objeto semilla con dimensiones definidas (con o sin items).
#' @param n_por_dimension Numero de items \emph{candidatos} a generar por
#'   dimension (default: 30).
#' @param n_finales Numero de items \emph{finales} por dimension tras
#'   filtrado (default: 15).
#' @param api_key API key de OpenAI.
#' @param modelo Modelo de OpenAI (default: \code{"gpt-4.1-mini"}).
#' @param umbral_redundancia Similitud coseno por encima de la cual se
#'   considera item redundante (default: 0.85).
#' @param formato_export Formato de exportacion: "mirt" (default) o "catR".
#' @param verbose Mostrar progreso.
#'
#' @return Lista de clase \code{semilla_banco_cat} con:
#' \itemize{
#'   \item \code{banco_completo}: data.frame con todos los items
#'     candidatos, dimension, embedding, discriminacion predicha,
#'     dificultad predicha y bandera de inclusion.
#'   \item \code{banco_final}: subset filtrado.
#'   \item \code{export_path}: ruta del archivo exportado para CAT.
#'   \item \code{estadisticos}: resumen por dimension.
#' }
#'
#' @details
#' Discriminacion predicha = unicidad semantica (1 - max similitud con
#' el centroide de las otras dimensiones).
#' Dificultad predicha = z-score de la distancia al centroide propio
#' (items mas alejados son mas dificiles de endosar).
#' Estos parametros son \emph{seeds} para calibracion empirica posterior
#' con \code{mirt} o \code{catR}; no sustituyen IRT real.
#'
#' @examples
#' \dontrun{
#' base <- semilla("autoeficacia academica", n_items = 5)
#' banco <- banco_cat(base,
#'                    n_por_dimension = 40,
#'                    n_finales = 20,
#'                    api_key = Sys.getenv("OPENAI_API_KEY"))
#' banco$banco_final
#' }
#'
#' @references
#' Gao, Y., Ma, Y., Qi, Y., & Liu, T. (2026). Development of a
#' computerized adaptive item bank for the Big Five personality based on
#' large language models. \emph{Assessment}.
#' \doi{10.1177/10731911261427877}
#'
#' @export
banco_cat <- function(x,
                      n_por_dimension = 30,
                      n_finales = 15,
                      api_key,
                      modelo = "gpt-4.1-mini",
                      umbral_redundancia = 0.85,
                      formato_export = c("mirt", "catR"),
                      verbose = TRUE) {

  if (!inherits(x, "semilla")) stop("x debe ser un objeto 'semilla'.")
  .validar_api_key(api_key)
  formato_export <- match.arg(formato_export)

  concepto <- x$concepto
  dimensiones <- if (!is.null(concepto$dimensiones)) {
    sapply(concepto$dimensiones, function(d) d$nombre)
  } else {
    unique(x$items$dimension)
  }
  if (length(dimensiones) == 0) stop("No se detectaron dimensiones en x.")

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("BANCO DE ITEMS PARA CAT"), "\n")
    cat(.linea("-"), "\n\n", sep = "")
    cat("  Dimensiones:        ", length(dimensiones), "\n", sep = "")
    cat("  Items candidatos x dim: ", n_por_dimension, "\n", sep = "")
    cat("  Items finales x dim:    ", n_finales, "\n\n", sep = "")
  }

  # 1) Generar candidatos por dimension (re-usa generar_escala con n_items grande)
  if (verbose) cat("  [1/3] Generando candidatos via LLM...\n")
  base_grande <- generar_escala(
    concepto = x$metadata$concepto_original,
    n_items = n_por_dimension,
    idioma = x$metadata$idioma %||% "es",
    poblacion = x$metadata$poblacion,
    api_key = api_key,
    modelo = modelo,
    verbose = FALSE
  )
  base_grande <- obtener_embeddings(base_grande, api_key = api_key, verbose = FALSE)

  items_df <- base_grande$items
  emb <- base_grande$embeddings

  # 2) Calcular discriminacion y dificultad predichas
  if (verbose) cat("  [2/3] Calculando propiedades psicometricas predichas...\n")
  dims <- unique(items_df$dimension)
  centroides <- t(vapply(dims, function(d) {
    colMeans(emb[items_df$dimension == d, , drop = FALSE])
  }, numeric(ncol(emb))))
  rownames(centroides) <- dims

  cos_norm <- function(M) M / sqrt(rowSums(M^2))
  emb_n <- cos_norm(emb)
  cen_n <- cos_norm(centroides)
  sim_centroides <- emb_n %*% t(cen_n)  # items x dims

  discriminacion <- numeric(nrow(items_df))
  dificultad     <- numeric(nrow(items_df))
  for (i in seq_len(nrow(items_df))) {
    propio <- which(dims == items_df$dimension[i])
    sim_propio <- sim_centroides[i, propio]
    sim_otros  <- max(sim_centroides[i, -propio])
    discriminacion[i] <- sim_propio - sim_otros
    dificultad[i] <- 1 - sim_propio
  }
  # Z-score de dificultad por dimension
  dificultad_z <- ave(dificultad, items_df$dimension, FUN = function(v) {
    if (sd(v) == 0) rep(0, length(v)) else as.numeric(scale(v))
  })

  # 3) Filtrar redundancia y seleccionar top n_finales por dimension
  if (verbose) cat("  [3/3] Filtrando por redundancia y discriminacion...\n")
  items_df$discriminacion_pred <- round(discriminacion, 3)
  items_df$dificultad_pred     <- round(dificultad_z, 3)
  items_df$incluido <- FALSE

  for (d in dims) {
    idx <- which(items_df$dimension == d)
    sub_emb <- emb_n[idx, , drop = FALSE]
    sim_intra <- sub_emb %*% t(sub_emb)
    diag(sim_intra) <- 0

    orden <- idx[order(-discriminacion[idx])]
    elegidos <- integer(0)
    for (k in orden) {
      if (length(elegidos) >= n_finales) break
      if (length(elegidos) == 0) {
        elegidos <- c(elegidos, k)
      } else {
        max_sim <- max(emb_n[k, , drop = FALSE] %*% t(emb_n[elegidos, , drop = FALSE]))
        if (max_sim < umbral_redundancia) elegidos <- c(elegidos, k)
      }
    }
    items_df$incluido[elegidos] <- TRUE
  }

  banco_final <- items_df[items_df$incluido, , drop = FALSE]

  # Tabla de export para mirt/catR
  export_df <- data.frame(
    item_id = paste0(banco_final$dimension, "_", seq_len(nrow(banco_final))),
    item    = banco_final$item,
    dim     = banco_final$dimension,
    a       = pmax(banco_final$discriminacion_pred * 5, 0.3),
    b       = banco_final$dificultad_pred,
    c       = 0,
    stringsAsFactors = FALSE
  )

  estadisticos <- do.call(rbind, lapply(dims, function(d) {
    sub <- banco_final[banco_final$dimension == d, ]
    data.frame(
      Dimension = d,
      N_Final = nrow(sub),
      Discriminacion_media = round(mean(sub$discriminacion_pred), 3),
      Dificultad_rango = paste0(sprintf("%.2f", min(sub$dificultad_pred)),
                                " ; ",
                                sprintf("%.2f", max(sub$dificultad_pred))),
      stringsAsFactors = FALSE
    )
  }))

  if (verbose) {
    cat("\n  Resumen del banco final:\n")
    print(estadisticos, row.names = FALSE)
    cat("\n  Formato de exportacion: ", formato_export, "\n", sep = "")
    cat("  ", .color_check(),
        " Use mirt::mirt() o catR::randomCAT() con la columna 'a','b','c'.\n\n", sep = "")
  }

  resultado <- list(
    banco_completo = items_df,
    banco_final = banco_final,
    export_df = export_df,
    estadisticos = estadisticos,
    formato_export = formato_export,
    n_por_dimension = n_por_dimension,
    n_finales = n_finales,
    embeddings = emb
  )
  class(resultado) <- c("semilla_banco_cat", "list")
  resultado
}

#' @export
print.semilla_banco_cat <- function(x, ...) {
  cat("Banco de Items para CAT (SeMiLLa)\n")
  cat("  Total candidatos: ", nrow(x$banco_completo), "\n", sep = "")
  cat("  Total finales:    ", nrow(x$banco_final), "\n", sep = "")
  cat("  Dimensiones:      ", nrow(x$estadisticos), "\n", sep = "")
  cat("  Formato:          ", x$formato_export, "\n", sep = "")
  invisible(x)
}
