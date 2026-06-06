# =============================================================================
# DIF SEMANTICO: deteccion temprana de funcionamiento diferencial via embeddings
# Inspirado en Belzak (2023), regDIF R package
# =============================================================================

#' @title Deteccion Temprana de DIF Semantico
#'
#' @description
#' Detecta posible \emph{Differential Item Functioning} (DIF) antes de
#' recolectar datos, comparando los embeddings de cada item entre versiones
#' (idiomas, traducciones, formulaciones alternativas) o entre subgrupos.
#'
#' Identifica items cuyo desplazamiento semantico entre versiones es
#' anomalo respecto al desplazamiento promedio del constructo. Sirve como
#' tamizaje complementario al analisis confirmatorio de DIF basado en
#' respuestas (regDIF, IRT-LR, MNLFA).
#'
#' @param x Objeto semilla con embeddings calculados (version base).
#' @param y Objeto semilla con embeddings calculados (version comparada),
#'   o lista \code{list(items = ..., embeddings = ...)} con la misma
#'   cantidad y orden de items que \code{x}.
#' @param emparejamiento Como emparejar items: "orden" (default, asume
#'   misma posicion = mismo item) o "codigo" (empareja por columna codigo).
#' @param umbral_z Z-score absoluto para marcar DIF severo (default: 2.0;
#'   equivale a los items mas tipicos al 95 por ciento).
#' @param verbose Mostrar progreso.
#'
#' @return Lista de clase \code{semilla_dif} con:
#' \itemize{
#'   \item \code{distancias}: distancia coseno por item entre versiones.
#'   \item \code{drift_global}: desplazamiento medio del constructo (esperado).
#'   \item \code{z_drift}: distancia centrada y estandarizada por item.
#'   \item \code{items_dif}: items con z_drift > umbral_z.
#'   \item \code{recomendaciones}: items que requieren panel de expertos
#'     o que deben re-traducirse antes de aplicar.
#' }
#'
#' @details
#' SeMiLLa parte de la idea de Belzak (2023) de evaluar simultaneamente
#' multiples fuentes de sesgo, pero la traslada al espacio semantico:
#' un item con propiedades estables a traves de versiones tendra una
#' distancia coseno cercana a la mediana del constructo. Cuando un item
#' se aleja de esa mediana mas de lo esperado, hay riesgo de DIF y debe
#' priorizarse en el analisis empirico posterior con \code{regDIF}.
#'
#' @examples
#' \dontrun{
#' escala_es <- semilla("autoeficacia academica", idioma = "es")
#' escala_en <- semilla("academic self-efficacy", idioma = "en")
#' dif <- detectar_dif_semantico(escala_es, escala_en)
#' dif$items_dif
#' }
#'
#' @references
#' Belzak, W. C. M. (2023). The regDIF R package: Evaluating complex
#' sources of measurement bias using regularized differential item
#' functioning. \emph{Structural Equation Modeling}, 30(6), 935-948.
#' \doi{10.1080/10705511.2023.2170235}
#'
#' @export
detectar_dif_semantico <- function(x, y,
                                   emparejamiento = c("orden", "codigo"),
                                   umbral_z = 2.0,
                                   verbose = TRUE) {

  if (!inherits(x, "semilla")) stop("x debe ser un objeto 'semilla'.")
  if (is.null(x$embeddings)) stop("x sin embeddings; ejecuta obtener_embeddings(x).")

  if (inherits(y, "semilla")) {
    items_y <- y$items
    emb_y <- y$embeddings
  } else if (is.list(y) && !is.null(y$items) && !is.null(y$embeddings)) {
    items_y <- y$items
    emb_y <- y$embeddings
  } else {
    stop("y debe ser objeto 'semilla' o lista con $items y $embeddings.")
  }
  if (is.null(emb_y)) stop("y no tiene embeddings.")

  emparejamiento <- match.arg(emparejamiento)
  items_x <- x$items
  emb_x <- x$embeddings

  # Emparejamiento
  if (emparejamiento == "codigo") {
    if (!"codigo" %in% names(items_x) || !"codigo" %in% names(items_y)) {
      stop("Ambos objetos deben tener columna 'codigo' para emparejar.")
    }
    comunes <- intersect(items_x$codigo, items_y$codigo)
    if (length(comunes) == 0) stop("No hay codigos comunes entre versiones.")
    idx_x <- match(comunes, items_x$codigo)
    idx_y <- match(comunes, items_y$codigo)
    emb_x <- emb_x[idx_x, , drop = FALSE]
    emb_y <- emb_y[idx_y, , drop = FALSE]
    items_x <- items_x[idx_x, , drop = FALSE]
    items_y <- items_y[idx_y, , drop = FALSE]
  } else {
    if (nrow(emb_x) != nrow(emb_y)) {
      stop("Para emparejamiento='orden' las dos versiones deben tener el mismo numero de items.")
    }
  }

  # Distancia coseno por item
  norm_x <- sqrt(rowSums(emb_x^2))
  norm_y <- sqrt(rowSums(emb_y^2))
  cos_sim <- rowSums(emb_x * emb_y) / (norm_x * norm_y)
  cos_dist <- 1 - cos_sim

  # Drift global (desplazamiento esperado por el cambio de version completa)
  drift_global <- median(cos_dist)
  mad_drift <- mad(cos_dist)
  if (mad_drift == 0) mad_drift <- sd(cos_dist)
  z_drift <- (cos_dist - drift_global) / mad_drift

  codigos <- if ("codigo" %in% names(items_x)) items_x$codigo else paste0("Item_", seq_len(nrow(emb_x)))

  tabla <- data.frame(
    Codigo = codigos,
    Item_X = items_x$item,
    Item_Y = items_y$item,
    Distancia_Coseno = round(cos_dist, 4),
    Z_Drift = round(z_drift, 3),
    Riesgo_DIF = ifelse(abs(z_drift) > umbral_z, "alto",
                ifelse(abs(z_drift) > umbral_z * 0.6, "moderado", "bajo")),
    stringsAsFactors = FALSE
  )

  items_dif <- tabla[tabla$Riesgo_DIF == "alto", , drop = FALSE]
  recomendaciones <- if (nrow(items_dif) > 0) {
    paste0("Revisar/re-traducir y testear con regDIF: ",
           paste(items_dif$Codigo, collapse = ", "))
  } else {
    "Ningun item supera el umbral de DIF semantico."
  }

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("DIF SEMANTICO (tamizaje pre-empirico)"), "\n")
    cat(.linea("-"), "\n\n", sep = "")
    cat("  Items comparados:    ", nrow(tabla), "\n", sep = "")
    cat("  Drift global (mediana de distancia): ", sprintf("%.3f", drift_global), "\n", sep = "")
    cat("  Items con riesgo alto: ", nrow(items_dif), "\n", sep = "")
    cat("  Items con riesgo moderado: ",
        sum(tabla$Riesgo_DIF == "moderado"), "\n\n", sep = "")
    if (nrow(items_dif) > 0) {
      cat("  ", .color_warning(), " Items con DIF semantico alto:\n", sep = "")
      for (i in seq_len(min(8, nrow(items_dif)))) {
        cat("    - ", items_dif$Codigo[i], " (z = ",
            sprintf("%.2f", items_dif$Z_Drift[i]), ")\n", sep = "")
      }
      if (nrow(items_dif) > 8) cat("    ... y ", nrow(items_dif) - 8, " mas\n", sep = "")
    } else {
      cat("  ", .color_check(), " Sin items con DIF semantico alto.\n\n", sep = "")
    }
  }

  resultado <- list(
    distancias = tabla,
    drift_global = drift_global,
    mad_drift = mad_drift,
    z_drift = setNames(z_drift, codigos),
    items_dif = items_dif,
    umbral_z = umbral_z,
    recomendaciones = recomendaciones
  )
  class(resultado) <- c("semilla_dif", "list")
  resultado
}

#' @export
print.semilla_dif <- function(x, ...) {
  cat("DIF Semantico SeMiLLa\n")
  cat("  Items: ", nrow(x$distancias), "\n", sep = "")
  cat("  Drift global: ", sprintf("%.3f", x$drift_global), "\n", sep = "")
  cat("  Items con DIF alto: ", nrow(x$items_dif), "\n", sep = "")
  invisible(x)
}
