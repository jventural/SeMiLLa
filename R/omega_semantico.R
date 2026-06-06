# =============================================================================
# omega_semantico() — Fiabilidad de McDonald derivada de embeddings
# =============================================================================
# Calcula omega de McDonald (1999) en el dominio semantico. A diferencia de
# fiabilidad_semantica() que aplica Spearman-Brown sobre la similitud media
# (asume tau-equivalencia: cargas iguales), omega_semantico() estima las
# cargas factoriales reales sobre la matriz de similitud y aplica la formula
# de McDonald:
#
#     omega = (sum lambda)^2 / [(sum lambda)^2 + sum (1 - lambda^2)]
#
# donde lambda_i es la carga estandarizada del item i en su factor (estimada
# por EFA unifactorial sobre la submatriz de similitud de esa dimension).
#
# REFERENCIAS:
#   McDonald, R. P. (1999). Test theory: A unified treatment. Erlbaum.
#   Wulff, D. U., & Mata, R. (2025). Nature Human Behaviour, 9(5), 944-954.
# =============================================================================

#' Omega semantico (McDonald) por dimension
#'
#' Calcula omega de McDonald usando cargas factoriales derivadas de la matriz
#' de similitud semantica. Cuando el paper empirico reporta omega en vez de
#' alpha, esta funcion da una comparacion homogenea.
#'
#' @param x Objeto \code{semilla} o lista con \code{similitud} e \code{items}
#' @param verbose Mostrar progreso
#'
#' @return data.frame con columnas:
#'   - dimension: nombre de la dimension
#'   - n_items: numero de items
#'   - omega_semantico: omega de McDonald
#'   - alpha_semantico: alpha de Spearman-Brown (para comparar)
#'   - lambda_promedio: media de las cargas estimadas
#'   - heterogeneidad: coeficiente de variacion de las cargas
#'
#' @details
#' Para k>=3 items se usa \code{psych::fa(nfactors=1, fm="minres")} sobre la
#' submatriz de similitud. Para k=2 se usa \code{sqrt(|r|)} como aproximacion.
#' Las cargas se truncan a \code{[0,1]} antes de la formula de McDonald.
#'
#' @examples
#' \dontrun{
#' escala <- semilla(fuente = "usuario", archivo = "items.xlsx",
#'                   api_key = Sys.getenv("OPENAI_API_KEY"))
#' omega_semantico(escala)
#' }
#'
#' @export
omega_semantico <- function(x, verbose = TRUE) {

  # ---- Extraer matriz de similitud + items ----
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$similitud)) stop("Objeto sin matriz de similitud")
    sim      <- x$similitud
    items_df <- x$items
  } else if (is.list(x) && !is.null(x$similitud)) {
    sim      <- x$similitud
    items_df <- x$items
  } else {
    stop("Objeto no valido. Usa semilla() o lista con $similitud y $items.")
  }
  if (is.null(items_df) || !"dimension" %in% names(items_df)) {
    stop("Se requiere $items con columna 'dimension'.")
  }

  rownames(sim) <- colnames(sim) <- as.character(seq_len(nrow(sim)))
  dims <- unique(items_df$dimension)

  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("OMEGA SEMANTICO (McDonald) POR DIMENSION\n")
    cat(strrep("=", 60), "\n\n", sep = "")
    cat("  Items:       ", nrow(items_df), "\n", sep = "")
    cat("  Dimensiones: ", length(dims), "\n\n", sep = "")
  }

  filas <- list()
  for (d in dims) {
    idx <- which(items_df$dimension == d)
    k   <- length(idx)
    if (k < 2) {
      filas[[length(filas) + 1]] <- data.frame(
        dimension = d, n_items = k,
        omega_semantico = NA_real_, alpha_semantico = NA_real_,
        lambda_promedio = NA_real_, heterogeneidad = NA_real_,
        stringsAsFactors = FALSE)
      next
    }

    sim_d <- sim[idx, idx, drop = FALSE]

    # ---- Estimar cargas factoriales ----
    lambdas <- if (k >= 3 && requireNamespace("psych", quietly = TRUE)) {
      fa_d <- tryCatch(
        suppressWarnings(suppressMessages(
          psych::fa(r = sim_d, nfactors = 1, fm = "minres",
                    rotate = "none", warnings = FALSE)
        )),
        error = function(e) NULL
      )
      if (!is.null(fa_d)) {
        as.numeric(fa_d$loadings[, 1])
      } else {
        # Fallback: comunalidad promedio como proxy
        sim_off <- sim_d; diag(sim_off) <- NA
        sqrt(abs(rowMeans(sim_off, na.rm = TRUE)))
      }
    } else {
      # k == 2
      r <- sim_d[1, 2]
      rep(sqrt(abs(r)), 2)
    }

    # Truncar a [0, 1]
    lambdas <- pmin(pmax(lambdas, 0), 1)

    # ---- Formula de McDonald ----
    suma_l_sq    <- sum(lambdas)^2
    uniqueness   <- 1 - lambdas^2
    suma_uniq    <- sum(uniqueness)
    omega_d      <- suma_l_sq / (suma_l_sq + suma_uniq)

    # Alpha (Spearman-Brown) para comparar
    sim_off <- sim_d; diag(sim_off) <- NA
    r_intra <- mean(sim_off, na.rm = TRUE)
    alpha_d <- (k * r_intra) / (1 + (k - 1) * r_intra)

    het <- if (mean(lambdas) > 0) sd(lambdas) / mean(lambdas) else NA_real_

    if (verbose) {
      cat(sprintf("  [%s]\n", d))
      cat(sprintf("     n=%d  lambda media=%.3f (SD=%.3f, CV=%.2f)\n",
                  k, mean(lambdas), sd(lambdas), het))
      cat(sprintf("     omega = %.3f   alpha = %.3f   delta = %+.3f\n\n",
                  omega_d, alpha_d, omega_d - alpha_d))
    }

    filas[[length(filas) + 1]] <- data.frame(
      dimension       = d,
      n_items         = k,
      omega_semantico = omega_d,
      alpha_semantico = alpha_d,
      lambda_promedio = mean(lambdas),
      heterogeneidad  = het,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, filas)

  if (verbose) {
    cat(strrep("-", 60), "\n", sep = "")
    cat(sprintf("  omega promedio: %.3f   |   alpha promedio: %.3f\n",
                mean(res$omega_semantico, na.rm = TRUE),
                mean(res$alpha_semantico, na.rm = TRUE)))
    cat("  Nota: omega no asume tau-equivalencia (cargas iguales).\n")
    cat("        Cuando hay heterogeneidad de cargas, omega es preferible.\n")
    cat(strrep("=", 60), "\n\n", sep = "")
  }

  # ---- Alerta de homogeneidad sintactica ----
  # Si los items comparten plantilla (patron tipo Escala de Celos), el omega
  # semantico puede no ser interpretable como consistencia: se advierte.
  if (!is.null(items_df$item)) {
    hs <- .homogeneidad_sintactica(items_df$item)
    if (isTRUE(hs$alerta)) {
      msg <- sprintf(paste0("Homogeneidad sintactica alta (prefijo compartido %.0f%%, ",
                            "solapamiento n-grama %.2f): el omega semantico puede no ser ",
                            "interpretable como consistencia. Revise auditar_redundancia()."),
                     100 * hs$prefijo_frac, hs$ngram_media)
      if (verbose) cat("  ", .color_warning(), " ", msg, "\n\n", sep = "")
      warning(msg, call. = FALSE)
      attr(res, "alerta_homogeneidad") <- hs
    }
  }

  res
}
