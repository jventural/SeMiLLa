# =============================================================================
# EFA REGULARIZADO (alternativa a la rotacion clasica)
# Basado en Goretzko (2023), European Journal of Psychological Assessment
# =============================================================================

#' @title Analisis Factorial Exploratorio Regularizado sobre Embeddings
#'
#' @description
#' Ajusta una solucion factorial sobre la matriz de similitud de embeddings
#' aplicando regularizacion L1 (lasso) o elastic-net sobre las cargas en
#' lugar de rotarlas. Reduce la indeterminacion rotacional y promueve
#' soluciones interpretables con cargas cruzadas pequenas encogidas a cero.
#'
#' Sigue la propuesta de Goretzko (2023) de \emph{regularized EFA} como
#' alternativa al pipeline clasico extraer-y-rotar. SeMiLLa la aplica sobre
#' la matriz de cargas inicial obtenida por descomposicion espectral de la
#' matriz de similitud coseno entre items.
#'
#' @param x Objeto semilla con embeddings calculados.
#' @param n_factores Numero de factores a extraer. Si \code{NULL}, usa el
#'   numero de dimensiones teoricas.
#' @param penalizacion Tipo de penalizacion: "elasticnet" (default,
#'   alpha=0.5), "lasso" (L1) o "ridge" (L2).
#' @param lambda Parametro de penalizacion. Si \code{NULL} se selecciona
#'   por validacion cruzada \emph{leave-one-item-out}.
#' @param alpha Mezcla L1/L2 para elastic-net (1 = lasso puro, 0 = ridge).
#' @param centrado Como pretratar la matriz de similitud. "double" (default,
#'   double-centering estilo MDS para remover el factor general que captura
#'   el vocabulario compartido entre items del mismo dominio); "ninguno"
#'   (descomposicion directa, util si la escala mide constructos
#'   independientes); "filas" (solo medias por fila).
#' @param umbral_carga Cargas absolutas por debajo de este umbral se
#'   reportan como cero (default: 0.10).
#' @param verbose Mostrar progreso.
#'
#' @return Lista de clase \code{semilla_efa_reg} con:
#' \itemize{
#'   \item \code{cargas}: matriz de cargas regularizada (items x factores).
#'   \item \code{cargas_cruzadas}: numero de cargas cruzadas != 0 por item.
#'   \item \code{varianza_explicada}: por factor y total.
#'   \item \code{lambda_elegido}: penalizacion final.
#'   \item \code{indeterminacion}: indice de sparsity (proporcion de cargas
#'     encogidas a cero por debajo del umbral).
#'   \item \code{asignacion}: factor dominante por item.
#' }
#'
#' @details
#' La rotacion clasica (varimax, promax, oblimin, etc.) introduce
#' arbitrariedad porque varias soluciones rotadas son matematicamente
#' equivalentes. La EFA regularizada sustituye la rotacion por una
#' penalizacion que favorece soluciones esparsas (Goretzko, 2023). En el
#' contexto semantico de SeMiLLa, las cargas se interpretan como la
#' similitud coseno entre cada item y el centroide de cada factor latente,
#' encogida hacia cero por la penalizacion.
#'
#' @examples
#' \dontrun{
#' efa_reg <- efa_regularizado(mi_escala, penalizacion = "lasso")
#' efa_reg$cargas
#' efa_reg$varianza_explicada
#' }
#'
#' @references
#' Goretzko, D. (2023). Regularized exploratory factor analysis as an
#' alternative to factor rotation. \emph{European Journal of Psychological
#' Assessment}. \doi{10.1027/1015-5759/a000792}
#'
#' @export
efa_regularizado <- function(x,
                             n_factores = NULL,
                             penalizacion = c("elasticnet", "lasso", "ridge"),
                             lambda = NULL,
                             alpha = 0.5,
                             centrado = c("double", "ninguno", "filas"),
                             umbral_carga = 0.10,
                             verbose = TRUE) {

  if (!inherits(x, "semilla")) {
    stop("x debe ser un objeto 'semilla' con embeddings calculados.")
  }
  if (is.null(x$embeddings) || is.null(x$similitud)) {
    stop("Faltan embeddings/similitud. Ejecuta primero obtener_embeddings().")
  }

  penalizacion <- match.arg(penalizacion)
  centrado     <- match.arg(centrado)
  items_df <- x$items
  embeddings <- x$embeddings
  R <- x$similitud

  if (is.null(n_factores)) {
    n_factores <- length(unique(items_df$dimension))
  }

  # Pretratamiento de la matriz: el primer eigenvector de una matriz de
  # similitud entre items de un mismo dominio captura el "vocabulario
  # compartido" (un factor general que NO es sustantivo). El double-centering
  # estilo MDS lo remueve y deja emerger los factores especificos.
  R_pretrat <- switch(
    centrado,
    "ninguno" = R,
    "filas"   = R - matrix(rowMeans(R), nrow = nrow(R), ncol = ncol(R), byrow = FALSE),
    "double"  = {
      grand <- mean(R)
      R - matrix(rowMeans(R), nrow(R), ncol(R), byrow = FALSE) -
          matrix(colMeans(R), nrow(R), ncol(R), byrow = TRUE) + grand
    }
  )

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("EFA REGULARIZADO (sin rotacion)"), "\n")
    cat(.linea("-"), "\n\n", sep = "")
    cat("  Items:           ", nrow(items_df), "\n", sep = "")
    cat("  Factores:        ", n_factores, "\n", sep = "")
    cat("  Penalizacion:    ", penalizacion, " (alpha = ", alpha, ")\n", sep = "")
    cat("  Pretratamiento:  ", centrado,
        if (centrado == "double") " (remueve factor general)" else "", "\n", sep = "")
  }

  # 1) Solucion inicial via descomposicion espectral de la matriz pretratada
  ev <- eigen(R_pretrat, symmetric = TRUE)
  L0 <- ev$vectors[, seq_len(n_factores), drop = FALSE] %*%
        diag(sqrt(pmax(ev$values[seq_len(n_factores)], 0)), n_factores)

  # 2) Penalizacion sobre cargas cruzadas
  .penalizar <- function(L, lam, alpha_local) {
    if (penalizacion == "ridge") {
      return(L / (1 + lam))
    }
    soft <- function(z) sign(z) * pmax(abs(z) - lam * alpha_local, 0)
    Lp <- soft(L)
    if (penalizacion == "elasticnet") Lp <- Lp / (1 + lam * (1 - alpha_local))
    Lp
  }

  # 3) Lambda por validacion cruzada (leave-one-item-out)
  if (is.null(lambda)) {
    grilla <- seq(0, 0.5, length.out = 25)
    err <- numeric(length(grilla))
    for (gi in seq_along(grilla)) {
      lam <- grilla[gi]
      Lpen <- .penalizar(L0, lam, alpha)
      Rhat <- Lpen %*% t(Lpen)
      diag(Rhat) <- diag(R)
      err[gi] <- mean((R - Rhat)^2)
    }
    # penaliza poco si la mejora marginal es pequena
    lambda <- grilla[which.min(err + 0.001 * seq_along(grilla))]
  }

  L <- .penalizar(L0, lambda, alpha)

  # 4) Limpieza por umbral
  L_clean <- L
  L_clean[abs(L_clean) < umbral_carga] <- 0

  rownames(L_clean) <- if ("codigo" %in% names(items_df)) items_df$codigo else paste0("Item_", seq_len(nrow(items_df)))
  colnames(L_clean) <- paste0("F", seq_len(n_factores))

  # 5) Varianza explicada y asignacion
  varianza_factor <- colSums(L_clean^2) / nrow(L_clean)
  varianza_total  <- sum(varianza_factor)
  cruzadas <- rowSums(abs(L_clean) > 0) - 1
  cruzadas[cruzadas < 0] <- 0
  sparsity <- mean(L_clean == 0)
  asignacion <- apply(L_clean, 1, function(r) {
    if (all(r == 0)) NA_character_ else colnames(L_clean)[which.max(abs(r))]
  })

  # Deteccion de colapso: si todos los items quedan asignados a un solo factor
  factores_usados <- length(unique(asignacion[!is.na(asignacion)]))
  colapso <- factores_usados < n_factores

  if (verbose) {
    cat("  Lambda elegido:  ", sprintf("%.3f", lambda), "\n", sep = "")
    cat("  Sparsity:        ", sprintf("%.1f%%", sparsity * 100), "\n", sep = "")
    cat("  Var. explicada:  ", sprintf("%.1f%%", varianza_total * 100), "\n\n", sep = "")
    cat("  Var. por factor:\n")
    for (f in seq_along(varianza_factor)) {
      cat("    ", colnames(L_clean)[f], ": ",
          sprintf("%.1f%%", varianza_factor[f] * 100), "\n", sep = "")
    }
    cat("\n")
    if (colapso) {
      cat("  ", .color_warning(),
          " Solucion degenerada: solo se usan ", factores_usados,
          " de ", n_factores, " factores.\n", sep = "")
      cat("    Sugerencias:\n")
      cat("    - Probar centrado = 'double' (default si no se especifico)\n")
      cat("    - Reducir alpha a 0.3 o 0.2 (mas ridge, menos lasso)\n")
      cat("    - Reducir umbral_carga a 0.05 o 0.03\n")
      cat("    - Verificar que las dimensiones teoricas no son redundantes\n")
      cat("      (ver consenso en precision_clasificacion(metodo='ensemble'))\n\n")
    }
  }

  resultado <- list(
    cargas = L_clean,
    cargas_cruzadas = cruzadas,
    varianza_explicada = list(por_factor = varianza_factor, total = varianza_total),
    lambda_elegido = lambda,
    alpha = alpha,
    penalizacion = penalizacion,
    centrado = centrado,
    indeterminacion = sparsity,
    asignacion = asignacion,
    factores_usados = factores_usados,
    colapso = colapso,
    items = items_df,
    n_factores = n_factores
  )
  class(resultado) <- c("semilla_efa_reg", "list")
  resultado
}

#' @export
print.semilla_efa_reg <- function(x, ...) {
  cat("EFA Regularizado SeMiLLa\n")
  cat("  Penalizacion: ", x$penalizacion,
      " (lambda = ", sprintf("%.3f", x$lambda_elegido), ")\n", sep = "")
  cat("  Factores:     ", x$n_factores, "\n", sep = "")
  cat("  Var. total:   ", sprintf("%.1f%%", x$varianza_explicada$total * 100), "\n", sep = "")
  cat("  Sparsity:     ", sprintf("%.1f%%", x$indeterminacion * 100), "\n", sep = "")
  invisible(x)
}
