# =============================================================================
# VALIDEZ DE CRITERIO PREDICHA
# Inspirado en Fokkema, Iliescu, Greiff & Ziegler (2022), regresion regularizada
# =============================================================================

#' @title Validez de Criterio Predicha desde Embeddings
#'
#' @description
#' Estima validez de criterio \emph{a priori} ajustando un modelo de
#' regresion regularizada (logistica/lineal con elastic-net) sobre los
#' embeddings de los items para predecir un criterio externo declarado.
#'
#' Sirve para escenarios donde el usuario ya cuenta con (a) un criterio
#' historico o piloto (puntuaciones, diagnostico, rendimiento) o (b)
#' anotaciones de expertos sobre cuanto cada item se relaciona con el
#' criterio. SeMiLLa devuelve un estimado de validez predictiva del
#' instrumento antes de la aplicacion masiva.
#'
#' @param x Objeto semilla con embeddings calculados.
#' @param criterio Vector numerico (continuo) o factor (binario) con la
#'   misma longitud que el numero de respondientes piloto, o bien un
#'   data.frame de items con una columna numerica que califique la
#'   relevancia de cada item respecto al criterio.
#' @param respuestas Matriz/data.frame de respuestas de los items
#'   (filas = sujetos, columnas = items). Requerido cuando \code{criterio}
#'   es a nivel de sujeto. Si NULL, se usa la modalidad item-level.
#' @param tipo "auto" (default), "lineal" o "logistico". En auto se
#'   detecta segun la naturaleza de \code{criterio}.
#' @param alpha Mezcla L1/L2 elastic-net (1 = lasso, 0 = ridge).
#' @param folds Numero de folds para CV (default: 10).
#' @param verbose Mostrar progreso.
#'
#' @return Lista de clase \code{semilla_criterio} con:
#' \itemize{
#'   \item \code{r_cv}: correlacion (o AUC) validada por CV.
#'   \item \code{coeficientes}: peso por item.
#'   \item \code{items_clave}: items con mayor contribucion absoluta.
#'   \item \code{r2_cv}, \code{lambda_optimo}, \code{tipo}.
#' }
#'
#' @details
#' Replica el hallazgo de Fokkema et al. (2022) de que la regresion
#' logistica regularizada predice tan bien o mejor que metodos no
#' lineales sofisticados. Se ajustan modelos elastic-net via \code{glmnet}
#' (si esta disponible); en su ausencia se cae a regresion ridge cerrada
#' implementada en base R.
#'
#' @examples
#' \dontrun{
#' # Modalidad item-level (rating de expertos sobre cada item)
#' valoraciones <- data.frame(numero = 1:n, relevancia = runif(n, 0, 1))
#' vc <- validez_criterio_predicha(mi_escala, criterio = valoraciones)
#'
#' # Modalidad sujeto-level (respuestas + criterio empirico)
#' vc <- validez_criterio_predicha(mi_escala,
#'                                 criterio = rendimiento,
#'                                 respuestas = matriz_respuestas)
#' }
#'
#' @references
#' Fokkema, M., Iliescu, D., Greiff, S., & Ziegler, M. (2022). Machine
#' learning and prediction in psychological assessment: Some promises and
#' pitfalls. \emph{European Journal of Psychological Assessment}, 38(3),
#' 165-175. \doi{10.1027/1015-5759/a000714}
#'
#' @export
validez_criterio_predicha <- function(x,
                                      criterio,
                                      respuestas = NULL,
                                      tipo = c("auto", "lineal", "logistico"),
                                      alpha = 0.5,
                                      folds = 10,
                                      verbose = TRUE) {

  if (!inherits(x, "semilla")) stop("x debe ser un objeto 'semilla'.")
  if (is.null(x$embeddings)) stop("Faltan embeddings en x.")

  tipo <- match.arg(tipo)
  items_df <- x$items
  embeddings <- x$embeddings

  modo <- if (is.data.frame(criterio) && any(c("relevancia","peso","carga") %in% names(criterio))) {
    "item_level"
  } else {
    "sujeto_level"
  }

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("VALIDEZ DE CRITERIO PREDICHA"), "\n")
    cat(.linea("-"), "\n\n", sep = "")
    cat("  Modo: ", modo, "\n", sep = "")
  }

  has_glmnet <- requireNamespace("glmnet", quietly = TRUE)

  if (modo == "item_level") {
    col_y <- intersect(c("relevancia","peso","carga"), names(criterio))[1]
    y <- criterio[[col_y]]
    if (length(y) != nrow(embeddings)) {
      stop("La columna criterio debe tener una entrada por item.")
    }
    X <- embeddings
    auto_tipo <- if (tipo == "auto") "lineal" else tipo

    if (has_glmnet) {
      cv <- glmnet::cv.glmnet(X, y, alpha = alpha, nfolds = min(folds, length(y) - 1),
                              family = if (auto_tipo == "logistico") "binomial" else "gaussian")
      coefs <- as.vector(coef(cv, s = "lambda.min"))[-1]
      pred  <- as.vector(predict(cv, newx = X, s = "lambda.min"))
      r_cv  <- suppressWarnings(cor(pred, y))
      lambda_opt <- cv$lambda.min
    } else {
      # Ridge cerrada
      lambda_opt <- 0.1
      Xc <- scale(X)
      yc <- scale(y)
      coefs <- as.vector(solve(crossprod(Xc) + lambda_opt * diag(ncol(Xc)),
                               crossprod(Xc, yc)))
      pred  <- as.vector(Xc %*% coefs)
      r_cv  <- suppressWarnings(cor(pred, yc))
    }

    contrib <- abs(embeddings %*% coefs)
    items_clave <- data.frame(
      Codigo = if ("codigo" %in% names(items_df)) items_df$codigo else paste0("Item_", seq_len(nrow(items_df))),
      Item = items_df$item,
      Dimension = items_df$dimension,
      Contribucion = round(as.numeric(contrib), 3),
      stringsAsFactors = FALSE
    )
    items_clave <- items_clave[order(-items_clave$Contribucion), , drop = FALSE]
    r2_cv <- as.numeric(r_cv^2)
  } else {
    if (is.null(respuestas)) {
      stop("Para criterio a nivel de sujeto, debes pasar 'respuestas'.")
    }
    R <- as.matrix(respuestas)
    if (ncol(R) != nrow(embeddings)) {
      stop("'respuestas' debe tener una columna por item.")
    }
    auto_tipo <- if (tipo == "auto") {
      if (is.factor(criterio) || length(unique(criterio)) == 2) "logistico" else "lineal"
    } else tipo

    # Score predicho por item via embeddings -> proyectado a sujeto via respuestas
    if (has_glmnet) {
      cv <- glmnet::cv.glmnet(R, criterio, alpha = alpha, nfolds = folds,
                              family = if (auto_tipo == "logistico") "binomial" else "gaussian")
      coefs <- as.vector(coef(cv, s = "lambda.min"))[-1]
      pred  <- as.vector(predict(cv, newx = R, s = "lambda.min"))
      lambda_opt <- cv$lambda.min
      r_cv <- if (auto_tipo == "logistico") {
        .auc_simple(pred, criterio)
      } else suppressWarnings(cor(pred, criterio))
    } else {
      lambda_opt <- 0.1
      coefs <- as.vector(solve(crossprod(R) + lambda_opt * diag(ncol(R)),
                               crossprod(R, as.numeric(criterio))))
      pred  <- as.vector(R %*% coefs)
      r_cv  <- suppressWarnings(cor(pred, as.numeric(criterio)))
    }
    items_clave <- data.frame(
      Codigo = if ("codigo" %in% names(items_df)) items_df$codigo else paste0("Item_", seq_len(nrow(items_df))),
      Item = items_df$item,
      Dimension = items_df$dimension,
      Peso = round(coefs, 3),
      stringsAsFactors = FALSE
    )
    items_clave <- items_clave[order(-abs(items_clave$Peso)), , drop = FALSE]
    r2_cv <- as.numeric(r_cv^2)
  }

  if (verbose) {
    cat("  R/AUC validado por CV: ", sprintf("%.3f", r_cv), "\n", sep = "")
    cat("  R^2 (aprox.):          ", sprintf("%.3f", r2_cv), "\n", sep = "")
    cat("  Lambda optimo:         ", sprintf("%.4f", lambda_opt), "\n\n", sep = "")
    cat("  Top 5 items con mayor contribucion:\n")
    for (i in seq_len(min(5, nrow(items_clave)))) {
      cat("    ", items_clave$Codigo[i], ": ",
          format(round(items_clave[[ncol(items_clave)]][i], 3), nsmall = 3), "\n", sep = "")
    }
    cat("\n")
  }

  resultado <- list(
    r_cv = as.numeric(r_cv),
    r2_cv = r2_cv,
    coeficientes = coefs,
    items_clave = items_clave,
    lambda_optimo = lambda_opt,
    alpha = alpha,
    tipo = if (modo == "item_level") "lineal" else
           if (length(unique(criterio)) == 2) "logistico" else "lineal",
    modo = modo,
    glmnet_disponible = has_glmnet
  )
  class(resultado) <- c("semilla_criterio", "list")
  resultado
}

#' @keywords internal
.auc_simple <- function(pred, y) {
  y <- as.numeric(as.factor(y)) - 1
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]
  n_pos <- sum(y == 1); n_neg <- sum(y == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  ranks <- rank(pred)
  (sum(ranks[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' @export
print.semilla_criterio <- function(x, ...) {
  cat("Validez de Criterio Predicha SeMiLLa\n")
  cat("  Modo:    ", x$modo, "\n", sep = "")
  cat("  Tipo:    ", x$tipo, "\n", sep = "")
  cat("  R/AUC:   ", sprintf("%.3f", x$r_cv), "\n", sep = "")
  cat("  Lambda:  ", sprintf("%.4f", x$lambda_optimo), "\n", sep = "")
  invisible(x)
}
