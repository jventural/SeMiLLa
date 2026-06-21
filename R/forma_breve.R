#' @title Forma breve por discriminacion (semantica o hibrida con piloto)
#'
#' @description
#' Selecciona una forma breve eligiendo, por dimension, los items mas
#' discriminantes. \strong{Por defecto opera SIN datos de respuesta} (modo
#' semantico); el modo hibrido es \emph{opcional} y solo se activa si el usuario
#' dispone de un piloto de respuestas. Tiene, pues, dos modos:
#' \itemize{
#'   \item \strong{Semantico} (defecto, sin datos): puntua cada item por su
#'     \emph{representatividad neta de discriminacion}, la similitud media con
#'     los demas items de su propia dimension (representatividad del factor
#'     comun) penalizada por la similitud media con otras dimensiones (riesgo de
#'     carga cruzada): \code{repr - beta * cross}. Es el analogo semantico de
#'     "carga factorial alta y discriminante".
#'   \item \strong{Hibrido} (si se pasa \code{respuestas_piloto}): hace el corte
#'     final por discriminacion \emph{empirica} (correlacion item-resto dentro de
#'     la dimension) calculada sobre un pequeno piloto de respuestas reales,
#'     usando lo semantico solo como guardia anti-redundancia y desempate. Captura
#'     propiedades (varianza, efecto techo) que los embeddings no observan, por lo
#'     que coincide mucho mejor con la seleccion empirica que el modo semantico.
#' }
#' A diferencia de \code{\link{forma_corta}} (k-means + item prototipico/centroide).
#'
#' @section Recomendacion de uso (forma_corta vs forma_breve):
#' \itemize{
#'   \item \strong{Sin datos de respuesta (caso habitual): preferir
#'     \code{\link{forma_corta}}.} En la validacion interna (EEAP) el criterio
#'     k-means de \code{forma_corta} coincidio mejor con la seleccion empirica
#'     (~69\%) que \code{forma_breve} en modo semantico (~56\%).
#'   \item \strong{Con un piloto de respuestas reales (~90+): usar
#'     \code{forma_breve} en modo hibrido} (\code{respuestas_piloto}), que sube
#'     la coincidencia a ~88\%.
#' }
#' En resumen: el aporte distintivo de \code{forma_breve} es su modo hibrido;
#' sin datos, \code{forma_corta} sigue siendo el default recomendado.
#'
#' @param x Objeto \code{semilla} o lista con \code{$embeddings} y \code{$items}
#'   (con columna \code{dimension}; usa \code{$similitud} si esta disponible).
#' @param n_items Numero total de items a retener.
#' @param por_dimension Si \code{TRUE} (defecto), reparte \code{n_items} de forma
#'   proporcional entre dimensiones y selecciona dentro de cada una.
#' @param respuestas_piloto \strong{\code{NULL} por defecto} = modo semantico, no
#'   requiere datos (recomendado cuando no hay respuestas piloto, el caso
#'   habitual). Solo si se pasa una matriz/data.frame de respuestas con una
#'   columna por item (en el mismo orden que \code{x$items}) se activa el modo
#'   hibrido; basta un piloto pequeno (~90+ respuestas para mejorar de forma
#'   estable la coincidencia con la seleccion empirica).
#' @param beta_discriminante Peso de la penalizacion por similitud
#'   cross-dimension en el puntaje semantico (defecto 1).
#' @param umbral_redundancia Si la similitud coseno entre un candidato y un item
#'   ya elegido de la misma dimension supera este umbral, el candidato se omite
#'   (evita casi-duplicados). Defecto 0.95.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_forma_corta} (compatible con
#'   \code{plot_forma_corta} y \code{ensamblar}) con \code{$items},
#'   \code{$indices}, \code{$puntajes} (repr, cross, score y, en modo hibrido,
#'   \code{disc_empirica}), \code{$metodo} y metadatos.
#'
#' @seealso \code{\link{forma_corta}}, \code{\link{discriminacion_semantica}}
#' @export
forma_breve <- function(x,
                        n_items,
                        por_dimension = TRUE,
                        respuestas_piloto = NULL,
                        beta_discriminante = 1.0,
                        umbral_redundancia = 0.95,
                        verbose = TRUE) {

  # --- Extraer componentes ---
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$embeddings)) {
      stop("El objeto no tiene embeddings. Ejecuta obtener_embeddings() primero.")
    }
    embeddings <- x$embeddings; items_df <- x$items; sim <- x$similitud
  } else if (is.list(x) && !is.null(x$embeddings)) {
    embeddings <- x$embeddings; items_df <- x$items; sim <- x$similitud
  } else {
    stop("Objeto no valido. Usa un objeto semilla o lista con $embeddings.")
  }

  n_total <- nrow(embeddings)
  if (is.null(sim)) sim <- .calcular_similitud_coseno(embeddings)

  if (n_items >= n_total) {
    warning("n_items >= items totales. Retornando todos los items.")
    res <- list(items = items_df, indices = seq_len(n_total),
                n_original = n_total, n_seleccionados = n_total,
                metodo = "completa", por_dimension = por_dimension, puntajes = NULL)
    class(res) <- c("semilla_forma_breve", "semilla_forma_corta", "list")
    return(res)
  }

  if (!("dimension" %in% names(items_df))) por_dimension <- FALSE
  dimv <- if (por_dimension) as.character(items_df$dimension) else rep("global", n_total)

  # --- Puntaje semantico: repr - beta * cross ---
  repr <- numeric(n_total); cross <- numeric(n_total)
  for (i in seq_len(n_total)) {
    same  <- setdiff(which(dimv == dimv[i]), i)
    other <- which(dimv != dimv[i])
    repr[i]  <- if (length(same))  mean(sim[i, same])  else 0
    cross[i] <- if (length(other)) mean(sim[i, other]) else 0
  }
  score <- repr - beta_discriminante * cross

  # --- Modo hibrido: discriminacion empirica (item-resto por dimension) ---
  hibrido <- !is.null(respuestas_piloto)
  disc_emp <- rep(NA_real_, n_total)
  if (hibrido) {
    R <- as.matrix(respuestas_piloto)
    if (ncol(R) != n_total) {
      stop("respuestas_piloto debe tener una columna por item (", n_total,
           "), en el mismo orden que x$items.")
    }
    storage.mode(R) <- "numeric"
    for (dn in unique(dimv)) {
      idx <- which(dimv == dn)
      for (i in idx) {
        otros <- setdiff(idx, i)
        if (!length(otros)) next
        rest <- rowSums(R[, otros, drop = FALSE], na.rm = TRUE)
        disc_emp[i] <- suppressWarnings(
          stats::cor(R[, i], rest, use = "pairwise.complete.obs"))
      }
    }
  }

  # metrica de ranking: empirica si hay piloto, semantica si no (NA -> al final)
  rank_metric <- if (hibrido) disc_emp else score
  rank_metric[is.na(rank_metric)] <- -Inf
  metodo <- if (hibrido) "hibrido_piloto" else "discriminacion_neta"

  if (verbose) {
    cat("\n", .linea("="), "\n", sep = "")
    cat(.color_verde(sprintf("FORMA BREVE - %s",
        if (hibrido) "HIBRIDA (piloto empirico + guardia semantica)"
        else "DISCRIMINACION SEMANTICA NETA (repr - beta*cross)")), "\n")
    cat(.linea("="), "\n\n")
    cat("  Items originales: ", n_total, " | objetivo: ", n_items,
        " | por dimension: ", por_dimension, "\n\n", sep = "")
  }

  # --- Reparto proporcional por dimension ---
  dims <- unique(dimv)
  if (por_dimension) {
    tab <- table(dimv)[dims]
    n_por_dim <- round(tab / sum(tab) * n_items)
    d <- n_items - sum(n_por_dim)
    if (d != 0) {
      orden <- order(n_por_dim, decreasing = TRUE)
      for (i in seq_len(abs(d))) {
        idx <- orden[((i - 1) %% length(orden)) + 1]
        n_por_dim[idx] <- n_por_dim[idx] + sign(d)
      }
    }
  } else {
    n_por_dim <- stats::setNames(n_items, "global")
  }

  # --- Seleccion greedy por dimension con guardia anti-redundancia ---
  seleccion <- integer(0)
  for (dn in names(n_por_dim)) {
    k <- n_por_dim[dn]; if (k < 1) next
    idx_dim <- which(dimv == dn)
    cand <- idx_dim[order(rank_metric[idx_dim], decreasing = TRUE)]
    elegidos <- integer(0)
    for (cc in cand) {
      if (length(elegidos) && max(sim[cc, elegidos]) > umbral_redundancia) next
      elegidos <- c(elegidos, cc)
      if (length(elegidos) >= k) break
    }
    if (length(elegidos) < k) {
      resto <- setdiff(cand, elegidos)
      elegidos <- c(elegidos, head(resto, k - length(elegidos)))
    }
    seleccion <- c(seleccion, elegidos)
    if (verbose) cat("  [", dn, "] ", length(elegidos), " items\n", sep = "")
  }
  seleccion <- sort(unique(seleccion))

  items_sel <- items_df[seleccion, , drop = FALSE]
  items_sel$numero_original <- seleccion
  items_sel$numero <- seq_len(nrow(items_sel))

  puntajes <- data.frame(
    numero_original = seleccion, dimension = dimv[seleccion],
    repr = round(repr[seleccion], 4), cross = round(cross[seleccion], 4),
    score = round(score[seleccion], 4), row.names = NULL)
  if (hibrido) puntajes$disc_empirica <- round(disc_emp[seleccion], 4)

  res <- list(
    items = items_sel, indices = seleccion,
    n_original = n_total, n_seleccionados = length(seleccion),
    metodo = metodo, por_dimension = por_dimension,
    beta_discriminante = beta_discriminante, puntajes = puntajes)
  class(res) <- c("semilla_forma_breve", "semilla_forma_corta", "list")

  if (verbose) {
    cat("\n  ", .color_verde("Reduccion: "),
        round((1 - length(seleccion) / n_total) * 100, 1), "%\n", sep = "")
    cat(.linea("="), "\n\n")
  }
  res
}
