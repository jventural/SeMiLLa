# =============================================================================
# SeMiLLa - Auditoria multi-indice de redundancia de items
# =============================================================================

#' @title Auditoria multi-indice de redundancia de items
#'
#' @description
#' Amplia \code{\link{analizar_redundancia}} con un conjunto de indices que
#' distinguen la cohesion deseable del parafraseo encubierto y de la
#' homogeneidad sintactica. Sobre la matriz de similitud coseno y el texto de
#' los items calcula: (1) similitud maxima entre pares, (2) pares redundantes
#' (coseno > umbral), (3) solapamiento de n-gramas (Jaccard lexico),
#' (4) homogeneidad sintactica (plantilla compartida) con una alerta, y
#' (5) diversidad lexica (type-token ratio). El indice de homogeneidad
#' sintactica detecta escalas cuyos items comparten una plantilla casi
#' identica (por ejemplo, "Si mi pareja..., me sentiria..."), patron que
#' infla la similitud coseno sin reflejar contenido psicologico compartido.
#'
#' @param x Objeto \code{semilla} o \code{semilla_embeddings} (debe contener la
#'   matriz \code{$similitud} y el data.frame \code{$items} con la columna
#'   \code{item}).
#' @param umbral_sem Umbral de similitud coseno para marcar pares redundantes
#'   (por defecto 0.85).
#' @param n_gram Tamano del n-grama de palabras para el solapamiento lexico y
#'   sintactico (por defecto 2).
#' @param umbral_sintactico Umbral del solapamiento medio de n-gramas a partir
#'   del cual se dispara la alerta de homogeneidad sintactica (por defecto 0.30).
#'
#' @return Objeto de clase \code{semilla_redundancia} (lista) con:
#' \itemize{
#'   \item \code{similitud_maxima}: lista con \code{global}, \code{media} y el
#'     vector \code{por_item}.
#'   \item \code{pares_redundantes}: data.frame de pares con coseno >= \code{umbral_sem}.
#'   \item \code{ngram_overlap}: lista con \code{media}, \code{maxima} y la matriz \code{J}.
#'   \item \code{homogeneidad_sintactica}: lista con \code{indice},
#'     \code{prefijo_compartido} y \code{alerta} (logico).
#'   \item \code{diversidad_lexica}: lista con \code{ttr_global} y \code{ttr_item}.
#'   \item \code{resumen}: data.frame por item (codigo, sim_max, ttr, prefijo).
#'   \item \code{alerta}: mensaje de texto con el veredicto.
#' }
#'
#' @details
#' La homogeneidad sintactica se operacionaliza como el solapamiento medio de
#' n-gramas de palabras entre todos los pares de items (indice de Jaccard) y la
#' fraccion de items que comparten el mismo prefijo de \code{n_gram} palabras.
#' Un valor alto, junto con baja diversidad lexica, senala que la similitud
#' coseno esta capturando forma y no contenido; en ese caso los indices
#' semanticos pierden validez y la escala requiere revision de la redaccion.
#'
#' @examples
#' \dontrun{
#' esc <- semilla("celos en relaciones de pareja", api_key = Sys.getenv("OPENAI_API_KEY"))
#' aud <- auditar_redundancia(esc)
#' print(aud)
#' aud$homogeneidad_sintactica$alerta
#' }
#'
#' @seealso \code{\link{analizar_redundancia}}
#' @export
auditar_redundancia <- function(x, umbral_sem = 0.85, n_gram = 2,
                                umbral_sintactico = 0.30) {

  # --- extraer similitud + items (mismo contrato que analizar_redundancia) ---
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    S <- x$similitud
    items <- x$items
  } else {
    stop("Objeto no valido: se espera 'semilla' o 'semilla_embeddings'.")
  }
  if (is.null(S) || is.null(items) || is.null(items$item)) {
    stop("El objeto no contiene $similitud y $items$item.")
  }
  textos <- as.character(items$item)
  n <- length(textos)
  if (n < 2) stop("Se requieren al menos 2 items.")
  codigos <- if (!is.null(items$codigo)) as.character(items$codigo) else paste0("I", seq_len(n))

  # --- 1. similitud maxima entre pares -------------------------------------
  Sd <- S; diag(Sd) <- NA
  sim_max_item <- apply(Sd, 1, function(r) max(r, na.rm = TRUE))
  ut <- upper.tri(S)
  sim_max_global <- max(S[ut])
  sim_media <- mean(S[ut])

  # --- 2. pares redundantes (coseno >= umbral) -----------------------------
  pares <- data.frame()
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (S[i, j] >= umbral_sem) {
      pares <- rbind(pares, data.frame(
        item1 = i, item2 = j, similitud = round(S[i, j], 4),
        stringsAsFactors = FALSE))
    }
  }
  if (nrow(pares) > 0) pares <- pares[order(-pares$similitud), ]

  # --- tokenizacion y n-gramas (base R) ------------------------------------
  .tok <- function(t) {
    t <- tolower(t)
    t <- gsub("[^[:alnum:][:space:]]", " ", t)
    w <- strsplit(trimws(t), "\\s+")[[1]]
    w[nzchar(w)]
  }
  .ngr <- function(w, k) {
    if (length(w) < k) return(if (length(w)) paste(w, collapse = " ") else character(0))
    vapply(seq_len(length(w) - k + 1),
           function(i) paste(w[i:(i + k - 1)], collapse = " "), character(1))
  }
  .jac <- function(a, b) {
    a <- unique(a); b <- unique(b)
    u <- length(union(a, b))
    if (u == 0) 0 else length(intersect(a, b)) / u
  }
  toks <- lapply(textos, .tok)
  ng   <- lapply(toks, .ngr, k = n_gram)

  # --- 3. solapamiento de n-gramas (Jaccard lexico) ------------------------
  J <- matrix(0, n, n)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    J[i, j] <- J[j, i] <- .jac(ng[[i]], ng[[j]])
  }
  ngram_media <- mean(J[ut]); ngram_max <- max(J[ut])

  # --- 4. homogeneidad sintactica + alerta ---------------------------------
  pref <- vapply(toks, function(w) paste(utils::head(w, n_gram), collapse = " "),
                 character(1))
  pref_frac <- if (n > 0) max(table(pref)) / n else 0
  alerta_sint <- (ngram_media >= umbral_sintactico) || (pref_frac >= 0.5)

  # --- 5. diversidad lexica (type-token ratio) -----------------------------
  all_tok <- unlist(toks)
  ttr_global <- if (length(all_tok)) length(unique(all_tok)) / length(all_tok) else NA_real_
  ttr_item <- vapply(toks, function(w)
    if (length(w)) length(unique(w)) / length(w) else NA_real_, numeric(1))

  # --- resumen por item ----------------------------------------------------
  resumen <- data.frame(
    num = seq_len(n), codigo = codigos,
    sim_max = round(sim_max_item, 3),
    ttr = round(ttr_item, 3),
    prefijo = pref, stringsAsFactors = FALSE)

  # --- veredicto -----------------------------------------------------------
  alerta_txt <- if (alerta_sint) {
    sprintf(paste0("ALERTA de homogeneidad sintactica: el solapamiento medio de ",
                   "%d-gramas es %.2f y el %.0f%% de los items comparte el prefijo ",
                   "\"%s\". La similitud coseno puede estar capturando forma y no ",
                   "contenido; revise la redaccion antes de interpretar los indices ",
                   "semanticos."),
            n_gram, ngram_media, 100 * pref_frac, names(which.max(table(pref)))[1])
  } else {
    sprintf(paste0("Sin homogeneidad sintactica relevante (solapamiento medio de ",
                   "%d-gramas = %.2f). %d par(es) redundante(s) por encima del %.0f%%."),
            n_gram, ngram_media, nrow(pares), 100 * umbral_sem)
  }

  out <- list(
    similitud_maxima = list(global = sim_max_global, media = sim_media,
                            por_item = stats::setNames(round(sim_max_item, 4), codigos)),
    pares_redundantes = pares,
    ngram_overlap = list(media = ngram_media, maxima = ngram_max, n_gram = n_gram, J = J),
    homogeneidad_sintactica = list(indice = ngram_media, prefijo_compartido = pref_frac,
                                   alerta = alerta_sint),
    diversidad_lexica = list(ttr_global = ttr_global, ttr_item = ttr_item),
    resumen = resumen,
    alerta = alerta_txt,
    parametros = list(umbral_sem = umbral_sem, n_gram = n_gram,
                      umbral_sintactico = umbral_sintactico))
  class(out) <- c("semilla_redundancia", "list")
  out
}

#' @title Imprimir auditoria de redundancia
#' @param x Objeto \code{semilla_redundancia}.
#' @param ... Ignorado.
#' @return El objeto \code{x} de forma invisible.
#' @export
print.semilla_redundancia <- function(x, ...) {
  ok_color <- exists(".color_verde", mode = "function")
  verde <- function(s) if (ok_color) .color_verde(s) else s
  amar  <- function(s) if (ok_color) .color_amarillo(s) else s
  lin   <- function() if (exists(".linea", mode = "function")) .linea("-") else strrep("-", 60)

  cat("\n", verde("AUDITORIA DE REDUNDANCIA MULTI-INDICE"), "\n", sep = "")
  cat(lin(), "\n")
  cat(sprintf("Similitud coseno  : maxima = %.3f | media = %.3f\n",
              x$similitud_maxima$global, x$similitud_maxima$media))
  cat(sprintf("Pares redundantes : %d (umbral %.0f%%)\n",
              nrow(x$pares_redundantes), 100 * x$parametros$umbral_sem))
  cat(sprintf("Solapamiento %d-grama: media = %.3f | maximo = %.3f\n",
              x$ngram_overlap$n_gram, x$ngram_overlap$media, x$ngram_overlap$maxima))
  cat(sprintf("Diversidad lexica : TTR global = %.3f\n", x$diversidad_lexica$ttr_global))
  cat(sprintf("Prefijo compartido: %.0f%% de los items\n",
              100 * x$homogeneidad_sintactica$prefijo_compartido))
  cat(lin(), "\n")
  if (isTRUE(x$homogeneidad_sintactica$alerta)) {
    cat(amar("[!] "), x$alerta, "\n", sep = "")
  } else {
    cat(x$alerta, "\n")
  }
  cat("\n")
  invisible(x)
}

# Chequeo ligero de homogeneidad sintactica sobre los textos de los items.
# Reutilizado por omega_semantico() y fiabilidad_semantica() para advertir
# cuando los indices semanticos pueden no ser interpretables (patron Celos).
.homogeneidad_sintactica <- function(textos, n_gram = 2, umbral = 0.30) {
  textos <- as.character(textos)
  textos <- textos[!is.na(textos) & nzchar(textos)]
  n <- length(textos)
  if (n < 2) return(list(alerta = FALSE, ngram_media = NA_real_, prefijo_frac = NA_real_))
  .tok <- function(t) {
    t <- tolower(t); t <- gsub("[^[:alnum:][:space:]]", " ", t)
    w <- strsplit(trimws(t), "\\s+")[[1]]; w[nzchar(w)]
  }
  .ngr <- function(w, k) {
    if (length(w) < k) return(if (length(w)) paste(w, collapse = " ") else character(0))
    vapply(seq_len(length(w) - k + 1), function(i) paste(w[i:(i + k - 1)], collapse = " "), character(1))
  }
  .jac <- function(a, b) {
    a <- unique(a); b <- unique(b); u <- length(union(a, b))
    if (u == 0) 0 else length(intersect(a, b)) / u
  }
  toks <- lapply(textos, .tok); ng <- lapply(toks, .ngr, k = n_gram)
  acc <- 0; cnt <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) { acc <- acc + .jac(ng[[i]], ng[[j]]); cnt <- cnt + 1 }
  ngram_media <- if (cnt > 0) acc / cnt else 0
  pref <- vapply(toks, function(w) paste(utils::head(w, n_gram), collapse = " "), character(1))
  prefijo_frac <- max(table(pref)) / n
  list(alerta = (ngram_media >= umbral) || (prefijo_frac >= 0.5),
       ngram_media = ngram_media, prefijo_frac = prefijo_frac)
}
