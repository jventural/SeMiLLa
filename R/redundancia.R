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
#' @param umbral_sem Umbral de similitud coseno para marcar pares redundantes.
#'   Default \code{"auto"}: cuantil .95 de la distribucion de similitudes de
#'   la propia escala, acotado a [0.70, 0.85]. La calibracion con dos
#'   escalas reales (bateria policial, n=280) mostro que ningun umbral FIJO
#'   sirve: en PM (linea base de similitud baja, media=.49) las parafrasis
#'   daninas vivian en .56-.78 y el antiguo 0.85 capturo 0 de 8; en ACO
#'   (actitud hacia UN objeto nombrado en cada item, media alta) un 0.70
#'   fijo marcaba 28 pares en una escala que empiricamente funciono,
#'   mientras que los pares de similitud alta RELATIVA (>= ~.78) coincidian
#'   con los items que la specification search empirica descarto. Puede
#'   fijarse un numero manualmente.
#' @param n_gram Tamano del n-grama de palabras para el solapamiento lexico y
#'   sintactico (por defecto 2).
#' @param umbral_sintactico Umbral del solapamiento medio de n-gramas a partir
#'   del cual se dispara la alerta de homogeneidad sintactica (por defecto 0.30).
#' @param umbral_faceta Similitud media intra-cluster a partir de la cual un
#'   grupo de items se considera una FACETA REPETIDA. Default \code{"auto"}:
#'   cuantil .75 de las similitudes de la escala, acotado a [0.55, 0.70].
#'   El analisis por pares no ve este patron: tres o mas items sobre la
#'   misma conducta pueden pasar el filtro de a pares y aun asi formar un
#'   bloque de dependencia local que rompe el ajuste (RMSEA) e infla la
#'   fiabilidad. Puede fijarse un numero manualmente.
#' @param min_items_faceta Numero minimo de items de un cluster para reportarlo
#'   como faceta repetida (por defecto 3).
#'
#' @return Objeto de clase \code{semilla_redundancia} (lista) con:
#' \itemize{
#'   \item \code{similitud_maxima}: lista con \code{global}, \code{media} y el
#'     vector \code{por_item}.
#'   \item \code{pares_redundantes}: data.frame de pares con coseno >= \code{umbral_sem}.
#'   \item \code{facetas_repetidas}: data.frame de clusters de 3+ items sobre la
#'     misma conducta (cluster, n_items, sim_media, nucleo lexico compartido,
#'     items), con la recomendacion de conservar 1-2 por cluster.
#'   \item \code{ngram_overlap}: lista con \code{media}, \code{maxima} y la matriz \code{J}.
#'   \item \code{homogeneidad_sintactica}: lista con \code{indice},
#'     \code{prefijo_compartido} y \code{alerta} (logico).
#'   \item \code{diversidad_lexica}: lista con \code{ttr_global} y \code{ttr_item}.
#'   \item \code{resumen}: data.frame por item (codigo, sim_max, ttr, prefijo).
#'   \item \code{alerta}: mensaje de texto con el veredicto.
#' }
#'
#' @section Limite de la similitud semantica:
#' La similitud de embeddings NO captura la correlacion empirica inducida por
#' deseabilidad social uniforme ni por estilos de respuesta: dos items de
#' temas distintos pero ambos muy deseables pueden correlacionar > .70 en
#' aplicacion con similitud coseno < .45 (observado en PM policial). Por eso
#' esta auditoria debe complementarse SIEMPRE con
#' \code{calificar_deseabilidad()} y \code{simular_estructura()} antes de
#' aplicar la escala.
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
auditar_redundancia <- function(x, umbral_sem = "auto", n_gram = 2,
                                umbral_sintactico = 0.30,
                                umbral_faceta = "auto",
                                min_items_faceta = 3L) {

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

  # --- umbrales adaptativos a la linea base de la escala -------------------
  # Escalas de actitud hacia UN objeto nombrado en cada item tienen linea
  # base de similitud alta y un umbral fijo sobre-alarma; escalas de rasgo
  # con linea base baja lo necesitan mas sensible. La senal es RELATIVA.
  # Techo 0.70: la calibracion con datos reales (bateria policial, n=280)
  # mostro que los pares con r policorica >= .70 (dependencia local real)
  # viven en coseno 0.43-0.78; un umbral de 0.80-0.85 no detecta NINGUNO.
  # 0.70 es la linea de deteccion; por debajo, la deteccion fiable es el
  # juez LLM de parafrasis (.juzgar_parafrasis_llm / blindar_escala).
  umbral_sem_auto <- identical(umbral_sem, "auto")
  if (umbral_sem_auto) {
    umbral_sem <- min(0.70, max(0.62,
      as.numeric(stats::quantile(S[ut], 0.95, na.rm = TRUE))))
  }
  umbral_faceta_auto <- identical(umbral_faceta, "auto")
  if (umbral_faceta_auto) {
    umbral_faceta <- min(0.70, max(0.55,
      as.numeric(stats::quantile(S[ut], 0.75, na.rm = TRUE))))
  }

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

  # --- 2b. facetas repetidas (clusters de 3+ items, misma conducta) --------
  # El chequeo por pares no ve el patron mas danino: varios items que de a
  # pares parecen "suficientemente distintos" pero todos parafrasean la misma
  # conducta. Se agrupan por clustering jerarquico (enlace promedio) sobre la
  # distancia 1 - similitud y se reportan los clusters con similitud media
  # interna >= umbral_faceta.
  dim_vec <- if (!is.null(items$dimension)) as.character(items$dimension)
             else rep(NA_character_, n)
  # El clustering es un subanalisis OPCIONAL de la auditoria: si falla, se
  # reporta y se sigue con los demas indices, en lugar de perder tambien la
  # alerta de homogeneidad sintactica y los pares redundantes.
  # Nombres declarados del constructo y de sus dimensiones: sirven para no
  # confundir cohesion TEMATICA con plantilla repetida (ver la excepcion (c) de
  # .detectar_facetas_repetidas). Solo los nombres, nunca las definiciones.
  nombres_decl <- .tokens_nombres_constructo(x, dim_vec)

  facetas <- tryCatch(
    .detectar_facetas_repetidas(
      S = S, textos = textos, codigos = codigos, dims = dim_vec,
      umbral_faceta = umbral_faceta, min_items = min_items_faceta,
      nombres_constructo = nombres_decl),
    error = function(e) {
      warning("Deteccion de facetas repetidas omitida (", conditionMessage(e),
              "); el resto de la auditoria de redundancia se mantiene.",
              call. = FALSE)
      data.frame(cluster = integer(0), n_items = integer(0),
                 sim_media = numeric(0), inter_dimension = logical(0),
                 nucleo_lexico = character(0), codigos = character(0),
                 items = character(0), stringsAsFactors = FALSE)
    })

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
  if (nrow(facetas) > 0) {
    alerta_txt <- paste0(
      alerta_txt, sprintf(paste0(" ALERTA de faceta repetida: %d cluster(s) de ",
                                 "%d+ items parafrasean la misma conducta; ",
                                 "conserve 1-2 items por cluster o el bloque ",
                                 "generara dependencia local (RMSEA alto) y ",
                                 "fiabilidad inflada."),
                          nrow(facetas), min_items_faceta))
  }

  out <- list(
    similitud_maxima = list(global = sim_max_global, media = sim_media,
                            por_item = stats::setNames(round(sim_max_item, 4), codigos)),
    pares_redundantes = pares,
    facetas_repetidas = facetas,
    ngram_overlap = list(media = ngram_media, maxima = ngram_max, n_gram = n_gram, J = J),
    homogeneidad_sintactica = list(indice = ngram_media, prefijo_compartido = pref_frac,
                                   alerta = alerta_sint),
    diversidad_lexica = list(ttr_global = ttr_global, ttr_item = ttr_item),
    resumen = resumen,
    alerta = alerta_txt,
    parametros = list(umbral_sem = umbral_sem, n_gram = n_gram,
                      umbral_sintactico = umbral_sintactico,
                      umbral_faceta = umbral_faceta,
                      min_items_faceta = min_items_faceta,
                      umbral_sem_auto = umbral_sem_auto,
                      umbral_faceta_auto = umbral_faceta_auto))
  class(out) <- c("semilla_redundancia", "list")
  out
}


# Agrupa los items por clustering jerarquico (enlace promedio) sobre
# 1 - similitud y devuelve los clusters de >= min_items cuya similitud media
# interna alcanza umbral_faceta. Para cada cluster se extrae el "nucleo
# lexico": palabras de contenido presentes en al menos el 60% de sus items,
# que nombran la conducta repetida (p. ej. "ayudo companeros").

#' @keywords internal
#' Palabras de contenido del NOMBRE del constructo y de sus dimensiones
#'
#' Deliberadamente NO incluye las definiciones: arrastran vocabulario incidental
#' ("companeros", "escuela") que aparece tambien en items genuinamente repetidos,
#' y usarlas silenciaba alarmas legitimas (medido el 2026-08-07 sobre PM).
#' @keywords internal
#' @noRd
.tokens_nombres_constructo <- function(x, dims = NULL) {
  stop_es <- c("de", "la", "el", "los", "las", "a", "en", "que", "y", "o", "u",
               "un", "una", "por", "del", "al", "para", "ante", "hacia",
               "sobre", "con", "sin", "su", "sus")
  cc <- if (is.list(x)) x$concepto else NULL
  d  <- if (is.list(cc)) cc$dimensiones else NULL
  partes <- c(
    if (!is.null(cc) && is.character(cc$concepto)) cc$concepto else
      if (is.character(cc)) cc else character(0),
    if (!is.null(names(d))) names(d) else
      if (is.character(d)) d else character(0),
    if (!is.null(dims)) unique(dims[!is.na(dims)]) else character(0))
  partes <- partes[nzchar(partes)]
  if (length(partes) == 0) return(character(0))
  t <- tolower(paste(partes, collapse = " "))
  t <- chartr("áéíóúüñ", "aeiouun", t)
  t <- gsub("[^[:alnum:][:space:]]", " ", t)
  w <- strsplit(trimws(t), "\\s+")[[1]]
  unique(w[nzchar(w) & nchar(w) > 3 & !(w %in% stop_es)])
}

.detectar_facetas_repetidas <- function(S, textos, codigos, dims,
                                        umbral_faceta = 0.55,
                                        min_items = 3L,
                                        nombres_constructo = character(0)) {
  vacio <- data.frame(cluster = integer(0), n_items = integer(0),
                      sim_media = numeric(0), inter_dimension = logical(0),
                      nucleo_lexico = character(0), codigos = character(0),
                      items = character(0), stringsAsFactors = FALSE)
  n <- length(textos)
  if (n < min_items) return(vacio)

  # El coseno puede volver como 1 + 1e-16 (o venir redondeado): se acota a
  # [-1, 1] para que la distancia 1 - S nunca sea negativa, y las alturas se
  # monotonizan porque los empates de similitud invierten el dendrograma por
  # redondeo y cutree() lo rechazaria (ver .hc_monotono).
  S <- pmin(pmax(as.matrix(S), -1), 1)
  hc <- .hc_monotono(stats::hclust(stats::as.dist(1 - S), method = "average"))
  grupos <- stats::cutree(hc, h = 1 - umbral_faceta)

  stop_es <- c("de", "la", "el", "los", "las", "a", "en", "que", "cuando",
               "mis", "mi", "me", "con", "y", "o", "u", "un", "una", "por",
               "se", "es", "ser", "esta", "estan", "esta", "le", "lo", "sin",
               "aunque", "nadie", "como", "del", "al", "su", "sus", "para",
               "no", "si", "mas", "muy", "ya", "hay", "he", "ha", "son")
  .tok_contenido <- function(t) {
    t <- tolower(t)
    t <- chartr("áéíóúüñ",
                "aeiouun", t)
    t <- gsub("[^[:alnum:][:space:]]", " ", t)
    w <- strsplit(trimws(t), "\\s+")[[1]]
    w[nzchar(w) & !(w %in% stop_es)]
  }

  filas <- list()
  k <- 0L
  for (g in unique(grupos)) {
    idx <- which(grupos == g)
    if (length(idx) < min_items) next
    Sg <- S[idx, idx, drop = FALSE]
    sim_media <- mean(Sg[upper.tri(Sg)])
    if (is.na(sim_media) || sim_media < umbral_faceta) next

    # Dos excepciones de cohesion LEGITIMA (calibradas con ACO, que
    # funciono en campo): (a) un cluster que es (casi) toda una dimension
    # teorica cohesiona porque debe; (b) un cluster cuyo nucleo lexico es
    # el OBJETO de la escala entera (palabra presente en la mayoria de
    # TODOS los items, p. ej. "corrupcion" en una escala de actitud hacia
    # la corrupcion) cohesiona por tema, no por parafraseo. En ambos casos
    # solo se marca si ADEMAS comparte plantilla fuerte (sim media >= .70;
    # la plantilla afectiva "siento X" de ACO llego a .70 y la poda
    # empirica retiro 4 de sus 8 items).
    dims_cluster <- dims[idx][!is.na(dims[idx])]
    es_dimension_completa <- FALSE
    if (length(dims_cluster) == length(idx) &&
        length(unique(dims_cluster)) == 1) {
      n_dim_total <- sum(dims == dims_cluster[1], na.rm = TRUE)
      es_dimension_completa <- length(idx) >= 0.75 * n_dim_total
    }

    toks <- lapply(textos[idx], .tok_contenido)
    todas <- unique(unlist(toks))
    frec <- vapply(todas, function(w)
      mean(vapply(toks, function(tt) w %in% tt, logical(1))), numeric(1))
    nucleo <- names(sort(frec[frec >= 0.60], decreasing = TRUE))
    if (length(nucleo) == 0 && length(frec) > 0) {
      # Cluster de nucleo difuso: etiquetar con los tokens mas frecuentes
      # (prefijo "~" = aproximado) para que el reporte nunca quede vacio.
      nucleo <- paste0("~", names(sort(frec, decreasing = TRUE))[
        seq_len(min(2, length(frec)))])
    }

    # (b) nucleo = objeto de la escala entera?
    es_objeto_escala <- FALSE
    if (length(nucleo) > 0) {
      toks_escala <- lapply(textos, .tok_contenido)
      pres_global <- mean(vapply(toks_escala, function(tt)
        nucleo[1] %in% tt, logical(1)))
      es_objeto_escala <- pres_global > 0.50
    }

    # (c) nucleo = el NOMBRE del constructo o de una dimension. La regla (b)
    # queria cubrir esto pero lo infiere por frecuencia y exige presencia en mas
    # del 50% de los items, asi que se le escapa el caso normal: en una escala
    # de "Ansiedad ante la estadistica" la palabra "estadistica" aparecio en el
    # 33% de los items -lo correcto: repetirla en los 24 seria redundancia de
    # verdad- y 7 items que describian sintomas DISTINTOS (bloqueo, sudoracion,
    # mente en blanco, no concentrarse...) se reportaron como "escritos con la
    # misma plantilla". El umbral premiaba la mala redaccion.
    # Aqui no se infiere: el nombre del constructo y los de las dimensiones son
    # dato declarado. Que los items lo mencionen es TEMA, no parafraseo.
    # Ojo, solo los NOMBRES: probado tambien con las definiciones largas
    # (2026-08-07) silenciaba un grupo de PM con redundancia real -"uso el
    # uniforme... frente a mis companeros" / "cuido mi uniforme para mostrar
    # respeto a mis companeros"- porque "companeros" salia en la definicion.
    # Apagar una alarma legitima es peor que dejar una de mas.
    es_nombre_declarado <- length(nucleo) > 0 &&
      length(nombres_constructo) > 0 &&
      any(sub("^~", "", nucleo) %in% nombres_constructo)

    if ((es_dimension_completa || es_objeto_escala || es_nombre_declarado) &&
        sim_media < 0.70) next

    k <- k + 1L
    filas[[k]] <- data.frame(
      cluster = k,
      n_items = length(idx),
      sim_media = round(sim_media, 3),
      inter_dimension = length(unique(dims[idx][!is.na(dims[idx])])) > 1,
      nucleo_lexico = paste(utils::head(nucleo, 5), collapse = " "),
      codigos = paste(codigos[idx], collapse = ", "),
      items = paste(textos[idx], collapse = " | "),
      stringsAsFactors = FALSE
    )
  }
  if (k == 0L) return(vacio)
  do.call(rbind, filas)
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
  cat(sprintf("Similitud coseno  : maxima = %.3f | media (linea base) = %.3f\n",
              x$similitud_maxima$global, x$similitud_maxima$media))
  cat(sprintf("Pares redundantes : %d (umbral %.0f%%%s)\n",
              nrow(x$pares_redundantes), 100 * x$parametros$umbral_sem,
              if (isTRUE(x$parametros$umbral_sem_auto))
                " auto, adaptado a la linea base" else ""))
  cat(sprintf("Solapamiento %d-grama: media = %.3f | maximo = %.3f\n",
              x$ngram_overlap$n_gram, x$ngram_overlap$media, x$ngram_overlap$maxima))
  cat(sprintf("Diversidad lexica : TTR global = %.3f\n", x$diversidad_lexica$ttr_global))
  cat(sprintf("Prefijo compartido: %.0f%% de los items\n",
              100 * x$homogeneidad_sintactica$prefijo_compartido))
  fac <- x$facetas_repetidas
  if (!is.null(fac) && nrow(fac) > 0) {
    cat(lin(), "\n")
    cat(amar("[!] FACETAS REPETIDAS"), " (clusters de ",
        x$parametros$min_items_faceta, "+ items sobre la misma conducta):\n",
        sep = "")
    for (i in seq_len(nrow(fac))) {
      cat(sprintf("  Cluster %d: %d items, sim media %.2f%s | nucleo: \"%s\"\n",
                  fac$cluster[i], fac$n_items[i], fac$sim_media[i],
                  if (fac$inter_dimension[i]) " [CRUZA DIMENSIONES]" else "",
                  fac$nucleo_lexico[i]))
      cat("    ", fac$codigos[i], "\n", sep = "")
    }
    cat("  Recomendacion: conserve 1-2 items por cluster y reemplace el resto\n")
    cat("  por manifestaciones DISTINTAS de la dimension (refinar_escala()).\n")
  }
  cat(lin(), "\n")
  if (isTRUE(x$homogeneidad_sintactica$alerta) ||
      (!is.null(fac) && nrow(fac) > 0)) {
    cat(amar("[!] "), x$alerta, "\n", sep = "")
  } else {
    cat(x$alerta, "\n")
  }
  cat("Nota: la similitud semantica NO detecta la correlacion inducida por\n")
  cat("deseabilidad social uniforme; complemente con calificar_deseabilidad()\n")
  cat("y simular_estructura() antes de aplicar la escala.\n")
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
