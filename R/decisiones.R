# =============================================================================
# SeMiLLa - Modelos de DECISION (Jev, por la Decisions API de OpenRouter)
# =============================================================================
#
# Un modelo de decision no redacta: recibe un texto y una pregunta con opciones
# cerradas y devuelve la PROBABILIDAD de cada opcion. Para SeMiLLa eso sirve
# donde hoy se le pide a un modelo de chat un numero (que dimension mide un
# item, cuan deseable es) y el numero cambia de una llamada a otra.
#
# Medido el 09-10-2026 en 9 escalas reales (306 items, n = 1500 por escala):
# Jev asigno cada item a su dimension con AMI 0,67 frente a la estructura
# empirica (el clustering de embeddings, 0,50) y dio la misma probabilidad en
# dos corridas (r = 0,998). Leer las 9 escalas costo US$ 0,002.
#
# Solo funciona con una clave de OpenRouter ("sk-or-..."). Con otra clave las
# funciones que lo usan caen a su camino de siempre.
# =============================================================================

.URL_DECISIONES <- "https://openrouter.ai/api/alpha/decisions"

#' @keywords internal
#' @noRd
.decision_disponible <- function(api_key) {
  is.character(api_key) && length(api_key) == 1L && !is.na(api_key) &&
    grepl("^sk-or-", trimws(api_key)) &&
    !isFALSE(getOption("SeMiLLa.usar_jev", TRUE))
}

# Envia preguntas a un modelo de decision y devuelve la lista de respuestas
# (nombrada como las preguntas). Cada respuesta trae, segun el tipo:
#   choice -> $choice, $probabilities (lista opcion -> p), $confidence
#   score  -> $score (posicion esperada, base 0), $probabilities, $confidence
#   noul   -> $noul (probabilidad de "si")
# Las preguntas se mandan en lotes de 'lote' para no pasar el contexto de
# 32.000 tokens. Devuelve NULL si la API no responde (el llamador decide el
# camino alternativo). attr(, "costo") lleva el costo en dolares.
#' @keywords internal
#' @noRd
.llamar_decisiones <- function(preguntas, estado, api_key,
                               modelo = getOption("SeMiLLa.modelo_decision", "typesafe/jev-1.13"),
                               lote = 20L, intentos = 3L) {
  if (!length(preguntas)) return(list())
  respuestas <- list()
  costo <- 0
  grupos <- split(names(preguntas), ceiling(seq_along(preguntas) / lote))
  for (g in grupos) {
    payload <- list(model = modelo, state = estado, questions = preguntas[g])
    cache_path <- NULL
    if (.cache_enabled()) {
      cache_path <- .cache_key("decision", payload)
      cached <- .cache_get(cache_path)
      if (!is.null(cached)) {
        respuestas[names(cached)] <- cached
        next
      }
    }
    js <- NULL
    for (i in seq_len(intentos)) {
      r <- tryCatch(httr::POST(
        .URL_DECISIONES,
        httr::add_headers(Authorization = paste("Bearer", trimws(api_key)),
                          `Content-Type` = "application/json"),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
        encode = "raw", httr::timeout(180)), error = function(e) NULL)
      if (!is.null(r) && httr::status_code(r) == 200) {
        js <- tryCatch(jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                                          simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(js$answers)) break
      }
      js <- NULL
      Sys.sleep(2 * i)
    }
    if (is.null(js)) return(NULL)
    costo <- costo + (js$usage$cost %||% 0)
    respuestas[names(js$answers)] <- js$answers
    if (!is.null(cache_path)) .cache_set(cache_path, js$answers)
  }
  attr(respuestas, "costo") <- costo
  respuestas
}

# Probabilidades de una respuesta 'choice' o 'score' como vector numerico con
# nombre, en el orden de 'opciones'. Las opciones ausentes valen 0.
#' @keywords internal
#' @noRd
.probs_decision <- function(respuesta, opciones) {
  p <- respuesta$probabilities
  v <- vapply(opciones, function(o) as.numeric(p[[o]] %||% 0), numeric(1))
  s <- sum(v)
  if (s > 0) v / s else v
}

# --- Correlacion entre dimensiones (phi por par) -------------------------------
# Medido el 09-10-2026 contra la correlacion REAL de 72 pares de dimensiones
# (AFC con n = 1500 en 9 escalas): suponer 0,47 para todos los pares erraba en
# promedio 0,248; Jev erro 0,154, Haiku 0,130 y su promedio 0,135 (r = 0,83
# con el phi real). Jev dio la misma estimacion en dos corridas (r = 0,998).
# 2.12.5: la escala terminaba en "muy fuerte (.70 o mas)" = 0,72, asi que la
# estimacion no podia distinguir un par de .70 de uno de .90, que es justo el
# tramo donde dos dimensiones dejan de separarse (PM policial: .92 real). Se
# abren dos anclas mas. Las ocho anteriores quedan en .SeMiLLa_anclas_phi_8 por
# si hay que reproducir una corrida de 2.12.0-2.12.4
# (options(SeMiLLa.anclas_phi = "8")).
.ANCLAS_PHI <- c("sin relacion (cerca de 0)", "muy debil (cerca de .10)", "debil (cerca de .20)",
                 "moderada-baja (cerca de .30)", "moderada (cerca de .40)",
                 "moderada-alta (cerca de .50)", "fuerte (cerca de .60)",
                 "muy fuerte (cerca de .70)",
                 "casi no se distinguen (cerca de .80)",
                 "miden practicamente lo mismo (.90 o mas)")
.VALORES_PHI <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.92)
.ANCLAS_PHI_8 <- c(.ANCLAS_PHI[1:7], "muy fuerte (.70 o mas)")
.VALORES_PHI_8 <- c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.72)

#' @keywords internal
#' @noRd
.anclas_phi <- function() {
  if (identical(as.character(getOption("SeMiLLa.anclas_phi", "10")), "8"))
    list(anclas = .ANCLAS_PHI_8, valores = .VALORES_PHI_8)
  else list(anclas = .ANCLAS_PHI, valores = .VALORES_PHI)
}

# Devuelve la matriz K x K de |phi| estimada (diagonal 1), con las dimensiones
# en el orden de unique(x$items$dimension). NULL si ningun lector respondio.
#' @keywords internal
#' @noRd
.estimar_phi_pares <- function(x, api_key, modelo = "gpt-4.1-mini", n_ejemplos = 8L,
                               resp_juez = NULL, poblacion = NULL) {
  pp <- .prompts_phi(x, n_ejemplos, poblacion = poblacion)
  if (is.null(pp)) return(NULL)
  dims <- pp$dims; K <- length(dims); pares <- pp$pares; enunciado <- pp$enunciado
  est <- list()
  ap <- .anclas_phi()

  if (.decision_disponible(api_key)) {
    preg <- stats::setNames(lapply(pares, function(pr) list(
      type = "score", criteria = as.list(ap$anclas), instructions = enunciado(pr))),
      paste0("p", seq_along(pares)))
    r <- .llamar_decisiones(preg, list(task = "Estimar la correlacion latente entre dimensiones"), api_key)
    if (!is.null(r)) est$jev <- vapply(seq_along(pares), function(k) {
      a <- r[[paste0("p", k)]]
      if (is.null(a$probabilities)) NA_real_ else
        sum(.probs_decision(a, as.character(seq_along(ap$valores) - 1L)) * ap$valores)
    }, numeric(1))
  }
  # El modelo de juicio (Haiku con OpenRouter), un par por llamada en paralelo.
  # 16-10-2026: la compuerta puede traer estas respuestas ya hechas (resp_juez),
  # enviadas en la misma tanda que los otros jueces.
  resp <- resp_juez %||% tryCatch(
    .chat_en_paralelo(pp$prompts, api_key, modelo, max_tokens = 100L,
                      temperature = 0, razonamiento = "low"),
    error = function(e) NULL)
  if (!is.null(resp)) est$juez <- vapply(resp, function(t) {
    if (is.null(t)) return(NA_real_)
    m <- regmatches(t, regexpr("[01]?\\.[0-9]+|\\b[01]\\b", t))
    if (length(m)) min(1, abs(as.numeric(m))) else NA_real_
  }, numeric(1))

  if (!length(est)) return(NULL)
  v <- rowMeans(do.call(cbind, est), na.rm = TRUE)
  if (all(!is.finite(v))) return(NULL)
  v[!is.finite(v)] <- mean(v, na.rm = TRUE)
  Phi <- diag(K)
  for (k in seq_along(pares)) Phi[pares[[k]][1], pares[[k]][2]] <- Phi[pares[[k]][2], pares[[k]][1]] <- v[k]
  dimnames(Phi) <- list(dims, dims)
  attr(Phi, "lectores") <- names(est)
  .phi_definida_positiva(Phi)
}

# Pares de dimensiones y prompts del juez para .estimar_phi_pares(). NULL con
# una sola dimension.
#' @keywords internal
#' @noRd
.prompts_phi <- function(x, n_ejemplos = 8L, poblacion = NULL) {
  dims <- unique(as.character(x$items$dimension)); K <- length(dims)
  if (K < 2L) return(NULL)
  defs <- x$concepto$dimensiones
  # 2.12.5: hasta 2.12.4 se mostraban los 4 PRIMEROS items de cada dimension.
  # Con un indicador por item, eso era siempre las mismas 4 facetas. Ahora van
  # todos si caben en n_ejemplos, y si no, repartidos a lo largo de la dimension.
  describir <- function(d) {
    todos <- x$items$item[x$items$dimension == d]
    ej <- if (length(todos) <= n_ejemplos) todos else
      todos[unique(round(seq(1, length(todos), length.out = n_ejemplos)))]
    def <- if (is.list(defs) && is.character(defs[[d]])) paste0(" (", defs[[d]], ")") else ""
    paste0("\"", gsub("_", " ", d), "\"", def, ". Items de ejemplo: ", paste(ej, collapse = " | "))
  }
  pares <- utils::combn(K, 2, simplify = FALSE)
  # 2.12.5: la pregunta decia "adultos" para cualquier escala.
  poblacion <- poblacion %||% x$metadata$poblacion
  # Solo la primera oracion: la poblacion puede traer pegadas instrucciones de
  # contexto ("... (18 a 25 anos). PROHIBIDO mencionar ...").
  quienes <- if (is.character(poblacion) && length(poblacion) == 1L && nzchar(trimws(poblacion)))
    substr(trimws(sub("\\.\\s.*$", "", trimws(poblacion))), 1, 160) else "adultos"
  quienes <- sub("\\.$", "", quienes)
  enunciado <- function(pr) paste0(
    "En una muestra grande de ", quienes,
    ", que tan fuerte seria la correlacion entre los puntajes de ",
    "estas dos dimensiones de un cuestionario (fuerza absoluta, sin importar el signo)?\n",
    "Dimension A: ", describir(dims[pr[1]]), "\nDimension B: ", describir(dims[pr[2]]))
  prompts <- vapply(pares, function(pr) paste0(enunciado(pr),
    "\nResponde SOLO un objeto JSON {\"r\": <numero entre 0 y 1>}."), character(1))
  list(dims = dims, pares = pares, enunciado = enunciado, prompts = prompts)
}

# Banda objetivo de la correlacion entre dimensiones (2.12.5). Clasifica cada
# par de la matriz estimada: "alta" (por encima de la banda: riesgo de que no se
# separen), "baja" (por debajo: apenas se relacionan) o "dentro". La estimacion
# orienta, no pronostica (r = .83 con la real en 72 pares): por eso la banda da
# acciones y no entra al veredicto.
#' @keywords internal
#' @noRd
.banda_phi <- function(Phi, objetivo = c(0.25, 0.45)) {
  if (is.null(Phi) || !is.matrix(Phi) || nrow(Phi) < 2L || is.null(objetivo)) return(NULL)
  objetivo <- sort(as.numeric(objetivo))
  if (length(objetivo) != 2L || any(!is.finite(objetivo))) return(NULL)
  ij <- which(upper.tri(Phi), arr.ind = TRUE)
  v <- abs(Phi[upper.tri(Phi)])
  dims <- rownames(Phi) %||% paste0("D", seq_len(nrow(Phi)))
  out <- data.frame(
    dimension_a = dims[ij[, 1]], dimension_b = dims[ij[, 2]],
    phi_estimado = round(v, 3),
    estado = ifelse(v > objetivo[2], "alta", ifelse(v < objetivo[1], "baja", "dentro")),
    stringsAsFactors = FALSE)
  out <- out[order(-out$phi_estimado), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "objetivo") <- objetivo
  out
}

# Una matriz de correlaciones estimada par a par puede no ser definida
# positiva (p. ej. tres dimensiones con .70 entre dos pares y .05 en el
# tercero). Se recortan los autovalores y se reescala a diagonal 1.
#' @keywords internal
#' @noRd
.phi_definida_positiva <- function(Phi, minimo = 0.05) {
  e <- eigen(Phi, symmetric = TRUE)
  if (min(e$values) >= minimo) return(Phi)
  M <- e$vectors %*% diag(pmax(e$values, minimo)) %*% t(e$vectors)
  d <- sqrt(diag(M)); M <- M / outer(d, d)
  dimnames(M) <- dimnames(Phi)
  attributes(M)[c("lectores")] <- attributes(Phi)[c("lectores")]
  M
}
