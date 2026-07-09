# =============================================================================
#  SeMiLLa · Deseabilidad social + simulacion de estructura ANTES de aplicar
# -----------------------------------------------------------------------------
#  Cierra el vacio "distinto en embeddings != separable empiricamente": SeMiLLa
#  valida SEMANTICA (embeddings), pero el colapso de factores ocurre en el
#  PROCESO DE RESPUESTA (deseabilidad social + aquiescencia). Estas dos funciones
#  predicen ese colapso sin recoger datos reales.
# =============================================================================

#' Calificar la deseabilidad social de los items (con LLM)
#'
#' Para cada item, un modelo de lenguaje estima cuan socialmente deseable es
#' endosarlo/estar de acuerdo (0 = indeseable admitirlo, 1 = muy deseable) en la
#' poblacion objetivo. Si TODAS las dimensiones son uniformemente deseables, la
#' escala sufrira halo de deseabilidad y las dimensiones tenderan a colapsar.
#'
#' Los items se barajan antes de formar los lotes (para que la deriva de escala
#' entre lotes no se confunda con las dimensiones) y se califican en
#' \code{n_pasadas} pasadas independientes cuyo promedio es la calificacion
#' final; la correlacion media entre pasadas se reporta como \code{estabilidad}.
#' Ademas del contraste ENTRE dimensiones (favorable a la separabilidad), se
#' vigila el contraste DENTRO de cada dimension (desfavorable: deseabilidad
#' mixta intra-factor genera varianza de metodo que rompe el propio factor).
#'
#' @param x Objeto \code{semilla} (o \code{semilla_items}) con \code{$items}
#'   (columnas \code{item}, \code{dimension}) y \code{$concepto}.
#' @param api_key Clave de OpenAI. Por defecto \code{Sys.getenv("OPENAI_API_KEY")}.
#' @param modelo Modelo de chat. Por defecto \code{"gpt-4.1-mini"}.
#' @param poblacion Descripcion de la poblacion (contexto para juzgar deseabilidad).
#' @param umbral_uniforme DE entre dimensiones por debajo de la cual se considera
#'   la deseabilidad "uniforme" (riesgo de halo). Por defecto 0.05,
#'   recalibrado en v2.7.0 con dos escalas de la misma bateria (n=280): PM
#'   colapso en campo con DE = .031 (Phi = .92) mientras que ACO se separo
#'   con DE = .059-.077 (Phi = .64-.84); el antiguo 0.10 marcaba como
#'   "uniforme" tambien a ACO (falsa alarma).
#' @param n_pasadas Numero de pasadas independientes del LLM que se promedian.
#'   Por defecto 2.
#' @param max_imputados Proporcion maxima de items sin calificar tolerada; por
#'   encima se aborta con error (evita diagnosticos optimistas tras un fallo de
#'   API). Los pocos items no calificados se imputan a 0.5 con warning.
#' @param seed Semilla opcional para el barajado de items (reproducibilidad del
#'   orden; la respuesta del LLM sigue siendo estocastica).
#' @param verbose Logico; imprime resumen y advertencia.
#' @return Lista con \code{deseabilidad} (vector 0-1 por item), \code{por_dimension},
#'   \code{sd_entre_dim}, \code{sd_intra_dim}, \code{alerta_intra},
#'   \code{estabilidad} (correlacion media entre pasadas), \code{n_imputados},
#'   \code{uniforme} (logico), \code{riesgo_halo} y \code{mensaje}.
#' @export
calificar_deseabilidad <- function(x, api_key = Sys.getenv("OPENAI_API_KEY"),
                                    modelo = "gpt-4.1-mini", poblacion = NULL,
                                    umbral_uniforme = 0.05, n_pasadas = 2,
                                    max_imputados = 0.25, seed = NULL,
                                    verbose = TRUE) {
  items <- x$items
  if (is.null(items) || is.null(items$item)) stop("x debe tener $items con columna 'item'.")
  concepto <- x$concepto %||% "constructo psicologico"
  # En objetos semilla $concepto es una lista anidada; al prompt va solo la
  # definicion (evita inyectar estructuras deparseadas tipo 'list(...)').
  if (is.list(concepto)) {
    concepto <- concepto$definicion %||%
      Filter(is.character, concepto)[[1]] %||% "constructo psicologico"
  }
  dims <- items$dimension %||% rep("General", nrow(items))
  txt  <- items$item
  p <- length(txt)
  if (is.null(poblacion) && !is.null(x$metadata$poblacion)) poblacion <- x$metadata$poblacion
  if (is.null(poblacion)) poblacion <- "poblacion general"
  # Colapsar a UN string (evita que paste0 genere un prompt vectorizado -> error 400)
  concepto  <- paste(concepto,  collapse = " ")
  poblacion <- paste(poblacion, collapse = " ")
  n_pasadas <- max(1L, as.integer(n_pasadas))

  .parse_des <- function(cont, k) {
    cont <- trimws(gsub("```json|```", "", cont))
    m <- regmatches(cont, regexpr("\\[[^]]*\\]", cont))   # array entre corchetes
    if (length(m) == 1) {
      v <- suppressWarnings(as.numeric(strsplit(gsub("[\\[\\] ]", "", m), ",")[[1]]))
      if (length(v) == k && !any(is.na(v))) return(v)
    }
    nums <- suppressWarnings(as.numeric(regmatches(cont, gregexpr("[0-9]*\\.?[0-9]+", cont))[[1]]))
    nums <- nums[!is.na(nums) & nums >= 0 & nums <= 1]
    if (length(nums) >= k) nums[seq_len(k)] else NULL
  }
  .calificar_chunk <- function(openai, idx) {
    lista <- paste(sprintf("%d. %s", seq_along(idx), txt[idx]), collapse = "\n")
    prompt <- paste0(
      "Eres experto en deseabilidad social.\nCONSTRUCTO: ", concepto, "\nPOBLACION: ", poblacion, "\n\n",
      "Para CADA item da un numero 0-1: cuan socialmente deseable es estar de acuerdo/endosarlo\n",
      "(0 = quedaria mal visto admitirlo, 0.5 = neutro, 1 = quedaria muy bien visto).\n",
      "DIFERENCIA de verdad entre items segun su contenido. Ejemplo de calibracion:\n",
      "'Ayudo a un companero'=0.9 ; 'Doy ordenes para que me obedezcan'=0.35 ; 'Busco emociones fuertes'=0.6.\n\n",
      "ITEMS:\n", lista, "\n\nResponde SOLO el JSON: {\"d\": [",
      paste(rep("x", length(idx)), collapse = ","), "]} con ", length(idx), " numeros 0-1.")
    for (intento in 1:2) {
      resp <- tryCatch(openai$chat$completions$create(
                model = modelo, messages = list(list(role = "user", content = prompt)),
                temperature = 0.3, max_tokens = 300L), error = function(e) NULL)
      v <- if (!is.null(resp)) .parse_des(resp$choices[[1]]$message$content, length(idx)) else NULL
      if (!is.null(v)) return(v)
    }
    NULL
  }

  # Calificar por LOTES pequenos con ejemplo-ancla (evita la respuesta perezosa
  # de "todo 0" que da el modelo cuando se le pasan 32 items de golpe). Los
  # items se BARAJAN por pasada: los lotes secuenciales coinciden con las
  # dimensiones (los items llegan agrupados) y cualquier deriva de escala entre
  # lotes sesgaria sd_entre_dim, que es justo el numero del diagnostico.
  if (!is.null(seed)) set.seed(seed)
  openai <- .configurar_openai(api_key)
  des_mat <- matrix(NA_real_, nrow = p, ncol = n_pasadas)
  for (pas in seq_len(n_pasadas)) {
    orden <- sample.int(p)
    chunks <- split(orden, ceiling(seq_along(orden) / 6))
    for (ch in chunks) {
      v <- .calificar_chunk(openai, ch)
      if (!is.null(v)) des_mat[ch, pas] <- v
    }
  }
  des <- rowMeans(des_mat, na.rm = TRUE)   # NaN si el item fallo en todas las pasadas

  # Fallo de API: abortar si quedo demasiado sin calificar (imputarlo todo a 0.5
  # apagaria el factor de deseabilidad y la simulacion posterior saldria rosada).
  sin_calificar <- !is.finite(des)
  if (mean(sin_calificar) > max_imputados) {
    stop(sprintf(paste0(
      "calificar_deseabilidad: %d de %d items sin calificar (fallo de API o de ",
      "parseo). Revisa la api_key/conexion; no se imputa para no producir un ",
      "diagnostico optimista."), sum(sin_calificar), p))
  }
  n_imputados <- sum(sin_calificar)
  if (n_imputados > 0) {
    warning(sprintf("%d item(s) no se pudieron calificar; se imputan a 0.5.", n_imputados))
    des[sin_calificar] <- 0.5
  }
  des <- pmin(1, pmax(0, des))

  # Estabilidad entre pasadas (correlacion media de a pares)
  estabilidad <- NA_real_
  if (n_pasadas > 1) {
    R <- suppressWarnings(stats::cor(des_mat, use = "pairwise.complete.obs"))
    off <- R[lower.tri(R)]
    if (length(off) && any(is.finite(off))) estabilidad <- mean(off, na.rm = TRUE)
  }

  por_dim  <- tapply(des, dims, mean)
  sd_entre <- stats::sd(por_dim)
  # Contraste DENTRO de cada dimension: deseabilidad mixta intra-factor crea
  # varianza de metodo con signos opuestos que rompe el propio factor.
  sd_intra <- mean(tapply(des, dims, stats::sd), na.rm = TRUE)
  alerta_intra <- is.finite(sd_intra) && sd_intra > 0.15
  uniforme <- sd_entre < umbral_uniforme
  riesgo_halo <- uniforme && mean(des) > 0.65
  msg <- if (riesgo_halo)
    "RIESGO ALTO de halo: deseabilidad uniforme y alta entre dimensiones -> las dimensiones pueden colapsar. Considera anclas de frecuencia, contenido contrastante ENTRE dimensiones o formato ipsativo."
  else if (uniforme) "Deseabilidad uniforme entre dimensiones (vigilar separabilidad)."
  else "Deseabilidad contrastante ENTRE dimensiones (favorable a la separabilidad)."
  if (alerta_intra) msg <- paste0(msg,
    " ADVERTENCIA: contraste de deseabilidad DENTRO de dimensiones (DE intra=",
    sprintf("%.2f", sd_intra),
    "): items deseables e indeseables mezclados en un mismo factor generan varianza de metodo que lo fragmenta.")

  if (verbose) {
    cat("== Deseabilidad social por dimension ==\n")
    print(round(por_dim, 2))
    cat(sprintf("Media=%.2f | DE entre dimensiones=%.3f | DE intra=%.3f | uniforme=%s\n",
                mean(des), sd_entre, sd_intra, uniforme))
    if (is.finite(estabilidad))
      cat(sprintf("Estabilidad entre %d pasadas del LLM: r=%.2f%s\n", n_pasadas, estabilidad,
                  if (estabilidad < 0.70) "  (BAJA: interpretar con cautela o subir n_pasadas)" else ""))
    cat(">> ", msg, "\n")
  }
  invisible(list(deseabilidad = des, por_dimension = por_dim, sd_entre_dim = sd_entre,
                 sd_intra_dim = sd_intra, alerta_intra = alerta_intra,
                 estabilidad = estabilidad, n_imputados = n_imputados,
                 uniforme = uniforme, riesgo_halo = riesgo_halo, mensaje = msg))
}


#' Simular la estructura factorial esperada ANTES de aplicar la escala
#'
#' Monte Carlo del PROCESO DE RESPUESTA (no de los embeddings). Cada respondiente
#' sintetico tiene: rasgo por dimension, un factor de DESEABILIDAD social cuya
#' carga en cada item es \code{2*deseabilidad-1} (deseable = +, indeseable = -),
#' aquiescencia, y una capa de DEPENDENCIA LOCAL para pares de items con similitud
#' de embedding alta. En cada replica se ajusta el CFA previsto y se resume el
#' ajuste y la correlacion interfactorial, estimando la probabilidad de que la
#' estructura se sostenga empiricamente.
#'
#' Como la fuerza real de la deseabilidad en la poblacion es desconocida,
#' \code{fuerza_deseabilidad} acepta un VECTOR y la simulacion se repite por
#' escenario (analisis de sensibilidad); el veredicto usa el escenario central
#' y \code{$sensibilidad} muestra el rango completo. Las replicas que no
#' convergen o producen soluciones inadmisibles (varianzas negativas, matrices
#' no definidas positivas) cuentan como estructura NO limpia: la no-convergencia
#' suele ser sintoma de mala estructura, no ruido ignorable. La variable latente
#' de respuesta se estandariza analiticamente para que los umbrales ordinales
#' operen en la metrica prevista; \code{$carga_estandarizada_media} reporta la
#' carga efectiva del rasgo tras sumar los componentes de metodo.
#'
#' @param x Objeto \code{semilla} (usa \code{$items$dimension} y \code{$similitud})
#'   o un vector de caracteres con la dimension de cada item. Con UNA sola
#'   dimension el criterio de estructura limpia se reduce al RMSEA (no hay
#'   correlacion interfactorial que evaluar).
#' @param deseabilidad Vector 0-1 por item. Si \code{x} es \code{semilla} y es
#'   \code{NULL}, se llama a \code{\link{calificar_deseabilidad}}.
#' @param similitud Matriz de similitud de embeddings (para la dependencia local).
#' @param carga_propia,phi_teorico,sd_aquiescencia Parametros del modelo
#'   generador de datos.
#' @param fuerza_deseabilidad Escalar o vector de fuerzas del factor de
#'   deseabilidad a simular. Por defecto \code{c(0.3, 0.6, 0.9)} (sensibilidad).
#' @param umbral_ld,fuerza_ld Umbral de similitud y fuerza de la dependencia local.
#' @param n,n_rep,k_cat Tamano muestral simulado, replicas por escenario y
#'   categorias ordinales. \code{n_rep} por defecto 100 (con 40 el semaforo
#'   cambia de color por ruido Monte Carlo de +-8 puntos).
#' @param umbral_rmsea,umbral_phi Criterios de "estructura limpia". El
#'   default de \code{umbral_phi} sube de 0.50 a 0.70 en v2.7.0: factores
#'   correlacionados .50-.70 son comunes y discriminables (el propio caso
#'   ACO funciono en campo con Phi = .64-.84 y Phi simulado = .58, que el
#'   criterio 0.50 marcaba como colapso); |Phi| >= .70 es la heuristica
#'   conservadora de discriminabilidad.
#' @param api_key Clave OpenAI (solo si hay que calificar deseabilidad).
#' @param seed,verbose Semilla y salida por consola.
#' @return Lista que incluye \code{phi_pares} (matriz |Phi| simulada POR PAR
#'   de dimensiones, escenario central) y \code{mapa_fusion} (grupos de
#'   dimensiones que se espera se FUNDAN en un factor empirico, con
#'   \code{k_esperado}; caso VP: 3 factores prosociales fundidos + Apertura
#'   separada), ademas de \code{prob_limpia} (escenario central), \code{prob_ic}
#'   (IC 95\% binomial), \code{prob_min}/\code{prob_max} (rango entre escenarios),
#'   \code{veredicto}, \code{fuerza_central}, medianas de RMSEA/|Phi|,
#'   \code{sensibilidad} (data.frame por escenario, con tasas de no-convergencia
#'   e inadmisibilidad), \code{carga_estandarizada_media} y la matriz
#'   \code{resumen} por replica del escenario central.
#' @export
simular_estructura <- function(x, deseabilidad = NULL, similitud = NULL,
                               carga_propia = 0.60, phi_teorico = 0.30,
                               fuerza_deseabilidad = c(0.3, 0.6, 0.9),
                               sd_aquiescencia = 0.35,
                               umbral_ld = 0.80, fuerza_ld = 1.4,
                               n = 300, n_rep = 100, k_cat = 5,
                               umbral_rmsea = 0.06, umbral_phi = 0.70,
                               api_key = Sys.getenv("OPENAI_API_KEY"),
                               seed = 2026, verbose = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Necesitas el paquete 'lavaan'.")
  if (!requireNamespace("MASS",  quietly = TRUE)) stop("Necesitas el paquete 'MASS'.")
  if (inherits(x, "semilla") || (is.list(x) && !is.null(x$items))) {
    dim_por_item <- x$items$dimension
    if (is.null(similitud) && !is.null(x$similitud)) similitud <- x$similitud
    if (is.null(deseabilidad)) deseabilidad <- calificar_deseabilidad(x, api_key = api_key,
                                                                      verbose = verbose)$deseabilidad
  } else dim_por_item <- x
  if (is.null(deseabilidad) || length(deseabilidad) != length(dim_por_item))
    stop("deseabilidad debe tener un valor por item.")

  dims <- unique(dim_por_item); K <- length(dims); p <- length(dim_por_item)
  memb <- match(dim_por_item, dims)
  cargas <- if (length(carga_propia) == 1) rep(carga_propia, p) else carga_propia
  dir_j  <- 2 * deseabilidad - 1
  int_j  <- 0.5 * dir_j
  taus   <- stats::qnorm(seq(1, k_cat - 1) / k_cat)
  Phi    <- matrix(phi_teorico, K, K); diag(Phi) <- 1
  its    <- sprintf("i%02d", seq_len(p))
  syn    <- paste(sapply(seq_len(K), function(kk)
             paste0("F", kk, " =~ ", paste(its[memb == kk], collapse = " + "))), collapse = "\n")
  sd_e   <- sqrt(pmax(0.20, 1 - cargas^2))

  pares_ld <- list()
  if (!is.null(similitud)) {
    S <- as.matrix(similitud)
    for (a in 1:(p - 1)) for (b in (a + 1):p) if (S[a, b] > umbral_ld)
      pares_ld[[length(pares_ld) + 1]] <- list(a = a, b = b,
        g = sqrt(pmin(0.9, fuerza_ld * (S[a, b] - umbral_ld) / (1 - umbral_ld))))
  }
  g2_j <- rep(0, p)
  for (pl in pares_ld) { g2_j[pl$a] <- g2_j[pl$a] + pl$g^2
                         g2_j[pl$b] <- g2_j[pl$b] + pl$g^2 }

  # ---- Un escenario (una fuerza de deseabilidad), n_rep replicas ----
  sim_escenario <- function(f_des, seed_esc) {
    set.seed(seed_esc)
    dirf <- f_des * dir_j
    # Varianza analitica del indice latente -> estandarizar para que los
    # umbrales ordinales (taus) operen en la metrica prevista.
    sd_tot <- sqrt(cargas^2 + dirf^2 + sd_aquiescencia^2 + g2_j + sd_e^2)
    # Vector de |Phi| por PAR de factores (para el mapa de fusion): permite
    # anticipar QUE dimensiones se fundiran, no solo si "algo" colapsa
    # (caso VP: 3 factores prosociales se funden y Apertura se separa).
    n_par <- if (K > 1) K * (K - 1) / 2 else 0
    nom_par <- if (n_par > 0) {
      comb <- utils::combn(K, 2)
      paste0("phipar_", comb[1, ], "_", comb[2, ])
    } else character(0)
    one_rep <- function() {
      theta <- MASS::mvrnorm(n, rep(0, K), Phi)
      delta <- stats::rnorm(n); alpha <- stats::rnorm(n, 0, sd_aquiescencia)
      ld <- matrix(0, n, p)
      for (pl in pares_ld) { s <- stats::rnorm(n); ld[, pl$a] <- ld[, pl$a] + pl$g * s
                                                   ld[, pl$b] <- ld[, pl$b] + pl$g * s }
      Y <- sapply(seq_len(p), function(j)
        findInterval((int_j[j] + cargas[j] * theta[, memb[j]] + dirf[j] * delta +
                      alpha + ld[, j] + stats::rnorm(n, 0, sd_e[j])) / sd_tot[j],
                     taus) + 1L)
      D <- as.data.frame(Y); names(D) <- its
      fit <- tryCatch(lavaan::cfa(syn, data = D, ordered = its, estimator = "WLSMV",
                                  std.lv = TRUE), error = function(e) NULL)
      conv <- !is.null(fit) && isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"),
                                               error = function(e) FALSE))
      base_na <- c(cfi = NA, rmsea = NA, srmr = NA, phi = NA, conv = 0, adm = 0)
      if (n_par > 0) base_na <- c(base_na, stats::setNames(rep(NA_real_, n_par), nom_par))
      if (!conv) return(base_na)
      # Solucion inadmisible (Heywood, matrices no PD) = evidencia de mala
      # estructura, no una replica valida.
      adm <- isTRUE(tryCatch(suppressWarnings(lavaan::lavInspect(fit, "post.check")),
                             error = function(e) FALSE))
      fm <- lavaan::fitMeasures(fit, c("cfi.scaled", "rmsea.scaled", "srmr"))
      phi <- NA_real_; phi_par <- numeric(0)
      if (K > 1) {
        R <- lavaan::lavInspect(fit, "cor.lv")
        phi <- mean(abs(R[lower.tri(R)]))
        phi_par <- stats::setNames(abs(R[lower.tri(R)]), nom_par)
      }
      out <- c(cfi = as.numeric(fm[1]), rmsea = as.numeric(fm[2]), srmr = as.numeric(fm[3]),
               phi = phi, conv = 1, adm = as.numeric(adm))
      if (n_par > 0) out <- c(out, phi_par)
      out
    }
    M <- t(replicate(n_rep, one_rep()))
    conv <- M[, "conv"] == 1
    limpia <- conv & M[, "adm"] == 1 & M[, "rmsea"] <= umbral_rmsea
    if (K > 1) limpia <- limpia & M[, "phi"] < umbral_phi
    limpia[is.na(limpia)] <- FALSE           # no convergida/inadmisible = NO limpia
    list(prob = mean(limpia),
         rmsea_med = stats::median(M[conv, "rmsea"], na.rm = TRUE),
         phi_med   = if (K > 1) stats::median(M[conv, "phi"], na.rm = TRUE) else NA_real_,
         phi_pares_med = if (n_par > 0)
           apply(M[conv, nom_par, drop = FALSE], 2, stats::median, na.rm = TRUE)
         else NULL,
         tasa_no_conv = mean(!conv),
         tasa_inadmisible = mean(conv & M[, "adm"] == 0),
         carga_std = mean(cargas / sd_tot), M = M)
  }

  fuerzas <- sort(unique(as.numeric(fuerza_deseabilidad)))
  esc <- lapply(seq_along(fuerzas), function(i) sim_escenario(fuerzas[i], seed + (i - 1) * 1000L))
  sensibilidad <- data.frame(
    fuerza_deseabilidad = fuerzas,
    prob_limpia      = vapply(esc, `[[`, numeric(1), "prob"),
    rmsea_med        = vapply(esc, `[[`, numeric(1), "rmsea_med"),
    phi_med          = vapply(esc, `[[`, numeric(1), "phi_med"),
    tasa_no_conv     = vapply(esc, `[[`, numeric(1), "tasa_no_conv"),
    tasa_inadmisible = vapply(esc, `[[`, numeric(1), "tasa_inadmisible"))

  i_c  <- ceiling(length(fuerzas) / 2)       # escenario central
  prob <- esc[[i_c]]$prob
  ic   <- prob + c(-1, 1) * 1.96 * sqrt(prob * (1 - prob) / n_rep)
  ic   <- pmin(1, pmax(0, ic))
  veredicto <- if (prob >= .80) "ALTA (aplicar con confianza)"
               else if (prob >= .40) "MEDIA (revisar antes de aplicar)"
               else "BAJA (las dimensiones colapsaran; redisenar items/formato)"

  # Matriz |Phi| simulada POR PAR de dimensiones (escenario central) y mapa
  # de fusion: que dimensiones se fundiran entre si y cuales sobreviviran.
  # El mapa integra DOS rutas: (a) Phi simulado >= umbral, y (b) "halo
  # local": ambas dimensiones con deseabilidad alta (>= .80) y pareja
  # (dif < .10). La ruta (b) es necesaria porque el motor de simulacion es
  # conservador en el nivel absoluto de la fusion (validado con 3 escalas
  # reales n=280: PM fusion real .92 / simulada .71; VP fusion real .89 /
  # simulada .63; el patron DIRECCIONAL coincide pero el nivel queda corto).
  phi_pares <- NULL
  mapa_fusion <- NULL
  if (K > 1 && !is.null(esc[[i_c]]$phi_pares_med)) {
    phi_pares <- matrix(NA_real_, K, K, dimnames = list(dims, dims))
    diag(phi_pares) <- 1
    comb <- utils::combn(K, 2)
    for (q in seq_len(ncol(comb))) {
      phi_pares[comb[1, q], comb[2, q]] <-
        phi_pares[comb[2, q], comb[1, q]] <- esc[[i_c]]$phi_pares_med[q]
    }
    des_dim <- tapply(deseabilidad, dim_por_item, mean)[dims]
    mapa_fusion <- .mapa_fusion(phi_pares, umbral = umbral_phi,
                                des_por_dim = des_dim)
  }

  if (verbose) {
    cat(sprintf("Dimensiones=%d | items=%d | n=%d | reps/escenario=%d | deseab. media=%.2f (DE=%.2f) | pares dep.local=%d\n",
                K, p, n, n_rep, mean(deseabilidad), stats::sd(deseabilidad), length(pares_ld)))
    cat(sprintf("Carga estandarizada efectiva del rasgo (escenario central): %.2f\n",
                esc[[i_c]]$carga_std))
    cat("-- Sensibilidad a la fuerza de deseabilidad (parametro supuesto):\n")
    for (i in seq_along(fuerzas))
      cat(sprintf("   fuerza=%.2f -> prob limpia=%3.0f%% | RMSEAmed=%.3f | |Phi|med=%s | no-conv=%.0f%% | inadmis=%.0f%%\n",
                  fuerzas[i], 100 * sensibilidad$prob_limpia[i], sensibilidad$rmsea_med[i],
                  ifelse(is.na(sensibilidad$phi_med[i]), "-", sprintf("%.2f", sensibilidad$phi_med[i])),
                  100 * sensibilidad$tasa_no_conv[i], 100 * sensibilidad$tasa_inadmisible[i]))
    cat(sprintf(">> PROBABILIDAD de estructura limpia (escenario central, fuerza=%.2f): %.0f%% [IC95: %.0f-%.0f%%]\n",
                fuerzas[i_c], 100 * prob, 100 * ic[1], 100 * ic[2]))
    if (length(fuerzas) > 1)
      cat(sprintf(">> RANGO entre escenarios: %.0f%% - %.0f%% (la fuerza real es desconocida; interpretar el rango, no un numero unico)\n",
                  100 * min(sensibilidad$prob_limpia), 100 * max(sensibilidad$prob_limpia)))
    if (!is.null(mapa_fusion) && mapa_fusion$k_esperado < K) {
      cat(">> MAPA DE FUSION (phi simulado >= ", umbral_phi,
          " o halo local): se esperan ",
          mapa_fusion$k_esperado, " factor(es) empirico(s), no ", K, ":\n", sep = "")
      for (gi in seq_along(mapa_fusion$grupos)) {
        g <- mapa_fusion$grupos[[gi]]
        etiqueta <- if (length(g) > 1) {
          paste0("[SE FUNDEN por ", mapa_fusion$causas[gi], "] ")
        } else "[se separa] "
        cat("   ", etiqueta, paste(g, collapse = " + "), "\n", sep = "")
      }
    }
    cat(sprintf(">> VEREDICTO: %s\n", veredicto))
  }
  invisible(list(prob_limpia = prob, prob_ic = ic,
                 prob_min = min(sensibilidad$prob_limpia),
                 prob_max = max(sensibilidad$prob_limpia),
                 veredicto = veredicto, fuerza_central = fuerzas[i_c],
                 rmsea_med = esc[[i_c]]$rmsea_med, phi_med = esc[[i_c]]$phi_med,
                 phi_pares = phi_pares, mapa_fusion = mapa_fusion,
                 sensibilidad = sensibilidad,
                 carga_estandarizada_media = esc[[i_c]]$carga_std,
                 resumen = esc[[i_c]]$M))
}


# Componentes conexos del grafo de dimensiones que se espera se FUNDAN.
# Dos rutas de union entre dimensiones i-j:
#   (a) |Phi| simulado >= umbral (la simulacion misma anticipa la fusion), o
#   (b) "halo local": ambas con deseabilidad >= umbral_halo y diferencia
#       < tol_halo — dimensiones igualmente deseables comparten el factor
#       de respuesta socialmente correcta y en campo se funden aunque el
#       motor de simulacion no alcance el nivel absoluto.
# Calibrado con 3 escalas reales (n=280): PM Int+Sim (real .92) via (b);
# ACO Cog+Con (real .84, "casi colineales") via (b); VP AT+CO (real .89)
# via (b) con Apertura (.73) y Autopromocion (.62) fuera del bloque.

#' @keywords internal
.mapa_fusion <- function(phi_pares, umbral = 0.70, des_por_dim = NULL,
                         umbral_halo = 0.80, tol_halo = 0.10) {
  K <- nrow(phi_pares)
  dims <- rownames(phi_pares)
  des <- if (!is.null(des_por_dim)) {
    d <- as.numeric(des_por_dim)
    names(d) <- names(des_por_dim)
    d[dims]
  } else NULL
  padre <- seq_len(K)
  raiz <- function(i) { while (padre[i] != i) i <- padre[i]; i }
  causa <- matrix("", K, K)
  for (i in 1:(K - 1)) for (j in (i + 1):K) {
    une_phi <- !is.na(phi_pares[i, j]) && phi_pares[i, j] >= umbral
    une_halo <- !is.null(des) && !is.na(des[i]) && !is.na(des[j]) &&
      des[i] >= umbral_halo && des[j] >= umbral_halo &&
      abs(des[i] - des[j]) < tol_halo
    if (une_phi || une_halo) {
      padre[raiz(j)] <- raiz(i)
      causa[i, j] <- causa[j, i] <-
        paste(c(if (une_phi) "phi", if (une_halo) "halo"), collapse = "+")
    }
  }
  comp <- vapply(seq_len(K), raiz, integer(1))
  grupos <- lapply(unique(comp), function(r) dims[comp == r])
  # Ordenar: primero los bloques fundidos (mas de una dimension)
  ord <- order(-vapply(grupos, length, integer(1)))
  grupos <- grupos[ord]
  causas <- vapply(grupos, function(g) {
    if (length(g) < 2) return("")
    ii <- match(g, dims)
    cs <- unique(as.vector(causa[ii, ii]))
    paste(cs[nzchar(cs)], collapse = "/")
  }, character(1))
  list(grupos = grupos,
       causas = causas,
       k_esperado = length(grupos),
       umbral = umbral,
       hay_fusion = any(vapply(grupos, length, integer(1)) > 1))
}
