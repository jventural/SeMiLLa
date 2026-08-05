# =============================================================================
#  SeMiLLa · PRUEBA DE ESTRES DE ESCALA (estres_escala) + motor paralelo
# -----------------------------------------------------------------------------
#  Extiende la compuerta pre-aplicacion: en vez de simular solo deseabilidad y
#  aquiescencia con dosis fijas, somete la escala a una BATERIA de sesgos de
#  respuesta con dosis crecientes (curva dosis-respuesta), detecta el PUNTO DE
#  QUIEBRE de cada sesgo (dosis donde la estructura deja de sostenerse) y
#  calcula un INDICE DE FRAGILIDAD global y por item (que items colapsan
#  primero). Pensado para correr en la PC del usuario: multinucleo con streams
#  RNG L'Ecuyer POR REPLICA, de modo que el resultado es IDENTICO con 1 o con
#  N nucleos dado el mismo seed (paridad app Shiny <-> R local).
# =============================================================================

# ---- Infraestructura de replicas reproducibles (secuencial == paralelo) -----

# n streams L'Ecuyer-CMRG independientes derivados de un seed, sin perturbar
# de forma permanente el RNG global del usuario.
#' @keywords internal
.semillas_lecuyer <- function(seed, n) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  old_kind <- RNGkind()[1]
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  s <- get(".Random.seed", envir = .GlobalEnv)
  out <- vector("list", n)
  for (i in seq_len(n)) { out[[i]] <- s; s <- parallel::nextRNGStream(s) }
  RNGkind(old_kind)
  if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  out
}

# Worker de UNA replica: fija su stream RNG y ejecuta. Es funcion de paquete
# (su environment es el namespace) para que al serializarla hacia los nodos
# PSOCK no arrastre el frame de la llamada (que contiene el cluster mismo).
#' @keywords internal
.rep_worker <- function(s, fn, par) {
  assign(".Random.seed", s, envir = .GlobalEnv)
  fn(par)
}

# Ejecuta las replicas (una por stream) en secuencial o en paralelo y apila.
# El codigo de cada replica es EL MISMO en ambas rutas -> mismos numeros.
#' @keywords internal
.ejecutar_reps <- function(seeds, fn, par, cl = NULL) {
  res <- if (is.null(cl)) lapply(seeds, .rep_worker, fn = fn, par = par)
         else parallel::parLapplyLB(cl, seeds, .rep_worker, fn = fn, par = par)
  do.call(rbind, res)
}

# Barra de progreso de texto: |########······| 62%
#' @keywords internal
.barra_txt <- function(frac, ancho = 24L) {
  frac <- max(0, min(1, frac))
  lleno <- as.integer(round(frac * ancho))
  paste0("|", strrep("#", lleno), strrep("·", ancho - lleno), "| ",
         sprintf("%3.0f%%", 100 * frac))
}

# Duracion en m:ss (para ETA y tiempo transcurrido).
#' @keywords internal
.mmss <- function(seg) {
  if (!is.finite(seg) || seg < 0) return(" --:--")
  sprintf("%3d:%02d", as.integer(seg) %/% 60L, as.integer(seg) %% 60L)
}

# ETA en minutos legible: evita el "~0 min" de las corridas cortas.
#' @keywords internal
.eta_txt <- function(min) {
  if (!is.finite(min)) return("desconocida")
  if (min < 1) "menos de 1 min" else sprintf("~%s min", format(round(min, 1)))
}

# Igual que .ejecutar_reps, pero por LOTES de replicas, dibujando una barra de
# progreso con ETA en la misma linea (\r). Cada replica conserva su propio
# stream L'Ecuyer y se apila en el orden original -> el resultado es IDENTICO
# al de .ejecutar_reps (el troceado solo afecta a lo que se imprime).
# El lote nunca baja de n_nucleos replicas, para no perder el balanceo de carga.
#' @keywords internal
.ejecutar_reps_prog <- function(seeds, fn, par, cl = NULL, verbose = TRUE,
                                etiqueta = "", n_nucleos = 1L,
                                max_actualizaciones = 20L,
                                una_linea = interactive()) {
  n <- length(seeds)
  if (!verbose || n < 2L) return(.ejecutar_reps(seeds, fn, par, cl))
  tam <- max(as.integer(n_nucleos %||% 1L),
             as.integer(ceiling(n / max_actualizaciones)), 1L)
  lotes <- split(seq_len(n), ceiling(seq_len(n) / tam))
  # En consola interactiva la barra se redibuja sobre si misma (\r); cuando la
  # salida se lee linea por linea (Rscript, callr, log de la app) cada avance
  # va en su propia linea, que si no nunca se veria hasta el salto final.
  ini <- if (una_linea) "\r" else ""
  fin <- if (una_linea) "" else "\n"
  t0 <- Sys.time()
  res <- vector("list", length(lotes))
  hechas <- 0L
  for (b in seq_along(lotes)) {
    idx <- lotes[[b]]
    res[[b]] <- .ejecutar_reps(seeds[idx], fn, par, cl)
    hechas <- hechas + length(idx)
    if (hechas == n) break            # el 100% lo imprime la linea de cierre
    tr <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta <- tr / hechas * (n - hechas)
    cat(sprintf("%s %-24s %s (%d/%d reps) ETA %s%s", ini, etiqueta,
                .barra_txt(hechas / n), hechas, n, .mmss(eta), fin))
    utils::flush.console()
  }
  tr <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("%s %-24s %s (%d/%d reps) en %s\n", ini, etiqueta,
              .barra_txt(1), n, n, .mmss(tr)))
  utils::flush.console()
  do.call(rbind, res)
}

# Cluster PSOCK con lavaan/MASS precargados. NULL si n_nucleos <= 1.
#' @keywords internal
.cluster_estres <- function(n_nucleos, verbose = TRUE) {
  if (is.null(n_nucleos) || n_nucleos <= 1) return(NULL)
  cl <- parallel::makeCluster(n_nucleos)
  invisible(parallel::clusterEvalQ(cl, {
    suppressMessages({ requireNamespace("lavaan"); requireNamespace("MASS") })
  }))
  cl
}

# ---- Aviso accionable cuando un canal LLM no pudo correr --------------------
#  El aviso viejo era una linea ("¿API?") que no distinguia las tres causas
#  posibles ni decia DONDE se pone la clave. Aqui se diagnostica la causa
#  (sin clave / entorno Python ausente / la llamada fallo) y se imprimen las
#  tres formas validas de proveer la clave, con la funcion que la recibe.
#' @keywords internal
.aviso_canal_llm <- function(que, fn = "estres_escala", api_key = "",
                             err = NULL, consecuencia = NULL,
                             como_apagar = NULL) {
  det <- if (is.null(err)) "" else conditionMessage(err)
  linea <- function(...) cat(" ", ..., "\n", sep = "")
  # Clasificacion POR PRIORIDAD: el mensaje que devuelve reticulate menciona
  # "openai" y "reticulate" incluso cuando el fallo real es un 401, asi que las
  # firmas de credencial, cuota y red se evaluan ANTES que las de entorno.
  causa <- if (!nzchar(api_key %||% "")) "sin_clave"
    else if (grepl("401|Authentication|invalid_api_key|Incorrect API key|no API key",
                   det, ignore.case = TRUE)) "clave_mala"
    else if (grepl("429|insufficient_quota|quota|billing|rate.?limit",
                   det, ignore.case = TRUE)) "sin_saldo"
    else if (grepl("ModuleNotFoundError|No se pudo importar openai|Python no disponible|pip install|py_install|instalar reticulate",
                   det, ignore.case = TRUE)) "sin_python"
    else if (grepl("Connection|timeout|timed out|SSL|proxy|getaddrinfo|Could not resolve|APIConnection",
                   det, ignore.case = TRUE)) "sin_red"
    else if (nzchar(det)) "otro" else "sin_resultado"
  # El detalle de reticulate trae varias lineas; se recorta a lo informativo.
  det_corto <- if (nzchar(det))
    substr(gsub("[\r\n]+", " ", det), 1, 220) else ""

  cat("\n -------------------------------------------------------------\n")
  linea(toupper(que), ": DESACTIVADO -> se omite")
  switch(causa,
    sin_clave = linea("  Causa: no se encontro ninguna clave de OpenAI",
                      " (api_key vacia y OPENAI_API_KEY sin definir)."),
    clave_mala = {
      linea("  Causa: la clave SI llego, pero OpenAI la rechazo (401).")
      linea("    Revisa que este completa, sin espacios ni comillas de mas,")
      linea("    y que no haya sido revocada: platform.openai.com/api-keys")
    },
    sin_saldo = {
      linea("  Causa: la clave es valida pero la cuenta no tiene saldo o")
      linea("  excedio su limite (429). Revisa platform.openai.com/usage.")
    },
    sin_python = {
      linea("  Causa: la clave SI esta, pero falta el entorno Python que usa")
      linea("  el juez (reticulate + el paquete 'openai').")
      linea("    Solucion: install.packages(\"reticulate\"); ",
            "reticulate::py_install(\"openai\")")
    },
    sin_red = linea("  Causa: no hubo conexion con la API (internet, firewall",
                    " o proxy corporativo)."),
    otro = linea("  Causa: la clave SI esta, pero la consulta al LLM fallo."),
    sin_resultado = linea("  Causa: el canal no devolvio resultado utilizable.")
  )
  if (nzchar(det_corto)) linea("    Detalle: ", det_corto)
  if (causa %in% c("sin_clave", "clave_mala", "otro", "sin_resultado")) {
    linea("  DONDE PONER LA CLAVE (elige UNA opcion):")
    linea("   1. En la llamada:  ", fn, "(..., api_key = \"sk-...\")")
    linea("   2. En esta sesion: Sys.setenv(OPENAI_API_KEY = \"sk-...\")")
    linea("   3. Permanente:     usethis::edit_r_environ() y agrega la linea")
    linea("                      OPENAI_API_KEY=sk-...  (guarda y reinicia R)")
    linea("   Obtener una clave: https://platform.openai.com/api-keys")
  }
  if (!is.null(consecuencia)) linea("  ", consecuencia)
  if (!is.null(como_apagar))
    linea("  Para no volver a ver este aviso: ", como_apagar)
  cat(" -------------------------------------------------------------\n")
  utils::flush.console()
  invisible(NULL)
}

# ---- Cargas informadas por semantica (v2.9.0) -------------------------------
#  El ORDEN de las cargas se toma de la cohesion del item con su dimension
#  (similitud media de embeddings con los demas items de su dimension) y se
#  reescala a un rango plausible [0.45, 0.75]. La semantica es un proxy DEBIL
#  de las cargas empiricas (cor similitud-policorica = .21 en la calibracion
#  PM n=280): por eso se conserva solo el ORDEN (no el nivel), se acota el
#  rango, y el default del motor sigue siendo la carga uniforme. Si la
#  afinidad con OTRA dimension alcanza el 90% de la propia, se inyecta una
#  carga cruzada pequena (0.20) en el proceso generador: el riesgo de carga
#  cruzada queda visible en la simulacion aunque el AFC previsto no la modele.
#' @keywords internal
.lambda_semantica <- function(similitud, memb, K,
                              # 2.9.13: rango recalibrado contra 246 items de 8
                              # escalas con AFC empirico (n=1500). La carga
                              # estandarizada real tiene media .695 y p05-p95 de
                              # .48 a .85; el rango anterior (.45-.75) estaba
                              # desplazado hacia abajo y hacia que los factores
                              # se volvieran indistinguibles en cuanto el phi
                              # subia -- justo lo que hacia fallar al oraculo.
                              rango = c(0.48, 0.85),
                              umbral_cruce = 0.90,
                              carga_cruzada = 0.20) {
  S <- as.matrix(similitud)
  p <- length(memb)
  A <- matrix(NA_real_, p, K)      # afinidad media item x dimension
  for (j in seq_len(p)) for (k in seq_len(K)) {
    idx <- setdiff(which(memb == k), j)
    A[j, k] <- if (length(idx)) mean(S[j, idx]) else NA_real_
  }
  a_prop <- A[cbind(seq_len(p), memb)]
  rg <- range(a_prop, na.rm = TRUE)
  lam <- if (!all(is.finite(rg)) || diff(rg) < 1e-8) rep(mean(rango), p) else
    rango[1] + (rango[2] - rango[1]) * (a_prop - rg[1]) / diff(rg)
  LAMBDA <- matrix(0, p, K)
  LAMBDA[cbind(seq_len(p), memb)] <- lam
  n_cruces <- 0L
  if (K > 1) for (j in seq_len(p)) for (k in seq_len(K)) {
    if (k != memb[j] && is.finite(A[j, k]) && is.finite(a_prop[j]) &&
        a_prop[j] > 0 && A[j, k] / a_prop[j] >= umbral_cruce) {
      LAMBDA[j, k] <- carga_cruzada
      n_cruces <- n_cruces + 1L
    }
  }
  list(LAMBDA = LAMBDA, lambda_propia = lam, afinidad = A, n_cruces = n_cruces)
}

# Construye la matriz de cargas del generador segun carga_propia: numerica
# (uniforme o vector, sin cruces) o "semantica" (ver .lambda_semantica).
#' @keywords internal
.resolver_cargas <- function(carga_propia, similitud, memb, K, p, verbose = TRUE) {
  if (identical(carga_propia, "semantica")) {
    if (is.null(similitud))
      stop("carga_propia = 'semantica' requiere la matriz de similitud (embeddings).")
    ls <- .lambda_semantica(similitud, memb, K)
    if (verbose)
      cat(sprintf("Cargas semanticas: rango %.2f-%.2f (orden por cohesion con su dimension) | cargas cruzadas inyectadas: %d\n",
                  min(ls$lambda_propia), max(ls$lambda_propia), ls$n_cruces))
    list(LAMBDA = ls$LAMBDA, lambda_main = ls$lambda_propia,
         n_cruces = ls$n_cruces)
  } else {
    lam <- if (length(carga_propia) == 1) rep(as.numeric(carga_propia), p)
           else as.numeric(carga_propia)
    LAMBDA <- matrix(0, p, K)
    LAMBDA[cbind(seq_len(p), memb)] <- lam
    list(LAMBDA = LAMBDA, lambda_main = lam, n_cruces = 0L)
  }
}

# ---- Una replica del proceso de respuesta bajo UN sesgo y UNA dosis --------
#  par: lista con el modelo generador (ver .par_estres) + sesgo/dosis activos.
#  Devuelve ajuste global + cargas estandarizadas por item (para fragilidad).
#' @keywords internal
.estres_rep <- function(par) {
  n <- par$n; p <- par$p; K <- par$K; k_cat <- par$k_cat
  theta <- MASS::mvrnorm(n, rep(0, K), par$Phi)
  delta <- stats::rnorm(n)
  alpha <- if (par$sd_aq > 0) stats::rnorm(n, 0, par$sd_aq) else numeric(n)
  ld <- matrix(0, n, p)
  for (pl in par$pares_ld) {
    s <- stats::rnorm(n)
    ld[, pl$a] <- ld[, pl$a] + pl$g * s
    ld[, pl$b] <- ld[, pl$b] + pl$g * s
  }
  # Parte de rasgo: theta %*% t(LAMBDA) admite cargas cruzadas (v2.9.0);
  # con LAMBDA diagonal-por-dimension equivale al producto carga x rasgo.
  TH <- theta %*% t(par$LAMBDA)
  eta <- vapply(seq_len(p), function(j)
    (par$int_j[j] + TH[, j] + par$dirf[j] * delta +
     alpha + ld[, j] + stats::rnorm(n, 0, par$sd_e[j])) / par$sd_tot[j],
    numeric(n))

  # Estilos de respuesta por PERSONA: una proporcion 'prop' de respondientes
  # sesgados. extremos = umbrales comprimidos (respuestas 1/k); punto_medio =
  # umbrales expandidos (todo al centro); descuido = respuestas uniformes al
  # azar; lineas_rectas = repite una misma categoria en todos los items.
  idx_b <- if (par$prop > 0) sample.int(n, round(par$prop * n)) else integer(0)
  tf <- rep(1, n)
  if (par$sesgo == "extremos")    tf[idx_b] <- 0.35
  if (par$sesgo == "punto_medio") tf[idx_b] <- 2.50
  Y <- matrix(0L, n, p)
  for (tfv in unique(tf)) {
    filas <- which(tf == tfv)
    Y[filas, ] <- matrix(findInterval(eta[filas, , drop = FALSE], par$taus * tfv),
                         length(filas), p) + 1L
  }
  if (par$sesgo == "descuido" && length(idx_b))
    Y[idx_b, ] <- matrix(sample.int(k_cat, length(idx_b) * p, replace = TRUE),
                         length(idx_b), p)
  if (par$sesgo == "lineas_rectas" && length(idx_b))
    Y[idx_b, ] <- matrix(rep(sample.int(k_cat, length(idx_b), replace = TRUE), p),
                         length(idx_b), p)

  D <- as.data.frame(Y); names(D) <- par$its
  fit <- tryCatch(lavaan::cfa(par$syn, data = D, ordered = par$its,
                              estimator = "WLSMV", std.lv = TRUE),
                  error = function(e) NULL)
  conv <- !is.null(fit) && isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"),
                                           error = function(e) FALSE))
  base <- c(cfi = NA, rmsea = NA, srmr = NA, phi = NA, conv = 0, adm = 0,
            stats::setNames(rep(NA_real_, p), paste0("cg_", par$its)))
  if (!conv) return(base)
  adm <- isTRUE(tryCatch(suppressWarnings(lavaan::lavInspect(fit, "post.check")),
                         error = function(e) FALSE))
  fm <- lavaan::fitMeasures(fit, c("cfi.scaled", "rmsea.scaled", "srmr"))
  phi <- NA_real_
  if (K > 1) {
    R <- lavaan::lavInspect(fit, "cor.lv")
    phi <- mean(abs(R[lower.tri(R)]))
  }
  LAM <- tryCatch(lavaan::lavInspect(fit, "std")$lambda, error = function(e) NULL)
  cg <- if (!is.null(LAM)) rowSums(LAM) else rep(NA_real_, p)
  c(cfi = as.numeric(fm[1]), rmsea = as.numeric(fm[2]), srmr = as.numeric(fm[3]),
    phi = phi, conv = 1, adm = as.numeric(adm),
    stats::setNames(as.numeric(cg), paste0("cg_", par$its)))
}

# Intervalo de Wilson para la proporcion de replicas limpias (v2.9.5). Se usa
# en lugar del Wald porque la probabilidad cae con frecuencia cerca de 0 o 1,
# donde Wald colapsa a ancho nulo o se sale de [0, 1] (es el IC declarado en
# el articulo del stress test).
#' @keywords internal
.ic_wilson <- function(pr, n, z = 1.96) {
  den    <- 1 + z^2 / n
  centro <- (pr + z^2 / (2 * n)) / den
  mitad  <- z * sqrt(pr * (1 - pr) / n + z^2 / (4 * n^2)) / den
  c(max(0, centro - mitad), min(1, centro + mitad))
}

# Punto de quiebre por interpolacion lineal: primera dosis (el grid incluye la
# linea base en dosis 0) donde prob cae por debajo del umbral. NA = no quiebra
# dentro del rango probado; 0 = ya quebrada sin sesgo.
#' @keywords internal
.dosis_quiebre <- function(dosis, prob, umbral) {
  bajo <- which(prob < umbral)
  if (!length(bajo)) return(NA_real_)
  i <- bajo[1]
  if (i == 1) return(0)
  d0 <- dosis[i - 1]; d1 <- dosis[i]; p0 <- prob[i - 1]; p1 <- prob[i]
  if (!is.finite(p0) || !is.finite(p1) || p0 == p1) return(d1)
  d0 + (p0 - umbral) / (p0 - p1) * (d1 - d0)
}

#' Prueba de estres de una escala: sesgos de respuesta con dosis crecientes
#'
#' Somete la escala a una bateria de SESGOS DE RESPUESTA simulados con dosis
#' crecientes y estima, para cada sesgo, la curva dosis-respuesta de la
#' probabilidad de estructura limpia (mismo motor Monte Carlo y criterios que
#' \code{\link{simular_estructura}}): a que dosis se QUIEBRA la estructura
#' (punto de quiebre), cuan FRAGIL es la escala ante cada sesgo (perdida media
#' de probabilidad respecto de la linea base) y QUE ITEMS COLAPSAN PRIMERO
#' (caida de la carga estandarizada respecto de la linea base, a la dosis
#' maxima de cada sesgo).
#'
#' Sesgos disponibles y unidad de su dosis:
#' \itemize{
#'   \item \code{aquiescencia}: DE del factor de aquiescencia (dosis 0.2-0.8).
#'   \item \code{deseabilidad}: fuerza del factor de deseabilidad social
#'         (0.3-1.2); requiere el vector \code{deseabilidad} (de
#'         \code{\link{calificar_deseabilidad}} o de la compuerta ya corrida).
#'   \item \code{extremos}: proporcion de respondientes con estilo extremo
#'         (0.1-0.4).
#'   \item \code{punto_medio}: proporcion que tiende al punto medio (0.1-0.4).
#'   \item \code{descuido}: proporcion con respuestas al azar (0.05-0.30).
#'   \item \code{lineas_rectas}: proporcion que repite la misma categoria en
#'         todos los items, straight-lining (0.05-0.30).
#' }
#'
#' PENSADA PARA CORRER EN LA PC DEL USUARIO (no en un servidor compartido):
#' por defecto usa todos los nucleos menos uno. Los streams RNG son POR
#' REPLICA (L'Ecuyer-CMRG), asi que con el mismo \code{seed} el resultado es
#' IDENTICO con 1 o con N nucleos: sirve para verificar la paridad entre la
#' app Shiny y el script local.
#'
#' @param x Objeto \code{semilla} (usa \code{$items$dimension}, \code{$similitud}
#'   y, si existe, la deseabilidad cacheada en \code{$compuerta}) o un vector de
#'   caracteres con la dimension de cada item.
#' @param deseabilidad Vector 0-1 por item (opcional). Si falta y no puede
#'   calcularse, el sesgo \code{deseabilidad} se omite con un aviso.
#' @param similitud Matriz de similitud de embeddings (dependencia local).
#' @param sesgos Vector con los sesgos a probar (ver Detalles).
#' @param dosis Lista nombrada opcional que sobreescribe el grid de dosis de
#'   cada sesgo, p. ej. \code{list(descuido = c(.1, .2, .4))}.
#' @param carga_propia,phi_teorico,umbral_ld,fuerza_ld,n,k_cat Parametros del
#'   modelo generador, identicos a \code{\link{simular_estructura}} (incluida
#'   la opcion \code{carga_propia = "semantica"}, v2.9.0).
#' @param n_rep Replicas Monte Carlo por celda (sesgo x dosis). Por defecto 60.
#' @param umbral_rmsea,umbral_phi Criterios de estructura limpia.
#' @param umbral_quiebre Probabilidad bajo la cual se declara el punto de
#'   quiebre. Por defecto 0.50.
#' @param n_nucleos Nucleos a usar. Por defecto (\code{NULL}) todos menos uno.
#'   Use \code{1} en servidores compartidos (p. ej. Connect Cloud).
#' @param optimizar_estres Logico (default \code{TRUE}). Si la escala resulta
#'   VULNERABLE/FRAGIL, reescribe iterativamente los items que colapsan primero
#'   (con instruccion anti-sesgo), recalcula embeddings + deseabilidad y
#'   re-corre el stress test hasta bajar la fragilidad. \code{FALSE} = solo
#'   diagnostico (comportamiento clasico). Requiere una escala regenerable
#'   (objeto \code{semilla} con textos de items) y \code{api_key}.
#' @param max_iteraciones Maximo de vueltas de reescritura (default 5).
#' @param n_reescribir Cuantos items reescribir por vuelta. Si \code{NULL},
#'   \code{ceiling(0.15 * n_items)} acotado a [1, 5].
#' @param n_rep_intermedio Replicas Monte Carlo en las vueltas intermedias
#'   (mas rapido; el diagnostico final usa \code{n_rep}). Default 30.
#' @param modelo Modelo LLM para reescribir los items en la optimizacion.
#' @param poblacion Poblacion objetivo (para la reescritura; si \code{NULL} se toma
#'   del metadata de la escala).
#' @param api_key Clave OpenAI (para calificar deseabilidad y, si
#'   \code{optimizar_estres=TRUE}, reescribir items y recalcular embeddings).
#' @param seed Semilla global (streams por replica derivados de ella).
#' @param verbose Imprime el avance celda a celda y el resumen final.
#' @return Objeto \code{semilla_estres} con \code{curvas} (data.frame sesgo x
#'   dosis con prob_limpia, IC de Wilson, rmsea/phi medianos, tasas de
#'   no-convergencia), \code{quiebres} (punto de quiebre y fragilidad por
#'   sesgo), \code{fragilidad_items} (caida de carga por item y sesgo critico),
#'   \code{linea_base}, \code{indice_global}, \code{veredicto} (EJE 2:
#'   resistencia a las dosis de sesgo), \code{origen} (EJE 1: estructura de
#'   partida, con \code{$etiqueta} LIMPIA / EN DUDA / COMPROMETIDA y
#'   \code{$escenarios}, la linea base recalculada con los pares de cada canal
#'   de dependencia local) y \code{params}.
#'   Si \code{optimizar_estres=TRUE} y hubo mejora, ademas \code{optimizacion}
#'   (historial iteracion a iteracion, indices inicial/final, items reescritos)
#'   y \code{escala_final} (la escala mejorada, lista para seguir usandose).
#'   Tiene metodos \code{print()} y \code{plot()}.
#' @seealso \code{\link{simular_estructura}}, \code{\link{compuerta_pre_aplicacion}}
#' @export
estres_escala <- function(x, deseabilidad = NULL, similitud = NULL,
                          sesgos = c("aquiescencia", "deseabilidad", "extremos",
                                     "punto_medio", "descuido", "lineas_rectas"),
                          dosis = NULL,
                          carga_propia = 0.60, phi_teorico = 0.30,
                          umbral_ld = 0.80, fuerza_ld = 1.4,
                          dl_juez = c("escenario", "off", "on"),
                          dl_adyacencia = c("off", "escenario", "on"),
                          pares_juez = NULL,
                          modelo_jueces = "gpt-4.1-mini",
                          g_juez = 0.35, g_ady = 0.25,
                          n = 300, n_rep = 60, k_cat = 5,
                          umbral_rmsea = 0.06, umbral_phi = 0.70,
                          umbral_quiebre = 0.50,
                          optimizar_estres = TRUE, max_iteraciones = 5L,
                          n_reescribir = NULL, n_rep_intermedio = 30L,
                          umbral_redundancia = 0.70,
                          modelo = "gpt-4.1-mini", poblacion = NULL,
                          n_nucleos = NULL,
                          api_key = Sys.getenv("OPENAI_API_KEY"),
                          seed = 2026, verbose = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Necesitas el paquete 'lavaan'.")
  if (!requireNamespace("MASS",  quietly = TRUE)) stop("Necesitas el paquete 'MASS'.")
  t0 <- Sys.time()

  # ---- Resolver escala, similitud y deseabilidad ----------------------------
  if (inherits(x, "semilla") || (is.list(x) && !is.null(x$items))) {
    dim_por_item <- x$items$dimension
    if (is.null(similitud) && !is.null(x$similitud)) similitud <- x$similitud
    if (is.null(deseabilidad)) {
      des_cache <- x$compuerta$deseabilidad$deseabilidad
      if (!is.null(des_cache) && length(des_cache) == length(dim_por_item)) {
        deseabilidad <- des_cache
        if (verbose) cat("Deseabilidad tomada de la compuerta ya corrida (sin costo LLM).\n")
      }
    }
  } else dim_por_item <- x
  p <- length(dim_por_item)

  sesgos <- match.arg(sesgos, c("aquiescencia", "deseabilidad", "extremos",
                                "punto_medio", "descuido", "lineas_rectas"),
                      several.ok = TRUE)
  if ("deseabilidad" %in% sesgos && is.null(deseabilidad)) {
    ok <- FALSE
    err_des <- NULL
    if (nzchar(api_key) && (inherits(x, "semilla") || (is.list(x) && !is.null(x$items)))) {
      d <- tryCatch(calificar_deseabilidad(x, api_key = api_key, verbose = verbose),
                    error = function(e) { err_des <<- e; NULL })
      if (!is.null(d)) { deseabilidad <- d$deseabilidad; ok <- TRUE }
    }
    if (!ok) {
      sesgos <- setdiff(sesgos, "deseabilidad")
      if (verbose) .aviso_canal_llm(
        que = "sesgo \"deseabilidad\" (Eje 2)",
        fn = "estres_escala", api_key = api_key, err = err_des,
        consecuencia = paste0(
          "Alternativa sin costo LLM: pasa el vector ya calificado con",
          " deseabilidad = <vector de 1 valor por item>, o corre antes la",
          " compuerta (calificar_deseabilidad) y reutiliza su resultado."),
        como_apagar = "quitar \"deseabilidad\" del argumento sesgos")
    }
  }
  if (!length(sesgos)) stop("No queda ningun sesgo por probar.")

  # ---- Modelo generador (identico al de simular_estructura) -----------------
  dims <- unique(dim_por_item); K <- length(dims)
  memb <- match(dim_por_item, dims)
  dir_j <- if (!is.null(deseabilidad)) 2 * deseabilidad - 1 else rep(0, p)
  int_j <- 0.5 * dir_j
  taus  <- stats::qnorm(seq(1, k_cat - 1) / k_cat)
  Phi   <- matrix(phi_teorico, K, K); diag(Phi) <- 1
  its   <- sprintf("i%02d", seq_len(p))
  syn   <- paste(sapply(seq_len(K), function(kk)
             paste0("F", kk, " =~ ", paste(its[memb == kk], collapse = " + "))),
             collapse = "\n")
  cg <- .resolver_cargas(carga_propia, similitud, memb, K, p, verbose)
  LAMBDA <- cg$LAMBDA; lambda_main <- cg$lambda_main
  var_theta <- diag(LAMBDA %*% Phi %*% t(LAMBDA))   # var del componente de rasgo
  sd_e  <- sqrt(pmax(0.20, 1 - lambda_main^2))
  pares_ld <- list()
  if (!is.null(similitud)) {
    S <- as.matrix(similitud)
    for (a in 1:(p - 1)) for (b in (a + 1):p) if (S[a, b] > umbral_ld)
      pares_ld[[length(pares_ld) + 1]] <- list(a = a, b = b,
        g = sqrt(pmin(0.9, fuerza_ld * (S[a, b] - umbral_ld) / (1 - umbral_ld))))
  }
  # ---- Canales adicionales de dependencia local (calibracion 2026-07) --------
  # El coseno >= umbral_ld solo captura gemelos LEXICOS; la dependencia local
  # real tiene ademas dos fuentes que el fraseo no delata con coseno alto:
  # (a) gemelos SEMANTICOS que un juez LLM detecta (viven en coseno 0.43-0.78)
  #     - pero el juez sobre-alarma en escalas ya depuradas (casos ASS y
  #       MitosAmor: pares marcados sin dependencia empirica), por eso el
  #       default es "escenario": se reporta la linea base CON y SIN esos
  #       pares, en lugar de darlos por hecho;
  # (b) ADYACENCIA: items consecutivos (8/9 pares con MI >= 10 en las escalas
  #     de pareja y 3/3 errores correlacionados del articulo de Celos fueron
  #     consecutivos). Default "off" porque el orden de aplicacion suele
  #     aleatorizarse; activarlo si el test se aplica en orden fijo.
  dl_juez <- match.arg(dl_juez)
  dl_adyacencia <- match.arg(dl_adyacencia)
  tiene_items <- (inherits(x, "semilla") || is.list(x)) && !is.null(x$items$item)
  clave_ld <- vapply(pares_ld, function(pl) paste(pl$a, pl$b), character(1))
  pares_canal <- list(juez = list(), adyacencia = list())
  if (dl_juez != "off") {
    pj <- pares_juez
    err_juez <- NULL
    if (is.null(pj) && tiene_items && nzchar(api_key)) {
      pj <- tryCatch(
        .juzgar_parafrasis_llm(x$items, .configurar_openai(api_key),
                               modelo = modelo_jueces),
        error = function(e) { err_juez <<- e; NULL })
    }
    # El juez pudo devolver un data.frame vacio MARCADO (attr "fallo_llm"):
    # eso no es "no hay gemelos", es "el canal no pudo correr".
    fallo_pj <- if (is.null(pj)) NULL else attr(pj, "fallo_llm")
    if (!is.null(pj) && nrow(pj)) {
      for (r in seq_len(nrow(pj))) {
        a <- min(pj$item1[r], pj$item2[r]); b <- max(pj$item1[r], pj$item2[r])
        if (a >= 1 && b <= p && a != b && !(paste(a, b) %in% clave_ld)) {
          pares_canal$juez[[length(pares_canal$juez) + 1]] <-
            list(a = a, b = b, g = g_juez)
        }
      }
    } else if ((is.null(pj) || !is.null(fallo_pj)) && verbose && tiene_items) {
      if (is.null(err_juez) && !is.null(fallo_pj))
        err_juez <- simpleError(fallo_pj)
      .aviso_canal_llm(
        que = "canal \"juez de parafrasis\" (Eje 1)",
        fn = "estres_escala", api_key = api_key, err = err_juez,
        consecuencia = paste0(
          "Que te pierdes: solo el escenario de gemelos SEMANTICOS del Eje 1",
          " (pares que el coseno no delata). El resto del analisis (Eje 1 por",
          " coseno y Eje 2 de sesgos) corre completo y no se afecta."),
        como_apagar = "dl_juez = \"off\"")
    } else if (verbose && nrow(pj) == 0L && tiene_items) {
      cat(" Canal juez de parafrasis: consultado, sin pares gemelos detectados.\n")
      utils::flush.console()
    }
  }
  if (dl_adyacencia != "off") {
    for (j in seq_len(p - 1)) {
      if (!(paste(j, j + 1) %in% clave_ld)) {
        pares_canal$adyacencia[[length(pares_canal$adyacencia) + 1]] <-
          list(a = j, b = j + 1, g = g_ady)
      }
    }
  }
  # modo "on": los pares del canal entran al pronostico BASE como un hecho
  if (dl_juez == "on" && length(pares_canal$juez)) {
    pares_ld <- c(pares_ld, pares_canal$juez); pares_canal$juez <- list()
  }
  if (dl_adyacencia == "on" && length(pares_canal$adyacencia)) {
    pares_ld <- c(pares_ld, pares_canal$adyacencia); pares_canal$adyacencia <- list()
  }

  g2_j <- rep(0, p)
  for (pl in pares_ld) { g2_j[pl$a] <- g2_j[pl$a] + pl$g^2
                         g2_j[pl$b] <- g2_j[pl$b] + pl$g^2 }

  .par_estres <- function(sesgo, d) {
    f_des <- if (sesgo == "deseabilidad") d else 0
    sd_aq <- if (sesgo == "aquiescencia") d else 0
    prop  <- if (sesgo %in% c("extremos", "punto_medio", "descuido",
                              "lineas_rectas")) d else 0
    dirf  <- f_des * dir_j
    list(n = n, p = p, K = K, k_cat = k_cat, memb = memb, Phi = Phi,
         LAMBDA = LAMBDA, int_j = int_j, dirf = dirf, sd_aq = sd_aq,
         sd_tot = sqrt(var_theta + dirf^2 + sd_aq^2 + g2_j + sd_e^2),
         taus = taus, pares_ld = pares_ld, sd_e = sd_e, its = its, syn = syn,
         sesgo = sesgo, prop = prop)
  }

  # ---- Grids de dosis --------------------------------------------------------
  grids <- list(aquiescencia  = c(0.2, 0.4, 0.6, 0.8),
                deseabilidad  = c(0.3, 0.6, 0.9, 1.2),
                extremos      = c(0.1, 0.2, 0.3, 0.4),
                punto_medio   = c(0.1, 0.2, 0.3, 0.4),
                descuido      = c(0.05, 0.10, 0.20, 0.30),
                lineas_rectas = c(0.05, 0.10, 0.20, 0.30))
  unidades <- c(aquiescencia = "DE del factor", deseabilidad = "fuerza del factor",
                extremos = "proporcion", punto_medio = "proporcion",
                descuido = "proporcion", lineas_rectas = "proporcion")
  if (!is.null(dosis)) for (nm in names(dosis))
    if (nm %in% names(grids)) grids[[nm]] <- sort(unique(as.numeric(dosis[[nm]])))

  celdas <- rbind(data.frame(sesgo = "linea_base", dosis = 0),
                  do.call(rbind, lapply(sesgos, function(sg)
                    data.frame(sesgo = sg, dosis = grids[[sg]]))))
  n_celdas <- nrow(celdas)

  # ---- Nucleos, streams y ETA ------------------------------------------------
  n_nucleos <- n_nucleos %||% max(1L, parallel::detectCores() - 1L)
  n_nucleos <- max(1L, as.integer(n_nucleos))
  seeds <- .semillas_lecuyer(seed, n_celdas * n_rep)
  t_cfa <- 0.4 * (p / 16)^1.7
  eta_min <- round(n_celdas * n_rep * t_cfa / n_nucleos * 1.15 / 60, 1)

  if (verbose) {
    cat("=============================================================\n")
    cat(" SeMiLLa - PRUEBA DE ESTRES DE ESCALA (estres_escala)\n")
    cat("=============================================================\n")
    cat(sprintf(" Escala: %d items | %d dimension(es) | n simulado = %d\n", p, K, n))
    cat(sprintf(" Sesgos: %s\n", paste(sesgos, collapse = ", ")))
    cat(sprintf(" Celdas: %d (linea base + %d dosis) x %d replicas = %d CFAs\n",
                n_celdas, n_celdas - 1, n_rep, n_celdas * n_rep))
    cat(sprintf(" Nucleos: %d de %d disponibles | ETA aproximada: %s\n",
                n_nucleos, parallel::detectCores(), .eta_txt(eta_min)))
    cat("-------------------------------------------------------------\n")
    utils::flush.console()
  }

  cl <- .cluster_estres(n_nucleos, verbose)
  if (!is.null(cl)) on.exit(parallel::stopCluster(cl), add = TRUE)

  # ---- Correr celda por celda -------------------------------------------------
  res_celda <- vector("list", n_celdas)
  for (i in seq_len(n_celdas)) {
    tc <- Sys.time()
    par_i <- .par_estres(celdas$sesgo[i], celdas$dosis[i])
    if (celdas$sesgo[i] == "linea_base") { par_i$sesgo <- "ninguno"; par_i$prop <- 0 }
    sd_i <- seeds[((i - 1) * n_rep + 1):(i * n_rep)]
    M <- .ejecutar_reps(sd_i, .estres_rep, par_i, cl)
    conv <- M[, "conv"] == 1
    limpia <- conv & M[, "adm"] == 1 & M[, "rmsea"] <= umbral_rmsea
    if (K > 1) limpia <- limpia & M[, "phi"] < umbral_phi
    limpia[is.na(limpia)] <- FALSE
    prob <- mean(limpia)
    cg_cols <- paste0("cg_", its)
    res_celda[[i]] <- list(
      prob = prob,
      rmsea_med = stats::median(M[conv, "rmsea"], na.rm = TRUE),
      phi_med = if (K > 1) stats::median(M[conv, "phi"], na.rm = TRUE) else NA_real_,
      tasa_no_conv = mean(!conv),
      tasa_inadmisible = mean(conv & M[, "adm"] == 0),
      cargas_med = if (any(conv))
        apply(M[conv, cg_cols, drop = FALSE], 2, stats::median, na.rm = TRUE)
      else stats::setNames(rep(NA_real_, p), cg_cols))
    if (verbose) {
      etiq <- if (celdas$sesgo[i] == "linea_base") "linea base (sin sesgos)"
              else sprintf("%s dosis %.2f", celdas$sesgo[i], celdas$dosis[i])
      cat(sprintf(" [%2d/%2d] %-38s prob limpia %3.0f%% (%.1f s)\n",
                  i, n_celdas, paste0(etiq, " ",
                    strrep(".", max(0, 30 - nchar(etiq)))),
                  100 * prob, as.numeric(difftime(Sys.time(), tc, units = "secs"))))
      utils::flush.console()
    }
  }

  # ---- Curvas dosis-respuesta -------------------------------------------------
  base <- res_celda[[1]]
  ic_bin <- function(pr) .ic_wilson(pr, n_rep)

  # ---- Escenarios de ORIGEN (dosis 0 con canales de dependencia local) --------
  # Se recalcula SOLO la linea base inyectando los pares de cada canal en modo
  # "escenario"; el grid de dosis no se repite (el eje de resistencia a sesgos
  # se evalua sobre el pronostico base).
  escenarios_origen <- NULL
  esc_defs <- Filter(length, pares_canal)
  if (length(esc_defs)) {
    filas_esc <- list()
    for (nm in names(esc_defs)) {
      pl2 <- c(pares_ld, esc_defs[[nm]])
      g2_e <- rep(0, p)
      for (pl in pl2) { g2_e[pl$a] <- g2_e[pl$a] + pl$g^2
                        g2_e[pl$b] <- g2_e[pl$b] + pl$g^2 }
      par_e <- .par_estres("ninguno", 0)
      par_e$prop <- 0
      par_e$pares_ld <- pl2
      par_e$sd_tot <- sqrt(var_theta + g2_e + sd_e^2)
      sd_esc <- .semillas_lecuyer(seed + 7000L + match(nm, names(esc_defs)), n_rep)
      Me <- .ejecutar_reps(sd_esc, .estres_rep, par_e, cl)
      conv_e <- Me[, "conv"] == 1
      limpia_e <- conv_e & Me[, "adm"] == 1 & Me[, "rmsea"] <= umbral_rmsea
      if (K > 1) limpia_e <- limpia_e & Me[, "phi"] < umbral_phi
      limpia_e[is.na(limpia_e)] <- FALSE
      etiq_pares <- vapply(esc_defs[[nm]], function(z)
        paste0(its[z$a], "~", its[z$b]), character(1))
      filas_esc[[nm]] <- data.frame(
        canal = nm, n_pares = length(esc_defs[[nm]]),
        prob_limpia = mean(limpia_e),
        rmsea_med = stats::median(Me[conv_e, "rmsea"], na.rm = TRUE),
        pares = paste(etiq_pares, collapse = ", "),
        stringsAsFactors = FALSE)
      if (verbose) {
        cat(sprintf(" [escenario %s] linea base con %d par(es) inyectado(s): prob limpia %3.0f%%\n",
                    nm, length(esc_defs[[nm]]), 100 * mean(limpia_e)))
        utils::flush.console()
      }
    }
    escenarios_origen <- do.call(rbind, filas_esc)
    rownames(escenarios_origen) <- NULL
  }
  curvas <- do.call(rbind, lapply(sesgos, function(sg) {
    idx <- which(celdas$sesgo == sg)
    d <- data.frame(sesgo = sg, unidad = unname(unidades[sg]),
                    dosis = c(0, celdas$dosis[idx]),
                    prob_limpia = c(base$prob, vapply(res_celda[idx], `[[`, numeric(1), "prob")),
                    rmsea_med = c(base$rmsea_med, vapply(res_celda[idx], `[[`, numeric(1), "rmsea_med")),
                    phi_med = c(base$phi_med, vapply(res_celda[idx], `[[`, numeric(1), "phi_med")),
                    tasa_no_conv = c(base$tasa_no_conv, vapply(res_celda[idx], `[[`, numeric(1), "tasa_no_conv")),
                    tasa_inadmisible = c(base$tasa_inadmisible, vapply(res_celda[idx], `[[`, numeric(1), "tasa_inadmisible")))
    ic <- t(vapply(d$prob_limpia, ic_bin, numeric(2)))
    d$prob_ic_inf <- ic[, 1]; d$prob_ic_sup <- ic[, 2]
    d
  }))
  rownames(curvas) <- NULL

  # ---- Punto de quiebre + fragilidad por sesgo ---------------------------------
  quiebres <- do.call(rbind, lapply(sesgos, function(sg) {
    cv <- curvas[curvas$sesgo == sg, ]
    fr <- if (base$prob > 0)
      min(1, max(0, mean(pmax(0, base$prob - cv$prob_limpia[cv$dosis > 0])) / base$prob))
    else NA_real_
    data.frame(sesgo = sg, unidad = unname(unidades[sg]),
               dosis_quiebre = .dosis_quiebre(cv$dosis, cv$prob_limpia, umbral_quiebre),
               fragilidad = fr)
  }))
  rownames(quiebres) <- NULL
  indice_global <- mean(quiebres$fragilidad, na.rm = TRUE)

  # ---- Fragilidad por item: caida de carga a la dosis maxima de cada sesgo ------
  frag_items <- NULL
  if (any(is.finite(base$cargas_med))) {
    caidas <- sapply(sesgos, function(sg) {
      i_max <- max(which(celdas$sesgo == sg))
      base$cargas_med - res_celda[[i_max]]$cargas_med
    })                                        # p x n_sesgos
    caidas <- as.matrix(caidas)
    peor <- apply(caidas, 1, function(z)
      if (all(is.na(z))) NA_integer_ else which.max(z))
    item_txt <- its
    if ((inherits(x, "semilla") || is.list(x)) && !is.null(x$items$item) &&
        length(x$items$item) == p) item_txt <- x$items$item
    frag_items <- data.frame(
      item = item_txt,
      codigo = its,
      dimension = dim_por_item,
      carga_base = round(unname(base$cargas_med), 3),
      caida_max = round(apply(caidas, 1, max, na.rm = TRUE), 3),
      sesgo_critico = sesgos[peor])
    frag_items <- frag_items[order(-frag_items$caida_max), ]
    rownames(frag_items) <- NULL
  }

  # ---- Veredicto de DOS EJES -------------------------------------------------
  # EJE 1: estructura de PARTIDA (dosis 0), con los escenarios de los canales.
  origen_etiqueta <- if (base$prob < umbral_quiebre) {
    "COMPROMETIDA (la linea base ya es sucia: corregir items antes de aplicar)"
  } else if (!is.null(escenarios_origen) &&
             any(escenarios_origen$prob_limpia < umbral_quiebre)) {
    malos <- escenarios_origen[escenarios_origen$prob_limpia < umbral_quiebre, ,
                               drop = FALSE]
    paste0("EN DUDA: si los pares candidatos fueran dependencia local real, ",
           "la estructura caeria (",
           paste(sprintf("canal %s: prob %d%%", malos$canal,
                         round(100 * malos$prob_limpia)), collapse = "; "),
           "); verificar o reescribir los pares señalados antes de aplicar")
  } else {
    "LIMPIA (sin defecto de origen detectable desde el fraseo)"
  }
  # EJE 2: resistencia a las DOSIS de sesgo (como antes; compatibilidad total)
  veredicto <- if (base$prob < umbral_quiebre) {
    "FRAGIL DE BASE (la estructura no se sostiene ni sin sesgos; revisar items antes de estresar)"
  } else if (indice_global < 0.15 && all(is.na(quiebres$dosis_quiebre))) {
    "ROBUSTA (ningun sesgo la quiebra en el rango probado)"
  } else if (indice_global < 0.40) {
    "VULNERABLE (se degrada ante algunos sesgos; ver quiebres y items criticos)"
  } else "FRAGIL (varios sesgos la quiebran; redisenar items o formato antes de campo)"

  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  out <- list(curvas = curvas, quiebres = quiebres,
              fragilidad_items = frag_items,
              linea_base = list(prob_limpia = base$prob,
                                prob_ic = ic_bin(base$prob),
                                rmsea_med = base$rmsea_med, phi_med = base$phi_med,
                                cargas_med = base$cargas_med),
              indice_global = indice_global,
              veredicto = veredicto,
              origen = list(etiqueta = origen_etiqueta,
                            prob_base = base$prob,
                            escenarios = escenarios_origen),
              umbral_quiebre = umbral_quiebre,
              params = list(n = n, n_rep = n_rep, k_cat = k_cat,
                            carga_propia = carga_propia,
                            lambda_rango = range(lambda_main),
                            n_cruces = cg$n_cruces, phi_teorico = phi_teorico,
                            umbral_rmsea = umbral_rmsea, umbral_phi = umbral_phi,
                            umbral_ld = umbral_ld, fuerza_ld = fuerza_ld,
                            dl_juez = dl_juez, dl_adyacencia = dl_adyacencia,
                            g_juez = g_juez, g_ady = g_ady,
                            sesgos = sesgos, grids = grids[sesgos],
                            n_nucleos = n_nucleos, seed = seed,
                            n_items = p, n_dimensiones = K),
              minutos = round(mins, 1))
  class(out) <- "semilla_estres"
  if (verbose) { cat("-------------------------------------------------------------\n")
                 print(out) }

  # ---- Optimizacion iterativa dirigida por el stress test --------------------
  #  Solo si el usuario lo pide (default TRUE), hay una escala regenerable
  #  (objeto semilla con textos de items), hay API key, y la escala se degrada
  #  con algun sesgo (ni ROBUSTA ni fragil de base). Reescribe los items que
  #  colapsan primero y re-testea hasta max_iteraciones.
  puede_opt <- isTRUE(optimizar_estres) &&
    (inherits(x, "semilla") || (is.list(x) && !is.null(x$items) &&
                                  !is.null(x$items$item))) &&
    nzchar(api_key) &&
    !grepl("^ROBUSTA|^FRAGIL DE BASE", out$veredicto)
  if (puede_opt) {
    params_estres <- list(sesgos = sesgos, dosis = dosis,
      carga_propia = carga_propia, phi_teorico = phi_teorico,
      umbral_ld = umbral_ld, fuerza_ld = fuerza_ld, n = n, k_cat = k_cat,
      umbral_rmsea = umbral_rmsea, umbral_phi = umbral_phi,
      umbral_quiebre = umbral_quiebre, n_nucleos = n_nucleos)
    out <- tryCatch(
      .optimizar_por_estres(x, out, params_estres = params_estres,
        max_iteraciones = max_iteraciones, n_reescribir = n_reescribir,
        n_rep_intermedio = n_rep_intermedio, n_rep_final = n_rep,
        umbral_redundancia = umbral_redundancia,
        modelo = modelo, poblacion = poblacion, api_key = api_key,
        seed = seed, verbose = verbose),
      error = function(e) {
        warning("La optimizacion por estres fallo (", conditionMessage(e),
                "); se devuelve el diagnostico sin optimizar.")
        out$optimizacion <- list(aplicada = FALSE, error = conditionMessage(e))
        out
      })
  } else {
    out$optimizacion <- list(aplicada = FALSE)
  }
  invisible(out)
}

#' @export
print.semilla_estres <- function(x, ...) {
  cat(sprintf(" LINEA BASE (sin sesgos): prob limpia %.0f%% [IC95: %.0f-%.0f%%]\n",
              100 * x$linea_base$prob_limpia, 100 * x$linea_base$prob_ic[1],
              100 * x$linea_base$prob_ic[2]))
  if (!is.null(x$origen)) {
    cat(" EJE 1 - ESTRUCTURA DE PARTIDA:", x$origen$etiqueta, "\n")
    if (!is.null(x$origen$escenarios)) {
      for (i in seq_len(nrow(x$origen$escenarios))) {
        e <- x$origen$escenarios[i, ]
        cat(sprintf("   escenario %-11s (%d par(es)): prob limpia %3.0f%% | %s\n",
                    e$canal, e$n_pares, 100 * e$prob_limpia, e$pares))
      }
    }
    cat(" EJE 2 - RESISTENCIA A SESGOS (dosis-respuesta):\n")
  }
  cat(sprintf(" RESUMEN POR SESGO (quiebre = dosis donde prob < %.0f%%):\n",
              100 * x$umbral_quiebre))
  for (i in seq_len(nrow(x$quiebres))) {
    q <- x$quiebres[i, ]
    barra <- if (is.na(q$fragilidad)) "" else strrep("|", round(10 * min(1, q$fragilidad)))
    cat(sprintf("   %-14s quiebre: %-18s fragilidad: %s %s\n", q$sesgo,
                if (is.na(q$dosis_quiebre)) "no quiebra"
                else sprintf("%.2f (%s)", q$dosis_quiebre, q$unidad),
                ifelse(is.na(q$fragilidad), "  NA",
                       sprintf("%.2f", q$fragilidad)),
                paste0(barra, strrep("-", 10 - nchar(barra)))))
  }
  if (!is.null(x$fragilidad_items)) {
    cat(" ITEMS QUE COLAPSAN PRIMERO (caida de carga vs linea base):\n")
    top <- utils::head(x$fragilidad_items, 5)
    for (i in seq_len(nrow(top)))
      cat(sprintf("   %d. %s [%s | %s]: carga %.2f -> caida %.2f bajo '%s'\n",
                  i, top$codigo[i], substr(top$item[i], 1, 42), top$dimension[i],
                  top$carga_base[i], top$caida_max[i], top$sesgo_critico[i]))
  }
  cat(sprintf(" >> INDICE GLOBAL DE FRAGILIDAD: %.2f (0 = robusta, 1 = fragil)\n",
              x$indice_global))
  cat(sprintf(" >> ESCENARIO PREVISTO: %s\n", x$veredicto))
  if (!is.null(x$minutos)) cat(sprintf(" (completado en %.1f min con %d nucleo(s))\n",
                                       x$minutos, x$params$n_nucleos))
  invisible(x)
}

#' @export
plot.semilla_estres <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Necesitas 'ggplot2'.")
  cv <- x$curvas
  cv$etiqueta <- paste0(cv$sesgo, " (", cv$unidad, ")")
  qq <- x$quiebres[!is.na(x$quiebres$dosis_quiebre), ]
  if (nrow(qq)) qq$etiqueta <- paste0(qq$sesgo, " (", qq$unidad, ")")
  g <- ggplot2::ggplot(cv, ggplot2::aes(x = dosis, y = prob_limpia)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = prob_ic_inf, ymax = prob_ic_sup),
                         fill = "#2e7d52", alpha = 0.15) +
    ggplot2::geom_line(color = "#2e7d52", linewidth = 1) +
    ggplot2::geom_point(color = "#2e7d52", size = 2) +
    ggplot2::geom_hline(yintercept = x$umbral_quiebre, linetype = 2,
                        color = "#A11616") +
    ggplot2::facet_wrap(~etiqueta, scales = "free_x") +
    ggplot2::scale_y_continuous(labels = function(v) sprintf("%.0f%%", 100 * v),
                                limits = c(0, 1)) +
    ggplot2::labs(x = "Dosis del sesgo", y = "Prob. de estructura limpia",
                  title = "Prueba de estres de la escala",
                  subtitle = sprintf("Linea roja: umbral de quiebre (%.0f%%). Escenario previsto: %s",
                                     100 * x$umbral_quiebre, x$veredicto)) +
    ggplot2::theme_minimal(base_size = 12)
  if (nrow(qq)) g <- g + ggplot2::geom_vline(data = qq,
    ggplot2::aes(xintercept = dosis_quiebre), linetype = 3, color = "#A11616")
  g
}
