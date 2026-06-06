#' @title Generar test cognitivo procedural (memoria de trabajo, atencion)
#'
#' @description
#' Quinto formato de SeMiLLa: tests cognitivos donde los estimulos se
#' generan PROCEDURALMENTE (no via LLM) porque su validez no proviene de la
#' semantica sino de la estructura geometrica/numerica del estimulo y del
#' control experimental.
#'
#' Soporta paradigmas clasicos de memoria de trabajo. La version inicial
#' implementa el \strong{Operation Span (OSPAN)} de Turner & Engle (1989)
#' en su variante automatizada de Unsworth, Heitz, Schrock & Engle (2005).
#'
#' Estructura de un trial OSPAN:
#' \preformatted{
#'   1) Operacion matematica + V/F      (control de procesamiento)
#'   2) Letra/digito a memorizar         (almacenamiento)
#'   3) Repetir set_size veces
#'   4) Recuperacion: recordar las letras EN EL ORDEN PRESENTADO
#' }
#'
#' Esta funcion incluye dos garantias procedurales que son los analogos
#' cognitivos de los pasos de SeMiLLa para escalas LLM:
#' \itemize{
#'   \item \strong{Refinamiento procedural}: equivalente al refinamiento por
#'         redundancia de \code{generar_escala()}. Verifica y corrige:
#'         (a) letras no repetidas dentro del set, (b) operaciones no
#'         triviales (a+0, b*1), (c) balance V/F (50/50 dentro del nivel),
#'         (d) distractores plausibles en operaciones falsas (cerca del
#'         valor verdadero), (e) letras no perceptualmente confundibles
#'         juntas (e.g., M/N, B/D, P/Q).
#'   \item \strong{Clustering por dificultad procedural}: equivalente al
#'         clustering K-means de \code{precision_clasificacion()} pero sobre
#'         features procedurales (set_size, magnitud_numeros,
#'         complejidad_op, similitud_letras). K-means clasifica cada trial
#'         en niveles "facil/medio/dificil" para verificar que la dificultad
#'         predicha coincide con el nivel teorico asignado.
#' }
#'
#' @section Referencias metodologicas:
#' \itemize{
#'   \item Turner, M. L., & Engle, R. W. (1989). Is working memory capacity
#'         task dependent? \emph{Journal of Memory and Language, 28}(2),
#'         127-154.
#'   \item Unsworth, N., Heitz, R. P., Schrock, J. C., & Engle, R. W.
#'         (2005). An automated version of the operation span task.
#'         \emph{Behavior Research Methods, 37}(3), 498-505.
#' }
#'
#' @param paradigma Paradigma cognitivo. Por ahora solo \code{"ospan"}.
#'   Reservado para futuros: \code{"n_back"}, \code{"digit_span"},
#'   \code{"corsi"}, \code{"reading_span"}.
#' @param niveles_dificultad Vector con los tamanos de set (set sizes) que
#'   se aplicaran. Default \code{c(3, 4, 5, 6, 7)} (rango clasico OSPAN).
#'   Cada nivel representa la cantidad de letras que el participante debe
#'   memorizar en un trial.
#' @param n_trials_por_nivel Numero de trials por nivel de dificultad.
#'   Default 3 (replica el OSPAN automatizado).
#' @param complejidad_operacion Tipo de operacion matematica:
#'   \code{"simple"} (a+b o a-b), \code{"media"} (a*b+c o a*b-c) o
#'   \code{"compleja"} ((a+b)*c+d). Default \code{"media"}.
#' @param estimulo_memoria Tipo de estimulo a memorizar:
#'   \code{"letras"} (default, consonantes seguras), \code{"digitos"} o
#'   \code{"palabras"} (palabras CV-CV de 4 letras).
#' @param refinar_estimulos Logico. Si TRUE (default), aplica el
#'   refinamiento procedural automatico (ver descripcion).
#' @param cluster_dificultad Logico. Si TRUE (default), corre K-means
#'   sobre features procedurales y reporta concordancia con el nivel
#'   teorico asignado.
#' @param max_iter_refinamiento Maximo de iteraciones de refinamiento por
#'   trial antes de aceptar (default 10).
#' @param idioma "es" o "en" (afecta solo etiquetas de instrucciones).
#' @param seed Semilla para reproducibilidad.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_test_cognitivo} con:
#' \itemize{
#'   \item \code{paradigma}, \code{niveles_dificultad}, \code{config}.
#'   \item \code{trials}: data.frame en formato largo (un row por
#'         operacion+letra) con columnas:
#'         \code{n_trial, nivel_dificultad, set_size, posicion_en_set,
#'         operacion_str, valor_dado, valor_verdadero, es_verdadera,
#'         estimulo_memoria, dificultad_procedural,
#'         cluster_dificultad}.
#'   \item \code{trials_resumen}: un row por trial con la lista de
#'         estimulos a memorizar (clave).
#'   \item \code{validacion_procedural}: data.frame con metricas de
#'         calidad (% letras unicas, % balance V/F, plausibilidad).
#'   \item \code{cluster_summary}: tabla nivel_teorico x cluster.
#'   \item \code{metadata}.
#' }
#'
#' @examples
#' \dontrun{
#' tc <- generar_test_cognitivo(
#'   paradigma          = "ospan",
#'   niveles_dificultad = c(3, 4, 5, 6, 7),
#'   n_trials_por_nivel = 3,
#'   complejidad_operacion = "media",
#'   estimulo_memoria   = "letras",
#'   seed = 2026
#' )
#' print(tc)
#' }
#'
#' @export
generar_test_cognitivo <- function(
  paradigma             = c("ospan"),
  niveles_dificultad    = c(3L, 4L, 5L, 6L, 7L),
  n_trials_por_nivel    = 3L,
  complejidad_operacion = c("media", "simple", "compleja"),
  estimulo_memoria      = c("letras", "digitos", "palabras"),
  refinar_estimulos     = TRUE,
  cluster_dificultad    = TRUE,
  max_iter_refinamiento = 10L,
  idioma                = c("es", "en"),
  seed                  = 2026,
  verbose               = TRUE
) {

  paradigma             <- match.arg(paradigma)
  complejidad_operacion <- match.arg(complejidad_operacion)
  estimulo_memoria      <- match.arg(estimulo_memoria)
  idioma                <- match.arg(idioma)

  if (!is.null(seed)) set.seed(seed)

  if (verbose) {
    cat("\n[generar_test_cognitivo] Paradigma: ", paradigma, "\n", sep = "")
    cat("  Niveles de dificultad (set sizes): ",
        paste(niveles_dificultad, collapse = ", "), "\n", sep = "")
    cat("  Trials por nivel: ", n_trials_por_nivel, "\n", sep = "")
    cat("  Total trials    : ",
        length(niveles_dificultad) * n_trials_por_nivel, "\n", sep = "")
    cat("  Complejidad op  : ", complejidad_operacion, "\n", sep = "")
    cat("  Estimulo memoria: ", estimulo_memoria, "\n", sep = "")
  }

  # Pool de letras seguras (consonantes, sin pares confundibles juntos)
  pool_letras <- c("F", "H", "J", "K", "L", "P", "Q", "R", "S", "T",
                    "X", "Y", "Z")
  # Pares confundibles que el refinamiento intenta evitar
  pares_confundibles <- list(c("M", "N"), c("B", "D"), c("P", "Q"),
                              c("E", "F"), c("V", "W"), c("U", "V"),
                              c("I", "L"), c("O", "Q"), c("C", "G"),
                              c("R", "K"))

  # Generar todos los trials
  trials_lista <- list()
  trials_resumen_lista <- list()
  contador_trial <- 0L

  for (nivel in niveles_dificultad) {
    for (j in seq_len(n_trials_por_nivel)) {
      contador_trial <- contador_trial + 1L
      trial <- .generar_trial_ospan(
        n_trial               = contador_trial,
        set_size              = nivel,
        complejidad_operacion = complejidad_operacion,
        estimulo_memoria      = estimulo_memoria,
        pool_letras           = pool_letras,
        pares_confundibles    = pares_confundibles,
        refinar_estimulos     = refinar_estimulos,
        max_iter              = max_iter_refinamiento
      )
      trials_lista[[contador_trial]] <- trial$detalle
      trials_resumen_lista[[contador_trial]] <- trial$resumen
    }
  }

  trials_df         <- do.call(rbind, trials_lista)
  trials_resumen_df <- do.call(rbind, trials_resumen_lista)

  # Auditoria de validacion procedural
  validacion <- .auditoria_procedural_ospan(trials_df, trials_resumen_df)
  if (verbose) {
    cat("\n[Validacion procedural]\n")
    print(validacion)
  }

  # Clustering procedural por dificultad
  cluster_summary <- NULL
  if (isTRUE(cluster_dificultad)) {
    clst <- .cluster_dificultad_ospan(trials_df, trials_resumen_df,
                                        n_clusters = 3L)
    trials_df$cluster_dificultad <- clst$asignacion[trials_df$n_trial]
    cluster_summary <- clst$resumen
    if (verbose) {
      cat("\n[Clustering procedural - tabla nivel x cluster]\n")
      print(cluster_summary)
    }
  }

  # Calcular dificultad procedural a nivel de TRIAL si no se hizo
  if (!"dificultad_procedural" %in% names(trials_df)) {
    trials_df$dificultad_procedural <- NA_real_
  }

  resultado <- list(
    paradigma           = paradigma,
    niveles_dificultad  = niveles_dificultad,
    n_trials_por_nivel  = n_trials_por_nivel,
    config              = list(
      complejidad_operacion = complejidad_operacion,
      estimulo_memoria      = estimulo_memoria,
      pool_letras           = pool_letras,
      refinar_estimulos     = refinar_estimulos,
      cluster_dificultad    = cluster_dificultad
    ),
    trials              = trials_df,
    trials_resumen      = trials_resumen_df,
    validacion_procedural = validacion,
    cluster_summary     = cluster_summary,
    idioma              = idioma,
    metadata            = list(
      seed     = seed,
      fecha    = format(Sys.Date()),
      n_trials = nrow(trials_resumen_df)
    )
  )
  class(resultado) <- c("semilla_test_cognitivo", "list")
  resultado
}


# =============================================================================
# Helpers internos: generacion de trial OSPAN
# =============================================================================

#' @keywords internal
.generar_operacion <- function(complejidad) {
  if (complejidad == "simple") {
    a <- sample(2:9, 1L)
    b <- sample(2:9, 1L)
    op <- sample(c("+", "-"), 1L)
    valor_verdadero <- if (op == "+") a + b else a - b
    operacion_str <- paste0(a, " ", op, " ", b)
  } else if (complejidad == "media") {
    a <- sample(2:5, 1L)
    b <- sample(2:5, 1L)
    c <- sample(1:9, 1L)
    op <- sample(c("+", "-"), 1L)
    valor_verdadero <- if (op == "+") (a * b) + c else (a * b) - c
    operacion_str <- paste0("(", a, " * ", b, ") ", op, " ", c)
  } else {  # compleja
    a <- sample(2:4, 1L)
    b <- sample(2:5, 1L)
    c <- sample(2:5, 1L)
    d <- sample(1:9, 1L)
    op1 <- sample(c("+", "-"), 1L)
    op2 <- sample(c("+", "-"), 1L)
    base <- if (op1 == "+") (a + b) else (a - b)
    valor_verdadero <- if (op2 == "+") (base * c) + d else (base * c) - d
    operacion_str <- paste0("((", a, " ", op1, " ", b, ") * ", c, ") ",
                              op2, " ", d)
  }
  list(str = operacion_str, valor_verdadero = valor_verdadero)
}

#' @keywords internal
.es_operacion_trivial <- function(op_str, valor_verdadero) {
  # Trivial si: a+0, a*1, a-a, 0*x, 1*a
  if (grepl("\\* 1\\b", op_str) || grepl("\\b1 \\*", op_str)) return(TRUE)
  if (grepl("\\+ 0\\b", op_str) || grepl("\\b0 \\+", op_str)) return(TRUE)
  if (grepl("- 0\\b", op_str)) return(TRUE)
  if (valor_verdadero == 0L) return(TRUE)
  FALSE
}

#' @keywords internal
.generar_letras_set <- function(set_size, pool_letras, pares_confundibles,
                                  max_iter = 10L) {
  for (it in seq_len(max_iter)) {
    letras <- sample(pool_letras, set_size, replace = FALSE)
    # Verificar pares confundibles consecutivos
    confundibles_juntas <- FALSE
    for (par in pares_confundibles) {
      pos <- which(letras %in% par)
      if (length(pos) >= 2L) {
        if (any(diff(sort(pos)) == 1L)) {
          confundibles_juntas <- TRUE
          break
        }
      }
    }
    if (!confundibles_juntas) return(letras)
  }
  # Si no se logro, devolver con warning
  warning("No se pudo generar set sin letras confundibles consecutivas tras ",
          max_iter, " intentos. Usando ultimo intento.")
  letras
}

#' @keywords internal
.generar_trial_ospan <- function(n_trial, set_size, complejidad_operacion,
                                  estimulo_memoria, pool_letras,
                                  pares_confundibles, refinar_estimulos,
                                  max_iter) {

  # 1) Generar letras del set (refinamiento: no repetidas, no confundibles)
  if (estimulo_memoria == "letras") {
    estimulos <- if (refinar_estimulos)
      .generar_letras_set(set_size, pool_letras, pares_confundibles, max_iter)
    else sample(pool_letras, set_size, replace = TRUE)
  } else if (estimulo_memoria == "digitos") {
    estimulos <- as.character(sample(0:9, set_size, replace = FALSE))
  } else {  # palabras
    pool_pal <- c("MESA", "CASA", "SOPA", "LUNA", "PATO", "ROSA", "PERA",
                   "JUGO", "MAPA", "TINA", "ROCA", "PINO")
    estimulos <- sample(pool_pal, set_size, replace = FALSE)
  }

  # 2) Generar set_size operaciones, balanceadas V/F (~50/50)
  ops <- vector("list", set_size)
  # Decidir V/F balanceado
  n_v <- ceiling(set_size / 2)
  veracidades <- sample(c(rep(TRUE, n_v), rep(FALSE, set_size - n_v)))

  for (k in seq_len(set_size)) {
    # Refinamiento: regenerar si la operacion es trivial
    iter <- 0L
    repeat {
      iter <- iter + 1L
      o <- .generar_operacion(complejidad_operacion)
      es_trivial <- if (refinar_estimulos)
        .es_operacion_trivial(o$str, o$valor_verdadero) else FALSE
      if (!es_trivial || iter >= max_iter) break
    }

    es_verdadera <- veracidades[k]
    if (es_verdadera) {
      valor_dado <- o$valor_verdadero
    } else {
      # Refinamiento: distractor plausible (cerca del verdadero, no igual)
      diferencia <- if (refinar_estimulos)
        sample(c(-3L, -2L, -1L, 1L, 2L, 3L), 1L)
      else sample(c(-10L:-1L, 1L:10L), 1L)
      valor_dado <- o$valor_verdadero + diferencia
    }
    ops[[k]] <- list(
      operacion_str   = o$str,
      valor_verdadero = o$valor_verdadero,
      valor_dado      = valor_dado,
      es_verdadera    = es_verdadera
    )
  }

  # 3) Construir data.frames de salida
  detalle <- do.call(rbind, lapply(seq_len(set_size), function(k) {
    data.frame(
      n_trial            = n_trial,
      nivel_dificultad   = paste0("Set-", set_size),
      set_size           = set_size,
      posicion_en_set    = k,
      operacion_str      = ops[[k]]$operacion_str,
      valor_dado         = ops[[k]]$valor_dado,
      valor_verdadero    = ops[[k]]$valor_verdadero,
      es_verdadera       = ops[[k]]$es_verdadera,
      estimulo_memoria   = estimulos[k],
      dificultad_procedural = .calcular_dificultad_procedural(
                                  set_size,
                                  complejidad_operacion,
                                  abs(ops[[k]]$valor_verdadero)),
      stringsAsFactors = FALSE
    )
  }))

  resumen <- data.frame(
    n_trial          = n_trial,
    nivel_dificultad = paste0("Set-", set_size),
    set_size         = set_size,
    secuencia_estimulos = paste(estimulos, collapse = " "),
    n_operaciones_V  = sum(vapply(ops, function(o) o$es_verdadera, logical(1))),
    n_operaciones_F  = sum(vapply(ops, function(o) !o$es_verdadera, logical(1))),
    dificultad_procedural_media = mean(detalle$dificultad_procedural),
    stringsAsFactors = FALSE
  )

  list(detalle = detalle, resumen = resumen)
}


#' @keywords internal
.calcular_dificultad_procedural <- function(set_size, complejidad, magnitud) {
  # Score 0-1 que combina set_size + complejidad + magnitud
  comp_score <- switch(complejidad,
                        "simple" = 0.3, "media" = 0.6, "compleja" = 1.0)
  set_score  <- (set_size - 3L) / 4L   # 3->0, 7->1
  set_score  <- max(0, min(1, set_score))
  mag_score  <- min(1, magnitud / 30)  # ~30 es magnitud alta para op media
  round(0.5 * set_score + 0.3 * comp_score + 0.2 * mag_score, 3)
}


#' @keywords internal
.auditoria_procedural_ospan <- function(trials_df, trials_resumen_df) {

  n_trials <- nrow(trials_resumen_df)

  # Repeticion de letras dentro de un mismo trial
  rep_letras <- vapply(seq_len(n_trials), function(i) {
    secs <- strsplit(trials_resumen_df$secuencia_estimulos[i], " ")[[1]]
    length(secs) - length(unique(secs))
  }, integer(1))

  # Balance V/F dentro del nivel
  bal_vf <- vapply(seq_len(n_trials), function(i) {
    nv <- trials_resumen_df$n_operaciones_V[i]
    nf <- trials_resumen_df$n_operaciones_F[i]
    abs(nv - nf) / (nv + nf)  # 0 = perfecto, 1 = todo a un lado
  }, numeric(1))

  # Plausibilidad de distractores: |diff| <= 3 en operaciones falsas
  ops_falsas <- trials_df[!trials_df$es_verdadera, ]
  if (nrow(ops_falsas) > 0) {
    plausibilidad <- mean(abs(ops_falsas$valor_dado -
                                 ops_falsas$valor_verdadero) <= 3L)
  } else {
    plausibilidad <- NA_real_
  }

  # Operaciones triviales
  triviales <- vapply(seq_len(nrow(trials_df)), function(i) {
    .es_operacion_trivial(trials_df$operacion_str[i],
                          trials_df$valor_verdadero[i])
  }, logical(1))

  data.frame(
    metrica = c(
      "Trials totales",
      "Letras unicas dentro de cada trial (%)",
      "Balance V/F medio (0=perfecto, 1=todo a un lado)",
      "Distractores plausibles |diff|<=3 (%)",
      "Operaciones triviales detectadas"
    ),
    valor = c(
      n_trials,
      round(mean(rep_letras == 0L) * 100, 1),
      round(mean(bal_vf), 3),
      round(plausibilidad * 100, 1),
      sum(triviales)
    ),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
.cluster_dificultad_ospan <- function(trials_df, trials_resumen_df,
                                        n_clusters = 3L) {

  # Features procedurales por TRIAL (no por operacion)
  features <- data.frame(
    n_trial         = trials_resumen_df$n_trial,
    set_size        = trials_resumen_df$set_size,
    dif_proc_media  = trials_resumen_df$dificultad_procedural_media,
    n_operaciones_V = trials_resumen_df$n_operaciones_V,
    stringsAsFactors = FALSE
  )

  X <- scale(as.matrix(features[, c("set_size", "dif_proc_media")]))
  km <- kmeans(X, centers = n_clusters, nstart = 25L, iter.max = 50L)

  # Ordenar clusters por centroide de set_size (ascendente)
  centros_orig  <- km$centers[, "set_size"]
  orden_cluster <- order(centros_orig)
  mapa <- setNames(c("Bajo", "Medio", "Alto")[order(orden_cluster)],
                    seq_len(n_clusters))
  asignacion <- mapa[as.character(km$cluster)]

  # Tabla nivel teorico vs cluster procedural
  tab <- table(
    nivel_teorico = trials_resumen_df$nivel_dificultad,
    cluster_procedural = asignacion
  )

  list(
    asignacion = setNames(asignacion, features$n_trial),
    centros    = km$centers,
    resumen    = as.data.frame.matrix(tab)
  )
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_test_cognitivo <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test cognitivo procedural (SeMiLLa - quinto formato)\n")
  cat("===========================================================\n")
  cat("  Paradigma         : ", x$paradigma, "\n", sep = "")
  cat("  Niveles dificultad: ",
      paste(x$niveles_dificultad, collapse = ", "), "\n", sep = "")
  cat("  Trials por nivel  : ", x$n_trials_por_nivel, "\n", sep = "")
  cat("  Total trials      : ", nrow(x$trials_resumen), "\n", sep = "")
  cat("  Complejidad op    : ", x$config$complejidad_operacion, "\n", sep = "")
  cat("  Estimulo memoria  : ", x$config$estimulo_memoria, "\n", sep = "")
  cat("-----------------------------------------------------------\n")
  cat("  Validacion procedural:\n")
  print(x$validacion_procedural, row.names = FALSE)
  if (!is.null(x$cluster_summary)) {
    cat("\n  Concordancia nivel teorico x cluster procedural:\n")
    print(x$cluster_summary)
  }
  cat("-----------------------------------------------------------\n")
  cat("  Primer trial (ejemplo):\n")
  t1 <- x$trials[x$trials$n_trial == 1, ]
  for (k in seq_len(nrow(t1))) {
    cat("    Op ", k, ": ", t1$operacion_str[k], " = ",
        t1$valor_dado[k], " (", ifelse(t1$es_verdadera[k], "V", "F"),
        ", verdadero=", t1$valor_verdadero[k], ")  -> memorizar: '",
        t1$estimulo_memoria[k], "'\n", sep = "")
  }
  cat("    Recordar al final: ",
      x$trials_resumen$secuencia_estimulos[1], "\n", sep = "")
  cat("===========================================================\n\n")
  invisible(x)
}
