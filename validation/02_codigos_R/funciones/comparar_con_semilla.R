# =============================================================================
# comparar_con_semilla() - Concordancia entre psicometria empirica vs semantica
# =============================================================================
# Toma:
#   - empirico : objeto psicometria_extraida (de extraer_psicometria_pdf)
#   - escala   : objeto semilla con $efa (ensemble guardado)
# Computa:
#   - Correlacion alpha empirico vs alpha semantico (Pearson + Spearman + MAE)
#   - Correlacion Tucker (factor-level) vs Precision ensemble
#   - Correlacion item-level: carga convergente vs coherencia intra
#   - Tabla 2x2 items problematicos -> Cohen kappa + Precision/Sensibilidad/Especificidad
#
# Autor: Dr. Jose Ventura-Leon
# =============================================================================

comparar_con_semilla <- function(empirico,
                                 escala,
                                 modo = c("conservador", "sensible"),
                                 umbral_coherencia = "adaptivo",
                                 percentil_adaptivo = 0.15,
                                 umbral_por_dimension = NULL,
                                 logica_flagged       = NULL,
                                 umbral_consenso_ens  = NULL,
                                 umbral_carga_emp     = 0.40,
                                 dir_salida = NULL,
                                 prefijo    = "comparacion",
                                 verbose    = TRUE) {
  # === PRESETS ===
  # modo="sensible" (default):
  #     umbral_por_dimension=FALSE, logica="OR", consenso=0
  #     -> alta Sensibilidad, posible baja Esp
  # modo="conservador":
  #     umbral_por_dimension=TRUE, logica="AND", consenso=0.80
  #     -> alta Especificidad, posible menor Sens
  # Los argumentos individuales sobreescriben el preset.

  modo <- match.arg(modo)
  # "sensible":   OR, global, sin filtro consenso  -> alta Sens
  # "conservador": OR pero por_dim + consenso>=.70 -> reduce FP sin
  #                eliminar TP. AND era demasiado estricto en escalas con
  #                solapamiento conceptual.
  if (is.null(umbral_por_dimension))
    umbral_por_dimension <- (modo == "conservador")
  if (is.null(logica_flagged))
    logica_flagged <- "OR"  # AND era demasiado restrictivo en la practica
  if (is.null(umbral_consenso_ens))
    umbral_consenso_ens <- ifelse(modo == "conservador", 0.70, 0)

  if (!inherits(empirico, "psicometria_extraida"))
    stop("'empirico' debe venir de extraer_psicometria_pdf()")
  if (!inherits(escala, "semilla"))
    stop("'escala' debe ser un objeto SeMiLLa")
  if (is.null(escala$efa))
    stop("'escala' no tiene $efa. Asigna escala$efa <- precision_clasificacion(..., metodo='ensemble')")

  # ---- 1. ALPHA: empirico vs semantico --------------------------------------
  if (verbose) cat("\n[1/4] Comparando alpha por dimension...\n")

  # Mapear dimensiones empiricas a las de semilla:
  # empirico$dimensiones$codigo + nombre vs items$dimension de semilla
  # Convencion: matchear por NOMBRE COMPLETO de la dimension
  dims_emp <- empirico$dimensiones
  dims_sem <- unique(escala$items$dimension)

  # Helper: matching tolerante (busca codigo o nombre dentro del string de la dim_sem)
  match_dim <- function(emp_codigo, emp_nombre, dims_sem) {
    # 1) match exacto contra nombre completo
    hit <- dims_sem[dims_sem == emp_nombre]
    if (length(hit) > 0) return(hit[1])
    # 2) match por codigo dentro del nombre (ej: '...(SEA)' contiene SEA)
    hit <- dims_sem[grepl(paste0("\\b", emp_codigo, "\\b"), dims_sem, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
    # 3) match por nombre dentro del nombre (substring)
    hit <- dims_sem[grepl(emp_nombre, dims_sem, fixed = TRUE, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
    NA_character_
  }
  dims_emp$nombre_sem <- mapply(match_dim,
                                dims_emp$codigo,
                                dims_emp$nombre,
                                MoreArgs = list(dims_sem = dims_sem),
                                USE.NAMES = FALSE)

  # Determinar la metrica empirica
  alpha_disponible <- !all(is.na(dims_emp$alpha))
  omega_disponible <- "omega" %in% names(dims_emp) && !all(is.na(dims_emp$omega))

  # Respetar preferencia desde empirico$meta$tipo_metrica_preferida si existe
  preferida <- if (!is.null(empirico$meta$tipo_metrica_preferida))
    empirico$meta$tipo_metrica_preferida else "alpha"

  # Si omega esta disponible Y se prefiere omega -> usar omega
  if (omega_disponible && preferida == "omega") {
    alpha_disponible <- FALSE  # forzar caer en la rama omega abajo
    if (verbose) cat("  [metrica] omega es preferida y esta disponible -> usando omega\n")
  }

  if (alpha_disponible) {
    metrica_emp_tipo <- "alpha"
    metrica_emp_val  <- dims_emp$alpha
  } else if (omega_disponible) {
    metrica_emp_tipo <- "omega"
    metrica_emp_val  <- dims_emp$omega
    if (verbose) cat("  Nota: paper reporta omega (no alpha). Usando omega-omega comparison.\n")
  } else {
    metrica_emp_tipo <- "ninguna"
    metrica_emp_val  <- rep(NA_real_, nrow(dims_emp))
  }

  # Fiabilidad semantica — usar omega_semantico cuando el paper reporta omega
  # (comparacion homogenea omega-omega), si no usar alpha_semantico
  if (metrica_emp_tipo == "omega") {
    om_sem <- omega_semantico(escala, verbose = FALSE)
    alpha_sem <- setNames(om_sem$omega_semantico, om_sem$dimension)
  } else {
    fia <- fiabilidad_semantica(escala, verbose = FALSE)
    alpha_sem <- setNames(fia$alpha_dimensiones$alpha_semantico,
                          fia$alpha_dimensiones$dimension)
  }

  df_alpha <- data.frame(
    codigo_emp = dims_emp$codigo,
    dim_emp    = dims_emp$nombre,
    dim_sem    = dims_emp$nombre_sem,
    alpha_emp  = metrica_emp_val,
    alpha_sem  = unname(alpha_sem[dims_emp$nombre_sem]),
    tipo_emp   = metrica_emp_tipo,
    stringsAsFactors = FALSE
  )

  ok <- complete.cases(df_alpha$alpha_emp, df_alpha$alpha_sem)
  if (sum(ok) >= 2) {
    r_alpha_pearson  <- cor(df_alpha$alpha_emp[ok], df_alpha$alpha_sem[ok], method = "pearson")
    r_alpha_spearman <- cor(df_alpha$alpha_emp[ok], df_alpha$alpha_sem[ok], method = "spearman")
    mae_alpha        <- mean(abs(df_alpha$alpha_emp[ok] - df_alpha$alpha_sem[ok]))
  } else {
    r_alpha_pearson  <- NA_real_
    r_alpha_spearman <- NA_real_
    mae_alpha        <- NA_real_
  }

  # ---- 2. TUCKER (factor-level) vs PRECISION ensemble -----------------------
  if (verbose) cat("[2/4] Comparando Tucker vs precision ensemble...\n")
  prec_dim <- escala$efa$precision_por_dimension
  precision_sem <- setNames(prec_dim$Precision / 100, prec_dim$Dimension)

  df_struct <- data.frame(
    codigo_emp = dims_emp$codigo,
    dim_emp    = dims_emp$nombre,
    tucker     = dims_emp$tucker,
    precision  = unname(precision_sem[dims_emp$nombre_sem]),
    stringsAsFactors = FALSE
  )

  ok_s <- complete.cases(df_struct$tucker, df_struct$precision)
  if (sum(ok_s) >= 2) {
    r_struct <- cor(df_struct$tucker[ok_s], df_struct$precision[ok_s])
  } else {
    r_struct <- NA_real_
  }

  # ---- 3. ITEM-LEVEL: carga convergente vs coherencia intra -----------------
  if (verbose) cat("[3/4] Comparando item-level...\n")
  coh <- analizar_coherencia(escala, verbose = FALSE)
  coh_item <- coh$datos_por_item
  names(coh_item) <- c("dimension_sem", "codigo", "similitud_intra")

  # ---- 3b. UMBRAL DE COHERENCIA: GLOBAL o POR DIMENSION ---------------------
  if (identical(umbral_coherencia, "adaptivo")) {
    if (umbral_por_dimension) {
      # Q-percentil DENTRO de cada dimension (mas justo para escalas con
      # dimensiones de coherencia heterogenea, ej. DASS-21 o UWES-9)
      dims_unicas <- unique(coh_item$dimension_sem)
      umbrales_dim <- sapply(dims_unicas, function(d) {
        x <- coh_item$similitud_intra[coh_item$dimension_sem == d]
        u <- as.numeric(quantile(x, percentil_adaptivo, na.rm = TRUE))
        if (!is.na(u) && u > 0.60) u <- 0.50
        u
      })
      names(umbrales_dim) <- dims_unicas
      coh_item$umbral_local <- unname(umbrales_dim[coh_item$dimension_sem])
      umbral_efectivo <- mean(umbrales_dim, na.rm = TRUE) # reportar media
      items_lowcoh <- coh_item$codigo[
        !is.na(coh_item$similitud_intra) &
        coh_item$similitud_intra < coh_item$umbral_local
      ]
      if (verbose) cat(sprintf("  Umbral por dimension (Q%.0f, media): %.3f\n",
                               percentil_adaptivo*100, umbral_efectivo))
    } else {
      # Global (todas las dims juntas)
      umbral_efectivo <- as.numeric(
        stats::quantile(coh_item$similitud_intra,
                        probs = percentil_adaptivo, na.rm = TRUE))
      if (umbral_efectivo > 0.60) umbral_efectivo <- 0.50
      items_lowcoh <- coh_item$codigo[coh_item$similitud_intra < umbral_efectivo]
      if (verbose) cat(sprintf("  Umbral coherencia global Q%.0f: %.3f\n",
                               percentil_adaptivo*100, umbral_efectivo))
    }
  } else if (is.numeric(umbral_coherencia)) {
    umbral_efectivo <- umbral_coherencia
    items_lowcoh <- coh_item$codigo[coh_item$similitud_intra < umbral_efectivo]
    if (verbose) cat(sprintf("  Umbral coherencia absoluto: %.3f\n", umbral_efectivo))
  } else {
    stop("umbral_coherencia debe ser 'adaptivo' o un numero")
  }

  # ---- 3c. ITEMS MAL CLASIFICADOS (con filtro de consenso opcional) ---------
  items_mal_all <- escala$efa$items_mal_clasificados$Codigo
  if (umbral_consenso_ens > 0 && !is.null(escala$efa$consenso)) {
    cons <- escala$efa$consenso
    items_firmes <- cons$Codigo[cons$Consenso >= umbral_consenso_ens]
    items_mal <- intersect(items_mal_all, items_firmes)
    if (verbose) cat(sprintf("  Filtro consenso ensemble >= %.2f: %d -> %d items\n",
                             umbral_consenso_ens, length(items_mal_all), length(items_mal)))
  } else {
    items_mal <- items_mal_all
  }

  # ---- 3d. COMBINACION OR/AND ----------------------------------------------
  flagged_sem <- if (logica_flagged == "AND") {
    intersect(items_mal, items_lowcoh)
  } else {
    unique(c(items_mal, items_lowcoh))
  }
  if (verbose) cat(sprintf("  Logica '%s': %d items flagged\n",
                           logica_flagged, length(flagged_sem)))

  df_items <- merge(empirico$items, coh_item, by = "codigo", all.x = TRUE)
  df_items$flagged_sem <- df_items$codigo %in% flagged_sem

  # ---- 3e. SCORE CONTINUO de problematicidad (0-1) --------------------------
  # Combina 3 senales normalizadas:
  #   s1: 1 si esta en items_mal con consenso alto, sino 0
  #   s2: max(0, 1 - similitud_intra / umbral)  --> mientras menos coherencia, mas score
  #   s3: 1 si la carga empirica < umbral_carga_emp (cuando esta disponible)
  s1 <- as.numeric(df_items$codigo %in% items_mal_all)  # base sin filtro de consenso
  if (!is.null(escala$efa$consenso)) {
    cons <- escala$efa$consenso
    cons_map <- setNames(cons$Consenso, cons$Codigo)
    s1 <- s1 * unname(cons_map[df_items$codigo])
    s1[is.na(s1)] <- 0
  }
  umb_ref <- if (umbral_por_dimension)
    df_items$umbral_local[match(df_items$codigo, coh_item$codigo)] else umbral_efectivo
  umb_ref[is.na(umb_ref) | umb_ref == 0] <- umbral_efectivo
  s2 <- pmax(0, 1 - df_items$similitud_intra / umb_ref)
  s2[is.na(s2)] <- 0
  s3 <- as.numeric(!is.na(df_items$carga_convergente) &
                     df_items$carga_convergente < umbral_carga_emp)
  df_items$score_problematicidad <- pmax(s1, s2, s3)

  # Si el paper no marcó explicitamente, usar carga<.40 como umbral
  if (any(is.na(df_items$problematico))) {
    df_items$problematico[is.na(df_items$problematico)] <-
      !is.na(df_items$carga_convergente[is.na(df_items$problematico)]) &
      df_items$carga_convergente[is.na(df_items$problematico)] < 0.40
  }

  ok_i <- complete.cases(df_items$carga_convergente, df_items$similitud_intra)
  if (sum(ok_i) >= 3) {
    r_carga_coh <- cor(df_items$carga_convergente[ok_i],
                       df_items$similitud_intra[ok_i])
  } else {
    r_carga_coh <- NA_real_
  }

  # ---- 4. TABLA 2x2: PROBLEMATICO EMP vs FLAGGED SEM ------------------------
  if (verbose) cat("[4/4] Calculando concordancia 2x2 (kappa, Sens/Esp/Prec)...\n")
  # Forzar factores con ambos niveles para garantizar tabla 2x2 completa
  emp_fac <- factor(ifelse(df_items$problematico, "Problem", "OK"),
                    levels = c("OK", "Problem"))
  sem_fac <- factor(ifelse(df_items$flagged_sem,  "Problem", "OK"),
                    levels = c("OK", "Problem"))
  tab <- table(empirico = emp_fac, semantico = sem_fac)

  TN <- tab["OK","OK"]; FP <- tab["OK","Problem"]
  FN <- tab["Problem","OK"]; TP <- tab["Problem","Problem"]

  sens <- if ((TP+FN) > 0) TP/(TP+FN) else NA_real_   # recall sobre problematicos
  esp  <- if ((TN+FP) > 0) TN/(TN+FP) else NA_real_   # especificidad
  prec <- if ((TP+FP) > 0) TP/(TP+FP) else NA_real_   # precision
  acc  <- (TP+TN) / sum(tab)
  f1   <- if (!is.na(sens) && !is.na(prec) && (sens+prec) > 0)
            2*sens*prec/(sens+prec) else NA_real_

  kappa <- if (requireNamespace("psych", quietly = TRUE) &&
               nrow(tab) == 2 && ncol(tab) == 2 && sum(tab) > 0) {
    suppressWarnings(psych::cohen.kappa(tab)$kappa)
  } else NA_real_

  # ---- ARMAR RESULTADO ------------------------------------------------------
  resumen <- list(
    escala_nombre = empirico$escala$nombre,
    n_empirico    = empirico$escala$n_participantes,
    n_items       = nrow(df_items),
    n_dimensiones = nrow(dims_emp),
    metrica_emp_tipo  = metrica_emp_tipo,
    modo              = modo,
    logica_flagged    = logica_flagged,
    umbral_por_dim    = umbral_por_dimension,
    umbral_consenso_ens = umbral_consenso_ens,
    umbral_coherencia = umbral_efectivo,

    alpha_pearson  = r_alpha_pearson,
    alpha_spearman = r_alpha_spearman,
    alpha_mae      = mae_alpha,
    alpha_mean_emp = mean(df_alpha$alpha_emp[ok], na.rm = TRUE),
    alpha_mean_sem = mean(df_alpha$alpha_sem[ok], na.rm = TRUE),

    tucker_precision_r = r_struct,
    carga_coh_r        = r_carga_coh,

    kappa       = kappa,
    accuracy    = acc,
    sensibilidad = sens,
    especificidad = esp,
    precision    = prec,
    f1           = f1,

    tabla_2x2    = tab,
    df_alpha     = df_alpha,
    df_struct    = df_struct,
    df_items     = df_items[, c("codigo","dimension","carga_convergente",
                                "similitud_intra","problematico","flagged_sem")]
  )
  class(resumen) <- c("comparacion_psicometrica", "list")

  # ---- GUARDAR ARTEFACTOS (opcional) ----------------------------------------
  if (!is.null(dir_salida)) {
    if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)
    write.csv(df_alpha,  file.path(dir_salida, paste0(prefijo, "_alpha.csv")),
              row.names = FALSE, fileEncoding = "UTF-8")
    write.csv(df_struct, file.path(dir_salida, paste0(prefijo, "_estructura.csv")),
              row.names = FALSE, fileEncoding = "UTF-8")
    write.csv(resumen$df_items, file.path(dir_salida, paste0(prefijo, "_items.csv")),
              row.names = FALSE, fileEncoding = "UTF-8")
    # Fila resumen 1-linea para el reporte global
    fila <- data.frame(
      escala    = resumen$escala_nombre,
      n         = resumen$n_empirico,
      n_items   = resumen$n_items,
      n_dim     = resumen$n_dimensiones,
      alpha_r   = round(resumen$alpha_pearson, 3),
      alpha_mae = round(resumen$alpha_mae,     3),
      carga_coh_r = round(resumen$carga_coh_r, 3),
      kappa     = round(resumen$kappa,         3),
      sens      = round(resumen$sensibilidad,  3),
      esp       = round(resumen$especificidad, 3),
      prec      = round(resumen$precision,     3),
      f1        = round(resumen$f1,            3),
      acc       = round(resumen$accuracy,      3)
    )
    write.csv(fila, file.path(dir_salida, paste0(prefijo, "_fila_resumen.csv")),
              row.names = FALSE, fileEncoding = "UTF-8")
  }

  resumen
}


#' @export
print.comparacion_psicometrica <- function(x, ...) {
  cat("\n===========================================================\n")
  cat("  CONCORDANCIA: ", x$escala_nombre,
      "  (n=", x$n_empirico, ", k=", x$n_items, " items)\n", sep = "")
  cat("  Modo: ", x$modo, " | Logica: ", x$logica_flagged,
      " | Por-dim: ", x$umbral_por_dim,
      " | Consenso minimo: ", x$umbral_consenso_ens, "\n", sep = "")
  cat("  Metrica empirica: ", x$metrica_emp_tipo,
      " | Umbral coherencia: ", sprintf("%.3f", x$umbral_coherencia), "\n", sep = "")
  cat("===========================================================\n")
  cat(sprintf("  %s emp vs sem    Pearson r : %+.3f\n",
              toupper(substr(x$metrica_emp_tipo,1,5)), x$alpha_pearson))
  cat(sprintf("                      Spearman  : %+.3f\n",  x$alpha_spearman))
  cat(sprintf("                      MAE       : %.3f\n",   x$alpha_mae))
  cat(sprintf("                      mean emp  : %.3f\n",   x$alpha_mean_emp))
  cat(sprintf("                      mean sem  : %.3f\n",   x$alpha_mean_sem))
  cat(sprintf("  Tucker vs Precision r         : %+.3f\n",  x$tucker_precision_r))
  cat(sprintf("  Carga conv ~ Coherencia (item): %+.3f\n",  x$carga_coh_r))
  cat("-----------------------------------------------------------\n")
  cat("  ITEMS PROBLEMATICOS (gold = empirico)\n")
  cat(sprintf("  Cohen kappa     : %+.3f\n", x$kappa))
  cat(sprintf("  Accuracy        : %.3f\n",  x$accuracy))
  cat(sprintf("  Sensibilidad    : %.3f  (recall sobre items problematicos)\n", x$sensibilidad))
  cat(sprintf("  Especificidad   : %.3f\n", x$especificidad))
  cat(sprintf("  Precision (PPV) : %.3f\n", x$precision))
  cat(sprintf("  F1              : %.3f\n", x$f1))
  cat("\n  Tabla 2x2:\n")
  print(x$tabla_2x2)
  cat("===========================================================\n")
  invisible(x)
}
