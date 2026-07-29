# =============================================================================
#  SeMiLLa · OPTIMIZACION DIRIGIDA POR EL STRESS TEST (optimizar_estres)
# -----------------------------------------------------------------------------
#  estres_escala() es diagnostico; este modulo agrega el LOOP que, cuando el
#  stress test marca la escala como VULNERABLE/FRAGIL, toma los items que
#  colapsan primero (bajo su peor sesgo), los REESCRIBE con una instruccion
#  anti-sesgo, recalcula embeddings + deseabilidad y RE-CORRE el stress test,
#  hasta max_iteraciones, quedandose con la version de menor fragilidad.
#  Se activa con optimizar_estres = TRUE (default) en estres_escala() y en
#  semilla(). Es generico: sirve para cualquier sesgo que salga mal.
# =============================================================================

# Instruccion de reescritura segun el sesgo que hace colapsar al item.
#' @keywords internal
.instruccion_anti_sesgo <- function(sesgo) {
  switch(as.character(sesgo),
    "deseabilidad" = paste0(
      "OBJETIVO ANTI-DESEABILIDAD SOCIAL: reescribe el item para BAJAR su ",
      "deseabilidad. Describe una conducta CONCRETA y OBSERVABLE con un costo ",
      "personal real (tiempo, esfuerzo, incomodidad, renunciar a un beneficio), ",
      "en una situacion especifica. EVITA autoatribuciones globales halagadoras ",
      "(del tipo 'soy honesto', 'valoro la justicia'). Debe ser creible que una ",
      "persona promedio responda que NO lo hace sin quedar mal.\n"),
    "aquiescencia" = paste0(
      "OBJETIVO ANTI-AQUIESCENCIA: reescribe el item para que NO sea facil ",
      "asentir por inercia. Usa un anclaje conductual concreto y especifico; ",
      "evita afirmaciones vagas, generales o socialmente obvias con las que ",
      "casi todos estarian de acuerdo.\n"),
    "extremos" = ,
    "punto_medio" = paste0(
      "OBJETIVO ANTI-ESTILO DE RESPUESTA: reescribe el item para que capte ",
      "gradaciones reales de conducta; evita enunciados absolutos o triviales ",
      "que empujen a respuestas extremas o de punto medio.\n"),
    "descuido" = ,
    "lineas_rectas" = paste0(
      "OBJETIVO ANTI-DESCUIDO: reescribe el item MAS corto y claro, con UNA ",
      "sola idea y vocabulario simple, para reducir respuestas por descuido o ",
      "en linea recta.\n"),
    ""
  )
}

# Loop de optimizacion. Devuelve un objeto semilla_estres (diagnostico final de
# la MEJOR escala) con $optimizacion (historial) y $escala_final (la escala
# mejorada, para seguir usandola).
#' @keywords internal
.optimizar_por_estres <- function(x, res0, params_estres,
                                  max_iteraciones = 5L, n_reescribir = NULL,
                                  n_rep_intermedio = 30L, n_rep_final = 60L,
                                  umbral_redundancia = 0.70,
                                  modelo = "gpt-4.1-mini", poblacion = NULL,
                                  api_key = "", seed = 2026, verbose = TRUE) {

  p <- nrow(x$items)
  if (is.null(n_reescribir)) n_reescribir <- max(1L, min(5L, ceiling(p * 0.15)))

  # Contexto de generacion (robusto a escalas cargadas por el usuario)
  concepto  <- x$concepto$concepto %||% x$metadata$concepto_original %||% "constructo"
  dims_def  <- x$concepto$dimensiones
  carac     <- x$concepto$caracteristicas
  idioma    <- x$metadata$idioma %||% "es"
  poblacion <- poblacion %||% x$metadata$poblacion
  complej   <- x$metadata$complejidad_linguistica %||% "intermedio"
  tiporesp  <- x$metadata$tipo_escala_respuesta %||% "frecuencia"
  maxpal    <- x$metadata$max_palabras %||%
    switch(complej, minimo = 10L, basico = 12L, intermedio = 18L, avanzado = 25L)
  openai    <- .configurar_openai(api_key)

  correr_estres <- function(esc, n_rep, verbose = FALSE) {
    do.call(estres_escala, c(list(x = esc, optimizar_estres = FALSE,
             n_rep = n_rep, api_key = api_key, seed = seed, verbose = verbose),
             params_estres))
  }

  best_esc <- x; best_res <- res0
  historial <- data.frame(iteracion = 0L,
    indice_global = round(res0$indice_global, 3),
    veredicto = res0$veredicto, items_reescritos = 0L, stringsAsFactors = FALSE)
  reescritos_total <- character(0)
  sin_mejora <- 0L

  if (verbose) {
    cat("\n=============================================================\n")
    cat(" OPTIMIZACION DIRIGIDA POR EL STRESS TEST (optimizar_estres)\n")
    cat("=============================================================\n")
    cat(sprintf(" Indice inicial: %.3f | %s\n", res0$indice_global, res0$veredicto))
    cat(sprintf(" Reescribe hasta %d item(s) por iteracion, max %d iteraciones.\n",
                n_reescribir, max_iteraciones))
    utils::flush.console()
  }

  for (iter in seq_len(max_iteraciones)) {
    if (grepl("^ROBUSTA", best_res$veredicto)) break
    fi <- best_res$fragilidad_items
    if (is.null(fi) || !nrow(fi)) break
    fi  <- fi[order(-fi$caida_max), , drop = FALSE]
    sel <- utils::head(fi[fi$caida_max > 0, , drop = FALSE], n_reescribir)
    if (!nrow(sel)) break

    esc_new <- best_esc
    reescritos_iter <- character(0)
    for (r in seq_len(nrow(sel))) {
      codigo <- as.character(sel$codigo[r])          # i01..
      idx <- match(codigo, sprintf("i%02d", seq_len(p)))
      if (is.na(idx)) next
      dim_nombre <- as.character(best_esc$items$dimension[idx])
      def_dim <- dims_def[[dim_nombre]] %||% dim_nombre
      caract  <- carac[[dim_nombre]]
      extra   <- .instruccion_anti_sesgo(sel$sesgo_critico[r])
      nuevo <- tryCatch(.generar_items_dimension(
        openai = openai, concepto = concepto, dimension = dim_nombre,
        definicion_dim = def_dim, caracteristicas = caract, n_items = 1L,
        idioma = idioma, poblacion = poblacion, modelo = modelo,
        items_evitar = esc_new$items$item, complejidad_linguistica = complej,
        tipo_escala_respuesta = tiporesp, max_palabras = maxpal,
        incluir_inversos = FALSE, instruccion_extra = extra),
        error = function(e) NULL)
      if (!is.null(nuevo) && nrow(nuevo) >= 1) {
        txt <- trimws(nuevo$item[1])
        if (nzchar(txt) && !(tolower(txt) %in% tolower(esc_new$items$item))) {
          esc_new$items$item[idx] <- txt
          reescritos_iter <- c(reescritos_iter, codigo)
        }
      }
    }
    if (!length(reescritos_iter)) {
      sin_mejora <- sin_mejora + 1L
      if (sin_mejora >= 2L) break else next
    }

    # Recalcular embeddings + similitud y deseabilidad (la cache ahorra los
    # items no cambiados: mismo texto -> mismo vector / mismo prompt).
    emb <- tryCatch(obtener_embeddings(esc_new$items, api_key = api_key,
                                       verbose = FALSE), error = function(e) NULL)
    if (is.null(emb)) break
    esc_new$embeddings <- emb$embeddings
    esc_new$similitud  <- emb$similitud

    # GUARDIA ANTI-REDUNDANCIA: la instruccion anti-sesgo estrecha el espacio y
    # puede producir un item casi identico a otro ya existente. Si un item
    # reescrito queda demasiado similar (coseno) a cualquier otro, se REVIERTE al
    # texto anterior (mejor un item robustecible que un gemelo).
    S <- as.matrix(esc_new$similitud)
    revertidos <- character(0)
    for (cod in reescritos_iter) {
      j <- match(cod, sprintf("i%02d", seq_len(p)))
      if (is.na(j)) next
      sim_max <- max(S[j, -j])
      if (is.finite(sim_max) && sim_max > umbral_redundancia) {
        esc_new$items$item[j] <- best_esc$items$item[j]   # revertir
        revertidos <- c(revertidos, cod)
      }
    }
    reescritos_iter <- setdiff(reescritos_iter, revertidos)
    if (length(revertidos)) {
      if (verbose) cat(sprintf("   (anti-redundancia: revertidos %s por gemelo)\n",
                               paste(revertidos, collapse = ",")))
      if (!length(reescritos_iter)) {
        sin_mejora <- sin_mejora + 1L
        if (sin_mejora >= 2L) break else next
      }
      emb <- tryCatch(obtener_embeddings(esc_new$items, api_key = api_key,
                                         verbose = FALSE), error = function(e) NULL)
      if (is.null(emb)) break
      esc_new$embeddings <- emb$embeddings
      esc_new$similitud  <- emb$similitud
    }
    des <- tryCatch(calificar_deseabilidad(esc_new, api_key = api_key,
             modelo = modelo, poblacion = poblacion, seed = seed, verbose = FALSE),
             error = function(e) NULL)
    if (!is.null(des) && length(des$deseabilidad) == p)
      esc_new$compuerta$deseabilidad$deseabilidad <- des$deseabilidad

    res_new <- correr_estres(esc_new, n_rep_intermedio, verbose = FALSE)
    mejora  <- res_new$indice_global < best_res$indice_global - 0.005

    if (verbose) cat(sprintf(
      " [iter %d] reescritos %d {%s} -> indice %.3f (%s)%s\n",
      iter, length(reescritos_iter), paste(reescritos_iter, collapse = ","),
      res_new$indice_global, res_new$veredicto,
      if (mejora) "  [MEJORA]" else "  (sin mejora)"))

    historial <- rbind(historial, data.frame(iteracion = iter,
      indice_global = round(res_new$indice_global, 3),
      veredicto = res_new$veredicto, items_reescritos = length(reescritos_iter),
      stringsAsFactors = FALSE))

    if (mejora) {
      best_esc <- esc_new; best_res <- res_new
      reescritos_total <- unique(c(reescritos_total, reescritos_iter))
      sin_mejora <- 0L
    } else {
      sin_mejora <- sin_mejora + 1L
      if (sin_mejora >= 2L) {
        if (verbose) cat(" Sin mejora en 2 iteraciones: se detiene.\n"); break
      }
    }
  }

  # Diagnostico final completo (n_rep_final) sobre la MEJOR escala.
  final <- correr_estres(best_esc, n_rep_final, verbose = FALSE)
  final$optimizacion <- list(
    aplicada = TRUE,
    iteraciones = nrow(historial) - 1L,
    indice_inicial = res0$indice_global, indice_final = final$indice_global,
    veredicto_inicial = res0$veredicto, veredicto_final = final$veredicto,
    items_reescritos = reescritos_total, historial = historial)
  final$escala_final <- best_esc

  if (verbose) {
    cat("-------------------------------------------------------------\n")
    cat(sprintf(
      " OPTIMIZACION: indice %.3f -> %.3f | %s -> %s | %d item(s) reescrito(s)\n",
      res0$indice_global, final$indice_global, res0$veredicto,
      final$veredicto, length(reescritos_total)))
    print(final)
  }
  final
}
