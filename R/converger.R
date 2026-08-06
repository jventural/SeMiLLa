# =============================================================================
# SeMiLLa - Bucle de convergencia psicometrica
# =============================================================================
#
# La compuerta DIAGNOSTICA y optimizar_para_campo() CORRIGE la redaccion. Esta
# funcion cierra el circulo: reune en un solo diagnostico lo que hoy vive
# disperso (compuerta, validez de contenido y discriminacion), reescribe SOLO
# los items que fallan, y repite hasta alcanzar un veredicto aplicable o agotar
# el presupuesto de iteraciones.
#
# Regla de salida: NUNCA se devuelve una escala en "NO APLICAR TODAVIA" sin
# alternativa. Si reescribir no basta, se construye la version de ELECCION
# FORZADA y se entrega como cambio de FORMATO, no como reparacion.
#
# Calibrado con dos escalas reales el 2026-08-03 (ansiedad ante la estadistica y
# aprendizaje autonomo, 18-20 items). Decisiones que salieron de esos datos:
#
#  * SE MANTIENE la V de Aiken dentro del bucle. Se probaron las dos variantes
#    sobre la misma escala: CON Aiken 11.0 min / 3 iteraciones / para sola;
#    SIN Aiken 15.9 min / 5 iteraciones / gira en vacio y acaba devolviendo la
#    escala de partida. Quitarla ahorra 2.2 min por pasada pero el score pierde
#    resolucion, el bucle deja de distinguir mejoras y corre MAS vueltas. Es el
#    unico control que mide si el item SIGUE MIDIENDO el constructo.
#
#  * NO se cambia la estrategia de redaccion a mitad del bucle. Se probo pedir
#    "conducta concreta con costo explicito" cuando regenerar no bastaba: en las
#    dos veces que se aplico degrado la escala (score 2005 -> 978 -> 969) y los
#    items marcados subieron de 5 a 9. Rompe la homogeneidad de longitud y
#    registro con el resto de la escala.
#
#  * PARADA POR AGOTAMIENTO. Con los vetos acumulados el LLM deja de producir
#    alternativas: en la iteracion 4 devolvia "sin reemplazo viable" para casi
#    todos los items. Subir el tope a 10 iteraciones no aporta nada.
# =============================================================================


#' @title Convergencia psicometrica iterativa
#'
#' @description
#' Diagnostica la escala con los tres ejes de la compuerta MAS la validez de
#' contenido (V de Aiken con jueces LLM) y la discriminacion semantica,
#' reescribe unicamente los items que fallan, y repite hasta alcanzar
#' \code{objetivo} o agotar \code{max_iteraciones}.
#'
#' Cada version se puntua y se devuelve la MEJOR, no la ultima: el bucle puede
#' empeorar la escala mientras la corrige, y sin esta proteccion devolveria una
#' version peor que la de partida (verificado en las tres corridas de
#' calibracion).
#'
#' @param x Objeto \code{semilla} con \code{$items} y \code{$embeddings}.
#' @param api_key Clave del proveedor LLM.
#' @param modelo Modelo LLM (default "gpt-4.1-mini").
#' @param paciencia Vueltas seguidas sin mejorar tras las que se detiene el
#'   ciclo. Cada vuelta recalcula la compuerta completa, la V de Aiken con
#'   jueces LLM y la discriminacion semantica: del orden de 10 minutos en una
#'   escala de 24 items. Sin este corte el bucle agotaba \code{max_iteraciones}
#'   aunque el puntaje bajara en todas. Use \code{Inf} para el comportamiento
#'   anterior.
#' @param max_iteraciones Tope de vueltas (default 5). No conviene subirlo: el
#'   LLM se agota antes, ver la nota de calibracion arriba.
#' @param objetivo Veredicto con el que se detiene: \code{"APLICAR CON CAUTELA"}
#'   (default), \code{"LISTA PARA CAMPO"} o \code{"APLICAR COMO ESCALA GLOBAL"}.
#' @param n Respondientes simulados por replica (default 280).
#' @param n_rep Replicas de la simulacion de estructura (default 30).
#' @param umbral_aiken V de Aiken minima por item (default 0.70).
#' @param umbral_ic Limite inferior del IC de la V minimo (default 0.50).
#' @param n_jueces Jueces LLM para la validez de contenido (default 10).
#' @param formato_si_falla Si \code{TRUE} (default) y el bucle no alcanza el
#'   objetivo, construye la version de eleccion forzada cuasi-ipsativa.
#' @param seed Semilla de la simulacion.
#' @param verbose Mostrar progreso.
#'
#' @return Lista con \code{escala} (la mejor version), \code{diagnostico},
#'   \code{historial}, \code{cambios} (item viejo -> nuevo y por que),
#'   \code{forced_choice}, \code{objetivo_alcanzado} y \code{minutos}.
#'
#' @seealso \code{\link{compuerta_pre_aplicacion}},
#'   \code{\link{optimizar_para_campo}}
#' @export
converger_escala <- function(x,
                             api_key,
                             modelo          = "gpt-4.1-mini",
                             max_iteraciones = 5L,
                             paciencia       = 2L,
                             objetivo        = c("APLICAR CON CAUTELA",
                                                 "LISTA PARA CAMPO",
                                                 "APLICAR COMO ESCALA GLOBAL"),
                             n               = 280,
                             n_rep           = 30,
                             umbral_aiken    = 0.70,
                             umbral_ic       = 0.50,
                             n_jueces        = 10,
                             formato_si_falla = TRUE,
                             seed            = 2026,
                             verbose         = TRUE) {

  objetivo <- match.arg(objetivo)
  if (is.null(x$items) || is.null(x$items$item))
    stop("'x' debe contener $items con la columna 'item'.")
  if (is.null(x$embeddings))
    stop("'x' debe traer $embeddings: ejecuta antes obtener_embeddings().")

  t_ini  <- Sys.time()
  openai <- .configurar_openai(api_key, modelo = modelo)
  crono  <- list()
  reloj  <- function(etq, expr) {
    t0 <- Sys.time(); v <- force(expr)
    s  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    crono[[length(crono) + 1L]] <<- data.frame(fase = etq, segundos = round(s, 1),
                                               stringsAsFactors = FALSE)
    if (verbose) cat(sprintf("    [%6.1f s] %s\n", s, etq))
    v
  }

  if (verbose) {
    cat("\n", .linea("="), "\n", sep = "")
    cat(.color_azul("  CONVERGENCIA PSICOMETRICA"), "\n")
    # El objetivo se GUARDA con el vocabulario antiguo (esta funcion ramifica
    # sobre esas cadenas), pero se MUESTRA con el de escenario, que es el que
    # el usuario ve en la compuerta. Si no, la misma meta aparece con dos
    # nombres distintos en la misma corrida.
    .obj_legible <- c("LISTA PARA CAMPO"           = "estructura ROBUSTA",
                      "APLICAR CON CAUTELA"        = "que deje de ser FRAGIL",
                      "APLICAR COMO ESCALA GLOBAL" = "utilizable con puntaje total")
    cat("  objetivo: ", .obj_legible[[objetivo]] %||% objetivo,
        " | max ", max_iteraciones, " iteraciones\n", sep = "")
    cat(.linea("="), "\n", sep = "")
  }

  # --- Diagnostico inicial ---------------------------------------------------
  if (verbose) cat("\n[ITER 0] diagnostico inicial\n")
  dg <- .diagnostico_convergencia(x, api_key, modelo, n, n_rep, seed,
                                  umbral_aiken, umbral_ic, n_jueces,
                                  TRUE, reloj, verbose)
  x$compuerta <- dg$compuerta
  aiken_ini   <- dg$aiken

  mejor <- list(escala = x, dg = dg, score = dg$score, iter = 0L,
                cambios = data.frame())
  # 'escenario' viaja junto al veredicto (2.9.16): el bucle sigue decidiendo con
  # las cadenas antiguas -esta funcion ramifica sobre ellas- pero quien lea el
  # historial tiene que poder mostrar el mismo vocabulario que la compuerta, o
  # la misma escala aparece descrita de dos formas en la misma pantalla.
  sin_mejora <- 0L   # vueltas seguidas que no superan a la mejor version
  historial <- data.frame(iteracion = 0L, veredicto = dg$compuerta$veredicto,
                          escenario = dg$compuerta$escenario %||% NA_character_,
                          n_marcados = nrow(dg$marcados), reemplazos = 0L,
                          rechazos_texto = 0L, score = round(dg$score, 2),
                          stringsAsFactors = FALSE)
  if (verbose)
    cat(sprintf("  -> %s | items marcados: %d | score: %.1f\n",
                dg$compuerta$veredicto, nrow(dg$marcados), dg$score))

  vetos <- character(0); it <- 0L; secas <- 0L

  while (it < max_iteraciones) {
    if (.veredicto_cumple(dg$compuerta$veredicto, objetivo)) {
      if (verbose) cat("\n  Objetivo alcanzado.\n"); break
    }
    it <- it + 1L
    if (verbose) cat(sprintf("\n[ITER %d] reescribir items marcados\n", it))

    mk <- dg$marcados
    if (nrow(mk) == 0) { if (verbose) cat("  Nada que reescribir.\n"); break }
    tope <- max(1L, floor(nrow(x$items) * 0.4))
    if (nrow(mk) > tope) mk <- mk[seq_len(tope), , drop = FALSE]

    fac <- dg$compuerta$redaccion$facetas_repetidas
    if (!is.null(fac) && nrow(fac) > 0)
      vetos <- unique(c(vetos, fac$nucleo_lexico))

    desea <- dg$compuerta$deseabilidad$deseabilidad
    if (!is.null(desea) && length(desea) != nrow(x$items)) desea <- NULL
    rango <- .rango_palabras_escala(x$items$item)
    emb   <- x$embeddings
    u_ac  <- suppressWarnings(as.numeric(
      dg$compuerta$redaccion$parametros$umbral_sem))
    u_ac  <- if (is.na(u_ac)) 0.70 else max(0.40, u_ac - 0.03)

    n_ok <- 0L; n_tx <- 0L; cambios <- data.frame(); t_reg <- Sys.time()
    for (r in seq_len(nrow(mk))) {
      j <- mk$idx[r]
      conservados <- setdiff(
        x$items$item[x$items$dimension == x$items$dimension[j]], x$items$item[j])
      viejo <- x$items$item[j]

      nuevo <- tryCatch(.reemplazar_item_dirigido(
        x = x, openai = openai, modelo = modelo, idx_item = j,
        motivo = mk$motivo[r], conductas_prohibidas = vetos, anti_halo = FALSE,
        poblacion = x$metadata$poblacion, rango_palabras = rango,
        umbral_redundancia = u_ac, embeddings_existentes = emb,
        items_dimension_conservados = conservados,
        deseabilidad_objetivo = .desea_dimension(desea, x$items, j),
        deseabilidad_otras    = .desea_otras_dim(desea, x$items, j)),
        error = function(e) NULL)

      emb_new <- NULL
      if (is.list(nuevo)) { emb_new <- nuevo$emb; nuevo <- nuevo$item }
      if (is.null(nuevo)) {
        if (verbose) cat(sprintf("    I%-3d sin reemplazo viable\n", j)); next
      }
      defecto <- .texto_mal_formado(nuevo)
      if (!is.na(defecto)) {
        n_tx <- n_tx + 1L
        if (verbose) cat(sprintf("    I%-3d rechazado (%s)\n", j, defecto)); next
      }

      x$items$item[j] <- nuevo
      if (!is.null(emb_new) && !is.null(emb) && length(emb_new) == ncol(emb))
        emb[j, ] <- emb_new
      n_ok <- n_ok + 1L
      cambios <- rbind(cambios, data.frame(
        iteracion = it, item = j, item_viejo = viejo, item_nuevo = nuevo,
        motivo = mk$motivo[r], stringsAsFactors = FALSE))
      if (verbose) cat(sprintf("    I%-3d -> %s\n", j, substr(nuevo, 1, 58)))
    }
    if (verbose)
      cat(sprintf("    [%6.1f s] %d reescritos, %d rechazados por redaccion\n",
                  as.numeric(difftime(Sys.time(), t_reg, units = "secs")),
                  n_ok, n_tx))

    if (n_ok == 0L) { if (verbose) cat("  Ningun reemplazo viable.\n"); break }
    secas <- if (n_ok <= max(1L, floor(nrow(mk) / 4))) secas + 1L else 0L
    if (secas >= 2L) {
      if (verbose) cat("  El LLM ya no produce alternativas nuevas: se detiene.\n")
      break
    }

    e2 <- reloj("re-embeddings", obtener_embeddings(x, api_key = api_key,
                                                    verbose = FALSE))
    x$embeddings <- e2$embeddings; x$similitud <- e2$similitud

    ultima <- (it == max_iteraciones)
    dg <- .diagnostico_convergencia(x, api_key, modelo, n, n_rep, seed,
                                    umbral_aiken, umbral_ic, n_jueces,
                                    ultima, reloj, verbose)
    x$compuerta <- dg$compuerta
    if (is.null(dg$aiken)) dg$aiken <- aiken_ini
    dg$score <- .score_convergencia(dg$compuerta, dg$aiken)

    historial <- rbind(historial, data.frame(
      iteracion = it, veredicto = dg$compuerta$veredicto,
      escenario = dg$compuerta$escenario %||% NA_character_,
      n_marcados = nrow(dg$marcados), reemplazos = n_ok,
      rechazos_texto = n_tx, score = round(dg$score, 2),
      stringsAsFactors = FALSE))
    if (verbose)
      cat(sprintf("  -> %s | marcados: %d | score: %.1f (mejor: %.1f)\n",
                  dg$compuerta$veredicto, nrow(dg$marcados), dg$score, mejor$score))

    if (dg$score > mejor$score) {
      mejor <- list(escala = x, dg = dg, score = dg$score, iter = it,
                    cambios = rbind(mejor$cambios, cambios))
      sin_mejora <- 0L
      if (verbose) cat("     (nueva mejor version)\n")
    } else {
      sin_mejora <- sin_mejora + 1L
      if (verbose)
        cat(sprintf("     (no mejora: se conserva la iteracion %d) [%d seguida(s)]\n",
                    mejor$iter, sin_mejora))
    }

    # ---- PARADA TEMPRANA (2.9.17) -------------------------------------------
    # Cada vuelta cuesta ~10 minutos: recalcula la compuerta entera (3 ejes con
    # LLM), la V de Aiken con jueces y la discriminacion semantica. Sin este
    # corte el ciclo agotaba las 5 aunque ninguna mejorase. Caso real (ansiedad
    # ante la estadistica, 24 items): score 977 -> 961 -> 965 -> 959 -> 965 ->
    # 954, cinco vueltas, 50.7 min, y se devolvio la version 0. Unos 30 minutos
    # gastados en versiones que ya se sabia que se iban a descartar.
    if (sin_mejora >= paciencia) {
      if (verbose)
        cat(sprintf(paste0("\n  Se detiene: %d vueltas seguidas sin mejorar.\n",
                           "  Reescribir mas no esta ayudando; se conserva la ",
                           "iteracion %d.\n"), sin_mejora, mejor$iter))
      break
    }
  }

  # --- Cierre: la salida nunca queda en rojo sin alternativa -----------------
  vfin      <- mejor$dg$compuerta$veredicto
  alcanzado <- .veredicto_cumple(vfin, objetivo)
  fc <- NULL
  if (!alcanzado && isTRUE(formato_si_falla)) {
    if (verbose) {
      cat("\n[FORMATO] Reescribir items no basto.\n")
      cat("  Se construye la version de ELECCION FORZADA (cuasi-ipsativa).\n")
    }
    dims <- unique(as.character(mejor$escala$items$dimension))
    desc <- mejor$escala$concepto$dimensiones
    desc <- if (is.list(desc)) unlist(desc[dims]) else NULL
    fc <- reloj("escala de eleccion forzada", tryCatch(
      generar_escala_forcedchoice(
        concepto = .extraer_concepto_str(mejor$escala), api_key = api_key,
        dimensiones = dims, descripcion_dimensiones = desc,
        poblacion = mejor$escala$metadata$poblacion,
        n_items_por_dimension = 6L, block_size = 4L, n_bloques = 12L,
        metodo = "most_least", idioma = "es", modelo = modelo,
        seed = seed, verbose = FALSE),
      error = function(e) NULL))
  }

  mins <- as.numeric(difftime(Sys.time(), t_ini, units = "mins"))
  tiempos <- if (length(crono)) do.call(rbind, crono) else data.frame()

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    print(historial, row.names = FALSE)
    cat(sprintf("\n  Mejor version: iteracion %d | %s\n", mejor$iter, vfin))
    cat(sprintf("  Objetivo '%s': %s\n", objetivo,
                if (alcanzado) "ALCANZADO" else "no alcanzado reescribiendo"))
    if (!is.null(fc)) cat("  Alternativa de formato: construida\n")
    cat(sprintf("  Tiempo total: %.1f min\n", mins))
    cat(.linea("-"), "\n", sep = "")
  }

  out <- list(escala = mejor$escala, diagnostico = mejor$dg,
              historial = historial, cambios = mejor$cambios,
              forced_choice = fc, objetivo_alcanzado = alcanzado,
              objetivo = objetivo, minutos = mins, tiempos = tiempos)
  class(out) <- c("semilla_convergencia", "list")
  out
}


# -----------------------------------------------------------------------------
#  Diagnostico unificado: una sola tabla de items marcados, cada uno con el
#  MOTIVO por el que falla. Ese motivo es lo que despues recibe el LLM, asi que
#  tiene que ser accionable, no una etiqueta.
# -----------------------------------------------------------------------------

#' @keywords internal
.diagnostico_convergencia <- function(x, api_key, modelo, n, n_rep, seed,
                                      umbral_aiken, umbral_ic, n_jueces,
                                      con_aiken, reloj, verbose) {
  if (verbose) cat("  DIAGNOSTICO\n")

  g <- reloj("compuerta (3 ejes)", compuerta_pre_aplicacion(
    x, api_key = api_key, modelo = modelo, n = n, n_rep = n_rep,
    seed = seed, verbose = FALSE))

  p <- nrow(x$items)
  marcados <- data.frame(idx = integer(0), motivo = character(0),
                         fuente = character(0), stringsAsFactors = FALSE)
  add <- function(idx, motivo, fuente) {
    idx <- unique(idx[!is.na(idx) & idx >= 1 & idx <= p])
    if (!length(idx)) return(invisible())
    marcados <<- rbind(marcados, data.frame(idx = idx, motivo = motivo,
                                            fuente = fuente,
                                            stringsAsFactors = FALSE))
  }

  fac <- g$redaccion$facetas_repetidas
  if (!is.null(fac) && nrow(fac) > 0) {
    for (i in seq_len(nrow(fac))) {
      idx <- suppressWarnings(as.integer(gsub("\\D", "",
        trimws(strsplit(fac$codigos[i], ",")[[1]]))))
      if (length(idx) > 2)
        add(idx[-(1:2)], paste0("faceta repetida (", fac$nucleo_lexico[i], ")"),
            "redaccion")
    }
  }
  gem <- g$redaccion$gemelos_llm
  if (!is.null(gem) && nrow(gem) > 0)
    add(gem$item2, "parafrasis gemela confirmada por juez LLM", "redaccion")

  des <- g$deseabilidad
  if (!is.null(des) && length(des$deseabilidad) == p) {
    d <- as.numeric(des$deseabilidad)
    for (dm in unique(x$items$dimension)) {
      k <- which(x$items$dimension == dm)
      if (length(k) < 3) next
      md <- stats::median(d[k]); fuera <- k[abs(d[k] - md) > 0.20]
      if (length(fuera))
        add(fuera, sprintf("deseabilidad lejos de la mediana de su dimension (%.2f vs %.2f)",
                           d[fuera][1], md), "deseabilidad")
    }
  }

  cv <- NULL
  if (isTRUE(con_aiken)) {
    cv <- reloj("V de Aiken (jueces LLM)", tryCatch(
      validez_contenido(x, api_key = api_key, n_jueces = n_jueces,
                        criterios = c("relevancia", "representatividad"),
                        modelo = modelo, verbose = FALSE),
      error = function(e) NULL))
    if (!is.null(cv$v_aiken)) {
      va <- cv$v_aiken
      malos <- which(va$V_promedio < umbral_aiken |
                     (!is.na(va$IC_inf) & va$IC_inf < umbral_ic))
      if (length(malos))
        add(va$numero[malos],
            sprintf("validez de contenido baja (V = %.2f)", va$V_promedio[malos][1]),
            "aiken")
    }
  }

  disc <- reloj("discriminacion semantica", tryCatch(
    discriminacion_semantica(x, verbose = FALSE), error = function(e) NULL))
  if (!is.null(disc) && "unicidad" %in% names(disc)) {
    q <- stats::quantile(disc$unicidad, 0.15, na.rm = TRUE)
    bajos <- which(disc$unicidad <= q)
    if (length(bajos))
      add(bajos, "unicidad baja: no se distingue del resto de la escala",
          "discriminacion")
  }

  if (nrow(marcados) > 0) {
    ag <- stats::aggregate(motivo ~ idx, data = marcados,
                           FUN = function(v) paste(unique(v), collapse = " + "))
    marcados <- ag[order(ag$idx), , drop = FALSE]
  }
  list(compuerta = g, aiken = cv, discriminacion = disc, marcados = marcados,
       score = .score_convergencia(g, cv))
}


# Score global: el veredicto manda, y dentro del mismo veredicto pesan la
# redundancia, las alertas de deseabilidad y la validez de contenido. Sin el
# termino de Aiken el score pierde resolucion y el bucle deja de distinguir
# mejoras (medido: 5 iteraciones sin converger frente a 3 con Aiken).

#' @keywords internal
.score_convergencia <- function(g, cv = NULL) {
  sc <- .score_compuerta(g)
  if (!is.null(cv$v_aiken)) {
    v  <- cv$v_aiken$V_promedio
    sc <- sc + 20 * mean(v, na.rm = TRUE) - 5 * sum(v < 0.70, na.rm = TRUE)
  }
  sc
}


# Control de redaccion del item generado. Nace de un caso real: "Alver fuentes
# en linea" (por "Al ver") llego hasta la version final sin que ningun control
# lo mirara. La deteccion de palabras pegadas necesita diccionario, y hay que
# afinarla: sin las salvaguardas de abajo rechazaba "puntaje" ("punta"+"je") y
# "autoevaluacion" ("auto"+"evaluacion"), items perfectamente correctos.

#' @keywords internal
.texto_mal_formado <- function(txt) {
  if (!nzchar(trimws(txt)))       return("vacio")
  if (grepl("[a-z][A-Z]", txt))   return("mayuscula dentro de palabra")
  if (grepl("\\s{2,}", txt))      return("espacios dobles")
  if (grepl("[.,;:]{2,}", txt))   return("puntuacion repetida")
  if (grepl("^[[:lower:]]", txt)) return("no empieza en mayuscula")
  if (length(strsplit(trimws(txt), "\\s+")[[1]]) < 4) return("demasiado corto")

  if (!requireNamespace("hunspell", quietly = TRUE)) return(NA_character_)
  dic <- tryCatch(hunspell::dictionary("es_ES"), error = function(e) NULL)
  if (is.null(dic)) return(NA_character_)

  particulas <- c("al","el","la","lo","un","de","en","mi","tu","su","me","te",
                  "se","no","ni","si","ya","es","he","y","o")
  prefijos   <- c("auto","co","re","pre","post","anti","sub","super","inter",
                  "intra","multi","micro","macro","semi","pseudo","contra",
                  "sobre","extra","ultra","meta","mono","bi","tri")

  malas <- tryCatch(hunspell::hunspell(txt, dict = dic)[[1]],
                    error = function(e) character(0))
  for (w in malas) {
    if (nchar(w) < 5) next
    for (k in 2:(nchar(w) - 2)) {
      a <- substr(w, 1, k); b <- substr(w, k + 1, nchar(w))
      if (nchar(b) < 3) next
      if (nchar(a) < 3 && !(tolower(a) %in% particulas)) next
      if (tolower(a) %in% prefijos) next
      ok <- tryCatch(all(hunspell::hunspell_check(c(a, b), dict = dic)),
                     error = function(e) FALSE)
      if (isTRUE(ok))
        return(paste0("palabras pegadas ('", w, "' = '", a, " ", b, "')"))
    }
  }
  NA_character_
}


#' @export
print.semilla_convergencia <- function(x, ...) {
  cat("\n===========================================================\n")
  cat("  CONVERGENCIA PSICOMETRICA (SeMiLLa)\n")
  cat("===========================================================\n")
  print(x$historial, row.names = FALSE)
  cat("\n  Veredicto final : ", x$diagnostico$compuerta$veredicto, "\n", sep = "")
  cat("  Objetivo '", x$objetivo, "': ",
      if (isTRUE(x$objetivo_alcanzado)) "ALCANZADO" else
        "no alcanzado reescribiendo items", "\n", sep = "")
  cat("  Items reescritos: ", nrow(x$cambios %||% data.frame()), "\n", sep = "")
  if (!is.null(x$forced_choice))
    cat("  Alternativa de formato (eleccion forzada): disponible en $forced_choice\n")
  cat(sprintf("  Tiempo: %.1f min\n", x$minutos))
  cat("-----------------------------------------------------------\n")
  invisible(x)
}
