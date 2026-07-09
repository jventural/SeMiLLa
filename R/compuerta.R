# =============================================================================
# SeMiLLa - Compuerta pre-aplicacion (redaccion -> deseabilidad -> estructura)
# =============================================================================
#
# Motivada por el caso PM policial (n=280, 2026): una escala generada con
# SeMiLLa llego a campo con bloques de items parafraseados y deseabilidad
# uniforme alta; el resultado fue dependencia local (RMSEA=.122), factores
# fundidos (Phi=.92) y fiabilidad inflada. Cada uno de los tres mecanismos
# era detectable ANTES de aplicar con una funcion que ya existia; la
# compuerta las encadena y emite un veredicto unico.

#' @title Compuerta pre-aplicacion: auditoria integral antes de ir a campo
#'
#' @description
#' Encadena las tres auditorias que una escala debe pasar ANTES de aplicarse
#' y las integra en un veredicto unico con semaforo por paso:
#'
#' \enumerate{
#'   \item \strong{Redaccion} (\code{\link{auditar_redundancia}}): pares de
#'         items parafraseados, clusters de faceta repetida (3+ items sobre
#'         la misma conducta) y homogeneidad sintactica. Estos bloques
#'         generan dependencia local (RMSEA alto) y fiabilidad inflada.
#'   \item \strong{Deseabilidad} (\code{\link{calificar_deseabilidad}}):
#'         deseabilidad social uniforme y alta ENTRE dimensiones colapsa los
#'         factores (halo); deseabilidad mixta DENTRO de una dimension la
#'         fragmenta. Este mecanismo es INVISIBLE para la similitud
#'         semantica (en PM policial, el par con r=.77 tenia similitud .42).
#'   \item \strong{Estructura simulada} (\code{\link{simular_estructura}}):
#'         probabilidad de obtener una estructura factorial limpia con estos
#'         items, integrando similitud y deseabilidad, a costo de datos cero.
#' }
#'
#' Veredictos posibles (4 niveles):
#' \itemize{
#'   \item \code{"LISTA PARA CAMPO"}: todo ok.
#'   \item \code{"APLICAR CON CAUTELA"}: advertencias sin riesgos.
#'   \item \code{"APLICAR COMO ESCALA GLOBAL"}: la redaccion esta limpia y
#'         el ajuste simulado es aceptable (RMSEA <= .08), pero las
#'         dimensiones no se separaran (Phi alto por deseabilidad uniforme
#'         u otra causa estructural). La escala ES aplicable puntuando el
#'         TOTAL (o bifactor usando solo el factor general); lo que no debe
#'         hacerse es interpretar subescalas. Separar las facetas requiere
#'         una decision de diseno (deseabilidad contrastante, anclas,
#'         formato ipsativo), no mas reescritura de items.
#'   \item \code{"NO APLICAR TODAVIA"}: riesgos de redaccion pendientes o
#'         ajuste simulado inaceptable incluso como medida global.
#' }
#' Siempre con la lista de acciones concretas recomendadas.
#'
#' Desde v2.7.0 \code{\link{semilla}} ejecuta esta compuerta automaticamente
#' al final del pipeline (desactivable con \code{compuerta = FALSE}).
#'
#' @param x Objeto \code{semilla} (o lista con \code{$items} y, de
#'   preferencia, \code{$similitud}; si falta la matriz de similitud se
#'   calculan los embeddings).
#' @param api_key Clave del proveedor LLM (para deseabilidad y, si hace
#'   falta, embeddings).
#' @param poblacion Poblacion objetivo (mejora la calificacion de
#'   deseabilidad); si NULL se toma de \code{x$metadata$poblacion}.
#' @param umbral_sem Umbral de similitud para pares redundantes. Default
#'   \code{"auto"}: adaptativo a la linea base de similitud de la escala
#'   (ver \code{\link{auditar_redundancia}}).
#' @param umbral_faceta Similitud media intra-cluster para faceta repetida.
#'   Default \code{"auto"} (adaptativo).
#' @param n Tamano muestral simulado (default 300).
#' @param n_rep Replicas por escenario de la simulacion (default 100).
#' @param modelo Modelo LLM para calificar deseabilidad.
#' @param seed Semilla de la simulacion.
#' @param verbose Mostrar el detalle de cada paso.
#' @param ... Argumentos adicionales para \code{\link{simular_estructura}}
#'   (p. ej. \code{umbral_ld}, \code{carga_propia}, \code{k_cat}).
#'
#' @return Objeto de clase \code{semilla_compuerta} (lista) con:
#' \itemize{
#'   \item \code{semaforo}: data.frame (paso, estado ok/advertencia/riesgo,
#'         detalle).
#'   \item \code{veredicto}: uno de los tres veredictos globales.
#'   \item \code{acciones}: vector de acciones recomendadas (vacio si lista).
#'   \item \code{redaccion}, \code{deseabilidad}, \code{estructura}: los
#'         objetos completos de cada auditoria para inspeccion.
#'   \item \code{mapa_fusion} y \code{estructura_alternativa}: cuando la
#'         simulacion anticipa que algunas dimensiones se fundiran, el mapa
#'         de que dimensiones colapsan entre si y la HIPOTESIS B
#'         pre-registrable (factores esperables con sus items), para ir a
#'         campo con ambos modelos declarados y contrastarlos como rivales.
#' }
#'
#' @examples
#' \dontrun{
#' esc <- semilla("gratitud", api_key = key)   # ya incluye la compuerta
#' esc$compuerta                                # veredicto integrado
#'
#' # O de forma manual sobre una escala existente:
#' g <- compuerta_pre_aplicacion(esc, api_key = key)
#' g$semaforo
#' g$acciones
#' }
#'
#' @seealso \code{\link{auditar_redundancia}},
#'   \code{\link{calificar_deseabilidad}}, \code{\link{simular_estructura}}
#'
#' @export
compuerta_pre_aplicacion <- function(x,
                                     api_key       = Sys.getenv("OPENAI_API_KEY"),
                                     poblacion     = NULL,
                                     umbral_sem    = "auto",
                                     umbral_faceta = "auto",
                                     n             = 300,
                                     n_rep         = 100,
                                     modelo        = "gpt-4.1-mini",
                                     seed          = 2026,
                                     verbose       = TRUE,
                                     ...) {

  if (is.null(x$items) || is.null(x$items$item))
    stop("'x' debe contener $items con la columna 'item'.")

  # Asegurar matriz de similitud (necesaria para redaccion y simulacion)
  if (is.null(x$similitud)) {
    if (verbose) cat("  (calculando embeddings: el objeto no traia $similitud)\n")
    emb <- obtener_embeddings(items = x, api_key = api_key, verbose = FALSE)
    x$similitud  <- emb$similitud
    x$embeddings <- emb$embeddings
  }

  estados  <- character(3)
  detalles <- character(3)
  acciones <- character(0)

  # ---------------------------------------------------------------------------
  # PASO 1/3: REDACCION (pares + facetas + sintaxis)
  # ---------------------------------------------------------------------------
  if (verbose) cat("\n", .color_azul("[COMPUERTA 1/3] REDACCION"), "\n", sep = "")
  redaccion <- auditar_redundancia(x, umbral_sem = umbral_sem,
                                   umbral_faceta = umbral_faceta)
  n_pares   <- nrow(redaccion$pares_redundantes)
  fac       <- redaccion$facetas_repetidas
  sint      <- isTRUE(redaccion$homogeneidad_sintactica$alerta)

  # Facetas FUERTES (plantilla compartida o bloque grande) = riesgo real de
  # dependencia local. Facetas debiles y pares sueltos = advertencia:
  # calibrado con ACO-16, que funciono en campo (CFI=.941) con 6 pares de
  # similitud .71-.77 y un cluster debil de 3 items; los bloques que si
  # danaron (PM "companeros" 7 items, ACO plantilla afectiva .70) cumplen
  # todos el criterio fuerte.
  fac_fuertes <- if (nrow(fac) > 0)
    fac[fac$sim_media >= 0.65 | fac$n_items >= 4L, , drop = FALSE]
  else fac
  fac_debiles <- if (nrow(fac) > 0)
    fac[!(fac$sim_media >= 0.65 | fac$n_items >= 4L), , drop = FALSE]
  else fac

  if (nrow(fac_fuertes) > 0) {
    estados[1]  <- "riesgo"
    detalles[1] <- sprintf("%d cluster(s) FUERTE(s) de faceta repetida, %d debil(es) y %d par(es)",
                           nrow(fac_fuertes), nrow(fac_debiles), n_pares)
    acciones <- c(acciones, sprintf(
      paste0("Conservar 1-2 items por cluster de faceta repetida (%s) y ",
             "reemplazar el resto por manifestaciones distintas ",
             "(refinar_escala())."),
      paste(fac_fuertes$nucleo_lexico, collapse = "; ")))
  } else if (nrow(fac_debiles) > 0 || n_pares > 0 || sint) {
    estados[1]  <- "advertencia"
    detalles[1] <- sprintf("%d faceta(s) debil(es), %d par(es) redundante(s)%s: revisar, no bloquea",
                           nrow(fac_debiles), n_pares,
                           if (sint) " + homogeneidad sintactica" else "")
    acciones <- c(acciones,
      "Revisar pares/facetas debiles y decidir que conservar (no bloquea la aplicacion).")
  } else {
    estados[1]  <- "ok"
    detalles[1] <- "sin pares redundantes ni facetas repetidas"
  }
  if (verbose) print(redaccion)

  # ---------------------------------------------------------------------------
  # PASO 2/3: DESEABILIDAD SOCIAL
  # ---------------------------------------------------------------------------
  if (verbose) cat("\n", .color_azul("[COMPUERTA 2/3] DESEABILIDAD"), "\n", sep = "")
  deseab <- tryCatch(
    calificar_deseabilidad(x, api_key = api_key, modelo = modelo,
                           poblacion = poblacion, verbose = verbose),
    error = function(e) e
  )
  # Robustez: si el juicio del LLM es INESTABLE entre pasadas (r < .70), el
  # diagnostico de halo puede oscilar alrededor del umbral entre corridas
  # (observado en PM2: r = .41 -> "contrastante" y "halo" con los mismos
  # items). Se recalifica con el doble de pasadas antes de emitir juicio.
  if (!inherits(deseab, "error") && is.finite(deseab$estabilidad %||% NA) &&
      deseab$estabilidad < 0.70) {
    if (verbose) cat("  (estabilidad r=", sprintf("%.2f", deseab$estabilidad),
                     " < .70: recalificando con 4 pasadas...)\n", sep = "")
    deseab2 <- tryCatch(
      calificar_deseabilidad(x, api_key = api_key, modelo = modelo,
                             poblacion = poblacion, n_pasadas = 4,
                             verbose = verbose),
      error = function(e) NULL
    )
    if (!is.null(deseab2)) deseab <- deseab2
  }
  if (inherits(deseab, "error")) {
    estados[2]  <- "advertencia"
    detalles[2] <- paste("no evaluada:", conditionMessage(deseab))
    deseab <- NULL
  } else if (isTRUE(deseab$riesgo_halo)) {
    estados[2]  <- "riesgo"
    detalles[2] <- sprintf("halo probable: uniforme y alta (media=%.2f, DE entre dim=%.3f)",
                           mean(deseab$deseabilidad), deseab$sd_entre_dim)
    acciones <- c(acciones,
      paste0("Reescribir items buscando deseabilidad CONTRASTANTE entre ",
             "dimensiones, o cambiar el FORMATO: construir la version de ",
             "eleccion forzada con generar_escala_forcedchoice() / anclas ",
             "de frecuencia. En constructos de valores/virtudes (todos los ",
             "items deseables por definicion) la reescritura no basta: el ",
             "formato comparativo es la salida estandar (Schwartz PVQ)."))
  } else if (isTRUE(deseab$uniforme) || isTRUE(deseab$alerta_intra)) {
    estados[2]  <- "advertencia"
    detalles[2] <- deseab$mensaje
    if (isTRUE(deseab$alerta_intra)) {
      acciones <- c(acciones,
        "Homogeneizar la polaridad de deseabilidad DENTRO de cada dimension.")
    } else {
      acciones <- c(acciones,
        "Vigilar la separabilidad de dimensiones (deseabilidad uniforme).")
    }
  } else {
    estados[2]  <- "ok"
    detalles[2] <- "deseabilidad contrastante entre dimensiones"
  }

  # ---------------------------------------------------------------------------
  # PASO 3/3: ESTRUCTURA SIMULADA
  # ---------------------------------------------------------------------------
  if (verbose) cat("\n", .color_azul("[COMPUERTA 3/3] ESTRUCTURA SIMULADA"), "\n", sep = "")
  estructura <- tryCatch(
    simular_estructura(x,
                       deseabilidad = if (!is.null(deseab)) deseab$deseabilidad else NULL,
                       similitud = x$similitud,
                       n = n, n_rep = n_rep,
                       api_key = api_key, seed = seed, verbose = verbose, ...),
    error = function(e) e
  )
  if (inherits(estructura, "error")) {
    estados[3]  <- "advertencia"
    detalles[3] <- paste("no evaluada:", conditionMessage(estructura))
    estructura <- NULL
  } else {
    prob <- estructura$prob_limpia
    # Distincion clave (caso PM policial): "estructura no limpia" puede
    # significar dos cosas MUY distintas. Si el RMSEA simulado es aceptable
    # y el fallo es solo la separabilidad (Phi alto), la escala SI es
    # utilizable como puntaje GLOBAL; solo las subescalas estan en riesgo.
    ajuste_ok <- !is.na(estructura$rmsea_med) && estructura$rmsea_med <= 0.08
    solo_separabilidad <- ajuste_ok && !is.na(estructura$phi_med)
    if (prob >= 0.80) {
      estados[3]  <- "ok"
      detalles[3] <- sprintf("prob. de estructura limpia = %.0f%%", 100 * prob)
    } else if (prob >= 0.50) {
      estados[3]  <- "advertencia"
      detalles[3] <- sprintf("prob. de estructura limpia = %.0f%% (rango %.0f-%.0f%%)",
                             100 * prob, 100 * estructura$prob_min,
                             100 * estructura$prob_max)
      acciones <- c(acciones,
        paste0("Simulacion en zona media: corregir primero redaccion/",
               "deseabilidad y re-simular antes de aplicar."))
    } else if (solo_separabilidad) {
      estados[3]  <- "riesgo"
      detalles[3] <- sprintf(
        paste0("las dimensiones no se separaran (|Phi| simulado = %.2f) pero ",
               "el ajuste global es aceptable (RMSEA = %.3f): utilizable ",
               "como puntaje total"),
        estructura$phi_med, estructura$rmsea_med)
    } else {
      estados[3]  <- "riesgo"
      detalles[3] <- sprintf("prob. de estructura limpia = %.0f%% (rango %.0f-%.0f%%)",
                             100 * prob, 100 * estructura$prob_min,
                             100 * estructura$prob_max)
      acciones <- c(acciones,
        paste0("NO aplicar con esta version: la simulacion anticipa ",
               "estructura sucia. Corregir items segun pasos 1-2 y re-pasar ",
               "la compuerta."))
    }
  }

  # ---------------------------------------------------------------------------
  # Mapa de fusion -> hipotesis alternativa PRE-REGISTRABLE
  # ---------------------------------------------------------------------------
  # Si la simulacion anticipa que ALGUNAS dimensiones se fundiran (caso VP:
  # 3 factores prosociales fundidos, Apertura separada), el investigador debe
  # ir a campo con la hipotesis B declarada, no descubrirla a posteriori.
  estructura_alternativa <- NULL
  mapa <- if (!is.null(estructura)) estructura$mapa_fusion else NULL
  if (!is.null(mapa) && isTRUE(mapa$hay_fusion)) {
    estructura_alternativa <- do.call(rbind, lapply(
      seq_along(mapa$grupos), function(gi) {
        g <- mapa$grupos[[gi]]
        idx <- x$items$dimension %in% g
        data.frame(
          factor_esperado = if (length(g) > 1)
            paste0("F", gi, " (fusion de ", length(g), " dimensiones)")
          else paste0("F", gi),
          dimensiones = paste(g, collapse = " + "),
          n_items = sum(idx),
          items = paste(if (!is.null(x$items$codigo))
                          x$items$codigo[idx]
                        else which(idx), collapse = ", "),
          stringsAsFactors = FALSE)
      }))
    detalles[3] <- paste0(detalles[3], sprintf(
      "; estructura esperable: %d factor(es), no %d",
      mapa$k_esperado, length(unique(x$items$dimension))))
    acciones <- c(acciones, sprintf(
      paste0("PRE-REGISTRAR la hipotesis alternativa antes de aplicar: ",
             "ademas del modelo teorico, declarar el modelo B con %d ",
             "factor(es) [%s] y contrastarlos como modelos rivales."),
      mapa$k_esperado,
      paste(vapply(mapa$grupos, function(g)
        paste(g, collapse = "+"), character(1)), collapse = " | ")))
    # Una fusion anticipada no bloquea, pero tampoco es "todo limpio": la
    # estructura teorica no se reproducira tal cual.
    if (estados[3] == "ok") estados[3] <- "advertencia"
  }

  # ---------------------------------------------------------------------------
  # Veredicto global (4 niveles)
  # ---------------------------------------------------------------------------
  # Caso especial validado con PM policial (n=280): redaccion ya limpia +
  # riesgo restante SOLO de separabilidad con ajuste global aceptable. La
  # escala se puede aplicar puntuando el TOTAL (alli el bifactor con ECV y
  # omegaH altos avalo el puntaje unico); lo unico que no debe hacerse es
  # interpretar subescalas. "NO APLICAR" seria un falso bloqueo.
  caso_global <- !is.null(estructura) &&
    estados[1] != "riesgo" &&
    estados[3] == "riesgo" &&
    !is.na(estructura$rmsea_med) && estructura$rmsea_med <= 0.08 &&
    !is.na(estructura$phi_med)

  veredicto <- if (caso_global) {
    acciones <- c(acciones,
      paste0("Aplicar puntuando el TOTAL (o modelar bifactor con solo el ",
             "factor general); NO interpretar ni puntuar las subescalas ",
             "por separado."),
      paste0("Para separar las facetas en una futura version: deseabilidad ",
             "contrastante entre dimensiones, formato de eleccion forzada ",
             "(generar_escala_forcedchoice()) o anclas de frecuencia ",
             "(decision de diseno, no de redaccion)."))
    # Retirar la accion de "NO aplicar" si venia del paso de deseabilidad
    "APLICAR COMO ESCALA GLOBAL"
  } else if (any(estados == "riesgo")) {
    "NO APLICAR TODAVIA"
  } else if (any(estados == "advertencia")) {
    "APLICAR CON CAUTELA"
  } else {
    "LISTA PARA CAMPO"
  }

  out <- list(
    semaforo = data.frame(
      paso    = c("redaccion", "deseabilidad", "estructura_simulada"),
      estado  = estados,
      detalle = detalles,
      stringsAsFactors = FALSE
    ),
    veredicto    = veredicto,
    acciones     = unique(acciones),
    redaccion    = redaccion,
    deseabilidad = deseab,
    estructura   = estructura,
    mapa_fusion  = mapa,
    estructura_alternativa = estructura_alternativa,
    parametros   = list(umbral_sem = umbral_sem, umbral_faceta = umbral_faceta,
                        n = n, n_rep = n_rep, fecha = format(Sys.Date()))
  )
  class(out) <- c("semilla_compuerta", "list")

  if (verbose) print(out)
  invisible(out)
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_compuerta <- function(x, ...) {
  marca <- function(estado) switch(estado,
    "ok"          = .color_check(),
    "advertencia" = .color_warning(),
    "riesgo"      = if (.soporta_colores()) "\033[31m✖\033[0m" else "[X]",
    "?")
  cat("\n")
  cat("===========================================================\n")
  cat("  COMPUERTA PRE-APLICACION (SeMiLLa)\n")
  cat("===========================================================\n")
  for (i in seq_len(nrow(x$semaforo))) {
    cat("  ", marca(x$semaforo$estado[i]), " ",
        format(x$semaforo$paso[i], width = 20), " ",
        x$semaforo$detalle[i], "\n", sep = "")
  }
  if (!is.null(x$estructura_alternativa)) {
    cat("-----------------------------------------------------------\n")
    cat("  ESTRUCTURA EMPIRICA ESPERABLE (mapa de fusion, hipotesis B):\n")
    for (i in seq_len(nrow(x$estructura_alternativa))) {
      ea <- x$estructura_alternativa[i, ]
      cat("    ", ea$factor_esperado, ": ", ea$dimensiones,
          " (", ea$n_items, " items)\n", sep = "")
    }
    cat("    Pre-registrar como modelo rival del teorico.\n")
  }
  cat("-----------------------------------------------------------\n")
  col_ver <- if (x$veredicto == "LISTA PARA CAMPO") .color_verde
             else .color_amarillo
  cat("  VEREDICTO: ", col_ver(x$veredicto), "\n", sep = "")
  if (length(x$acciones) > 0) {
    cat("  ACCIONES RECOMENDADAS:\n")
    for (a in x$acciones) {
      cat("   - ", a, "\n", sep = "")
    }
  }
  cat("===========================================================\n\n")
  invisible(x)
}
