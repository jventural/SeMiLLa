# =============================================================================
#  ESTRUCTURA POR CONSENSO ENSEMBLE (mide, decide y corrige)  - SeMiLLa 2.9.25
# -----------------------------------------------------------------------------
#  Fusiona en una sola llamada lo que hasta 2.9.24 eran dos pasos separados del
#  asistente: precision_clasificacion(metodo = "ensemble") para MEDIR y
#  refinar_escala(criterio = "ensemble") para CORREGIR. Separados, quien usaba
#  la app tenia que (a) leer los indices, (b) decidir por su cuenta si estaban
#  mal y (c) volver a elegir A MANO el criterio y el umbral en la pantalla
#  siguiente: nada garantizaba que se corrigiera contra la MISMA regla con la
#  que se acababa de medir.
#
#  Tres momentos explicitos:
#    1. MEDIR    -> ensemble sobre la escala tal como entra
#    2. DECIDIR  -> compuerta de estructura (una regla escrita y auditable)
#    3. CORREGIR -> si falla, refinar_escala() con EL MISMO umbral, blindaje de
#                   cierre, y se VUELVE A MEDIR
#  El objeto devuelto guarda los DOS momentos (antes / despues).
#
#  Dos decisiones que vienen de laboratorio (D:/16_Shinys/SeMiLLa_lab_paso67):
#
#  (a) La silhouette NO puede ser criterio de bloqueo. Sobre embeddings de 1536
#      dimensiones vale ~0.07 incluso en una escala con ARI = 1.00 y precision
#      100%: bloquearia siempre. Entra como indice INFORMATIVO.
#
#  (b) El ciclo es refinar -> blindar -> MEDIR OTRA VEZ, y se repite hasta
#      max_ciclos. No es adorno: blindar_escala() recalcula embeddings, y
#      medido en ese laboratorio UN solo item reescrito por el blindaje llevo a
#      la escala de 20/20 y ARI 1.000 a 18/20 y ARI 0.756. Como nadie volvia a
#      medir despues del blindaje, el refinamiento cerraba anunciando
#      "precision final 100%" sobre una escala que ya no era la que se
#      entregaba.
# =============================================================================

# -----------------------------------------------------------------------------
#  Medicion: un ensemble + los indices que mira la compuerta
# -----------------------------------------------------------------------------
#  El argumento 'seed' se acepta y se fija por coherencia con el resto del
#  paquete, pero el ensemble NO depende de el: .clusterizar() fija sus propias
#  semillas (2024, 2024 + replica) dentro de precision_clasificacion(), tanto
#  para el clustering como para el submuestreo del 90%. Comprobado con seed = 1
#  y seed = 999999: resultados identicos hasta el ultimo decimal.
.medir_estructura <- function(escala, algoritmos, n_replicas, seed, verbose = FALSE) {
  set.seed(seed)
  precision_clasificacion(escala, metodo = "ensemble", algoritmos = algoritmos,
                          n_replicas = n_replicas, verbose = verbose)
}

# -----------------------------------------------------------------------------
#  La compuerta de estructura: una regla por indice, en una tabla
# -----------------------------------------------------------------------------
.compuerta_estructura <- function(ens, umbral_consenso, min_precision, min_ari) {
  cons <- ens$consenso$Consenso
  n_bajo <- sum(cons < umbral_consenso, na.rm = TRUE)

  data.frame(
    indice = c("Items con consenso suficiente",
               "Consenso medio del ensemble",
               "Precision de clasificacion",
               "ARI (teorica vs empirica)",
               "Silhouette"),
    clave  = c("items_ok", "consenso_medio", "precision", "ari", "silhouette"),
    valor  = c(sum(cons >= umbral_consenso, na.rm = TRUE) / length(cons),
               mean(cons, na.rm = TRUE),
               ens$precision_global / 100,
               ens$ari,
               ens$silhouette),
    crudo  = c(sprintf("%d/%d", length(cons) - n_bajo, length(cons)),
               sprintf("%.3f", mean(cons, na.rm = TRUE)),
               sprintf("%.1f%%", ens$precision_global),
               sprintf("%.3f", ens$ari),
               sprintf("%.3f", ens$silhouette)),
    umbral = c(1, NA, min_precision / 100, min_ari, NA),
    regla  = c(sprintf("todos >= %.3f", umbral_consenso),
               "informativo",
               sprintf(">= %.0f%%", min_precision),
               sprintf(">= %.2f", min_ari),
               "informativo (no bloquea)"),
    bloquea = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ) -> tb
  tb$cumple <- ifelse(!tb$bloquea, NA, tb$valor >= tb$umbral)
  tb
}

# -----------------------------------------------------------------------------
#  PASO 6 (fusionado)
# -----------------------------------------------------------------------------
#' @param escala          objeto semilla con embeddings
#' @param api_key         clave OpenAI (solo se usa si hay que refinar)
#' @param algoritmos      algoritmos del ensemble (default: 3 familias)
#' @param n_replicas      replicas por algoritmo
#' @param umbral_consenso consenso minimo por item (misma regla para medir y
#'                        para corregir: es el punto de la fusion)
#' @param min_precision   precision global minima (%)
#' @param min_ari         ARI minimo
#' @param auto_refinar    TRUE = si la compuerta falla, refina solo
#' @param max_iteraciones tope de iteraciones del refinamiento
#' @param blindaje_cierre aplicar blindar_escala() despues de refinar (lo que
#'                        ya hace la app: el refinamiento reescribe items sin
#'                        pasar por los jueces de la generacion)
#' @param escala_refinada si ya tienes la escala refinada (p. ej. el script
#'                        reproducible: la app la refino y volver a llamar al
#'                        LLM daria OTROS items y rompería la paridad), pasala
#'                        aqui: se mide como SEGUNDO momento sin tocar el LLM.
#' @param items_cambiados numeros de los items que reescribio el refinamiento,
#'                        para pintarlos en el grafico cuando se usa
#'                        \code{escala_refinada}.
estructura_por_consenso <- function(escala,
                                    api_key         = Sys.getenv("OPENAI_API_KEY"),
                                    algoritmos      = c("kmeans", "ward", "gmm"),
                                    n_replicas      = 10,
                                    umbral_consenso = 0.667,
                                    min_precision   = 90,
                                    min_ari         = 0.65,
                                    auto_refinar    = TRUE,
                                    max_iteraciones = 8,
                                    max_ciclos      = 2,
                                    modelo          = "gpt-4.1-mini",
                                    blindaje_cierre = TRUE,
                                    escala_refinada = NULL,
                                    items_cambiados = integer(0),
                                    seed            = 2026,
                                    verbose         = TRUE) {

  # Marcas de avance para quien ejecute esto en un proceso aparte y lea el
  # stdout (la app lo hace con callr::r_bg, igual que con la compuerta). El
  # formato "[ESTRUCTURA n/3]" es el contrato: si cambia aqui, el poller de la
  # app deja de saber por donde va y la barra se queda quieta sin estar colgada.
  .hito <- function(n, txt) if (verbose)
    cat(sprintf("\n[ESTRUCTURA %d/3] %s\n", n, txt))

  t0 <- Sys.time()
  .hito(1, "MEDIR - ensemble sobre la escala que entra")
  ens0 <- .medir_estructura(escala, algoritmos, n_replicas, seed)
  gate0 <- .compuerta_estructura(ens0, umbral_consenso, min_precision, min_ari)

  falla <- any(gate0$cumple %in% FALSE)
  if (verbose) {
    for (i in seq_len(nrow(gate0))) {
      marca <- if (is.na(gate0$cumple[i])) "  ·" else if (gate0$cumple[i]) "  v" else "  x"
      cat(sprintf("%s %-32s %-8s  (%s)\n", marca, gate0$indice[i],
                  gate0$crudo[i], gate0$regla[i]))
    }
  }
  .hito(2, if (!falla) "DECIDIR - se cumple: no hace falta corregir"
           else if (!is.null(escala_refinada))
             "DECIDIR - no se cumple (la correccion ya se hizo fuera)"
           else if (isTRUE(auto_refinar))
             "DECIDIR - no se cumple: se corrige solo"
           else "DECIDIR - no se cumple (correccion desactivada)")

  # Estado inicial que hay que conservar para el grafico de dos momentos
  items0 <- escala$items
  cons0  <- data.frame(numero    = items0$numero,
                       dimension = items0$dimension,
                       item      = items0$item,
                       consenso  = ens0$consenso$Consenso,
                       stringsAsFactors = FALSE)

  # ---- Segundo momento YA DADO (script reproducible: no se llama al LLM) ----
  if (!is.null(escala_refinada)) {
    .hito(3, "MEDIR OTRA VEZ - sobre la escala refinada que se entrego")
    ens1  <- .medir_estructura(escala_refinada, algoritmos, n_replicas, seed)
    gate1 <- .compuerta_estructura(ens1, umbral_consenso, min_precision, min_ari)
    escala_refinada$efa <- ens1
    cons1 <- data.frame(numero    = escala_refinada$items$numero,
                        dimension = escala_refinada$items$dimension,
                        item      = escala_refinada$items$item,
                        consenso  = ens1$consenso$Consenso,
                        stringsAsFactors = FALSE)
    return(structure(list(
      escala_inicial = escala, escala_final = escala_refinada,
      antes = ens0, despues = ens1,
      gate_antes = gate0, gate_despues = gate1,
      consenso_antes = cons0, consenso_despues = cons1,
      refinamiento = NULL, historial = NULL, cambiados = items_cambiados,
      evolucion = NULL, refinado = TRUE,
      veredicto = if (any(gate1$cumple %in% FALSE)) "refinado_incompleto" else "refinado_ok",
      parametros = list(algoritmos = algoritmos, n_replicas = n_replicas,
                        umbral_consenso = umbral_consenso,
                        min_precision = min_precision, min_ari = min_ari,
                        max_iteraciones = max_iteraciones, seed = seed),
      minutos = as.numeric(difftime(Sys.time(), t0, units = "mins"))
    ), class = "semilla_estructura"))
  }

  if (!falla || !isTRUE(auto_refinar)) {
    escala$efa <- ens0
    return(structure(list(
      escala_inicial = escala, escala_final = escala,
      antes = ens0, despues = NULL,
      gate_antes = gate0, gate_despues = NULL,
      consenso_antes = cons0, consenso_despues = NULL,
      refinamiento = NULL, historial = NULL, cambiados = integer(0),
      evolucion = NULL,
      refinado = FALSE,
      veredicto = if (!falla) "cumple" else "falla_sin_refinar",
      parametros = list(algoritmos = algoritmos, n_replicas = n_replicas,
                        umbral_consenso = umbral_consenso,
                        min_precision = min_precision, min_ari = min_ari,
                        max_iteraciones = max_iteraciones, seed = seed),
      minutos = as.numeric(difftime(Sys.time(), t0, units = "mins"))
    ), class = "semilla_estructura"))
  }

  # ---- CORREGIR ------------------------------------------------------------
  #  El ciclo completo es refinar -> blindar -> MEDIR OTRA VEZ, y se repite
  #  hasta max_ciclos. El bucle externo no es adorno: medido en este laboratorio
  #  (INFORME.md, corrida 1), el blindaje de cierre reescribio UN item y la
  #  estructura paso de 20/20 y ARI 1.000 a 18/20 y ARI 0.756. El refinamiento
  #  cerraba anunciando "precision final 100%" sobre una escala que ya no era la
  #  que se entregaba: nadie volvia a medir despues del blindaje.
  escala_f  <- escala
  escala_f$efa <- ens0
  cambiados <- integer(0)
  hist_all  <- NULL
  n_blind   <- 0L
  ref       <- NULL
  ciclos    <- data.frame()

  # ---------------------------------------------------------------------------
  #  EVOLUCION COMPLETA. refinar_escala() ya devuelve su propia $evolucion, pero
  #  describe UN ciclo y termina ANTES del blindaje: es la misma cifra que
  #  motivo esta fusion. Un grafico hecho con ella ensena una subida limpia y
  #  esconde justo el punto interesante -la caida cuando el blindaje reescribe
  #  un item-. Aqui se encadenan todos los ciclos y, sobre todo, se anade el
  #  punto "tras blindar", que es la unica medicion de la escala que de verdad
  #  se entrega.
  evol <- data.frame(paso = 0L, etiqueta = "inicial", ciclo = 0L,
                     precision = ens0$precision_global,
                     items_bajo_umbral = sum(ens0$consenso$Consenso < umbral_consenso,
                                             na.rm = TRUE),
                     tipo = "medicion", stringsAsFactors = FALSE)

  for (ciclo in seq_len(max_ciclos)) {
    .hito(2, sprintf("CORREGIR - ciclo %d/%d: reescribiendo con el MISMO umbral",
                     ciclo, max_ciclos))
    ref_c <- refinar_escala(
      escala_f, api_key = api_key,
      criterio        = "ensemble",
      umbral_consenso = umbral_consenso,
      max_iteraciones = max_iteraciones,
      modelo          = modelo,
      exportar_excel  = FALSE,
      verbose         = verbose)
    if (is.null(ref)) ref <- ref_c
    escala_f  <- ref_c$escala_final
    cambiados <- unique(c(cambiados, ref_c$historial$item_original_num %||% integer(0)))
    hist_all  <- rbind(hist_all, ref_c$historial)

    # Blindaje de cierre (espejo de lo que hace la app tras el paso 7)
    if (isTRUE(blindaje_cierre)) {
      antes_txt <- escala_f$items$item
      i_cam <- integer(0)
      eb <- tryCatch(blindar_escala(escala_f, api_key = api_key,
                                             modelo = modelo, verbose = FALSE),
                     error = function(e) e)
      if (!inherits(eb, "error")) {
        i_cam <- which(antes_txt != eb$items$item)
        n_blind <- n_blind + length(i_cam)
        if (length(i_cam) > 0) {
          hist_all <- rbind(
            hist_all[, intersect(names(hist_all),
                                 c("iteracion","item_original_num","dimension",
                                   "item_original_texto","item_nuevo_texto","razon")),
                     drop = FALSE],
            data.frame(iteracion = paste0("blindaje c", ciclo),
                       item_original_num   = eb$items$numero[i_cam],
                       dimension           = eb$items$dimension[i_cam],
                       item_original_texto = antes_txt[i_cam],
                       item_nuevo_texto    = eb$items$item[i_cam],
                       razon = "blindaje de cierre (contexto / gemelos / longitud)",
                       stringsAsFactors = FALSE))
          cambiados <- unique(c(cambiados, eb$items$numero[i_cam]))
        }
        escala_f <- eb
      }
      if (verbose) cat("  Blindaje de cierre: ", length(i_cam),
                       " item(s) corregido(s)\n", sep = "")
    }

    # ---- VOLVER A MEDIR (esto es lo que hoy no hace nadie) -----------------
    .hito(3, sprintf("MEDIR OTRA VEZ - ciclo %d: sobre la escala que se entrega",
                     ciclo))
    ens1  <- .medir_estructura(escala_f, algoritmos, n_replicas, seed)
    gate1 <- .compuerta_estructura(ens1, umbral_consenso, min_precision, min_ari)
    escala_f$efa <- ens1
    ciclos <- rbind(ciclos, data.frame(
      ciclo = ciclo,
      items_bajo_umbral = sum(ens1$consenso$Consenso < umbral_consenso, na.rm = TRUE),
      consenso_medio = mean(ens1$consenso$Consenso, na.rm = TRUE),
      precision = ens1$precision_global, ari = ens1$ari,
      cumple = !any(gate1$cumple %in% FALSE), stringsAsFactors = FALSE))

    # Las vueltas de ESTE ciclo, tal como las midio refinar_escala (su punto 0
    # es el estado con el que entro, que ya esta en la curva: se descarta).
    ev_c <- ref_c$evolucion
    if (!is.null(ev_c) && nrow(ev_c) > 1) {
      ev_c <- ev_c[-1, , drop = FALSE]
      evol <- rbind(evol, data.frame(
        paso = nrow(evol) - 1L + seq_len(nrow(ev_c)),
        etiqueta = sprintf("c%d.v%d", ciclo, seq_len(nrow(ev_c))),
        ciclo = ciclo, precision = ev_c$Precision,
        items_bajo_umbral = NA_integer_, tipo = "iteracion",
        stringsAsFactors = FALSE))
    }
    # Y el punto que no existia: la escala DESPUES del blindaje. Aqui es donde
    # se ve la caida si el blindaje deshizo lo ganado.
    evol <- rbind(evol, data.frame(
      paso = nrow(evol), etiqueta = sprintf("c%d tras blindar", ciclo),
      ciclo = ciclo, precision = ens1$precision_global,
      items_bajo_umbral = sum(ens1$consenso$Consenso < umbral_consenso, na.rm = TRUE),
      tipo = "tras_blindaje", stringsAsFactors = FALSE))

    if (!any(gate1$cumple %in% FALSE)) break
    if (verbose && ciclo < max_ciclos)
      cat("  La compuerta sigue sin cumplirse tras el blindaje: otro ciclo.\n")
  }

  cons1 <- data.frame(numero    = escala_f$items$numero,
                      dimension = escala_f$items$dimension,
                      item      = escala_f$items$item,
                      consenso  = ens1$consenso$Consenso,
                      stringsAsFactors = FALSE)

  structure(list(
    escala_inicial = escala, escala_final = escala_f,
    antes = ens0, despues = ens1,
    gate_antes = gate0, gate_despues = gate1,
    consenso_antes = cons0, consenso_despues = cons1,
    refinamiento = ref, historial = hist_all, cambiados = cambiados,
    n_blindaje = n_blind, ciclos = ciclos,
    # La completa (todos los ciclos + el punto tras blindar). La de un solo
    # ciclo sigue disponible en $refinamiento$evolucion por si se compara.
    evolucion = evol,
    refinado = TRUE,
    veredicto = if (any(gate1$cumple %in% FALSE)) "refinado_incompleto" else "refinado_ok",
    parametros = list(algoritmos = algoritmos, n_replicas = n_replicas,
                      umbral_consenso = umbral_consenso,
                      min_precision = min_precision, min_ari = min_ari,
                      max_iteraciones = max_iteraciones,
                      max_ciclos = max_ciclos, seed = seed),
    minutos = as.numeric(difftime(Sys.time(), t0, units = "mins"))
  ), class = "semilla_estructura")
}

# -----------------------------------------------------------------------------
#  print()
# -----------------------------------------------------------------------------
print.semilla_estructura <- function(x, ...) {
  cat("\n== Estructura por consenso ensemble ==\n")
  cat("  Veredicto: ", switch(x$veredicto,
      cumple              = "la escala YA cumplia (no se refino)",
      falla_sin_refinar   = "no cumple (refinamiento desactivado)",
      refinado_ok         = "no cumplia -> se refino -> ahora cumple",
      refinado_incompleto = "no cumplia -> se refino -> sigue sin cumplir"), "\n\n")
  g <- x$gate_antes
  if (!is.null(x$gate_despues)) {
    tb <- data.frame(Indice = g$indice, Antes = g$crudo,
                     Despues = x$gate_despues$crudo, Regla = g$regla,
                     Antes_ok = g$cumple, Despues_ok = x$gate_despues$cumple)
  } else {
    tb <- data.frame(Indice = g$indice, Valor = g$crudo, Regla = g$regla, OK = g$cumple)
  }
  print(tb, row.names = FALSE)
  if (isTRUE(x$refinado)) {
    cat("\n  Items reescritos: ", length(x$cambiados), " (",
        paste(sort(x$cambiados), collapse = ", "), ")\n", sep = "")
    if (!is.null(x$ciclos) && nrow(x$ciclos) > 0) {
      cat("  Ciclos refinar -> blindar -> medir:\n")
      print(x$ciclos, row.names = FALSE)
    }
  }
  cat("  Tiempo: ", sprintf("%.1f", x$minutos), " min\n\n", sep = "")
  invisible(x)
}
