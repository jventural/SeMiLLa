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
.compuerta_estructura <- function(ens, umbral_consenso, min_precision, min_ari,
                                  escala = NULL, min_prop_items = 0.90) {
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
    #  v2.9.35: era 1 (TODOS los items). En 8 corridas del laboratorio no se
    #  cumplio ni una vez, asi que el paso nunca cerraba por exito y gastaba
    #  ciclos persiguiendo dos o tres items. Pasa al 90 %.
    umbral = c(min_prop_items, NA, min_precision / 100, min_ari, NA),
    regla  = c(sprintf("%.0f%% de los items >= %.3f", 100 * min_prop_items,
                       umbral_consenso),
               "informativo",
               sprintf(">= %.0f%%", min_precision),
               sprintf(">= %.2f", min_ari),
               "informativo (no bloquea)"),
    bloquea = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ) -> tb
  tb$cumple <- ifelse(!tb$bloquea, NA, tb$valor >= tb$umbral)

  # v2.9.29: CONTENIDO -----------------------------------------------------
  #  Las cinco filas de arriba miden estructura y solo estructura. El consenso
  #  del ensemble premia que los items de una dimension se parezcan entre si, y
  #  la via mas corta para lograrlo es que todos midan la misma faceta. Una
  #  corrida real subio la precision de 66.7% a 95.8% dejando DOS facetas
  #  declaradas del constructo en cero items, y la compuerta lo dio por bueno
  #  porque no miraba el contenido. Esta fila bloquea: una escala a la que le
  #  falta una faceta de su propia definicion no puede darse por buena por muy
  #  bien que clasifique.
  #  Si $disponible es FALSE la fila NO se anade: puede ser que la escala no
  #  declare facetas, o que las etiquetas de los items ya no casen con las
  #  declaradas (el refinamiento reescribe items$caracteristica sin tocar
  #  concepto$caracteristicas). En ese caso no se sabe que esta cubierto, y
  #  bloquear seria un falso positivo: medido sobre una escala real, el cruce
  #  por igualdad exacta daba 0 de 9 facetas cubiertas en una escala correcta.
  cob <- tryCatch(auditar_cobertura_facetas(escala, verbose = FALSE),
                  error = function(e) NULL)
  if (!is.null(cob) && isTRUE(cob$disponible) && cob$n_declaradas > 0) {
    n_huer <- nrow(cob$huerfanas)
    tb <- rbind(tb, data.frame(
      indice  = "Facetas del constructo cubiertas",
      clave   = "cobertura_facetas",
      valor   = cob$prop_cubierta,
      crudo   = sprintf("%d/%d", cob$n_cubiertas, cob$n_declaradas),
      umbral  = 1,
      regla   = "ninguna faceta declarada puede quedarse sin items",
      bloquea = TRUE,
      cumple  = n_huer == 0,
      stringsAsFactors = FALSE))
    # v2.9.32: el reparto dentro de la dimension, INFORMATIVO (no bloquea).
    #  cubierta = n >= 1 deja pasar un 6/1/1 en una dimension de 8 items. No
    #  bloquea porque hay constructos donde una faceta legitimamente pesa mas,
    #  pero se muestra: es la senal temprana del 6/2/0 que si bloquea.
    if (!is.null(cob$equilibrio) && nrow(cob$equilibrio) > 0) {
      conc <- cob$equilibrio$concentrada
      tb <- rbind(tb, data.frame(
        indice  = "Reparto de items entre facetas",
        clave   = "equilibrio_facetas",
        valor   = 1 - max(cob$equilibrio$max_prop, na.rm = TRUE),
        crudo   = paste(cob$equilibrio$reparto, collapse = " | "),
        umbral  = NA_real_,
        regla   = "informativo (avisa si una faceta pasa del 50%)",
        bloquea = FALSE,
        cumple  = NA,
        stringsAsFactors = FALSE))
      attr(tb, "equilibrio") <- cob$equilibrio
    }
    attr(tb, "cobertura") <- cob
  }
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
                                    #  v2.9.35: la regla de items ya no exige
                                    #  todos (ver .compuerta_estructura).
                                    min_prop_items  = 0.90,
                                    min_precision   = 90,
                                    min_ari         = 0.65,
                                    auto_refinar    = TRUE,
                                    max_iteraciones = 8,
                                    #  v2.9.35: sube a 3. Con 2, la parada por
                                    #  convergencia no llegaba a ahorrar nada
                                    #  (el ciclo 2 ya habia corrido cuando se
                                    #  detecta que no mejoro).
                                    max_ciclos      = 3,
                                    modelo          = "gpt-4.1-mini",
                                    #  v2.9.35: APAGADO por defecto. Medido en
                                    #  D:/16_Shinys/SeMiLLa_lab_compuerta_consenso:
                                    #  hubo que revertirlo en 6 de 6 ciclos, o sea
                                    #  que se pagaba una llamada al LLM para
                                    #  deshacerla siempre. blindar_escala() sigue
                                    #  corriendo DENTRO de generar_escala(), que es
                                    #  donde limpia sin pisar nada, y sigue
                                    #  disponible suelta.
                                    blindaje_cierre = FALSE,
                                    escala_refinada = NULL,
                                    items_cambiados = integer(0),
                                    # v2.9.29: parametros del refinamiento que antes
                                    # no se podian tocar desde aqui. Ver refinar_escala().
                                    umbral_redundancia       = "compuerta",
                                    max_intentos_redundancia = 3,
                                    max_reescrituras_item    = 2L,
                                    devolver_mejor           = TRUE,
                                    # --- v2.9.35: cierre del ciclo y topes ---
                                    #  Medido en D:/16_Shinys/SeMiLLa_lab_compuerta_consenso
                                    #  (DASS-21 es, 21 items). Ver su INFORME.md.
                                    cerrar_ciclo         = TRUE,
                                    revertir_blindaje    = TRUE,
                                    parar_si_no_mejora   = TRUE,
                                    max_ciclos_secos     = 1L,
                                    max_minutos          = 25,
                                    aviso_prop_reescrita = 0.5,
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
  gate0 <- .compuerta_estructura(ens0, umbral_consenso, min_precision, min_ari,
                                escala = escala, min_prop_items = min_prop_items)

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
    gate1 <- .compuerta_estructura(ens1, umbral_consenso, min_precision, min_ari,
                                   escala = escala_refinada, min_prop_items = min_prop_items)
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
  #  EL EJE 3 DE LA COMPUERTA, sobre una escala cualquiera. Es lo que la
  #  simulacion anticipa que pasara en campo: cuantos factores se separaran de
  #  verdad (mapa de fusion) y con que probabilidad el ajuste sale limpio.
  #  Cuesta una recalificacion de deseabilidad (~0,1 min) y una simulacion
  #  (~0,4 min); ninguna llamada mas al LLM que esa.
  .medir_eje3 <- function(esc, texto_cambio) {
    if (!isTRUE(cerrar_ciclo) || is.null(escala$compuerta)) return(NULL)
    par_prev <- escala$compuerta$parametros
    des <- escala$compuerta$deseabilidad$deseabilidad
    if (texto_cambio) {
      dn <- tryCatch(calificar_deseabilidad(esc, api_key = api_key,
                                            modelo = modelo, verbose = FALSE),
                     error = function(e) NULL)
      des <- if (!is.null(dn)) dn$deseabilidad else NULL
    }
    #  Un tercio de las replicas de la compuerta (minimo 30): aqui se compara
    #  un ciclo con otro, no se informa. Con 50 items y 5 factores, las 100 de
    #  la compuerta se van a horas.
    n_rep_ciclo <- max(30L, round((par_prev$n_rep %||% 100) / 3))
    sim <- tryCatch(simular_estructura(esc, deseabilidad = des,
                                       similitud = esc$similitud,
                                       n = par_prev$n %||% 300,
                                       n_rep = n_rep_ciclo,
                                       seed = seed, verbose = FALSE),
                    error = function(e) NULL)
    if (is.null(sim)) return(NULL)
    k_teo <- length(unique(esc$items$dimension))
    list(prob = sim$prob_limpia %||% NA_real_,
         n_rep = n_rep_ciclo,
         k_esperado = sim$mapa_fusion$k_esperado %||% k_teo,
         k_teorico = k_teo,
         phi_med = sim$phi_med %||% NA_real_,
         con_deseabilidad = !is.null(des),
         sim = sim)
  }

  #  Como se compara un estado con otro. Orden lexicografico compactado, en el
  #  mismo orden en que manda la regla: reglas bloqueantes cumplidas -> items
  #  por encima del umbral -> precision -> ARI. Sin esto no se puede decir cual
  #  de dos ciclos dejo mejor la escala, y el bucle entregaba SIEMPRE el ultimo.
  #  Orden de importancia, de mas a menos:
  #    1. reglas bloqueantes cumplidas
  #    2. FACTORES QUE SE ESPERA RECUPERAR (eje 3): si las dimensiones se
  #       funden, la escala no mide lo que dice aunque sus items agrupen bien.
  #       Este es el criterio que pidio el autor y el que la compuerta fue
  #       disenada para estimar.
  #    3. items por encima del umbral de consenso
  #    4. ARI y precision
  #    5. probabilidad de ajuste limpio, como desempate fino: se le da poco
  #       peso a proposito porque tiene ruido de simulacion y perseguirla
  #       llevaria a elegir por decimas que no significan nada.
  .puntuar <- function(ens, gate, eje3 = NULL) {
    n_ok  <- sum(gate$bloquea & gate$cumple %in% TRUE, na.rm = TRUE)
    sobre <- sum(ens$consenso$Consenso >= umbral_consenso, na.rm = TRUE)
    s <- n_ok * 10000 + sobre * 10 +
      (ens$ari %||% 0) + (ens$precision_global %||% 0) / 100
    if (!is.null(eje3) && is.finite(eje3$k_esperado))
      s <- s + 1000 * eje3$k_esperado + 0.1 * (eje3$prob %||% 0)
    s
  }

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

  #  El estado inicial ES un candidato: medido en el laboratorio, un ciclo
  #  completo puede dejar la escala PEOR que como entro (61,9 % -> 76,2 % ->
  #  61,9 % tras el blindaje, con un item mas por debajo del umbral). Si ningun
  #  ciclo mejora, se entrega la escala de entrada y se dice.
  eje3_0 <- .medir_eje3(escala, FALSE)
  if (verbose && !is.null(eje3_0))
    cat(sprintf("  [eje 3] de entrada: %d de %d factores se separan, prob %.2f\n",
                eje3_0$k_esperado, eje3_0$k_teorico, eje3_0$prob))
  mejor <- list(escala = escala_f, ens = ens0, gate = gate0, ciclo = 0L,
                eje3 = eje3_0, score = .puntuar(ens0, gate0, eje3_0))
  secos  <- 0L
  motivo <- NA_character_

  for (ciclo in seq_len(max_ciclos)) {
    #  TOPE DE TIEMPO. La parada por convergencia puede pedir varios ciclos y
    #  cada uno son minutos y llamadas al LLM: el reloj corta por encima de
    #  cualquier otro criterio.
    if (is.finite(max_minutos) &&
        as.numeric(difftime(Sys.time(), t0, units = "mins")) >= max_minutos) {
      motivo <- sprintf("tope de tiempo (%g min)", max_minutos)
      if (verbose) cat("  ", motivo, ": no se abre otro ciclo.\n", sep = "")
      break
    }
    .hito(2, sprintf("CORREGIR - ciclo %d/%d: reescribiendo con el MISMO umbral",
                     ciclo, max_ciclos))
    # v2.9.29: se le pasan los parametros de redundancia y los del ensemble.
    #  Antes el bucle optimizaba contra un ensemble de 10 replicas fijas
    #  mientras la compuerta lo juzgaba con las que pidio el usuario (20 en la
    #  corrida que motivo esto): perseguia un objetivo que no era el del
    #  examinador. Y no habia forma de tocar el umbral de redundancia ni el
    #  tope de reescrituras desde aqui.
    ref_c <- refinar_escala(
      escala_f, api_key = api_key,
      criterio        = "ensemble",
      umbral_consenso = umbral_consenso,
      max_iteraciones = max_iteraciones,
      umbral_redundancia       = umbral_redundancia,
      max_intentos_redundancia = max_intentos_redundancia,
      max_reescrituras_item    = max_reescrituras_item,
      devolver_mejor           = devolver_mejor,
      modelo          = modelo,
      exportar_excel  = FALSE,
      verbose         = verbose)
    if (is.null(ref)) ref <- ref_c
    escala_f  <- ref_c$escala_final
    cambiados <- unique(c(cambiados, ref_c$historial$item_original_num %||% integer(0)))
    hist_all  <- rbind(hist_all, ref_c$historial)

    #  Medicion PREVIA al blindaje: es la referencia para decidir si el cierre
    #  ayudo o estropeo. Cuesta un ensemble (sin LLM) y evita entregar una
    #  escala que el propio blindaje empeoro.
    escala_pre <- escala_f
    ens_pre <- gate_pre <- NULL
    i_cam <- integer(0)
    if (isTRUE(blindaje_cierre) && isTRUE(revertir_blindaje)) {
      ens_pre  <- .medir_estructura(escala_f, algoritmos, n_replicas, seed)
      gate_pre <- .compuerta_estructura(ens_pre, umbral_consenso, min_precision,
                                        min_ari, escala = escala_f, min_prop_items = min_prop_items)
    }

    # Blindaje de cierre (espejo de lo que hace la app tras el paso 7)
    if (isTRUE(blindaje_cierre)) {
      antes_txt <- escala_f$items$item
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
    gate1 <- .compuerta_estructura(ens1, umbral_consenso, min_precision, min_ari,
                                   escala = escala_f, min_prop_items = min_prop_items)
    #  ¿El blindaje ayudo? Si no, se vuelve a la escala de antes de blindar.
    #  Hasta ahora esto se MEDIA pero no se ACTUABA: el cierre podia deshacer lo
    #  que el refinamiento acababa de conseguir y se entregaba igual.
    revertido <- FALSE
    if (!is.null(ens_pre) && .puntuar(ens1, gate1) < .puntuar(ens_pre, gate_pre)) {
      if (verbose)
        cat("  El blindaje empeoro la estructura: se vuelve a la version",
            "previa al blindaje.\n")
      escala_f <- escala_pre; ens1 <- ens_pre; gate1 <- gate_pre
      revertido <- TRUE
      n_blind <- max(0L, n_blind - length(i_cam))
      if (!is.null(hist_all) && nrow(hist_all) > 0) {
        fuera <- hist_all$iteracion == paste0("blindaje c", ciclo)
        hist_all <- hist_all[!fuera, , drop = FALSE]
      }
    }
    escala_f$efa <- ens1
    ciclos <- rbind(ciclos, data.frame(
      ciclo = ciclo,
      items_bajo_umbral = sum(ens1$consenso$Consenso < umbral_consenso, na.rm = TRUE),
      consenso_medio = mean(ens1$consenso$Consenso, na.rm = TRUE),
      precision = ens1$precision_global, ari = ens1$ari,
      cumple = !any(gate1$cumple %in% FALSE),
      blindaje_revertido = revertido, stringsAsFactors = FALSE))

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

    #  El eje 3 de ESTE ciclo, para que la decision mire las dos cosas.
    eje3_c <- .medir_eje3(escala_f, !identical(escala$items$item,
                                               escala_f$items$item))
    if (verbose && !is.null(eje3_c))
      cat(sprintf("  [eje 3] ciclo %d: %d de %d factores se separan, prob %.2f\n",
                  ciclo, eje3_c$k_esperado, eje3_c$k_teorico, eje3_c$prob))

    #  ¿Este ciclo mejoro? Se guarda el mejor estado MEDIDO, no el ultimo.
    score_c <- .puntuar(ens1, gate1, eje3_c)
    if (score_c > mejor$score) {
      mejor <- list(escala = escala_f, ens = ens1, gate = gate1,
                    ciclo = ciclo, eje3 = eje3_c, score = score_c)
      secos <- 0L
    } else {
      secos <- secos + 1L
      if (verbose) cat("  El ciclo ", ciclo, " no mejoro lo ya conseguido",
                       " (ciclos secos: ", secos, ").\n", sep = "")
    }

    if (!any(gate1$cumple %in% FALSE)) { motivo <- "cumple la regla"; break }

    #  PARADA POR CONVERGENCIA, con sus topes: si el bucle deja de ganar
    #  terreno no tiene sentido gastar otro ciclo. max_ciclos y max_minutos
    #  siguen mandando por encima de esto.
    if (isTRUE(parar_si_no_mejora) && secos >= max_ciclos_secos) {
      motivo <- sprintf("%d ciclo(s) sin mejorar", secos)
      if (verbose) cat("  ", motivo, ": se cierra el paso.\n", sep = "")
      break
    }
    if (verbose && ciclo < max_ciclos)
      cat("  La compuerta sigue sin cumplirse tras el blindaje: otro ciclo.\n")
  }
  if (is.na(motivo)) motivo <- sprintf("tope de ciclos (%d)", max_ciclos)

  #  Se entrega el MEJOR estado medido, que puede ser el de entrada.
  ultimo_ciclo <- if (nrow(ciclos)) max(ciclos$ciclo) else 0L
  if (verbose && mejor$ciclo != ultimo_ciclo)
    cat("\n  Se entrega la mejor version medida (",
        if (mejor$ciclo == 0L) "la de entrada" else paste("ciclo", mejor$ciclo),
        "), no la ultima.\n", sep = "")
  escala_f <- mejor$escala; ens1 <- mejor$ens; gate1 <- mejor$gate
  escala_f$efa <- ens1

  #  CERRAR EL CICLO. La compuerta alimenta a este paso (umbral, vetos,
  #  deseabilidad) pero nadie volvia a mirarla despues: el eje 3 -lo que la
  #  simulacion anticipa que pasara en campo- quedaba obsoleto en cuanto se
  #  reescribia un item. Se recalcula aqui sobre la escala que SE ENTREGA.
  #  No cuesta ninguna llamada al LLM: es simulacion sobre los embeddings ya
  #  recalculados. La deseabilidad solo se reutiliza si el texto no cambio;
  #  si cambio, seria de otros items.
  #  CERRAR EL CICLO. Ya no hace falta recalcular nada: el eje 3 se midio en
  #  cada ciclo y el de la version entregada viaja con ella.
  eje3 <- NULL
  if (!is.null(mejor$eje3)) {
    e_ini <- eje3_0
    eje3 <- list(
      antes = e_ini$sim, despues = mejor$eje3$sim,
      con_deseabilidad = isTRUE(mejor$eje3$con_deseabilidad),
      k_teorico = mejor$eje3$k_teorico,
      cambio = c(prob_antes = e_ini$prob %||% NA_real_,
                 prob_despues = mejor$eje3$prob %||% NA_real_,
                 k_antes = e_ini$k_esperado %||% NA_real_,
                 k_despues = mejor$eje3$k_esperado %||% NA_real_))
    if (verbose) {
      cat("\n[CIERRE] Eje 3 sobre la escala que se entrega:\n")
      cat(sprintf("  factores que se separan: %s -> %s (de %d declarados)\n",
                  eje3$cambio[["k_antes"]], eje3$cambio[["k_despues"]],
                  eje3$k_teorico))
      cat(sprintf("  prob. de estructura limpia: %.2f -> %.2f\n",
                  eje3$cambio[["prob_antes"]], eje3$cambio[["prob_despues"]]))
    }
  }

  #  Aviso: si se reescribio media escala o mas, ya no es el mismo instrumento.
  aviso_reescritura <- NULL
  prop_cambiada <- length(cambiados) / max(1L, nrow(escala$items))
  if (prop_cambiada >= aviso_prop_reescrita) {
    aviso_reescritura <- sprintf(
      paste0("Se reescribio el %.0f%% de los items (%d de %d). La escala que ",
             "sale ya no es la que entro: documentalo como version nueva, no ",
             "como la original corregida."),
      100 * prop_cambiada, length(cambiados), nrow(escala$items))
    if (verbose) cat("\n[AVISO] ", aviso_reescritura, "\n", sep = "")
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
    # v2.9.35
    eje3 = eje3, aviso_reescritura = aviso_reescritura,
    motivo_parada = motivo, mejor_ciclo = mejor$ciclo,
    # La completa (todos los ciclos + el punto tras blindar). La de un solo
    # ciclo sigue disponible en $refinamiento$evolucion por si se compara.
    evolucion = evol,
    refinado = TRUE,
    veredicto = if (!any(gate1$cumple %in% FALSE)) "refinado_ok"
                else if (mejor$ciclo == 0L) "refinado_sin_mejora"
                else "refinado_incompleto",
    parametros = list(algoritmos = algoritmos, n_replicas = n_replicas,
                      umbral_consenso = umbral_consenso,
                      min_precision = min_precision, min_ari = min_ari,
                      max_iteraciones = max_iteraciones,
                      max_ciclos = max_ciclos,
                      max_ciclos_secos = max_ciclos_secos,
                      max_minutos = max_minutos,
                      revertir_blindaje = revertir_blindaje,
                      cerrar_ciclo = cerrar_ciclo, seed = seed),
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
      refinado_incompleto = "no cumplia -> se refino -> sigue sin cumplir",
      refinado_sin_mejora = "no cumplia -> se refino -> NINGUN ciclo mejoro: se entrega la escala de entrada"), "\n\n")
  if (!is.null(x$motivo_parada))
    cat("  El paso termino por: ", x$motivo_parada, "\n", sep = "")
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
  if (!is.null(x$eje3))
    cat("  Eje 3 (recalculado): prob. de estructura limpia ",
        sprintf("%.2f -> %.2f", x$eje3$cambio[["prob_antes"]],
                x$eje3$cambio[["prob_despues"]]), "\n", sep = "")
  if (!is.null(x$aviso_reescritura))
    cat("  AVISO: ", x$aviso_reescritura, "\n", sep = "")
  cat("  Tiempo: ", sprintf("%.1f", x$minutos), " min\n\n", sep = "")
  invisible(x)
}
