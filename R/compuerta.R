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
#' @param n_banda Numero de vectores de deseabilidad con los que se re-simula
#'   para estimar la BANDA de incertidumbre del eje 3 (default 4; 0 desactiva).
#'   No cuesta llamadas al LLM: los vectores se remuestrean de las pasadas que
#'   el juez ya hizo. Existe porque el eje 3 amplifica diferencias minimas del
#'   juez: con vectores correlacionados a r = .90 la probabilidad de estructura
#'   limpia paso de .87 a .20 (Experimento 1), y en 6 corridas de la misma
#'   escala oscilo entre .13 y .97. Reportar un punto sin banda transmite una
#'   precision que el dato no tiene.
#' @param n_pasadas_gemelos Numero de consultas al juez de parafrasis-gemelas
#'   (eje 1); un par se acepta si sale en la mayoria de ellas. Una sola llamada
#'   no es reproducible: en 6 corridas de la misma escala se obtuvieron 5
#'   resultados distintos, y con \code{seed} 3 de 6. Los pares legitimos salen
#'   en 9 de 9 pasadas y los dudosos en 1 a 5, asi que la mayoria separa unos de
#'   otros. Use 1 para volver al comportamiento de una sola consulta.
#' @param modelo Modelo LLM para calificar deseabilidad.
#' @param n_nucleos Nucleos para el eje 3 (la simulacion). Por defecto
#'   (\code{NULL}) todos menos uno, igual que \code{\link{estres_escala}}.
#'   Hasta 2.9.30 la compuerta no exponia este parametro y
#'   \code{\link{simular_estructura}} corria en 1 solo nucleo: medido el
#'   2026-08-20 en la misma maquina, la compuerta tardaba 12.0 min (432 CFAs,
#'   1 nucleo) mientras \code{estres_escala()} despachaba 1500 CFAs en 2.7 min
#'   con 22. Use 1 para volver al comportamiento secuencial.
#' @param seed Semilla. Alimenta \code{set.seed()} de la simulacion y, desde
#'   2.9.15, tambien la opcion \code{SeMiLLa.seed} que \code{.llamar_openai()}
#'   envia a la API. Ojo: el seed de OpenAI es best-effort y no garantiza
#'   reproducibilidad; quien la aporta es la votacion del eje 1 y las
#'   \code{n_pasadas} del eje 2.
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
                                     n_pasadas     = 4,
                                     n_pasadas_gemelos = 3,
                                     n_banda       = 4,
                                     modelo        = "gpt-4.1-mini",
                                     n_nucleos     = NULL,
                                     seed          = 2026,
                                     verbose       = TRUE,
                                     ...) {

  if (is.null(x$items) || is.null(x$items$item))
    stop("'x' debe contener $items con la columna 'item'.")

  # ---------------------------------------------------------------------------
  # La semilla tambien viaja a la API (2.9.15)
  # ---------------------------------------------------------------------------
  # Hasta 2.9.14 'seed' solo alimentaba set.seed() de la simulacion (eje 3).
  # .llamar_openai() toma el seed de la opcion global SeMiLLa.seed, que nadie
  # fijaba aqui: los jueces LLM de los ejes 1 y 2 llamaban SIN seed. Quien
  # pasaba seed = 2026 creia estar fijando toda la corrida y solo fijaba un
  # tercio de ella.
  #
  # Aviso honesto: el seed de OpenAI es best-effort y NO garantiza
  # reproducibilidad (medido: 6 corridas con seed -> 3 resultados distintos).
  # Lo que estabiliza de verdad es la votacion por mayoria del eje 1 y las
  # n_pasadas del eje 2. Esto solo deja de mentir sobre lo que hace el
  # parametro y ayuda en el margen.
  if (!is.null(seed)) {
    .op_prev <- options(SeMiLLa.seed = as.integer(seed))
    on.exit(options(.op_prev), add = TRUE)
  }

  # Asegurar matriz de similitud (necesaria para redaccion y simulacion)
  if (is.null(x$similitud)) {
    if (verbose) cat("  (calculando embeddings: el objeto no traia $similitud)\n")
    emb <- obtener_embeddings(items = x, api_key = api_key, verbose = FALSE)
    x$similitud  <- emb$similitud
    x$embeddings <- emb$embeddings
  }

  # 2.9.36: cuatro ejes. El cuarto (asignacion) se anade al FINAL para no mover
  # los indices 1-3, sobre los que ramifica el resto de la funcion.
  estados  <- character(4)
  detalles <- character(4)
  acciones <- character(0)

  # ---------------------------------------------------------------------------
  # AVISOS DE DISENO (2.9.14) — se leen ANTES de simular, no cuestan nada
  # ---------------------------------------------------------------------------
  # Vienen del Experimento 3: tres escalas construidas con SeMiLLa que se
  # aplicaron a 200 personas y COLAPSARON (5 factores -> 2, 3 -> 1, 10 -> 2),
  # con correlaciones entre factores mayores que 1 en dos de ellas. La compuerta
  # les habia dado probabilidad de estructura limpia de .80, 1.00 y .97.
  #
  # Los dos defectos que las hundieron eran visibles en el DISENO, sin simular
  # ni recoger un dato. Cuestan una linea cada uno y no dependen del LLM.
  avisos_diseno <- character(0)

  # (a) RETIRADO. Se habia anadido un aviso para dimensiones con menos de 3
  #     items, justificado en que "Valores positivos" tenia 10 factores de 2
  #     items. Al verificarlo resulto que esa era una version TEMPRANA que nunca
  #     se aplico: la bateria de campo tenia 4 factores de 8 items. El chequeo
  #     puede ser razonable en teoria -un factor con 2 indicadores no queda
  #     identificado- pero se queda sin la evidencia que lo motivaba, y aqui no
  #     se anaden reglas sin un caso que las respalde.
  n_por_dim <- table(x$items$dimension)

  # (b) Dimensiones definidas por POLARIDAD y no por contenido. "Personalidad
  #     moral" tenia una dimension llamada "Reflexion sociomoral (invertir
  #     puntuaciones)" con 8 de sus 20 items: lo que emergio fue un factor de
  #     METODO. De los 17 items inversos de aquella bateria, ninguno sobrevivio.
  pol <- grepl("invert|inverso|reverse|recodific|puntuacion(es)? inversa",
               names(n_por_dim), ignore.case = TRUE)
  if (any(pol)) {
    avisos_diseno <- c(avisos_diseno, sprintf(
      paste0("Dimension(es) definida(s) por la POLARIDAD y no por el ",
             "contenido (%s). Los items inversos tienden a agruparse por COMO ",
             "se puntuan, no por lo que miden: lo que aparece es un factor de ",
             "metodo. Definir la dimension por su contenido y repartir los ",
             "items inversos entre las demas."),
      paste(names(n_por_dim)[pol], collapse = ", ")))
    acciones <- c(acciones, paste0(
      "Redefinir por contenido la(s) dimension(es) nombrada(s) por su ",
      "polaridad: ", paste(names(n_por_dim)[pol], collapse = ", ")))
  }
  if (verbose && length(avisos_diseno)) {
    cat("\n", .color_amarillo("[AVISOS DE DISENO]"), "\n", sep = "")
    for (a in avisos_diseno) cat("  - ", a, "\n", sep = "")
  }

  # ---------------------------------------------------------------------------
  # PASO 0: ASIGNACION ITEM -> DIMENSION (2.9.36)
  # ---------------------------------------------------------------------------
  # No simula ni llama al LLM: lee la matriz de similitud. Existe porque los
  # otros tres ejes NO ven que un item este en la dimension equivocada -el eje 3
  # arma el modelo generador desde las etiquetas y se las cree- y ese defecto SI
  # degrada el ajuste real (CFI .899 -> .790 con 4 items movidos, n = 3.178).
  # Medido: 90 de 90 detectados, 0 falsos positivos en 39 condiciones limpias.
  # Ver el encabezado de asignacion.R.
  if (verbose) cat("\n", .color_azul("[COMPUERTA 0] ASIGNACION"), "\n", sep = "")
  asig <- tryCatch(auditar_asignacion(x, verbose = FALSE), error = function(e) NULL)
  if (is.null(asig) || is.null(asig$items)) {
    estados[4]  <- "ok"
    detalles[4] <- if (is.null(asig)) "no evaluada" else "una sola dimension"
  } else if (asig$n_mal_asignados > 0) {
    estados[4]  <- "riesgo"
    detalles[4] <- sprintf(
      "%d item(s) se parecen mas a otra dimension que a la suya",
      asig$n_mal_asignados)
    acciones <- c(acciones, sprintf(
      paste0("Revisar la asignacion de %d item(s): se parecen mas a otra ",
             "dimension que a la suya. Reasignarlos, reescribirlos o retirarlos ",
             "(ver $asignacion$mal_asignados)."), asig$n_mal_asignados))
    if (verbose) {
      cat("  ", .color_amarillo(detalles[4]), "\n", sep = "")
      for (i in seq_len(min(5L, nrow(asig$mal_asignados)))) {
        r <- asig$mal_asignados[i, ]
        cat(sprintf("    - [%s -> %s] margen %+.3f | %s\n", r$dimension,
                    r$dim_mas_cercana, r$margen, substr(r$item, 1, 60)))
      }
      if (nrow(asig$mal_asignados) > 5)
        cat("    ... y ", nrow(asig$mal_asignados) - 5, " mas\n", sep = "")
    }
  } else {
    estados[4]  <- "ok"
    detalles[4] <- "cada item se parece mas a su propia dimension"
    if (verbose) cat("  ", detalles[4], "\n", sep = "")
  }

  # ---------------------------------------------------------------------------
  # PASO 1/3: REDACCION (pares + facetas + sintaxis)
  # ---------------------------------------------------------------------------
  if (verbose) cat("\n", .color_azul("[COMPUERTA 1/3] REDACCION"), "\n", sep = "")
  redaccion <- auditar_redundancia(x, umbral_sem = umbral_sem,
                                   umbral_faceta = umbral_faceta)

  # Segunda capa: juez LLM de parafrasis. El coseno SUBDETECTA gemelos (pares
  # con r policorica >= .70 en campo vivieron en coseno 0.43-0.78, bateria
  # policial n=280); esos gemelos producen dependencia local y correlaciones
  # interfactoriales infladas (Phi hasta .92), asi que NO pueden pasar.
  # Se vota por mayoria entre n_pasadas_gemelos llamadas: una sola no es
  # reproducible (6 corridas -> 5 resultados distintos) ni con seed. Ver el
  # bloque de .juzgar_parafrasis_votado() para la medicion.
  gemelos_llm <- tryCatch(
    .juzgar_parafrasis_votado(x$items, .configurar_openai(api_key),
                              modelo = modelo,
                              n_pasadas = n_pasadas_gemelos, verbose = verbose),
    error = function(e) NULL)
  n_gemelos_llm <- if (!is.null(gemelos_llm)) nrow(gemelos_llm) else 0L

  # v2.9.30: la BANDA del juez de gemelos --------------------------------------
  #  El juez de parafrasis no es determinista, y el voto por mayoria reduce la
  #  varianza pero no la elimina. Medido sobre una escala de 36 items, tres
  #  repeticiones del votado dieron 15, 4 y 7 gemelos. Sin la banda, comparar
  #  dos versiones de la misma escala hace creer que hubo una mejora donde solo
  #  hubo suerte. El eje 2 ya reporta su estabilidad; este ahora tambien.
  gem_banda <- attr(gemelos_llm, "banda")
  gem_estab <- attr(gemelos_llm, "estabilidad")
  gem_pasadas <- attr(gemelos_llm, "n_por_pasada")
  txt_banda <- if (!is.null(gem_banda) && length(gem_banda) == 2 &&
                   gem_banda[1] != gem_banda[2])
    sprintf(" [el juez vio entre %d y %d segun la pasada%s]", gem_banda[1], gem_banda[2],
            if (!is.null(gem_estab) && !is.na(gem_estab))
              sprintf("; acuerdo entre pasadas %.2f", gem_estab) else "")
  else ""
  if (n_gemelos_llm > 0) {
    S_g <- x$similitud
    pr <- redaccion$pares_redundantes
    clave <- function(a, b) paste(pmin(a, b), pmax(a, b))
    ya <- if (nrow(pr) > 0) clave(pr$item1, pr$item2) else character(0)
    nuevos <- gemelos_llm[!(clave(gemelos_llm$item1, gemelos_llm$item2) %in% ya),
                          , drop = FALSE]
    if (nrow(nuevos) > 0) {
      add <- data.frame(
        item1 = nuevos$item1, item2 = nuevos$item2,
        similitud = round(mapply(function(a, b) S_g[a, b],
                                 nuevos$item1, nuevos$item2), 4),
        stringsAsFactors = FALSE)
      pr <- rbind(pr, add)
      redaccion$pares_redundantes <- pr[order(-pr$similitud), , drop = FALSE]
    }
    redaccion$gemelos_llm <- gemelos_llm
    # v2.9.30: la banda viaja con el resultado, no solo en el texto
    redaccion$gemelos_banda       <- gem_banda
    redaccion$gemelos_estabilidad <- gem_estab
    redaccion$gemelos_por_pasada  <- gem_pasadas
    if (verbose) {
      cat("  ", .color_warning(), " Juez LLM: ", n_gemelos_llm,
          " par(es) de parafrasis-gemelas confirmado(s)\n", sep = "")
      for (k in seq_len(nrow(gemelos_llm))) {
        cat(sprintf("     %d ~ %d: %s\n", gemelos_llm$item1[k],
                    gemelos_llm$item2[k], gemelos_llm$razon[k]))
      }
    }
  }
  n_pares   <- nrow(redaccion$pares_redundantes)
  fac       <- redaccion$facetas_repetidas
  sint      <- isTRUE(redaccion$homogeneidad_sintactica$alerta)

  # Facetas FUERTES (plantilla compartida o bloque grande) = riesgo real de
  # dependencia local. Facetas debiles y pares sueltos = advertencia:
  # calibrado con ACO-16, que funciono en campo (CFI=.941) con 6 pares de
  # similitud .71-.77 y un cluster debil de 3 items; los bloques que si
  # danaron (PM "companeros" 7 items, ACO plantilla afectiva .70) cumplen
  # todos el criterio fuerte.
  es_fuerte <- if (nrow(fac) > 0) {
    vapply(seq_len(nrow(fac)), function(i) {
      fuerte <- fac$sim_media[i] >= 0.65 || fac$n_items[i] >= 4L
      if (!fuerte) return(FALSE)
      # Exencion por tipo: un cluster INTRA-dimension cognitiva/afectiva con
      # cohesion tematica moderada (sim < .70) es contenido legitimo de la
      # actitud, no parafraseo (piloto ACO: cognitivos cohesivos, omega=.72).
      idx <- suppressWarnings(as.integer(gsub("\\D", "",
        trimws(strsplit(fac$codigos[i], ",")[[1]]))))
      idx <- idx[!is.na(idx) & idx <= nrow(x$items)]
      dims_c <- unique(x$items$dimension[idx])
      if (length(dims_c) == 1 && fac$sim_media[i] < 0.70) {
        def_d <- if (is.list(x$concepto$dimensiones))
          x$concepto$dimensiones[[dims_c]] %||% "" else ""
        if (.tipo_dimension(dims_c, def_d) %in% c("cognitiva", "afectiva"))
          return(FALSE)
      }
      TRUE
    }, logical(1))
  } else logical(0)
  fac_fuertes <- fac[es_fuerte, , drop = FALSE]
  fac_debiles <- fac[!es_fuerte, , drop = FALSE]

  if (nrow(fac_fuertes) > 0 || n_gemelos_llm > 0) {
    estados[1]  <- "riesgo"
    detalles[1] <- sprintf(
      "%d gemelo(s) confirmado(s) por juez LLM%s, %d cluster(s) FUERTE(s), %d debil(es) y %d par(es)",
      n_gemelos_llm, txt_banda, nrow(fac_fuertes), nrow(fac_debiles), n_pares)
    if (n_gemelos_llm > 0) {
      acciones <- c(acciones, paste0(
        "Reescribir un miembro de cada par gemelo confirmado por el juez LLM ",
        "(producen dependencia local y correlaciones interfactoriales ",
        "infladas): optimizar_para_campo() lo hace automaticamente."))
    }
    if (nrow(fac_fuertes) > 0) {
      acciones <- c(acciones, sprintf(
        paste0("Conservar 1-2 items por cluster de faceta repetida (%s) y ",
               "reemplazar el resto por manifestaciones distintas ",
               "(refinar_escala())."),
        paste(fac_fuertes$nucleo_lexico, collapse = "; ")))
    }
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
                           poblacion = poblacion, n_pasadas = n_pasadas,
                           verbose = verbose),
    error = function(e) e
  )
  # Robustez: si el juicio del LLM es INESTABLE entre pasadas (r < .70), el
  # diagnostico de halo puede oscilar alrededor del umbral entre corridas
  # (observado en PM2: r = .41 -> "contrastante" y "halo" con los mismos
  # items). Se recalifica con el doble de pasadas antes de emitir juicio.
  if (!inherits(deseab, "error") && is.finite(deseab$estabilidad %||% NA) &&
      deseab$estabilidad < 0.70) {
    # Recalifica con el DOBLE de pasadas de las que se acaban de usar: con un
    # 8 fijo, llamar a la compuerta con n_pasadas = 8 repetia lo ya hecho.
    n_pas2 <- max(8L, as.integer(n_pasadas) * 2L)
    if (verbose) cat("  (estabilidad r=", sprintf("%.2f", deseab$estabilidad),
                     " < .70: recalificando con ", n_pas2, " pasadas...)\n", sep = "")
    deseab2 <- tryCatch(
      calificar_deseabilidad(x, api_key = api_key, modelo = modelo,
                             poblacion = poblacion, n_pasadas = n_pas2,
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
                       n = n, n_rep = n_rep, n_nucleos = n_nucleos,
                       api_key = api_key, seed = seed, verbose = verbose, ...),
    error = function(e) e
  )
  if (inherits(estructura, "error")) {
    estados[3]  <- "advertencia"
    detalles[3] <- paste("no evaluada:", conditionMessage(estructura))
    estructura <- NULL
  } else {
    prob <- estructura$prob_limpia

    # -------------------------------------------------------------------------
    # BANDA DE INCERTIDUMBRE DEL JUEZ (2.9.12)
    # -------------------------------------------------------------------------
    # El motor de simulacion es determinista (mismo vector + misma semilla =
    # mismo resultado, verificado 3/3), pero AMPLIFICA el ruido del juez: dos
    # vectores con r = .90 dieron prob .87 y .20. Se re-simula con n_banda
    # vectores remuestreados de las pasadas que el juez YA hizo -sin llamadas
    # nuevas- y se decide con la MEDIANA, no con el punto de una sola pasada.
    banda <- NULL
    pas <- if (!inherits(deseab, "error")) deseab$pasadas else NULL
    if (!is.null(pas) && is.matrix(pas) && ncol(pas) >= 2 && n_banda > 0) {
      if (verbose) cat("  (banda: re-simulando con ", n_banda,
                       " vectores del juez...)\n", sep = "")
      n_rep_b <- max(10L, round(n_rep / 3))
      probs_b <- vapply(seq_len(n_banda), function(b) {
        set.seed((seed %||% 2026) + 1000L + b)
        cols <- sample.int(ncol(pas), ncol(pas), replace = TRUE)
        v <- rowMeans(pas[, cols, drop = FALSE], na.rm = TRUE)
        v[!is.finite(v)] <- 0.5
        v <- pmin(1, pmax(0, v))
        s <- tryCatch(simular_estructura(x, deseabilidad = v,
                                         similitud = x$similitud,
                                         n = n, n_rep = n_rep_b,
                                         n_nucleos = n_nucleos,
                                         api_key = api_key, seed = seed,
                                         verbose = FALSE, ...),
                      error = function(e) NULL)
        if (is.null(s)) NA_real_ else s$prob_limpia
      }, numeric(1))
      probs_b <- probs_b[is.finite(probs_b)]
      if (length(probs_b) >= 2) {
        banda <- list(probs = probs_b,
                      mediana = stats::median(probs_b),
                      min = min(probs_b), max = max(probs_b),
                      n_rep_banda = n_rep_b)
        # La mediana de la banda sustituye al punto: es la misma medida, pero
        # sin depender de que una sola llamada al juez saliera alta o baja.
        prob <- banda$mediana
      }
    }

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
    # -------------------------------------------------------------------------
    # INDETERMINACION (2.9.12): la banda cruza el umbral de decision
    # -------------------------------------------------------------------------
    # Un umbral duro sobre un input estocastico es fragil por construccion. Si
    # con los vectores del propio juez unas simulaciones quedan por encima de
    # .80 y otras por debajo, el "ok" no describe la escala: describe cual de
    # las llamadas al LLM toco. En ese caso no se afirma que este limpia.
    if (!is.null(banda) && banda$min < 0.80 && banda$max >= 0.80) {
      if (estados[3] == "ok") estados[3] <- "advertencia"
      detalles[3] <- sprintf(
        paste0("INDETERMINADO: con los vectores del propio juez la prob. de ",
               "estructura limpia va de %.0f%% a %.0f%% (mediana %.0f%%) y ",
               "cruza el umbral de decision; no puede afirmarse que la ",
               "estructura sea limpia"),
        100 * banda$min, 100 * banda$max, 100 * banda$mediana)
      acciones <- c(acciones, paste0(
        "La estructura queda INDETERMINADA por la variabilidad del juez de ",
        "deseabilidad: correr el modo Completo (mas replicas) o subir ",
        "n_pasadas antes de decidir sobre las subescalas."))
    } else if (!is.null(banda)) {
      detalles[3] <- paste0(detalles[3], sprintf(
        " [banda del juez: %.0f-%.0f%%]", 100 * banda$min, 100 * banda$max))
    }

    # Extremo opuesto al halo: dimensiones casi ORTOGONALES. Un |Phi| ~ 0
    # tambien es problema (caso VP: Apertura casi ortogonal al resto): las
    # subescalas no se relacionan, el puntaje total pierde sentido y el
    # constructo como unidad queda en duda.
    if (!is.na(estructura$phi_med) && abs(estructura$phi_med) < 0.15 &&
        estados[3] == "ok") {
      estados[3]  <- "advertencia"
      detalles[3] <- paste0(detalles[3],
        sprintf("; dimensiones casi ortogonales (|Phi| simulado = %.2f)",
                abs(estructura$phi_med)))
      acciones <- c(acciones, paste0(
        "Las dimensiones apenas correlacionan entre si: verificar que ",
        "pertenecen al mismo constructo (o reportar solo subescalas, sin ",
        "puntaje total)."))
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
    # 2.9.14: la FUSION va DELANTE, no como coletilla. En el Experimento 3 la
    # probabilidad de estructura limpia dio .80, 1.00 y .97 a tres escalas que
    # colapsaron, mientras el mapa acertaba la direccion en dos de ellas (y en
    # "Valores positivos" agrupo exactamente los valores adyacentes de Schwartz:
    # Seguridad + Conformidad + Tradicion + Benevolencia + Universalismo).
    # Para escalas NUEVAS el mapa es la senal util; la probabilidad, no.
    detalles[3] <- sprintf(
      paste0("BAJO EL PHI SUPUESTO se esperarian %d factor(es) y no %d: la ",
             "simulacion sugiere que algunas dimensiones no se separarian. Es ",
             "una hipotesis a contrastar, no un pronostico. [%s]"),
      mapa$k_esperado, length(unique(x$items$dimension)), detalles[3])
    acciones <- c(acciones, sprintf(
      paste0("PRE-REGISTRAR la hipotesis alternativa antes de aplicar: ",
             "ademas del modelo teorico, declarar el modelo B con %d ",
             "factor(es) [%s] y contrastarlos como modelos rivales."),
      mapa$k_esperado,
      paste(vapply(mapa$grupos, function(g)
        paste(g, collapse = "+"), character(1)), collapse = " | ")))
    # Una fusion anticipada no bloquea, pero tampoco es "todo limpio": la
    # estructura teorica no se reproducira tal cual.
    #
    # EXCEPCION (2.9.11). La ruta de "halo local" (.mapa_fusion) funde dos
    # dimensiones por ser ambas deseables y parecidas, SIN exigir que la
    # simulacion las haya visto correlacionar. Existe porque el motor
    # INFRAESTIMA el nivel de fusion (PM real .92 / simulada .71), y como
    # correccion a priori esta bien. Pero cuando la simulacion es contundente
    # -90 CFAs y >= 95% de replicas limpias- esa correccion acaba
    # sobrescribiendo la evidencia que la propia simulacion produjo, y el eje 3
    # no puede dar "ok" ni con estructura 100% limpia y RMSEA de .02.
    # Medido sobre los 4 constructos del Experimento 1: con la regla anterior
    # NINGUNO podia tener el eje 3 en verde, pese a que dos superaban el
    # umbral de prob_limpia. Ningun par alcanzo phi >= .70 en ningun caso.
    #
    # Con esta excepcion la fusion se sigue REPORTANDO (el mapa, la accion de
    # pre-registro y el detalle no cambian): lo unico que no ocurre es la
    # degradacion automatica del estado.
    causas_reales <- mapa$causas[nzchar(mapa$causas)]
    solo_halo <- length(causas_reales) > 0 &&
      all(!grepl("phi", causas_reales, fixed = TRUE))
    prob_sim <- if (!is.null(estructura)) estructura$prob_limpia else NA_real_
    evidencia_fuerte <- !is.null(prob_sim) && length(prob_sim) == 1 &&
      !is.na(prob_sim) && prob_sim >= 0.95
    if (estados[3] == "ok" && !(solo_halo && evidencia_fuerte)) {
      estados[3] <- "advertencia"
    } else if (estados[3] == "ok") {
      detalles[3] <- paste0(detalles[3],
        "; fusion anticipada SOLO por halo local, no confirmada por la ",
        "simulacion (se reporta como hipotesis, no degrada el veredicto)")
    }
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
  # Variante del mismo caso: el UNICO riesgo es el halo de deseabilidad,
  # con redaccion aceptable, ajuste simulado bueno y el mapa de fusion
  # anticipando la fusion TOTAL de las dimensiones. La escala es aplicable
  # como puntaje global (precedente empirico PM v1: halo -> bifactor con
  # ECV=.84 y omegaH=.92 -> total interpretable).
  caso_global_halo <- !is.null(estructura) &&
    estados[1] != "riesgo" &&
    estados[2] == "riesgo" &&
    estados[3] != "riesgo" &&
    !is.na(estructura$rmsea_med) && estructura$rmsea_med <= 0.08 &&
    !is.null(mapa) && isTRUE(mapa$hay_fusion) &&
    mapa$k_esperado == 1
  caso_global <- caso_global || caso_global_halo

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

  # ---------------------------------------------------------------------------
  # ESCENARIO (2.9.13): que le pasa a la escala, no si tiene permiso
  # ---------------------------------------------------------------------------
  # Medido en 9 escalas clasicas con AFC empirico (n = 1500): el veredicto
  # decia "NO APLICAR TODAVIA" a DASS-21 (CFI .995), CFCS (.994), SD3 (.966) y
  # NPI (.976). Cuatro instrumentos publicados que funcionan. El fallo no era
  # del motor -el eje 3 acerto en 6 de 9- sino de convertir tres observaciones
  # heterogeneas en un permiso, y de que el peor de los tres mandara.
  #
  # Dos correcciones:
  #  1. La REDACCION se informa aparte y NO toca el escenario. Que dos items se
  #     parezcan afecta a la fiabilidad y a la dependencia local, pero no dice
  #     nada sobre si la estructura se sostiene: DASS-21 tiene el eje 1 en
  #     riesgo, los ejes 2 y 3 en "ok", y ajusta con CFI .995 sobre n = 1500.
  #     Llamarla "vulnerable" por como estan escritos sus items seria mezclar
  #     dos juicios distintos; se dicen los dos, por separado y ambos ciertos:
  #        "Estructura ROBUSTA · Redaccion con redundancia alta"
  #  2. El resultado se enuncia como CONDICION, con el mismo vocabulario que
  #     ya usa estres_escala(): decir de DASS-21 que es vulnerable ante
  #     dependencia local es cierto; decir que no se aplique es falso.
  peor <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return("ok")
    if (any(v == "riesgo")) "riesgo" else if (any(v == "advertencia")) "advertencia" else "ok"
  }
  # Deseabilidad, estructura y ASIGNACION deciden la condicion de la escala. La
  # redaccion sigue fuera (ver arriba: por que se informa aparte).
  # La asignacion entra porque su especificidad medida es perfecta -0 falsos
  # positivos en 39 condiciones sin defecto-, asi que no puede reintroducir el
  # problema de los falsos "no aplicar" que motivo sacar la redaccion.
  nucleo <- peor(c(estados[2:3], estados[4]))
  escenario <- if (caso_global) "SOLO PUNTAJE TOTAL" else
    switch(nucleo,
           "riesgo"      = "FRAGIL",
           "advertencia" = "VULNERABLE",
           "ROBUSTA")

  # ---------------------------------------------------------------------------
  # LA FUSION MANDA (2.9.14)
  # ---------------------------------------------------------------------------
  # El mapa de fusion existe desde 2.7.0 -nacio del caso "Valores positivos" de
  # la bateria EESTP- pero iba como coletilla del eje 3, y quien mandaba era
  # prob_limpia. Medido sobre las tres escalas de esa bateria, ya aplicadas a
  # 200 personas:
  #
  #   escala  diseno  real  mapa    prob_limpia   escenario que salia
  #   ACO       3       2     2 OK     0.800       VULNERABLE
  #   PM        2       1     1 OK     0.333       FRAGIL
  #   VP        4       2     3 ~      0.967       VULNERABLE
  #
  # El mapa acerto en dos y se quedo corto en una, mientras prob_limpia decia
  # "saldra limpia el 97% de las veces" de una escala que perdio la mitad de sus
  # items. Un usuario que lee 0.967 sigue adelante; el aviso estaba, pero no en
  # el sitio que se mira.
  #
  # Que dos factores se fundan no es un matiz del ajuste: cambia QUE mide la
  # escala. Por eso pasa al primer plano, con los grupos concretos.
  K_teor <- length(unique(x$items$dimension))
  if (!is.null(mapa) && isTRUE(mapa$hay_fusion) &&
      !is.null(mapa$k_esperado) && mapa$k_esperado < K_teor) {
    grupos_f <- Filter(function(g) length(g) > 1, mapa$grupos)
    escenario <- sprintf("SE ESPERAN %d FACTOR(ES), NO %d", mapa$k_esperado, K_teor)
    escenario_fusion <- paste(vapply(grupos_f, function(g)
      paste(g, collapse = " + "), character(1)), collapse = " · ")
    # 2.9.36: se declara de que depende. La fusion se decide con el phi, y el
    # phi es el SUPUESTO de entrada devuelto con ruido, no una lectura de la
    # escala. Medido el 22-ago-2026 sobre 99 configuraciones: el phi simulado
    # tuvo rango 0.000 (siempre ~.61) mientras el phi empirico de las mismas
    # escalas iba de .226 a .857. Y en las 3 escalas limpias sobreestimo el
    # empirico entre 1.6 y 4.2 veces, siempre por encima: de ahi que el motor
    # vea riesgo de fusion casi siempre. Decirlo no cuesta nada y evita leer
    # como pronostico lo que es una consecuencia del supuesto.
    det_fusion <- sprintf(
      paste0("BAJO EL PHI SUPUESTO, la simulacion anticipa que estas dimensiones ",
             "no se separarian: %s. Es una posibilidad a comprobar, no un ",
             "pronostico: la fusion se decide con el phi, que es el supuesto de ",
             "entrada y no se lee de la escala. Si se funden, la escala mediria ",
             "algo distinto de lo que se diseno%s"),
      escenario_fusion,
      if (nucleo == "riesgo") " (y ademas la estructura sale fragil)" else "")
  } else {
    escenario_fusion <- NULL; det_fusion <- NULL
  }
  calidad_redaccion <- switch(estados[1],
    "ok"          = "limpia",
    "advertencia" = "con solapamientos leves",
    "riesgo"      = "con redundancia alta (revisar antes de publicar la escala)",
    NA_character_)

  # el detalle de la fusion, si lo hay, gana al generico
  escenario_detalle <- if (!is.null(det_fusion)) det_fusion else switch(escenario,
    "ROBUSTA"    = "la estructura se sostiene en las condiciones simuladas",
    "VULNERABLE" = paste0("la estructura se sostiene, pero es sensible a ",
                          "las condiciones simuladas: ver que eje avisa"),
    "FRAGIL"     = paste0("la estructura NO se sostiene en las condiciones ",
                          "simuladas: corregir antes de ir a campo"),
    "SOLO PUNTAJE TOTAL" = paste0("utilizable puntuando el total; las ",
                          "subescalas no se separaran"),
    NA_character_)

  # 2.9.36: el aviso de asignacion no puede perderse cuando la fusion se lleva
  # el titular. Medido el 22-ago-2026 sobre eammi: las CUATRO condiciones -la
  # limpia incluida- salieron con el mismo escenario "SE ESPERAN 1 FACTOR(ES),
  # NO 2", porque el bloque de fusion sobrescribe 'escenario' DESPUES de
  # calcular el nucleo; en CL4_r1 el eje de asignacion marcaba riesgo y no se
  # veia por ningun lado en la linea que el usuario mira.
  # La cadena de 'escenario' NO se toca: la app v2 la parsea con un regex
  # anclado (app.R L3215) y la traduce a "PODRIAN SALIR N FACTORES". El aviso
  # se antepone aqui, que es la linea inmediatamente debajo del titular tanto
  # en el print del paquete como en la app.
  if (!is.null(asig) && !is.null(asig$items) && asig$n_mal_asignados > 0) {
    ma <- asig$mal_asignados
    # Se AGRUPA por par de dimensiones: con dos dimensiones el par es siempre el
    # mismo y listarlo una vez por item daba "[D1 -> D2], [D1 -> D2], [D1 -> D2],
    # [D1 -> D2] y 1 mas", que no dice nada. Agrupado: "5 de D1 a D2".
    tb <- table(sprintf("%s a %s", ma$dimension, ma$dim_mas_cercana))
    tb <- sort(tb, decreasing = TRUE)
    pares <- sprintf("%d de %s", as.integer(tb), names(tb))
    muestra <- paste(utils::head(pares, 4), collapse = ", ")
    if (length(pares) > 4)
      muestra <- sprintf("%s y %d par(es) mas", muestra, length(pares) - 4)
    aviso_asig <- sprintf(
      paste0("%d item(s) estan en la dimension equivocada: se parecen mas a ",
             "otra dimension que a la suya %s. Eso se corrige ANTES de mirar ",
             "nada mas: mientras esten ahi, la estructura que se simula no es ",
             "la que se diseno"),
      asig$n_mal_asignados, muestra)
    escenario_detalle <- if (is.na(escenario_detalle)) aviso_asig
      else paste0(aviso_asig, ". Ademas, ", escenario_detalle)
  }

  out <- list(
    # veredicto: se conserva el vocabulario antiguo porque converger_escala(),
    # optimizar_para_campo() y el asistente ramifican sobre estas cadenas.
    # Lo que se muestra al usuario es 'escenario'.
    escenario         = escenario,
    # dimensiones que la simulacion anticipa que NO se separaran (NULL si no hay)
    escenario_fusion  = escenario_fusion,
    # defectos visibles en el diseno, sin simular (ver bloque de arriba)
    avisos_diseno     = if (length(avisos_diseno)) avisos_diseno else NULL,
    escenario_detalle = escenario_detalle,
    calidad_redaccion = calidad_redaccion,
    semaforo = data.frame(
      paso    = c("redaccion", "deseabilidad", "estructura_simulada",
                  "asignacion"),
      estado  = estados,
      detalle = detalles,
      stringsAsFactors = FALSE
    ),
    veredicto    = veredicto,
    acciones     = unique(acciones),
    redaccion    = redaccion,
    asignacion   = asig,
    deseabilidad = deseab,
    estructura   = estructura,
    mapa_fusion  = mapa,
    estructura_alternativa = estructura_alternativa,
    # banda: NULL si n_banda = 0 o si el juez no dejo pasadas utilizables
    banda_estructura = if (exists("banda", inherits = FALSE)) banda else NULL,
    parametros   = list(umbral_sem = umbral_sem, umbral_faceta = umbral_faceta,
                        n = n, n_rep = n_rep, n_banda = n_banda,
                        fecha = format(Sys.Date()))
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
  # 2.9.13: se muestra el ESCENARIO (condicion de la escala), no el permiso.
  # Se mantiene x$veredicto en el objeto por compatibilidad interna.
  esc <- x$escenario %||% x$veredicto
  col_ver <- if (esc %in% c("ROBUSTA", "LISTA PARA CAMPO")) .color_verde
             else .color_amarillo
  cat("  ESCENARIO PREVISTO: ", col_ver(esc), "\n", sep = "")
  # la fusion, si la hay, con los grupos concretos y en su propia linea
  if (!is.null(x$escenario_fusion))
    cat("    Podrian no separarse: ", .color_amarillo(x$escenario_fusion), "\n", sep = "")
  # Los pares ORDENADOS. El sí/no del umbral se queda con una decision binaria y
  # tira el matiz: en "Valores positivos" un par estaba en .73 y los otros cinco
  # entre .59 y .62 -- ver ese salto dice mas que el veredicto.
  pp <- x$estructura$phi_pares
  if (!is.null(pp) && is.matrix(pp) && nrow(pp) > 1) {
    ij <- which(upper.tri(pp), arr.ind = TRUE)
    v  <- pp[upper.tri(pp)]
    o  <- order(-v)
    cat("  PARES DE DIMENSIONES (phi simulado, de mayor a menor):\n")
    for (q in utils::head(o, 6)) {
      if (!is.finite(v[q])) next
      marca <- if (v[q] >= 0.65) " <- riesgo de que no se separen" else ""
      cat(sprintf("    %-28s ~ %-28s %.3f%s\n",
                  substr(rownames(pp)[ij[q,1]], 1, 28),
                  substr(colnames(pp)[ij[q,2]], 1, 28), v[q], marca))
    }
    if (length(o) > 6) cat("    ... y ", length(o) - 6, " par(es) mas\n", sep = "")
  }
  if (!is.null(x$escenario_detalle) && !is.na(x$escenario_detalle))
    cat("    ", x$escenario_detalle, "\n", sep = "")
  if (!is.null(x$calidad_redaccion) && !is.na(x$calidad_redaccion))
    cat("  REDACCION DE LOS ITEMS: ", x$calidad_redaccion,
        "\n    (afecta a la fiabilidad y a la dependencia local; no impide aplicar)\n", sep = "")
  if (length(x$acciones) > 0) {
    cat("  ACCIONES RECOMENDADAS:\n")
    for (a in x$acciones) {
      cat("   - ", a, "\n", sep = "")
    }
  }
  cat("===========================================================\n\n")
  invisible(x)
}
