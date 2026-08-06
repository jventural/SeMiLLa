# =============================================================================
# SeMiLLa - Optimizacion guiada por la compuerta pre-aplicacion
# =============================================================================
#
# La compuerta (compuerta_pre_aplicacion) DIAGNOSTICA; esta funcion CORRIGE:
# convierte cada hallazgo en una accion concreta sobre los items (podar
# clusters de faceta, reemplazar pares redundantes y muletillas, reescribir
# con menor deseabilidad) y vuelve a pasar la compuerta, iterando hasta
# alcanzar el veredicto objetivo o agotar las iteraciones. Si al agotar las
# iteraciones planificadas quedan hallazgos de redaccion CORREGIBLES
# (facetas o muletillas nuevas), se concede un numero limitado de
# iteraciones extra en vez de detenerse con trabajo pendiente.

#' @title Optimizar la escala hasta pasar la compuerta pre-aplicacion
#'
#' @description
#' Bucle de correccion automatica guiado por
#' \code{\link{compuerta_pre_aplicacion}}. En cada iteracion:
#'
#' \enumerate{
#'   \item \strong{Poda de facetas repetidas}: de cada cluster de 3+ items
#'         que parafrasean la misma conducta conserva los
#'         \code{conservar_por_cluster} mas prototipicos (mayor similitud
#'         media con su cluster) y marca el resto para reemplazo.
#'   \item \strong{Poda de muletillas}: si 3+ items comparten la misma
#'         formula literal (p. ej. todos terminando en "aunque me incomode"
#'         o empezando con "Dedico tiempo a..."), conserva uno y marca el
#'         resto. Las formulas repetidas son un artefacto tipico de los
#'         reemplazos generados en tanda por el LLM.
#'   \item \strong{Poda de pares redundantes}: del par conserva el item menos
#'         redundante con el resto de la escala y marca el otro.
#'   \item \strong{Regeneracion dirigida}: cada item marcado se reemplaza con
#'         el LLM bajo restricciones adicionales: (a) PROHIBIDAS las
#'         conductas y muletillas vetadas (acumuladas entre iteraciones),
#'         (b) variacion sintactica obligatoria, (c) longitud dentro del
#'         rango de la escala (homogeneidad), y (d) si la compuerta detecto
#'         riesgo de deseabilidad, conducta especifica con costo personal.
#'   \item \strong{Re-auditoria}: recalcula embeddings y vuelve a pasar la
#'         compuerta completa (redaccion, deseabilidad, simulacion).
#' }
#'
#' El bucle termina cuando el veredicto alcanza \code{veredicto_objetivo}.
#' Si se agotan las \code{max_iteraciones} pero quedan hallazgos de
#' redaccion corregibles (facetas o muletillas), se conceden hasta
#' \code{max_iteraciones_extra} iteraciones adicionales; si el bloqueo
#' restante NO es corregible reescribiendo items (formato de respuesta,
#' deseabilidad estructural del constructo), se detiene y lo informa.
#'
#' Desde v2.7.0, \code{\link{semilla}} invoca esta funcion automaticamente
#' cuando la compuerta devuelve "NO APLICAR TODAVIA" (parametro
#' \code{optimizar = TRUE}).
#'
#' @param x Objeto \code{semilla} con \code{$items} y \code{$similitud}.
#' @param api_key Clave del proveedor LLM.
#' @param max_iteraciones Iteraciones planificadas de correccion (default 3).
#' @param max_iteraciones_extra Iteraciones ADICIONALES concedidas solo si al
#'   agotar las planificadas quedan facetas o muletillas corregibles
#'   (default 2). Evita detenerse con trabajo de redaccion pendiente sin
#'   permitir bucles infinitos.
#' @param veredicto_objetivo Veredicto con el que se detiene el bucle:
#'   \code{"LISTA PARA CAMPO"} (default, exigente),
#'   \code{"APLICAR CON CAUTELA"} (acepta advertencias) o
#'   \code{"APLICAR COMO ESCALA GLOBAL"} (acepta que las subescalas no se
#'   separen si el puntaje total es utilizable). Orden de severidad:
#'   NO APLICAR < GLOBAL < CAUTELA < LISTA.
#' @param conservar_por_cluster Items que se conservan de cada cluster de
#'   faceta repetida (default 2).
#' @param max_reemplazos_prop Proporcion maxima de la escala reemplazable por
#'   iteracion (default 0.5).
#' @param n_rep Replicas por escenario en la simulacion de la compuerta
#'   FINAL (la que emite el veredicto reportado; default 100).
#' @param n_rep_intermedio Replicas por escenario en las compuertas
#'   INTERMEDIAS del bucle (default 40). Las intermedias solo orientan la
#'   correccion (mejoro o no?); la precision completa se reserva para la
#'   compuerta final. Con escalas grandes (32 items, 4 factores) esto
#'   recorta cerca de la mitad del tiempo total sin perder rigor en el
#'   veredicto reportado.
#' @param modelo Modelo LLM para los reemplazos y la deseabilidad.
#' @param poblacion Poblacion objetivo; si NULL se toma de
#'   \code{x$metadata$poblacion}.
#' @param seed Semilla de la simulacion.
#' @param verbose Mostrar progreso.
#'
#' @return El objeto \code{semilla} corregido, con:
#' \itemize{
#'   \item \code{compuerta}: la compuerta de la version final.
#'   \item \code{optimizacion}: lista con \code{iteraciones},
#'         \code{historial} (data.frame iteracion x veredicto x
#'         n_reemplazos x score), \code{reemplazos} (data.frame con item
#'         viejo, item nuevo, dimension y motivo), \code{balance}
#'         (data.frame antes/despues por indicador, con el sentido del
#'         cambio) y \code{revertido} (TRUE si la ultima iteracion degrado
#'         la escala y se devolvio una version anterior).
#' }
#'
#' @section No regresion:
#' El bucle puede \emph{empeorar} la escala mientras corrige: al romper un
#' cluster de faceta puede introducir parafrasis gemelas, o desbalancear la
#' deseabilidad DENTRO de una dimension. Por eso cada version se puntua con
#' \code{.score_compuerta()} (veredicto primero; luego gemelos, facetas,
#' pares, alertas de deseabilidad y probabilidad de estructura limpia) y se
#' devuelve la MEJOR version vista, no la ultima. \code{$optimizacion$balance}
#' muestra que indicador mejoro y cual empeoro respecto del punto de partida.
#'
#' @examples
#' \dontrun{
#' esc <- semilla("personalidad moral", api_key = key)  # compuerta + optimiza
#' esc$optimizacion$historial
#' esc$optimizacion$reemplazos[, c("item_viejo", "item_nuevo", "motivo")]
#' }
#'
#' @seealso \code{\link{compuerta_pre_aplicacion}}, \code{\link{refinar_escala}}
#'
#' @export
optimizar_para_campo <- function(x,
                                 api_key,
                                 max_iteraciones       = 3,
                                 max_iteraciones_extra = 2,
                                 veredicto_objetivo    = c("LISTA PARA CAMPO",
                                                           "APLICAR CON CAUTELA",
                                                           "APLICAR COMO ESCALA GLOBAL"),
                                 conservar_por_cluster = 2L,
                                 max_reemplazos_prop   = 0.5,
                                 n_rep                 = 100,
                                 n_rep_intermedio      = 40,
                                 modelo                = "gpt-4.1-mini",
                                 poblacion             = NULL,
                                 seed                  = 2026,
                                 verbose               = TRUE) {

  veredicto_objetivo <- match.arg(veredicto_objetivo)
  if (is.null(x$items) || is.null(x$items$item))
    stop("'x' debe contener $items con la columna 'item'.")
  if (is.null(poblacion)) poblacion <- x$metadata$poblacion

  openai <- .configurar_openai(api_key, modelo = modelo)

  # Compuerta inicial (si no viene ya calculada). Usa las replicas
  # intermedias: solo orienta el bucle; el veredicto final se re-estima
  # con n_rep completo al terminar.
  if (is.null(x$compuerta)) {
    if (verbose) cat("\n[optimizar_para_campo] Compuerta inicial...\n")
    x$compuerta <- compuerta_pre_aplicacion(
      x, api_key = api_key, poblacion = poblacion,
      n_rep = n_rep_intermedio, modelo = modelo, seed = seed,
      verbose = verbose)
  }

  historial <- data.frame(
    iteracion = 0L,
    veredicto = x$compuerta$veredicto,
    n_reemplazos = 0L,
    score = round(.score_compuerta(x$compuerta), 2),
    stringsAsFactors = FALSE
  )
  reemplazos <- data.frame()

  # NO-REGRESION: el bucle puede empeorar la escala mientras corrige (romper
  # un cluster de faceta e introducir gemelos, o desbalancear la deseabilidad
  # dentro de una dimension). Se guarda la MEJOR version vista y es esa la que
  # se devuelve, no la ultima.
  mejor <- list(items = x$items, embeddings = x$embeddings,
                similitud = x$similitud, compuerta = x$compuerta,
                score = .score_compuerta(x$compuerta), iteracion = 0L,
                n_reemplazos = 0L)
  compuerta_inicial <- x$compuerta

  # Conductas y muletillas prohibidas ACUMULADAS entre iteraciones: sin esta
  # memoria, los reemplazos de una tanda derivan hacia una formula nueva
  # comun (validado con PM: iter 1 elimino "ayudo a companeros" y creo
  # "dedico tiempo a corregir"), y el bucle persigue facetas moviles.
  prohibidas_acum <- character(0)

  it <- 0L
  tope_total <- max_iteraciones + max_iteraciones_extra
  while (it < tope_total) {
    if (.veredicto_cumple(x$compuerta$veredicto, veredicto_objetivo)) break

    plan <- .plan_reemplazos_compuerta(
      x, conservar_por_cluster = conservar_por_cluster,
      max_prop = max_reemplazos_prop)

    if (nrow(plan) == 0) {
      # Nada que reemplazar por redaccion (p.ej. solo falla la simulacion
      # o la deseabilidad global): no hay accion automatica segura.
      if (verbose) {
        cat("  Sin items reemplazables automaticamente: los hallazgos\n")
        cat("  restantes requieren decision humana (formato de respuesta,\n")
        cat("  anclas de frecuencia, rediseno de dimensiones).\n")
      }
      break
    }

    it <- it + 1L
    es_extra <- it > max_iteraciones

    if (verbose) {
      cat("\n", .linea("="), "\n", sep = "")
      cat(.color_amarillo(paste0(
        "[OPTIMIZACION ", it, "/", max_iteraciones,
        if (es_extra) paste0(" +EXTRA (quedan hallazgos de redaccion",
                             " corregibles)") else "",
        "] veredicto actual: ", x$compuerta$veredicto)), "\n")
      cat(.linea("="), "\n")
    }

    anti_halo <- any(x$compuerta$semaforo$estado[
      x$compuerta$semaforo$paso == "deseabilidad"] %in%
        c("riesgo", "advertencia"))

    # Acumular TODOS los nucleos y muletillas detectados como vetados
    prohibidas_acum <- unique(c(
      prohibidas_acum,
      plan$conducta_prohibida[nzchar(plan$conducta_prohibida)],
      x$compuerta$redaccion$facetas_repetidas$nucleo_lexico
    ))
    prohibidas_acum <- prohibidas_acum[nzchar(prohibidas_acum)]

    # Rango de longitud objetivo: homogeneo con la escala actual (los
    # reemplazos no deben destacar por largos; DeVellis: evitar items
    # excepcionalmente largos).
    rango_palabras <- .rango_palabras_escala(x$items$item)
    # Los reemplazos deben respetar el tope de palabras pedido por el usuario
    # (complejidad linguistica); sin este techo el rango derivado de la escala
    # permitia items de hasta 16 palabras en escalas de nivel "minimo".
    maxp_meta <- suppressWarnings(as.integer(x$metadata$max_palabras %||% NA))
    if (!is.na(maxp_meta)) {
      rango_palabras[2] <- min(rango_palabras[2], maxp_meta)
      rango_palabras[1] <- min(rango_palabras[1], rango_palabras[2])
    }

    if (verbose) {
      cat("  Items a reemplazar: ", nrow(plan),
          if (anti_halo) "  (con restriccion anti-halo)" else "", "\n", sep = "")
      if (length(prohibidas_acum) > 0) {
        cat("  Conductas/muletillas vetadas: ",
            paste(prohibidas_acum, collapse = " | "), "\n", sep = "")
      }
      cat("  Longitud objetivo: ", rango_palabras[1], "-", rango_palabras[2],
          " palabras\n", sep = "")
    }

    # Umbral de aceptacion del candidato = el MISMO que usara la compuerta
    # para detectarlo. Antes el filtro era fijo en 0.70 mientras la compuerta
    # detecta con un umbral adaptativo en [0.62, 0.70]: cada reemplazo se
    # aceptaba y a la iteracion siguiente aparecia como par nuevo, y el bucle
    # perseguia su propia cola (5 de 15 reemplazos del caso 2026-08-03 fueron
    # reemplazos de reemplazos).
    umbral_acept <- x$compuerta$redaccion$parametros$umbral_sem %||% 0.70
    # Margen de seguridad: quedar JUSTO en el umbral vuelve a marcarse en
    # cuanto la linea base de similitud se mueve al cambiar los items.
    umbral_acept <- max(0.40, umbral_acept - 0.03)

    # Deseabilidad por item (si la compuerta la calculo): sirve para pedir que
    # el reemplazo conserve la POLARIDAD del item que sustituye. Sin esto, el
    # optimizador mete conductas de evitacion (comprometedoras) junto a items
    # fisiologicos (inocuos) y abre un contraste DENTRO de la dimension, que
    # es justo la alerta que fragmenta el factor.
    desea_vec <- x$compuerta$deseabilidad$deseabilidad
    if (!is.null(desea_vec) && length(desea_vec) != nrow(x$items))
      desea_vec <- NULL

    # Embeddings vivos: se actualizan con cada candidato aceptado para que el
    # siguiente reemplazo de la MISMA tanda ya lo tenga en cuenta.
    emb_vivos <- x$embeddings

    n_ok <- 0L
    for (r in seq_len(nrow(plan))) {
      idx_r <- plan$idx[r]
      # Items que SI se conservan en esa dimension: el prompt debe listarlos
      # para exigir una manifestacion distinta a todas ellas. Sin esta lista,
      # los reemplazos de una dimension derivan todos hacia la misma idea
      # (9 de 15 del caso 2026-08-03 acabaron en "dejar preguntas en blanco").
      conservados <- x$items$item[x$items$dimension == x$items$dimension[idx_r]]
      conservados <- setdiff(conservados, x$items$item[idx_r])

      nuevo <- .reemplazar_item_dirigido(
        x = x, openai = openai, modelo = modelo,
        idx_item = idx_r,
        motivo = plan$motivo[r],
        conductas_prohibidas = prohibidas_acum,
        anti_halo = anti_halo,
        poblacion = poblacion,
        rango_palabras = rango_palabras,
        umbral_redundancia = umbral_acept,
        embeddings_existentes = emb_vivos,
        items_dimension_conservados = conservados,
        deseabilidad_objetivo = .desea_dimension(desea_vec, x$items, idx_r),
        deseabilidad_otras    = .desea_otras_dim(desea_vec, x$items, idx_r)
      )
      # Contrato ampliado: devuelve list(item, emb) para poder mantener
      # emb_vivos sin re-embeber toda la escala en cada candidato.
      emb_nuevo <- NULL
      if (is.list(nuevo)) { emb_nuevo <- nuevo$emb; nuevo <- nuevo$item }
      if (!is.null(nuevo) && !is.null(emb_nuevo) && !is.null(emb_vivos) &&
          length(emb_nuevo) == ncol(emb_vivos)) {
        emb_vivos[idx_r, ] <- emb_nuevo
      }
      if (!is.null(nuevo)) {
        reemplazos <- rbind(reemplazos, data.frame(
          iteracion  = it,
          dimension  = x$items$dimension[plan$idx[r]],
          item_viejo = x$items$item[plan$idx[r]],
          item_nuevo = nuevo,
          motivo     = plan$motivo[r],
          stringsAsFactors = FALSE
        ))
        x$items$item[plan$idx[r]] <- nuevo
        n_ok <- n_ok + 1L
        if (verbose) {
          cat("    ", .color_check(), " [", x$items$dimension[plan$idx[r]],
              "] ", substr(nuevo, 1, 62),
              if (nchar(nuevo) > 62) "..." else "", "\n", sep = "")
        }
      } else if (verbose) {
        cat("    ", .color_warning(), " No se logro reemplazo no redundante; ",
            "se mantiene el item ", plan$idx[r], "\n", sep = "")
      }
    }

    if (n_ok == 0L) {
      if (verbose) cat("  Ningun reemplazo viable en esta iteracion.\n")
      break
    }

    # Re-representar y re-auditar la escala corregida (replicas intermedias)
    if (verbose) cat("\n  Recalculando embeddings y re-pasando la compuerta...\n")
    emb <- obtener_embeddings(items = x, api_key = api_key, verbose = FALSE)
    x$embeddings <- emb$embeddings
    x$similitud  <- emb$similitud

    x$compuerta <- compuerta_pre_aplicacion(
      x, api_key = api_key, poblacion = poblacion,
      n_rep = n_rep_intermedio, modelo = modelo, seed = seed,
      verbose = verbose)

    sc <- .score_compuerta(x$compuerta)
    historial <- rbind(historial, data.frame(
      iteracion = it, veredicto = x$compuerta$veredicto,
      n_reemplazos = n_ok, score = round(sc, 2), stringsAsFactors = FALSE))

    if (sc > mejor$score) {
      mejor <- list(items = x$items, embeddings = x$embeddings,
                    similitud = x$similitud, compuerta = x$compuerta,
                    score = sc, iteracion = it, n_reemplazos = n_ok)
    } else if (verbose) {
      cat("  ", .color_warning(), " Esta iteracion NO mejora la version ",
          "guardada (iter ", mejor$iteracion, "): se conserva aquella.\n",
          sep = "")
    }
  }

  # NO-REGRESION: se devuelve la MEJOR version vista, no la ultima. Si la
  # ultima iteracion degrado la escala, se revierte a la guardada y se recorta
  # la trazabilidad de reemplazos a las iteraciones que si sobrevivieron.
  revertido <- FALSE
  if (it > 0 && .score_compuerta(x$compuerta) < mejor$score) {
    revertido <- TRUE
    if (verbose) {
      cat("\n  ", .color_warning(), " La ultima version es PEOR que la de la ",
          "iteracion ", mejor$iteracion, ": se revierte a esa.\n", sep = "")
    }
    x$items      <- mejor$items
    x$embeddings <- mejor$embeddings
    x$similitud  <- mejor$similitud
    x$compuerta  <- mejor$compuerta
    if (nrow(reemplazos) > 0)
      reemplazos <- reemplazos[reemplazos$iteracion <= mejor$iteracion, ,
                               drop = FALSE]
    it <- mejor$iteracion
  }

  # Compuerta FINAL con las replicas completas: es la que emite el veredicto
  # reportado. Solo se re-estima si hubo iteraciones (la escala cambio) y si
  # el presupuesto final difiere del intermedio.
  if (it > 0 && n_rep > n_rep_intermedio) {
    if (verbose) cat("\n  Compuerta FINAL (replicas completas: ", n_rep, ")...\n", sep = "")
    x$compuerta <- compuerta_pre_aplicacion(
      x, api_key = api_key, poblacion = poblacion,
      n_rep = n_rep, modelo = modelo, seed = seed, verbose = verbose)
    historial <- rbind(historial, data.frame(
      iteracion = it, veredicto = x$compuerta$veredicto,
      n_reemplazos = 0L, score = round(.score_compuerta(x$compuerta), 2),
      stringsAsFactors = FALSE))
  }

  # Comparacion honesta antes/despues: que mejoro y que empeoro. El bucle
  # optimiza redundancia semantica; puede pagar ese arreglo en otro eje, y el
  # usuario tiene que verlo sin recalcular nada.
  g0 <- historial$veredicto[1]
  balance <- .balance_optimizacion(compuerta_inicial, x$compuerta)

  x$optimizacion <- list(
    iteraciones = max(historial$iteracion),
    veredicto_objetivo = veredicto_objetivo,
    objetivo_alcanzado = .veredicto_cumple(x$compuerta$veredicto,
                                           veredicto_objetivo),
    veredicto_inicial = g0,
    revertido = revertido,
    balance = balance,
    historial = historial,
    reemplazos = reemplazos
  )

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat("  Optimizacion terminada: ", nrow(reemplazos),
        " item(s) reemplazado(s) en ", max(historial$iteracion),
        " iteracion(es).\n", sep = "")
    cat("  Escenario previsto final: ", x$compuerta$veredicto,
        if (x$optimizacion$objetivo_alcanzado) paste0(" (objetivo '",
          veredicto_objetivo, "' alcanzado)") else
          paste0(" (objetivo '", veredicto_objetivo,
                 "' NO alcanzado: revisar x$compuerta$acciones)"),
        "\n", sep = "")
    if (revertido)
      cat("  (se revirtio a la iteracion ", mejor$iteracion,
          ": las posteriores empeoraban la escala)\n", sep = "")
    cat("\n  BALANCE antes -> despues:\n")
    for (i in seq_len(nrow(balance))) {
      b <- balance[i, ]
      cat(sprintf("    %-12s %6s -> %6s   %s\n", b$indicador,
                  if (is.na(b$antes)) "-" else format(round(b$antes, 2)),
                  if (is.na(b$despues)) "-" else format(round(b$despues, 2)),
                  switch(b$cambio, "mejora" = "[mejora]",
                         "empeora" = "[EMPEORA]", b$cambio)))
    }
    if (any(balance$cambio == "empeora")) {
      cat("\n  ", .color_warning(), " Este optimizador minimiza redundancia ",
          "semantica: puede pagar ese arreglo en otro eje. Revisa las filas ",
          "marcadas [EMPEORA] antes de dar la escala por buena.\n", sep = "")
    }
    cat(.linea("-"), "\n")
  }

  x
}


# =============================================================================
# Helpers internos
# =============================================================================

# Tipo de una dimension segun su nombre y definicion. Las restricciones de
# correccion DEBEN respetar el tipo: exigir "conducta observable con costo"
# a una dimension COGNITIVA convierte creencias en conductas (bug detectado
# con ACO2: el optimizador destruyo la dimension Cognitiva que en el piloto
# empirico funcionaba, transformando "Enterarme de favoritismos reduce mi
# confianza" en "dedico tiempo a recopilar evidencia y denunciarlo").

#' @keywords internal
.tipo_dimension <- function(nombre, definicion = "") {
  t <- tolower(paste(nombre, definicion))
  t <- chartr("áéíóúüñ",
              "aeiouun", t)
  if (grepl("cognitiv|creenc|juicio|pienso|piensa|belief|opinion", t)) return("cognitiva")
  if (grepl("afectiv|emocion|sentimient|siento|siente|feel", t))       return("afectiva")
  if (grepl("conductual|conducta|disposicion|comportamiento|behav|accion", t)) return("conductual")
  "general"
}


# Orden de severidad de los veredictos de la compuerta.

#' @keywords internal
.veredicto_cumple <- function(veredicto, objetivo) {
  rango <- c("NO APLICAR TODAVIA" = 1L,
             "APLICAR COMO ESCALA GLOBAL" = 2L,
             "APLICAR CON CAUTELA" = 3L,
             "LISTA PARA CAMPO" = 4L)
  r_v <- rango[veredicto]; r_o <- rango[objetivo]
  if (is.na(r_v) || is.na(r_o)) return(FALSE)
  r_v >= r_o
}


# Puntaje ordenable de una compuerta: MAYOR = mejor. Existe porque el bucle
# de optimizacion puede DEGRADAR la escala mientras corrige (caso verificado
# 2026-08-03, escala de ansiedad ante la estadistica: los 2 clusters de
# faceta se eliminaron, pero aparecieron 2 gemelos confirmados y un
# desbalance de deseabilidad INTRA-dimension que antes no existia). Sin este
# puntaje el bucle devolvia la ULTIMA version, no la mejor.
#
# El veredicto manda (bloque de 1000); dentro del mismo veredicto se premia
# menos redundancia, ausencia de alertas de deseabilidad y mayor
# probabilidad de estructura limpia.

# Comparacion antes/despues de la optimizacion en los indicadores que la
# compuerta vigila. Devuelve un data.frame con el valor inicial, el final y
# el sentido del cambio, para que el usuario vea de un vistazo que se arreglo
# y que se rompio (arreglar un eje a costa de otro es el modo de fallo tipico
# de este optimizador).

#' @title Comparar dos compuertas indicador por indicador
#'
#' @description
#' Devuelve que mejoro y que empeoro entre dos ejecuciones de
#' \code{\link{compuerta_pre_aplicacion}}: gemelos, facetas repetidas, pares,
#' deseabilidad intra y entre dimensiones, probabilidad de estructura limpia y
#' |Phi|. Es la tabla que \code{\link{optimizar_para_campo}} imprime al
#' terminar, expuesta para poder compararla en cualquier otro punto del flujo
#' (por ejemplo antes y despues del refinamiento).
#'
#' @param g0,g1 Objetos \code{semilla_compuerta} (antes y despues).
#' @return \code{data.frame} con indicador, antes, despues y sentido del cambio
#'   (\code{"mejora"}, \code{"empeora"}, \code{"igual"} o \code{"cambio"}).
#' @seealso \code{\link{compuerta_pre_aplicacion}}, \code{\link{optimizar_para_campo}}
#' @export
balance_optimizacion <- function(g0, g1) .balance_optimizacion(g0, g1)

#' @keywords internal
.balance_optimizacion <- function(g0, g1) {
  ind <- function(g) {
    red <- g$redaccion
    c(gemelos  = if (!is.null(red$gemelos_llm)) nrow(red$gemelos_llm) else 0,
      facetas  = if (!is.null(red$facetas_repetidas)) nrow(red$facetas_repetidas) else 0,
      pares    = if (!is.null(red$pares_redundantes)) nrow(red$pares_redundantes) else 0,
      desea_intra = g$deseabilidad$sd_intra_dim %||% NA_real_,
      desea_entre = g$deseabilidad$sd_entre_dim %||% NA_real_,
      prob_limpia = g$estructura$prob_limpia %||% NA_real_,
      phi         = g$estructura$phi_med %||% NA_real_)
  }
  a <- ind(g0); b <- ind(g1)
  # Sentido deseable de cada indicador: -1 = conviene que baje, +1 = que suba.
  bueno <- c(gemelos = -1, facetas = -1, pares = -1, desea_intra = -1,
             desea_entre = 1, prob_limpia = 1, phi = 0)
  # Minimo para considerar que algo cambio. Los conteos son enteros y cualquier
  # diferencia cuenta; los continuos se PINTAN con dos decimales, asi que una
  # diferencia por debajo de .005 produce una fila que dice "0.08 -> 0.08
  # empeora" y se lee como un error de la app (visto en la corrida completa del
  # 2026-08-06). Se declara igual lo que se muestra igual.
  minimo <- c(gemelos = 1e-8, facetas = 1e-8, pares = 1e-8,
              desea_intra = 0.005, desea_entre = 0.005,
              prob_limpia = 0.005, phi = 0.005)
  cambio <- vapply(names(a), function(k) {
    if (is.na(a[[k]]) || is.na(b[[k]])) return("—")
    d <- b[[k]] - a[[k]]
    if (abs(d) < (minimo[[k]] %||% 1e-8)) return("igual")
    if (bueno[[k]] == 0) return("cambio")
    if (sign(d) == bueno[[k]]) "mejora" else "empeora"
  }, character(1))
  data.frame(indicador = names(a), antes = as.numeric(a),
             despues = as.numeric(b), cambio = unname(cambio),
             stringsAsFactors = FALSE, row.names = NULL)
}


#' @keywords internal
.score_compuerta <- function(g) {
  if (is.null(g)) return(-Inf)
  rango <- c("NO APLICAR TODAVIA" = 1L,
             "APLICAR COMO ESCALA GLOBAL" = 2L,
             "APLICAR CON CAUTELA" = 3L,
             "LISTA PARA CAMPO" = 4L)
  niv <- rango[g$veredicto]
  if (is.na(niv)) niv <- 0L

  red   <- g$redaccion
  n_gem <- if (!is.null(red$gemelos_llm)) nrow(red$gemelos_llm) else 0L
  fac   <- red$facetas_repetidas
  n_fac <- if (!is.null(fac)) nrow(fac) else 0L
  n_par <- if (!is.null(red$pares_redundantes)) nrow(red$pares_redundantes) else 0L

  des    <- g$deseabilidad
  intra  <- if (isTRUE(des$alerta_intra)) 1L else 0L
  halo   <- if (isTRUE(des$riesgo_halo))  1L else 0L
  unifor <- if (isTRUE(des$uniforme))     1L else 0L

  prob <- g$estructura$prob_limpia
  if (is.null(prob) || is.na(prob)) prob <- 0

  # Un gemelo confirmado por el juez LLM pesa mas que un cluster, y un
  # cluster mas que un par suelto (jerarquia de la propia compuerta).
  penal <- 4 * n_gem + 2.5 * n_fac + 0.5 * n_par +
           5 * intra + 8 * halo + 2 * unifor
  as.numeric(niv) * 1000 - penal + 10 * prob
}


# Rango de longitud (en palabras) para reemplazos: mediana de la escala
# actual con margen de +-3/+4, acotado a [6, 16]. Mantiene la homogeneidad
# de longitud (items excepcionalmente largos comprometen la comprension y
# rompen la uniformidad superficial de la escala).

#' @keywords internal
.rango_palabras_escala <- function(textos) {
  np <- vapply(strsplit(trimws(textos), "\\s+"), length, integer(1))
  med <- stats::median(np, na.rm = TRUE)
  c(max(6L, as.integer(med) - 3L), min(16L, as.integer(med) + 4L))
}


# Detecta MULETILLAS: n-gramas de 2-4 palabras compartidos literalmente por
# >= min_items items. A diferencia de las facetas (similitud semantica),
# aqui se busca la formula superficial repetida ("aunque me incomode",
# "dedico tiempo a"), artefacto tipico de reemplazos generados en tanda.
# Devuelve data.frame(frase, n_items, idx_csv) con las frases maximales.

#' @keywords internal
.detectar_muletillas <- function(textos, min_items = 3L) {
  vacio <- data.frame(frase = character(0), n_items = integer(0),
                      idx_csv = character(0), stringsAsFactors = FALSE)
  n <- length(textos)
  if (n < min_items) return(vacio)

  stop_es <- c("de", "la", "el", "los", "las", "a", "en", "que", "y", "o",
               "un", "una", "mi", "mis", "me", "se", "es", "lo", "le",
               "del", "al", "su", "sus", "por", "con", "para", "como")
  .tok <- function(t) {
    t <- tolower(t)
    t <- chartr("áéíóúüñ",
                "aeiouun", t)
    t <- gsub("[^[:alnum:][:space:]]", " ", t)
    w <- strsplit(trimws(t), "\\s+")[[1]]
    w[nzchar(w)]
  }
  toks <- lapply(textos, .tok)

  # n-gramas unicos por item (2 a 4 palabras)
  ngrams_item <- lapply(toks, function(w) {
    out <- character(0)
    for (k in 2:4) {
      if (length(w) >= k) {
        out <- c(out, vapply(seq_len(length(w) - k + 1), function(i)
          paste(w[i:(i + k - 1)], collapse = " "), character(1)))
      }
    }
    unique(out)
  })

  todos <- unlist(ngrams_item)
  cand <- if (length(todos) > 0) {
    tab <- table(todos)
    names(tab)[tab >= min_items]
  } else character(0)

  # Excluir n-gramas hechos SOLO de stopwords
  if (length(cand) > 0) {
    solo_stop <- vapply(cand, function(f) {
      all(strsplit(f, " ")[[1]] %in% stop_es)
    }, logical(1))
    cand <- cand[!solo_stop]
  }

  # Conservar frases MAXIMALES (descartar sub-frases contenidas en otra
  # candidata con el mismo alcance o mayor)
  cand <- cand[order(-nchar(cand))]
  maximales <- character(0)
  for (f in cand) {
    if (!any(vapply(maximales, function(m)
      grepl(f, m, fixed = TRUE), logical(1)))) {
      maximales <- c(maximales, f)
    }
  }

  filas <- lapply(maximales, function(f) {
    idx <- which(vapply(ngrams_item, function(g) f %in% g, logical(1)))
    if (length(idx) < min_items) return(NULL)
    data.frame(frase = f, n_items = length(idx),
               idx_csv = paste(idx, collapse = ","),
               stringsAsFactors = FALSE)
  })
  filas <- Filter(Negate(is.null), filas)

  # Muletilla de UNA palabra: el mismo verbo/palabra INICIAL (no stopword)
  # repetido en 3+ items ("Elijo...", "Uso..."). Los n-gramas no la ven y
  # es el residuo tipico tras podar las frases (validado con PM v2).
  primeras <- vapply(toks, function(w) if (length(w)) w[1] else "",
                     character(1))
  tab1 <- table(primeras[nzchar(primeras) & !(primeras %in% stop_es)])
  for (p in names(tab1)[tab1 >= min_items]) {
    idx <- which(primeras == p)
    filas[[length(filas) + 1]] <- data.frame(
      frase = paste0("empezar con \"", p, "\""), n_items = length(idx),
      idx_csv = paste(idx, collapse = ","), stringsAsFactors = FALSE)
  }

  if (length(filas) == 0) return(vacio)
  out <- do.call(rbind, filas)
  out[order(-out$n_items, -nchar(out$frase)), , drop = FALSE]
}


# Convierte los hallazgos de redaccion de la compuerta en un plan de
# reemplazo: data.frame(idx, motivo, conducta_prohibida). Fuentes:
# (1) clusters de faceta (conserva los mas prototipicos), (2) muletillas
# literales (conserva 1 por formula), (3) pares redundantes sueltos
# (reemplaza el miembro mas redundante con el resto de la escala).

#' @keywords internal
.plan_reemplazos_compuerta <- function(x, conservar_por_cluster = 2L,
                                       max_prop = 0.5) {
  vacio <- data.frame(idx = integer(0), motivo = character(0),
                      conducta_prohibida = character(0),
                      stringsAsFactors = FALSE)
  red <- x$compuerta$redaccion
  if (is.null(red)) return(vacio)
  S <- x$similitud
  n <- nrow(x$items)
  marcados <- integer(0)
  motivos  <- character(0)
  prohibidas <- character(0)

  fac <- red$facetas_repetidas
  if (!is.null(fac) && nrow(fac) > 0) {
    for (i in seq_len(nrow(fac))) {
      cods <- trimws(strsplit(fac$codigos[i], ",")[[1]])
      idx <- if (!is.null(x$items$codigo))
               match(cods, as.character(x$items$codigo))
             else suppressWarnings(as.integer(gsub("\\D", "", cods)))
      idx <- idx[!is.na(idx) & idx >= 1 & idx <= n]
      if (length(idx) <= conservar_por_cluster) next

      # En dimensiones COGNITIVAS y AFECTIVAS de una actitud, las creencias/
      # emociones sobre el mismo objeto cohesionan de forma legitima (piloto
      # ACO: los cognitivos cohesivos funcionaron, omega=.72). Solo se podan
      # si comparten PLANTILLA fuerte (sim media >= .70), no por tema comun.
      dims_cluster <- unique(x$items$dimension[idx])
      if (length(dims_cluster) == 1) {
        def_d <- if (is.list(x$concepto$dimensiones))
          x$concepto$dimensiones[[dims_cluster]] %||% "" else ""
        tipo <- .tipo_dimension(dims_cluster, def_d)
        if (tipo %in% c("cognitiva", "afectiva") && fac$sim_media[i] < 0.70) next
      }
      Sg <- S[idx, idx, drop = FALSE]; diag(Sg) <- NA
      protot <- rowMeans(Sg, na.rm = TRUE)
      conservar <- idx[order(-protot)][seq_len(conservar_por_cluster)]
      fuera <- setdiff(idx, conservar)
      marcados <- c(marcados, fuera)
      motivos  <- c(motivos, rep(paste0("faceta repetida (",
                                        fac$nucleo_lexico[i], ")"),
                                 length(fuera)))
      prohibidas <- c(prohibidas, rep(fac$nucleo_lexico[i], length(fuera)))
    }
  }

  # Muletillas literales (formula superficial compartida por 3+ items)
  mul <- .detectar_muletillas(x$items$item, min_items = 3L)
  if (nrow(mul) > 0) {
    for (i in seq_len(nrow(mul))) {
      idx <- suppressWarnings(as.integer(strsplit(mul$idx_csv[i], ",")[[1]]))
      idx <- setdiff(idx[!is.na(idx)], marcados)
      if (length(idx) < 2) next  # tras otras podas queda 1: ya no es muletilla
      # Conservar el primero (mantiene la conducta que representa) y marcar
      # el resto para reescritura sin la formula.
      fuera <- idx[-1]
      marcados <- c(marcados, fuera)
      motivos  <- c(motivos, rep(paste0("muletilla (\"", mul$frase[i], "\")"),
                                 length(fuera)))
      prohibidas <- c(prohibidas, rep(mul$frase[i], length(fuera)))
    }
  }

  pares <- red$pares_redundantes
  if (!is.null(pares) && nrow(pares) > 0) {
    Sd <- S; diag(Sd) <- NA
    sim_global <- rowMeans(Sd, na.rm = TRUE)
    for (i in seq_len(nrow(pares))) {
      a <- pares$item1[i]; b <- pares$item2[i]
      if (a %in% marcados || b %in% marcados) next  # ya cubierto
      peor <- if (sim_global[a] >= sim_global[b]) a else b
      marcados <- c(marcados, peor)
      motivos  <- c(motivos, sprintf("par redundante (sim %.2f)",
                                     pares$similitud[i]))
      prohibidas <- c(prohibidas, "")
    }
  }

  if (length(marcados) == 0) return(vacio)
  # Deduplicar conservando el primer motivo y respetar el tope por iteracion
  dup <- duplicated(marcados)
  plan <- data.frame(idx = marcados[!dup], motivo = motivos[!dup],
                     conducta_prohibida = prohibidas[!dup],
                     stringsAsFactors = FALSE)
  tope <- max(1L, floor(n * max_prop))
  if (nrow(plan) > tope) plan <- plan[seq_len(tope), , drop = FALSE]
  plan
}


# NIVEL DE EXPOSICION para un item de reemplazo. Mantener al item nuevo en el
# mismo "cuanto cuesta admitirlo" que los demas de su dimension: mezclar items
# comprometedores con items inocuos DENTRO de una dimension abre varianza de
# metodo y la parte en dos (alerta_intra de la compuerta). Lo usan tanto el
# reemplazo dirigido como el refinamiento, que antes generaba a ciegas.

# Nivel de exposicion de referencia para el item idx: la MEDIANA de su dimension
# SIN contarlo a el. Si justamente ese item era el desviado, copiarle su propio
# valor perpetuaria la desviacion que se quiere corregir.

#' @keywords internal
.desea_dimension <- function(desea, items, idx) {
  if (is.null(desea) || length(desea) != nrow(items)) return(NA_real_)
  otros <- setdiff(which(items$dimension == items$dimension[idx]), idx)
  if (length(otros) == 0) return(NA_real_)
  stats::median(as.numeric(desea[otros]), na.rm = TRUE)
}


#' @keywords internal
.bloque_exposicion <- function(deseabilidad_objetivo, otras = NULL) {
  if (length(deseabilidad_objetivo) != 1 || is.na(deseabilidad_objetivo))
    return("")
  # Medido el 2026-08-06 sobre los 6 constructos del curso: pedir solo "parecete
  # a tu dimension" NO conserva el contraste ENTRE dimensiones, que es lo que la
  # compuerta mide como sd_entre_dim. En las dos corridas donde el refinamiento
  # actuo, ese contraste BAJO (0.113 -> 0.098 y 0.026 -> 0.020) pese a que la
  # herencia estaba activa: un item que cae hacia el centro cumple la
  # instruccion y aun asi acerca las dimensiones entre si. Por eso se le dan
  # tambien los niveles de las OTRAS y se le pide mantener la distancia.
  otras <- otras[!is.na(otras)]
  bloque_otras <- if (length(otras) > 0) paste0(
    "Las demas dimensiones de esta escala estan en: ",
    paste(sprintf("%s = %.2f", names(otras), otras), collapse = "; "), ". ",
    "MANTEN LA DISTANCIA con ellas: no acerques el item nuevo a esos niveles. ",
    "Que cada dimension cueste distinto de admitir es lo que impide que se ",
    "peguen entre si; si todas acaban costando lo mismo, ese peso comun actua ",
    "como un factor extra y las funde.\n") else ""
  paste0(
    "NIVEL DE EXPOSICION OBLIGATORIO: los items de esta dimension tienen una ",
    "deseabilidad social de ", sprintf("%.2f", deseabilidad_objetivo),
    " en escala 0-1 (0 = admitirlo deja mal, 1 = admitirlo queda bien). ",
    "El nuevo item debe costar admitirlo APROXIMADAMENTE LO MISMO que los ",
    "demas items de su dimension: ",
    if (deseabilidad_objetivo < 0.40)
      "algo que a la persona le incomoda reconocer, pero sin convertirlo en una confesion grave.\n"
    else if (deseabilidad_objetivo > 0.60)
      "algo que no compromete a quien lo admite, en la misma linea que el resto.\n"
    else "algo de exposicion intermedia, ni confesion ni tramite.\n",
    bloque_otras)
}

# Medianas de deseabilidad de las dimensiones DISTINTAS a la del item idx.
#' @keywords internal
.desea_otras_dim <- function(desea, items, idx) {
  if (is.null(desea) || length(desea) != nrow(items)) return(NULL)
  d <- items$dimension[idx]
  otras <- items$dimension != d
  if (!any(otras)) return(NULL)
  m <- tapply(as.numeric(desea)[otras], items$dimension[otras], stats::median,
              na.rm = TRUE)
  m[!is.na(m)]
}


# Genera el reemplazo de UN item bajo las restricciones de la compuerta:
# conductas/muletillas vetadas (acumuladas), variacion sintactica, longitud
# homogenea con la escala y, si hay riesgo de halo, conducta especifica con
# costo personal. Reintenta hasta 3 veces verificando redundancia contra
# TODA la escala. Devuelve el texto nuevo o NULL.

#' @keywords internal
.reemplazar_item_dirigido <- function(x, openai, modelo, idx_item, motivo,
                                      conductas_prohibidas = character(0),
                                      anti_halo = FALSE,
                                      poblacion = NULL,
                                      rango_palabras = c(6L, 16L),
                                      umbral_redundancia = 0.70,
                                      embeddings_existentes = NULL,
                                      items_dimension_conservados = character(0),
                                      deseabilidad_objetivo = NA_real_,
                                      deseabilidad_otras = NULL) {
  dim_nombre <- x$items$dimension[idx_item]
  def_dim <- if (is.list(x$concepto$dimensiones))
               x$concepto$dimensiones[[dim_nombre]] %||% dim_nombre
             else dim_nombre
  caract <- if (!is.null(x$concepto$caracteristicas))
              x$concepto$caracteristicas[[dim_nombre]] else NULL
  concepto_str <- .extraer_concepto_str(x)
  idioma <- x$metadata$idioma %||% "es"

  conductas_prohibidas <- conductas_prohibidas[nzchar(conductas_prohibidas)]
  extra <- paste0(
    "CONTEXTO DE CORRECCION: el item que reemplazas fue eliminado por '",
    motivo, "'.\n",
    if (length(conductas_prohibidas) > 0) paste0(
      "PROHIBIDO ABSOLUTO: cualquier item sobre estas conductas ya ",
      "cubiertas o estas formulas vetadas (incluidos sinonimos y ",
      "reformulaciones): ",
      paste0("\"", conductas_prohibidas, "\"", collapse = ", "), ". ",
      "El nuevo item debe tratar una manifestacion DISTINTA de la ",
      "dimension y NO usar esas formulas.\n") else "",
    "VARIACION SINTACTICA OBLIGATORIA: no empieces ni termines el item con ",
    "la misma formula que otros items de la escala (p. ej. todos iniciando ",
    "con 'Dedico tiempo a...' o cerrando con 'aunque me incomode'). Cambia ",
    "el verbo inicial, el contexto y la estructura de la oracion.\n",
    "LONGITUD OBLIGATORIA: el item debe tener entre ", rango_palabras[1],
    " y ", rango_palabras[2], " palabras, similar al resto de la escala. ",
    "Gasta las palabras en el contenido, no en adornos.\n",
    # FACETAS YA CUBIERTAS: sin esta lista los reemplazos de una dimension
    # derivan todos hacia la misma manifestacion (caso 2026-08-03: 9 de 15
    # reemplazos acabaron siendo variantes de "dejar preguntas en blanco").
    if (length(items_dimension_conservados) > 0) paste0(
      "MANIFESTACIONES YA CUBIERTAS en esta dimension (el nuevo item debe ",
      "medir una DISTINTA de todas ellas, no una variante):\n",
      paste0("  - ", items_dimension_conservados, collapse = "\n"), "\n",
      "Elige otra via de expresion de la misma dimension: si las cubiertas ",
      "son conductas, considera una reaccion fisica o un pensamiento; si son ",
      "reacciones fisicas, considera una conducta o una anticipacion.\n") else "",
    # POLARIDAD DE DESEABILIDAD: mantener al nuevo item en el mismo nivel de
    # "cuanto cuesta admitirlo" que el que sustituye. Mezclar items
    # comprometedores con items inocuos DENTRO de una dimension abre varianza
    # de metodo y la parte en dos (alerta_intra de la compuerta).
    .bloque_exposicion(deseabilidad_objetivo, deseabilidad_otras),
    if (anti_halo) {
      tipo <- .tipo_dimension(dim_nombre, def_dim)
      switch(tipo,
        "cognitiva" = paste0(
          "RESTRICCION DE DESEABILIDAD (dimension COGNITIVA): redacta una ",
          "CREENCIA o JUICIO especifico sobre una consecuencia DISTINTA a ",
          "las ya cubiertas, con matiz discutible (que una persona ",
          "razonable pueda no compartir del todo). PROHIBIDO convertir el ",
          "item en una conducta o accion: debe seguir siendo una creencia ",
          "('creo que...', 'pienso que...', 'X hace que...').\n"),
        "afectiva" = paste0(
          "RESTRICCION DE DESEABILIDAD (dimension AFECTIVA): redacta una ",
          "EMOCION especifica y DIFERENCIADA de las ya cubiertas, ante una ",
          "situacion concreta distinta. PROHIBIDO convertir el item en una ",
          "conducta o accion: debe seguir siendo una reaccion emocional.\n"),
        paste0(
          "RESTRICCION DE DESEABILIDAD: redacta una CONDUCTA ESPECIFICA y ",
          "observable que implique un costo personal real (tiempo, esfuerzo, ",
          "incomodidad, renuncia), en una situacion concreta. EVITA ",
          "autoatribuciones globales halagadoras (del tipo 'soy honesto', ",
          "'valoro la justicia'): debe ser creible que una persona promedio ",
          "responda que NO la realiza sin quedar mal.\n"))
    } else ""
  )

  items_evitar <- unique(x$items$item)
  intentos <- 0L
  while (intentos < 3L) {
    intentos <- intentos + 1L
    df <- tryCatch(
      .generar_items_dimension(
        openai = openai,
        concepto = concepto_str,
        dimension = dim_nombre,
        definicion_dim = def_dim,
        caracteristicas = caract,
        n_items = 1,
        idioma = idioma,
        poblacion = poblacion,
        modelo = modelo,
        items_evitar = items_evitar,
        complejidad_linguistica = x$metadata$complejidad_linguistica %||% "intermedio",
        tipo_escala_respuesta = x$metadata$tipo_escala_respuesta %||% "frecuencia",
        evitar_cuantificadores = TRUE,
        max_palabras = rango_palabras[2],
        incluir_inversos = FALSE,
        instruccion_extra = extra
      ),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) next
    candidato <- trimws(df$item[1])
    if (!nzchar(candidato) ||
        tolower(candidato) %in% tolower(items_evitar)) {
      items_evitar <- unique(c(items_evitar, candidato))
      next
    }
    # Rechazar si reintroduce una formula vetada. Las vetadas de tipo
    # 'empezar con "palabra"' se chequean contra el INICIO del candidato.
    cand_norm <- chartr("áéíóúüñ",
                        "aeiouun", tolower(candidato))
    reintroduce <- FALSE
    for (f in conductas_prohibidas) {
      m <- regmatches(f, regexec("^empezar con \"(.+)\"$", f))[[1]]
      if (length(m) == 2) {
        if (grepl(paste0("^", m[2], "\\b"), cand_norm)) {
          reintroduce <- TRUE; break
        }
      } else if (grepl(f, cand_norm, fixed = TRUE)) {
        reintroduce <- TRUE; break
      }
    }
    if (reintroduce) {
      items_evitar <- unique(c(items_evitar, candidato))
      next
    }
    # Aceptacion con el MISMO umbral con el que la compuerta va a detectar
    # (adaptativo, no el 0.70 fijo de antes) y contra los embeddings VIVOS de
    # la escala, que ya incluyen los reemplazos de esta misma tanda. Es lo que
    # impide que el bucle persiga su propia cola.
    otros <- x$items$item[-idx_item]
    emb_otros <- if (!is.null(embeddings_existentes) &&
                     nrow(embeddings_existentes) == nrow(x$items))
                   embeddings_existentes[-idx_item, , drop = FALSE] else NULL
    check <- .verificar_redundancia_item(
      openai = openai, nuevo_item = candidato,
      items_existentes = otros,
      embeddings_existentes = emb_otros,
      umbral = umbral_redundancia)
    if (!check$redundante) {
      return(list(item = candidato, emb = check$embedding_nuevo))
    }
    items_evitar <- unique(c(items_evitar, candidato, check$items_similares))
  }
  NULL
}
