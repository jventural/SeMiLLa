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
#'         n_reemplazos) y \code{reemplazos} (data.frame con item viejo,
#'         item nuevo, dimension y motivo).
#' }
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
    stringsAsFactors = FALSE
  )
  reemplazos <- data.frame()

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

    n_ok <- 0L
    for (r in seq_len(nrow(plan))) {
      nuevo <- .reemplazar_item_dirigido(
        x = x, openai = openai, modelo = modelo,
        idx_item = plan$idx[r],
        motivo = plan$motivo[r],
        conductas_prohibidas = prohibidas_acum,
        anti_halo = anti_halo,
        poblacion = poblacion,
        rango_palabras = rango_palabras
      )
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

    historial <- rbind(historial, data.frame(
      iteracion = it, veredicto = x$compuerta$veredicto,
      n_reemplazos = n_ok, stringsAsFactors = FALSE))
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
      n_reemplazos = 0L, stringsAsFactors = FALSE))
  }

  x$optimizacion <- list(
    iteraciones = max(historial$iteracion),
    veredicto_objetivo = veredicto_objetivo,
    objetivo_alcanzado = .veredicto_cumple(x$compuerta$veredicto,
                                           veredicto_objetivo),
    historial = historial,
    reemplazos = reemplazos
  )

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat("  Optimizacion terminada: ", nrow(reemplazos),
        " item(s) reemplazado(s) en ", max(historial$iteracion),
        " iteracion(es).\n", sep = "")
    cat("  Veredicto final: ", x$compuerta$veredicto,
        if (x$optimizacion$objetivo_alcanzado) paste0(" (objetivo '",
          veredicto_objetivo, "' alcanzado)") else
          paste0(" (objetivo '", veredicto_objetivo,
                 "' NO alcanzado: revisar x$compuerta$acciones)"),
        "\n", sep = "")
    cat(.linea("-"), "\n")
  }

  x
}


# =============================================================================
# Helpers internos
# =============================================================================

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
                                      rango_palabras = c(6L, 16L)) {
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
    "Gasta las palabras en la situacion y la conducta, no en adornos.\n",
    if (anti_halo) paste0(
      "RESTRICCION DE DESEABILIDAD: redacta una CONDUCTA ESPECIFICA y ",
      "observable que implique un costo personal real (tiempo, esfuerzo, ",
      "incomodidad, renuncia), en una situacion concreta. EVITA ",
      "autoatribuciones globales halagadoras (del tipo 'soy honesto', ",
      "'valoro la justicia'): debe ser creible que una persona promedio ",
      "responda que NO la realiza sin quedar mal.\n") else ""
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
    otros <- x$items$item[-idx_item]
    check <- .verificar_redundancia_item(
      openai = openai, nuevo_item = candidato,
      items_existentes = otros, umbral = 0.70)
    if (!check$redundante) return(candidato)
    items_evitar <- unique(c(items_evitar, candidato, check$items_similares))
  }
  NULL
}
