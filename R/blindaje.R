# =============================================================================
# BLINDAJE DE GENERACION: jueces LLM de contexto y de parafrasis-gemelas
# =============================================================================
# Motivacion (bateria policial, n=280): los embeddings SUBDETECTAN parafrasis.
# Pares que en datos reales correlacionaron r policorica >= .70 (dependencia
# local, Phi interfactorial inflado hasta .92) vivian en similitud coseno
# 0.43-0.78, por debajo de cualquier umbral util. La deteccion fiable de
# gemelos es SEMANTICA (juicio), no geometrica (coseno). Estos jueces se
# ejecutan DENTRO de la generacion (generar_escala) para que la escala salga
# limpia desde el origen, y tambien estan disponibles para la compuerta y
# para escalas ya construidas (blindar_escala).
# =============================================================================


# Marca un resultado vacio de un juez LLM con el motivo del fallo, para que
# quien lo consuma pueda distinguir "no encontro nada" de "no pudo correr".
#' @keywords internal
.marcar_fallo_llm <- function(df, motivo) {
  attr(df, "fallo_llm") <- as.character(motivo)[1]
  df
}

# Juez LLM de parafrasis-gemelas. Devuelve data.frame(item1, item2, razon)
# con indices de fila de items_df (pares; los grupos de k items se expanden
# a pares consecutivos con el primero como referencia). Si la consulta al LLM
# falla, devuelve el data.frame vacio con el atributo "fallo_llm".

#' @keywords internal
.juzgar_parafrasis_llm <- function(items_df, openai, modelo = "gpt-4.1-mini",
                                   idioma = "es") {
  vacio <- data.frame(item1 = integer(0), item2 = integer(0),
                      razon = character(0), stringsAsFactors = FALSE)
  n <- nrow(items_df)
  if (n < 2) return(vacio)
  dims <- if (!is.null(items_df$dimension)) as.character(items_df$dimension)
          else rep("(unica)", n)
  lista <- paste0(seq_len(n), ". [", dims, "] ", items_df$item, collapse = "\n")

  prompt <- paste0(
    "Eres experto en construccion de escalas psicometricas. Abajo va la lista ",
    "numerada de items de una escala (entre corchetes, su dimension).\n\n",
    "TAREA: detectar PARAFRASIS-GEMELAS: pares o grupos de items que miden ",
    "EXACTAMENTE la misma conducta, creencia o emocion especifica con otras ",
    "palabras (mismo actor + misma accion/emocion + mismo tipo de situacion). ",
    "En datos reales estos pares producen dependencia local, fiabilidad ",
    "inflada y correlaciones entre factores artificialmente altas.\n\n",
    "SI son gemelos (aunque cambien las palabras):\n",
    "- misma conducta con escenario superficialmente distinto (p. ej. ",
    "'devuelvo dinero que encuentro' vs 'devuelvo objetos olvidados');\n",
    "- misma emocion ante el mismo tipo de evento (p. ej. 'me indigna que un ",
    "instructor cobre' vs 'siento enojo si un instructor acepta dinero');\n",
    "- misma conducta por canales equivalentes (p. ej. 'denuncio por escrito' ",
    "vs 'presento una denuncia firmada' vs 'doy parte por escrito');\n",
    "- misma conducta o emocion donde solo cambia un VERBO sinonimo, el objeto, ",
    "el recurso o el beneficiario (p. ej. 'veo a un superior manipular/alterar/",
    "cambiar notas y siento indignacion' son UN solo item repetido; 'reparto mi ",
    "bebida/mi equipo/mis horas entre companeros necesitados' tambien);\n",
    "- misma consecuencia valorada dos veces (p. ej. 'la corrupcion reduce la ",
    "confianza en X' vs 'la corrupcion hace perder la confianza en Y').\n\n",
    "NO son gemelos:\n",
    "- items de una misma dimension que comparten el TEMA del constructo pero ",
    "describen conductas, situaciones o emociones DISTINTAS (eso es legitimo ",
    "y necesario);\n",
    "- items que solo comparten palabras sueltas o el objeto de actitud.\n\n",
    "Se ESTRICTO pero PRECISO: incluye en la lista SOLO pares que claramente ",
    "son gemelos y que un revisor experto pediria reescribir; si un par te ",
    "parece dudoso o solo 'relacionado', NO lo incluyas en el JSON (no pongas ",
    "pares con razones tipo 'no son gemelos' o 'solo para contraste').\n\n",
    "ITEMS:\n", lista, "\n\n",
    "Responde SOLO con JSON valido, sin texto adicional:\n",
    "{\"gemelos\": [{\"items\": [3, 7], \"razon\": \"...\"}]}\n",
    "Si no hay gemelos: {\"gemelos\": []}"
  )

  # Un fallo de la llamada devuelve el data.frame vacio MARCADO con el motivo
  # (attr "fallo_llm"): sin esa marca, "la API fallo" era indistinguible de
  # "el juez corrio y no hallo gemelos" y el canal se apagaba en silencio.
  err <- NULL
  raw <- tryCatch(
    .llamar_openai(openai,
                   messages = list(list(role = "user", content = prompt)),
                   modelo = modelo, max_tokens = 2000L, temperature = 0),
    error = function(e) { err <<- conditionMessage(e); NULL })
  if (is.null(raw))
    return(.marcar_fallo_llm(vacio, err %||% "la llamada al LLM no devolvio texto"))
  parsed <- tryCatch(
    jsonlite::fromJSON(.extraer_bloque_json(raw), simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(parsed))
    return(.marcar_fallo_llm(vacio, "la respuesta del LLM no es JSON interpretable"))
  if (is.null(parsed$gemelos) || !length(parsed$gemelos)) return(vacio)

  filas <- list()
  for (g in parsed$gemelos) {
    idx <- suppressWarnings(as.integer(unlist(g$items)))
    idx <- unique(idx[!is.na(idx) & idx >= 1 & idx <= n])
    if (length(idx) < 2) next
    idx <- sort(idx)
    for (k in 2:length(idx)) {
      filas[[length(filas) + 1]] <- data.frame(
        item1 = idx[1], item2 = idx[k],
        razon = as.character(g$razon %||% "parafrasis"),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(filas)) return(vacio)
  out <- unique(do.call(rbind, filas))
  out[out$item1 != out$item2, , drop = FALSE]
}


# Juez LLM de fidelidad de contexto poblacional. Devuelve
# data.frame(numero, problema) con los items que rompen el contexto.

#' @keywords internal
.juzgar_contexto_llm <- function(items_df, poblacion, openai,
                                 modelo = "gpt-4.1-mini", idioma = "es") {
  vacio <- data.frame(numero = integer(0), problema = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(poblacion) || !nzchar(poblacion)) return(vacio)
  n <- nrow(items_df)
  lista <- paste0(seq_len(n), ". ", items_df$item, collapse = "\n")

  prompt <- paste0(
    "POBLACION OBJETIVO: ", poblacion, ".\n\n",
    "Revisa la lista numerada de items de un cuestionario dirigido a esa ",
    "poblacion y devuelve los que ROMPEN su contexto real:\n",
    "- vocabulario de OTRA institucion o etapa (p. ej. terminos de colegio de ",
    "menores como 'escolar', 'colegio', 'aula', 'maestro', 'acoso escolar' ",
    "cuando la poblacion es adulta o de otra institucion);\n",
    "- roles o vinculos que no corresponden al marco (p. ej. 'mi pareja', ",
    "'mi jefe', 'mis hijos', vida laboral, si la poblacion esta en formacion);\n",
    "- actividades o responsabilidades que la poblacion AUN no realiza por su ",
    "etapa (p. ej. tareas del servicio profesional activo para quien todavia ",
    "es estudiante);\n",
    "- escenarios inverosimiles o ajenos a su entorno cotidiano;\n",
    "- vocabulario demasiado tecnico o culto para su nivel de lectura.\n\n",
    "NO marques items solo por ser genericos: marca solo conflictos reales ",
    "de contexto.\n\n",
    "ITEMS:\n", lista, "\n\n",
    "Responde SOLO con JSON valido, sin texto adicional:\n",
    "{\"fuera_contexto\": [{\"numero\": 5, \"problema\": \"...\"}]}\n",
    "Si todos calzan: {\"fuera_contexto\": []}"
  )

  raw <- tryCatch(
    .llamar_openai(openai,
                   messages = list(list(role = "user", content = prompt)),
                   modelo = modelo, max_tokens = 1500L, temperature = 0),
    error = function(e) NULL)
  if (is.null(raw)) return(vacio)
  parsed <- tryCatch(
    jsonlite::fromJSON(.extraer_bloque_json(raw), simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$fuera_contexto) ||
      !length(parsed$fuera_contexto)) return(vacio)

  filas <- lapply(parsed$fuera_contexto, function(f) {
    num <- suppressWarnings(as.integer(f$numero))
    if (is.na(num) || num < 1 || num > n) return(NULL)
    data.frame(numero = num, problema = as.character(f$problema %||% ""),
               stringsAsFactors = FALSE)
  })
  filas <- filas[!vapply(filas, is.null, logical(1))]
  if (!length(filas)) return(vacio)
  unique(do.call(rbind, filas))
}


# Juez rapido de UN candidato contra la escala: ¿es parafrasis-gemela de
# alguno de los items existentes? Se usa ANTES de aceptar un reemplazo
# (simetrico al juicio de contexto del candidato): sin este paso, un
# reemplazo podia nacer gemelo de otro item y quedar sin re-juicio si era
# la ultima ronda del blindaje.

#' @keywords internal
.es_gemelo_candidato <- function(txt, items_df, openai,
                                 modelo = "gpt-4.1-mini") {
  lista <- paste0(seq_len(nrow(items_df)), ". ", items_df$item, collapse = "\n")
  prompt <- paste0(
    "ITEM CANDIDATO: \"", txt, "\"\n\n",
    "ITEMS YA EXISTENTES EN LA ESCALA:\n", lista, "\n\n",
    "¿El candidato es una PARAFRASIS-GEMELA de alguno de los existentes ",
    "(mide exactamente la misma conducta, creencia o emocion especifica, ",
    "aunque cambien las palabras, el verbo sinonimo, el objeto, el recurso ",
    "o el beneficiario)? Compartir tema o dimension NO es ser gemelo.\n",
    "Responde SOLO JSON: {\"gemelo\": true, \"de\": N} o {\"gemelo\": false}")
  raw <- tryCatch(
    .llamar_openai(openai,
                   messages = list(list(role = "user", content = prompt)),
                   modelo = modelo, max_tokens = 400L, temperature = 0),
    error = function(e) NULL)
  if (is.null(raw)) return(FALSE)   # ante fallo del juez, no bloquear
  parsed <- tryCatch(
    jsonlite::fromJSON(.extraer_bloque_json(raw), simplifyVector = TRUE),
    error = function(e) NULL)
  isTRUE(parsed$gemelo)
}


# Extrae el primer bloque JSON de una respuesta LLM (tolera texto o fences).

#' @keywords internal
.extraer_bloque_json <- function(txt) {
  txt <- gsub("```json|```", "", txt)
  ini <- regexpr("\\{", txt)
  fin <- max(gregexpr("\\}", txt)[[1]])
  if (ini > 0 && fin > ini) substr(txt, ini, fin) else txt
}


# Nucleo del blindaje: rondas de (contexto + parafrasis) -> reescritura
# dirigida de los ofensores con .generar_items_dimension. Trabaja sobre el
# data.frame de items + la info del concepto; devuelve list(items, reporte).

#' @keywords internal
.blindar_items <- function(items_df, info_concepto, openai, modelo,
                           concepto_nombre = NULL,
                           poblacion = NULL, idioma = "es",
                           complejidad_linguistica = "intermedio",
                           max_palabras = 18L,
                           tipo_escala_respuesta = "frecuencia",
                           contexto_prohibido = NULL,
                           instrucciones_estilo = NULL,
                           modelo_jueces = "gpt-4.1-mini",
                           max_rondas = 3L, verbose = TRUE) {

  # Las instrucciones de ESTILO (como escribir) van SOLO a los prompts de
  # regeneracion, NUNCA a los jueces: si el juez de contexto recibe criterios
  # de estilo dentro de la poblacion, los trata como violaciones y sobre-marca
  # items sanos (observado: 12/16 items marcados "fuera de contexto" por no
  # calzar con instrucciones de redaccion).
  estilo_txt <- if (!is.null(instrucciones_estilo) && nzchar(instrucciones_estilo))
    paste0("ESTILO DE REDACCION (obligatorio): ", instrucciones_estilo, "\n")
  else ""

  # Terminos vetados por el usuario: se aplican por REGEX (deterministico),
  # sin depender del criterio del juez LLM, que en terminos ambiguos (p. ej.
  # "patrulla" para estudiantes de escuela policial) puede ser inconsistente.
  pat_veto <- if (!is.null(contexto_prohibido) && length(contexto_prohibido)) {
    paste0("(", paste(contexto_prohibido, collapse = "|"), ")")
  } else NULL
  veto_txt <- if (!is.null(pat_veto)) {
    paste0("PROHIBIDO ABSOLUTO usar o aludir a: ",
           paste(contexto_prohibido, collapse = ", "),
           " (terminos vetados por el usuario para esta poblacion).\n")
  } else ""

  reporte <- list()
  for (ronda in seq_len(max_rondas)) {
    # Gemelos LEXICOS por coseno (deterministico): si el llamador provee la
    # matriz de similitud, todo par >= 0.80 se marca como parafrasis aunque
    # el juez LLM no lo señale. Motivo: las reescrituras del optimizador y
    # del estres pueden crear gemelos casi literales (observado: coseno .91)
    # y el juez, al ver la escala completa, a veces los pasa por alto.
    par_cos <- NULL
    if (!is.null(attr(items_df, "similitud"))) {
      S_b <- attr(items_df, "similitud")
      if (is.matrix(S_b) && nrow(S_b) == nrow(items_df)) {
        idx <- which(S_b >= 0.80 & upper.tri(S_b), arr.ind = TRUE)
        if (nrow(idx)) par_cos <- data.frame(
          item1 = idx[, 1], item2 = idx[, 2],
          razon = sprintf("gemelo lexico (coseno %.2f)",
                          S_b[idx]), stringsAsFactors = FALSE)
      }
    }
    ctx <- .juzgar_contexto_llm(items_df, poblacion, openai, modelo_jueces, idioma)
    if (!is.null(pat_veto)) {
      idx_v <- grep(pat_veto, items_df$item, ignore.case = TRUE)
      idx_v <- setdiff(idx_v, ctx$numero)
      if (length(idx_v)) {
        ctx <- rbind(ctx, data.frame(
          numero = idx_v,
          problema = "contiene un termino vetado por el usuario",
          stringsAsFactors = FALSE))
      }
    }
    par <- .juzgar_parafrasis_llm(items_df, openai, modelo_jueces, idioma)
    if (!is.null(par_cos)) {
      clave_p <- if (nrow(par)) paste(pmin(par$item1, par$item2),
                                      pmax(par$item1, par$item2)) else character(0)
      nuevos_c <- par_cos[!(paste(par_cos$item1, par_cos$item2) %in% clave_p),
                          , drop = FALSE]
      if (nrow(nuevos_c)) par <- rbind(par, nuevos_c)
    }

    # De cada par gemelo se reescribe el segundo miembro (se conserva el
    # primero); un item puede aparecer en varios pares: se reescribe una vez.
    idx_par <- unique(par$item2)
    razon_par <- vapply(idx_par, function(i)
      paste(unique(par$razon[par$item2 == i]), collapse = "; "), character(1))
    idx_ctx <- setdiff(unique(ctx$numero), idx_par)
    razon_ctx <- vapply(idx_ctx, function(i)
      paste(unique(ctx$problema[ctx$numero == i]), collapse = "; "), character(1))

    ofensores <- c(idx_par, idx_ctx)
    motivos   <- c(paste0("parafrasis de otro item (", razon_par, ")"),
                   paste0("fuera de contexto (", razon_ctx, ")"))
    reporte[[ronda]] <- list(n_parafrasis = length(idx_par),
                             n_contexto = length(idx_ctx),
                             detalle_parafrasis = par,
                             detalle_contexto = ctx)
    if (!length(ofensores)) {
      if (verbose) cat("  ", .color_check(),
                       " Blindaje: items limpios (ronda ", ronda, ")\n", sep = "")
      break
    }
    if (verbose) {
      cat("  ", .color_warning(), " Blindaje ronda ", ronda, ": ",
          length(idx_par), " parafrasis + ", length(idx_ctx),
          " fuera de contexto -> reescribiendo\n", sep = "")
    }

    for (k in seq_along(ofensores)) {
      i <- ofensores[k]
      dim_i <- as.character(items_df$dimension[i])
      hermanos <- items_df$item[items_df$dimension == dim_i &
                                seq_len(nrow(items_df)) != i]
      extra <- paste0(
        "El item anterior para esta posicion fue RECHAZADO por: ", motivos[k],
        ".\nGenera UN item nuevo que corrija ese problema.\n", veto_txt, estilo_txt,
        "ANTI-PARAFRASIS (critico): el item debe aportar una conducta, ",
        "situacion o emocion CLARAMENTE DISTINTA de TODOS los items que ya ",
        "existen en su dimension (no cambies solo las palabras):\n",
        paste0("- ", hermanos, collapse = "\n"), "\n")

      # IMPORTANTE: con seed fijado la temperatura global es 0, asi que dos
      # intentos con el MISMO prompt devuelven el MISMO candidato. Cada
      # candidato rechazado se acumula en 'rechazados' y entra al prompt del
      # intento siguiente (via items_evitar + instruccion), forzando variacion
      # real; sin esto los reintentos eran identicos y el item sucio quedaba.
      nuevo <- NULL; rechazados <- character(0)
      for (intento in 1:5) {
        extra_i <- if (length(rechazados)) paste0(
          extra, "CANDIDATOS YA RECHAZADOS (propon algo DISTINTO a estos):\n",
          paste0("- ", rechazados, collapse = "\n"), "\n") else extra
        cand <- tryCatch(.generar_items_dimension(
          openai = openai,
          concepto = concepto_nombre %||% info_concepto$concepto %||% "",
          dimension = dim_i,
          definicion_dim = if (is.list(info_concepto$dimensiones))
            info_concepto$dimensiones[[dim_i]] %||% dim_i else dim_i,
          caracteristicas = if (!is.null(info_concepto$caracteristicas))
            info_concepto$caracteristicas[[dim_i]] else NULL,
          n_items = 1L, idioma = idioma, poblacion = poblacion,
          modelo = modelo, items_evitar = c(items_df$item, rechazados),
          complejidad_linguistica = complejidad_linguistica,
          tipo_escala_respuesta = tipo_escala_respuesta,
          max_palabras = max_palabras, incluir_inversos = FALSE,
          instruccion_extra = extra_i), error = function(e) NULL)
        if (is.null(cand) || !nrow(cand)) next
        txt <- trimws(cand$item[1])
        n_pal <- length(strsplit(txt, "\\s+")[[1]])
        if (!nzchar(txt) || n_pal > max_palabras ||
            tolower(txt) %in% tolower(items_df$item) ||
            tolower(txt) %in% tolower(rechazados)) {
          rechazados <- unique(c(rechazados, txt)); next
        }
        if (!is.null(pat_veto) && grepl(pat_veto, txt, ignore.case = TRUE)) {
          rechazados <- unique(c(rechazados, txt)); next
        }
        # El candidato se juzga ANTES de aceptarse: sin este paso el propio
        # regenerador reintroducia fugas de contexto (p. ej. "en patrulla"
        # para cadetes) que quedaban sin verificar al agotarse las rondas.
        ctx_cand <- .juzgar_contexto_llm(
          data.frame(item = txt, dimension = dim_i), poblacion,
          openai, modelo_jueces, idioma)
        if (nrow(ctx_cand) > 0) { rechazados <- unique(c(rechazados, txt)); next }
        if (.es_gemelo_candidato(txt, items_df[-i, , drop = FALSE],
                                 openai, modelo_jueces)) {
          rechazados <- unique(c(rechazados, txt)); next
        }
        nuevo <- txt; break
      }
      if (!is.null(nuevo)) {
        if (verbose) cat("     [", i, " ", substr(dim_i, 1, 14), "] ",
                         items_df$item[i], "\n       -> ", nuevo, "\n", sep = "")
        items_df$item[i] <- nuevo
      } else if (verbose) {
        cat("     [", i, "] sin reemplazo valido; se conserva: ",
            items_df$item[i], "\n", sep = "")
      }
    }
  }

  # Garantia dura de longitud: cualquier item que aun exceda max_palabras se
  # regenera (el acortador de .auditar_longitud a veces no logra comprimir).
  np <- vapply(strsplit(items_df$item, "\\s+"), length, integer(1))
  largos <- which(np > max_palabras)
  for (i in largos) {
    dim_i <- as.character(items_df$dimension[i])
    extra <- paste0(
      "El item anterior fue RECHAZADO por exceder ", max_palabras,
      " palabras. Genera UN item nuevo con la MISMA idea central en ",
      max_palabras, " palabras o menos (cuenta cada palabra). Item anterior: '",
      items_df$item[i], "'.\n", veto_txt, estilo_txt)
    rechazados <- character(0)
    for (intento in 1:5) {
      extra_i <- if (length(rechazados)) paste0(
        extra, "CANDIDATOS YA RECHAZADOS (propon algo DISTINTO):\n",
        paste0("- ", rechazados, collapse = "\n"), "\n") else extra
      cand <- tryCatch(.generar_items_dimension(
        openai = openai,
        concepto = concepto_nombre %||% info_concepto$concepto %||% "",
        dimension = dim_i,
        definicion_dim = if (is.list(info_concepto$dimensiones))
          info_concepto$dimensiones[[dim_i]] %||% dim_i else dim_i,
        caracteristicas = if (!is.null(info_concepto$caracteristicas))
          info_concepto$caracteristicas[[dim_i]] else NULL,
        n_items = 1L, idioma = idioma, poblacion = poblacion,
        modelo = modelo, items_evitar = c(items_df$item[-i], rechazados),
        complejidad_linguistica = complejidad_linguistica,
        tipo_escala_respuesta = tipo_escala_respuesta,
        max_palabras = max_palabras, incluir_inversos = FALSE,
        instruccion_extra = extra_i), error = function(e) NULL)
      if (is.null(cand) || !nrow(cand)) next
      txt <- trimws(cand$item[1])
      n_pal <- length(strsplit(txt, "\\s+")[[1]])
      if (!nzchar(txt) || n_pal > max_palabras ||
          tolower(txt) %in% tolower(items_df$item)) {
        rechazados <- unique(c(rechazados, txt)); next
      }
      if (!is.null(pat_veto) && grepl(pat_veto, txt, ignore.case = TRUE)) {
        rechazados <- unique(c(rechazados, txt)); next
      }
      ctx_cand <- .juzgar_contexto_llm(
        data.frame(item = txt, dimension = dim_i), poblacion,
        openai, modelo_jueces, idioma)
      if (nrow(ctx_cand) > 0) { rechazados <- unique(c(rechazados, txt)); next }
      if (.es_gemelo_candidato(txt, items_df[-i, , drop = FALSE],
                               openai, modelo_jueces)) {
        rechazados <- unique(c(rechazados, txt)); next
      }
      if (verbose) cat("     [", i, " longitud] ", items_df$item[i],
                       "\n       -> ", txt, "\n", sep = "")
      items_df$item[i] <- txt
      break
    }
  }

  list(items = items_df, reporte = reporte)
}


#' Blindar una escala ya construida (contexto + parafrasis por juez LLM)
#'
#' Ejecuta sobre una escala existente los mismos jueces LLM que
#' \code{generar_escala()} aplica durante la generacion: (1) fidelidad de
#' contexto poblacional y (2) deteccion de parafrasis-gemelas (pares de items
#' que miden exactamente la misma conducta/creencia/emocion con otras
#' palabras). Los items ofensores se reescriben de forma dirigida y, si el
#' objeto traia embeddings, se recalculan al final.
#'
#' Motivacion: los embeddings subdetectan parafrasis (pares con r policorica
#' >= .70 en datos reales viven en coseno 0.43-0.78), y esos gemelos producen
#' dependencia local y correlaciones interfactoriales infladas.
#'
#' @param x Objeto \code{semilla} (o lista con \code{$items}, \code{$concepto},
#'   \code{$metadata}).
#' @param api_key Clave API de OpenAI.
#' @param poblacion Poblacion objetivo; default: la del metadata del objeto.
#' @param modelo Modelo LLM para jueces y reescritura.
#' @param max_rondas Rondas maximas de juicio + reescritura.
#' @param verbose Mostrar progreso.
#' @return El objeto con \code{$items} blindados, \code{$blindaje} (reporte)
#'   y embeddings/similitud recalculados si existian.
#' @export
blindar_escala <- function(x, api_key = Sys.getenv("OPENAI_API_KEY"),
                           poblacion = NULL, modelo = "gpt-4.1-mini",
                           contexto_prohibido = NULL,
                           instrucciones_estilo = NULL,
                           modelo_jueces = "gpt-4.1-mini",
                           max_rondas = 3L, verbose = TRUE) {
  if (is.null(x$items) || is.null(x$items$item))
    stop("'x' debe contener $items con la columna 'item'.")
  openai <- .configurar_openai(api_key)
  poblacion <- poblacion %||% x$metadata$poblacion
  contexto_prohibido <- contexto_prohibido %||% x$metadata$contexto_prohibido
  instrucciones_estilo <- instrucciones_estilo %||% x$metadata$instrucciones_estilo
  compl <- x$metadata$complejidad_linguistica %||% "intermedio"
  maxp  <- x$metadata$max_palabras %||%
    switch(compl, minimo = 10L, basico = 12L, intermedio = 18L, avanzado = 25L)

  # Hasta 2 pases: el segundo re-verifica con embeddings FRESCOS que los
  # reemplazos no hayan creado gemelos lexicos nuevos (coseno >= 0.80).
  for (pase in 1:2) {
    items_in <- x$items
    if (!is.null(x$similitud)) attr(items_in, "similitud") <- as.matrix(x$similitud)
    res <- .blindar_items(
      items_df = items_in, info_concepto = x$concepto, openai = openai,
      modelo = modelo,
      concepto_nombre = x$metadata$concepto_original %||% NULL,
      poblacion = poblacion,
      idioma = x$metadata$idioma %||% "es",
      complejidad_linguistica = compl, max_palabras = maxp,
      tipo_escala_respuesta = x$metadata$tipo_escala_respuesta %||% "frecuencia",
      contexto_prohibido = contexto_prohibido,
      instrucciones_estilo = instrucciones_estilo,
      modelo_jueces = modelo_jueces,
      max_rondas = max_rondas, verbose = verbose)

    cambio <- !identical(res$items$item, x$items$item)
    attr(res$items, "similitud") <- NULL
    x$items <- res$items
    x$blindaje <- res$reporte
    if (cambio) {
      if (verbose) cat("  (recalculando embeddings tras el blindaje, pase ",
                       pase, ")\n", sep = "")
      emb <- obtener_embeddings(x$items, api_key = api_key, verbose = FALSE)
      x$embeddings <- emb$embeddings
      x$similitud  <- emb$similitud
    } else break
    # si tras recalcular no quedan gemelos lexicos, no hace falta otro pase
    S_chk <- as.matrix(x$similitud)
    if (!any(S_chk[upper.tri(S_chk)] >= 0.80)) break
  }
  x
}
