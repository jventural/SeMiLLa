#' @title Validez de Contenido via LLM (V de Aiken)
#'
#' @description
#' Evalua la validez de contenido de los items usando un LLM como panel de expertos.
#' Calcula la V de Aiken con intervalos de confianza para cada item y criterio.
#'
#' Basado en: Ventura-Leon (2022). De regreso a la validez basada en el contenido.
#' Adicciones, 34(4), 323-326.
#'
#' @param x Objeto semilla, semilla_items, o dataframe con columnas 'item' y 'dimension'
#' @param api_key API key de OpenAI
#' @param criterios Vector de criterios a evaluar. Default: relevancia,
#'   representatividad (criterios de validez de contenido). La claridad de
#'   redaccion se evalua por separado con \code{auditar_redaccion_items()}.
#' @param n_jueces Numero de "jueces expertos" LLM a simular (default: 10)
#' @param confianza Nivel de confianza para IC (default: 0.95)
#' @param modelo Modelo de OpenAI: "gpt-4.1-mini" (default), "gpt-4o", "gpt-4o-mini",
#'   "gpt-4-turbo", "gpt-3.5-turbo", o cualquier modelo compatible
#' @param verbose Mostrar progreso
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{v_aiken}: V de Aiken para cada item y criterio
#'   \item \code{v_aiken_escala}: V de Aiken promedio por criterio
#'   \item \code{evaluaciones}: Matriz completa de evaluaciones
#'   \item \code{recomendaciones}: Items que necesitan revision (V < 0.70 o IC_inf < 0.70)
#' }
#'
#' @details
#' El LLM actua como panel de jueces expertos evaluando cada item en escala 0-3:
#' \itemize{
#'   \item 0 = Muy en desacuerdo / No cumple
#'   \item 1 = En desacuerdo / Cumple poco
#'   \item 2 = De acuerdo / Cumple parcialmente
#'   \item 3 = Muy de acuerdo / Cumple totalmente
#' }
#'
#' Formula V de Aiken (Penfield & Giacobbi, 2004):
#' \deqn{V = \frac{\bar{X} - l}{k}}
#'
#' Donde:
#' \itemize{
#'   \item X_bar = promedio de calificaciones de los jueces
#'   \item l = puntuacion minima posible (0)
#'   \item k = rango (valor maximo - valor minimo = 3)
#' }
#'
#' Criterios de aceptacion (Napitupulu et al., 2018; Charter, 2003):
#' - V >= 0.70 es el minimo aceptable
#' - Limite inferior del IC >= 0.70
#'
#' @examples
#' \dontrun{
#' # Evaluar validez de contenido
#' cv <- validez_contenido(mi_escala, api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Ver resultados
#' print(cv)
#'
#' # Items que necesitan revision
#' cv$recomendaciones
#'
#' # V de Aiken por item
#' cv$v_aiken
#' }
#'
#' @references
#' Ventura-Leon, J. (2022). De regreso a la validez basada en el contenido.
#' Adicciones, 34(4), 323-326.
#'
#' Penfield, R. D., & Giacobbi, P. R. (2004). Applying a score confidence interval
#' to Aiken's item content-relevance index. Measurement in Physical Education
#' and Exercise Science, 8(4), 213-225.
#'
#' @export
validez_contenido <- function(x,
                               api_key,
                               criterios = c("relevancia", "representatividad"),
                               n_jueces = 10,
                               confianza = 0.95,
                               modelo = "gpt-4.1-mini",
                               verbose = TRUE) {

  # Extraer items y concepto
  if (inherits(x, "semilla")) {
    items_df <- x$items
    concepto <- x$concepto
    concepto_original <- x$metadata$concepto_original
  } else if (inherits(x, "semilla_items")) {
    items_df <- x$items
    concepto <- x$concepto
    concepto_original <- x$metadata$concepto_original
  } else if (is.data.frame(x)) {
    items_df <- x
    concepto <- NULL
    concepto_original <- "constructo evaluado"
  } else {
    stop("Objeto no valido. Usa un objeto semilla o dataframe.")
  }

  .validar_api_key(api_key)
  openai <- .configurar_openai(api_key)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("VALIDEZ DE CONTENIDO - V DE AIKEN"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Items a evaluar: ", nrow(items_df), "\n", sep = "")
    cat("  Jueces simulados: ", n_jueces, "\n", sep = "")
    cat("  Nivel de confianza: ", confianza * 100, "%\n", sep = "")
    cat("  Criterios: ", paste(criterios, collapse = ", "), "\n\n", sep = "")
  }

  # Construir contexto para el LLM
  contexto <- .construir_contexto_validez(concepto, concepto_original, criterios)

  # Detectar columna de items invertidos (si existe)
  col_invertido <- intersect(c("invertido", "inverso", "direccion_inversa"),
                              names(items_df))
  tiene_marca_inversion <- length(col_invertido) > 0

  # Evaluar cada item con multiples "jueces"
  todas_evaluaciones <- list()

  for (i in 1:nrow(items_df)) {
    item <- items_df$item[i]
    dimension <- items_df$dimension[i]
    caracteristica <- if ("caracteristica" %in% names(items_df)) {
      items_df$caracteristica[i]
    } else ""

    # Determinar si el item es inverso. Prioridad:
    # (1) columna explicita "invertido"/"inverso"/"direccion_inversa"
    # (2) heuristica: caracteristica contradice la direccion teorica de la
    #     dimension (palabras de negacion + caracteristica positiva en
    #     dimension negativa, o viceversa)
    es_inverso <- if (tiene_marca_inversion) {
      isTRUE(as.logical(items_df[[col_invertido[1]]][i]))
    } else {
      .detectar_item_inverso(item, dimension, caracteristica)
    }

    def_dimension <- if (!is.null(concepto$dimensiones[[dimension]])) {
      concepto$dimensiones[[dimension]]
    } else ""

    if (verbose) {
      marca <- if (es_inverso) " [inverso]" else ""
      cat("  [", i, "/", nrow(items_df), "] Evaluando", marca, ": ",
          substr(item, 1, 40), "...\n", sep = "")
    }

    # Simular n jueces expertos
    evaluaciones_item <- .evaluar_item_jueces(
      openai = openai,
      item = item,
      dimension = dimension,
      definicion_dimension = def_dimension,
      caracteristica = caracteristica,
      es_inverso = es_inverso,
      contexto = contexto,
      criterios = criterios,
      n_jueces = n_jueces,
      modelo = modelo
    )

    todas_evaluaciones[[i]] <- evaluaciones_item
    Sys.sleep(0.5)  # Rate limit
  }

  # Calcular V de Aiken para cada item
  if (verbose) cat("\n  ", .color_flecha(), " Calculando V de Aiken...\n", sep = "")

  resultados_v <- .calcular_v_aiken(todas_evaluaciones, items_df, criterios, n_jueces, confianza)

  # Identificar items problematicos (V < 0.70 o IC_inf < 0.70)
  umbral_v <- 0.70
  items_revision <- resultados_v$v_aiken[
    resultados_v$v_aiken$V_promedio < umbral_v |
    resultados_v$v_aiken$IC_inf < umbral_v,
  ]

  # Construir resultado
  resultado <- list(
    v_aiken = resultados_v$v_aiken,
    v_aiken_escala = resultados_v$v_aiken_escala,
    evaluaciones = resultados_v$evaluaciones,
    recomendaciones = items_revision,
    metadata = list(
      n_items = nrow(items_df),
      n_jueces = n_jueces,
      criterios = criterios,
      confianza = confianza,
      modelo = modelo,
      fecha = Sys.time()
    )
  )

  class(resultado) <- c("semilla_cv", "list")

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("RESULTADOS:"), "\n\n")

    cat("  V de Aiken por criterio:\n")
    for (crit in criterios) {
      v_crit <- resultados_v$v_aiken_escala[[crit]]
      status <- if (v_crit >= 0.70) .color_check() else .color_warning()
      cat("    - ", crit, ": ", sprintf("%.3f", v_crit), " ", status, "\n", sep = "")
    }

    cat("\n  V de Aiken promedio: ", sprintf("%.3f", resultados_v$v_aiken_escala$V_total), "\n", sep = "")
    cat("  Items con V < 0.70 o IC_inf < 0.70: ", nrow(items_revision), "\n", sep = "")

    if (resultados_v$v_aiken_escala$V_total >= 0.70) {
      cat("\n  ", .color_check(), " Validez de contenido ACEPTABLE\n", sep = "")
    } else {
      cat("\n  ", .color_warning(), " Validez de contenido REQUIERE REVISION\n", sep = "")
    }
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


#' @title Fiabilidad Semantica
#'
#' @description
#' Calcula la fiabilidad (consistencia interna) basada en similitud semantica
#' de los embeddings, sin necesidad de datos empiricos.
#'
#' Utiliza la formula de Spearman-Brown para predecir el Alpha de Cronbach
#' a partir de la similitud coseno promedio entre items.
#'
#' Basado en: Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal
#' and address taxonomic incommensurability in psychological measurement.
#' Nature Human Behaviour, 9(5), 944-954.
#' Los autores aplican Spearman-Brown sobre la similitud coseno entre
#' embeddings y reportan correlacion entre alpha observado y predicho
#' de r = .75 (in-sample) y r = .61 (out-of-sample) en 449 escalas.
#'
#' @param x Objeto semilla con embeddings, o matriz de similitud
#' @param metodo Metodo de calculo: "spearman_brown"
#' @param verbose Mostrar progreso
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{alpha_dimensiones}: Alpha semantico por dimension (principal)
#'   \item \code{alpha_promedio}: Promedio ponderado de alphas por dimension
#'   \item \code{similitud_intra}: Similitud promedio DENTRO de cada dimension
#'   \item \code{similitud_inter}: Similitud promedio ENTRE dimensiones
#' }
#'
#' @details
#' El Alpha Semantico se calcula POR DIMENSION usando la formula de Spearman-Brown:
#'
#' \deqn{\alpha_{sem} = \frac{n \cdot \bar{r}}{1 + (n-1) \cdot \bar{r}}}
#'
#' Donde:
#' \itemize{
#'   \item n = numero de items EN LA DIMENSION
#'   \item r_bar = similitud coseno promedio entre items de esa dimension
#' }
#'
#' IMPORTANTE: El alpha se calcula por dimension, NO como total de la escala.
#' Esto es porque:
#' \itemize{
#'   \item Alpha aumenta artificialmente con mas items (sesgo conocido)
#'   \item Items de diferentes dimensiones no deben correlacionar alto
#'   \item El alpha total mezcla varianza de constructos distintos
#' }
#'
#' Interpretacion (George & Mallery, 2003):
#' \itemize{
#'   \item >= 0.90: Excelente
#'   \item >= 0.80: Bueno
#'   \item >= 0.70: Aceptable
#'   \item >= 0.60: Cuestionable
#'   \item < 0.60: Pobre
#' }
#'
#' @examples
#' \dontrun{
#' # Calcular fiabilidad semantica
#' fiab <- fiabilidad_semantica(mi_escala)
#'
#' # Ver resultados
#' print(fiab)
#'
#' # Acceder a valores por dimension
#' fiab$alpha_dimensiones
#'
#' # Promedio ponderado
#' fiab$alpha_promedio
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement.
#' Nature Human Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' @export
fiabilidad_semantica <- function(x,
                                  metodo = "spearman_brown",
                                  verbose = TRUE) {

  # Extraer similitud y items
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$similitud)) {
      stop("El objeto no tiene matriz de similitud. Ejecuta obtener_embeddings() primero.")
    }
    similitud <- x$similitud
    items_df <- x$items
  } else if (is.list(x) && !is.null(x$similitud)) {
    # Aceptar listas con estructura correcta aunque no tengan clase formal
    similitud <- x$similitud
    items_df <- x$items
  } else if (is.matrix(x)) {
    similitud <- x
    items_df <- NULL
  } else {
    stop("Objeto no valido. Usa un objeto semilla, lista con $similitud, o matriz de similitud.")
  }

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("FIABILIDAD SEMANTICA - ALPHA POR DIMENSION"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Metodo: ", metodo, "\n", sep = "")
    cat("  Items totales: ", nrow(similitud), "\n", sep = "")
  }

  # Alerta de homogeneidad sintactica (patron tipo Escala de Celos): si los
  # items comparten plantilla, los indices de fiabilidad semantica pueden no
  # ser interpretables como consistencia.
  if (!is.null(items_df) && !is.null(items_df$item)) {
    .hs <- .homogeneidad_sintactica(items_df$item)
    if (isTRUE(.hs$alerta)) {
      .msg <- sprintf(paste0("Homogeneidad sintactica alta (prefijo compartido %.0f%%, ",
                             "solapamiento n-grama %.2f): los indices de fiabilidad semantica ",
                             "pueden no ser interpretables. Revise auditar_redundancia()."),
                      100 * .hs$prefijo_frac, .hs$ngram_media)
      if (verbose) cat("\n  ", .color_warning(), " ", .msg, "\n", sep = "")
      warning(.msg, call. = FALSE)
    }
  }

  # Si no hay informacion de dimensiones, calcular alpha unico con advertencia

  if (is.null(items_df)) {
    if (verbose) {
      cat("\n  ", .color_warning(), " Sin informacion de dimensiones.\n", sep = "")
      cat("  Calculando alpha unico (interpretar con precaucion).\n\n", sep = "")
    }

    alpha_unico <- .calcular_alpha_semantico(similitud)

    resultado <- list(
      alpha_dimensiones = data.frame(
        dimension = "total",
        n_items = nrow(similitud),
        similitud_promedio = alpha_unico$similitud_promedio,
        alpha_semantico = alpha_unico$alpha,
        stringsAsFactors = FALSE
      ),
      alpha_promedio = alpha_unico$alpha,
      similitud_intra = alpha_unico$similitud_promedio,
      similitud_inter = NA,
      advertencia = "Alpha calculado sin dimensiones - puede estar inflado por n items",
      metadata = list(
        metodo = metodo,
        n_items = nrow(similitud),
        n_dimensiones = 1,
        fecha = Sys.time()
      )
    )

    class(resultado) <- c("semilla_fiabilidad", "list")
    return(resultado)
  }

  # Calcular alpha POR DIMENSION
  if (verbose) {
    dimensiones <- unique(items_df$dimension)
    cat("  Dimensiones: ", length(dimensiones), "\n\n", sep = "")
    cat("  ", .color_flecha(), " Calculando alpha por dimension...\n", sep = "")
  }

  dimensiones <- unique(items_df$dimension)
  alpha_dimensiones <- data.frame(
    dimension = character(),
    n_items = integer(),
    similitud_promedio = numeric(),
    alpha_semantico = numeric(),
    stringsAsFactors = FALSE
  )

  # Calcular similitud INTRA-dimension (dentro de cada dimension)
  similitudes_intra <- c()

  for (dim in dimensiones) {
    idx <- which(items_df$dimension == dim)

    if (length(idx) >= 2) {
      sim_dim <- similitud[idx, idx]
      alpha_dim <- .calcular_alpha_semantico(sim_dim)

      alpha_dimensiones <- rbind(alpha_dimensiones, data.frame(
        dimension = dim,
        n_items = length(idx),
        similitud_promedio = alpha_dim$similitud_promedio,
        alpha_semantico = alpha_dim$alpha,
        stringsAsFactors = FALSE
      ))

      similitudes_intra <- c(similitudes_intra, alpha_dim$similitud_promedio)

      if (verbose) {
        interp <- .interpretar_alpha(alpha_dim$alpha)
        cat("    [", dim, "] n=", length(idx),
            ", r_intra=", sprintf("%.3f", alpha_dim$similitud_promedio),
            ", alpha=", sprintf("%.3f", alpha_dim$alpha),
            " (", interp, ")\n", sep = "")
      }
    } else {
      if (verbose) {
        cat("    [", dim, "] n=", length(idx), " - Insuficientes items\n", sep = "")
      }
    }
  }

  # Calcular similitud INTER-dimension (entre dimensiones)
  similitud_inter <- NA
  if (length(dimensiones) > 1) {
    pares_inter <- c()
    for (i in 1:(length(dimensiones)-1)) {
      for (j in (i+1):length(dimensiones)) {
        idx_i <- which(items_df$dimension == dimensiones[i])
        idx_j <- which(items_df$dimension == dimensiones[j])
        pares_inter <- c(pares_inter, as.vector(similitud[idx_i, idx_j]))
      }
    }
    similitud_inter <- mean(pares_inter)
  }

  # Alpha promedio ponderado por numero de items
  if (nrow(alpha_dimensiones) > 0) {
    alpha_promedio <- weighted.mean(
      alpha_dimensiones$alpha_semantico,
      alpha_dimensiones$n_items
    )
    similitud_intra_promedio <- mean(similitudes_intra)
  } else {
    alpha_promedio <- NA
    similitud_intra_promedio <- NA
  }

  # Construir resultado
  resultado <- list(
    alpha_dimensiones = alpha_dimensiones,
    alpha_promedio = alpha_promedio,
    similitud_intra = similitud_intra_promedio,
    similitud_inter = similitud_inter,
    discriminacion = if (!is.na(similitud_inter)) similitud_intra_promedio - similitud_inter else NA,
    metadata = list(
      metodo = metodo,
      n_items = nrow(similitud),
      n_dimensiones = length(dimensiones),
      fecha = Sys.time()
    )
  )

  class(resultado) <- c("semilla_fiabilidad", "list")

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("RESUMEN:"), "\n\n")

    cat("  Alpha promedio ponderado: ", sprintf("%.3f", alpha_promedio),
        " (", .interpretar_alpha(alpha_promedio), ")\n\n", sep = "")

    cat("  Similitud INTRA-dimension: ", sprintf("%.3f", similitud_intra_promedio), "\n", sep = "")
    cat("  Similitud INTER-dimension: ", sprintf("%.3f", similitud_inter), "\n", sep = "")

    if (!is.na(resultado$discriminacion)) {
      cat("  Discriminacion (intra-inter): ", sprintf("%.3f", resultado$discriminacion), sep = "")
      if (resultado$discriminacion > 0.10) {
        cat(" ", .color_check(), "\n")
      } else {
        cat(" ", .color_warning(), " (baja discriminacion entre factores)\n")
      }
    }

    cat("\n")
    cat(.linea("-"), "\n")
    cat("NOTA: Alpha calculado POR DIMENSION para evitar inflacion\n")
    cat("por numero de items (sesgo conocido de Cronbach's alpha).\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


# =============================================================================
# FUNCIONES INTERNAS - V DE AIKEN
# =============================================================================

#' @keywords internal
.construir_contexto_validez <- function(concepto, concepto_original, criterios) {

  definicion <- if (!is.null(concepto$definicion)) concepto$definicion else ""
  dimensiones_texto <- if (!is.null(concepto$dimensiones)) {
    paste(names(concepto$dimensiones), collapse = ", ")
  } else ""

  list(
    concepto = concepto_original,
    definicion = definicion,
    dimensiones = dimensiones_texto,
    criterios = criterios
  )
}


#' @keywords internal
#' @keywords internal
#' Heuristica para detectar si un item esta redactado en sentido inverso al
#' rasgo teorico de la dimension. Usa dos sennales:
#'  (1) presencia de palabras de negacion explicita en el item
#'  (2) discrepancia semantica entre la "caracteristica" (atributo positivo
#'      o negativo del rasgo segun el LLM lo definio) y la valencia tipica de
#'      la dimension.
#' Conservadora: marca como inverso solo cuando ambas senales coinciden.
.detectar_item_inverso <- function(item, dimension, caracteristica = "") {
  if (!is.character(item) || length(item) == 0) return(FALSE)

  txt_item <- tolower(item)
  txt_dim  <- tolower(dimension)
  txt_car  <- tolower(if (is.null(caracteristica)) "" else caracteristica)

  # 1) Palabras tipicas de negacion en espannol
  patrones_neg <- c(
    "\\bno\\s",         # "no presto", "no me siento"
    "\\bnunca\\b",
    "\\brara(s)? vez\\b",
    "\\bpoco\\b", "\\bpocas\\b",
    "\\bsin\\b",        # "sin estar pendiente"
    "\\bevito\\b", "\\bevita(r|ndo)\\b",
    "\\brechazo\\b", "\\brechaza"
  )
  tiene_negacion <- any(vapply(patrones_neg, function(p) grepl(p, txt_item),
                                logical(1)))

  # 2) Palabras de "permisividad/autonomia" en items que pertenecen a una
  #    dimension de sobreproteccion (Ansioso) o de hostilidad (Desorganizado)
  patrones_autonomia <- c(
    "\\bdejo\\b", "\\bdeja\\b", "\\bdejar\\b",
    "\\bconfio\\b", "\\bconfia\\b",
    "\\bautonom",
    "\\btomar decision",
    "\\bsin (mi|estar)"
  )
  tiene_autonomia <- any(vapply(patrones_autonomia, function(p) grepl(p, txt_item),
                                 logical(1)))

  # 3) Diccionario de valencia tipica por palabra clave en la dimension
  dim_es_negativa <- grepl("evitativo|ansioso|desorgan|inseguro|hostil|distan|negat",
                            txt_dim)
  dim_es_positiva <- grepl("seguro|sensib|empat|positiv|adecuado|present", txt_dim)

  # Caracteristica con valencia opuesta a la dimension
  car_es_positiva <- grepl("respondo|atien|present|claro|tranquil|adecuad|empat|sensib|cerca|disponible",
                            txt_car)
  car_es_negativa <- grepl("no respondo|distancia|rechaz|miedo|ausent|frio|inconsist",
                            txt_car)

  # Reglas combinadas (muy conservadoras: solo marca cuando la senal es
  # inequivoca; en casos sutiles deja la deteccion al LLM via "caracteristica").
  # caso_a: dim positiva (seguro) y item describe explicitamente algo
  #   incompatible (rechazo, evito, miedo) en su contenido.
  caso_a <- dim_es_positiva && grepl("\\b(rechaz|evito|temeroso|miedo|distancia)\\b", txt_item)
  # caso_b: dim negativa (ansioso/evitativo/desorganizado) e item describe
  #   permisividad/autonomia o caracteristica explicita es positiva.
  caso_b <- dim_es_negativa && (tiene_autonomia || car_es_positiva)
  # caso_c: el item niega explicitamente palabras de la caracteristica
  #   (deteccion de antonimia local).
  caso_c <- FALSE
  if (nzchar(txt_car) && tiene_negacion) {
    palabras_car <- unlist(strsplit(txt_car, "\\W+"))
    palabras_car <- palabras_car[nchar(palabras_car) > 4]
    if (length(palabras_car) > 0) {
      caso_c <- any(vapply(palabras_car,
                            function(p) grepl(paste0("\\bno\\s+[^\\.]*\\b", p),
                                              txt_item),
                            logical(1)))
    }
  }

  # caso_d: en dimensiones negativas, items que niegan explicitamente la
  #   emocion/conducta nuclear ("no me siento temeroso", "no rechazo",
  #   "no evito") describen el polo opuesto.
  caso_d <- dim_es_negativa &&
            grepl("\\bno\\s+(me\\s+siento\\s+|me\\s+|)(temeroso|temer|miedo|molest|hostil|rechaz|evit|frio|distancia|inconsist)",
                   txt_item)

  isTRUE(caso_a || caso_b || caso_c || caso_d)
}

.evaluar_item_jueces <- function(openai, item, dimension, definicion_dimension,
                                  caracteristica = "", es_inverso = FALSE,
                                  contexto, criterios, n_jueces, modelo) {

  criterios_texto <- paste(sapply(criterios, function(c) {
    switch(c,
      "relevancia" = "RELEVANCIA: Grado en que el item es importante y debe ser incluido en la medicion del constructo",
      "representatividad" = "REPRESENTATIVIDAD: Grado en que el item representa el constructo que se pretende medir",
      "claridad" = "CLARIDAD: Grado en que el item es claro, comprensible y sin ambiguedades",
      c
    )
  }), collapse = "\n")

  criterios_json <- paste(sprintf('"%s": <0-3>', criterios), collapse = ", ")

  # Bloque adicional para items invertidos: comunica al LLM que la valoracion
  # debe hacerse considerando que el item esta redactado en sentido inverso.
  bloque_inversion <- if (isTRUE(es_inverso)) {
    paste0(
      "\nNOTA CRITICA SOBRE DIRECCION DEL ITEM:\n",
      "Este item es INVERSO: esta redactado en sentido OPUESTO a la dimension declarada.\n",
      "Una respuesta de mayor frecuencia/acuerdo en este item indica MENOR presencia del rasgo.\n",
      "En la puntuacion final se invertira mediante recodificacion (k+1 - x).\n",
      "Por tanto, los jueces deben evaluar la RELEVANCIA y REPRESENTATIVIDAD\n",
      "considerando que MIDE el rasgo en sentido inverso (no penalizar por ser inverso).\n",
      "Trate el item como un indicador valido del rasgo siempre que su contenido\n",
      "sea claramente la negacion conductual o emocional de la dimension declarada.\n"
    )
  } else ""

  caract_block <- if (nzchar(caracteristica)) {
    paste0("CARACTERISTICA QUE PRETENDE MEDIR: ", caracteristica, "\n")
  } else ""

  prompt <- paste0(
    "Actua como un panel de ", n_jueces, " jueces expertos en psicometria evaluando items de una escala psicologica.\n\n",
    "CONSTRUCTO: ", contexto$concepto, "\n",
    "DEFINICION: ", contexto$definicion, "\n\n",
    "DIMENSION DEL ITEM: ", dimension, "\n",
    "DEFINICION DE LA DIMENSION: ", definicion_dimension, "\n",
    caract_block,
    bloque_inversion,
    "\nITEM A EVALUAR: \"", item, "\"\n\n",
    "Cada juez debe evaluar el item en los siguientes criterios usando escala 0-3:\n",
    "0 = Muy en desacuerdo / No cumple en absoluto\n",
    "1 = En desacuerdo / Cumple minimamente\n",
    "2 = De acuerdo / Cumple parcialmente\n",
    "3 = Muy de acuerdo / Cumple totalmente\n\n",
    "CRITERIOS A EVALUAR:\n", criterios_texto, "\n\n",
    "Responde SOLO con un JSON array con las evaluaciones de los ", n_jueces, " jueces:\n",
    "[\n",
    "  {\"juez\": 1, ", criterios_json, "},\n",
    "  {\"juez\": 2, ", criterios_json, "},\n",
    "  ...\n",
    "]\n\n",
    "IMPORTANTE:\n",
    "- Simula variabilidad real entre jueces expertos (no todos dan la misma puntuacion)\n",
    "- Considera que los jueces tienen diferentes perspectivas y experiencias\n",
    "- Las puntuaciones deben reflejar un juicio critico y profesional\n",
    "- Solo responde con el JSON, sin explicaciones adicionales."
  )

  respuesta <- openai$chat$completions$create(
    model = modelo,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = 0.7
  )

  texto <- respuesta$choices[[1]]$message$content

  # Parsear JSON
  tryCatch({
    texto_limpio <- gsub("```json|```", "", texto)
    texto_limpio <- trimws(texto_limpio)
    evaluaciones <- jsonlite::fromJSON(texto_limpio)
    return(evaluaciones)
  }, error = function(e) {
    warning("Error parseando evaluaciones para item: ", substr(item, 1, 30))
    # Retornar evaluaciones neutrales si falla
    df <- data.frame(juez = 1:n_jueces)
    for (crit in criterios) {
      df[[crit]] <- rep(2, n_jueces)  # Valor neutral
    }
    return(df)
  })
}


#' @keywords internal
.calcular_v_aiken <- function(evaluaciones, items_df, criterios, n_jueces, confianza) {

  n_items <- length(evaluaciones)

  # Parametros para V de Aiken
  l <- 0   # Valor minimo
  s <- 3   # Valor maximo
  k <- s - l  # Rango

  # Valor z para el nivel de confianza
  z <- qnorm(1 - (1 - confianza) / 2)

  # Matriz de evaluaciones por criterio
  matrices_criterios <- list()
  for (crit in criterios) {
    mat <- matrix(NA, nrow = n_items, ncol = n_jueces)
    for (i in 1:n_items) {
      if (crit %in% names(evaluaciones[[i]])) {
        vals <- evaluaciones[[i]][[crit]]
        # Asegurar que tenemos n_jueces valores
        if (length(vals) >= n_jueces) {
          mat[i, ] <- vals[1:n_jueces]
        } else {
          mat[i, 1:length(vals)] <- vals
          mat[i, (length(vals)+1):n_jueces] <- 2  # Completar con neutral
        }
      } else {
        mat[i, ] <- rep(2, n_jueces)
      }
    }
    matrices_criterios[[crit]] <- mat
  }

  # Calcular V de Aiken para cada item y criterio
  v_por_criterio <- list()
  ic_inf_por_criterio <- list()
  ic_sup_por_criterio <- list()

  for (crit in criterios) {
    mat <- matrices_criterios[[crit]]

    # V = (X_bar - l) / k
    x_bar <- rowMeans(mat)
    v <- (x_bar - l) / k

    # Intervalos de confianza (Penfield & Giacobbi, 2004)
    # L = (2nkV + z^2 - z*sqrt(4nkV(1-V) + z^2)) / (2(nk + z^2))
    # U = (2nkV + z^2 + z*sqrt(4nkV(1-V) + z^2)) / (2(nk + z^2))

    ic1 <- 2 * n_jueces * k * v + z^2
    ic2 <- z * sqrt(4 * n_jueces * k * v * (1 - v) + z^2)
    ic3 <- 2 * (n_jueces * k + z^2)

    ic_inf <- (ic1 - ic2) / ic3
    ic_sup <- (ic1 + ic2) / ic3

    # Asegurar rango [0, 1]
    v <- pmax(0, pmin(1, v))
    ic_inf <- pmax(0, pmin(1, ic_inf))
    ic_sup <- pmax(0, pmin(1, ic_sup))

    v_por_criterio[[crit]] <- v
    ic_inf_por_criterio[[crit]] <- ic_inf
    ic_sup_por_criterio[[crit]] <- ic_sup
  }

  # V promedio entre criterios
  v_promedio <- rowMeans(do.call(cbind, v_por_criterio))
  ic_inf_promedio <- rowMeans(do.call(cbind, ic_inf_por_criterio))
  ic_sup_promedio <- rowMeans(do.call(cbind, ic_sup_por_criterio))

  # Construir dataframe de resultados
  v_aiken <- data.frame(
    numero = items_df$numero,
    dimension = items_df$dimension,
    item = items_df$item,
    V_promedio = round(v_promedio, 3),
    IC_inf = round(ic_inf_promedio, 3),
    IC_sup = round(ic_sup_promedio, 3),
    stringsAsFactors = FALSE
  )

  # Agregar V por criterio
  for (crit in criterios) {
    v_aiken[[paste0("V_", crit)]] <- round(v_por_criterio[[crit]], 3)
  }

  # V de Aiken a nivel de escala
  v_aiken_escala <- list(
    V_total = round(mean(v_promedio), 3)
  )
  for (crit in criterios) {
    v_aiken_escala[[crit]] <- round(mean(v_por_criterio[[crit]]), 3)
  }

  list(
    v_aiken = v_aiken,
    v_aiken_escala = v_aiken_escala,
    evaluaciones = matrices_criterios
  )
}


# =============================================================================
# FUNCIONES INTERNAS - FIABILIDAD SEMANTICA
# =============================================================================

#' @keywords internal
.calcular_alpha_semantico <- function(similitud) {
  # Spearman-Brown formula: alpha = (n * r_bar) / (1 + (n-1) * r_bar)
  # donde r_bar es la similitud coseno promedio

  n <- nrow(similitud)

  # Obtener triangulo inferior (sin diagonal)
  sim_lower <- similitud[lower.tri(similitud)]

  # Similitud promedio
  r_bar <- mean(sim_lower)

  # Formula Spearman-Brown
  alpha <- (n * r_bar) / (1 + (n - 1) * r_bar)

  # Asegurar que este en rango [0, 1]
  alpha <- max(0, min(1, alpha))

  list(
    alpha = alpha,
    similitud_promedio = r_bar,
    n_items = n
  )
}


#' @keywords internal
.interpretar_alpha <- function(alpha) {
  if (alpha >= 0.90) {
    "Excelente"
  } else if (alpha >= 0.80) {
    "Bueno"
  } else if (alpha >= 0.70) {
    "Aceptable"
  } else if (alpha >= 0.60) {
    "Cuestionable"
  } else {
    "Pobre"
  }
}


# =============================================================================
# METODOS PRINT
# =============================================================================

#' @title Print Content Validity Results (V de Aiken)
#' @param x Objeto semilla_cv
#' @param ... Argumentos adicionales
#' @export
print.semilla_cv <- function(x, ...) {
  cat("\n")
  cat(.linea("="), "\n")
  cat(.color_verde("VALIDEZ DE CONTENIDO - V DE AIKEN"), "\n")
  cat("Ref: Ventura-Leon (2022). Adicciones, 34(4), 323-326\n")
  cat(.linea("="), "\n\n")

  # V de Aiken por criterio
  cat(.color_azul("V de Aiken por criterio:"), "\n")
  for (crit in x$metadata$criterios) {
    v_crit <- x$v_aiken_escala[[crit]]
    status <- if (v_crit >= 0.70) .color_check() else .color_warning()
    cat("  ", crit, ": ", sprintf("%.3f", v_crit), " ", status, "\n", sep = "")
  }

  cat("\n")
  cat(.color_azul("V de Aiken Total:"), " ", sprintf("%.3f", x$v_aiken_escala$V_total), sep = "")
  if (x$v_aiken_escala$V_total >= 0.70) {
    cat(" ", .color_check(), "\n")
  } else {
    cat(" ", .color_warning(), " (< 0.70)\n")
  }

  cat("\nItems evaluados: ", x$metadata$n_items, "\n", sep = "")
  cat("Jueces simulados: ", x$metadata$n_jueces, "\n", sep = "")
  cat("Nivel de confianza: ", x$metadata$confianza * 100, "%\n\n", sep = "")

  if (nrow(x$recomendaciones) > 0) {
    cat(.color_azul("Items que requieren revision (V < 0.70 o IC_inf < 0.70):"), "\n")
    for (i in 1:min(10, nrow(x$recomendaciones))) {
      cat("  [", x$recomendaciones$numero[i], "] V=",
          sprintf("%.2f", x$recomendaciones$V_promedio[i]),
          " IC[", sprintf("%.2f", x$recomendaciones$IC_inf[i]), ",",
          sprintf("%.2f", x$recomendaciones$IC_sup[i]), "] - ",
          substr(x$recomendaciones$item[i], 1, 40), "...\n", sep = "")
    }
    if (nrow(x$recomendaciones) > 10) {
      cat("  ... y ", nrow(x$recomendaciones) - 10, " items mas\n", sep = "")
    }
  } else {
    cat(.color_check(), " Todos los items tienen V >= 0.70 con IC_inf >= 0.70\n")
  }

  cat("\n")
  cat(.linea("="), "\n\n")
  invisible(x)
}


#' @title Print Semantic Reliability Results
#' @param x Objeto semilla_fiabilidad
#' @param ... Argumentos adicionales
#' @export
print.semilla_fiabilidad <- function(x, ...) {
  cat("\n")
  cat(.linea("="), "\n")
  cat(.color_verde("FIABILIDAD SEMANTICA - ALPHA POR DIMENSION"), "\n")
  cat("Metodo: Spearman-Brown sobre similitud coseno entre embeddings\n")
  cat("Ref: Wulff & Mata (2025) Nature Human Behaviour, 9(5), 944-954\n")
  cat("     [r alpha observado vs predicho = .75 in-sample, .61 out-of-sample, n = 449]\n")
  cat(.linea("="), "\n\n")

  # Alpha por dimension
  if (!is.null(x$alpha_dimensiones) && nrow(x$alpha_dimensiones) > 0) {
    cat(.color_azul("Alpha Semantico por dimension:"), "\n")
    for (i in 1:nrow(x$alpha_dimensiones)) {
      interp <- .interpretar_alpha(x$alpha_dimensiones$alpha_semantico[i])
      cat("  ", x$alpha_dimensiones$dimension[i], ": ",
          sprintf("%.3f", x$alpha_dimensiones$alpha_semantico[i]),
          " (", interp, ", n=", x$alpha_dimensiones$n_items[i],
          ", r=", sprintf("%.2f", x$alpha_dimensiones$similitud_promedio[i]), ")\n", sep = "")
    }
    cat("\n")
  }

  # Resumen
  cat(.color_azul("Resumen:"), "\n")
  cat("  Alpha promedio ponderado: ", sprintf("%.3f", x$alpha_promedio),
      " (", .interpretar_alpha(x$alpha_promedio), ")\n", sep = "")
  cat("  Similitud INTRA-dimension: ", sprintf("%.3f", x$similitud_intra), "\n", sep = "")

  if (!is.na(x$similitud_inter)) {
    cat("  Similitud INTER-dimension: ", sprintf("%.3f", x$similitud_inter), "\n", sep = "")
    cat("  Discriminacion (intra-inter): ", sprintf("%.3f", x$discriminacion), sep = "")
    if (x$discriminacion > 0.10) {
      cat(" ", .color_check(), "\n")
    } else {
      cat(" ", .color_warning(), "\n")
    }
  }

  # Advertencia si existe
  if (!is.null(x$advertencia)) {
    cat("\n  ", .color_warning(), " ", x$advertencia, "\n", sep = "")
  }

  cat("\n")
  cat(.linea("-"), "\n")
  cat("NOTA: Alpha calculado POR DIMENSION (no total) para evitar\n")
  cat("inflacion artificial por numero de items.\n")
  cat(.linea("="), "\n\n")

  invisible(x)
}


# =============================================================================
# FUNCIONES AVANZADAS - BASADAS EN LITERATURA RECIENTE
# =============================================================================

#' @title Generar Forma Corta de la Escala
#'
#' @description
#' Genera una version abreviada de la escala utilizando K-means clustering
#' sobre los embeddings semanticos. Selecciona items representativos de cada
#' cluster (los mas cercanos al centroide), manteniendo la estructura factorial.
#'
#' Este metodo permite reducir escalas SIN necesidad de datos de respuesta,
#' basandose unicamente en la similitud semantica de los items.
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param n_items Numero de items deseados en la forma corta
#' @param por_dimension Si TRUE, selecciona items proporcionalmente por dimension
#' @param metodo Metodo de seleccion: "centroide" (default) o "diverso"
#' @param verbose Mostrar progreso
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{items}: Dataframe con items seleccionados
#'   \item \code{indices}: Indices de los items seleccionados
#'   \item \code{clusters}: Asignacion de clusters
#'   \item \code{distancia_centroide}: Distancia de cada item a su centroide
#' }
#'
#' @details
#' El algoritmo funciona asi:
#' \enumerate{
#'   \item Aplica K-means sobre la matriz de embeddings
#'   \item Identifica el centroide de cada cluster
#'   \item Selecciona los items mas cercanos a cada centroide
#'   \item Distribuye proporcionalmente si se usa por_dimension=TRUE
#' }
#'
#' El metodo "diverso" selecciona items que maximizan la cobertura semantica,
#' prefiriendo items mas alejados entre si dentro de cada cluster.
#'
#' @section Recomendacion de uso (forma_corta vs forma_breve):
#' \itemize{
#'   \item \strong{Sin datos de respuesta (caso habitual): usar
#'     \code{forma_corta} (esta funcion).} En la validacion interna (EEAP) fue el
#'     criterio puramente semantico mas robusto (coincidencia ~69\% con la
#'     seleccion empirica, frente a ~56\% de \code{forma_breve} semantico).
#'   \item \strong{Con un piloto de respuestas reales (~90+): usar
#'     \code{\link{forma_breve}} en modo hibrido} (argumento
#'     \code{respuestas_piloto}), que sube la coincidencia a ~88\%.
#' }
#' Es decir: \code{forma_corta} es el \emph{default sin datos};
#' \code{forma_breve} aporta sobre todo cuando hay piloto empirico.
#'
#' @seealso \code{\link{forma_breve}}, \code{\link{discriminacion_semantica}}
#'
#' @examples
#' \dontrun{
#' # Generar forma corta de 15 items
#' corta <- forma_corta(mi_escala, n_items = 15)
#'
#' # Ver items seleccionados
#' corta$items
#'
#' # Forma corta proporcional por dimension
#' corta <- forma_corta(mi_escala, n_items = 20, por_dimension = TRUE)
#' }
#'
#' @references
#' Yang, Y., & Chiu, C. P. (2025). A transformer-based embedding approach to
#' developing short-form psychological measures. Frontiers in Psychology, 16.
#'
#' Olaru, G., Schroeders, U., & Wilhelm, O. (2025). Shortening Psychological
#' Scales: Semantic Similarity Matters. European Journal of Psychological
#' Assessment.
#'
#' @export
forma_corta <- function(x,
                        n_items,
                        por_dimension = TRUE,
                        metodo = "centroide",
                        verbose = TRUE) {

  # Extraer embeddings y items
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$embeddings)) {
      stop("El objeto no tiene embeddings. Ejecuta obtener_embeddings() primero.")
    }
    embeddings <- x$embeddings
    items_df <- x$items
  } else if (is.list(x) && !is.null(x$embeddings)) {
    embeddings <- x$embeddings
    items_df <- x$items
  } else {
    stop("Objeto no valido. Usa un objeto semilla o lista con $embeddings.")
  }

  n_total <- nrow(embeddings)

  if (n_items >= n_total) {
    warning("n_items >= items totales. Retornando todos los items.")
    return(list(
      items = items_df,
      indices = 1:n_total,
      clusters = NULL,
      distancia_centroide = NULL
    ))
  }

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("FORMA CORTA - K-MEANS CLUSTERING"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Items originales: ", n_total, "\n", sep = "")
    cat("  Items objetivo: ", n_items, "\n", sep = "")
    cat("  Metodo: ", metodo, "\n", sep = "")
    cat("  Por dimension: ", por_dimension, "\n\n", sep = "")
  }

  if (por_dimension && "dimension" %in% names(items_df)) {
    # Seleccionar proporcionalmente por dimension
    dimensiones <- unique(items_df$dimension)
    n_dims <- length(dimensiones)

    # Calcular items por dimension (proporcional al tamano original)
    items_por_dim <- table(items_df$dimension)
    prop_dim <- items_por_dim / sum(items_por_dim)
    n_por_dim <- round(prop_dim * n_items)

    # Ajustar para que sume exactamente n_items
    diff <- n_items - sum(n_por_dim)
    if (diff != 0) {
      orden <- order(n_por_dim, decreasing = TRUE)
      for (i in 1:abs(diff)) {
        idx <- orden[((i - 1) %% length(orden)) + 1]
        n_por_dim[idx] <- n_por_dim[idx] + sign(diff)
      }
    }

    if (verbose) {
      cat("  ", .color_flecha(), " Items por dimension:\n", sep = "")
      for (d in names(n_por_dim)) {
        cat("    [", d, "] ", n_por_dim[d], " items\n", sep = "")
      }
      cat("\n")
    }

    # Seleccionar items de cada dimension
    indices_seleccionados <- c()

    for (dim_nombre in names(n_por_dim)) {
      n_sel <- n_por_dim[dim_nombre]
      if (n_sel < 1) next

      idx_dim <- which(items_df$dimension == dim_nombre)
      emb_dim <- embeddings[idx_dim, , drop = FALSE]

      if (length(idx_dim) <= n_sel) {
        indices_seleccionados <- c(indices_seleccionados, idx_dim)
      } else {
        # K-means dentro de la dimension
        n_clusters <- min(n_sel, length(idx_dim))
        set.seed(42)
        km <- kmeans(emb_dim, centers = n_clusters, nstart = 10)

        # Seleccionar item mas cercano a cada centroide
        for (k in 1:n_clusters) {
          idx_cluster <- which(km$cluster == k)
          if (length(idx_cluster) == 0) next

          if (metodo == "centroide") {
            # Distancia al centroide
            centroide <- km$centers[k, ]
            distancias <- apply(emb_dim[idx_cluster, , drop = FALSE], 1, function(e) {
              sqrt(sum((e - centroide)^2))
            })
            mejor <- idx_cluster[which.min(distancias)]
          } else {
            # Metodo diverso: item mas unico (menor similitud promedio)
            if (length(idx_cluster) == 1) {
              mejor <- idx_cluster[1]
            } else {
              sim_cluster <- .calcular_similitud_coseno(emb_dim[idx_cluster, , drop = FALSE])
              sim_promedio <- rowMeans(sim_cluster)
              mejor <- idx_cluster[which.min(sim_promedio)]
            }
          }

          indices_seleccionados <- c(indices_seleccionados, idx_dim[mejor])
        }
      }
    }

  } else {
    # Seleccion global sin considerar dimensiones
    if (verbose) cat("  ", .color_flecha(), " Aplicando K-means global...\n", sep = "")

    set.seed(42)
    km <- kmeans(embeddings, centers = n_items, nstart = 10)

    indices_seleccionados <- c()
    distancias_centroide <- numeric(n_items)

    for (k in 1:n_items) {
      idx_cluster <- which(km$cluster == k)
      if (length(idx_cluster) == 0) next

      centroide <- km$centers[k, ]
      distancias <- apply(embeddings[idx_cluster, , drop = FALSE], 1, function(e) {
        sqrt(sum((e - centroide)^2))
      })

      mejor <- idx_cluster[which.min(distancias)]
      indices_seleccionados <- c(indices_seleccionados, mejor)
      distancias_centroide[length(indices_seleccionados)] <- min(distancias)
    }
  }

  # Eliminar duplicados y ordenar
  indices_seleccionados <- unique(indices_seleccionados)
  indices_seleccionados <- sort(indices_seleccionados)

  # Ajustar si hay menos items de los esperados
  if (length(indices_seleccionados) < n_items && length(indices_seleccionados) < n_total) {
    faltantes <- setdiff(1:n_total, indices_seleccionados)
    n_agregar <- min(n_items - length(indices_seleccionados), length(faltantes))
    indices_seleccionados <- sort(c(indices_seleccionados, faltantes[1:n_agregar]))
  }

  # Construir resultado
  items_seleccionados <- items_df[indices_seleccionados, ]
  items_seleccionados$numero_original <- indices_seleccionados
  items_seleccionados$numero <- 1:nrow(items_seleccionados)

  resultado <- list(
    items = items_seleccionados,
    indices = indices_seleccionados,
    n_original = n_total,
    n_seleccionados = length(indices_seleccionados),
    metodo = metodo,
    por_dimension = por_dimension
  )

  class(resultado) <- c("semilla_forma_corta", "list")

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("RESULTADO:"), "\n\n")
    cat("  Items seleccionados: ", length(indices_seleccionados), "/", n_total, "\n", sep = "")
    cat("  Reduccion: ", round((1 - length(indices_seleccionados)/n_total) * 100, 1), "%\n\n", sep = "")

    if ("dimension" %in% names(items_seleccionados)) {
      cat("  Por dimension:\n")
      tabla <- table(items_seleccionados$dimension)
      for (d in names(tabla)) {
        cat("    [", d, "] ", tabla[d], " items\n", sep = "")
      }
    }

    cat("\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


#' @title Discriminacion Semantica de Items
#'
#' @description
#' Predice el poder discriminativo de cada item basandose en su "unicidad
#' semantica" - items con baja similitud promedio tienden a tener mayor
#' poder de discriminacion segun la Teoria de Respuesta al Item (IRT).
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param verbose Mostrar progreso
#'
#' @return Dataframe con:
#' \itemize{
#'   \item \code{item}: Texto del item
#'   \item \code{dimension}: Dimension del item
#'   \item \code{similitud_media}: Similitud promedio con otros items de su dimension
#'   \item \code{unicidad}: 1 - similitud_media (indicador de discriminacion)
#'   \item \code{discriminacion_predicha}: Categoria predicha (alta/media/baja)
#' }
#'
#' @details
#' La idea (paradoja de la atenuacion; Loevinger, 1954) es que items casi
#' redundantes aportan poca informacion incremental, por lo que items
#' semanticamente mas unicos pueden discriminar mas. El indice replica
#' exactamente el de Kilmen & Bulut (2025): la \strong{similitud coseno media de
#' cada item con los demas de su MISMA subescala} (\code{similitud_media}), y la
#' unicidad como \code{1 - similitud_media}; se priorizan los items de mayor
#' unicidad.
#'
#' \strong{Evidencia (matiz importante):} el efecto es \emph{moderado y depende
#' de la subescala}, no es una ley general. En Kilmen & Bulut (2025) la
#' correlacion entre discriminacion IRT y similitud media fue \strong{r = -.546
#' (p < .05) en la subescala de ansiedad}, pero \strong{r = +.036 (no
#' significativo) en la de evitacion}. En una validacion interna con la EEAP
#' (n = 100) la relacion tambien fue inconsistente entre dimensiones (de
#' aprox. -.10 a +.67). Por tanto:
#' \itemize{
#'   \item La unicidad semantica es un proxy \emph{util pero inestable} de la
#'         discriminacion; conviene confirmarlo con datos de respuesta reales.
#'   \item Funciona mejor como filtro de \emph{redundancia} (descartar
#'         casi-duplicados) que como ranking fino de discriminacion.
#'   \item Para seleccion alineada con la carga factorial, comparar con
#'         \code{\link{forma_breve}} (criterio repr - cross).
#' }
#'
#' @examples
#' \dontrun{
#' # Calcular discriminacion semantica
#' disc <- discriminacion_semantica(mi_escala)
#'
#' # Ver items con mayor discriminacion predicha
#' disc[disc$discriminacion_predicha == "alta", ]
#'
#' # Ordenar por unicidad
#' disc[order(-disc$unicidad), ]
#' }
#'
#' @references
#' Kilmen, S., & Bulut, O. (2025). Shortening Psychological Scales: Semantic
#' Similarity Matters. Educational and Psychological Measurement, 85(5),
#' 910-934. https://doi.org/10.1177/00131644251319047
#'
#' Loevinger, J. (1954). The attenuation paradox in test theory. Psychological
#' Bulletin, 51(5), 493-504. https://doi.org/10.1037/h0058543
#'
#' @export
discriminacion_semantica <- function(x, verbose = TRUE) {

  # Extraer similitud y items
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$similitud)) {
      stop("El objeto no tiene matriz de similitud. Ejecuta obtener_embeddings() primero.")
    }
    similitud <- x$similitud
    items_df <- x$items
  } else if (is.list(x) && !is.null(x$similitud)) {
    similitud <- x$similitud
    items_df <- x$items
  } else {
    stop("Objeto no valido. Usa un objeto semilla o lista con $similitud.")
  }

  n_items <- nrow(similitud)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("DISCRIMINACION SEMANTICA"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Analizando ", n_items, " items...\n\n", sep = "")
  }

  # Calcular similitud media por item (dentro de su dimension)
  similitud_media <- numeric(n_items)
  dimensiones <- items_df$dimension

  for (i in 1:n_items) {
    # Items de la misma dimension (excluyendo el item actual)
    idx_dim <- which(dimensiones == dimensiones[i])
    idx_dim <- idx_dim[idx_dim != i]

    if (length(idx_dim) > 0) {
      similitud_media[i] <- mean(similitud[i, idx_dim])
    } else {
      similitud_media[i] <- 0
    }
  }

  # Unicidad = 1 - similitud media
  unicidad <- 1 - similitud_media

  # Categorizar discriminacion
  # Basado en: unicidad alta -> discriminacion alta
  discriminacion_predicha <- cut(
    unicidad,
    breaks = c(-Inf, 0.3, 0.5, Inf),
    labels = c("baja", "media", "alta")
  )

  # Construir dataframe de resultados
  resultado <- data.frame(
    numero = 1:n_items,
    dimension = items_df$dimension,
    item = items_df$item,
    similitud_media = round(similitud_media, 3),
    unicidad = round(unicidad, 3),
    discriminacion_predicha = as.character(discriminacion_predicha),
    stringsAsFactors = FALSE
  )

  # Ordenar por unicidad descendente
  resultado <- resultado[order(-resultado$unicidad), ]
  rownames(resultado) <- NULL

  if (verbose) {
    cat(.color_flecha(), " Resultados por categoria:\n\n", sep = "")

    tabla <- table(discriminacion_predicha)
    cat("  Discriminacion ALTA: ", tabla["alta"], " items\n", sep = "")
    cat("  Discriminacion MEDIA: ", tabla["media"], " items\n", sep = "")
    cat("  Discriminacion BAJA: ", tabla["baja"], " items\n\n", sep = "")

    cat(.color_flecha(), " Top 5 items con mayor discriminacion predicha:\n\n", sep = "")
    for (i in 1:min(5, nrow(resultado))) {
      cat("  [", resultado$numero[i], "] unicidad=", sprintf("%.2f", resultado$unicidad[i]),
          " | ", substr(resultado$item[i], 1, 45), "...\n", sep = "")
    }

    cat("\n")
    cat(.linea("-"), "\n")
    cat("NOTA: Unicidad alta = baja similitud promedio = mayor discriminacion\n")
    cat("Ref: Kilmen & Bulut (2025) EPM 85(5), 910-934 - r = -0.55 (sub-escala ansiedad)\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


#' @title Cargas Semanticas (Semantic Loadings)
#'
#' @description
#' Calcula el solapamiento entre clusters semanticos (basados en embeddings)
#' y factores psicometricos (del EFA), usando el indice de Jaccard.
#'
#' Las cargas semanticas cuantifican cuanto del contenido linguistico de
#' los items se corresponde con su agrupacion factorial, identificando
#' items "semanticamente incoherentes" con su factor asignado.
#'
#' @param x Objeto semilla con EFA realizado, o semilla_efa
#' @param n_clusters Numero de clusters semanticos (NULL = igual a n_factores)
#' @param verbose Mostrar progreso
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{jaccard_matrix}: Matriz de indices Jaccard (clusters x factores)
#'   \item \code{correspondencia}: Mejor correspondencia cluster-factor
#'   \item \code{items_incoherentes}: Items donde cluster != factor
#'   \item \code{jaccard_promedio}: Indice Jaccard promedio
#' }
#'
#' @details
#' El proceso:
#' \enumerate{
#'   \item Aplica K-means sobre embeddings para obtener clusters semanticos
#'   \item Obtiene asignacion factorial del EFA
#'   \item Calcula indice de Jaccard para cada par cluster-factor
#'   \item Identifica la mejor correspondencia y items incoherentes
#' }
#'
#' El indice de Jaccard mide la similitud entre dos conjuntos:
#' \deqn{J(A,B) = |A \cap B| / |A \cup B|}
#'
#' Valores altos indican que los items agrupados semanticamente tambien
#' se agrupan empiricamente en el EFA.
#'
#' @examples
#' \dontrun{
#' # Calcular cargas semanticas
#' cs <- cargas_semanticas(mi_escala)
#'
#' # Ver matriz de Jaccard
#' cs$jaccard_matrix
#'
#' # Items incoherentes
#' cs$items_incoherentes
#' }
#'
#' @references
#' Stanghellini, E., Perinelli, E., Lombardi, L., & Stella, M. (2024).
#' Introducing semantic loadings in factor analysis: Bridging network
#' psychometrics and cognitive networks for understanding depression,
#' anxiety and stress. Advances in Psychology, doi: 10.56296/aip00008
#'
#' @export
cargas_semanticas <- function(x,
                               n_clusters = NULL,
                               verbose = TRUE) {

  # Extraer datos necesarios
  if (inherits(x, "semilla") || (is.list(x) && !is.null(x$embeddings) && !is.null(x$efa))) {
    if (is.null(x$embeddings)) {
      stop("El objeto no tiene embeddings. Ejecuta obtener_embeddings() primero.")
    }
    if (is.null(x$efa)) {
      stop("El objeto no tiene estructura de clusters.")
    }
    embeddings <- x$embeddings
    efa <- x$efa
    items_df <- x$items
  } else if (inherits(x, "semilla_efa")) {
    stop("Para cargas_semanticas necesitas el objeto semilla completo con embeddings.")
  } else {
    stop("Objeto no valido. Usa un objeto semilla o lista con $embeddings y $efa.")
  }

  n_items <- nrow(embeddings)
  n_factores <- efa$metadata$n_factores

  if (is.null(n_clusters)) {
    n_clusters <- n_factores
  }

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("CARGAS SEMANTICAS - JACCARD INDEX"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Items: ", n_items, "\n", sep = "")
    cat("  Factores EFA: ", n_factores, "\n", sep = "")
    cat("  Clusters semanticos: ", n_clusters, "\n\n", sep = "")
  }

  # PASO 1: K-means sobre embeddings
  if (verbose) cat("  ", .color_flecha(), " Generando clusters semanticos...\n", sep = "")

  set.seed(42)
  km <- kmeans(embeddings, centers = n_clusters, nstart = 25)
  cluster_semantico <- km$cluster

  # PASO 2: Obtener asignacion EFA
  factor_efa <- as.numeric(gsub("F", "", efa$asignacion$factor_EFA))
  factor_efa_ordenado <- factor_efa[order(efa$asignacion$item_num)]

  # PASO 3: Calcular matriz de Jaccard
  if (verbose) cat("  ", .color_flecha(), " Calculando indices de Jaccard...\n", sep = "")

  jaccard_matrix <- matrix(0, nrow = n_clusters, ncol = n_factores)
  rownames(jaccard_matrix) <- paste0("Cluster_", 1:n_clusters)
  colnames(jaccard_matrix) <- paste0("F", 1:n_factores)

  for (c in 1:n_clusters) {
    for (f in 1:n_factores) {
      set_cluster <- which(cluster_semantico == c)
      set_factor <- which(factor_efa_ordenado == f)

      interseccion <- length(intersect(set_cluster, set_factor))
      union <- length(union(set_cluster, set_factor))

      if (union > 0) {
        jaccard_matrix[c, f] <- interseccion / union
      }
    }
  }

  # PASO 4: Encontrar mejor correspondencia
  correspondencia <- data.frame(
    cluster = 1:n_clusters,
    factor_correspondiente = apply(jaccard_matrix, 1, which.max),
    jaccard = apply(jaccard_matrix, 1, max),
    n_items_cluster = as.vector(table(cluster_semantico)),
    stringsAsFactors = FALSE
  )
  correspondencia$factor_correspondiente <- paste0("F", correspondencia$factor_correspondiente)

  # PASO 5: Identificar items incoherentes
  factor_esperado <- correspondencia$factor_correspondiente[cluster_semantico]
  factor_real <- paste0("F", factor_efa_ordenado)
  incoherente <- factor_esperado != factor_real

  items_incoherentes <- data.frame(
    numero = which(incoherente),
    item = items_df$item[incoherente],
    cluster_semantico = cluster_semantico[incoherente],
    factor_esperado = factor_esperado[incoherente],
    factor_efa = factor_real[incoherente],
    stringsAsFactors = FALSE
  )

  # Jaccard promedio
  jaccard_promedio <- mean(correspondencia$jaccard)

  resultado <- list(
    jaccard_matrix = round(jaccard_matrix, 3),
    correspondencia = correspondencia,
    items_incoherentes = items_incoherentes,
    jaccard_promedio = round(jaccard_promedio, 3),
    cluster_asignacion = cluster_semantico,
    metadata = list(
      n_clusters = n_clusters,
      n_factores = n_factores,
      n_items = n_items,
      n_incoherentes = sum(incoherente)
    )
  )

  class(resultado) <- c("semilla_cargas_sem", "list")

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("RESULTADOS:"), "\n\n")

    cat("  Correspondencia Cluster-Factor:\n")
    for (i in 1:nrow(correspondencia)) {
      status <- if (correspondencia$jaccard[i] >= 0.5) .color_check() else .color_warning()
      cat("    Cluster ", correspondencia$cluster[i], " -> ",
          correspondencia$factor_correspondiente[i],
          " (J=", sprintf("%.2f", correspondencia$jaccard[i]), ") ", status, "\n", sep = "")
    }

    cat("\n  Jaccard promedio: ", sprintf("%.3f", jaccard_promedio), sep = "")
    if (jaccard_promedio >= 0.5) {
      cat(" ", .color_check(), "\n")
    } else {
      cat(" ", .color_warning(), "\n")
    }

    cat("  Items incoherentes: ", sum(incoherente), "/", n_items,
        " (", round(sum(incoherente)/n_items * 100, 1), "%)\n", sep = "")

    if (sum(incoherente) > 0 && sum(incoherente) <= 5) {
      cat("\n  Items con discrepancia semantica-factorial:\n")
      for (i in 1:nrow(items_incoherentes)) {
        cat("    [", items_incoherentes$numero[i], "] Cluster ",
            items_incoherentes$cluster_semantico[i], " (espera ",
            items_incoherentes$factor_esperado[i], ") -> ",
            items_incoherentes$factor_efa[i], "\n", sep = "")
      }
    }

    cat("\n")
    cat(.linea("-"), "\n")
    cat("NOTA: Jaccard alto indica coherencia entre estructura\n")
    cat("semantica (embeddings) y factorial (EFA).\n")
    cat("Ref: Stanghellini, Perinelli, Lombardi & Stella (2024) Adv. in Psychology\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


#' @title Evaluar Calidad de Items con LLM
#'
#' @description
#' Evalua automaticamente la calidad de los items usando un LLM como evaluador
#' experto, combinado con reglas basadas en buenas practicas psicometricas.
#'
#' @param x Objeto semilla, semilla_items, o dataframe con columna 'item'
#' @param api_key API key de OpenAI
#' @param criterios Criterios a evaluar (default: todos)
#' @param modelo Modelo de OpenAI: "gpt-4.1-mini" (default), "gpt-4o", "gpt-4o-mini",
#'   "gpt-4-turbo", "gpt-3.5-turbo", o cualquier modelo compatible
#' @param verbose Mostrar progreso
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{evaluacion}: Dataframe con puntuaciones por criterio
#'   \item \code{items_problematicos}: Items que no cumplen criterios minimos
#'   \item \code{recomendaciones}: Sugerencias de mejora por item
#' }
#'
#' @details
#' Criterios evaluados (escala 1-5):
#' \itemize{
#'   \item \strong{claridad}: Redaccion clara y sin ambiguedades
#'   \item \strong{simplicidad}: Una sola idea por item
#'   \item \strong{relevancia}: Pertinencia al constructo
#'   \item \strong{especificidad}: Precision y no vaguedad
#'   \item \strong{neutralidad}: Sin sesgo o carga emocional excesiva
#' }
#'
#' Reglas automaticas aplicadas:
#' \itemize{
#'   \item Longitud: 10-25 palabras ideal
#'   \item Doble negacion: Penalizacion automatica
#'   \item Palabras absolutas: Detecta "siempre", "nunca", etc.
#' }
#'
#' @examples
#' \dontrun{
#' # Evaluar calidad de items
#' calidad <- evaluar_calidad_items(mi_escala, api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Ver items problematicos
#' calidad$items_problematicos
#'
#' # Ver recomendaciones
#' calidad$recomendaciones
#' }
#'
#' @references
#' Laverghetta, A., Luchini, S., Linell, A., Reiter-Palmon, R., & Beaty, R.
#' (2024). The Creative Psychometric Item Generator (CPIG): A Framework
#' for Item Generation and Validation Using Large Language Models.
#' CREAI 2024 Workshop, Santiago de Compostela, Spain. arXiv:2409.00202.
#'
#' Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quinonez, H. R.,
#' & Young, S. L. (2018). Best practices for developing and validating
#' scales for health, social, and behavioral research: A primer.
#' Frontiers in Public Health, 6, 149.
#'
#' @export
auditar_redaccion_items <- function(x,
                                    api_key,
                                    criterios = c("claridad", "simplicidad", "relevancia",
                                                  "especificidad", "neutralidad"),
                                    modelo = "gpt-4.1-mini",
                                    verbose = TRUE) {

  # Extraer items
  if (inherits(x, "semilla")) {
    items_df <- x$items
    concepto <- x$metadata$concepto_original
  } else if (inherits(x, "semilla_items")) {
    items_df <- x$items
    concepto <- x$metadata$concepto_original
  } else if (is.data.frame(x)) {
    items_df <- x
    concepto <- "constructo psicologico"
  } else {
    stop("Objeto no valido.")
  }

  .validar_api_key(api_key)
  openai <- .configurar_openai(api_key)

  n_items <- nrow(items_df)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("EVALUACION DE CALIDAD DE ITEMS"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Items a evaluar: ", n_items, "\n", sep = "")
    cat("  Criterios: ", paste(criterios, collapse = ", "), "\n\n", sep = "")
  }

  # Evaluar cada item
  evaluaciones <- list()
  recomendaciones <- list()

  for (i in 1:n_items) {
    item <- items_df$item[i]
    dim <- if ("dimension" %in% names(items_df)) items_df$dimension[i] else NA

    if (verbose) {
      cat("  [", i, "/", n_items, "] Evaluando...\r", sep = "")
    }

    # Evaluacion LLM
    eval_llm <- .evaluar_item_calidad_llm(
      openai = openai,
      item = item,
      dimension = dim,
      concepto = concepto,
      criterios = criterios,
      modelo = modelo
    )

    # Reglas automaticas
    eval_reglas <- .evaluar_item_reglas(item)

    # Combinar
    evaluaciones[[i]] <- list(
      llm = eval_llm$puntuaciones,
      reglas = eval_reglas,
      promedio = mean(c(eval_llm$puntuaciones, eval_reglas$penalizacion_ajustada))
    )

    recomendaciones[[i]] <- eval_llm$recomendacion

    Sys.sleep(0.3)  # Rate limit
  }

  if (verbose) cat("\n\n")

  # Construir dataframe de resultados
  eval_df <- data.frame(
    numero = 1:n_items,
    item = items_df$item,
    stringsAsFactors = FALSE
  )

  if ("dimension" %in% names(items_df)) {
    eval_df$dimension <- items_df$dimension
  }

  # Agregar puntuaciones por criterio
  for (crit in criterios) {
    eval_df[[crit]] <- sapply(evaluaciones, function(e) {
      if (crit %in% names(e$llm)) e$llm[[crit]] else NA
    })
  }

  # Agregar reglas
  eval_df$longitud_ok <- sapply(evaluaciones, function(e) e$reglas$longitud_ok)
  eval_df$doble_negacion <- sapply(evaluaciones, function(e) e$reglas$doble_negacion)
  eval_df$palabras_absolutas <- sapply(evaluaciones, function(e) e$reglas$palabras_absolutas)

  # Promedio general
  cols_criterios <- criterios[criterios %in% names(eval_df)]
  if (length(cols_criterios) > 0) {
    eval_df$promedio <- rowMeans(eval_df[, cols_criterios, drop = FALSE], na.rm = TRUE)
  } else {
    eval_df$promedio <- NA
  }

  # Identificar items problematicos (promedio < 3 o reglas violadas)
  problematicos <- eval_df$promedio < 3 |
                   !eval_df$longitud_ok |
                   eval_df$doble_negacion |
                   eval_df$palabras_absolutas

  items_problematicos <- eval_df[problematicos, ]

  # Dataframe de recomendaciones
  rec_df <- data.frame(
    numero = 1:n_items,
    item = items_df$item,
    recomendacion = unlist(recomendaciones),
    stringsAsFactors = FALSE
  )

  resultado <- list(
    evaluacion = eval_df,
    items_problematicos = items_problematicos,
    recomendaciones = rec_df,
    resumen = list(
      promedio_general = mean(eval_df$promedio, na.rm = TRUE),
      n_problematicos = sum(problematicos),
      pct_problematicos = round(sum(problematicos) / n_items * 100, 1)
    )
  )

  class(resultado) <- c("semilla_calidad", "list")

  if (verbose) {
    cat(.linea("-"), "\n")
    cat(.color_verde("RESULTADOS:"), "\n\n")

    cat("  Calidad promedio: ", sprintf("%.2f", resultado$resumen$promedio_general), "/5\n", sep = "")
    cat("  Items problematicos: ", resultado$resumen$n_problematicos, "/", n_items,
        " (", resultado$resumen$pct_problematicos, "%)\n\n", sep = "")

    if (nrow(items_problematicos) > 0 && nrow(items_problematicos) <= 5) {
      cat("  Items que requieren revision:\n")
      for (i in 1:nrow(items_problematicos)) {
        cat("    [", items_problematicos$numero[i], "] prom=",
            sprintf("%.1f", items_problematicos$promedio[i]),
            " | ", substr(items_problematicos$item[i], 1, 40), "...\n", sep = "")
      }
    }

    cat("\n")
    cat(.linea("-"), "\n")
    cat("Ref: Laverghetta et al. (2024) CPIG Framework, arXiv:2409.00202\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


#' @title Predecir Parametros IRT desde Embeddings
#'
#' @description
#' Estima parametros de la Teoria de Respuesta al Item (IRT) utilizando
#' propiedades de los embeddings semanticos, sin necesidad de datos de respuesta.
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param verbose Mostrar progreso
#'
#' @return Dataframe con:
#' \itemize{
#'   \item \code{item}: Texto del item
#'   \item \code{dificultad_estimada}: Estimacion de dificultad (norma del embedding)
#'   \item \code{discriminacion_estimada}: Estimacion de discriminacion (unicidad semantica)
#'   \item \code{informacion_estimada}: Informacion del item (combinacion de ambos)
#' }
#'
#' @details
#' Basado en JE-IRT (Joint Embedding IRT):
#' \itemize{
#'   \item La \strong{direccion} del embedding codifica la semantica
#'   \item La \strong{norma} (magnitud) del embedding se relaciona con la dificultad
#'   \item La \strong{unicidad semantica} predice la discriminacion
#' }
#'
#' La informacion del item se estima como:
#' \deqn{I(\theta) \propto a^2 \cdot p(1-p)}
#'
#' Donde se usan los proxies semanticos para a (discriminacion) y
#' se asume p = 0.5 para simplificar.
#'
#' NOTA: Estas son estimaciones basadas en propiedades semanticas.
#' Para parametros IRT precisos, se requieren datos de respuesta reales.
#'
#' @examples
#' \dontrun{
#' # Predecir parametros IRT
#' irt <- predecir_irt(mi_escala)
#'
#' # Ver items con mayor informacion estimada
#' irt[order(-irt$informacion_estimada), ]
#' }
#'
#' @references
#' Yao, L. H., Jarvis, N., Zhan, T., Ghosh, S., Liu, L., & Jiang, T. (2025).
#' JE-IRT: A Geometric Lens on LLM Abilities through Joint Embedding Item
#' Response Theory. arXiv:2509.22888.
#'
#' Huang, J., et al. (2025). Learning Compact Representations of LLM Abilities
#' via Item Response Theory. arXiv:2510.00844.
#'
#' @keywords internal
predecir_irt <- function(x, verbose = TRUE) {

  # Extraer embeddings y similitud
  if (inherits(x, "semilla") || inherits(x, "semilla_embeddings")) {
    if (is.null(x$embeddings)) {
      stop("El objeto no tiene embeddings. Ejecuta obtener_embeddings() primero.")
    }
    embeddings <- x$embeddings
    similitud <- x$similitud
    items_df <- x$items
  } else if (is.list(x) && !is.null(x$embeddings)) {
    embeddings <- x$embeddings
    similitud <- x$similitud
    items_df <- x$items
  } else {
    stop("Objeto no valido. Usa un objeto semilla o lista con $embeddings.")
  }

  n_items <- nrow(embeddings)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("PREDICCION DE PARAMETROS IRT"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Analizando ", n_items, " items...\n\n", sep = "")
  }

  # 1. Dificultad: basada en la norma del embedding
  # Embeddings con mayor norma tienden a representar conceptos mas complejos
  normas <- apply(embeddings, 1, function(e) sqrt(sum(e^2)))

  # Normalizar a escala tipica IRT (-3 a 3)
  normas_z <- scale(normas)[, 1]
  dificultad_estimada <- normas_z * 1.5  # Escalar a rango IRT tipico

  # 2. Discriminacion: basada en unicidad semantica
  # Items unicos discriminan mejor
  dimensiones <- items_df$dimension
  similitud_media <- numeric(n_items)

  for (i in 1:n_items) {
    idx_dim <- which(dimensiones == dimensiones[i])
    idx_dim <- idx_dim[idx_dim != i]
    if (length(idx_dim) > 0) {
      similitud_media[i] <- mean(similitud[i, idx_dim])
    }
  }

  unicidad <- 1 - similitud_media
  # Escalar a rango IRT tipico de discriminacion (0.5 a 2.5)
  discriminacion_estimada <- 0.5 + (unicidad / max(unicidad)) * 2

  # 3. Informacion: I = a^2 * p * (1-p), asumiendo p = 0.5
  # I_max = a^2 * 0.25
  informacion_estimada <- discriminacion_estimada^2 * 0.25

  # Normalizar informacion
  informacion_estimada <- informacion_estimada / max(informacion_estimada)

  # Construir dataframe
  resultado <- data.frame(
    numero = 1:n_items,
    dimension = items_df$dimension,
    item = items_df$item,
    dificultad_estimada = round(dificultad_estimada, 3),
    discriminacion_estimada = round(discriminacion_estimada, 3),
    informacion_estimada = round(informacion_estimada, 3),
    stringsAsFactors = FALSE
  )

  # Categorizar
  resultado$dificultad_cat <- cut(
    dificultad_estimada,
    breaks = c(-Inf, -1, 1, Inf),
    labels = c("facil", "medio", "dificil")
  )

  resultado$discriminacion_cat <- cut(
    discriminacion_estimada,
    breaks = c(-Inf, 1, 1.5, Inf),
    labels = c("baja", "media", "alta")
  )

  class(resultado) <- c("semilla_irt", "data.frame")

  if (verbose) {
    cat(.color_flecha(), " Distribucion de dificultad estimada:\n", sep = "")
    tabla_dif <- table(resultado$dificultad_cat)
    cat("    Facil: ", tabla_dif["facil"], " | Medio: ", tabla_dif["medio"],
        " | Dificil: ", tabla_dif["dificil"], "\n\n", sep = "")

    cat(.color_flecha(), " Distribucion de discriminacion estimada:\n", sep = "")
    tabla_disc <- table(resultado$discriminacion_cat)
    cat("    Baja: ", tabla_disc["baja"], " | Media: ", tabla_disc["media"],
        " | Alta: ", tabla_disc["alta"], "\n\n", sep = "")

    cat(.color_flecha(), " Top 5 items con mayor informacion:\n\n", sep = "")
    top5 <- resultado[order(-resultado$informacion_estimada), ][1:min(5, n_items), ]
    for (i in 1:nrow(top5)) {
      cat("  [", top5$numero[i], "] b=", sprintf("%+.2f", top5$dificultad_estimada[i]),
          " a=", sprintf("%.2f", top5$discriminacion_estimada[i]),
          " I=", sprintf("%.2f", top5$informacion_estimada[i]),
          " | ", substr(top5$item[i], 1, 35), "...\n", sep = "")
    }

    cat("\n")
    cat(.linea("-"), "\n")
    cat("NOTA: Estimaciones semanticas. Para IRT preciso,\n")
    cat("se requieren datos de respuesta reales.\n")
    cat("Ref: Yao et al. (2025) JE-IRT arXiv:2509.22888\n")
    cat(.linea("="), "\n\n")
  }

  return(resultado)
}


# =============================================================================
# COMPARAR ESCALAS - VERIFICAR REPRODUCIBILIDAD
# =============================================================================

#' @title Comparar Escalas
#'
#' @description
#' Compara dos escalas generadas para verificar reproducibilidad.
#' Util para confirmar que el uso de la misma semilla produce items identicos
#' o para analizar diferencias entre versiones.
#'
#' @param escala1 Primera escala (objeto semilla o lista con $items)
#' @param escala2 Segunda escala (objeto semilla o lista con $items)
#' @param metodo Metodo de comparacion: "exacto" (texto identico),
#'        "semantico" (similitud coseno), "ambos" (default)
#' @param umbral_similitud Umbral para considerar items similares (default: 0.90)
#' @param verbose Mostrar resultados en consola
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{identicos}: Proporcion de items exactamente iguales
#'   \item \code{similares}: Proporcion de items con similitud >= umbral
#'   \item \code{concordancia}: Dataframe con comparacion item por item
#'   \item \code{resumen}: Texto resumen de la comparacion
#' }
#'
#' @examples
#' \dontrun{
#' # Generar dos escalas con la misma semilla
#' escala1 <- semilla("autoeficacia", api_key, seed = 2024)
#' escala2 <- semilla("autoeficacia", api_key, seed = 2024)
#'
#' # Comparar
#' comp <- comparar_escalas(escala1, escala2)
#' print(comp)
#'
#' # Comparar con semillas diferentes
#' escala3 <- semilla("autoeficacia", api_key, seed = 12345)
#' comp2 <- comparar_escalas(escala1, escala3)
#' }
#'
#' @export
comparar_escalas <- function(escala1,
                              escala2,
                              metodo = "ambos",
                              umbral_similitud = 0.90,
                              verbose = TRUE) {

  # Extraer items de cada escala
  items1 <- .extraer_items_df(escala1)
  items2 <- .extraer_items_df(escala2)

  n1 <- nrow(items1)
  n2 <- nrow(items2)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("COMPARACION DE ESCALAS"), "\n")
    cat(.linea("="), "\n\n")
    cat("  Escala 1: ", n1, " items\n", sep = "")
    cat("  Escala 2: ", n2, " items\n", sep = "")
    cat("  Metodo: ", metodo, "\n", sep = "")
    cat("  Umbral similitud: ", umbral_similitud, "\n\n", sep = "")
  }

  # Comparacion exacta
  n_comparar <- min(n1, n2)
  items1_texto <- tolower(trimws(items1$item[1:n_comparar]))
  items2_texto <- tolower(trimws(items2$item[1:n_comparar]))

  # Items identicos (ignorando mayusculas y espacios)
  identicos <- items1_texto == items2_texto
  n_identicos <- sum(identicos)
  prop_identicos <- n_identicos / n_comparar

  # Crear dataframe de concordancia
  concordancia <- data.frame(
    numero = 1:n_comparar,
    item_escala1 = items1$item[1:n_comparar],
    item_escala2 = items2$item[1:n_comparar],
    identico = identicos,
    stringsAsFactors = FALSE
  )

  # Comparacion semantica (si hay embeddings o si se solicita)
  similitud_semantica <- NULL
  n_similares <- NA
  prop_similares <- NA

  if (metodo %in% c("semantico", "ambos")) {
    # Calcular similitud semantica usando distancia de Levenshtein normalizada
    # (aproximacion sin necesidad de API)
    similitud_semantica <- sapply(1:n_comparar, function(i) {
      .similitud_texto(items1_texto[i], items2_texto[i])
    })

    concordancia$similitud <- round(similitud_semantica, 3)
    concordancia$similar <- similitud_semantica >= umbral_similitud

    n_similares <- sum(concordancia$similar)
    prop_similares <- n_similares / n_comparar
  }

  # Determinar estado de reproducibilidad
  if (prop_identicos == 1) {
    estado <- "PERFECTA"
    mensaje <- "Las escalas son identicas. La semilla funciono correctamente."
  } else if (prop_identicos >= 0.9) {
    estado <- "MUY ALTA"
    mensaje <- paste0("El ", round(prop_identicos * 100, 1), "% de los items son identicos.")
  } else if (!is.na(prop_similares) && prop_similares >= 0.8) {
    estado <- "ALTA (SEMANTICA)"
    mensaje <- paste0("Items no identicos pero ", round(prop_similares * 100, 1), "% semanticamente similares.")
  } else if (prop_identicos >= 0.5) {
    estado <- "MODERADA"
    mensaje <- paste0("Solo ", round(prop_identicos * 100, 1), "% de items identicos. Revisar configuracion.")
  } else {
    estado <- "BAJA"
    mensaje <- "Las escalas son muy diferentes. Posiblemente se usaron semillas distintas."
  }

  if (verbose) {
    cat(.linea("-"), "\n")
    cat(.color_verde("RESULTADOS:"), "\n\n")

    cat("  Items comparados: ", n_comparar, "\n", sep = "")
    cat("  Items identicos: ", n_identicos, " (", round(prop_identicos * 100, 1), "%)\n", sep = "")

    if (!is.na(prop_similares)) {
      cat("  Items similares (>=", umbral_similitud, "): ", n_similares,
          " (", round(prop_similares * 100, 1), "%)\n", sep = "")
    }

    cat("\n  REPRODUCIBILIDAD: ", estado, "\n", sep = "")
    cat("  ", mensaje, "\n", sep = "")

    # Mostrar items diferentes
    if (n_identicos < n_comparar && n_identicos > 0) {
      cat("\n", .linea("-"), "\n", sep = "")
      cat("ITEMS DIFERENTES:\n\n")

      diferentes <- which(!identicos)
      for (i in diferentes[1:min(5, length(diferentes))]) {
        cat("  [", i, "] Escala 1: ", substr(concordancia$item_escala1[i], 1, 50), "...\n", sep = "")
        cat("       Escala 2: ", substr(concordancia$item_escala2[i], 1, 50), "...\n", sep = "")
        if (!is.null(similitud_semantica)) {
          cat("       Similitud: ", round(similitud_semantica[i], 3), "\n", sep = "")
        }
        cat("\n")
      }
      if (length(diferentes) > 5) {
        cat("  ... y ", length(diferentes) - 5, " items diferentes mas\n", sep = "")
      }
    }

    cat("\n", .linea("="), "\n\n", sep = "")
  }

  # Resultado
  resultado <- list(
    n_escala1 = n1,
    n_escala2 = n2,
    n_comparados = n_comparar,
    identicos = prop_identicos,
    n_identicos = n_identicos,
    similares = prop_similares,
    n_similares = n_similares,
    estado = estado,
    mensaje = mensaje,
    concordancia = concordancia,
    umbral = umbral_similitud
  )

  class(resultado) <- c("semilla_comparacion", "list")
  return(resultado)
}


#' @keywords internal
.extraer_items_df <- function(x) {
  if (inherits(x, "semilla") || inherits(x, "semilla_items")) {
    return(x$items)
  } else if (is.list(x) && !is.null(x$items)) {
    return(x$items)
  } else if (is.data.frame(x) && "item" %in% names(x)) {
    return(x)
  } else {
    stop("No se pudo extraer items del objeto.")
  }
}


#' @keywords internal
.similitud_texto <- function(texto1, texto2) {
  # Similitud basada en distancia de Levenshtein normalizada
  # 1 = identicos, 0 = completamente diferentes

  if (texto1 == texto2) return(1)

  # Calcular distancia de Levenshtein
  n1 <- nchar(texto1)
  n2 <- nchar(texto2)

  if (n1 == 0 || n2 == 0) return(0)

  # Usar adist() para distancia de edicion
  dist <- adist(texto1, texto2)[1, 1]

  # Normalizar por longitud maxima
  similitud <- 1 - (dist / max(n1, n2))

  return(max(0, similitud))
}


#' @export
print.semilla_comparacion <- function(x, ...) {
  cat("\n")
  cat(.linea("="), "\n")
  cat(.color_verde("COMPARACION DE ESCALAS"), "\n")
  cat(.linea("="), "\n\n")

  cat("  Reproducibilidad: ", x$estado, "\n", sep = "")
  cat("  Items identicos: ", x$n_identicos, "/", x$n_comparados,
      " (", round(x$identicos * 100, 1), "%)\n", sep = "")

  if (!is.na(x$similares)) {
    cat("  Items similares: ", x$n_similares, "/", x$n_comparados,
        " (", round(x$similares * 100, 1), "%)\n", sep = "")
  }

  cat("\n  ", x$mensaje, "\n", sep = "")
  cat(.linea("="), "\n\n")

  invisible(x)
}


# =============================================================================
# FUNCIONES INTERNAS - NUEVAS
# =============================================================================

#' @keywords internal
.evaluar_item_calidad_llm <- function(openai, item, dimension, concepto, criterios, modelo) {

  criterios_desc <- list(
    claridad = "Redaccion clara, sin ambiguedades ni terminos confusos",
    simplicidad = "Una sola idea por item, sin oraciones compuestas",
    relevancia = "Pertinencia directa al constructo que se mide",
    especificidad = "Precision y concrecion, no vaguedad",
    neutralidad = "Sin sesgo, carga emocional excesiva o direccion de respuesta"
  )

  criterios_texto <- paste(sapply(criterios, function(c) {
    paste0("- ", toupper(c), ": ", criterios_desc[[c]])
  }), collapse = "\n")

  dim_texto <- ifelse(is.na(dimension), "No especificada", dimension)
  criterios_json <- paste(sprintf('"%s": <1-5>', criterios), collapse = ", ")

  prompt <- paste0(
    "Eres un experto en psicometria evaluando la calidad de un item para una escala psicologica.\n\n",
    "CONSTRUCTO: ", concepto, "\n",
    "DIMENSION: ", dim_texto, "\n",
    "ITEM: \"", item, "\"\n\n",
    "Evalua el item en los siguientes criterios (escala 1-5, donde 1=muy pobre, 5=excelente):\n\n",
    criterios_texto, "\n\n",
    "Responde SOLO con un JSON:\n",
    "{\n  ", criterios_json, ",\n",
    "  \"recomendacion\": \"Una sugerencia breve de mejora o Item adecuado si cumple criterios\"\n",
    "}"
  )

  respuesta <- tryCatch({
    openai$chat$completions$create(
      model = modelo,
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3
    )
  }, error = function(e) NULL)

  if (is.null(respuesta)) {
    puntuaciones <- setNames(rep(3, length(criterios)), criterios)
    return(list(puntuaciones = puntuaciones, recomendacion = "Error en evaluacion"))
  }

  texto <- respuesta$choices[[1]]$message$content

  tryCatch({
    texto_limpio <- gsub("```json|```", "", texto)
    texto_limpio <- trimws(texto_limpio)
    resultado <- jsonlite::fromJSON(texto_limpio)

    puntuaciones <- setNames(
      sapply(criterios, function(c) as.numeric(resultado[[c]])),
      criterios
    )
    puntuaciones[is.na(puntuaciones)] <- 3

    list(
      puntuaciones = puntuaciones,
      recomendacion = resultado$recomendacion
    )
  }, error = function(e) {
    list(
      puntuaciones = setNames(rep(3, length(criterios)), criterios),
      recomendacion = "Error parseando respuesta"
    )
  })
}


#' @keywords internal
.evaluar_item_reglas <- function(item) {

  # Contar palabras
  palabras <- strsplit(item, "\\s+")[[1]]
  n_palabras <- length(palabras)

  # Regla 1: Longitud ideal (10-25 palabras)
  longitud_ok <- n_palabras >= 8 && n_palabras <= 30

  # Regla 2: Detectar doble negacion
  patron_doble_neg <- "no\\s+.*(no|nunca|nadie|nada|ninguno)|nunca\\s+.*no"
  doble_negacion <- grepl(patron_doble_neg, tolower(item), perl = TRUE)

  # Regla 3: Detectar palabras absolutas
  palabras_abs <- c("siempre", "nunca", "todos", "nadie", "completamente",
                    "absolutamente", "totalmente", "jamas")
  patron_abs <- paste(palabras_abs, collapse = "|")
  palabras_absolutas <- grepl(patron_abs, tolower(item))

  # Penalizacion basada en reglas (escala 1-5)
  penalizacion <- 5
  if (!longitud_ok) penalizacion <- penalizacion - 1
  if (doble_negacion) penalizacion <- penalizacion - 1.5
  if (palabras_absolutas) penalizacion <- penalizacion - 0.5

  list(
    n_palabras = n_palabras,
    longitud_ok = longitud_ok,
    doble_negacion = doble_negacion,
    palabras_absolutas = palabras_absolutas,
    penalizacion_ajustada = max(1, penalizacion)
  )
}


# =============================================================================
# ANALISIS DE COHERENCIA
# =============================================================================
# NOTA: analizar_redundancia() esta definida en germinar.R

#' @title Analizar Coherencia Intra vs Inter Dimension
#'
#' @description
#' Compara la similitud semantica entre items de la misma dimension (intra)
#' con la similitud entre items de diferentes dimensiones (inter).
#' Una escala bien construida debe mostrar mayor similitud intra que inter.
#'
#' @param x Objeto semilla con embeddings y similitud calculada
#' @param umbral_coherencia_min Umbral minimo de coherencia aceptable (default: 0.50)
#' @param verbose Mostrar progreso en consola (default: TRUE)
#'
#' @return Lista de clase 'semilla_coherencia' con:
#' \itemize{
#'   \item \code{similitud_intra}: Vector de similitudes dentro de dimensiones
#'   \item \code{similitud_inter}: Vector de similitudes entre dimensiones
#'   \item \code{mediana_intra}: Mediana de similitud intra-dimension
#'   \item \code{mediana_inter}: Mediana de similitud inter-dimension
#'   \item \code{diferencia_separabilidad}: Diferencia entre medianas (> 0.15 es bueno)
#'   \item \code{coherencia_por_dimension}: Estadisticas por dimension
#'   \item \code{items_baja_coherencia}: Items con coherencia < umbral
#' }
#'
#' @examples
#' \dontrun{
#' coh <- analizar_coherencia(mi_escala)
#' print(coh$diferencia_separabilidad)
#' }
#'
#' @export
analizar_coherencia <- function(x, umbral_coherencia_min = 0.50, verbose = TRUE) {

  # Validar entrada
  if (!inherits(x, "semilla")) {
    stop("x debe ser un objeto de clase 'semilla' con similitud calculada")
  }

  if (is.null(x$similitud)) {
    stop("El objeto no tiene matriz de similitud. Ejecuta primero obtener_embeddings()")
  }

  matriz_sim <- x$similitud
  items_df <- x$items
  n_items <- nrow(items_df)

  # Usar 'codigo' si existe, sino usar 'numero'
  if (!"codigo" %in% names(items_df)) {
    items_df$codigo <- paste0("Item_", items_df$numero)
  }

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("ANALISIS DE COHERENCIA INTRA VS INTER"), "\n")
    cat(.linea("-"), "\n\n")
    cat("  Umbral minimo de coherencia: ", umbral_coherencia_min, "\n", sep = "")
    cat("  Items analizados: ", n_items, "\n\n", sep = "")
  }

  # Calcular similitudes intra e inter
  similitud_intra <- c()
  similitud_inter <- c()

  for (i in 1:(n_items - 1)) {
    for (j in (i + 1):n_items) {
      sim <- matriz_sim[i, j]

      if (items_df$dimension[i] == items_df$dimension[j]) {
        similitud_intra <- c(similitud_intra, sim)
      } else {
        similitud_inter <- c(similitud_inter, sim)
      }
    }
  }

  # Estadisticas generales
  mediana_intra <- median(similitud_intra, na.rm = TRUE)
  mediana_inter <- median(similitud_inter, na.rm = TRUE)
  diferencia <- mediana_intra - mediana_inter

  media_intra <- mean(similitud_intra, na.rm = TRUE)
  media_inter <- mean(similitud_inter, na.rm = TRUE)

  # Coherencia por dimension
  dimensiones <- unique(items_df$dimension)
  coherencia_por_dim <- data.frame(
    Dimension = character(),
    N_Items = integer(),
    Similitud_Media = numeric(),
    Similitud_Min = numeric(),
    Similitud_Max = numeric(),
    Items_Baja_Coherencia = integer(),
    stringsAsFactors = FALSE
  )

  items_baja_coherencia <- data.frame(
    Codigo = character(),
    Item = character(),
    Dimension = character(),
    Similitud_Media_Dimension = numeric(),
    stringsAsFactors = FALSE
  )

  # Dataframe para datos por item (para violin plot)
  datos_por_item <- data.frame(
    Dimension = character(),
    Codigo = character(),
    Similitud_Media = numeric(),
    stringsAsFactors = FALSE
  )

  for (dim in dimensiones) {
    idx_dim <- which(items_df$dimension == dim)
    n_dim <- length(idx_dim)

    if (n_dim > 1) {
      # Calcular similitud media de cada item con su dimension
      sims_dim <- c()
      for (i in idx_dim) {
        otros_idx <- setdiff(idx_dim, i)
        sim_media_item <- mean(matriz_sim[i, otros_idx], na.rm = TRUE)
        sims_dim <- c(sims_dim, sim_media_item)

        # Guardar datos por item para violin plot
        datos_por_item <- rbind(datos_por_item, data.frame(
          Dimension = dim,
          Codigo = items_df$codigo[i],
          Similitud_Media = sim_media_item,
          stringsAsFactors = FALSE
        ))

        # Detectar items de baja coherencia
        if (sim_media_item < umbral_coherencia_min) {
          items_baja_coherencia <- rbind(items_baja_coherencia, data.frame(
            Codigo = items_df$codigo[i],
            Item = items_df$item[i],
            Dimension = dim,
            Similitud_Media_Dimension = round(sim_media_item, 3),
            stringsAsFactors = FALSE
          ))
        }
      }

      coherencia_por_dim <- rbind(coherencia_por_dim, data.frame(
        Dimension = dim,
        N_Items = n_dim,
        Similitud_Media = round(mean(sims_dim, na.rm = TRUE), 3),
        Similitud_Min = round(min(sims_dim, na.rm = TRUE), 3),
        Similitud_Max = round(max(sims_dim, na.rm = TRUE), 3),
        Items_Baja_Coherencia = sum(sims_dim < umbral_coherencia_min),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Evaluacion
  evaluacion <- ifelse(diferencia >= 0.20, "Excelente",
                       ifelse(diferencia >= 0.15, "Buena",
                              ifelse(diferencia >= 0.10, "Aceptable", "Baja")))

  if (verbose) {
    cat("  RESULTADOS:\n\n")
    cat("  Similitud INTRA-dimension:\n")
    cat("    - Mediana: ", sprintf("%.3f", mediana_intra), "\n", sep = "")
    cat("    - Media:   ", sprintf("%.3f", media_intra), "\n\n", sep = "")

    cat("  Similitud INTER-dimension:\n")
    cat("    - Mediana: ", sprintf("%.3f", mediana_inter), "\n", sep = "")
    cat("    - Media:   ", sprintf("%.3f", media_inter), "\n\n", sep = "")

    cat("  DIFERENCIA DE SEPARABILIDAD: ", sprintf("%.3f", diferencia), "\n", sep = "")
    cat("  Evaluacion: ", evaluacion, "\n\n", sep = "")

    cat("  Criterios:\n")
    cat("    >= 0.20: Excelente\n")
    cat("    >= 0.15: Buena\n")
    cat("    >= 0.10: Aceptable\n")
    cat("    <  0.10: Baja\n\n")

    if (nrow(items_baja_coherencia) > 0) {
      cat("  ", .color_warning(), " Items con baja coherencia (< ", umbral_coherencia_min, "):\n", sep = "")
      for (i in 1:nrow(items_baja_coherencia)) {
        cat("    - ", items_baja_coherencia$Codigo[i], " (",
            sprintf("%.3f", items_baja_coherencia$Similitud_Media_Dimension[i]), ")\n", sep = "")
      }
      cat("\n")
    } else {
      cat("  ", .color_check(), " Todos los items tienen coherencia adecuada.\n\n", sep = "")
    }
  }

  resultado <- list(
    similitud_intra = similitud_intra,
    similitud_inter = similitud_inter,
    mediana_intra = mediana_intra,
    mediana_inter = mediana_inter,
    media_intra = media_intra,
    media_inter = media_inter,
    diferencia_separabilidad = diferencia,
    evaluacion = evaluacion,
    coherencia_por_dimension = coherencia_por_dim,
    items_baja_coherencia = items_baja_coherencia,
    datos_por_item = datos_por_item,
    umbral = umbral_coherencia_min
  )

  class(resultado) <- c("semilla_coherencia", "list")
  return(resultado)
}


# =============================================================================
# PRECISION DE CLASIFICACION
# =============================================================================

#' @title Calcular Precision de Clasificacion Semantica
#'
#' @description
#' Compara la estructura teorica (dimensiones definidas) con la estructura
#' empirica (clusters semanticos). Calcula el porcentaje de items que se
#' agrupan correctamente segun su dimension teorica.
#'
#' Cuando \code{metodo = "ensemble"} se ejecutan multiples clusterizadores
#' y se reporta el consenso de asignacion por item (clustering consensus).
#' Este enfoque incrementa la varianza explicada en la prediccion de cargas
#' factoriales, segun Voss, Wu, Javalagi y Kell (2026).
#'
#' Voss et al. (2026) emplearon 9 algoritmos x 10 replicas (90 particiones).
#' SeMiLLa por defecto usa una version mas ligera con 3 algoritmos diversos
#' (uno de cada familia filosofica: centroide, jerarquico y modelo) x
#' 10 replicas (30 particiones). El usuario puede ampliar a los 9 originales
#' via el argumento \code{algoritmos}.
#'
#' @param x Objeto semilla con embeddings calculados
#' @param n_clusters Numero de clusters (default: numero de dimensiones)
#' @param metodo Metodo de clustering: "kmeans" (default), "jerarquico" o
#'   "ensemble" (consenso de varios algoritmos)
#' @param algoritmos Vector de algoritmos para el ensemble. Solo se usa
#'   cuando \code{metodo = "ensemble"}. Default:
#'   \code{c("kmeans", "ward", "gmm")} (3 familias filosoficas).
#'   Opciones: \code{"kmeans"} (centroide), \code{"ward"} (jerarquico),
#'   \code{"pam"} (centroide-medoide), \code{"diana"} (jerarquico divisivo),
#'   \code{"gmm"} (modelo gaussiano), \code{"spectral"} (grafo),
#'   \code{"affinity"} (densidad), \code{"som"} (red neural autoorganizada).
#'   Para replica fiel de Voss et al. (2026), usar todos los 8 disponibles
#'   (el noveno, latent block, no aplica a clustering de items individuales).
#' @param n_replicas Numero de replicas por algoritmo (default: 10, como
#'   Voss et al., 2026). Las replicas usan submuestras aleatorias del 90 por ciento
#'   de los items para evaluar robustez. Solo aplica con \code{metodo = "ensemble"}.
#' @param verbose Mostrar progreso en consola (default: TRUE)
#'
#' @return Lista de clase 'semilla_precision' con:
#' \itemize{
#'   \item \code{precision_global}: Porcentaje global de clasificacion correcta
#'   \item \code{precision_por_dimension}: Precision por cada dimension
#'   \item \code{matriz_confusion}: Tabla cruzada dimension vs cluster
#'   \item \code{items_mal_clasificados}: Items que no agrupan con su dimension
#'   \item \code{asignacion_clusters}: Cluster asignado a cada item (incluye silhouette)
#'   \item \code{ari}: Adjusted Rand Index
#'   \item \code{silhouette}: Silhouette score promedio (calidad intrinseca del clustering)
#'   \item \code{silhouette_por_item}: Silhouette de cada item (negativos = mal ubicados)
#'   \item \code{silhouette_por_cluster}: Silhouette promedio por cluster
#'   \item \code{items_silhouette_negativo}: Items con silhouette negativo (revisar)
#' }
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' print(prec$precision_global)
#' print(prec$precision_por_dimension)
#'
#' # Consenso ensemble (Voss et al., 2026)
#' prec_ens <- precision_clasificacion(mi_escala, metodo = "ensemble")
#' prec_ens$consenso  # frecuencia con la que cada item agrupa con su dimension
#' }
#'
#' @references
#' Voss, N. M., Wu, F. Y., Javalagi, A. A., & Kell, H. J. (2026).
#' Integrating ensemble clustering and text embeddings for estimating
#' the factor loadings of self-report scales. \emph{Educational and
#' Psychological Measurement}. \doi{10.1177/00131644261430762}
#'
#' @export
precision_clasificacion <- function(x, n_clusters = NULL, metodo = "kmeans",
                                    algoritmos = c("kmeans", "ward", "gmm"),
                                    n_replicas = 10,
                                    verbose = TRUE) {

  # Validar entrada
  if (!inherits(x, "semilla")) {
    stop("x debe ser un objeto de clase 'semilla' con embeddings calculados")
  }

  if (is.null(x$embeddings)) {
    stop("El objeto no tiene embeddings. Ejecuta primero obtener_embeddings()")
  }

  metodo <- match.arg(metodo, c("kmeans", "jerarquico", "ensemble"))
  alg_validos <- c("kmeans","ward","jerarquico","pam","diana","gmm",
                    "spectral","affinity","som")
  if (!all(algoritmos %in% alg_validos)) {
    stop("Algoritmos no validos: ",
         paste(setdiff(algoritmos, alg_validos), collapse = ", "),
         ". Validos: ", paste(alg_validos, collapse = ", "))
  }
  # Mapear "jerarquico" a "ward" (alias retrocompatible)
  algoritmos <- ifelse(algoritmos == "jerarquico", "ward", algoritmos)
  algoritmos <- unique(algoritmos)
  if (n_replicas < 1) stop("n_replicas debe ser >= 1")

  embeddings <- x$embeddings
  items_df <- x$items
  matriz_sim <- x$similitud

  # Usar 'codigo' si existe, sino usar 'numero'
  if (!"codigo" %in% names(items_df)) {
    items_df$codigo <- paste0("Item_", items_df$numero)
  }

  dimensiones <- unique(items_df$dimension)
  n_dims <- length(dimensiones)

  if (is.null(n_clusters)) {
    n_clusters <- n_dims
  }

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("PRECISION DE CLASIFICACION SEMANTICA"), "\n")
    cat(.linea("-"), "\n\n")
    cat("  Dimensiones teoricas: ", n_dims, "\n", sep = "")
    cat("  Clusters a formar: ", n_clusters, "\n", sep = "")
    cat("  Metodo: ", metodo, "\n\n", sep = "")
  }

  # Helper: ejecutar un clusterizador concreto
  # idx_subset: indices de items a usar (para replicas con submuestreo).
  #             NULL = todos los items.
  .clusterizar <- function(metodo_local, idx_subset = NULL, seed = 2024) {
    Xemb <- if (is.null(idx_subset)) embeddings else embeddings[idx_subset, , drop = FALSE]
    Rsim <- if (is.null(idx_subset)) matriz_sim
            else matriz_sim[idx_subset, idx_subset, drop = FALSE]
    set.seed(seed)
    out <- tryCatch({
      if (metodo_local == "kmeans") {
        kmeans(Xemb, centers = n_clusters, nstart = 25)$cluster
      } else if (metodo_local == "ward" || metodo_local == "jerarquico") {
        hc <- hclust(as.dist(1 - Rsim), method = "ward.D2")
        cutree(hc, k = n_clusters)
      } else if (metodo_local == "pam") {
        if (!requireNamespace("cluster", quietly = TRUE)) return(NULL)
        cluster::pam(as.dist(1 - Rsim), k = n_clusters, diss = TRUE)$clustering
      } else if (metodo_local == "diana") {
        if (!requireNamespace("cluster", quietly = TRUE)) return(NULL)
        cutree(as.hclust(cluster::diana(as.dist(1 - Rsim))), k = n_clusters)
      } else if (metodo_local == "gmm") {
        if (!requireNamespace("mclust", quietly = TRUE)) return(NULL)
        # Mclust con modelNames = "EII" (esferico) escala bien a 1536 dims.
        # Modelos mas complejos (VVV) requieren n > p, no aplica aqui.
        res <- mclust::Mclust(Xemb, G = n_clusters, modelNames = "EII", verbose = FALSE)
        if (is.null(res)) NULL else as.integer(res$classification)
      } else if (metodo_local == "spectral") {
        if (!requireNamespace("kernlab", quietly = TRUE)) return(NULL)
        as.integer(kernlab::specc(Xemb, centers = n_clusters))
      } else if (metodo_local == "affinity") {
        if (!requireNamespace("apcluster", quietly = TRUE)) return(NULL)
        ap <- apcluster::apcluster(apcluster::negDistMat(r = 2), Xemb)
        labs <- integer(nrow(Xemb))
        for (k in seq_along(ap@clusters)) labs[ap@clusters[[k]]] <- k
        # Forzar n_clusters via aggExCluster si la APC produjo otro numero
        if (length(unique(labs)) != n_clusters) {
          ag <- apcluster::aggExCluster(apcluster::negDistMat(r = 2), Xemb)
          labs <- integer(nrow(Xemb))
          cuts <- apcluster::cutree(ag, k = n_clusters)
          for (k in seq_along(cuts@clusters)) labs[cuts@clusters[[k]]] <- k
        }
        labs
      } else if (metodo_local == "som") {
        if (!requireNamespace("kohonen", quietly = TRUE)) return(NULL)
        # Grid simple: x = n_clusters, y = 1; clustering directo sobre nodos
        grid <- kohonen::somgrid(xdim = n_clusters, ydim = 1, topo = "rectangular")
        som_res <- kohonen::som(Xemb, grid = grid, rlen = 200)
        as.integer(som_res$unit.classif)
      } else NULL
    }, error = function(e) {
      if (verbose) message("  [", metodo_local, "] ", conditionMessage(e))
      NULL
    })
    out
  }

  # Helper: alinear etiquetas via Hungarian-like greedy matching contra una
  # particion de referencia (necesario para hacer voto mayoritario).
  .alinear_labels <- function(cand, ref) {
    tab <- table(cand, ref)
    mapeo <- integer(max(cand))
    for (i in seq_len(min(nrow(tab), ncol(tab)))) {
      idx_max <- which(tab == max(tab), arr.ind = TRUE)[1, ]
      mapeo[as.integer(rownames(tab)[idx_max[1]])] <- as.integer(colnames(tab)[idx_max[2]])
      tab[idx_max[1], ] <- -1
      tab[, idx_max[2]] <- -1
    }
    mapeo[cand]
  }

  # Realizar clustering (single method o ensemble)
  consenso_df <- NULL
  clusters_componentes <- NULL

  if (metodo == "ensemble") {

    n_items <- nrow(items_df)
    # Particion de referencia: kmeans con todos los items (para alinear etiquetas)
    ref_full <- .clusterizar("kmeans", idx_subset = NULL, seed = 2024)

    # Generar las particiones (algoritmo x replica)
    particiones <- list()      # cada particion: vector de clusters armonizados,
                                # extendido a longitud n_items con NA donde no se evaluo
    componentes_por_alg <- list()

    if (verbose) {
      n_total <- length(algoritmos) * n_replicas
      cat("  Algoritmos: ", paste(algoritmos, collapse = ", "), "\n", sep = "")
      cat("  Replicas: ", n_replicas, " | particiones totales: ", n_total, "\n\n", sep = "")
    }

    for (alg in algoritmos) {
      reps_alg <- list()
      for (r in seq_len(n_replicas)) {
        # Replica r: con r=1 se usan todos los items; con r>1 se hace
        # submuestreo aleatorio del 90% para evaluar robustez (Voss et al.)
        if (n_replicas == 1 || r == 1) {
          idx <- seq_len(n_items)
        } else {
          set.seed(2024 + r)
          idx <- sort(sample(seq_len(n_items), size = round(0.9 * n_items)))
        }
        cl_part <- .clusterizar(alg, idx_subset = idx, seed = 2024 + r)
        if (is.null(cl_part)) next
        # Armonizar contra la particion de referencia (mapeo greedy)
        cl_arm <- .alinear_labels(cl_part, ref_full[idx])
        # Extender a tamanno completo con NA en items no evaluados
        cl_full <- rep(NA_integer_, n_items)
        cl_full[idx] <- cl_arm
        particiones[[length(particiones) + 1]] <- cl_full
        reps_alg[[length(reps_alg) + 1]] <- cl_full
      }
      componentes_por_alg[[alg]] <- reps_alg
    }

    if (length(particiones) == 0) {
      stop("Ningun algoritmo del ensemble logro generar particiones. ",
           "Revisa la disponibilidad de los paquetes opcionales (mclust, cluster, etc).")
    }
    clusters_componentes <- componentes_por_alg

    # Voto mayoritario sobre todas las particiones (ignorando NAs por replica)
    matriz_part <- do.call(cbind, particiones)
    clusters <- apply(matriz_part, 1, function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0) return(NA_integer_)
      tt <- table(v); as.integer(names(tt)[which.max(tt)])
    })

    # Consenso por item: proporcion de PARTICIONES en las que el item
    # cae en el cluster ganador de su dimension teorica.
    asignaciones_por_part <- lapply(particiones, function(cl) {
      tab <- table(items_df$dimension, cl, useNA = "no")
      apply(tab, 1, function(r) names(which.max(r)))
    })
    consenso_vec <- numeric(n_items)
    for (i in seq_len(n_items)) {
      dim_i <- items_df$dimension[i]
      coincide_count <- 0
      eval_count     <- 0
      for (j in seq_along(particiones)) {
        if (is.na(particiones[[j]][i])) next   # item no evaluado en esa replica
        eval_count <- eval_count + 1
        if (as.character(particiones[[j]][i]) ==
            as.character(asignaciones_por_part[[j]][[dim_i]])) {
          coincide_count <- coincide_count + 1
        }
      }
      consenso_vec[i] <- if (eval_count == 0) NA_real_ else coincide_count / eval_count
    }
    consenso_df <- data.frame(
      Codigo = items_df$codigo,
      Item = items_df$item,
      Dimension = items_df$dimension,
      Consenso = round(consenso_vec, 3),
      stringsAsFactors = FALSE
    )
  } else {
    clusters <- .clusterizar(metodo)
  }

  items_df$cluster <- paste0("Cluster_", clusters)

  # Matriz de confusion
  matriz_confusion <- table(
    Dimension = items_df$dimension,
    Cluster = items_df$cluster
  )

  # Funcion para calcular ARI
  calcular_ari <- function(etiquetas_verdaderas, etiquetas_predichas) {
    tabla <- table(etiquetas_verdaderas, etiquetas_predichas)
    n <- sum(tabla)

    sum_ni <- sum(choose(rowSums(tabla), 2))
    sum_nj <- sum(choose(colSums(tabla), 2))
    sum_nij <- sum(choose(tabla, 2))

    esperado <- sum_ni * sum_nj / choose(n, 2)
    maximo <- (sum_ni + sum_nj) / 2

    if (maximo == esperado) return(1)
    ari <- (sum_nij - esperado) / (maximo - esperado)
    return(ari)
  }

  # Asignacion optima de clusters a dimensiones
  asignar_optimo <- function(matriz) {
    asignacion <- list()
    matriz_temp <- as.matrix(matriz)
    dims_usadas <- c()
    clusters_usados <- c()

    for (iter in 1:min(nrow(matriz_temp), ncol(matriz_temp))) {
      max_val <- -1
      max_i <- -1
      max_j <- -1

      for (i in 1:nrow(matriz_temp)) {
        for (j in 1:ncol(matriz_temp)) {
          if (!(i %in% dims_usadas) && !(j %in% clusters_usados)) {
            if (matriz_temp[i, j] > max_val) {
              max_val <- matriz_temp[i, j]
              max_i <- i
              max_j <- j
            }
          }
        }
      }

      if (max_i > 0) {
        asignacion[[rownames(matriz_temp)[max_i]]] <- colnames(matriz_temp)[max_j]
        dims_usadas <- c(dims_usadas, max_i)
        clusters_usados <- c(clusters_usados, max_j)
      }
    }
    return(asignacion)
  }

  asignacion <- asignar_optimo(matriz_confusion)

  # Calcular precision por dimension
  precision_por_dim <- data.frame(
    Dimension = character(),
    N_Items = integer(),
    N_Correctos = integer(),
    Precision = numeric(),
    Cluster_Asignado = character(),
    stringsAsFactors = FALSE
  )

  items_mal_clasificados <- data.frame(
    Codigo = character(),
    Item = character(),
    Dimension_Teorica = character(),
    Cluster_Asignado = character(),
    Cluster_Esperado = character(),
    stringsAsFactors = FALSE
  )

  total_correctos <- 0
  total_items <- nrow(items_df)

  for (dim in dimensiones) {
    cluster_esperado <- asignacion[[dim]]
    idx_dim <- which(items_df$dimension == dim)
    n_dim <- length(idx_dim)

    if (!is.null(cluster_esperado)) {
      correctos <- sum(items_df$cluster[idx_dim] == cluster_esperado)
      precision <- correctos / n_dim * 100
      total_correctos <- total_correctos + correctos

      # Identificar mal clasificados
      mal_idx <- idx_dim[items_df$cluster[idx_dim] != cluster_esperado]
      for (mi in mal_idx) {
        items_mal_clasificados <- rbind(items_mal_clasificados, data.frame(
          Codigo = items_df$codigo[mi],
          Item = items_df$item[mi],
          Dimension_Teorica = dim,
          Cluster_Asignado = items_df$cluster[mi],
          Cluster_Esperado = cluster_esperado,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      correctos <- 0
      precision <- 0
      cluster_esperado <- "NA"
    }

    precision_por_dim <- rbind(precision_por_dim, data.frame(
      Dimension = dim,
      N_Items = n_dim,
      N_Correctos = correctos,
      Precision = round(precision, 1),
      Cluster_Asignado = cluster_esperado,
      stringsAsFactors = FALSE
    ))
  }

  precision_global <- total_correctos / total_items * 100

  # Calcular ARI
  dims_num <- as.numeric(factor(items_df$dimension))
  ari <- calcular_ari(dims_num, clusters)

  # Calcular Silhouette
  silhouette_resultado <- tryCatch({
    dist_matrix <- dist(embeddings)
    sil <- cluster::silhouette(clusters, dist_matrix)
    sil_valores <- sil[, 3]  # Columna con valores silhouette
    names(sil_valores) <- items_df$codigo
    list(
      promedio = mean(sil_valores),
      por_item = sil_valores,
      por_cluster = tapply(sil_valores, clusters, mean),
      items_negativos = names(sil_valores[sil_valores < 0])
    )
  }, error = function(e) {
    list(promedio = NA, por_item = NA, por_cluster = NA, items_negativos = character(0))
  })

  silhouette_promedio <- silhouette_resultado$promedio

  # Evaluacion
  eval_precision <- ifelse(precision_global >= 90, "Excelente",
                           ifelse(precision_global >= 80, "Buena",
                                  ifelse(precision_global >= 70, "Aceptable", "Baja")))

  eval_ari <- ifelse(ari >= 0.65, "Alta correspondencia",
                     ifelse(ari >= 0.35, "Correspondencia moderada", "Baja correspondencia"))

  eval_silhouette <- ifelse(is.na(silhouette_promedio), "No calculado",
                            ifelse(silhouette_promedio >= 0.7, "Estructura fuerte",
                                   ifelse(silhouette_promedio >= 0.5, "Estructura razonable",
                                          ifelse(silhouette_promedio >= 0.25, "Estructura debil", "Sin estructura clara"))))

  if (verbose) {
    cat("  RESULTADOS:\n\n")
    cat("  Precision global: ", sprintf("%.1f", precision_global), "%\n", sep = "")
    cat("  Evaluacion: ", eval_precision, "\n\n", sep = "")

    cat("  Adjusted Rand Index (ARI): ", sprintf("%.3f", ari), "\n", sep = "")
    cat("  Evaluacion ARI: ", eval_ari, "\n\n", sep = "")

    if (!is.na(silhouette_promedio)) {
      cat("  Silhouette Score: ", sprintf("%.3f", silhouette_promedio), "\n", sep = "")
      cat("  Evaluacion Silhouette: ", eval_silhouette, "\n", sep = "")
      if (length(silhouette_resultado$items_negativos) > 0) {
        cat("  ", .color_warning(), " Items con silhouette negativo: ",
            length(silhouette_resultado$items_negativos), "\n", sep = "")
      }
      cat("\n")
    }

    cat("  Precision por dimension:\n")
    for (i in 1:nrow(precision_por_dim)) {
      cat("    - ", precision_por_dim$Dimension[i], ": ",
          sprintf("%.1f", precision_por_dim$Precision[i]), "% (",
          precision_por_dim$N_Correctos[i], "/", precision_por_dim$N_Items[i], ")\n", sep = "")
    }
    cat("\n")

    if (nrow(items_mal_clasificados) > 0) {
      cat("  ", .color_warning(), " Items mal clasificados: ", nrow(items_mal_clasificados), "\n", sep = "")
      for (i in 1:min(5, nrow(items_mal_clasificados))) {
        cat("    - ", items_mal_clasificados$Codigo[i], ": ",
            items_mal_clasificados$Dimension_Teorica[i], " -> ",
            items_mal_clasificados$Cluster_Asignado[i], "\n", sep = "")
      }
      if (nrow(items_mal_clasificados) > 5) {
        cat("    ... y ", nrow(items_mal_clasificados) - 5, " mas\n", sep = "")
      }
    } else {
      cat("  ", .color_check(), " Todos los items clasificados correctamente.\n", sep = "")
    }
    cat("\n")
  }

  # Agregar silhouette por item a la asignacion

  asignacion_con_sil <- items_df[, c("codigo", "item", "dimension", "cluster")]
  if (!is.na(silhouette_resultado$promedio)) {
    asignacion_con_sil$silhouette <- silhouette_resultado$por_item[asignacion_con_sil$codigo]
  }

  resultado <- list(
    precision_global = precision_global,
    precision_por_dimension = precision_por_dim,
    matriz_confusion = matriz_confusion,
    items_mal_clasificados = items_mal_clasificados,
    asignacion_clusters = asignacion_con_sil,
    ari = ari,
    silhouette = silhouette_promedio,
    silhouette_por_item = silhouette_resultado$por_item,
    silhouette_por_cluster = silhouette_resultado$por_cluster,
    items_silhouette_negativo = silhouette_resultado$items_negativos,
    evaluacion_precision = eval_precision,
    evaluacion_ari = eval_ari,
    evaluacion_silhouette = eval_silhouette,
    n_clusters = n_clusters,
    metodo = metodo,
    consenso = consenso_df,
    clusters_componentes = clusters_componentes
  )

  class(resultado) <- c("semilla_precision", "list")
  return(resultado)
}


#' @title Exportar Items Problematicos a Excel
#'
#' @description
#' Genera un archivo Excel con multiples hojas conteniendo informacion
#' detallada sobre la clasificacion de items, similar al formato usado
#' en analisis de validacion de escalas.
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param archivo Ruta del archivo Excel a crear (sin extension)
#' @param incluir_todos Incluir hoja con todos los items (default: TRUE)
#'
#' @return Ruta del archivo creado (invisible)
#'
#' @details
#' El Excel generado contiene las siguientes hojas:
#' \itemize{
#'   \item \code{Instrucciones}: Descripcion del contenido
#'   \item \code{Todos_Items}: Todos los items con su clasificacion
#'   \item \code{Items_Problematicos}: Items mal clasificados con detalle
#'   \item \code{Resumen_Dimension}: Precision por dimension
#' }
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' exportar_items_problematicos(prec, "analisis_items")
#' }
#'
#' @export
#' @noRd
exportar_items_problematicos <- function(x, archivo, incluir_todos = TRUE) {

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  # Verificar writexl
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Requiere el paquete 'writexl'. Instala con: install.packages('writexl')")
  }

  # Agregar extension si no la tiene
  if (!grepl("\\.xlsx$", archivo)) {
    archivo <- paste0(archivo, ".xlsx")
  }

  # Obtener datos
  asig <- x$asignacion_clusters
  correctos <- x$precision_por_dimension

  # Crear columna de estado
  asig$Estado <- mapply(function(dim, clust) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0 && clust == expected) "Correcto" else "Incorrecto"
  }, asig$dimension, asig$cluster)

  # Agregar cluster esperado
  asig$Cluster_Esperado <- sapply(asig$dimension, function(dim) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0) expected else NA
  })

  # ===== HOJA 1: Instrucciones =====
  instrucciones <- data.frame(
    Seccion = c("PROPOSITO", "CONTENIDO", "USO", "METRICAS"),
    Descripcion = c(
      "Identificar items cuya clasificacion semantica difiere de su dimension teorica",
      paste0("Hoja 1: Instrucciones | Hoja 2: Todos los items | Hoja 3: Items problematicos | Hoja 4: Resumen"),
      "Revisar items problematicos, evaluar si requieren modificacion del fraseo o reasignacion de dimension",
      paste0("Precision Global: ", sprintf("%.1f", x$precision_global), "% | ARI: ", sprintf("%.3f", x$ari))
    ),
    stringsAsFactors = FALSE
  )

  # ===== HOJA 2: Todos los items =====
  todos_items <- data.frame(
    Codigo = asig$codigo,
    Item = asig$item,
    Dimension_Teorica = asig$dimension,
    Cluster_Esperado = asig$Cluster_Esperado,
    Cluster_Asignado = asig$cluster,
    Coincide = asig$Estado == "Correcto",
    Estado = ifelse(asig$Estado == "Correcto", "OK Correcto", "X Incorrecto"),
    stringsAsFactors = FALSE
  )

  # ===== HOJA 3: Items problematicos =====
  problematicos <- asig[asig$Estado == "Incorrecto", ]

  if (nrow(problematicos) > 0) {
    items_prob <- data.frame(
      Codigo = problematicos$codigo,
      Texto_Item = problematicos$item,
      Dimension_Teorica = problematicos$dimension,
      Cluster_Esperado = problematicos$Cluster_Esperado,
      Cluster_Asignado = problematicos$cluster,
      Problema = paste0("Item de '", problematicos$dimension,
                        "' clasificado en '", problematicos$cluster, "'"),
      stringsAsFactors = FALSE
    )
  } else {
    items_prob <- data.frame(
      Mensaje = "No hay items mal clasificados. Precision del 100%.",
      stringsAsFactors = FALSE
    )
  }

  # ===== HOJA 4: Resumen por dimension =====
  resumen <- x$precision_por_dimension
  resumen$Pct_Correcto <- round(resumen$Precision, 1)

  # Crear lista de hojas
  hojas <- list(
    Instrucciones = instrucciones,
    Todos_Items = todos_items,
    Items_Problematicos = items_prob,
    Resumen_Dimension = resumen
  )

  # Si no quiere todos los items, quitar esa hoja
  if (!incluir_todos) {
    hojas$Todos_Items <- NULL
  }

  # Escribir Excel
  writexl::write_xlsx(hojas, archivo)

  message("Excel exportado: ", archivo)
  message("  - Items totales: ", nrow(asig))
  message("  - Items problematicos: ", sum(asig$Estado == "Incorrecto"))
  message("  - Precision: ", sprintf("%.1f", x$precision_global), "%")

  invisible(archivo)
}
