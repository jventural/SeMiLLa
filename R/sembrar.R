#' @title Generar Escala: Items desde un Concepto
#'
#' @description
#' Genera items psicometricos a partir de un constructo psicologico.
#' Ofrece tres modos de conceptualizacion:
#' \itemize{
#'   \item \strong{Modo Conocimiento del LLM (fuente="llm")}: Usa el conocimiento
#'         interno del modelo de lenguaje para definir el constructo y sus dimensiones.
#'   \item \strong{Modo Manual (fuente="manual")}: El usuario proporciona la definicion
#'         operacional, las dimensiones y sus definiciones. El sistema genera las
#'         caracteristicas y los items correspondientes.
#'   \item \strong{Modo Cientifico (fuente="cientifico")}: Busca en bases de datos
#'         academicas (PubMed, Semantic Scholar) para extraer definiciones y dimensiones
#'         de articulos cientificos publicados.
#' }
#'
#' @param concepto Texto con el constructo a medir
#' @param api_key Tu API key de OpenAI
#' @param idioma Idioma: "es", "en", "pt"
#' @param poblacion Poblacion objetivo
#' @param n_items Numero de items a generar
#' @param n_dimensiones Numero de dimensiones (NULL = auto)
#' @param modelo Modelo de OpenAI. Opciones: "gpt-4.1-mini" (default), "gpt-4o",
#'   "gpt-4o-mini", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo", o cualquier modelo
#'   compatible con la API de OpenAI Chat Completions
#' @param fuente Fuente de conceptualizacion:
#'   \itemize{
#'     \item "llm" (default): Modo Conocimiento del LLM - usa el conocimiento
#'           interno del modelo para conceptualizar
#'     \item "manual": Modo Manual - el usuario ingresa la definicion operacional
#'           y las dimensiones con sus definiciones
#'     \item "cientifico": Modo Cientifico - busca en bases de datos academicas
#'           (PubMed, Semantic Scholar) para fundamentar la conceptualizacion
#'   }
#' @param definicion Para fuente="manual": definicion operacional del constructo
#' @param dimensiones Para fuente="manual": lista con dimensiones y sus definiciones.
#'   Ejemplo: list("Autoeficacia" = "Creencia en la propia capacidad...",
#'                 "Optimismo" = "Expectativa positiva sobre el futuro...")
#' @param bases_datos Para fuente="cientifico": bases a consultar.
#'   Opciones: "pubmed", "pmc", "scholar", "semantic_scholar". Default: c("pubmed", "scholar")
#' @param n_articulos Para fuente="cientifico": numero de articulos a revisar (default: 10)
#' @param complejidad_linguistica Nivel de complejidad del lenguaje de los items:
#'   "minimo", "basico", "intermedio" (default) o "avanzado".
#' @param tipo_escala_respuesta Tipo de escala de respuesta prevista:
#'   "frecuencia" (default), "acuerdo", "intensidad", "preferencia" o "ninguno".
#' @param evitar_cuantificadores Logico. Evitar cuantificadores tautologicos en
#'   los items. Si NULL (default), se decide segun \code{tipo_escala_respuesta}.
#' @param max_palabras Numero maximo de palabras por item (NULL = sin limite).
#' @param incluir_inversos Logico. Incluir items redactados en sentido inverso
#'   (default: TRUE).
#' @param seed Semilla para reproducibilidad. Cuando se especifica, se usa
#'        temperature=0 en el LLM para mayor consistencia
#' @param verbose Mostrar progreso
#'
#' @return Objeto de clase 'semilla_items' con items generados
#'
#' @details
#' \strong{Modo Conocimiento del LLM (fuente="llm")}:
#' El LLM analiza el constructo usando su base de conocimiento entrenada.
#' Identifica automaticamente la definicion operacional, dimensiones teoricas,
#' caracteristicas y genera los items. Es el modo mas rapido y conveniente.
#'
#' \strong{Modo Manual (fuente="manual")}:
#' El usuario proporciona la definicion operacional del constructo y las
#' dimensiones con sus respectivas definiciones. El sistema usa el LLM para
#' generar caracteristicas especificas para cada dimension y luego crea los items.
#' Util cuando se tiene una teoria especifica, se replica una escala existente,
#' o se requiere control preciso sobre la estructura dimensional.
#'
#' \strong{Modo Cientifico (fuente="cientifico")}:
#' Busca articulos cientificos en bases de datos academicas especializadas
#' (PubMed/PMC, Semantic Scholar). Extrae definiciones y dimensiones
#' directamente de la literatura publicada. Proporciona referencias
#' bibliograficas reales para fundamentar teoricamente la escala.
#' Requiere conexion a internet y puede tomar mas tiempo.
#'
#' @examples
#' \dontrun{
#' # ===== MODO 1: CONOCIMIENTO DEL LLM =====
#' # El modelo usa su base de conocimiento para conceptualizar
#' items_llm <- generar_escala(
#'   concepto = "resiliencia infantil",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "llm",  # Por defecto
#'   idioma = "es",
#'   poblacion = "ninos de 8 a 12 anos",
#'   n_items = 25
#' )
#'
#' # ===== MODO 2: MANUAL =====
#' # El usuario proporciona la definicion y dimensiones
#' items_manual <- generar_escala(
#'   concepto = "autoeficacia academica",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "manual",
#'   definicion = "Creencia del estudiante en su capacidad para
#'                 organizar y ejecutar acciones necesarias para
#'                 lograr metas academicas especificas",
#'   dimensiones = list(
#'     "Esfuerzo" = "Persistencia y dedicacion ante tareas academicas dificiles",
#'     "Capacidad" = "Confianza en las propias habilidades intelectuales",
#'     "Planificacion" = "Habilidad para organizar el tiempo y recursos de estudio",
#'     "Regulacion" = "Control de las emociones y motivacion durante el aprendizaje"
#'   ),
#'   idioma = "es",
#'   poblacion = "estudiantes universitarios",
#'   n_items = 20
#' )
#'
#' # ===== MODO 3: CIENTIFICO =====
#' # Busca en bases de datos academicas (PubMed, Semantic Scholar)
#' items_cientifico <- generar_escala(
#'   concepto = "burnout academico",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "cientifico",
#'   bases_datos = c("pubmed", "scholar"),
#'   n_articulos = 10,
#'   idioma = "es",
#'   poblacion = "estudiantes de medicina",
#'   n_items = 25
#' )
#' }
#'
#' @references
#' Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quinonez, H. R.,
#' & Young, S. L. (2018). Best practices for developing and validating scales
#' for health, social, and behavioral research: A primer. Frontiers in Public
#' Health, 6, 149.
#'
#' @export
generar_escala <- function(concepto,
                           api_key,
                           idioma = "es",
                           poblacion = NULL,
                           n_items = 20,
                           n_dimensiones = NULL,
                           modelo = "gpt-4.1-mini",
                           fuente = "llm",
                           definicion = NULL,
                           dimensiones = NULL,
                           bases_datos = c("pubmed", "scholar"),
                           n_articulos = 10,
                           complejidad_linguistica = "intermedio",
                           tipo_escala_respuesta = "frecuencia",
                           evitar_cuantificadores = NULL,
                           max_palabras = NULL,
                           incluir_inversos = TRUE,
                           blindaje = TRUE,
                           contexto_prohibido = NULL,
                           instrucciones_estilo = NULL,
                           modelo_jueces = "gpt-4.1-mini",
                           seed = NULL,
                           verbose = TRUE) {

  # Validaciones de los nuevos parametros
  complejidad_linguistica <- match.arg(
    complejidad_linguistica,
    c("minimo", "basico", "intermedio", "avanzado")
  )

  tipo_escala_respuesta <- match.arg(
    tipo_escala_respuesta,
    c("frecuencia", "acuerdo", "intensidad", "preferencia", "ninguno")
  )

  # Por defecto, si la escala de respuesta es de frecuencia o intensidad,
  # evitar que los items incluyan cuantificadores redundantes (tautologicos).
  if (is.null(evitar_cuantificadores)) {
    evitar_cuantificadores <- tipo_escala_respuesta %in%
                              c("frecuencia", "intensidad")
  }

  # Lunga maxima del item segun complejidad (si el usuario no especifica)
  if (is.null(max_palabras)) {
    max_palabras <- switch(complejidad_linguistica,
      "minimo"     = 10L,
      "basico"     = 12L,
      "intermedio" = 18L,
      "avanzado"   = 25L
    )
  }

  # ---- Auto-hint: poblacion infantil sugiere prompts_ilustracion() ----
  if (isTRUE(verbose) && !is.null(poblacion)) {
    pop_lower <- tolower(poblacion)
    pat_infantil <- paste0(
      "ni\u00f1[oa]s?|ninos?|ninas?|infantil|infantes?|ni\u00f1ez|",
      "primaria|preescolar|inicial|kinder|kinderg|",
      "children|kids?|child(?!ish)|infant|toddler|elementary"
    )
    if (grepl(pat_infantil, pop_lower, perl = TRUE)) {
      message(
        "\n[hint] Poblacion infantil detectada en `poblacion = '", poblacion, "'`.",
        "\n       Considera generar prompts visuales para los items con:",
        "\n         prompts_ilustracion(escala, api_key, paleta = 'bn')",
        "\n       facilita comprension lectora en menores y permite usar",
        "\n       el test con apoyo grafico (Gemini/Midjourney/ChatGPT).\n"
      )
    }
  }

  # Fijar semilla si se proporciona
  if (!is.null(seed)) {
    set.seed(seed)
    # Temperature 0 + seed API + top_p 1 para maximizar reproducibilidad
    old_temp  <- getOption("SeMiLLa.temperature")
    old_seed  <- getOption("SeMiLLa.seed")
    old_top_p <- getOption("SeMiLLa.top_p")
    options(SeMiLLa.temperature = 0,
            SeMiLLa.seed        = as.integer(seed),
            SeMiLLa.top_p       = 1)
    on.exit({
      options(SeMiLLa.temperature = old_temp,
              SeMiLLa.seed        = old_seed,
              SeMiLLa.top_p       = old_top_p)
    }, add = TRUE)
  }

  # Validaciones
  .validar_concepto(concepto)
  .validar_api_key(api_key)
  .validar_idioma(idioma)
  .validar_n_items(n_items)

  # Validar fuente
  fuente <- tolower(fuente)
  if (!fuente %in% c("llm", "manual", "cientifico")) {
    stop("fuente debe ser 'llm', 'manual' o 'cientifico'")
  }

  # Validar parametros segun fuente
  if (fuente == "manual") {
    if (is.null(definicion)) stop("Para fuente='manual' debes proporcionar 'definicion'")
    if (is.null(dimensiones) || length(dimensiones) == 0) {
      stop("Para fuente='manual' debes proporcionar 'dimensiones' como lista")
    }
    if (!is.list(dimensiones)) stop("'dimensiones' debe ser una lista con nombres")
  }

  # Configurar OpenAI
  openai <- .configurar_openai(api_key)

  # ==========================================================================
  # PASO 1: Obtener informacion del concepto segun la fuente
  # ==========================================================================

  if (fuente == "llm") {
    # ------- MODO LLM: Conocimiento del modelo -------
    if (verbose) {
      cat("\n")
      cat(.linea("-"), "\n")
      cat(.color_verde("FASE 1: ANALISIS TEORICO DEL CONSTRUCTO"), "\n")
      cat(.color_azul("Modo: CONOCIMIENTO DEL LLM"), "\n")
      cat(.linea("-"), "\n\n")
      cat("  ", .color_flecha(), " Consultando base de conocimiento del modelo...\n", sep = "")
      cat("    > Buscando definiciones academicas de '", concepto, "'...\n", sep = "")
      cat("    > Identificando teorias y modelos relevantes...\n")
      cat("    > Extrayendo dimensiones del constructo...\n")
      if (!is.null(poblacion)) {
        cat("    > Adaptando al contexto de: ", poblacion, "\n", sep = "")
      }
      cat("\n")
    }

    info_concepto <- .analizar_concepto(
      concepto = concepto,
      openai = openai,
      idioma = idioma,
      poblacion = poblacion,
      n_dimensiones = n_dimensiones,
      modelo = modelo
    )
    info_concepto$fuente <- "llm"

  } else if (fuente == "manual") {
    # ------- MODO MANUAL: Definicion del usuario -------
    if (verbose) {
      cat("\n")
      cat(.linea("-"), "\n")
      cat(.color_verde("FASE 1: ANALISIS TEORICO DEL CONSTRUCTO"), "\n")
      cat(.color_azul("Modo: MANUAL (definicion del usuario)"), "\n")
      cat(.linea("-"), "\n\n")
      cat("  ", .color_flecha(), " Procesando definicion proporcionada por el usuario...\n", sep = "")
      cat("    > Definicion operacional recibida\n")
      cat("    > ", length(dimensiones), " dimensiones especificadas\n", sep = "")
      cat("    > Generando caracteristicas para cada dimension con LLM...\n")
      cat("\n")
    }

    # Generar caracteristicas inteligentes para las dimensiones manuales
    caracteristicas <- .generar_caracteristicas_manual(
      openai = openai,
      concepto = concepto,
      definicion = definicion,
      dimensiones = dimensiones,
      idioma = idioma,
      modelo = modelo
    )

    # Construir info_concepto desde los parametros manuales
    info_concepto <- list(
      concepto = concepto,
      definicion = definicion,
      dimensiones = dimensiones,
      caracteristicas = caracteristicas,
      teorias = NULL,
      modelos = NULL,
      referencias = NULL,
      fuente = "manual"
    )

  } else if (fuente == "cientifico") {
    # ------- MODO CIENTIFICO: Busqueda en bases de datos -------
    if (verbose) {
      cat("\n")
      cat(.linea("-"), "\n")
      cat(.color_verde("FASE 1: ANALISIS TEORICO DEL CONSTRUCTO"), "\n")
      cat(.color_azul("Modo: CIENTIFICO (busqueda en bases de datos)"), "\n")
      cat(.linea("-"), "\n\n")
      cat("  ", .color_flecha(), " Buscando en bases de datos academicas...\n", sep = "")
      cat("    > Bases de datos: ", paste(bases_datos, collapse = ", "), "\n", sep = "")
      cat("    > Articulos a revisar: ", n_articulos, "\n", sep = "")
      cat("    > Termino de busqueda: '", concepto, "'\n\n", sep = "")
    }

    info_concepto <- .buscar_concepto_cientifico(
      concepto = concepto,
      openai = openai,
      idioma = idioma,
      poblacion = poblacion,
      bases_datos = bases_datos,
      n_articulos = n_articulos,
      n_dimensiones = n_dimensiones,
      modelo = modelo,
      verbose = verbose
    )
    info_concepto$fuente <- "cientifico"
  }

  # Mostrar resultados de la conceptualizacion
  if (verbose) {
    cat("  ", .color_check(), " Analisis teorico completado\n\n", sep = "")
    cat("  ", .color_verde("DEFINICION OPERACIONAL:"), "\n", sep = "")
    def_lines <- strwrap(info_concepto$definicion, width = 65)
    for (line in def_lines) {
      cat("    ", line, "\n", sep = "")
    }
    cat("\n")
    cat("  ", .color_verde("DIMENSIONES IDENTIFICADAS:"), " (", length(info_concepto$dimensiones), ")\n", sep = "")
    for (d in names(info_concepto$dimensiones)) {
      cat("    [+] ", d, "\n", sep = "")
      def_dim <- info_concepto$dimensiones[[d]]
      if (nchar(def_dim) > 70) {
        def_dim <- paste0(substr(def_dim, 1, 67), "...")
      }
      cat("        ", def_dim, "\n", sep = "")
    }
    cat("\n")

    if (!is.null(info_concepto$teorias) && length(info_concepto$teorias) > 0) {
      cat("  ", .color_verde("FUNDAMENTACION TEORICA:"), "\n", sep = "")
      for (t in info_concepto$teorias) {
        cat("    * ", t, "\n", sep = "")
      }
      cat("\n")
    }
  }

  # PASO 2: Generar items
  if (verbose) {
    cat(.linea("-"), "\n")
    cat(.color_verde("FASE 2: GENERACION DE ITEMS"), "\n")
    cat(.linea("-"), "\n\n")
    cat("  ", .color_flecha(), " Generando items por dimension...\n", sep = "")
    cat("    > Aplicando criterios psicometricos (claridad, simplicidad, relevancia)\n")
    cat("    > Siguiendo guias de redaccion de Ferrando et al. (2025)\n")
    cat("    > Adaptando lenguaje para: ", ifelse(is.null(poblacion), "poblacion general", poblacion), "\n\n", sep = "")
  }

  dimensiones <- names(info_concepto$dimensiones)
  n_dims <- length(dimensiones)
  items_por_dim <- ceiling(n_items / n_dims)

  todos_items <- data.frame()

  for (i in seq_along(dimensiones)) {
    dim_nombre <- dimensiones[i]
    if (verbose) {
      cat("  [", i, "/", n_dims, "] ", dim_nombre, "\n", sep = "")
      cat("        Generando ", items_por_dim, " items para esta dimension...", sep = "")
    }

    items_dim <- .generar_items_dimension(
      openai = openai,
      concepto = concepto,
      dimension = dim_nombre,
      definicion_dim = info_concepto$dimensiones[[dim_nombre]],
      caracteristicas = info_concepto$caracteristicas[[dim_nombre]],
      n_items = items_por_dim,
      idioma = idioma,
      poblacion = poblacion,
      modelo = modelo,
      complejidad_linguistica = complejidad_linguistica,
      tipo_escala_respuesta = tipo_escala_respuesta,
      evitar_cuantificadores = evitar_cuantificadores,
      max_palabras = max_palabras,
      incluir_inversos = incluir_inversos,
      instruccion_extra = instrucciones_estilo
    )

    if (!is.null(items_dim) && nrow(items_dim) > 0) {
      items_dim$dimension <- dim_nombre

      # Auditoria linguistica post-hoc: cuantificadores y longitud
      if (evitar_cuantificadores) {
        items_dim <- .auditar_cuantificadores(items_dim, verbose = verbose)
      }
      items_dim <- .auditar_longitud(items_dim, max_palabras,
                                     verbose = verbose,
                                     openai  = openai,
                                     modelo  = modelo,
                                     idioma  = idioma)

      todos_items <- rbind(todos_items, items_dim)
      if (verbose) cat(" ", .color_check(), " ", nrow(items_dim), " items generados\n", sep = "")
    } else {
      if (verbose) cat(" Error\n")
    }

    Sys.sleep(1)  # Rate limit
  }

  # Limpiar y numerar
  if (nrow(todos_items) > 0) {
    todos_items$item <- trimws(todos_items$item)
    todos_items$item <- gsub('^["\']|["\']$', '', todos_items$item)
    n_antes <- nrow(todos_items)
    todos_items <- todos_items[!duplicated(tolower(todos_items$item)), ]
    n_duplicados <- n_antes - nrow(todos_items)
    todos_items$numero <- 1:nrow(todos_items)
    todos_items <- todos_items[, c("numero", "dimension", "caracteristica", "item")]

    if (verbose && n_duplicados > 0) {
      cat("\n  ", .color_warning(), " Se eliminaron ", n_duplicados, " items duplicados\n", sep = "")
    }
  }

  # FASE 2b: BLINDAJE (jueces LLM de contexto poblacional y de parafrasis).
  # El prompt anti-redundancia y el bloque de poblacion son necesarios pero
  # NO suficientes: el modelo aun produce gemelos semanticos (que el coseno
  # subdetecta) y ocasionales fugas de contexto. Este cierre garantiza que
  # la escala salga limpia desde la generacion, sin depender de correcciones
  # posteriores.
  if (isTRUE(blindaje) && nrow(todos_items) > 1) {
    if (verbose) {
      cat("\n", .linea("-"), "\n", sep = "")
      cat(.color_verde("FASE 2b: BLINDAJE (contexto + parafrasis, juez LLM)"), "\n")
      cat(.linea("-"), "\n\n")
    }
    bl <- .blindar_items(
      items_df = todos_items, info_concepto = info_concepto, openai = openai,
      modelo = modelo, concepto_nombre = concepto,
      poblacion = poblacion, idioma = idioma,
      complejidad_linguistica = complejidad_linguistica,
      max_palabras = max_palabras,
      tipo_escala_respuesta = tipo_escala_respuesta,
      contexto_prohibido = contexto_prohibido,
      instrucciones_estilo = instrucciones_estilo,
      modelo_jueces = modelo_jueces,
      verbose = verbose)
    todos_items <- bl$items
    # Los reemplazos tambien pasan la auditoria de longitud
    todos_items <- .auditar_longitud(todos_items, max_palabras,
                                     verbose = FALSE, openai = openai,
                                     modelo = modelo, idioma = idioma)
    blindaje_reporte <- bl$reporte
  } else {
    blindaje_reporte <- NULL
  }

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_verde("RESUMEN DE GENERACION"), "\n")
    cat(.linea("-"), "\n")
    cat("  ", .color_check(), " Total items generados: ", nrow(todos_items), "\n", sep = "")
    cat("  ", .color_check(), " Dimensiones cubiertas: ", n_dims, "\n", sep = "")
    cat("  ", .color_check(), " Items por dimension: ~", round(nrow(todos_items)/n_dims, 1), "\n", sep = "")
    cat(.linea("-"), "\n\n")
  }

  # Resultado
  resultado <- list(
    items = todos_items,
    concepto = info_concepto,
    metadata = list(
      concepto_original = concepto,
      idioma = idioma,
      poblacion = poblacion,
      modelo = modelo,
      n_items_solicitados = n_items,
      n_items_generados = nrow(todos_items),
      complejidad_linguistica = complejidad_linguistica,
      tipo_escala_respuesta = tipo_escala_respuesta,
      evitar_cuantificadores = evitar_cuantificadores,
      max_palabras = max_palabras,
      incluir_inversos = incluir_inversos,
      blindaje = blindaje_reporte,
      contexto_prohibido = contexto_prohibido,
      instrucciones_estilo = instrucciones_estilo,
      fecha = Sys.time()
    )
  )

  class(resultado) <- c("semilla_items", "list")

  if (verbose) {
    cat("  ", .color_check(), " Items generados: ", nrow(todos_items), "\n", sep = "")
  }

  return(resultado)
}


#' @title Validar Escala Existente
#'
#' @description
#' Valida psicometricamente una escala ya existente utilizando analisis semantico.
#' En lugar de generar nuevos items, toma los items existentes y ejecuta el pipeline
#' de validacion: embeddings semanticos, analisis factorial confirmatorio (CFA),
#' analisis factorial exploratorio (EFA), y evaluacion psicometrica.
#'
#' @param nombre Nombre del constructo o escala (ej: "Resolucion de Problemas")
#' @param definicion Definicion operacional del constructo
#' @param dimensiones Lista con la estructura de dimensiones e items existentes.
#'   Cada dimension debe contener:
#'   \itemize{
#'     \item \code{definicion}: Definicion de la dimension
#'     \item \code{items}: Vector nombrado con los items (nombre = codigo, valor = texto)
#'   }
#' @param api_key Tu API key de OpenAI
#' @param incluir_cfa Ejecutar analisis factorial confirmatorio (default: TRUE)
#' @param incluir_efa Ejecutar analisis factorial exploratorio (default: TRUE)
#' @param n_factores_efa Numero de factores para EFA (NULL = parallel analysis)
#' @param estimador_cfa Estimador para CFA: "ML" (default), "MLR", "WLSMV", "ULS"
#' @param corr_residuales Permitir correlaciones residuales entre items adyacentes
#'   dentro del mismo factor (default: FALSE). Activar mejora el ajuste cuando
#'   hay items semanticamente muy similares.
#' @param correccion_semantica Aplicar transformacion Fisher a la matriz de
#'   similitud para mejorar el ajuste del CFA (default: TRUE). Esta correccion
#'   reduce el "efecto halo" semantico donde todos los items correlacionan alto.
#' @param verbose Mostrar progreso en consola (default: TRUE)
#'
#' @return Objeto de clase 'semilla' con:
#' \itemize{
#'   \item \code{concepto}: Informacion del constructo
#'   \item \code{items}: Dataframe con los items
#'   \item \code{embeddings}: Matriz de embeddings
#'   \item \code{similitud}: Matriz de similitud coseno
#'   \item \code{cfa}: Resultados del CFA (si incluir_cfa = TRUE)
#'   \item \code{efa}: Resultados del EFA (si incluir_efa = TRUE)
#'   \item \code{metadata}: Informacion del proceso
#' }
#'
#' @examples
#' \dontrun{
#' # Validar escala de Resolucion de Problemas
#' escala_rp <- validar_escala(
#'   nombre = "Resolucion de Problemas",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'
#'   definicion = "Proceso cognitivo que implica identificar, generar,
#'                 evaluar y seleccionar soluciones para resolver problemas.",
#'
#'   dimensiones = list(
#'     "Analisis" = list(
#'       definicion = "Capacidad para analizar el problema sistematicamente",
#'       items = c(
#'         "RP1" = "Hago una lista de todas las alternativas.",
#'         "RP2" = "Verifico si la solucion resuelve el problema.",
#'         "RP3" = "Comparo las alternativas seleccionadas."
#'       )
#'     ),
#'     "Evaluacion" = list(
#'       definicion = "Capacidad para evaluar resultados y ajustar",
#'       items = c(
#'         "RP4" = "Evaluo los resultados obtenidos.",
#'         "RP5" = "Identifico los obstaculos del problema."
#'       )
#'     )
#'   )
#' )
#'
#' # Ver resultados
#' print(escala_rp)
#'
#' # Ver ajuste del CFA
#' escala_rp$cfa$ajuste
#'
#' # Evaluar fiabilidad
#' fiabilidad_semantica(escala_rp)
#' }
#'
#' @export
#' @noRd
validar_escala <- function(nombre,
                           definicion,
                           dimensiones,
                           api_key,
                           incluir_cfa = TRUE,
                           incluir_efa = TRUE,
                           n_factores_efa = NULL,
                           estimador_cfa = "ML",
                           corr_residuales = FALSE,
                           correccion_semantica = TRUE,
                           verbose = TRUE) {

  # Validaciones
  if (missing(nombre) || is.null(nombre) || nombre == "") {
    stop("Debes proporcionar el nombre del constructo")
  }
  if (missing(definicion) || is.null(definicion) || definicion == "") {
    stop("Debes proporcionar la definicion operacional del constructo")
  }
  if (missing(dimensiones) || !is.list(dimensiones) || length(dimensiones) == 0) {
    stop("Debes proporcionar 'dimensiones' como lista con al menos una dimension")
  }
  .validar_api_key(api_key)

  # Validar estructura de dimensiones
  for (dim_nombre in names(dimensiones)) {
    dim_data <- dimensiones[[dim_nombre]]
    if (!is.list(dim_data)) {
      stop("Cada dimension debe ser una lista con 'definicion' e 'items'. Error en: ", dim_nombre)
    }
    if (is.null(dim_data$definicion)) {
      stop("Falta 'definicion' en la dimension: ", dim_nombre)
    }
    if (is.null(dim_data$items) || length(dim_data$items) == 0) {
      stop("Falta 'items' o esta vacio en la dimension: ", dim_nombre)
    }
  }

  # Contar pasos totales
  n_pasos <- 2  # Items + Embeddings
 if (incluir_cfa) n_pasos <- n_pasos + 1
  if (incluir_efa) n_pasos <- n_pasos + 1
  paso_actual <- 0

  if (verbose) {
    .mostrar_banner()
    cat("\n")
    cat(.color_verde("VALIDACION DE ESCALA EXISTENTE"), "\n")
    cat(.linea("="), "\n")
    cat("  Escala:       ", nombre, "\n", sep = "")
    cat("  Dimensiones:  ", length(dimensiones), "\n", sep = "")
    n_items_total <- sum(sapply(dimensiones, function(d) length(d$items)))
    cat("  Items:        ", n_items_total, "\n", sep = "")
    cat("  CFA:          ", ifelse(incluir_cfa, "Si (lavaan)", "No"), "\n", sep = "")
    cat("  EFA:          ", ifelse(incluir_efa, "Si", "No"), "\n", sep = "")
    cat(.linea("="), "\n")
  }

  # ==========================================================================
  # PASO 1: Construir estructura de items desde las dimensiones
  # ==========================================================================
  paso_actual <- paso_actual + 1

  if (verbose) {
    cat("\n")
    cat(.linea("-"), "\n")
    cat(.color_azul(paste0("[", paso_actual, "/", n_pasos, "] PROCESANDO ITEMS EXISTENTES")), "\n")
    cat(.linea("-"), "\n\n")
  }

  todos_items <- data.frame()
  dims_info <- list()
  items_por_dimension <- list()  # Para CFA
  numero_item <- 1

  for (dim_nombre in names(dimensiones)) {
    dim_data <- dimensiones[[dim_nombre]]

    # Guardar definicion de dimension
    dims_info[[dim_nombre]] <- dim_data$definicion

    # Procesar items
    items_dim <- dim_data$items

    # Si los items tienen nombres (codigos), usarlos
    if (!is.null(names(items_dim))) {
      codigos <- names(items_dim)
    } else {
      codigos <- paste0("I", numero_item:(numero_item + length(items_dim) - 1))
    }

    # Guardar codigos para CFA
    items_por_dimension[[dim_nombre]] <- codigos

    for (j in seq_along(items_dim)) {
      todos_items <- rbind(todos_items, data.frame(
        numero = numero_item,
        codigo = codigos[j],
        dimension = dim_nombre,
        item = as.character(items_dim[j]),
        stringsAsFactors = FALSE
      ))
      numero_item <- numero_item + 1
    }

    if (verbose) {
      cat("  [+] ", dim_nombre, ": ", length(items_dim), " items\n", sep = "")
    }
  }

  if (verbose) {
    cat("\n  ", .color_check(), " Total: ", nrow(todos_items), " items procesados\n\n", sep = "")
  }

  # Construir objeto concepto
  info_concepto <- list(
    concepto = nombre,
    definicion = definicion,
    dimensiones = dims_info,
    caracteristicas = NULL,
    teorias = NULL,
    modelos = NULL,
    referencias = NULL,
    fuente = "existente"
  )

  # Crear objeto items_result para compatibilidad
  items_result <- list(
    items = todos_items,
    concepto = info_concepto,
    metadata = list(
      concepto_original = nombre,
      idioma = "es",
      poblacion = NULL,
      modelo = NA,
      n_items_solicitados = nrow(todos_items),
      n_items_generados = nrow(todos_items),
      fecha = Sys.time()
    )
  )
  class(items_result) <- c("semilla_items", "list")

  # ==========================================================================
  # PASO 2: Calcular embeddings
  # ==========================================================================
  paso_actual <- paso_actual + 1

  if (verbose) {
    cat(.linea("-"), "\n")
    cat(.color_azul(paste0("[", paso_actual, "/", n_pasos, "] REPRESENTACION SEMANTICA")), "\n")
    cat(.linea("-"), "\n\n")
    cat("  ", .color_flecha(), " Calculando embeddings para cada item...\n", sep = "")
    cat("    > Convirtiendo items a vectores de 1536 dimensiones\n")
    cat("    > Calculando matriz de similitud coseno\n\n")
  }

  emb_result <- obtener_embeddings(
    items = items_result,
    api_key = api_key,
    verbose = verbose
  )

  # ==========================================================================
  # PASO 3: Analisis Factorial Confirmatorio (CFA)
  # ==========================================================================

  cfa_result <- NULL
  if (incluir_cfa) {
    paso_actual <- paso_actual + 1

    if (verbose) {
      cat("\n")
      cat(.linea("-"), "\n")
      cat(.color_azul(paste0("[", paso_actual, "/", n_pasos, "] ANALISIS FACTORIAL CONFIRMATORIO (CFA)")), "\n")
      cat(.linea("-"), "\n\n")
      cat("  ", .color_flecha(), " Ejecutando CFA con lavaan...\n", sep = "")
      cat("    > Estimador: ", estimador_cfa, "\n", sep = "")
      cat("    > Modelo: ", length(dimensiones), " factores correlacionados\n", sep = "")
      cat("    > Usando matriz de similitud semantica\n\n")
    }

    cfa_result <- .ejecutar_cfa_semantico(
      similitud = emb_result$similitud,
      items = todos_items,
      items_por_dimension = items_por_dimension,
      estimador = estimador_cfa,
      corr_residuales = corr_residuales,
      transformar = correccion_semantica,
      verbose = verbose
    )

    if (verbose && !is.null(cfa_result)) {
      cat("\n  ", .color_check(), " CFA completado\n\n", sep = "")
      cat("  ", .color_verde("INDICES DE AJUSTE:"), "\n", sep = "")
      cat("    CFI  = ", sprintf("%.3f", cfa_result$ajuste$cfi),
          ifelse(cfa_result$ajuste$cfi >= 0.95, " (Excelente)",
                 ifelse(cfa_result$ajuste$cfi >= 0.90, " (Aceptable)", " (Pobre)")), "\n", sep = "")
      cat("    TLI  = ", sprintf("%.3f", cfa_result$ajuste$tli),
          ifelse(cfa_result$ajuste$tli >= 0.95, " (Excelente)",
                 ifelse(cfa_result$ajuste$tli >= 0.90, " (Aceptable)", " (Pobre)")), "\n", sep = "")
      cat("    RMSEA = ", sprintf("%.3f", cfa_result$ajuste$rmsea),
          ifelse(cfa_result$ajuste$rmsea <= 0.05, " (Excelente)",
                 ifelse(cfa_result$ajuste$rmsea <= 0.08, " (Aceptable)", " (Pobre)")), "\n", sep = "")
      cat("    SRMR = ", sprintf("%.3f", cfa_result$ajuste$srmr),
          ifelse(cfa_result$ajuste$srmr <= 0.05, " (Excelente)",
                 ifelse(cfa_result$ajuste$srmr <= 0.08, " (Aceptable)", " (Pobre)")), "\n", sep = "")
      cat("\n")
    }
  }

  # ==========================================================================
  # PASO 4: Analisis Factorial Exploratorio (opcional)
  # ==========================================================================

  efa_result <- NULL
  if (incluir_efa) {
    paso_actual <- paso_actual + 1

    if (verbose) {
      cat(.linea("-"), "\n")
      cat(.color_azul(paste0("[", paso_actual, "/", n_pasos, "] ANALISIS FACTORIAL EXPLORATORIO (EFA)")), "\n")
      cat(.linea("-"), "\n\n")
      cat("  ", .color_flecha(), " Realizando EFA...\n", sep = "")
      cat("    > Determinando numero de factores via analisis paralelo\n")
      cat("    > Extrayendo factores con rotacion oblimin\n\n")
    }

    # Calcular clustering semantico
    temp_val <- list(
      items = todos_items,
      embeddings = emb_result$embeddings,
      similitud = emb_result$similitud
    )
    class(temp_val) <- c("semilla", "list")

    efa_result <- precision_clasificacion(
      x = temp_val,
      n_clusters = n_factores_efa,
      verbose = verbose
    )
  }

  # ==========================================================================
  # Construir resultado final
  # ==========================================================================

  resultado <- list(
    concepto = info_concepto,
    items = todos_items,
    embeddings = emb_result$embeddings,
    similitud = emb_result$similitud,
    cfa = cfa_result,
    efa = efa_result,
    metadata = list(
      concepto_original = nombre,
      idioma = "es",
      poblacion = NULL,
      modelo = NA,
      n_items_generados = nrow(todos_items),
      tipo = "validacion",
      seed = NULL,
      fecha = Sys.time(),
      version = packageVersion("SeMiLLa")
    )
  )

  class(resultado) <- c("semilla", "list")

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("VALIDACION COMPLETADA"), "\n")
    cat(.linea("="), "\n")
    cat("  Items procesados: ", nrow(todos_items), "\n", sep = "")
    cat("  Dimensiones: ", length(dimensiones), "\n", sep = "")
    if (!is.null(cfa_result)) {
      cat("  CFA - CFI: ", sprintf("%.3f", cfa_result$ajuste$cfi),
          " | RMSEA: ", sprintf("%.3f", cfa_result$ajuste$rmsea), "\n", sep = "")
    }
    if (!is.null(efa_result)) {
      cat("  EFA - Factores: ", efa_result$metadata$n_factores,
          " | Varianza: ", round(sum(efa_result$varianza$Prop_Var) * 100, 1), "%\n", sep = "")
    }
    cat(.linea("="), "\n\n")
    cat("  Resultados disponibles:\n")
    cat("    - escala$cfa$ajuste        # Indices de ajuste\n")
    cat("    - escala$cfa$cargas        # Cargas factoriales\n")
    cat("    - escala$cfa$modelo_lavaan # Objeto lavaan completo\n")
    cat("    - fiabilidad_semantica(escala)\n")
    cat("    - validez_contenido(escala, api_key)\n")
    cat("\n")
  }

  return(resultado)
}


#' @title Ver Items Generados
#'
#' @description
#' Devuelve un dataframe con los items organizados por factor.
#'
#' @param x Objeto semilla, semilla_items, o dataframe
#' @param dimension Filtrar por dimension (NULL = todas)
#'
#' @return Dataframe con columnas: factor, item
#'
#' @examples
#' \dontrun{
#' # Ver todos los items
#' ver_items(mi_escala)
#'
#' # Ver items de una dimension
#' ver_items(mi_escala, dimension = "autoeficacia")
#' }
#'
#' @export
ver_items <- function(x, dimension = NULL) {

  # Extraer items segun tipo de objeto
 if (inherits(x, "semilla")) {
    items <- x$items
  } else if (inherits(x, "semilla_items")) {
    items <- x$items
  } else if (is.data.frame(x) && "item" %in% names(x)) {
    items <- x
  } else {
    stop("Objeto no valido. Usa un objeto semilla o semilla_items.")
  }

  # Filtrar por dimension
  if (!is.null(dimension)) {
    items <- items[items$dimension == dimension, ]
    if (nrow(items) == 0) {
      dims_disponibles <- unique(x$items$dimension)
      stop("Dimension '", dimension, "' no encontrada. ",
           "Disponibles: ", paste(dims_disponibles, collapse = ", "))
    }
  }

  # Devolver dataframe simple
  resultado <- data.frame(
    factor = items$dimension,
    item = items$item,
    stringsAsFactors = FALSE
  )

  return(resultado)
}


#' @title Refinar Escala Iterativamente
#'
#' @description
#' Proceso iterativo que identifica items problematicos (mal clasificados),
#' los reemplaza con nuevos items generados por IA, y repite el analisis
#' hasta que todos los items encajen en su estructura teorica.
#'
#' @param escala Objeto semilla con la escala a refinar
#' @param api_key Tu API key de OpenAI
#' @param max_iteraciones Numero maximo de iteraciones (default: 5)
#' @param umbral_precision Precision minima aceptable 0-100 (default: 100).
#'        Solo se usa cuando \code{criterio = "kmeans"}.
#' @param criterio Metodo para identificar items problematicos:
#'        \itemize{
#'          \item \code{"kmeans"} (default, conservador): k-means simple sobre
#'                embeddings. Item es problematico si su cluster k-means no
#'                coincide con el cluster ganador de su dimension teorica.
#'          \item \code{"ensemble"} (estricto, recomendado para escalas
#'                cortas): consenso entre k-means + Ward jerarquico + PAM.
#'                Item es problematico si su consenso ensemble es inferior
#'                a \code{umbral_consenso}. Replica la propuesta de Voss et
#'                al. (2026) sobre clustering consensus.
#'        }
#' @param umbral_consenso Umbral minimo de consenso ensemble para considerar
#'        un item bien clasificado (default: 0.667 = al menos 2/3 algoritmos).
#'        Solo se usa cuando \code{criterio = "ensemble"}. Valores tipicos:
#'        0.667 (mayoria simple), 0.999 (unanimidad).
#' @param heredar_compuerta Si \code{TRUE} (default) y la escala trae
#'        \code{$compuerta}, el refinamiento hereda su umbral de deteccion y
#'        prohibe los nucleos lexicos que la compuerta mando a eliminar. Ademas
#'        devuelve \code{$concordancia} con la re-verificacion del eje 1. Es lo
#'        que impide que este paso reconstruya lo que la compuerta acababa de
#'        podar: el consenso empirico PREMIA a los items parecidos entre si.
#' @param umbral_redundancia Similitud maxima permitida entre items 0-1.
#'        Default \code{"compuerta"}: toma el umbral efectivo de la compuerta
#'        (adaptativo, tipicamente 0.62-0.70) menos 0.03 de margen; si no hay
#'        compuerta, usa 0.70. Antes era 0.70 FIJO, y quedaba una franja en la
#'        que este paso aceptaba items que la compuerta si marcaba.
#'        Valor historico (0.70 fijo):
#'        Items nuevos mas similares que este umbral seran regenerados. El default
#'        bajo de 0.85 a 0.70 en v2.7.0: la calibracion con datos reales (escala
#'        PM policial, n=280) mostro que las parafrasis que luego correlacionan
#'        >= .70 en aplicacion viven en similitud coseno 0.56-0.78, y 0.85 solo
#'        detecta clones casi literales (capturo 0 de 8 pares gemelos).
#'        La comparacion incluye items de TODAS las dimensiones.
#' @param max_intentos_redundancia Intentos maximos para generar item no redundante (default: 3)
#' @param modelo Modelo de OpenAI para generar nuevos items
#' @param exportar_excel Exportar historial a Excel (default: TRUE)
#' @param carpeta_salida Carpeta para guardar resultados
#' @param verbose Mostrar progreso en consola (default: TRUE)
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{escala_final}: Objeto semilla con la escala refinada
#'   \item \code{historial}: Dataframe con items reemplazados por iteracion
#'   \item \code{iteraciones}: Numero de iteraciones realizadas
#'   \item \code{precision_inicial}: Precision de clasificacion inicial
#'   \item \code{precision_final}: Precision de clasificacion final
#'   \item \code{evolucion}: Dataframe (\code{Iteracion}, \code{Precision}) con
#'     la precision en cada paso del refinamiento; apto para
#'     \code{plot_evolucion_precision()}
#' }
#'
#' @examples
#' \dontrun{
#' # Crear escala inicial
#' escala <- semilla("resiliencia infantil", api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Refinar hasta que todos los items encajen
#' resultado <- refinar_escala(escala, api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Ver historial de cambios
#' print(resultado$historial)
#' }
#'
#' @export
refinar_escala <- function(escala,
                           api_key,
                           max_iteraciones = 5,
                           umbral_precision = 100,
                           criterio = c("kmeans", "ensemble"),
                           umbral_consenso = 0.667,
                           umbral_redundancia = "compuerta",
                           max_intentos_redundancia = 3,
                           heredar_compuerta = TRUE,
                           modelo = "gpt-4.1-mini",
                           exportar_excel = TRUE,
                           carpeta_salida = NULL,
                           # v2.9.29 ------------------------------------------------
                           # max_reescrituras_item: tope de veces que un MISMO item
                           #   puede reescribirse en toda la corrida. Sin tope, el
                           #   bucle persigue su propia cola: en una corrida real
                           #   hubo 54 reescrituras sobre 16 items, con dos de ellos
                           #   reescritos OCHO veces. Un item que no converge en 2
                           #   intentos no es un problema de redaccion: o la
                           #   dimension esta mal definida, o ese item mide otra cosa.
                           #   Copiado del patron de converger_escala().
                           max_reescrituras_item = 2L,
                           # devolver_mejor: entregar la mejor iteracion vista y no
                           #   la ultima. El bucle es un paseo aleatorio (cada
                           #   reemplazo mueve los embeddings y reordena la
                           #   particion): en una corrida real paso por 75.0% y
                           #   termino en 62.5%.
                           devolver_mejor = TRUE,
                           verbose = TRUE) {

  criterio <- match.arg(criterio)
  if (umbral_consenso < 0 || umbral_consenso > 1) {
    stop("umbral_consenso debe estar entre 0 y 1")
  }

  # ---------------------------------------------------------------------------
  #  CONCORDANCIA CON LA COMPUERTA PRE-APLICACION
  #  El refinamiento maximiza que cada item caiga en su cluster empirico; la via
  #  mas facil de conseguirlo es que el item se PAREZCA MAS a sus vecinos de
  #  dimension. Es decir, tiene un incentivo estructural hacia la redundancia,
  #  justo lo que la compuerta acaba de podar. Si la escala trae una compuerta,
  #  se heredan sus dos contratos: el umbral con el que DETECTA parecidos y las
  #  conductas/muletillas que mando a eliminar.
  # ---------------------------------------------------------------------------
  comp_previa   <- if (isTRUE(heredar_compuerta)) escala$compuerta else NULL
  vetos_compuerta <- character(0)
  desea_compuerta <- NULL
  if (!is.null(comp_previa)) {
    fac <- comp_previa$redaccion$facetas_repetidas
    if (!is.null(fac) && nrow(fac) > 0)
      vetos_compuerta <- unique(c(vetos_compuerta, fac$nucleo_lexico))
    vetos_compuerta <- vetos_compuerta[nzchar(vetos_compuerta)]
    # Tercer contrato heredado (2.9.18): el nivel de exposicion. El refinamiento
    # solo optimizaba estructura, asi que un reemplazo podia entrar con una
    # deseabilidad muy distinta a la de su dimension y romper el eje 2 sin que
    # nadie lo mirara: la concordancia final solo revisa redaccion.
    d <- comp_previa$deseabilidad$deseabilidad
    if (!is.null(d) && length(d) == nrow(escala$items)) {
      # Referencia por DIMENSION (mediana), no el valor del item que sale: si
      # justamente ese item era el desviado, copiarle el nivel perpetuaria la
      # desviacion. La mediana describe a donde debe parecerse el nuevo.
      desea_compuerta <- tapply(as.numeric(d), escala$items$dimension,
                                stats::median, na.rm = TRUE)
      desea_compuerta <- desea_compuerta[!is.na(desea_compuerta)]
      if (length(desea_compuerta) == 0) desea_compuerta <- NULL
    }
  }
  if (verbose) {
    cat("  Nivel de exposicion: ",
        if (is.null(desea_compuerta))
          "sin referencia (la escala no trae deseabilidad medida)"
        else paste0("heredado por dimension (",
                    paste(sprintf("%.2f", desea_compuerta), collapse = " / "), ")"),
        "\n", sep = "")
  }
  # Umbral: por defecto el MISMO con el que la compuerta detecta (adaptativo,
  # tipicamente 0.62-0.70), menos un margen. Antes era 0.70 fijo y quedaba una
  # franja donde el refinamiento aceptaba lo que la compuerta rechazaba.
  if (identical(umbral_redundancia, "compuerta")) {
    u <- comp_previa$redaccion$parametros$umbral_sem
    u <- suppressWarnings(as.numeric(u))
    umbral_redundancia <- if (length(u) == 1 && !is.na(u))
      max(0.40, u - 0.03) else 0.70
    if (verbose) {
      cat("  Umbral de redundancia: ", sprintf("%.2f", umbral_redundancia),
          if (!is.null(comp_previa)) " (heredado de la compuerta)" else
            " (por defecto: la escala no traia compuerta)", "\n", sep = "")
    }
  }
  if (verbose && length(vetos_compuerta) > 0) {
    cat("  Vetado por la compuerta (no reintroducir): ",
        paste(vetos_compuerta, collapse = " | "), "\n", sep = "")
  }

  # Validaciones
  if (!inherits(escala, "semilla")) {
    stop("escala debe ser un objeto de clase 'semilla'")
  }
  .validar_api_key(api_key)

  if (is.null(escala$efa)) {
    stop("La escala debe tener estructura. Ejecuta precision_clasificacion() primero.")
  }

  # Configurar OpenAI
  openai <- .configurar_openai(api_key)

  # Carpeta de salida
  if (is.null(carpeta_salida)) {
    carpeta_salida <- getwd()
  }
  if (!dir.exists(carpeta_salida)) {
    dir.create(carpeta_salida, recursive = TRUE)
  }

  # Obtener informacion del concepto
  concepto <- escala$metadata$concepto_original
  idioma <- escala$metadata$idioma
  poblacion <- escala$metadata$poblacion
  definicion_concepto <- escala$concepto$definicion
  dimensiones_info <- escala$concepto$dimensiones
  caracteristicas_info <- escala$concepto$caracteristicas

  # Heredar polaridad de la escala original (default TRUE para retrocompatibilidad)
  incluir_inversos_meta <- if (!is.null(escala$metadata$incluir_inversos)) {
    isTRUE(escala$metadata$incluir_inversos)
  } else {
    TRUE
  }

  # Heredar reglas linguisticas de la escala original
  max_palabras_meta <- if (!is.null(escala$metadata$max_palabras)) {
    as.integer(escala$metadata$max_palabras)
  } else {
    18L
  }
  complejidad_meta <- if (!is.null(escala$metadata$complejidad_linguistica)) {
    as.character(escala$metadata$complejidad_linguistica)
  } else {
    "intermedio"
  }
  evitar_cuant_meta <- if (!is.null(escala$metadata$evitar_cuantificadores)) {
    isTRUE(escala$metadata$evitar_cuantificadores)
  } else {
    FALSE
  }
  tipo_resp_meta <- if (!is.null(escala$metadata$tipo_escala_respuesta)) {
    as.character(escala$metadata$tipo_escala_respuesta)
  } else {
    "frecuencia"
  }

  # Inicializar historial
  historial <- data.frame(
    iteracion = integer(),
    item_original_num = integer(),
    item_original_texto = character(),
    dimension = character(),
    item_nuevo_texto = character(),
    razon = character(),
    stringsAsFactors = FALSE
  )

  escala_actual <- escala
  iteracion <- 0
  precision_actual <- 0

  # Registro PERSISTENTE entre iteraciones de los textos ya intentados
  # para cada numero de item original. Evita bucles donde el LLM
  # regenera el mismo texto en cada iteracion.
  textos_intentados <- list()      # lista[[numero_item]] = vector de textos
  items_no_convergentes <- integer()  # numeros de items marcados como estables

  # v2.9.29 --------------------------------------------------------------
  #  Contabilidad de los reemplazos que NO se pudieron generar sin redundancia.
  #  Antes esto solo se imprimia ("Max intentos redundancia") y se perdia en el
  #  scroll; dentro de callr::r_bg no lo veia nadie. Ahora sale en el resultado:
  #  un numero alto significa que la dimension esta saturada y que lo que toca
  #  no es reescribir mas, sino reducir el numero de items.
  rechazos_redundancia <- 0L
  rechazos_detalle <- list()

  #  Cuantas veces se ha reescrito cada item (tope por item, ver max_reescrituras_item)
  veces_reescrito <- integer()

  #  Modelo de embedding de la escala. Estaba hardcodeado en el filtro de
  #  redundancia, asi que con una escala construida con otro modelo el filtro
  #  comparaba en un espacio distinto al de $similitud.
  modelo_emb_escala <- escala$metadata$modelo_embedding %||% "text-embedding-3-small"

  #  La mejor escala vista hasta ahora, para no entregar la ULTIMA vuelta si
  #  fue peor que una anterior. En una corrida real el ciclo paso por 75.0% y
  #  termino en 62.5%, y esa caida no la miraba nadie.
  mejor <- list(escala = escala, score = -Inf, iteracion = 0L)

  # Calcular precision inicial
  prec_inicial <- precision_clasificacion(escala, verbose = FALSE)
  precision_inicial <- prec_inicial$precision_global

  # Registro de la evolucion de precision para plot_evolucion_precision()
  # (paso 0 = estado inicial; cada paso siguiente = medicion durante el
  # refinamiento; el ultimo punto refleja la precision final).
  evolucion <- data.frame(Iteracion = 0L, Precision = precision_inicial,
                          stringsAsFactors = FALSE)

  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_verde("REFINAMIENTO ITERATIVO DE ESCALA"), "\n")
    cat(.linea("="), "\n")
    cat("  Concepto: ", concepto, "\n", sep = "")
    cat("  Items iniciales: ", nrow(escala$items), "\n", sep = "")
    cat("  Criterio: ", criterio, "\n", sep = "")
    if (criterio == "kmeans") {
      cat("  Umbral de precision: ", umbral_precision, "%\n", sep = "")
    } else {
      cat("  Umbral de consenso ensemble: ", umbral_consenso,
          " (", round(umbral_consenso*3), "/3 algoritmos)\n", sep = "")
    }
    cat("  Iteraciones maximas: ", max_iteraciones, "\n", sep = "")
    cat(.linea("-"), "\n\n")
  }

  # Loop de refinamiento
  while (iteracion < max_iteraciones) {
    iteracion <- iteracion + 1

    if (verbose) {
      cat(.color_azul(paste0("[ITERACION ", iteracion, "/", max_iteraciones, "]")), "\n")
    }

    # Calcular precision/consenso segun el criterio elegido
    metodo_clust <- if (criterio == "ensemble") "ensemble" else "kmeans"
    prec <- precision_clasificacion(escala_actual, metodo = metodo_clust, verbose = FALSE)
    precision_actual <- prec$precision_global
    evolucion <- rbind(evolucion,
                       data.frame(Iteracion = nrow(evolucion),
                                  Precision = precision_actual,
                                  stringsAsFactors = FALSE))

    if (verbose) {
      if (criterio == "ensemble") {
        n_alto <- sum(prec$consenso$Consenso >= umbral_consenso)
        cat("  Precision k-means: ", sprintf("%.1f", precision_actual),
            "%  |  Items con consenso >= ", umbral_consenso, ": ",
            n_alto, "/", nrow(prec$consenso), "\n", sep = "")
      } else {
        cat("  Precision actual: ", sprintf("%.1f", precision_actual), "%\n", sep = "")
      }
    }

    # Verificar si alcanzamos el umbral del criterio elegido
    umbral_alcanzado <- if (criterio == "ensemble") {
      all(prec$consenso$Consenso >= umbral_consenso)
    } else {
      precision_actual >= umbral_precision
    }
    if (umbral_alcanzado) {
      if (verbose) {
        cat("  ", .color_check(), " Umbral del criterio alcanzado!\n\n", sep = "")
      }
      break
    }

    # Obtener items problematicos segun el criterio
    asig <- prec$asignacion_clusters
    correctos_df <- prec$precision_por_dimension

    if (criterio == "ensemble") {
      # Items con consenso por debajo del umbral son problematicos.
      # Se cruza el consenso (por codigo) con la asignacion para obtener el
      # cluster asignado por el voto mayoritario del ensemble.
      cons_df <- prec$consenso
      asig$consenso <- cons_df$Consenso[match(asig$codigo, cons_df$Codigo)]
      asig$estado <- ifelse(is.na(asig$consenso) | asig$consenso < umbral_consenso,
                            "problematico", "correcto")
    } else {
      # Modo k-means clasico: item es problematico si su cluster no coincide
      # con el cluster ganador de su dimension teorica.
      asig$estado <- mapply(function(dim, clust) {
        expected <- correctos_df$Cluster_Asignado[correctos_df$Dimension == dim]
        if (length(expected) > 0 && clust == expected) "correcto" else "problematico"
      }, asig$dimension, asig$cluster)
    }

    items_problematicos <- asig[asig$estado == "problematico", ]
    n_problematicos <- nrow(items_problematicos)

    if (n_problematicos == 0) {
      if (verbose) {
        cat("  ", .color_check(), " No hay items problematicos!\n\n", sep = "")
      }
      break
    }

    if (verbose) {
      cat("  Items problematicos: ", n_problematicos, "\n", sep = "")
      cat("  Generando reemplazos...\n")
    }

    # Para cada item problematico, generar uno nuevo
    items_actuales <- escala_actual$items

    # Registro de items generados en esta iteracion (por dimension)
    # para evitar redundancia entre items nuevos de la misma dimension
    items_generados_iteracion <- list()

    for (i in seq_len(n_problematicos)) {
      item_prob <- items_problematicos[i, ]
      dim_nombre <- item_prob$dimension

      if (verbose) {
        cat("    [", i, "/", n_problematicos, "] Reemplazando item de '", dim_nombre, "'...", sep = "")
      }

      # Obtener definicion y caracteristicas de la dimension
      def_dim <- dimensiones_info[[dim_nombre]]
      if (is.null(def_dim)) def_dim <- dim_nombre

      caract_dim <- caracteristicas_info[[dim_nombre]]

      # Obtener items existentes de la misma dimension (excluyendo el problematico)
      items_misma_dim <- items_actuales$item[
        items_actuales$dimension == dim_nombre & items_actuales$item != item_prob$item
      ]

      # Agregar items ya generados en esta iteracion para la misma dimension
      if (!is.null(items_generados_iteracion[[dim_nombre]])) {
        items_misma_dim <- unique(c(items_misma_dim, items_generados_iteracion[[dim_nombre]]))
      }

      # Comparar TAMBIEN contra las otras dimensiones: la redundancia
      # inter-dimension es la mas danina (funde los factores). Evidencia
      # PM policial 2026 (n=280): los 2 pares con mayor correlacion
      # empirica (r >= .76) cruzaban dimensiones y el filtro intra-dim
      # nunca los comparo.
      items_otras_dim <- items_actuales$item[
        items_actuales$dimension != dim_nombre & items_actuales$item != item_prob$item
      ]
      items_misma_dim <- unique(c(items_misma_dim, items_otras_dim))

      # Numero del item original (para tracking persistente de textos)
      num_original <- if ("codigo" %in% names(item_prob)) {
        as.integer(sub("Item_", "", item_prob$codigo))
      } else {
        idx_orig <- which(items_actuales$item == item_prob$item)[1]
        if (length(idx_orig) > 0) items_actuales$numero[idx_orig] else NA_integer_
      }

      # Si este item ya fue marcado como no convergente en iteraciones previas,
      # saltarlo: dejarlo con el texto actual y no intentar reemplazarlo.
      if (!is.na(num_original) && num_original %in% items_no_convergentes) {
        if (verbose) cat(" no convergente (texto estable), se mantiene\n")
        next
      }

      # v2.9.29: tope de reescrituras por item. Reescribir el mismo item ocho
      # veces no arregla nada: si tras max_reescrituras_item sigue sin encajar,
      # el problema no es su redaccion. Se deja como esta y se avisa.
      if (!is.na(num_original) && !is.null(max_reescrituras_item) &&
          is.finite(max_reescrituras_item)) {
        k <- as.character(num_original)
        ya <- veces_reescrito[[k]] %||% 0L
        if (ya >= max_reescrituras_item) {
          if (verbose) cat(" tope de reescrituras alcanzado (", ya, "), se mantiene\n", sep = "")
          items_no_convergentes <- unique(c(items_no_convergentes, num_original))
          next
        }
      }

      # Cargar el historial persistente de textos previamente intentados
      # para este item original. La clave es el numero del item.
      key <- if (!is.na(num_original)) as.character(num_original) else ""
      historial_textos <- if (nzchar(key)) textos_intentados[[key]] else NULL

      # v2.9.29: el item original YA NO va en items_evitar --------------------
      #  Antes se metia ahi, es decir, bajo la cabecera "EVITA generar items
      #  similares a estos". El efecto era que al modelo se le pedia ALEJARSE
      #  del contenido que el item cubria: si el que salia decia "sin emitir
      #  juicios de valor", esa faceta se expulsaba por diseno. Medido: dos
      #  facetas declaradas del constructo se quedaron en 0 items tras refinar.
      #  Ahora el original viaja por separado, como faceta a PRESERVAR.
      items_evitar <- unique(historial_textos)

      # La faceta que cubria el item que sale. Es lo que NO puede perderse.
      faceta_orig <- if (!is.na(num_original) &&
                         "caracteristica" %in% names(items_actuales)) {
        f <- items_actuales$caracteristica[match(num_original, items_actuales$numero)]
        if (length(f) && !is.na(f) && nzchar(trimws(as.character(f)))) as.character(f) else NULL
      } else NULL

      # Arranques de frase ya saturados en la escala VIVA (se recalculan en
      # cada reemplazo, no se congelan al entrar en el bucle).
      moldes_saturados <- .prefijos_frecuentes(items_actuales$item, k = 2L, min_frac = 0.20)

      # Loop de generacion con control de redundancia
      item_aceptado <- FALSE
      intento <- 0
      duplicados_consecutivos <- 0

      while (!item_aceptado && intento < max_intentos_redundancia) {
        intento <- intento + 1

        # Generar nuevo item (heredando reglas linguisticas de la escala)
        nuevo_item <- .generar_items_dimension(
          openai = openai,
          concepto = concepto,
          dimension = dim_nombre,
          definicion_dim = def_dim,
          caracteristicas = caract_dim,
          n_items = 1,
          idioma = idioma,
          poblacion = poblacion,
          modelo = modelo,
          items_evitar = items_evitar,
          complejidad_linguistica = complejidad_meta,
          tipo_escala_respuesta = tipo_resp_meta,
          evitar_cuantificadores = evitar_cuant_meta,
          max_palabras = max_palabras_meta,
          incluir_inversos = incluir_inversos_meta,
          # v2.9.29 --------------------------------------------------------
          #  faceta_objetivo  : lo que el item nuevo TIENE que seguir midiendo
          #  items_contexto   : el resto de la dimension, desde el 1er intento
          #                     (antes solo se usaba despues, en el filtro de
          #                     coseno: el modelo escribia a ciegas y se le
          #                     rechazaba, gastando los intentos disponibles)
          #  moldes_prohibidos: arranques de frase ya saturados en la escala
          faceta_objetivo   = faceta_orig,
          items_contexto    = items_misma_dim,
          moldes_prohibidos = moldes_saturados,
          # Sin esto el refinamiento puede reconstruir la misma plantilla que la
          # compuerta acababa de podar: el consenso empirico premia justamente
          # los items parecidos entre si.
          instruccion_extra = {
            bloque_vetos <- if (length(vetos_compuerta) > 0) paste0(
              "PROHIBIDO ABSOLUTO: la compuerta pre-aplicacion ya elimino items ",
              "construidos sobre estas formulas o conductas, y no deben volver ",
              "(tampoco con sinonimos ni reformulaciones): ",
              paste0("\"", vetos_compuerta, "\"", collapse = ", "), ". ",
              "El nuevo item debe medir una manifestacion DISTINTA de su ",
              "dimension y no compartir plantilla con los demas items.\n") else ""
            # Se pasan tambien los niveles de las OTRAS dimensiones: sin eso el
            # item nuevo se acerca al centro y el contraste ENTRE dimensiones
            # cae, que es justo lo que la compuerta mide (medido 2026-08-06 en
            # los 6 constructos del curso: bajaba en las 2 corridas que
            # reescribieron items, con la herencia ya activa).
            bloque_exp <- .bloque_exposicion(
              if (!is.null(desea_compuerta) && dim_nombre %in% names(desea_compuerta))
                desea_compuerta[[dim_nombre]] else NA_real_,
              if (!is.null(desea_compuerta))
                desea_compuerta[setdiff(names(desea_compuerta), dim_nombre)] else NULL)
            extra <- paste0(bloque_vetos, bloque_exp)
            if (nzchar(extra)) extra else NULL
          }
        )

        # Anti-bucle: si el LLM devolvio un texto que ya estuvo en el historial
        # persistente o que es identico al item problematico, contar como duplicado
        if (!is.null(nuevo_item) && nrow(nuevo_item) > 0) {
          texto_gen <- trimws(tolower(nuevo_item$item[1]))
          historial_norm <- trimws(tolower(c(historial_textos, item_prob$item)))
          if (texto_gen %in% historial_norm) {
            duplicados_consecutivos <- duplicados_consecutivos + 1
            items_evitar <- unique(c(items_evitar, nuevo_item$item[1]))
            if (verbose) {
              cat("\n      Texto duplicado (intento ", intento, ", dup #",
                  duplicados_consecutivos, "), reintentando...", sep = "")
            }
            # Si el LLM devuelve duplicados 2 veces seguidas, abandonar este item
            if (duplicados_consecutivos >= 2) {
              if (verbose) cat("\n      ", .color_warning(),
                              " LLM no produce variantes nuevas. Item se mantiene.\n", sep = "")
              if (!is.na(num_original)) {
                items_no_convergentes <- unique(c(items_no_convergentes, num_original))
              }
              nuevo_item <- NULL
              break
            }
            next  # reintenta sin marcar item_aceptado
          }
        }

        # Auditar longitud y reescribir si excede (con cliente openai)
        if (!is.null(nuevo_item) && nrow(nuevo_item) > 0) {
          nuevo_item$dimension <- dim_nombre
          nuevo_item <- .auditar_longitud(
            nuevo_item, max_palabras_meta,
            verbose = FALSE,
            openai  = openai,
            modelo  = modelo,
            idioma  = idioma
          )
        }

        if (!is.null(nuevo_item) && nrow(nuevo_item) > 0) {
          nuevo_texto <- nuevo_item$item[1]

          # Verificar redundancia con items de la misma dimension
          if (length(items_misma_dim) > 0 && umbral_redundancia < 1) {
            check <- .verificar_redundancia_item(
              openai = openai,
              nuevo_item = nuevo_texto,
              items_existentes = items_misma_dim,
              umbral = umbral_redundancia,
              # v2.9.29: el modelo de embedding de la escala, no uno fijo
              modelo_embedding = modelo_emb_escala
            )

            # v2.9.29: NA = no se pudo verificar (fallo el embedding). Antes
            # esto llegaba como FALSE y el item entraba sin control.
            if (is.na(check$redundante)) {
              if (verbose) cat("\n      No verificable (",
                               check$motivo %||% "fallo el embedding",
                               "), no se acepta", sep = "")
            } else if (check$redundante) {
              # v2.9.29: se registra TAMBIEN el candidato rechazado. Antes solo
              # se anadian los items EXISTENTES con los que choco, asi que el
              # modelo podia volver a proponer casi lo mismo en el intento
              # siguiente y quemar los 3 intentos en variantes de un mismo texto.
              items_evitar <- unique(c(items_evitar, nuevo_texto, check$items_similares))
              rechazos_detalle[[length(rechazos_detalle) + 1L]] <- data.frame(
                iteracion = iteracion,
                item_num  = if (is.na(num_original)) NA_integer_ else as.integer(num_original),
                dimension = dim_nombre,
                candidato = nuevo_texto,
                similitud = round(max(check$similitudes), 3),
                choco_con = check$items_similares[which.max(check$similitudes)],
                stringsAsFactors = FALSE
              )
              if (verbose && intento < max_intentos_redundancia) {
                cat("\n      Redundante (", sprintf("%.0f%%", max(check$similitudes) * 100),
                    "), reintentando...", sep = "")
              }
            } else {
              item_aceptado <- TRUE
            }
          } else {
            item_aceptado <- TRUE
          }
        } else {
          break  # Error generando, salir del loop
        }

        Sys.sleep(0.3)  # Rate limit entre intentos
      }

      if (item_aceptado && !is.null(nuevo_item) && nrow(nuevo_item) > 0) {
        # Encontrar el item a reemplazar usando el texto del item
        # (asignacion_clusters tiene: codigo, item, dimension, cluster)
        idx <- which(items_actuales$item == item_prob$item)

        # Si no encuentra por texto exacto, intentar por codigo
        if (length(idx) == 0 && "codigo" %in% names(item_prob)) {
          # Extraer numero del codigo (formato "Item_X")
          num_str <- gsub("Item_", "", item_prob$codigo)
          num <- as.integer(num_str)
          if (!is.na(num)) {
            idx <- which(items_actuales$numero == num)
          }
        }

        if (length(idx) > 0) {
          idx <- idx[1]  # Usar el primero si hay duplicados

          # v2.9.29: contabilizar la reescritura para el tope por item
          if (!is.na(num_original)) {
            k <- as.character(num_original)
            veces_reescrito[[k]] <- (veces_reescrito[[k]] %||% 0L) + 1L
          }

          # Guardar en historial
          historial <- rbind(historial, data.frame(
            iteracion = iteracion,
            item_original_num = items_actuales$numero[idx],
            item_original_texto = item_prob$item,
            dimension = dim_nombre,
            item_nuevo_texto = nuevo_item$item[1],
            razon = if (criterio == "ensemble") {
              cs <- asig$consenso[match(item_prob$codigo, asig$codigo)]
              paste0("Consenso ensemble bajo (", round(cs, 3),
                     " < ", umbral_consenso, ", iter ", iteracion, ")")
            } else {
              paste0("Clasificado en cluster incorrecto (iter ", iteracion, ")")
            },
            stringsAsFactors = FALSE
          ))

          # Reemplazar item
          items_actuales$item[idx] <- nuevo_item$item[1]
          if (!is.null(nuevo_item$caracteristica) && "caracteristica" %in% names(items_actuales)) {
            items_actuales$caracteristica[idx] <- nuevo_item$caracteristica[1]
          }

          # Registrar texto en el historial persistente (anti-bucle)
          if (nzchar(key)) {
            textos_intentados[[key]] <- unique(c(historial_textos, nuevo_item$item[1]))
          }

          # Registrar item generado para evitar redundancia en esta iteracion
          if (is.null(items_generados_iteracion[[dim_nombre]])) {
            items_generados_iteracion[[dim_nombre]] <- nuevo_item$item[1]
          } else {
            items_generados_iteracion[[dim_nombre]] <- c(
              items_generados_iteracion[[dim_nombre]], nuevo_item$item[1]
            )
          }

          if (verbose) {
            if (intento > 1) {
              cat(" ", .color_check(), " (", intento, " intentos)\n", sep = "")
            } else {
              cat(" ", .color_check(), "\n", sep = "")
            }
          }
        } else {
          if (verbose) cat(" No encontrado\n")
        }
      } else {
        # v2.9.29: el reemplazo se descarta y el item original se CONSERVA
        # (esto ya era asi y esta bien). Lo que faltaba era contarlo: si esto
        # pasa muchas veces, la dimension no admite mas items distintos.
        if (intento >= max_intentos_redundancia) {
          rechazos_redundancia <- rechazos_redundancia + 1L
          # Ese item volveria a salir marcado en la iteracion siguiente y se
          # reintentaria indefinidamente. Se marca para no insistir.
          if (!is.na(num_original))
            items_no_convergentes <- unique(c(items_no_convergentes, num_original))
        }
        if (verbose) {
          if (intento >= max_intentos_redundancia) {
            cat(" Max intentos redundancia (se mantiene el original)\n")
          } else {
            cat(" Error generando\n")
          }
        }
      }

      Sys.sleep(0.5)  # Rate limit entre items
    }

    # Recalcular embeddings y EFA con los nuevos items
    if (verbose) {
      cat("  Recalculando embeddings y EFA...\n")
    }

    # Crear nuevo objeto con items actualizados
    items_result <- list(
      items = items_actuales,
      concepto = escala_actual$concepto,
      metadata = escala_actual$metadata
    )
    class(items_result) <- c("semilla_items", "list")

    # Recalcular embeddings
    # v2.9.29: con el modelo de la escala, no con el default. Antes, una escala
    # construida con text-embedding-3-large o con un modelo local se re-embebia
    # en otro espacio sin avisar, y a partir de ahi los indices no eran
    # comparables con los de la medicion anterior.
    emb_result <- obtener_embeddings(
      items = items_result,
      api_key = api_key,
      modelo_embedding = modelo_emb_escala,
      verbose = FALSE
    )

    # Recalcular EFA
    n_factores <- length(unique(items_actuales$dimension))
    # Recalcular clustering
    temp_escala <- list(
      items = items_actuales,
      embeddings = emb_result$embeddings,
      similitud = emb_result$similitud
    )
    class(temp_escala) <- c("semilla", "list")

    efa_result <- precision_clasificacion(
      x = temp_escala,
      n_clusters = n_factores,
      verbose = FALSE
    )

    # Actualizar escala
    escala_actual <- list(
      concepto = escala_actual$concepto,
      items = items_actuales,
      embeddings = emb_result$embeddings,
      similitud = emb_result$similitud,
      efa = efa_result,
      metadata = escala_actual$metadata
    )
    class(escala_actual) <- c("semilla", "list")

    # v2.9.29: guardar la MEJOR vuelta vista hasta ahora ---------------------
    #  El bucle es un paseo aleatorio: cada reemplazo mueve los embeddings, la
    #  particion se reorganiza y aparecen items problematicos que antes no lo
    #  eran. En una corrida real la precision fue 66.7 -> 70.8 -> 75.0 -> 70.8
    #  -> 66.7 -> 75.0 -> 75.0 -> 66.7 -> 66.7 -> 62.5, y se entregaba el 62.5
    #  porque era la ultima. El score penaliza la redundancia: subir la
    #  precision homogeneizando los items no puede salir gratis.
    score_actual <- tryCatch({
      red_it <- auditar_redundancia(escala_actual)
      n_it   <- nrow(escala_actual$items)
      n_pares_pos <- max(1, n_it * (n_it - 1) / 2)
      (efa_result$precision_global / 100) -
        0.5 * (nrow(red_it$pares_redundantes) / n_pares_pos) -
        0.1 * nrow(red_it$facetas_repetidas)
    }, error = function(e) efa_result$precision_global / 100)

    if (is.finite(score_actual) && score_actual > mejor$score) {
      mejor <- list(escala = escala_actual, score = score_actual, iteracion = iteracion)
    }

    if (verbose) {
      cat("  ", .color_check(), " Iteracion completada\n\n", sep = "")
    }
  }

  # v2.9.29: entregar la mejor vuelta, no la ultima
  iteracion_elegida <- iteracion
  if (isTRUE(devolver_mejor) && is.finite(mejor$score) && mejor$iteracion > 0 &&
      !identical(mejor$escala$items$item, escala_actual$items$item)) {
    if (verbose) {
      cat("  Se entrega la iteracion ", mejor$iteracion,
          " (la mejor vista), no la ultima (", iteracion, ").\n", sep = "")
    }
    escala_actual <- mejor$escala
    iteracion_elegida <- mejor$iteracion
  }

  # Calcular precision final
  prec_final <- precision_clasificacion(escala_actual, verbose = FALSE)

  # Cerrar la evolucion con la precision final si difiere del ultimo punto
  if (abs(utils::tail(evolucion$Precision, 1) - prec_final$precision_global) > 1e-9) {
    evolucion <- rbind(evolucion,
                       data.frame(Iteracion = nrow(evolucion),
                                  Precision = prec_final$precision_global,
                                  stringsAsFactors = FALSE))
  }

  # Resumen final
  if (verbose) {
    cat(.linea("="), "\n")
    cat(.color_verde("RESUMEN DE REFINAMIENTO"), "\n")
    cat(.linea("="), "\n")
    cat("  Iteraciones realizadas: ", iteracion, "\n", sep = "")
    cat("  Items reemplazados: ", nrow(historial), "\n", sep = "")
    cat("  Precision inicial: ", sprintf("%.1f", precision_inicial), "%\n", sep = "")
    cat("  Precision final: ", sprintf("%.1f", prec_final$precision_global), "%\n", sep = "")
    if (!identical(iteracion_elegida, iteracion))
      cat("  Se entrego la iteracion ", iteracion_elegida, " (la mejor), no la ", iteracion, "\n", sep = "")
    # v2.9.29: lo que antes se perdia en el scroll
    if (rechazos_redundancia > 0) {
      cat(.linea("-"), "\n")
      cat("  [!] ", rechazos_redundancia, " reemplazo(s) no se pudieron generar sin\n", sep = "")
      cat("      parecerse a los items que ya existen. Esos items se quedaron\n")
      cat("      como estaban. Es senal de que la dimension esta SATURADA: lo\n")
      cat("      que toca no es reescribir mas, sino reducir su numero de items.\n")
    }
    # v2.9.29: si el refinamiento borro una faceta declarada, decirlo aqui
    cob_fin <- tryCatch(auditar_cobertura_facetas(escala_actual, verbose = FALSE),
                        error = function(e) NULL)
    if (!is.null(cob_fin) && isTRUE(cob_fin$disponible) && nrow(cob_fin$huerfanas)) {
      cat(.linea("-"), "\n")
      cat("  [!] FACETAS DEL CONSTRUCTO QUE SE QUEDARON SIN ITEMS:\n")
      for (i in seq_len(nrow(cob_fin$huerfanas)))
        cat("      - ", cob_fin$huerfanas$dimension[i], " / ",
            cob_fin$huerfanas$faceta[i], "\n", sep = "")
      cat("      La escala ya no mide lo que su propia definicion promete.\n")
    }
    cat(.linea("-"), "\n")
  }

  # Exportar a Excel
  if (exportar_excel && nrow(historial) > 0) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      archivo_excel <- file.path(carpeta_salida,
                                 paste0("refinamiento_", gsub(" ", "_", concepto), ".xlsx"))

      wb <- openxlsx::createWorkbook()

      # Hoja 1: Items finales
      openxlsx::addWorksheet(wb, "Items_Finales")
      openxlsx::writeData(wb, "Items_Finales", escala_actual$items)

      # Hoja 2: Historial de cambios
      openxlsx::addWorksheet(wb, "Historial_Cambios")
      openxlsx::writeData(wb, "Historial_Cambios", historial)

      # Hoja 3: Resumen
      resumen <- data.frame(
        Metrica = c("Concepto", "Iteraciones", "Items reemplazados",
                    "Precision inicial", "Precision final", "Fecha"),
        Valor = c(concepto, iteracion, nrow(historial),
                  paste0(sprintf("%.1f", precision_inicial), "%"),
                  paste0(sprintf("%.1f", prec_final$precision_global), "%"),
                  as.character(Sys.time()))
      )
      openxlsx::addWorksheet(wb, "Resumen")
      openxlsx::writeData(wb, "Resumen", resumen)

      openxlsx::saveWorkbook(wb, archivo_excel, overwrite = TRUE)

      if (verbose) {
        cat("  ", .color_check(), " Resultados exportados a: ", archivo_excel, "\n", sep = "")
      }
    }
  }

  # ---------------------------------------------------------------------------
  #  CIERRE: re-verificar el EJE 1 de la compuerta (barato: sin LLM ni
  #  simulacion, solo la auditoria de redaccion sobre los embeddings nuevos).
  #  Responde a la pregunta "el refinamiento reintrodujo lo que la compuerta
  #  habia podado?" en segundos, en vez de exigir re-pasar la compuerta entera.
  # ---------------------------------------------------------------------------
  concordancia <- NULL
  if (!is.null(comp_previa)) {
    red_post <- tryCatch(
      auditar_redundancia(escala_actual, umbral_sem = "auto"),
      error = function(e) NULL)
    if (!is.null(red_post)) {
      red_pre <- comp_previa$redaccion
      n_fac_pre  <- if (!is.null(red_pre$facetas_repetidas)) nrow(red_pre$facetas_repetidas) else 0L
      n_fac_post <- nrow(red_post$facetas_repetidas)
      n_par_pre  <- if (!is.null(red_pre$pares_redundantes)) nrow(red_pre$pares_redundantes) else 0L
      n_par_post <- nrow(red_post$pares_redundantes)
      # Un nucleo lexico vetado que reaparece es la senal mas clara de que el
      # refinamiento deshizo el trabajo de la compuerta.
      reincidentes <- if (n_fac_post > 0 && length(vetos_compuerta) > 0)
        intersect(red_post$facetas_repetidas$nucleo_lexico, vetos_compuerta) else character(0)

      concordancia <- list(
        facetas_antes = n_fac_pre,   facetas_despues = n_fac_post,
        pares_antes   = n_par_pre,   pares_despues   = n_par_post,
        nucleos_reincidentes = reincidentes,
        vetados = vetos_compuerta,
        umbral_usado = umbral_redundancia,
        # concuerda = el refinamiento no empeoro el eje 1 ni resucito un veto
        concuerda = (n_fac_post <= n_fac_pre) && (n_par_post <= n_par_pre) &&
                    length(reincidentes) == 0,
        redaccion_post = red_post)

      if (verbose) {
        cat("\n  CONCORDANCIA CON LA COMPUERTA (eje 1):\n")
        cat(sprintf("    facetas repetidas: %d -> %d\n", n_fac_pre, n_fac_post))
        cat(sprintf("    pares redundantes: %d -> %d\n", n_par_pre, n_par_post))
        if (length(reincidentes) > 0)
          cat("    ", .color_warning(), " REAPARECIERON nucleos vetados: ",
              paste(reincidentes, collapse = ", "), "\n", sep = "")
        cat(if (concordancia$concuerda)
              "    [OK] El refinamiento no deshizo el trabajo de la compuerta.\n"
            else
              "    [!] El refinamiento DEGRADO el eje 1: vuelve a pasar la compuerta.\n")
      }
    }
  }

  # La compuerta viaja con la escala, marcada como obsoleta: antes se perdia al
  # devolver un objeto nuevo, y el paso siguiente (y el script reproducible)
  # se quedaban sin ella sin que nadie lo notara.
  if (!is.null(comp_previa)) {
    escala_actual$compuerta <- comp_previa
    escala_actual$compuerta_obsoleta <- TRUE
    escala_actual$concordancia_refinamiento <- concordancia
  }

  # Resultado
  resultado <- list(
    escala_final = escala_actual,
    historial = historial,
    iteraciones = iteracion,
    precision_inicial = precision_inicial,
    precision_final = prec_final$precision_global,
    evolucion = evolucion,
    concordancia = concordancia,
    items_no_convergentes = items_no_convergentes,
    textos_intentados = textos_intentados,
    # v2.9.29 ---------------------------------------------------------------
    #  iteracion_elegida : cual se entrego (puede no ser la ultima)
    #  rechazos_redundancia : cuantos reemplazos no se pudieron generar sin
    #    parecerse a lo que ya habia. Un numero alto NO es un fallo tecnico: es
    #    el sintoma de que la dimension ya no admite mas items distintos, y de
    #    que lo que toca es reducir su numero de items, no reescribirlos.
    #  rechazos_detalle : que candidato choco con que item y con que similitud
    #  veces_reescrito : cuantas veces se toco cada item
    #  cobertura : facetas declaradas que siguen cubiertas al final
    iteracion_elegida = iteracion_elegida,
    rechazos_redundancia = rechazos_redundancia,
    rechazos_detalle = if (length(rechazos_detalle))
      do.call(rbind, rechazos_detalle) else NULL,
    veces_reescrito = veces_reescrito,
    cobertura = tryCatch(auditar_cobertura_facetas(escala_actual, verbose = FALSE),
                         error = function(e) NULL)
  )

  class(resultado) <- c("semilla_refinamiento", "list")

  return(resultado)
}


# =============================================================================
# DISPATCHER UNIFICADO POR FORMATO (SeMiLLa v2.0)
# =============================================================================

#' @title Generar items segun formato del instrumento (interfaz v2.0)
#'
#' @description
#' Dispatcher unificado para crear items en cualquiera de los seis formatos
#' soportados por SeMiLLa. Sustituye las seis funciones especializadas
#' (\code{generar_escala()}, \code{generar_escala_historias()}, etc.), que
#' siguen disponibles como alias por retrocompatibilidad.
#'
#' @param tipo Tipo de instrumento a generar. Uno de:
#'   \code{"likert"} (default, escala Likert clasica),
#'   \code{"historias"} (vignette-based),
#'   \code{"guttman"} (construct map),
#'   \code{"objetiva"} (multiple choice de conocimiento),
#'   \code{"cognitivo"} (test cronometrado tipo OSPAN),
#'   \code{"forced_choice"} (Thurstoniano).
#' @param ... Argumentos especificos del tipo. Vea la documentacion de
#'   la funcion subyacente que se invoca segun \code{tipo}:
#'   \itemize{
#'     \item likert \code{->} \code{?generar_escala}
#'     \item historias \code{->} \code{?generar_escala_historias}
#'     \item guttman \code{->} \code{?generar_escala_guttman}
#'     \item objetiva \code{->} \code{?generar_prueba_objetiva}
#'     \item cognitivo \code{->} \code{?generar_test_cognitivo}
#'     \item forced_choice \code{->} \code{?generar_escala_forcedchoice}
#'   }
#'
#' @return Objeto del tipo correspondiente.
#'
#' @examples
#' \dontrun{
#' # Likert clasico
#' esc <- generar_items(tipo = "likert",
#'                       concepto = "resiliencia",
#'                       api_key  = Sys.getenv("OPENAI_API_KEY"),
#'                       n_items  = 32, n_dimensiones = 4)
#'
#' # Forced-choice
#' esc <- generar_items(tipo = "forced_choice",
#'                       concepto = "personalidad laboral",
#'                       api_key  = Sys.getenv("OPENAI_API_KEY"))
#' }
#'
#' @export
generar_items <- function(tipo = c("likert","historias","guttman",
                                    "objetiva","cognitivo","forced_choice"),
                          ...) {
  tipo <- match.arg(tipo)
  fn <- switch(tipo,
    "likert"        = generar_escala,
    "historias"     = generar_escala_historias,
    "guttman"       = generar_escala_guttman,
    "objetiva"      = generar_prueba_objetiva,
    "cognitivo"     = generar_test_cognitivo,
    "forced_choice" = generar_escala_forcedchoice
  )
  fn(...)
}
