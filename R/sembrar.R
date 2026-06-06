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
      incluir_inversos = incluir_inversos
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
#' @param umbral_redundancia Similitud maxima permitida entre items 0-1 (default: 0.85).
#'        Items nuevos mas similares que este umbral seran regenerados.
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
                           umbral_redundancia = 0.85,
                           max_intentos_redundancia = 3,
                           modelo = "gpt-4.1-mini",
                           exportar_excel = TRUE,
                           carpeta_salida = NULL,
                           verbose = TRUE) {

  criterio <- match.arg(criterio)
  if (umbral_consenso < 0 || umbral_consenso > 1) {
    stop("umbral_consenso debe estar entre 0 y 1")
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

      # Cargar el historial persistente de textos previamente intentados
      # para este item original. La clave es el numero del item.
      key <- if (!is.na(num_original)) as.character(num_original) else ""
      historial_textos <- if (nzchar(key)) textos_intentados[[key]] else NULL

      # Sembrar items_evitar con todo el historial previo
      items_evitar <- unique(c(historial_textos, item_prob$item))

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
          incluir_inversos = incluir_inversos_meta
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
              umbral = umbral_redundancia
            )

            if (check$redundante) {
              # Agregar items similares a la lista de evitar
              items_evitar <- unique(c(items_evitar, check$items_similares))
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
        if (verbose) {
          if (intento >= max_intentos_redundancia) {
            cat(" Max intentos redundancia\n")
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
    emb_result <- obtener_embeddings(
      items = items_result,
      api_key = api_key,
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

    if (verbose) {
      cat("  ", .color_check(), " Iteracion completada\n\n", sep = "")
    }
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

  # Resultado
  resultado <- list(
    escala_final = escala_actual,
    historial = historial,
    iteraciones = iteracion,
    precision_inicial = precision_inicial,
    precision_final = prec_final$precision_global,
    evolucion = evolucion,
    items_no_convergentes = items_no_convergentes,
    textos_intentados = textos_intentados
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
