# =============================================================================
# SeMiLLa - Funciones Internas (Utilities)
# =============================================================================


# =============================================================================
# FUNCION PUBLICA: LEER ESCALA DESDE EXCEL/CSV
# =============================================================================

#' @title Leer Escala desde Archivo Excel o CSV
#'
#' @description
#' Lee una escala existente desde un archivo Excel (.xlsx) o CSV y la transforma
#' al formato requerido por `validar_escala()`. Esto facilita enormemente el
#' ingreso de datos para usuarios no familiarizados con la sintaxis de R.
#'
#' @param archivo Ruta al archivo Excel (.xlsx) o CSV (.csv)
#' @param hoja Para archivos Excel, nombre o numero de la hoja (default: 1)
#'
#' @return Lista con tres elementos listos para usar en `validar_escala()`:
#' \itemize{
#'   \item \code{nombre}: Nombre del constructo (extraido de la primera dimension)
#'   \item \code{definicion}: Definicion operacional del constructo
#'   \item \code{dimensiones}: Lista estructurada con dimensiones e items
#' }
#'
#' @details
#' El archivo debe tener las siguientes columnas (en cualquier orden):
#' \itemize{
#'   \item \code{dimension}: Nombre de la dimension a la que pertenece el item
#'   \item \code{definicion_dimension}: Definicion de la dimension
#'   \item \code{codigo}: Codigo unico del item (ej: "RP1", "RP2")
#'   \item \code{item}: Texto del item
#' }
#'
#' Opcionalmente puede incluir:
#' \itemize{
#'   \item \code{constructo}: Nombre del constructo (si no se especifica, se usa el nombre del archivo)
#'   \item \code{definicion_constructo}: Definicion operacional del constructo
#' }
#'
#' Puedes generar una plantilla de ejemplo con `crear_plantilla_escala()`.
#'
#' @examples
#' \dontrun{
#' # Leer escala desde Excel
#' escala_data <- leer_escala("mi_escala.xlsx")
#'
#' # Usar directamente en validar_escala
#' resultado <- validar_escala(
#'   nombre = escala_data$nombre,
#'   definicion = escala_data$definicion,
#'   dimensiones = escala_data$dimensiones,
#'   api_key = Sys.getenv("OPENAI_API_KEY")
#' )
#'
#' # O de forma mas compacta con do.call
#' escala_data <- leer_escala("mi_escala.xlsx")
#' resultado <- do.call(validar_escala, c(escala_data, list(api_key = mi_api_key)))
#' }
#'
#' @seealso \code{validar_escala()}, \code{\link{crear_plantilla_escala}}
#'
#' @export
leer_escala <- function(archivo, hoja = 1) {

  # Validar que el archivo existe

  if (!file.exists(archivo)) {
    stop("El archivo no existe: ", archivo)
  }

  # Detectar extension

  extension <- tolower(tools::file_ext(archivo))

  # Leer archivo segun extension
  if (extension == "xlsx" || extension == "xls") {
    # Verificar que readxl este disponible
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Para leer archivos Excel necesitas instalar readxl:\n",
           "  install.packages('readxl')")
    }
    datos <- readxl::read_excel(archivo, sheet = hoja)

  } else if (extension == "csv") {
    datos <- utils::read.csv(archivo, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  } else {
    stop("Formato no soportado: ", extension, "\n",
         "Usa archivos .xlsx, .xls o .csv")
  }

  # Convertir a data.frame y limpiar nombres de columnas
  datos <- as.data.frame(datos)
  nombres_cols <- tolower(trimws(names(datos)))
  nombres_cols <- gsub("[^a-z0-9_]", "_", nombres_cols)
  names(datos) <- nombres_cols

  # Verificar columnas requeridas
  cols_requeridas <- c("dimension", "definicion_dimension", "codigo", "item")
  cols_faltantes <- setdiff(cols_requeridas, nombres_cols)

  if (length(cols_faltantes) > 0) {
    stop("Faltan columnas requeridas en el archivo:\n",
         "  Faltantes: ", paste(cols_faltantes, collapse = ", "), "\n",
         "  Encontradas: ", paste(names(datos), collapse = ", "), "\n\n",
         "El archivo debe tener las columnas:\n",
         "  - dimension: Nombre de la dimension\n",
         "  - definicion_dimension: Definicion de la dimension\n",
         "  - codigo: Codigo del item (ej: RP1, RP2)\n",
         "  - item: Texto del item")
  }

  # Extraer nombre del constructo
  if ("constructo" %in% nombres_cols && !is.na(datos$constructo[1]) && datos$constructo[1] != "") {
    nombre_constructo <- datos$constructo[1]
  } else {
    # Usar nombre del archivo sin extension
    nombre_constructo <- tools::file_path_sans_ext(basename(archivo))
    nombre_constructo <- gsub("_", " ", nombre_constructo)
    nombre_constructo <- gsub("\\s+", " ", nombre_constructo)
  }

  # Extraer definicion del constructo

  if ("definicion_constructo" %in% nombres_cols && !is.na(datos$definicion_constructo[1]) && datos$definicion_constructo[1] != "") {
    definicion_constructo <- datos$definicion_constructo[1]
  } else {
    # Generar definicion generica
    definicion_constructo <- paste0("Constructo psicologico que comprende ",
                                     length(unique(datos$dimension)), " dimensiones.")
  }

  # Construir estructura de dimensiones
  dimensiones <- list()
  dims_unicas <- unique(datos$dimension)

  for (dim_nombre in dims_unicas) {
    # Filtrar filas de esta dimension
    filas_dim <- datos[datos$dimension == dim_nombre, ]

    # Obtener definicion de la dimension
    def_dim <- filas_dim$definicion_dimension[1]
    if (is.na(def_dim) || def_dim == "") {
      def_dim <- paste0("Dimension que mide aspectos de ", dim_nombre)
    }

    # Construir vector de items nombrado
    items_vec <- filas_dim$item
    names(items_vec) <- filas_dim$codigo

    # Limpiar textos
    items_vec <- trimws(items_vec)
    def_dim <- trimws(gsub("\\s+", " ", def_dim))

    # Agregar a la lista
    dimensiones[[dim_nombre]] <- list(
      definicion = def_dim,
      items = items_vec
    )
  }

  # Mostrar resumen
  cat("\n")
  cat("=== ESCALA CARGADA EXITOSAMENTE ===\n\n")
  cat("  Constructo: ", nombre_constructo, "\n", sep = "")
  cat("  Dimensiones: ", length(dimensiones), "\n", sep = "")
  cat("  Items totales: ", nrow(datos), "\n\n", sep = "")

  for (dim_nombre in names(dimensiones)) {
    n_items <- length(dimensiones[[dim_nombre]]$items)
    cat("  [", n_items, " items] ", dim_nombre, "\n", sep = "")
  }
  cat("\n")
  cat("Usa estos datos con validar_escala():\n\n")
  cat("  resultado <- validar_escala(\n")
  cat("    nombre = escala$nombre,\n")
  cat("    definicion = escala$definicion,\n")
  cat("    dimensiones = escala$dimensiones,\n")
  cat("    api_key = tu_api_key\n")
  cat("  )\n\n")

  # Retornar lista
  resultado <- list(
    nombre = nombre_constructo,
    definicion = definicion_constructo,
    dimensiones = dimensiones
  )

  return(resultado)
}


#' @title Crear Plantilla de Escala para Excel
#'
#' @description
#' Genera un archivo Excel o CSV de plantilla que el usuario puede llenar
#' con los datos de su escala. Incluye un ejemplo con algunos items.
#'
#' @param archivo Ruta donde guardar la plantilla (incluir extension .xlsx o .csv)
#' @param ejemplo Incluir datos de ejemplo (default: TRUE)
#'
#' @return Invisible. Crea el archivo en la ruta especificada.
#'
#' @examples
#' \dontrun{
#' # Crear plantilla Excel
#' crear_plantilla_escala("mi_escala_plantilla.xlsx")
#'
#' # Crear plantilla CSV
#' crear_plantilla_escala("mi_escala_plantilla.csv")
#'
#' # Crear plantilla vacia (solo encabezados)
#' crear_plantilla_escala("plantilla_vacia.xlsx", ejemplo = FALSE)
#' }
#'
#' @seealso \code{\link{leer_escala}}
#'
#' @export
crear_plantilla_escala <- function(archivo, ejemplo = TRUE) {

  extension <- tolower(tools::file_ext(archivo))

  if (ejemplo) {
    # Crear datos de ejemplo
    datos <- data.frame(
      constructo = c(
        "Resolucion de Problemas",
        rep("", 14)
      ),
      definicion_constructo = c(
        "Proceso cognitivo complejo que implica identificar, generar, evaluar, seleccionar y verificar soluciones para abordar eficazmente un problema.",
        rep("", 14)
      ),
      dimension = c(
        rep("Analisis y planificacion", 4),
        rep("Evaluacion critica", 4),
        rep("Generacion de alternativas", 3),
        rep("Priorizacion y revision", 4)
      ),
      definicion_dimension = c(
        rep("Implica generar alternativas, establecer metas y evaluar si las soluciones propuestas resuelven efectivamente el problema.", 4),
        rep("Incluye evaluar los resultados obtenidos, identificar obstaculos y fallas, y generar nuevas ideas o ajustes.", 4),
        rep("Se refiere a crear multiples opciones de solucion y evaluar las consecuencias de cada una.", 3),
        rep("Consiste en seleccionar las alternativas mas relevantes y revisarlas constantemente.", 4)
      ),
      codigo = c(
        "RP1", "RP2", "RP3", "RP4",
        "RP5", "RP6", "RP7", "RP8",
        "RP9", "RP10", "RP11",
        "RP12", "RP13", "RP14", "RP15"
      ),
      item = c(
        "Hago una lista de todas las alternativas.",
        "Verifico si la solucion resuelve el problema.",
        "Comparo las alternativas seleccionadas.",
        "Establezco metas para entender el problema.",
        "Evaluo los resultados obtenidos.",
        "Identifico los obstaculos del problema.",
        "Propongo ideas antes de decidir.",
        "Analizo por que la solucion fallo.",
        "Creo la mayor cantidad de alternativas.",
        "Considero el impacto en otras personas.",
        "Considero las consecuencias a corto y largo plazo.",
        "Priorizo las alternativas segun su impacto.",
        "Verifico si las alternativas cumplen los objetivos.",
        "Reevaluo la informacion para asegurar comprension.",
        "Evaluo alternativas basandome en experiencias previas."
      ),
      stringsAsFactors = FALSE
    )
  } else {
    # Plantilla vacia con solo encabezados y una fila de ejemplo
    datos <- data.frame(
      constructo = "Nombre de tu constructo aqui",
      definicion_constructo = "Definicion operacional del constructo...",
      dimension = "Nombre de la dimension",
      definicion_dimension = "Definicion de esta dimension...",
      codigo = "I1",
      item = "Texto del primer item...",
      stringsAsFactors = FALSE
    )
  }

  # Guardar segun extension
  if (extension == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Para crear archivos Excel necesitas instalar writexl:\n",
           "  install.packages('writexl')")
    }
    writexl::write_xlsx(datos, archivo)

  } else if (extension == "csv") {
    utils::write.csv(datos, archivo, row.names = FALSE, fileEncoding = "UTF-8")

  } else {
    stop("Extension no soportada: ", extension, "\n",
         "Usa .xlsx o .csv")
  }

  cat("\n")
  cat("=== PLANTILLA CREADA ===\n\n")
  cat("  Archivo: ", archivo, "\n\n", sep = "")
  cat("INSTRUCCIONES:\n\n")
  cat("  1. Abre el archivo en Excel o LibreOffice\n")
  cat("  2. Modifica los datos con tu escala\n")
  cat("  3. Guarda el archivo\n")
  cat("  4. Cargalo en R con:\n\n")
  cat("     escala <- leer_escala(\"", basename(archivo), "\")\n\n", sep = "")
  cat("COLUMNAS REQUERIDAS:\n\n")
  cat("  - constructo: Nombre del constructo (solo en fila 1)\n")
  cat("  - definicion_constructo: Definicion operacional (solo en fila 1)\n")
  cat("  - dimension: Nombre de la dimension del item\n")
  cat("  - definicion_dimension: Definicion de esa dimension\n")
  cat("  - codigo: Codigo unico del item (ej: RP1, RP2)\n")
  cat("  - item: Texto completo del item\n\n")

  invisible(datos)
}


# =============================================================================
# HELPER INTERNO: CARGAR ITEMS PROVISTOS POR EL USUARIO
# =============================================================================
#
# Construye el objeto `items_result` con el mismo shape que devuelve
# generar_escala() pero sin pasar por el LLM. Lo usa semilla(fuente="usuario").
#
# Acepta UNO de estos inputs (prioridad en orden):
#   - archivo:     ruta a .xlsx o .csv (usa leer_escala() internamente)
#   - items_df:    data.frame con columnas item, dimension y opcionalmente
#                  codigo, definicion_dimension, constructo, definicion_constructo
#   - dimensiones: lista nombrada {dim -> list(definicion, items=c(cod=texto))}
#                  igual a la que acepta validar_escala() o que devuelve leer_escala()
#
# Devuelve: list(items=data.frame, concepto=list, metadata=list)
#           con class c("semilla_items", "list")

#' @keywords internal
.cargar_items_usuario <- function(concepto    = NULL,
                                  definicion  = NULL,
                                  archivo     = NULL,
                                  items_df    = NULL,
                                  dimensiones = NULL,
                                  idioma      = "es",
                                  poblacion   = NULL,
                                  hoja        = 1,
                                  verbose     = TRUE) {

  # 1. Resolver la fuente real de items (archivo > items_df > dimensiones)
  if (!is.null(archivo)) {
    if (verbose) cat("  ", .color_flecha(), " Leyendo items desde archivo: ", archivo, "\n", sep = "")
    leido <- leer_escala(archivo, hoja = hoja)
    # leer_escala() ya devuelve nombre/definicion/dimensiones validados
    if (is.null(concepto)  || nchar(concepto)  == 0) concepto  <- leido$nombre
    if (is.null(definicion)|| nchar(definicion)== 0) definicion <- leido$definicion
    dimensiones <- leido$dimensiones

  } else if (!is.null(items_df)) {
    if (!is.data.frame(items_df)) stop("'items_df' debe ser un data.frame")
    nombres_cols <- tolower(trimws(names(items_df)))
    nombres_cols <- gsub("[^a-z0-9_]", "_", nombres_cols)
    names(items_df) <- nombres_cols

    cols_req <- c("item", "dimension")
    falt <- setdiff(cols_req, names(items_df))
    if (length(falt) > 0) {
      stop("Faltan columnas obligatorias en items_df: ", paste(falt, collapse = ", "))
    }
    # Reconstruir dimensiones desde el data.frame
    if (!"codigo" %in% names(items_df)) {
      items_df$codigo <- paste0("I", seq_len(nrow(items_df)))
    }
    if (!"definicion_dimension" %in% names(items_df)) {
      items_df$definicion_dimension <- ""
    }
    dims_unicas <- unique(items_df$dimension)
    dimensiones <- lapply(dims_unicas, function(d) {
      filas <- items_df[items_df$dimension == d, , drop = FALSE]
      items_vec <- trimws(as.character(filas$item))
      names(items_vec) <- filas$codigo
      def_d <- filas$definicion_dimension[1]
      if (is.na(def_d) || def_d == "") def_d <- paste0("Dimension que mide aspectos de ", d)
      list(definicion = trimws(def_d), items = items_vec)
    })
    names(dimensiones) <- dims_unicas

    if ((is.null(concepto)  || nchar(concepto)  == 0) && "constructo" %in% names(items_df)) {
      concepto <- items_df$constructo[1]
    }
    if ((is.null(definicion)|| nchar(definicion)== 0) && "definicion_constructo" %in% names(items_df)) {
      definicion <- items_df$definicion_constructo[1]
    }

  } else if (!is.null(dimensiones)) {
    if (!is.list(dimensiones) || length(dimensiones) == 0) {
      stop("'dimensiones' debe ser una lista con al menos una dimension")
    }
  } else {
    stop("fuente = 'usuario' requiere uno de: 'archivo', 'items_df' o 'dimensiones'")
  }

  # 2. Validar estructura de dimensiones
  for (dim_nombre in names(dimensiones)) {
    d <- dimensiones[[dim_nombre]]
    if (!is.list(d) || is.null(d$items) || length(d$items) == 0) {
      stop("La dimension '", dim_nombre, "' no tiene items o tiene estructura invalida.\n",
           "  Cada dimension debe ser una lista con 'definicion' e 'items'.")
    }
    if (is.null(d$definicion) || is.na(d$definicion) || d$definicion == "") {
      dimensiones[[dim_nombre]]$definicion <- paste0("Dimension que mide aspectos de ", dim_nombre)
    }
  }

  # 3. Defaults para concepto / definicion
  if (is.null(concepto) || nchar(concepto) == 0) {
    concepto <- "Constructo provisto por el usuario"
  }
  if (is.null(definicion) || nchar(definicion) == 0) {
    definicion <- paste0("Constructo psicologico provisto por el usuario, compuesto por ",
                         length(dimensiones), " dimensiones.")
  }

  # 4. Construir data.frame de items (mismo shape que generar_escala)
  todos_items <- data.frame()
  dims_info   <- list()
  numero_item <- 1

  for (dim_nombre in names(dimensiones)) {
    d <- dimensiones[[dim_nombre]]
    dims_info[[dim_nombre]] <- d$definicion

    items_dim <- d$items
    if (!is.null(names(items_dim)) && all(nzchar(names(items_dim)))) {
      codigos <- names(items_dim)
    } else {
      codigos <- paste0("I", numero_item:(numero_item + length(items_dim) - 1))
    }

    for (j in seq_along(items_dim)) {
      todos_items <- rbind(todos_items, data.frame(
        numero    = numero_item,
        codigo    = codigos[j],
        dimension = dim_nombre,
        item      = as.character(items_dim[j]),
        stringsAsFactors = FALSE
      ))
      numero_item <- numero_item + 1
    }
  }

  # 5. Construir objeto concepto (shape de info_concepto que produce sembrar.R)
  info_concepto <- list(
    concepto       = concepto,
    definicion     = definicion,
    dimensiones    = dims_info,
    caracteristicas = NULL,
    teorias        = NULL,
    modelos        = NULL,
    referencias    = NULL,
    fuente         = "usuario"
  )

  # 6. Empaquetar y devolver
  resultado <- list(
    items    = todos_items,
    concepto = info_concepto,
    metadata = list(
      concepto_original   = concepto,
      idioma              = idioma,
      poblacion           = poblacion,
      modelo              = NA,
      n_items_solicitados = nrow(todos_items),
      n_items_generados   = nrow(todos_items),
      fuente              = "usuario",
      fecha               = Sys.time()
    )
  )
  class(resultado) <- c("semilla_items", "list")

  if (verbose) {
    cat("\n", .linea("-"), "\n", sep = "")
    cat(.color_verde("ITEMS CARGADOS (sin LLM)"), "\n")
    cat(.linea("-"), "\n")
    cat("  Constructo:   ", concepto, "\n", sep = "")
    cat("  Dimensiones:  ", length(dimensiones), "\n", sep = "")
    cat("  Items totales:", nrow(todos_items), "\n", sep = "")
    for (dim_nombre in names(dimensiones)) {
      n_items <- sum(todos_items$dimension == dim_nombre)
      cat("    [", n_items, " items] ", dim_nombre, "\n", sep = "")
    }
    cat(.linea("-"), "\n\n")
  }

  return(resultado)
}


# -----------------------------------------------------------------------------
# VALIDACIONES
# -----------------------------------------------------------------------------

#' @keywords internal
.validar_concepto <- function(concepto) {
  if (missing(concepto) || is.null(concepto)) {
    stop("Debes proporcionar un concepto psicologico")
  }
  if (!is.character(concepto) || nchar(trimws(concepto)) < 3) {
    stop("El concepto debe ser un texto de al menos 3 caracteres")
  }
  invisible(TRUE)
}

#' @keywords internal
.validar_api_key <- function(api_key) {
  if (missing(api_key) || is.null(api_key)) {
    stop("Debes proporcionar tu API key de OpenAI")
  }
  if (!is.character(api_key) || nchar(api_key) < 20) {
    stop("API key invalida. Debe ser una cadena de texto valida de OpenAI")
  }
  invisible(TRUE)
}

#' @keywords internal
.validar_idioma <- function(idioma) {
  idiomas_validos <- c("es", "en", "pt")
  if (!idioma %in% idiomas_validos) {
    stop("Idioma no soportado. Usa: ", paste(idiomas_validos, collapse = ", "))
  }
  invisible(TRUE)
}

#' @keywords internal
.validar_n_items <- function(n_items) {
  if (!is.numeric(n_items) || n_items < 5) {
    stop("n_items debe ser un numero >= 5")
  }
  if (n_items > 100) {
    warning("Generar mas de 100 items puede ser costoso y lento")
  }
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# FUNCIONES AUXILIARES MATEMATICAS
# -----------------------------------------------------------------------------

#' Hacer una matriz definida positiva
#' @keywords internal
.hacer_definida_positiva <- function(mat, tol = 1e-6) {
  # Descomposicion en valores propios
  eig <- eigen(mat, symmetric = TRUE)
  valores <- eig$values
  vectores <- eig$vectors

 # Reemplazar valores propios negativos o muy pequenos
  valores[valores < tol] <- tol

  # Reconstruir la matriz
  mat_pd <- vectores %*% diag(valores) %*% t(vectores)

  # Asegurar simetria perfecta
  mat_pd <- (mat_pd + t(mat_pd)) / 2

  # Mantener nombres
  rownames(mat_pd) <- rownames(mat)
  colnames(mat_pd) <- colnames(mat)

  return(mat_pd)
}


# -----------------------------------------------------------------------------
# CONFIGURACION OPENAI
# -----------------------------------------------------------------------------

#' @keywords internal
.configurar_openai <- function(api_key) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Necesitas instalar reticulate: install.packages('reticulate')")
  }

  # Verificar Python
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python no disponible. Instala Python y configura reticulate")
  }

  # Importar openai
  openai <- tryCatch({
    reticulate::import("openai")
  }, error = function(e) {
    stop("No se pudo importar openai. Instala con: pip install openai")
  })

  # Crear cliente
  cliente <- openai$OpenAI(api_key = api_key)

  return(cliente)
}


# -----------------------------------------------------------------------------
# LLAMADAS A OPENAI
# -----------------------------------------------------------------------------

#' @keywords internal
.llamar_openai <- function(openai, messages, modelo = "gpt-4.1-mini", max_tokens = 4000L,
                           temperature = NULL, seed = NULL, top_p = NULL) {

  # Temperatura: si no se especifica, lee opcion global (SeMiLLa.temperature).
  # Cuando el usuario pasa seed a semilla()/generar_escala(), esa opcion se
  # fija en 0 para maximizar la consistencia best-effort del LLM.
  if (is.null(temperature)) {
    temperature <- getOption("SeMiLLa.temperature", 0.7)
  }

  # Seed a nivel de API OpenAI (best-effort, desde nov-2023). Si no se pasa,
  # se lee la opcion global SeMiLLa.seed (fijada por semilla()/generar_escala()).
  if (is.null(seed)) {
    seed <- getOption("SeMiLLa.seed", NULL)
  }

  # top_p fijo en 1 cuando hay seed (reduce variabilidad por nucleus sampling)
  if (is.null(top_p)) {
    top_p <- getOption("SeMiLLa.top_p",
                       if (!is.null(seed)) 1 else NULL)
  }

  # ---------------------------------------------------------------------------
  # CACHE: si esta habilitado, intentar leer del disco antes de llamar a la API
  # ---------------------------------------------------------------------------
  cache_path <- NULL
  if (.cache_enabled()) {
    payload <- list(
      tipo = "chat",
      messages = messages,
      modelo = modelo,
      max_tokens = max_tokens,
      temperature = temperature,
      seed = seed,
      top_p = top_p
    )
    cache_path <- .cache_key("chat", payload)
    cached <- .cache_get(cache_path)
    if (!is.null(cached)) {
      .cache_msg_hit("chat")
      return(cached)
    }
    .cache_msg_miss("chat")
  }

  # Construir argumentos (omitiendo seed/top_p si son NULL para retrocompat)
  args <- list(
    model = modelo,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature
  )
  if (!is.null(seed)) args$seed <- as.integer(seed)
  if (!is.null(top_p)) args$top_p <- top_p

  respuesta <- tryCatch({
    do.call(openai$chat$completions$create, args)
  }, error = function(e) {
    stop("Error en llamada a OpenAI: ", e$message)
  })

  contenido <- respuesta$choices[[1]]$message$content

  # Guardar en cache
  if (!is.null(cache_path)) {
    .cache_set(cache_path, contenido)
  }

  return(contenido)
}


#' @keywords internal
.analizar_concepto <- function(openai, concepto, idioma, poblacion, n_dimensiones, modelo) {

  # Instrucciones de idioma
  instrucciones_idioma <- switch(
    idioma,
    "es" = "Responde completamente en espanol.",
    "en" = "Respond completely in English.",
    "pt" = "Responda completamente em portugues.",
    "Responde en espanol."
  )

  # Instrucciones de dimensiones
  if (!is.null(n_dimensiones) && n_dimensiones == 1) {
    instrucciones_dim <- "Este constructo es UNIDIMENSIONAL. No dividas en subdimensiones."
  } else if (!is.null(n_dimensiones)) {
    instrucciones_dim <- paste0("Identifica exactamente ", n_dimensiones, " dimensiones principales.")
  } else {
    instrucciones_dim <- "Identifica entre 3 y 6 dimensiones principales basadas en la literatura."
  }

  # Poblacion
  poblacion_texto <- if (!is.null(poblacion)) {
    paste0("Poblacion objetivo: ", poblacion, ". Adapta las dimensiones a esta poblacion.")
  } else {
    "Poblacion general."
  }

  prompt <- paste0(
    "Eres un experto en psicometria y desarrollo de escalas psicologicas.\n\n",
    "Analiza el constructo: '", concepto, "'\n\n",
    instrucciones_idioma, "\n",
    poblacion_texto, "\n",
    instrucciones_dim, "\n\n",
    "Proporciona un analisis estructurado en formato JSON con esta estructura exacta:\n",
    "{\n",
    "  \"definicion\": \"definicion operacional del constructo (2-3 oraciones)\",\n",
    "  \"fundamentacion_teorica\": {\n",
    "    \"teorias_base\": [\"teoria 1\", \"teoria 2\"],\n",
    "    \"modelos_referencia\": [\"modelo 1\", \"modelo 2\"],\n",
    "    \"justificacion\": \"explicacion breve de por que estas teorias son relevantes\"\n",
    "  },\n",
    "  \"dimensiones\": {\n",
    "    \"nombre_dimension1\": \"descripcion de la dimension\",\n",
    "    \"nombre_dimension2\": \"descripcion de la dimension\"\n",
    "  },\n",
    "  \"caracteristicas\": {\n",
    "    \"nombre_dimension1\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"],\n",
    "    \"nombre_dimension2\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"]\n",
    "  },\n",
    "  \"referencias\": [\"Autor (ano). Titulo. Revista.\", \"...\"]\n",
    "}\n\n",
    "IMPORTANTE: Responde SOLO con el JSON, sin texto adicional."
  )

  messages <- list(
    list(role = "user", content = prompt)
  )

  respuesta <- .llamar_openai(openai, messages, modelo)

  # Limpiar y parsear JSON
  respuesta <- gsub("```json|```", "", respuesta)
  respuesta <- trimws(respuesta)

  resultado <- tryCatch({
    jsonlite::fromJSON(respuesta, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Error al parsear respuesta del analisis conceptual: ", e$message)
  })

  return(resultado)
}


#' Verifica si un item nuevo es redundante con items existentes
#' @keywords internal
#' @noRd
#' @param openai Objeto de conexion OpenAI
#' @param nuevo_item Texto del nuevo item
#' @param items_existentes Vector de textos de items existentes
#' @param embeddings_existentes Matriz de embeddings existentes (opcional)
#' @param umbral Umbral de similitud para considerar redundante (default 0.85)
#' @return Lista con: redundante (logical), items_similares (textos), similitudes (valores)
.verificar_redundancia_item <- function(openai, nuevo_item, items_existentes,
                                        embeddings_existentes = NULL,
                                        umbral = 0.85) {

  # Obtener embedding del nuevo item
  nuevo_emb <- tryCatch({
    resp <- openai$embeddings$create(
      model = "text-embedding-3-small",
      input = nuevo_item
    )
    resp$data[[1]]$embedding
  }, error = function(e) {
    warning("Error obteniendo embedding: ", e$message)
    return(NULL)
  })

  if (is.null(nuevo_emb)) {
    return(list(redundante = FALSE, items_similares = character(0), similitudes = numeric(0)))
  }

  # Si no tenemos embeddings existentes, calcularlos
  if (is.null(embeddings_existentes)) {
    emb_exist <- tryCatch({
      resp <- openai$embeddings$create(
        model = "text-embedding-3-small",
        input = items_existentes
      )
      do.call(rbind, lapply(resp$data, function(x) x$embedding))
    }, error = function(e) {
      warning("Error obteniendo embeddings existentes: ", e$message)
      return(NULL)
    })
  } else {
    emb_exist <- embeddings_existentes
  }

  if (is.null(emb_exist)) {
    return(list(redundante = FALSE, items_similares = character(0), similitudes = numeric(0)))
  }

  # Calcular similitud coseno con cada item existente
  similitudes <- apply(emb_exist, 1, function(emb_row) {
    sum(nuevo_emb * emb_row) / (sqrt(sum(nuevo_emb^2)) * sqrt(sum(emb_row^2)))
  })

  # Identificar items redundantes
  idx_redundantes <- which(similitudes >= umbral)

  if (length(idx_redundantes) > 0) {
    return(list(
      redundante = TRUE,
      items_similares = items_existentes[idx_redundantes],
      similitudes = similitudes[idx_redundantes]
    ))
  } else {
    return(list(
      redundante = FALSE,
      items_similares = character(0),
      similitudes = numeric(0)
    ))
  }
}


#' @keywords internal
.generar_items_dimension <- function(openai, concepto, dimension, definicion_dim,
                                     caracteristicas, n_items, idioma, poblacion, modelo,
                                     items_evitar = NULL,
                                     complejidad_linguistica = "intermedio",
                                     tipo_escala_respuesta = "frecuencia",
                                     evitar_cuantificadores = TRUE,
                                     max_palabras = 18L,
                                     incluir_inversos = TRUE) {

  # Instrucciones de idioma
  instrucciones_idioma <- switch(
    idioma,
    "es" = "Los items deben estar en espanol.",
    "en" = "Items must be in English.",
    "pt" = "Os itens devem estar em portugues.",
    "Los items deben estar en espanol."
  )

  # Poblacion
  poblacion_texto <- if (!is.null(poblacion)) {
    paste0("Poblacion: ", poblacion, ". Adapta el lenguaje y contenido.")
  } else {
    ""
  }

  # Caracteristicas
  caract_texto <- if (!is.null(caracteristicas) && length(caracteristicas) > 0) {
    paste0("Caracteristicas a cubrir: ", paste(caracteristicas, collapse = ", "))
  } else {
    ""
  }

  # Items a evitar (para evitar redundancia)
  evitar_texto <- if (!is.null(items_evitar) && length(items_evitar) > 0) {
    paste0(
      "\n\nIMPORTANTE - EVITA generar items similares a estos (ya existen en la escala):\n",
      paste0("- \"", items_evitar, "\"", collapse = "\n"),
      "\n\nEl nuevo item debe ser SEMANTICAMENTE DIFERENTE a los anteriores. ",
      "Usa frases, estructura y enfoque distintos.\n"
    )
  } else {
    ""
  }

  # Reglas segun complejidad linguistica
  reglas_complejidad <- switch(complejidad_linguistica,
    "minimo" = paste0(
      "NIVEL LINGUISTICO: MINIMO (secundaria incompleta, lectura simple).\n",
      "- LA CLARIDAD ES LA PRIORIDAD ABSOLUTA. Una persona con secundaria\n",
      "  INCOMPLETA debe entender el item en LA PRIMERA LECTURA.\n",
      "- TEST DE COMPRENSION: si un nino o nina de 12 anos no puede\n",
      "  explicar el item con sus propias palabras tras leerlo una vez,\n",
      "  REESCRIBELO mas simple.\n",
      "- Maximo ", max_palabras, " palabras por item. Idealmente 6-9.\n",
      "- Estructura: SUJETO + VERBO + OBJETO. UNA sola idea por item.\n",
      "- Usa PRESENTE SIMPLE.\n",
      "- VERBOS PERMITIDOS (preferidos): hacer, decir, pensar, sentir,\n",
      "  querer, ver, tener, saber, dar, ir, estar, ser, mandar, mirar,\n",
      "  gritar, pelear, callar, salir, hablar, escribir, llamar, pasar.\n",
      "\n",
      "VOCABULARIO PROHIBIDO -> SUSTITUTO OBLIGATORIO:\n",
      "  controlar              -> 'mandar en' / 'decidir por mi' / 'no dejar'\n",
      "  depender               -> 'necesitar de'\n",
      "  emocionalmente         -> 'por dentro'\n",
      "  explicaciones          -> 'razones' / 'decir por que'\n",
      "  vinculos               -> 'amigos y familia'\n",
      "  autonomia              -> 'libertad'\n",
      "  desvalorizar           -> 'tratar mal' / 'hacer sentir mal'\n",
      "  microagresiones        -> 'cosas que duelen'\n",
      "  conflicto              -> 'pelea' / 'discusion'\n",
      "  justificar             -> 'buscar razones'\n",
      "  normalizar             -> 'ver como normal'\n",
      "  manifestar             -> 'mostrar'\n",
      "  comportamiento         -> 'lo que hace' / 'lo que dice'\n",
      "  percepcion             -> 'lo que pienso' / 'lo que siento'\n",
      "  cuestionar             -> 'no creer'\n",
      "  insistir               -> 'preguntar mucho'\n",
      "  restringir             -> 'no dejar'\n",
      "  acciones               -> 'lo que hago'\n",
      "  decisiones             -> 'lo que elijo'\n",
      "  detalles               -> 'cosas'\n",
      "  espontaneo             -> 'normal' / 'natural'\n",
      "  progresivo/gradual     -> 'poco a poco'\n",
      "  consecuencia           -> 'lo que pasa despues'\n",
      "  aislamiento            -> 'estar solo' / 'lejos de los demas'\n",
      "\n",
      "PROHIBIDO ABSOLUTO:\n",
      "  * jerga, tecnicismos, cultismos, latinismos, anglicismos.\n",
      "  * subordinadas encadenadas, gerundios largos.\n",
      "  * adverbios abstractos: 'de manera', 'en cierto modo'.\n",
      "  * conectores formales: 'sin embargo', 'mediante', 'a traves de'.\n",
      "  * pronombres ambiguos: 'aquello', 'cuyo'.\n",
      "  * adjetivos formales: 'consustancial', 'paulatino', 'idoneo'.\n",
      "  * cuantificadores abstractos en items: 'cierto grado de'.\n",
      "\n",
      "CONECTORES PERMITIDOS: 'y', 'pero', 'o', 'porque', 'cuando', 'si'.\n",
      "\n",
      "REGLA DE ORO: si una palabra tiene mas de 3 silabas, busca un\n",
      "sinonimo mas corto y mas comun. Las palabras MAS USADAS en\n",
      "espanol cotidiano (ser, estar, hacer, decir, ver, tener, ir,\n",
      "saber, querer, llegar, dar, deber, pasar, poner) DEBEN dominar\n",
      "el item. Solo se admiten palabras tecnicas si NO existe sinonimo.\n",
      "\n",
      "EJEMPLOS DE REESCRITURA:\n",
      "  MAL: 'Mi pareja controla mis decisiones sin dar explicaciones'\n",
      "  BIEN: 'Mi pareja decide por mi sin decir por que'\n",
      "  MAL: 'Siento que dependo emocionalmente de mi pareja'\n",
      "  BIEN: 'Necesito a mi pareja para sentirme bien'\n",
      "  MAL: 'Mis vinculos con otras personas se debilitan'\n",
      "  BIEN: 'Veo poco a mis amigos y familia'\n"
    ),
    "basico" = paste0(
      "NIVEL LINGUISTICO: BASICO (primaria completa, lectura funcional).\n",
      "- Vocabulario cotidiano, palabras comunes y cortas.\n",
      "- Oraciones de estructura SUJETO-VERBO-OBJETO simple.\n",
      "- Maximo ", max_palabras, " palabras por item.\n",
      "- PROHIBIDO: jerga tecnica, cultismos, subordinadas complejas,\n",
      "  adverbios abstractos (p. ej. 'de manera'), locuciones latinas.\n",
      "- USA palabras concretas y verbos directos.\n",
      "- El item debe entenderse en una sola lectura.\n"
    ),
    "intermedio" = paste0(
      "NIVEL LINGUISTICO: INTERMEDIO (secundaria completa).\n",
      "- Lenguaje claro, cotidiano.\n",
      "- Oraciones de hasta ", max_palabras, " palabras, una sola subordinada.\n",
      "- Evita tecnicismos; si son necesarios, usa sinonimos cotidianos.\n"
    ),
    "avanzado" = paste0(
      "NIVEL LINGUISTICO: AVANZADO (universitario).\n",
      "- Se admite vocabulario tecnico moderado.\n",
      "- Oraciones de hasta ", max_palabras, " palabras.\n",
      "- Mantener claridad y una sola idea por item.\n"
    )
  )

  # Reglas de compatibilidad con la escala de respuesta
  reglas_escala_resp <- if (evitar_cuantificadores) {
    if (tipo_escala_respuesta == "frecuencia") {
      paste0(
        "COMPATIBILIDAD CON ESCALA DE RESPUESTA: Esta escala se respondera con\n",
        "un formato de FRECUENCIA (Casi nunca ... Casi siempre).\n",
        "REGLA CRITICA: los items NO DEBEN contener cuantificadores de frecuencia\n",
        "ya incorporados en el enunciado. Son TAUTOLOGICOS con la escala y\n",
        "producen ambiguedad (ej.: 'Me preocupa CONSTANTEMENTE que algo malo...'\n",
        "obliga al respondiente a decir con que frecuencia le preocupa\n",
        "constantemente, lo cual es redundante).\n",
        "PROHIBIDO usar en los items: 'siempre', 'nunca', 'constantemente',\n",
        "'frecuentemente', 'a veces', 'a menudo', 'rara vez', 'casi nunca',\n",
        "'casi siempre', 'continuamente', 'habitualmente', 'cada vez que',\n",
        "'en ocasiones', 'ocasionalmente', 'repetidamente', 'permanentemente'.\n",
        "Redacta el comportamiento en PRESENTE SIMPLE sin especificar frecuencia\n",
        "(ej.: 'Me preocupa que algo malo le ocurra a mi hijo/a').\n"
      )
    } else if (tipo_escala_respuesta == "intensidad") {
      paste0(
        "COMPATIBILIDAD CON ESCALA DE RESPUESTA: formato de INTENSIDAD.\n",
        "PROHIBIDO usar en los items: 'mucho', 'poco', 'nada', 'bastante',\n",
        "'intensamente', 'profundamente', 'ligeramente'. Son redundantes con\n",
        "la escala de intensidad y generan ambiguedad.\n"
      )
    } else ""
  } else ""

  # Reglas de redaccion basadas en Ferrando et al. (2025)
  reglas_redaccion <- paste0(
    "REGLAS DE REDACCION (Ferrando et al., 2025):\n",
    "1. Redacta enunciados ESPECIFICOS y DIRECTOS\n",
    "2. Usa enunciados BREVES: maximo ", max_palabras, " palabras\n",
    "3. Escribe oraciones COMPLETAS, evita abreviaturas\n",
    "4. Cada enunciado debe contener UNA SOLA IDEA\n",
    "5. PROHIBIDO double-barreled: NUNCA conectar dos verbos, dos\n",
    "   adjetivos, dos emociones o dos conductas con 'y' / 'o'.\n",
    "   Si necesitas dos ideas, escoge SOLO UNA, la mas observable.\n",
    "   - MAL: 'Me siento cerca y atento/a cuando mi hijo me necesita'\n",
    "     (mide cercania E intensidad atencional al mismo tiempo)\n",
    "   - BIEN: 'Presto atencion a mi hijo cuando me necesita'\n",
    "   - MAL: 'Comprendo y respondo a las emociones de mi hijo'\n",
    "   - BIEN: 'Respondo a las emociones de mi hijo'\n",
    "   - MAL: 'Me siento triste y enojado'\n",
    "   - BIEN: 'Me siento triste'\n",
    "6. EVITA verbos vagos como 'sentirse cerca', 'estar disponible',\n",
    "   'tener presencia' que pueden interpretarse fisica o\n",
    "   emocionalmente. Usa verbos OBSERVABLES o EMOCIONES NOMBRADAS.\n",
    "   - MAL: 'Me siento cerca de mi hijo'  (cerca: ?fisicamente? ?afectivamente?)\n",
    "   - BIEN: 'Abrazo a mi hijo cuando lo veo triste' (conductual)\n",
    "   - BIEN: 'Siento carino por mi hijo'             (emocional, claro)\n",
    "7. Coloca la CONDICION o SITUACION al inicio del enunciado\n",
    "8. Usa lenguaje CLARO y COMPRENSIBLE\n",
    "9. EVITA jerga y tecnicismos\n",
    "10. Ajusta la redaccion al NIVEL LECTOR de la poblacion\n",
    "11. EVITA negaciones, especialmente dobles negaciones\n",
    "12. EVITA redaccion sesgada o sensible\n",
    "13. MINIMIZA redundancias en contenido y forma\n",
    "14. El enunciado debe estar LOGICAMENTE RELACIONADO con el constructo\n",
    "15. Los items deben indicar el MISMO CONSTRUCTO para la mayoria\n",
    "16. Los items deben DISCRIMINAR segun el nivel del constructo\n"
  )

  # Linea de polaridad (inversos si/no)
  linea_inversos <- if (isTRUE(incluir_inversos)) {
    "- Incluir algunos items inversos (redaccion negativa del constructo)\n"
  } else {
    paste0(
      "- POLARIDAD UNIFORME: TODOS los items deben estar redactados en\n",
      "  la MISMA direccion del constructo (todos positivos respecto al rasgo).\n",
      "- PROHIBIDO incluir items inversos, items con negaciones (ej. 'No siento',\n",
      "  'No me preocupa', 'No tengo'), ni items que describan la AUSENCIA del\n",
      "  comportamiento o rasgo. Cada item debe afirmar la PRESENCIA del\n",
      "  comportamiento, conducta o experiencia que define la dimension.\n",
      "- En el campo 'caracteristica' NO uses etiquetas como 'inverso', 'inv.',\n",
      "  'ausencia de', 'falta de'.\n"
    )
  }

  prompt <- paste0(
    "Genera exactamente ", n_items, " items psicometricos para medir:\n\n",
    "Constructo: ", concepto, "\n",
    "Dimension: ", dimension, "\n",
    "Definicion: ", definicion_dim, "\n",
    caract_texto, "\n\n",
    instrucciones_idioma, "\n",
    poblacion_texto,
    evitar_texto, "\n\n",
    reglas_complejidad, "\n",
    reglas_escala_resp, "\n",
    reglas_redaccion, "\n",
    "FORMATO:\n",
    "- Afirmaciones en primera persona\n",
    "- Apropiados para escala Likert (sin cuantificadores en el enunciado)\n",
    linea_inversos, "\n",
    "Responde en formato JSON:\n",
    "{\n",
    "  \"items\": [\n",
    "    {\"item\": \"texto del item\", \"caracteristica\": \"caracteristica que mide\"},\n",
    "    ...\n",
    "  ]\n",
    "}\n\n",
    "SOLO responde con el JSON."
  )

  messages <- list(
    list(role = "user", content = prompt)
  )

  respuesta <- .llamar_openai(openai, messages, modelo, max_tokens = 2000L)

  # Limpiar y parsear
  respuesta <- gsub("```json|```", "", respuesta)
  respuesta <- trimws(respuesta)

  resultado <- tryCatch({
    parsed <- jsonlite::fromJSON(respuesta, simplifyVector = FALSE)
    items_list <- parsed$items

    df <- data.frame(
      item = sapply(items_list, function(x) x$item),
      caracteristica = sapply(items_list, function(x) {
        if (!is.null(x$caracteristica)) x$caracteristica else NA
      }),
      stringsAsFactors = FALSE
    )
    return(df)
  }, error = function(e) {
    warning("Error al parsear items: ", e$message)
    return(NULL)
  })

  return(resultado)
}


# -----------------------------------------------------------------------------
# CALCULO DE SIMILITUD
# -----------------------------------------------------------------------------

#' @keywords internal
.calcular_similitud_coseno <- function(matriz) {
  # Normalizar filas
  normas <- sqrt(rowSums(matriz^2))
  normas[normas == 0] <- 1  # Evitar division por cero
  matriz_norm <- matriz / normas

  # Similitud coseno = producto punto de vectores normalizados
  similitud <- matriz_norm %*% t(matriz_norm)

  return(similitud)
}


# -----------------------------------------------------------------------------
# FUNCIONES DE DISPLAY
# -----------------------------------------------------------------------------

#' @keywords internal
.mostrar_banner <- function() {
  cat("\n")
  cat(.linea("="), "\n")
  cat("
   ____       __  __ _ _     _
  / ___|  ___|  \\/  (_) |   | |    __ _
  \\___ \\ / _ \\ |\\/| | | |   | |   / _` |
   ___) |  __/ |  | | | |___| |__| (_| |
  |____/ \\___|_|  |_|_|_____|_____\\__,_|
  ")
  cat("\n")
  cat("  SEmantic Measurement Items via LLM Assistance\n")
  cat("  Desarrollado por Dr. Jose Ventura-Leon\n")
  cat(.linea("="), "\n")
}

#' @keywords internal
.linea <- function(char = "-", n = 60) {
  paste(rep(char, n), collapse = "")
}

#' @keywords internal
.nombre_idioma <- function(codigo) {
  switch(
    codigo,
    "es" = "Espanol",
    "en" = "English",
    "pt" = "Portugues",
    codigo
  )
}


# -----------------------------------------------------------------------------
# COLORES PARA CONSOLA
# -----------------------------------------------------------------------------

#' @keywords internal
.soporta_colores <- function() {
  # Verificar si la terminal soporta colores ANSI
  isatty <- function(con) {
    tryCatch({
      isatty(con)
    }, error = function(e) FALSE)
  }

  # En RStudio siempre soporta colores
  if (Sys.getenv("RSTUDIO") == "1") return(TRUE)

  # Verificar variable de entorno
  if (Sys.getenv("TERM") != "" && Sys.getenv("TERM") != "dumb") return(TRUE)

  return(FALSE)
}

#' @keywords internal
.color_verde <- function(texto) {
  if (.soporta_colores()) {
    paste0("\033[32m", texto, "\033[0m")
  } else {
    texto
  }
}

#' @keywords internal
.color_azul <- function(texto) {
  if (.soporta_colores()) {
    paste0("\033[34m", texto, "\033[0m")
  } else {
    texto
  }
}

#' @keywords internal
.color_gris <- function(texto) {
  if (.soporta_colores()) {
    paste0("\033[90m", texto, "\033[0m")
  } else {
    texto
  }
}

#' @keywords internal
.color_amarillo <- function(texto) {
  if (.soporta_colores()) {
    paste0("\033[33m", texto, "\033[0m")
  } else {
    texto
  }
}

#' @keywords internal
.color_flecha <- function() {
  if (.soporta_colores()) {
    "\033[33m>\033[0m"
  } else {
    ">"
  }
}

#' @keywords internal
.color_check <- function() {
  if (.soporta_colores()) {
    "\033[32m\342\234\223\033[0m"
  } else {
    "[OK]"
  }
}

#' @keywords internal
.color_warning <- function() {

  if (.soporta_colores()) {
    "\033[33m\342\232\240\033[0m"
  } else {
    "[!]"
  }
}


# -----------------------------------------------------------------------------
# MODO MANUAL: GENERAR CARACTERISTICAS DESDE DIMENSIONES
# -----------------------------------------------------------------------------

#' @keywords internal
.generar_caracteristicas_manual <- function(openai, concepto, definicion,
                                             dimensiones, idioma, modelo) {

  # Instrucciones de idioma
  instrucciones_idioma <- switch(
    idioma,
    "es" = "Responde completamente en espanol.",
    "en" = "Respond completely in English.",
    "pt" = "Responda completamente em portugues.",
    "Responde en espanol."
  )

  # Construir lista de dimensiones
  dims_texto <- paste(sapply(names(dimensiones), function(d) {
    paste0("- ", d, ": ", dimensiones[[d]])
  }), collapse = "\n")

  prompt <- paste0(
    "Eres un experto en psicometria y desarrollo de escalas psicologicas.\n\n",
    instrucciones_idioma, "\n\n",
    "Para el siguiente constructo y sus dimensiones, genera 3 caracteristicas ",
    "especificas y medibles para cada dimension.\n\n",
    "CONSTRUCTO: ", concepto, "\n",
    "DEFINICION OPERACIONAL: ", definicion, "\n\n",
    "DIMENSIONES:\n", dims_texto, "\n\n",
    "Responde en formato JSON con esta estructura exacta:\n",
    "{\n",
    "  \"caracteristicas\": {\n",
    "    \"nombre_dimension1\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"],\n",
    "    \"nombre_dimension2\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"]\n",
    "  }\n",
    "}\n\n",
    "Las caracteristicas deben ser aspectos especificos y observables que permitan ",
    "generar items psicometricos claros.\n\n",
    "IMPORTANTE: Responde SOLO con el JSON, sin texto adicional."
  )

  messages <- list(
    list(role = "user", content = prompt)
  )

  respuesta <- .llamar_openai(openai, messages, modelo)

  # Limpiar y parsear JSON
  respuesta <- gsub("```json|```", "", respuesta)
  respuesta <- trimws(respuesta)

  resultado <- tryCatch({
    jsonlite::fromJSON(respuesta, simplifyVector = FALSE)
  }, error = function(e) {
    warning("Error al parsear caracteristicas: ", e$message)
    # Retornar caracteristicas genericas si falla
    list(caracteristicas = lapply(dimensiones, function(d) {
      c("Caracteristica 1", "Caracteristica 2", "Caracteristica 3")
    }))
  })

  return(resultado$caracteristicas)
}


# -----------------------------------------------------------------------------
# ANALISIS FACTORIAL CONFIRMATORIO (CFA) CON LAVAAN
# -----------------------------------------------------------------------------

#' Transformar matriz de similitud semantica para CFA
#'
#' Aplica transformaciones para que la matriz de similitud semantica
#' se comporte mas como una matriz de correlacion empirica.
#'
#' @keywords internal
.transformar_similitud_para_cfa <- function(similitud, metodo = "semantico", verbose = TRUE) {

  cor_matrix <- as.matrix(similitud)

  if (metodo == "semantico") {
    # Transformacion suave para embeddings semanticos
    # Solo aplicar ligera contraccion para evitar singularidad
    # sin destruir la estructura factorial
    lambda <- 0.05  # Contraccion muy suave
    identidad <- diag(nrow(cor_matrix))
    cor_trans <- (1 - lambda) * cor_matrix + lambda * identidad
    diag(cor_trans) <- 1

  } else if (metodo == "fisher") {
    # Transformacion Fisher con reduccion mas agresiva
    diag(cor_matrix) <- 0.999
    cor_trans <- tanh(atanh(cor_matrix) * 0.5)  # Factor 0.5 mas agresivo
    diag(cor_trans) <- 1

  } else if (metodo == "power") {
    # Elevar a potencia para aumentar diferencias
    signos <- sign(cor_matrix)
    cor_trans <- signos * (abs(cor_matrix) ^ 2)
    diag(cor_trans) <- 1

  } else if (metodo == "shrinkage") {
    # Contraccion hacia la identidad (Ledoit-Wolf simplificado)
    lambda <- 0.3  # Factor de contraccion
    identidad <- diag(nrow(cor_matrix))
    cor_trans <- (1 - lambda) * cor_matrix + lambda * identidad

  } else {
    cor_trans <- cor_matrix
  }

  # Asegurar simetria perfecta
  cor_trans <- (cor_trans + t(cor_trans)) / 2

  # Asegurar que sea definida positiva
  cor_trans <- .hacer_definida_positiva(cor_trans)

  # Mantener nombres
  rownames(cor_trans) <- rownames(similitud)
  colnames(cor_trans) <- colnames(similitud)

  return(cor_trans)
}


#' @keywords internal
.ejecutar_cfa_semantico <- function(similitud, items, items_por_dimension,
                                     estimador = "ML", ortogonal = FALSE,
                                     corr_residuales = FALSE, transformar = TRUE,
                                     verbose = TRUE) {

  # Verificar que lavaan este disponible (es dependencia dura del paquete)
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("El paquete 'lavaan' es requerido. Instala con: install.packages('lavaan')")
  }

  # Preparar matriz de correlacion (similitud semantica)
  cor_matrix <- as.matrix(similitud)
  rownames(cor_matrix) <- items$codigo
  colnames(cor_matrix) <- items$codigo

  # Aplicar transformacion para mejorar ajuste en analisis semantico
  if (transformar) {
    if (verbose) {
      cat("    > Aplicando correccion semantica a la matriz...\n")
      cat("    > Removiendo efecto halo de embeddings...\n")
    }
    cor_matrix <- .transformar_similitud_para_cfa(cor_matrix, metodo = "semantico", verbose = verbose)
  }

  # Limpiar nombres de factores para lavaan
  nombres_factores <- names(items_por_dimension)
  nombres_limpios <- gsub("[^a-zA-Z0-9]", "_", nombres_factores)
  nombres_limpios <- gsub("_+", "_", nombres_limpios)
  nombres_limpios <- gsub("^_|_$", "", nombres_limpios)
  nombres_limpios <- paste0("F", seq_along(nombres_limpios), "_", substr(nombres_limpios, 1, 10))

  # Construir sintaxis del modelo CFA
  lineas_modelo <- c()

  for (i in seq_along(items_por_dimension)) {
    factor_nombre <- nombres_limpios[i]
    items_factor <- items_por_dimension[[i]]
    linea <- paste0(factor_nombre, " =~ ", paste(items_factor, collapse = " + "))
    lineas_modelo <- c(lineas_modelo, linea)
  }

  # Agregar correlaciones residuales dentro de factores (mejora ajuste)
  if (corr_residuales) {
    if (verbose) {
      cat("    > Agregando correlaciones residuales intra-factor...\n")
    }
    for (i in seq_along(items_por_dimension)) {
      items_factor <- items_por_dimension[[i]]
      if (length(items_factor) >= 3) {
        # Correlacionar items adyacentes dentro del factor
        for (j in 1:(length(items_factor) - 1)) {
          linea_corr <- paste0(items_factor[j], " ~~ ", items_factor[j + 1])
          lineas_modelo <- c(lineas_modelo, linea_corr)
        }
      }
    }
  }

  modelo_sintaxis <- paste(lineas_modelo, collapse = "\n")

  if (verbose) {
    cat("  Modelo CFA:\n")
    for (linea in lineas_modelo[1:min(length(lineas_modelo), length(items_por_dimension) + 3)]) {
      cat("    ", linea, "\n", sep = "")
    }
    if (length(lineas_modelo) > length(items_por_dimension) + 3) {
      cat("    ... (", length(lineas_modelo) - length(items_por_dimension), " correlaciones residuales)\n", sep = "")
    }
    cat("\n")
  }

  # Tamanno de muestra - usar valor mas grande para mayor estabilidad
  n_obs <- 1000

  # Detectar si necesita datos simulados
  estimadores_robustos <- c("MLR", "MLM", "MLMV", "WLSMV", "WLSMVS", "ULSMV")
  usar_datos_simulados <- toupper(estimador) %in% estimadores_robustos

  tryCatch({
    # Suprimir warnings de lavaan durante la estimacion
    old_warn <- getOption("warn")
    options(warn = -1)
    on.exit(options(warn = old_warn), add = TRUE)

    if (usar_datos_simulados) {
      if (verbose) {
        cat("    > Simulando datos para estimador ", estimador, " (n=", n_obs, ")...\n", sep = "")
      }

      cor_matrix_pd <- .hacer_definida_positiva(cor_matrix)
      set.seed(12345)
      datos_sim <- MASS::mvrnorm(n = n_obs, mu = rep(0, ncol(cor_matrix_pd)),
                                  Sigma = cor_matrix_pd)
      datos_sim <- as.data.frame(datos_sim)
      names(datos_sim) <- colnames(cor_matrix)

      fit <- lavaan::cfa(
        model = modelo_sintaxis,
        data = datos_sim,
        estimator = estimador,
        std.lv = TRUE,
        orthogonal = ortogonal,
        check.post = FALSE,  # Evita warning de matriz no positiva
        warn = FALSE
      )
    } else {
      fit <- lavaan::cfa(
        model = modelo_sintaxis,
        sample.cov = cor_matrix,
        sample.nobs = n_obs,
        estimator = estimador,
        std.lv = TRUE,
        orthogonal = ortogonal,
        check.post = FALSE,
        warn = FALSE
      )
    }

    # Restaurar warnings
    options(warn = old_warn)

    # Extraer indices de ajuste
    ajuste_raw <- lavaan::fitmeasures(fit, c("chisq", "df", "pvalue",
                                              "cfi", "tli", "rmsea",
                                              "rmsea.ci.lower", "rmsea.ci.upper",
                                              "srmr", "aic", "bic"))

    ajuste <- list(
      chi2 = as.numeric(ajuste_raw["chisq"]),
      df = as.numeric(ajuste_raw["df"]),
      pvalue = as.numeric(ajuste_raw["pvalue"]),
      cfi = as.numeric(ajuste_raw["cfi"]),
      tli = as.numeric(ajuste_raw["tli"]),
      rmsea = as.numeric(ajuste_raw["rmsea"]),
      rmsea_ci_lower = as.numeric(ajuste_raw["rmsea.ci.lower"]),
      rmsea_ci_upper = as.numeric(ajuste_raw["rmsea.ci.upper"]),
      srmr = as.numeric(ajuste_raw["srmr"]),
      aic = as.numeric(ajuste_raw["aic"]),
      bic = as.numeric(ajuste_raw["bic"])
    )

    # Extraer cargas factoriales estandarizadas
    cargas_raw <- lavaan::standardizedSolution(fit)
    cargas_df <- cargas_raw[cargas_raw$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
    names(cargas_df) <- c("Factor", "Item", "Carga", "SE", "pvalue")

    # Mapear nombres limpios a originales
    mapa_factores <- setNames(nombres_factores, nombres_limpios)
    cargas_df$Factor_Original <- mapa_factores[cargas_df$Factor]

    # Extraer correlaciones entre factores
    cor_factores_raw <- cargas_raw[cargas_raw$op == "~~" &
                                     cargas_raw$lhs != cargas_raw$rhs &
                                     cargas_raw$lhs %in% nombres_limpios, ]
    if (nrow(cor_factores_raw) > 0) {
      cor_factores <- data.frame(
        Factor1 = mapa_factores[cor_factores_raw$lhs],
        Factor2 = mapa_factores[cor_factores_raw$rhs],
        Correlacion = cor_factores_raw$est.std,
        SE = cor_factores_raw$se,
        pvalue = cor_factores_raw$pvalue
      )
    } else {
      cor_factores <- NULL
    }

    # Calcular fiabilidad omega de McDonald por factor desde las cargas
    # estandarizadas: omega = (sum lambda)^2 / ((sum lambda)^2 + sum(1 - lambda^2)).
    # (lavaan::reliability() fue removido de lavaan y trasladado a semTools.)
    omega_por_factor <- tryCatch({
      om <- vapply(nombres_limpios, function(fac) {
        lam <- cargas_df$Carga[cargas_df$Factor == fac]
        lam <- lam[is.finite(lam)]
        if (length(lam) == 0L) return(NA_real_)
        sum_l <- sum(lam)
        (sum_l^2) / (sum_l^2 + sum(1 - lam^2))
      }, numeric(1))
      data.frame(
        Factor = nombres_factores,
        Omega = as.numeric(om)
      )
    }, error = function(e) NULL)

    resultado <- list(
      ajuste = ajuste,
      cargas = cargas_df,
      correlaciones_factores = cor_factores,
      omega = omega_por_factor,
      sintaxis = modelo_sintaxis,
      modelo_lavaan = fit,
      n_obs_simulado = n_obs,
      estimador = estimador
    )

    class(resultado) <- c("semilla_cfa", "list")
    return(resultado)

  }, error = function(e) {
    if (verbose) {
      cat("  ", .color_warning(), " Error en CFA: ", e$message, "\n", sep = "")
    }
    return(NULL)
  })
}


# -----------------------------------------------------------------------------
# MODO CIENTIFICO: BUSQUEDA EN BASES DE DATOS ACADEMICAS
# -----------------------------------------------------------------------------

#' @keywords internal
.buscar_concepto_cientifico <- function(concepto, openai, idioma, poblacion,
                                         bases_datos, n_articulos, n_dimensiones,
                                         modelo, verbose = TRUE) {

  # Construir termino de busqueda optimizado para escalas psicologicas
  # Usar terminos en ingles para mejor cobertura en bases de datos
  termino_busqueda <- paste0(
    "(", concepto, ") AND (scale OR questionnaire OR inventory OR measure OR assessment) ",
    "AND (validation OR psychometric OR factor OR dimension)"
  )

  if (verbose) {
    cat("    > Termino de busqueda: '", concepto, " [psychometric scale]'\n", sep = "")
  }

  # Recopilar informacion de articulos
  articulos_encontrados <- list()

  # Buscar en PubMed/PMC (principal fuente)
  if ("pubmed" %in% bases_datos || "pmc" %in% bases_datos) {
    if (verbose) cat("    > Buscando en PubMed/PMC...\n")

    # Usar la API de PubMed E-utilities con termino optimizado
    pubmed_url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
      "db=pubmed&retmax=", n_articulos,
      "&retmode=json&sort=relevance",
      "&term=", utils::URLencode(termino_busqueda)
    )

    pubmed_result <- tryCatch({
      response <- httr::GET(pubmed_url, httr::timeout(30))
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        jsonlite::fromJSON(content)
      } else {
        NULL
      }
    }, error = function(e) {
      if (verbose) cat("      [!] Error al buscar en PubMed: ", e$message, "\n")
      NULL
    })

    # Obtener abstracts de los articulos encontrados
    if (!is.null(pubmed_result) && !is.null(pubmed_result$esearchresult$idlist)) {
      ids <- pubmed_result$esearchresult$idlist
      if (length(ids) > 0) {
        if (verbose) cat("      [+] Encontrados ", length(ids), " articulos en PubMed\n")

        # Obtener abstracts usando formato XML para mejor parsing
        efetch_url <- paste0(
          "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
          "db=pubmed&rettype=abstract&retmode=xml&id=",
          paste(ids, collapse = ",")
        )

        abstracts_xml <- tryCatch({
          response <- httr::GET(efetch_url, httr::timeout(60))
          if (httr::status_code(response) == 200) {
            content <- httr::content(response, as = "text", encoding = "UTF-8")

            # Extraer titulos y abstracts del XML
            titulos <- regmatches(content, gregexpr("<ArticleTitle>[^<]+</ArticleTitle>", content))[[1]]
            titulos <- gsub("</?ArticleTitle>", "", titulos)

            abstracts <- regmatches(content, gregexpr("<AbstractText[^>]*>[^<]+</AbstractText>", content))[[1]]
            abstracts <- gsub("</?AbstractText[^>]*>", "", abstracts)

            # Combinar titulos y abstracts
            resultado <- ""
            for (i in seq_along(titulos)) {
              resultado <- paste0(resultado,
                                   "TITULO: ", titulos[i], "\n",
                                   "ABSTRACT: ", if(i <= length(abstracts)) abstracts[i] else "No disponible", "\n",
                                   "\n---\n\n")
            }
            resultado
          } else {
            ""
          }
        }, error = function(e) {
          if (verbose) cat("      [!] Error al obtener abstracts: ", e$message, "\n")
          ""
        })

        if (nchar(abstracts_xml) > 50) {
          articulos_encontrados$pubmed <- abstracts_xml
          if (verbose) cat("      [+] Abstracts recuperados de PubMed\n")
        }
      }
    }
  }

  # Buscar en Semantic Scholar (alternativa gratuita)
  if ("scholar" %in% bases_datos || "semantic_scholar" %in% bases_datos) {
    if (verbose) cat("    > Buscando en Semantic Scholar...\n")

    # Termino mas simple para Semantic Scholar
    ss_query <- paste0(concepto, " psychometric scale validation")

    ss_url <- paste0(
      "https://api.semanticscholar.org/graph/v1/paper/search?",
      "query=", utils::URLencode(ss_query),
      "&limit=", min(n_articulos, 10),
      "&fields=title,abstract,year"
    )

    ss_result <- tryCatch({
      Sys.sleep(1)  # Rate limiting
      response <- httr::GET(ss_url, httr::timeout(30))
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        jsonlite::fromJSON(content)
      } else {
        NULL
      }
    }, error = function(e) {
      if (verbose) cat("      [!] Error en Semantic Scholar: ", e$message, "\n")
      NULL
    })

    if (!is.null(ss_result) && !is.null(ss_result$data)) {
      papers <- ss_result$data

      # Determinar numero de articulos (puede ser data.frame o lista)
      n_papers <- if (is.data.frame(papers)) nrow(papers) else length(papers)

      if (n_papers > 0) {
        if (verbose) cat("      [+] Encontrados ", n_papers, " articulos en Semantic Scholar\n")

        # Extraer abstracts - manejar tanto data.frame como lista
        if (is.data.frame(papers)) {
          # Si es data.frame, iterar por filas
          abstracts_list <- character(0)
          for (i in 1:nrow(papers)) {
            abstract <- papers$abstract[i]
            title <- papers$title[i]
            if (!is.null(abstract) && !is.na(abstract) && nchar(abstract) > 10) {
              abstracts_list <- c(abstracts_list,
                                   paste0("TITULO: ", title, "\nABSTRACT: ", abstract))
            }
          }
          if (length(abstracts_list) > 0) {
            articulos_encontrados$scholar <- paste(abstracts_list, collapse = "\n\n---\n\n")
            if (verbose) cat("      [+] Abstracts recuperados de Semantic Scholar\n")
          }
        } else {
          # Si es lista, iterar por elementos
          abstracts_list <- character(0)
          for (p in papers) {
            if (!is.null(p$abstract) && nchar(p$abstract) > 10) {
              abstracts_list <- c(abstracts_list,
                                   paste0("TITULO: ", p$title, "\nABSTRACT: ", p$abstract))
            }
          }
          if (length(abstracts_list) > 0) {
            articulos_encontrados$scholar <- paste(abstracts_list, collapse = "\n\n---\n\n")
          }
        }
      }
    }
  }

  # Combinar toda la informacion recopilada
  texto_articulos <- paste(unlist(articulos_encontrados), collapse = "\n\n===\n\n")

  if (verbose) {
    cat("      [+] Total caracteres de literatura: ", nchar(texto_articulos), "\n")
  }

  if (nchar(texto_articulos) < 200) {
    if (verbose) {
      cat("      [!] No se encontraron suficientes articulos cientificos\n")
      cat("      [!] Utilizando conocimiento del LLM como respaldo\n")
    }
    # Fallback al modo LLM
    return(.analizar_concepto(openai, concepto, idioma, poblacion, n_dimensiones, modelo))
  }

  # Truncar si es muy largo
  if (nchar(texto_articulos) > 15000) {
    texto_articulos <- substr(texto_articulos, 1, 15000)
  }

  if (verbose) {
    cat("    > Analizando literatura cientifica con LLM...\n")
  }

  # Usar LLM para extraer definicion y dimensiones de los articulos
  instrucciones_idioma <- switch(
    idioma,
    "es" = "Responde completamente en espanol.",
    "en" = "Respond completely in English.",
    "pt" = "Responda completamente em portugues.",
    "Responde en espanol."
  )

  instrucciones_dim <- if (!is.null(n_dimensiones) && n_dimensiones == 1) {
    "Este constructo es UNIDIMENSIONAL. No dividas en subdimensiones."
  } else if (!is.null(n_dimensiones)) {
    paste0("Identifica exactamente ", n_dimensiones, " dimensiones principales.")
  } else {
    "Identifica entre 3 y 6 dimensiones principales basadas en la literatura."
  }

  prompt <- paste0(
    "Eres un experto en psicometria y revision sistematica de literatura.\n\n",
    instrucciones_idioma, "\n\n",
    "Analiza los siguientes abstracts de articulos cientificos sobre '", concepto, "':\n\n",
    "--- LITERATURA CIENTIFICA ---\n",
    texto_articulos, "\n",
    "--- FIN DE LITERATURA ---\n\n",
    "Basandote en la literatura proporcionada, extrae:\n",
    "1. Una definicion operacional del constructo basada en los estudios\n",
    "2. Las dimensiones o factores identificados en las escalas/cuestionarios\n",
    "3. Referencias bibliograficas REALES extraidas de los titulos de los articulos\n\n",
    instrucciones_dim, "\n\n",
    "INSTRUCCIONES IMPORTANTES:\n",
    "- Las CLAVES de 'dimensiones' deben ser NOMBRES DESCRIPTIVOS reales (ej: 'Autoeficacia', ",
    "'Regulacion Emocional', 'Apoyo Social'), NO 'dimension1', 'dimension2', etc.\n",
    "- Las CLAVES de 'caracteristicas' deben coincidir EXACTAMENTE con las claves de 'dimensiones'\n",
    "- Las referencias deben incluir los NOMBRES REALES de autores y anos que aparecen en los titulos\n\n",
    "Responde en formato JSON con esta estructura:\n",
    "{\n",
    "  \"definicion\": \"definicion operacional basada en la literatura\",\n",
    "  \"fundamentacion_teorica\": {\n",
    "    \"teorias_base\": [\"teoria mencionada en articulos\"],\n",
    "    \"modelos_referencia\": [\"modelos citados en la literatura\"]\n",
    "  },\n",
    "  \"dimensiones\": {\n",
    "    \"Nombre Descriptivo de Dimension 1\": \"descripcion segun la literatura\",\n",
    "    \"Nombre Descriptivo de Dimension 2\": \"descripcion segun la literatura\"\n",
    "  },\n",
    "  \"caracteristicas\": {\n",
    "    \"Nombre Descriptivo de Dimension 1\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"],\n",
    "    \"Nombre Descriptivo de Dimension 2\": [\"caracteristica1\", \"caracteristica2\", \"caracteristica3\"]\n",
    "  },\n",
    "  \"referencias\": [\"Apellido, N. (2023). Titulo real del articulo. Journal Name.\"]\n",
    "}\n\n",
    "Responde SOLO con el JSON, sin texto adicional."
  )

  messages <- list(
    list(role = "user", content = prompt)
  )

  respuesta <- .llamar_openai(openai, messages, modelo, max_tokens = 4000L)

  # Limpiar y parsear JSON
  respuesta <- gsub("```json|```", "", respuesta)
  respuesta <- trimws(respuesta)

  resultado <- tryCatch({
    jsonlite::fromJSON(respuesta, simplifyVector = FALSE)
  }, error = function(e) {
    if (verbose) cat("      [!] Error al parsear: ", e$message, "\n")
    # Fallback al modo LLM
    return(.analizar_concepto(openai, concepto, idioma, poblacion, n_dimensiones, modelo))
  })

  resultado$concepto <- concepto
  resultado$fuente <- "cientifico"
  resultado$bases_consultadas <- bases_datos

  return(resultado)
}


# =============================================================================
# AUDITORIAS LINGUISTICAS POST-GENERACION
# =============================================================================

#' @keywords internal
#' Detecta y suaviza cuantificadores de frecuencia/intensidad dentro de los
#' items generados, para evitar redundancia con la escala de respuesta.
.auditar_cuantificadores <- function(items_df, verbose = TRUE) {

  if (is.null(items_df) || nrow(items_df) == 0) return(items_df)

  # Lista de cuantificadores problematicos con sus reemplazos neutros
  reemplazos <- c(
    "constantemente"     = "",
    "continuamente"      = "",
    "permanentemente"    = "",
    "habitualmente"      = "",
    "repetidamente"      = "",
    "frecuentemente"     = "",
    "siempre"            = "",
    "nunca"              = "no",
    "casi siempre"       = "",
    "casi nunca"         = "no",
    "a menudo"           = "",
    "a veces"            = "",
    "rara vez"           = "no",
    "muy rara vez"       = "no",
    "en ocasiones"       = "",
    "ocasionalmente"     = "",
    "cada vez que"       = "cuando",
    "en pocas ocasiones" = "",
    "en muchas ocasiones"= "",
    "en la mayoria de las ocasiones" = "",
    "de vez en cuando"   = "",
    "todo el tiempo"     = "",
    "a cada momento"     = ""
  )

  normalizar <- function(s) {
    s <- tolower(s)
    chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1", "aeioun", s)
  }

  modificados <- 0
  n <- nrow(items_df)

  for (i in seq_len(n)) {
    item_original <- items_df$item[i]
    item_mod <- item_original

    for (cuant in names(reemplazos)) {
      # Busqueda case-insensitive y con/sin tildes
      patron <- paste0("\\b", normalizar(cuant), "\\b")
      item_norm <- normalizar(item_mod)
      if (grepl(patron, item_norm)) {
        # Reemplazo preservando mayusculas aproximadamente:
        # sustituimos la version normalizada
        reemplazo <- reemplazos[[cuant]]
        # Construir patron que acepte acentos
        patron_real <- gsub("a", "[aa\u00e1]", cuant, fixed = TRUE)
        patron_real <- gsub("e", "[ee\u00e9]", patron_real, fixed = TRUE)
        patron_real <- gsub("i", "[ii\u00ed]", patron_real, fixed = TRUE)
        patron_real <- gsub("o", "[oo\u00f3]", patron_real, fixed = TRUE)
        patron_real <- gsub("u", "[uu\u00fa]", patron_real, fixed = TRUE)
        patron_real <- paste0("(?i)\\b", patron_real, "\\b")

        item_mod <- gsub(patron_real, reemplazo, item_mod, perl = TRUE)
      }
    }

    # Limpieza: espacios multiples, comas sueltas, articulos descolgados
    item_mod <- gsub("\\s+", " ", item_mod)
    item_mod <- gsub(" ,", ",", item_mod)
    item_mod <- gsub(" \\.", ".", item_mod)
    item_mod <- trimws(item_mod)
    # Capitalizar primera letra si cambia
    if (nchar(item_mod) > 0) {
      substr(item_mod, 1, 1) <- toupper(substr(item_mod, 1, 1))
    }

    if (item_mod != item_original) {
      items_df$item[i] <- item_mod
      modificados <- modificados + 1
    }
  }

  if (modificados > 0 && verbose) {
    cat("\n        [!] Auditoria: ", modificados, "/", n,
        " items suavizados (se eliminaron cuantificadores redundantes)\n",
        sep = "")
  }

  items_df
}


#' @keywords internal
#' Audita longitud de items y, si se proporciona cliente openai, REESCRIBE
#' automaticamente los items que exceden max_palabras a una version mas corta
#' preservando significado y direccion del constructo. Sin openai, solo advierte.
.auditar_longitud <- function(items_df, max_palabras = 18L, verbose = TRUE,
                              openai = NULL, modelo = "gpt-4.1-mini",
                              idioma = "es", max_intentos = 3L) {

  if (is.null(items_df) || nrow(items_df) == 0) return(items_df)

  contar_palabras <- function(txt) {
    txt <- trimws(txt)
    if (nchar(txt) == 0) return(0L)
    length(strsplit(txt, "\\s+")[[1]])
  }

  n_palabras <- vapply(items_df$item, contar_palabras, integer(1))
  excedidos_idx <- which(n_palabras > max_palabras)
  excedidos <- length(excedidos_idx)

  if (excedidos == 0) return(items_df)

  # Sin cliente openai: solo advertencia (comportamiento legado)
  if (is.null(openai)) {
    if (verbose) {
      cat("        [!] Auditoria: ", excedidos, "/", nrow(items_df),
          " items exceden ", max_palabras,
          " palabras (se incluyen para revision)\n", sep = "")
    }
    return(items_df)
  }

  # Con cliente openai: reescribir automaticamente
  if (verbose) {
    cat("\n        [!] Auditoria: ", excedidos, "/", nrow(items_df),
        " items exceden ", max_palabras,
        " palabras. Reescribiendo a version mas corta...\n", sep = "")
  }

  instr_idioma <- switch(idioma,
    "es" = "Responde EXCLUSIVAMENTE en espanol.",
    "en" = "Reply ONLY in English.",
    "pt" = "Responda APENAS em portugues.",
    "Responde EXCLUSIVAMENTE en espanol."
  )

  for (i in excedidos_idx) {
    original <- items_df$item[i]
    n_orig <- n_palabras[i]
    dim_actual <- if ("dimension" %in% names(items_df)) {
      as.character(items_df$dimension[i])
    } else {
      NA_character_
    }
    caract_actual <- if ("caracteristica" %in% names(items_df)) {
      as.character(items_df$caracteristica[i])
    } else {
      NA_character_
    }

    contexto_dim <- if (!is.na(dim_actual)) {
      paste0(
        "DIMENSION DEL ITEM: ", dim_actual, "\n",
        if (!is.na(caract_actual) && nchar(caract_actual) > 0)
          paste0("CARACTERISTICA: ", caract_actual, "\n") else "",
        "El item reescrito DEBE seguir midiendo exactamente esta dimension/caracteristica.\n",
        "NO lo cambies a una dimension distinta ni a un concepto mas general.\n\n"
      )
    } else ""

    aceptado <- FALSE
    intento <- 0L
    nuevo <- original

    while (!aceptado && intento < max_intentos) {
      intento <- intento + 1L

      prompt <- paste0(
        "Reescribe el siguiente item para una persona con SECUNDARIA\n",
        "INCOMPLETA. Maximo ", max_palabras, " palabras. Manten el MISMO\n",
        "significado, la MISMA direccion y la MISMA dimension teorica.\n\n",
        contexto_dim,
        "REGLAS DE REESCRITURA (claridad maxima):\n",
        "- Maximo ", max_palabras, " palabras (cuenta cada palabra separada por espacio).\n",
        "- TEST DE COMPRENSION: un nino de 12 anos debe entenderlo en\n",
        "  la primera lectura.\n",
        "- PRESERVA la palabra clave de la dimension. Ejemplos:\n",
        "  * Aislamiento: conserva 'amigos', 'familia', 'tiempo con otros'.\n",
        "  * Control: conserva 'donde voy', 'que hago', 'mis cosas'.\n",
        "  * Desvalorizacion: conserva 'me hace sentir mal', 'me dice que'.\n",
        "  * Tension: conserva 'pelea', 'discusion', 'tension', 'mal'.\n",
        "- Estructura SUJETO-VERBO-OBJETO. Una sola idea.\n",
        "\n",
        "VOCABULARIO PROHIBIDO -> SUSTITUTO:\n",
        "  controlar     -> 'mandar en' / 'decidir por mi' / 'no dejar'\n",
        "  depender      -> 'necesitar de'\n",
        "  emocionalmente-> 'por dentro'\n",
        "  explicaciones -> 'decir por que'\n",
        "  vinculos      -> 'amigos y familia'\n",
        "  autonomia     -> 'libertad'\n",
        "  decisiones    -> 'lo que elijo' / 'lo que hago'\n",
        "  acciones      -> 'lo que hago'\n",
        "  detalles      -> 'cosas'\n",
        "  insistir      -> 'preguntar mucho'\n",
        "  cuestionar    -> 'no creer'\n",
        "  restringir    -> 'no dejar'\n",
        "  manifestar    -> 'mostrar'\n",
        "  consecuencia  -> 'lo que pasa despues'\n",
        "  aislamiento   -> 'estar solo' / 'lejos de los demas'\n",
        "  comportamiento-> 'lo que hace'\n",
        "  percepcion    -> 'lo que pienso' / 'lo que siento'\n",
        "\n",
        "PROHIBIDO: jerga, tecnicismos, conectores formales (sin embargo,\n",
        "mediante, a traves de), sustantivos abstractos, subordinadas\n",
        "encadenadas, gerundios largos.\n",
        "\n",
        "- Manten la perspectiva (primera persona) y el contexto (pareja).\n",
        "- NO inviertas el sentido. Si el original afirma una conducta\n",
        "  problematica, el nuevo tambien debe afirmarla.\n",
        "- ORACION COMPLETA, BIEN FORMADA, sin palabras sueltas al final.\n\n",
        instr_idioma, "\n\n",
        "ITEM ORIGINAL (", n_orig, " palabras):\n",
        original, "\n\n",
        "Responde UNICAMENTE con el item reescrito, sin comillas ni\n",
        "explicaciones, en una sola linea."
      )

      respuesta <- tryCatch(
        .llamar_openai(openai,
                       list(list(role = "user", content = prompt)),
                       modelo = modelo,
                       max_tokens = 100L),
        error = function(e) NA_character_
      )

      if (is.na(respuesta) || nchar(respuesta) == 0) break

      candidato <- trimws(gsub('^["\']|["\']$', '', respuesta))
      candidato <- gsub("\\s+", " ", candidato)
      # Eliminar puntuacion final repetida y palabras-basura sueltas al cierre
      candidato <- sub("\\s+(no|y|o|que|de|a|en|con)\\s*[\\.\\,\\;]?$", "",
                       candidato, perl = TRUE)
      candidato <- trimws(candidato)
      n_cand <- contar_palabras(candidato)

      # Validaciones extra: oracion bien formada
      mal_formado <- grepl("\\s(no|y|o|que|de)$", candidato, perl = TRUE) ||
                     nchar(candidato) < 8
      if (n_cand > 0 && n_cand <= max_palabras && !mal_formado) {
        nuevo <- candidato
        aceptado <- TRUE
      }
    }

    if (aceptado) {
      items_df$item[i] <- nuevo
      if (verbose) {
        cat("          [", i, "] ", n_orig, "->",
            contar_palabras(nuevo), " palabras\n", sep = "")
      }
    } else if (verbose) {
      cat("          [", i, "] no se logro acortar (mantenido original, ",
          n_orig, " palabras)\n", sep = "")
    }
  }

  items_df
}
