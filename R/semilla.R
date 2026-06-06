#' @title SeMiLLa: SEmantic Measurement Items via LLM Assistance
#'
#' @description
#' Flujo completo para generar una escala psicometrica desde un concepto.
#' Ejecuta todo el pipeline: analisis del concepto, generacion de items,
#' calculo de embeddings, y clustering semantico.
#'
#' @param concepto Texto con el constructo psicologico a medir
#'        (ej: "resiliencia infantil", "autoeficacia academica")
#' @param api_key Tu API key de OpenAI (requerido)
#' @param idioma Idioma de los items: "es" (espanol), "en" (ingles), "pt" (portugues)
#' @param poblacion Descripcion de la poblacion objetivo
#'        (ej: "ninos de 8 a 12 anos", "estudiantes universitarios")
#' @param n_items Numero total de items a generar (default: 25)
#' @param n_dimensiones Numero de dimensiones (NULL = automatico, 1 = unidimensional)
#' @param modelo Modelo de OpenAI: "gpt-4.1-mini" (default), "gpt-4o", "gpt-4o-mini",
#'        "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo", o cualquier modelo compatible
#' @param fuente Modo de conceptualizacion:
#'   \itemize{
#'     \item "llm" (default): Usa el conocimiento del modelo
#'     \item "manual": Usuario proporciona definicion + dimensiones (el LLM genera items)
#'     \item "cientifico": Busca en bases de datos academicas
#'     \item "usuario": El usuario provee los items ya redactados. SeMiLLa salta
#'           la Fase II (generacion) y arranca el pipeline desde embeddings,
#'           EFA regularizado, ensemble de clustering, validez de contenido,
#'           fiabilidad semantica, etc. Requiere uno de: \code{archivo},
#'           \code{items_df} o \code{dimensiones} (con items dentro).
#'   }
#' @param definicion Para fuente="manual"/"usuario": definicion operacional del constructo
#' @param dimensiones Para fuente="manual": lista con dimensiones y sus definiciones.
#'        Para fuente="usuario": lista nombrada \code{dim -> list(definicion, items)}
#'        donde \code{items} es un vector nombrado (codigo = texto).
#' @param archivo Para fuente="usuario": ruta a archivo .xlsx o .csv con los items
#'        del usuario (columnas: dimension, definicion_dimension, codigo, item, y
#'        opcionalmente constructo y definicion_constructo). Usa el mismo formato
#'        que \code{leer_escala()} y \code{crear_plantilla_escala()}.
#' @param items_df Para fuente="usuario": data.frame con los items en lugar de
#'        archivo. Debe tener al menos columnas \code{item} y \code{dimension};
#'        opcionalmente \code{codigo}, \code{definicion_dimension}, \code{constructo},
#'        \code{definicion_constructo}.
#' @param hoja Para fuente="usuario" + archivo Excel: nombre o numero de hoja (default: 1)
#' @param bases_datos Para fuente="cientifico": bases a consultar (default: c("pubmed", "scholar"))
#' @param n_articulos Para fuente="cientifico": numero de articulos a revisar (default: 10)
#' @param incluir_efa Ejecutar analisis factorial exploratorio (default: TRUE)
#' @param n_factores_efa Numero de factores para EFA (NULL = parallel analysis)
#' @param refinar Ejecutar refinamiento iterativo para optimizar items (default: FALSE).
#'        Si TRUE, reemplaza items problematicos hasta alcanzar umbral de precision.
#' @param max_iteraciones_refinar Maximo de iteraciones para refinamiento (default: 5)
#' @param umbral_precision Precision minima aceptable 0-100 (default: 100)
#' @param exportar_csv Exportar items a CSV (default: FALSE)
#' @param archivo_salida Nombre del archivo CSV de salida
#' @param seed Semilla para reproducibilidad (default: NULL). Usar un numero
#'        entero para obtener resultados reproducibles en los analisis.
#' @param verbose Mostrar progreso en consola (default: TRUE)
#'
#' @return Objeto de clase 'semilla' con:
#' \itemize{
#'   \item \code{concepto}: Informacion del constructo analizado
#'   \item \code{items}: Dataframe con los items generados
#'   \item \code{embeddings}: Matriz de embeddings (si incluir_efa = TRUE)
#'   \item \code{similitud}: Matriz de similitud coseno
#'   \item \code{efa}: Resultados del EFA (si incluir_efa = TRUE)
#'   \item \code{metadata}: Informacion del proceso
#' }
#'
#' @examples
#' \dontrun{
#' # ===== MODO 1: CONOCIMIENTO DEL LLM (default) =====
#' escala_llm <- semilla(
#'   concepto = "resiliencia infantil",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "llm",
#'   idioma = "es",
#'   poblacion = "ninos de 8 a 12 anos",
#'   n_items = 25,
#'   seed = 2024
#' )
#'
#' # ===== MODO 2: MANUAL =====
#' escala_manual <- semilla(
#'   concepto = "autoeficacia academica",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "manual",
#'   definicion = "Creencia del estudiante en su capacidad para lograr metas",
#'   dimensiones = list(
#'     "Esfuerzo" = "Persistencia ante tareas dificiles",
#'     "Capacidad" = "Confianza en habilidades propias"
#'   ),
#'   n_items = 20
#' )
#'
#' # ===== MODO 3: CIENTIFICO =====
#' escala_cientifico <- semilla(
#'   concepto = "academic burnout",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   fuente = "cientifico",
#'   bases_datos = c("pubmed", "scholar"),
#'   n_articulos = 10,
#'   idioma = "es",
#'   n_items = 25
#' )
#'
#' # ===== MODO 4: USUARIO (items ya redactados) =====
#' # 4a) Desde archivo Excel/CSV (mismo formato que crear_plantilla_escala)
#' escala_usuario <- semilla(
#'   fuente   = "usuario",
#'   archivo  = "mi_escala.xlsx",
#'   api_key  = Sys.getenv("OPENAI_API_KEY"),
#'   idioma   = "es"
#' )
#'
#' # 4b) Desde data.frame en R
#' df <- data.frame(
#'   dimension = c("Cognitiva","Cognitiva","Emocional","Emocional"),
#'   codigo    = c("C1","C2","E1","E2"),
#'   item      = c("Pienso antes de actuar.","Analizo el problema.",
#'                 "Controlo mis emociones.","Reconozco lo que siento.")
#' )
#' escala_usuario <- semilla(
#'   concepto   = "autorregulacion",
#'   definicion = "Capacidad de regular cogniciones y emociones",
#'   fuente     = "usuario",
#'   items_df   = df,
#'   api_key    = Sys.getenv("OPENAI_API_KEY")
#' )
#'
#' # Ver resultados
#' print(escala_llm)
#' ver_items(escala_llm)
#' }
#'
#' @export
semilla <- function(concepto = NULL,
                    api_key,
                    idioma = "es",
                    poblacion = NULL,
                    n_items = 25,
                    n_dimensiones = NULL,
                    modelo = "gpt-4.1-mini",
                    fuente = "llm",
                    definicion = NULL,
                    dimensiones = NULL,
                    archivo = NULL,
                    items_df = NULL,
                    hoja = 1,
                    bases_datos = c("pubmed", "scholar"),
                    n_articulos = 10,
                    incluir_efa = TRUE,
                    n_factores_efa = NULL,
                    refinar = FALSE,
                    max_iteraciones_refinar = 5,
                    umbral_precision = 100,
                    exportar_csv = FALSE,
                    archivo_salida = NULL,
                    seed = NULL,
                    verbose = TRUE) {

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

  # En modo "usuario" el concepto puede inferirse desde el archivo/items_df,
  # asi que solo lo exigimos en los demas modos.
  if (fuente != "usuario") {
    .validar_concepto(concepto)
    .validar_n_items(n_items)
  }
  .validar_api_key(api_key)
  .validar_idioma(idioma)

  # Nombre del modo de conceptualizacion

  nombre_fuente <- switch(
    fuente,
    "llm" = "Conocimiento del LLM",
    "manual" = "Manual (usuario)",
    "cientifico" = "Cientifico (bases de datos)",
    "usuario" = "Items provistos por el usuario (sin LLM)",
    fuente
  )

  if (verbose) {
    .mostrar_banner()
    cat("\n")
    cat(.color_verde("PARAMETROS DEL ESTUDIO"), "\n")
    cat(.linea("-"), "\n")
    cat("  Concepto:          ", ifelse(is.null(concepto), "(se inferira del archivo/items_df)", concepto), "\n", sep = "")
    cat("  Idioma:            ", .nombre_idioma(idioma), "\n", sep = "")
    cat("  Poblacion:         ", ifelse(is.null(poblacion), "General", poblacion), "\n", sep = "")
    if (fuente != "usuario") {
      cat("  Items objetivo:    ", n_items, "\n", sep = "")
    } else {
      cat("  Items objetivo:    (los del archivo/items_df del usuario)\n")
    }
    cat("  Conceptualizacion: ", nombre_fuente, "\n", sep = "")
    if (!is.null(seed)) {
      cat("  Semilla:           ", seed, " (resultados reproducibles)\n", sep = "")
    }
    cat(.linea("="), "\n")
  }

  # PASO 1: Conceptualizacion - Generar items (LLM) o cargar items del usuario
  if (fuente == "usuario") {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_azul("[1/5] CARGA DE ITEMS DEL USUARIO"), "\n")
      cat(.linea("="), "\n")
      cat("Saltando la generacion via LLM. Cargando items provistos...\n\n")
    }

    items_result <- .cargar_items_usuario(
      concepto    = concepto,
      definicion  = definicion,
      archivo     = archivo,
      items_df    = items_df,
      dimensiones = dimensiones,
      idioma      = idioma,
      poblacion   = poblacion,
      hoja        = hoja,
      verbose     = verbose
    )

    # Reasignar concepto y n_items inferidos para el resto del pipeline
    concepto <- items_result$concepto$concepto
    n_items  <- nrow(items_result$items)

  } else {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_azul("[1/5] CONCEPTUALIZACION"), "\n")
      cat(.linea("="), "\n")
      cat("Analizando el constructo '", concepto, "' y generando items...\n\n", sep = "")
    }

    items_result <- generar_escala(
      concepto = concepto,
      api_key = api_key,
      idioma = idioma,
      poblacion = poblacion,
      n_items = n_items,
      n_dimensiones = n_dimensiones,
      modelo = modelo,
      fuente = fuente,
      definicion = definicion,
      dimensiones = dimensiones,
      bases_datos = bases_datos,
      n_articulos = n_articulos,
      seed = seed,
      verbose = verbose
    )
  }

  # PASO 2: Representacion - Calcular embeddings
  if (verbose) {
    cat("\n")
    cat(.linea("="), "\n")
    cat(.color_azul("[2/5] REPRESENTACION"), "\n")
    cat(.linea("="), "\n")
    cat("Calculando embeddings semanticos para cada item...\n")
    cat("  > Convirtiendo items a vectores de 1536 dimensiones\n")
    cat("  > Calculando matriz de similitud coseno\n\n")
  }

  emb_result <- obtener_embeddings(
    items = items_result,
    api_key = api_key,
    verbose = verbose
  )

  # PASO 3: Estructura - EFA (opcional)
  efa_result <- NULL
  if (incluir_efa) {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_azul("[3/5] ESTRUCTURA"), "\n")
      cat(.linea("="), "\n")
      cat("Realizando clustering semantico...\n")
      cat("  > Asignando items a clusters semanticos\n")
      cat("  > Calculando precision de clasificacion\n")
      cat("  > Comparando estructura teorica vs empirica\n\n")
    }

    # Crear objeto temporal para clustering
    temp_escala <- list(
      items = items_result$items,
      embeddings = emb_result$embeddings,
      similitud = emb_result$similitud
    )
    class(temp_escala) <- c("semilla", "list")

    efa_result <- precision_clasificacion(
      x = temp_escala,
      n_clusters = length(unique(items_result$items$dimension)),
      verbose = verbose
    )
  } else {
    if (verbose) cat("\n", .color_gris("[3/5] ESTRUCTURA"), " - Omitido (incluir_efa = FALSE)\n", sep = "")
  }

  # Construir resultado intermedio para refinamiento
  resultado <- list(
    concepto = items_result$concepto,
    items = items_result$items,
    embeddings = emb_result$embeddings,
    similitud = emb_result$similitud,
    efa = efa_result,
    metadata = list(
      concepto_original = concepto,
      idioma = idioma,
      poblacion = poblacion,
      modelo = modelo,
      n_items_generados = nrow(items_result$items),
      seed = seed,
      fecha = Sys.time()
    )
  )
  class(resultado) <- c("semilla", "list")

  # PASO 3.5: Refinamiento iterativo (opcional)
  refinamiento_result <- NULL
  if (refinar && incluir_efa) {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_amarillo("[3.5/5] REFINAMIENTO"), "\n")
      cat(.linea("="), "\n")
      cat("Optimizando items iterativamente...\n")
      cat("  > Identificando items mal clasificados\n")
      cat("  > Regenerando items problematicos\n")
      cat("  > Iterando hasta precision: ", umbral_precision, "%\n\n", sep = "")
    }

    refinamiento_result <- refinar_escala(
      escala = resultado,
      api_key = api_key,
      max_iteraciones = max_iteraciones_refinar,
      umbral_precision = umbral_precision,
      modelo = modelo,
      exportar_excel = FALSE,
      verbose = verbose
    )

    # Actualizar resultado con escala refinada
    resultado <- refinamiento_result$escala_final
    resultado$refinamiento <- list(
      iteraciones = refinamiento_result$iteraciones,
      historial = refinamiento_result$historial,
      precision_inicial = refinamiento_result$precision_inicial,
      precision_final = refinamiento_result$precision_final
    )
  } else if (refinar && !incluir_efa) {
    if (verbose) cat("\n", .color_gris("[3.5/5] REFINAMIENTO"), " - Omitido (requiere incluir_efa = TRUE)\n", sep = "")
  } else {
    if (verbose) cat("\n", .color_gris("[3.5/5] REFINAMIENTO"), " - Omitido (refinar = FALSE)\n", sep = "")
  }

  # PASO 4: Integracion - Exportar (opcional)
  if (exportar_csv) {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_azul("[4/5] INTEGRACION"), "\n")
      cat(.linea("="), "\n")
      cat("Exportando resultados a archivo...\n\n")
    }

    if (is.null(archivo_salida)) {
      archivo_salida <- paste0("semilla_", gsub(" ", "_", concepto), ".csv")
    }

    exportar_escala(
      x = list(items = resultado$items, concepto = resultado$concepto, metadata = resultado$metadata, efa = resultado$efa),
      archivo = archivo_salida,
      verbose = verbose
    )
  } else {
    if (verbose) cat("\n", .color_gris("[4/5] INTEGRACION"), " - Omitido (exportar_csv = FALSE)\n", sep = "")
  }

  # Actualizar metadata final
  resultado$metadata$fecha    <- Sys.time()
  resultado$metadata$refinado <- refinar
  resultado$metadata$fuente   <- fuente

  if (verbose) {
    cat("\n")
    cat(.linea(), "\n")
    cat(.color_verde("COMPLETADO"), "\n")
    cat("Items generados: ", nrow(items_result$items), "\n", sep = "")
    cat("Dimensiones: ", length(unique(items_result$items$dimension)), "\n", sep = "")
    if (!is.null(efa_result)) {
      cat("Factores EFA: ", efa_result$metadata$n_factores, "\n", sep = "")
    }
    cat(.linea(), "\n")
  }

  return(resultado)
}


#' @title Imprimir objeto SeMiLLa
#' @param x Objeto de clase semilla
#' @param ... Argumentos adicionales
#' @export
print.semilla <- function(x, ...) {

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
  cat(.linea("="), "\n\n")

  # Concepto
  cat(.color_verde("CONCEPTO:"), x$metadata$concepto_original, "\n")
  cat(.color_verde("DEFINICION:"), "\n")
  cat(strwrap(x$concepto$definicion, width = 70, indent = 2, exdent = 2), sep = "\n")
  cat("\n")

  # Fundamentacion teorica
  if (!is.null(x$concepto$fundamentacion_teorica)) {
    cat(.linea("-"), "\n")
    cat(.color_verde("FUNDAMENTACION TEORICA:"), "\n\n")

    if (!is.null(x$concepto$fundamentacion_teorica$teorias_base)) {
      cat("  Teorias base:\n")
      for (t in x$concepto$fundamentacion_teorica$teorias_base) {
        cat("    * ", t, "\n", sep = "")
      }
    }

    if (!is.null(x$concepto$fundamentacion_teorica$modelos_referencia)) {
      cat("\n  Modelos de referencia:\n")
      for (m in x$concepto$fundamentacion_teorica$modelos_referencia) {
        cat("    * ", m, "\n", sep = "")
      }
    }
    cat("\n")
  }

  # Dimensiones
  cat(.linea("-"), "\n")
  cat(.color_verde("DIMENSIONES:"), "(", length(x$concepto$dimensiones), ")\n\n")

  for (d in names(x$concepto$dimensiones)) {
    n_items <- sum(x$items$dimension == d)
    cat("  [", n_items, " items] ", .color_azul(toupper(d)), "\n", sep = "")
    cat("  ", x$concepto$dimensiones[[d]], "\n\n", sep = "")
  }

  # Items
  cat(.linea("-"), "\n")
  cat(.color_verde("ITEMS GENERADOS:"), "(", nrow(x$items), " total)\n\n")

  for (i in 1:min(5, nrow(x$items))) {
    cat(sprintf("  %2d. [%s] %s\n",
                x$items$numero[i],
                x$items$dimension[i],
                x$items$item[i]))
  }
  if (nrow(x$items) > 5) {
    cat("  ... y ", nrow(x$items) - 5, " items mas\n", sep = "")
  }
  cat("\n")

  # EFA
  if (!is.null(x$efa)) {
    cat(.linea("-"), "\n")
    cat(.color_verde("CLUSTERING SEMANTICO:"), "\n\n")
    cat("  Clusters identificados: ", x$efa$metadata$n_factores, "\n", sep = "")
    cat("  Rotacion: ", x$efa$metadata$rotacion, "\n", sep = "")
    cat("  Varianza explicada: ", round(sum(x$efa$varianza$Prop_Var) * 100, 1), "%\n", sep = "")
    cat("\n")
  }

  # Referencias
  if (!is.null(x$concepto$referencias) && length(x$concepto$referencias) > 0) {
    cat(.linea("-"), "\n")
    cat(.color_verde("REFERENCIAS:"), "\n\n")
    refs <- unlist(x$concepto$referencias)
    for (i in 1:min(3, length(refs))) {
      cat("  [", i, "] ", refs[i], "\n", sep = "")
    }
    if (length(refs) > 3) {
      cat("  ... y ", length(refs) - 3, " referencias mas\n", sep = "")
    }
    cat("\n")
  }

  # Metadata
  cat(.linea("="), "\n")
  cat("  Generado: ", format(x$metadata$fecha, "%Y-%m-%d %H:%M"), "\n", sep = "")
  cat("  Modelo: ", x$metadata$modelo, " | Idioma: ", .nombre_idioma(x$metadata$idioma), "\n", sep = "")
  cat("  Guia: Ferrando et al. (2025) Psicothema\n")
  cat(.linea("="), "\n\n")

  invisible(x)
}


