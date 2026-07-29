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
#' @param complejidad_linguistica Nivel de lectura de los items: "minimo"
#'        (secundaria incompleta, items cortos y vocabulario simple con
#'        sustituciones), "basico" (primaria completa), "intermedio" (default,
#'        secundaria completa) o "avanzado" (universitario). Se preserva en el
#'        metadata para que la optimizacion/refinamiento mantengan el mismo nivel.
#' @param max_palabras Numero maximo de palabras por item. Si NULL, se deriva del
#'        nivel: minimo=10, basico=12, intermedio=18, avanzado=25.
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
#' @param compuerta Ejecutar la COMPUERTA PRE-APLICACION al final del
#'        pipeline (default: TRUE). Encadena \code{auditar_redundancia()}
#'        (pares + facetas repetidas), \code{calificar_deseabilidad()} y
#'        \code{simular_estructura()}, y emite un veredicto unico
#'        (LISTA PARA CAMPO / APLICAR CON CAUTELA / NO APLICAR TODAVIA)
#'        con acciones concretas. Motivada por el caso PM policial (n=280):
#'        los tres mecanismos que arruinaron esa aplicacion eran detectables
#'        antes de ir a campo. Ver \code{\link{compuerta_pre_aplicacion}}.
#' @param optimizar Si la compuerta devuelve "NO APLICAR TODAVIA", corregir
#'        automaticamente la escala con \code{\link{optimizar_para_campo}}:
#'        poda los clusters de faceta repetida y los pares redundantes,
#'        regenera esos items con restricciones anti-parafraseo y anti-halo,
#'        y re-pasa la compuerta (hasta \code{max_iteraciones_optimizar}).
#'        Default: TRUE. Requiere \code{compuerta = TRUE}.
#' @param max_iteraciones_optimizar Iteraciones maximas de la optimizacion
#'        automatica (default: 2).
#' @param estres Ejecutar la PRUEBA DE ESTRES (\code{estres_escala()}) al final
#'        del pipeline. Default \code{FALSE} (paso pesado; se corre a pedido).
#'        Sirve tambien con \code{fuente = "usuario"} para estresar un test ya
#'        redactado (p. ej. cargado de un articulo).
#' @param optimizar_estres Si \code{estres = TRUE} y la escala resulta fragil a
#'        algun sesgo, reescribir iterativamente los items criticos y adoptar la
#'        escala mejorada. Default \code{TRUE}; \code{FALSE} = solo diagnostico.
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
                    complejidad_linguistica = "intermedio",
                    max_palabras = NULL,
                    blindaje = TRUE,
                    contexto_prohibido = NULL,
                    instrucciones_estilo = NULL,
                    modelo_jueces = "gpt-4.1-mini",
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
                    compuerta = TRUE,
                    optimizar = TRUE,
                    max_iteraciones_optimizar = 2,
                    estres = FALSE,
                    optimizar_estres = TRUE,
                    seed = NULL,
                    verbose = TRUE) {

  # Terminos vetados por el usuario: se inyectan UNA vez en la descripcion de
  # la poblacion para que fluyan a TODOS los prompts del pipeline (generacion,
  # forced-choice, optimizacion, estres); el blindaje ademas los aplica por
  # regex deterministico (el juez LLM puede ser inconsistente en terminos
  # ambiguos como "patrulla" para estudiantes en formacion policial).
  if (!is.null(contexto_prohibido) && length(contexto_prohibido) &&
      !is.null(poblacion)) {
    poblacion <- paste0(
      poblacion, ". PROHIBIDO ABSOLUTO mencionar o aludir a: ",
      paste(contexto_prohibido, collapse = ", "),
      " (no corresponden al contexto real de esta poblacion)")
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
      complejidad_linguistica = complejidad_linguistica,
      max_palabras = max_palabras,
      blindaje = blindaje,
      contexto_prohibido = contexto_prohibido,
      instrucciones_estilo = instrucciones_estilo,
      modelo_jueces = modelo_jueces,
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
      # Se preservan del objeto de generacion para que optimizar_para_campo()
      # y refinar_escala() hereden el MISMO nivel de lectura y longitud (antes
      # se perdian aqui y la optimizacion volvia al default "intermedio").
      complejidad_linguistica =
        items_result$metadata$complejidad_linguistica %||% complejidad_linguistica,
      max_palabras = items_result$metadata$max_palabras %||% max_palabras,
      tipo_escala_respuesta = items_result$metadata$tipo_escala_respuesta,
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

  # PASO 5: COMPUERTA PRE-APLICACION (redaccion -> deseabilidad -> estructura)
  # Ninguna escala deberia ir a campo sin pasarla (caso PM policial, n=280).
  if (compuerta) {
    if (verbose) {
      cat("\n")
      cat(.linea("="), "\n")
      cat(.color_azul("[5/5] COMPUERTA PRE-APLICACION"), "\n")
      cat(.linea("="), "\n")
      cat("Auditando la escala ANTES de ir a campo...\n")
      cat("  > Redaccion: pares redundantes + facetas repetidas\n")
      cat("  > Deseabilidad social: halo entre/dentro de dimensiones\n")
      cat("  > Simulacion: probabilidad de estructura factorial limpia\n")
    }
    resultado$compuerta <- tryCatch(
      compuerta_pre_aplicacion(
        resultado,
        api_key   = api_key,
        poblacion = poblacion,
        verbose   = verbose
      ),
      error = function(e) {
        warning("La compuerta pre-aplicacion fallo (", conditionMessage(e),
                "). Ejecutela manualmente con compuerta_pre_aplicacion().")
        NULL
      }
    )

    # Correccion automatica guiada por la compuerta: podar facetas/pares y
    # regenerar con restricciones anti-parafraseo y anti-halo, re-auditando.
    if (optimizar && !is.null(resultado$compuerta) &&
        identical(resultado$compuerta$veredicto, "NO APLICAR TODAVIA")) {
      if (verbose) {
        cat("\n")
        cat(.linea("="), "\n")
        cat(.color_amarillo("[5b/5] OPTIMIZACION GUIADA POR LA COMPUERTA"), "\n")
        cat(.linea("="), "\n")
        cat("La compuerta detecto riesgos corregibles; refinando items...\n")
      }
      resultado <- tryCatch(
        optimizar_para_campo(
          resultado,
          api_key = api_key,
          max_iteraciones = max_iteraciones_optimizar,
          modelo = modelo,
          poblacion = poblacion,
          verbose = verbose
        ),
        error = function(e) {
          warning("La optimizacion automatica fallo (", conditionMessage(e),
                  "). Ejecutela manualmente con optimizar_para_campo().")
          resultado
        }
      )
    }
  } else {
    if (verbose) cat("\n", .color_gris("[5/5] COMPUERTA PRE-APLICACION"),
                     " - Omitida (compuerta = FALSE). La escala NO ha sido",
                     " auditada para campo.\n", sep = "")
  }

  # PASO 6 (OPCIONAL, apagado por defecto): PRUEBA DE ESTRES + optimizacion.
  #  estres = FALSE por defecto (paso pesado; se corre a pedido, tambien en el
  #  camino fuente="usuario" para estresar un test cargado de un articulo).
  #  Cuando estres = TRUE, optimizar_estres (default TRUE) reescribe los items
  #  fragiles y adopta la escala mejorada.
  if (isTRUE(estres)) {
    if (verbose) {
      cat("\n"); cat(.linea("="), "\n")
      cat(.color_azul("[6/6] PRUEBA DE ESTRES DE ESCALA"), "\n")
      cat(.linea("="), "\n")
    }
    est <- tryCatch(
      estres_escala(resultado, optimizar_estres = optimizar_estres,
                    modelo = modelo, poblacion = poblacion,
                    api_key = api_key, seed = seed %||% 2026, verbose = verbose),
      error = function(e) {
        warning("La prueba de estres fallo (", conditionMessage(e),
                "). Ejecutela manualmente con estres_escala().")
        NULL
      })
    if (!is.null(est)) {
      resultado$estres <- est
      if (isTRUE(est$optimizacion$aplicada) && !is.null(est$escala_final)) {
        ef <- est$escala_final
        resultado$items      <- ef$items
        resultado$embeddings <- ef$embeddings
        resultado$similitud  <- ef$similitud
      }
    }
  }

  # PASO FINAL: CIERRE DE BLINDAJE. La optimizacion guiada por la compuerta y
  # el optimizador de estres reescriben items por caminos que NO pasan por los
  # jueces de la generacion; sin este cierre la escala final puede volver a
  # salir con gemelos, fugas de contexto o items largos aunque la generacion
  # saliera limpia (observado en la bateria policial v4). Se re-aplican los
  # jueces LLM + la garantia de longitud sobre el objeto FINAL y se recalculan
  # los embeddings si algo cambio.
  if (isTRUE(blindaje) && fuente != "usuario" && !is.null(resultado$items)) {
    if (verbose) {
      cat("\n"); cat(.linea("="), "\n")
      cat(.color_azul("[CIERRE] BLINDAJE FINAL (contexto + parafrasis + longitud)"), "\n")
      cat(.linea("="), "\n")
    }
    antes <- resultado$items$item
    resultado <- tryCatch(
      blindar_escala(resultado, api_key = api_key, poblacion = poblacion,
                     modelo = modelo, contexto_prohibido = contexto_prohibido,
                     instrucciones_estilo = instrucciones_estilo,
                     modelo_jueces = modelo_jueces, verbose = verbose),
      error = function(e) {
        warning("El blindaje final fallo (", conditionMessage(e),
                "). Ejecutelo manualmente con blindar_escala().")
        resultado
      })
    if (verbose && !identical(antes, resultado$items$item)) {
      cat("  (el cierre corrigio ", sum(antes != resultado$items$item),
          " item(s) que las etapas posteriores habian ensuciado)\n", sep = "")
    }
  }

  if (verbose) {
    cat("\n")
    cat(.linea(), "\n")
    cat(.color_verde("COMPLETADO"), "\n")
    cat("Items generados: ", nrow(items_result$items), "\n", sep = "")
    cat("Dimensiones: ", length(unique(items_result$items$dimension)), "\n", sep = "")
    if (!is.null(efa_result)) {
      cat("Factores EFA: ", efa_result$metadata$n_factores, "\n", sep = "")
    }
    if (!is.null(resultado$compuerta)) {
      cat("Compuerta pre-aplicacion: ", resultado$compuerta$veredicto, "\n", sep = "")
    }
    if (!is.null(resultado$optimizacion)) {
      cat("Optimizacion automatica: ", nrow(resultado$optimizacion$reemplazos),
          " item(s) reemplazado(s) en ", resultado$optimizacion$iteraciones,
          " iteracion(es)\n", sep = "")
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

  # Compuerta pre-aplicacion
  cat(.linea("-"), "\n")
  if (!is.null(x$compuerta)) {
    col_ver <- if (identical(x$compuerta$veredicto, "LISTA PARA CAMPO"))
      .color_verde else .color_amarillo
    cat(.color_verde("COMPUERTA PRE-APLICACION:"), " ",
        col_ver(x$compuerta$veredicto), "\n", sep = "")
    if (length(x$compuerta$acciones) > 0) {
      cat("  Acciones pendientes: ", length(x$compuerta$acciones),
          " (ver x$compuerta)\n", sep = "")
    }
  } else {
    cat(.color_amarillo("COMPUERTA PRE-APLICACION: pendiente"),
        " - ejecute compuerta_pre_aplicacion(x, api_key) antes de aplicar\n",
        sep = "")
  }
  cat("\n")

  # Metadata
  cat(.linea("="), "\n")
  cat("  Generado: ", format(x$metadata$fecha, "%Y-%m-%d %H:%M"), "\n", sep = "")
  cat("  Modelo: ", x$metadata$modelo, " | Idioma: ", .nombre_idioma(x$metadata$idioma), "\n", sep = "")
  cat("  Guia: Ferrando et al. (2025) Psicothema\n")
  cat(.linea("="), "\n\n")

  invisible(x)
}


