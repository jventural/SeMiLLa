#' @title Ensamblar el test listo para aplicar
#'
#' @description
#' Construye un cuestionario completo listo para administrar a partir de un
#' objeto \code{semilla} y (opcionalmente) un objeto de
#' \code{sugerir_escala_respuesta()}. Incluye nombre del test, autor, version,
#' seccion de datos demograficos, instrucciones, recordatorio de la escala de
#' respuesta y los items numerados con su formato de respuesta.
#'
#' Soporta tres ordenes de presentacion (original, intercalado,
#' aleatorio) y exporta a Markdown, DOCX, HTML y TXT.
#'
#' @param escala Objeto \code{semilla}, \code{semilla_items} o data.frame con
#'   columnas 'item' y 'dimension'.
#' @param escala_respuesta Objeto \code{semilla_escala_respuesta} devuelto por
#'   \code{sugerir_escala_respuesta()}. Si es NULL, se usa Likert 1-5 generico.
#' @param forma Version a generar: "larga" (default, todos los items),
#'   "corta" (version reducida) o "ambas". Si es "corta" o "ambas" y no se
#'   suministra \code{forma_corta_obj}, se calcula internamente con
#'   \code{forma_corta()}.
#' @param n_items_corta Numero de items en la forma corta (solo si forma es
#'   "corta" o "ambas"). Default: 4 por dimension.
#' @param forma_corta_obj Objeto ya calculado con \code{forma_corta()}.
#'   Si se suministra, se usa directamente sin recalcular.
#' @param por_dimension Logico. Si TRUE (default), la forma corta toma la
#'   misma cantidad de items por dimension.
#' @param nombre_test Nombre comercial del test. Si NULL, se deriva del concepto.
#' @param sufijo_larga Sufijo para archivos de la forma larga (default "_larga").
#' @param sufijo_corta Sufijo para archivos de la forma corta (default "_corta").
#' @param subtitulo Subtitulo opcional (p. ej. "Version de investigacion").
#' @param instrucciones Texto de instrucciones. Si NULL, se autogeneran segun
#'   el tipo de escala de respuesta.
#' @param incluir_datos Logico. Si TRUE (default), incluye seccion de datos
#'   demograficos.
#' @param datos_solicitados Vector de campos demograficos. Opciones:
#'   "edad", "sexo", "nivel_educativo", "estado_civil", "n_hijos",
#'   "edad_hijo", "ocupacion", "pais", "codigo".
#' @param orden Orden de presentacion de los items:
#'   "intercalado" (default, rota dimensiones), "original" (como en el objeto)
#'   o "aleatorio".
#' @param archivo Ruta de salida SIN extension. Si NULL, solo se retorna el
#'   objeto sin escribir en disco.
#' @param formato Formatos de exportacion: "md" (default), "docx", "html", "txt".
#'   Se puede pasar un vector con varios.
#' @param autor Nombre del autor.
#' @param version Version del instrumento (default "1.0").
#' @param idioma Idioma del test ("es", "en", "pt").
#' @param incluir_puntuacion Logico. Si TRUE, anade anexo con la clave de
#'   puntuacion (no imprimir al respondiente).
#' @param tabla_respuesta Logico. Si TRUE (default), imprime los anclajes
#'   en una tabla antes de los items.
#' @param items_inversos Vector de numeros o codigos de items que deben
#'   puntuarse de forma inversa.
#' @param ilustraciones Lista opcional que asocia ilustraciones a los items
#'   (para tests con apoyo grafico). NULL por defecto.
#' @param respuesta_imagen Configuracion opcional de respuesta basada en
#'   imagenes. NULL por defecto.
#' @param seed Semilla aleatoria (solo para orden = "aleatorio").
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_test} con:
#' \itemize{
#'   \item \code{nombre_test}, \code{subtitulo}, \code{autor}, \code{version}
#'   \item \code{instrucciones}, \code{datos_demograficos}, \code{anclajes}
#'   \item \code{items_presentacion}: data.frame con los items ordenados
#'   \item \code{texto_md}: cadena de markdown ensamblada
#'   \item \code{archivos}: rutas generadas (si aplica)
#' }
#'
#' @examples
#' \dontrun{
#' test <- ensamblar_test(
#'   escala           = mi_escala,
#'   escala_respuesta = esc_llm,
#'   nombre_test      = "Escala de Estilos de Apego Parental (EEAP)",
#'   autor            = "Dr. J. Ventura-Leon",
#'   archivo          = "EEAP_v1",
#'   formato          = c("md", "docx")
#' )
#'
#' # Ver el test en consola
#' print(test)
#'
#' # Acceder al texto markdown
#' cat(test$texto_md)
#' }
#'
#' @export
ensamblar_test <- function(escala,
                           escala_respuesta   = NULL,
                           forma              = c("larga", "corta", "ambas"),
                           n_items_corta      = 16L,
                           forma_corta_obj    = NULL,
                           por_dimension      = TRUE,
                           nombre_test        = NULL,
                           sufijo_larga       = "_larga",
                           sufijo_corta       = "_corta",
                           subtitulo          = NULL,
                           instrucciones      = NULL,
                           incluir_datos      = TRUE,
                           datos_solicitados  = c("edad", "sexo",
                                                  "nivel_educativo",
                                                  "n_hijos", "edad_hijo"),
                           orden              = c("intercalado", "original",
                                                  "aleatorio"),
                           archivo            = NULL,
                           formato            = c("md"),
                           autor              = NULL,
                           version            = "1.0",
                           idioma             = "es",
                           incluir_puntuacion = FALSE,
                           tabla_respuesta    = TRUE,
                           items_inversos     = NULL,
                           ilustraciones      = NULL,
                           respuesta_imagen   = NULL,
                           seed               = NULL,
                           verbose            = TRUE) {

  forma   <- match.arg(forma)
  orden   <- match.arg(orden)
  formato <- tolower(formato)

  # Validar forma vs escala recibida
  items_df_total <- .extraer_df_items(escala)
  if (is.null(items_df_total) || nrow(items_df_total) == 0) {
    stop("No se encontraron items en 'escala'.")
  }

  # --- Despachar segun forma ---
  if (forma == "larga") {
    return(.ensamblar_una_forma(
      escala = escala, escala_respuesta = escala_respuesta,
      etiqueta_forma = "larga",
      nombre_test = nombre_test, subtitulo = subtitulo,
      instrucciones = instrucciones, incluir_datos = incluir_datos,
      datos_solicitados = datos_solicitados, orden = orden,
      archivo = archivo, sufijo = "",
      formato = formato, autor = autor, version = version,
      idioma = idioma, incluir_puntuacion = incluir_puntuacion,
      tabla_respuesta = tabla_respuesta,
      items_inversos = items_inversos,
      ilustraciones = ilustraciones, respuesta_imagen = respuesta_imagen,
      seed = seed, verbose = verbose
    ))
  }

  # Necesitamos forma corta
  escala_corta <- .construir_escala_corta(
    escala          = escala,
    forma_corta_obj = forma_corta_obj,
    n_items_corta   = n_items_corta,
    por_dimension   = por_dimension,
    verbose         = verbose
  )

  if (forma == "corta") {
    # Ajustar subtitulo automaticamente si no fue dado
    sub_corta <- if (is.null(subtitulo)) {
      paste0("Forma corta - ", nrow(escala_corta$items), " \u00EDtems")
    } else subtitulo

    return(.ensamblar_una_forma(
      escala = escala_corta, escala_respuesta = escala_respuesta,
      etiqueta_forma = "corta",
      nombre_test = nombre_test, subtitulo = sub_corta,
      instrucciones = instrucciones, incluir_datos = incluir_datos,
      datos_solicitados = datos_solicitados, orden = orden,
      archivo = archivo, sufijo = "",
      formato = formato, autor = autor, version = version,
      idioma = idioma, incluir_puntuacion = incluir_puntuacion,
      tabla_respuesta = tabla_respuesta,
      items_inversos = items_inversos,
      ilustraciones = ilustraciones, respuesta_imagen = respuesta_imagen,
      seed = seed, verbose = verbose
    ))
  }

  # forma == "ambas": generar las dos versiones con sufijos
  if (verbose) cat("\n[ensamblar_test] Generando AMBAS formas...\n")

  sub_larga <- if (is.null(subtitulo)) {
    paste0("Forma larga - ", nrow(items_df_total), " \u00EDtems")
  } else subtitulo
  sub_corta <- paste0("Forma corta - ", nrow(escala_corta$items), " \u00EDtems")

  archivo_larga <- if (!is.null(archivo)) paste0(archivo, sufijo_larga) else NULL
  archivo_corta <- if (!is.null(archivo)) paste0(archivo, sufijo_corta) else NULL

  test_larga <- .ensamblar_una_forma(
    escala = escala, escala_respuesta = escala_respuesta,
    etiqueta_forma = "larga",
    nombre_test = nombre_test, subtitulo = sub_larga,
    instrucciones = instrucciones, incluir_datos = incluir_datos,
    datos_solicitados = datos_solicitados, orden = orden,
    archivo = archivo_larga, sufijo = sufijo_larga,
    formato = formato, autor = autor, version = version,
    idioma = idioma, incluir_puntuacion = incluir_puntuacion,
    tabla_respuesta = tabla_respuesta,
    items_inversos = items_inversos,
    ilustraciones = ilustraciones, respuesta_imagen = respuesta_imagen,
    seed = seed, verbose = verbose
  )

  test_corta <- .ensamblar_una_forma(
    escala = escala_corta, escala_respuesta = escala_respuesta,
    etiqueta_forma = "corta",
    nombre_test = nombre_test, subtitulo = sub_corta,
    instrucciones = instrucciones, incluir_datos = incluir_datos,
    datos_solicitados = datos_solicitados, orden = orden,
    archivo = archivo_corta, sufijo = sufijo_corta,
    formato = formato, autor = autor, version = version,
    idioma = idioma, incluir_puntuacion = incluir_puntuacion,
    tabla_respuesta = tabla_respuesta,
    items_inversos = items_inversos,
    ilustraciones = ilustraciones, respuesta_imagen = respuesta_imagen,
    seed = seed, verbose = verbose
  )

  resultado <- list(
    larga = test_larga,
    corta = test_corta
  )
  class(resultado) <- c("semilla_test_multi", "list")
  resultado
}


# =============================================================================
# Logica de una sola forma (reutilizada)
# =============================================================================

#' @keywords internal
.ensamblar_una_forma <- function(escala, escala_respuesta, etiqueta_forma,
                                 nombre_test, subtitulo, instrucciones,
                                 incluir_datos, datos_solicitados, orden,
                                 archivo, sufijo = "", formato, autor, version,
                                 idioma, incluir_puntuacion, tabla_respuesta,
                                 items_inversos,
                                 ilustraciones    = NULL,
                                 respuesta_imagen = NULL,
                                 seed, verbose) {

  items_df <- .extraer_df_items(escala)

  # Nombre autogenerado
  if (is.null(nombre_test)) {
    concepto_txt <- .extraer_concepto_str(escala)
    nombre_test <- paste0("Escala de ",
                          .capitalizar(substr(concepto_txt, 1, 60)))
  }

  # Anclajes
  if (!is.null(escala_respuesta) &&
      inherits(escala_respuesta, "semilla_escala_respuesta")) {
    anclajes <- escala_respuesta$anclajes
    tipo_esc <- escala_respuesta$tipo_escala
    n_puntos <- escala_respuesta$n_puntos
  } else {
    anclajes <- c("1" = "Totalmente en desacuerdo", "2" = "En desacuerdo",
                  "3" = "Ni de acuerdo ni en desacuerdo",
                  "4" = "De acuerdo", "5" = "Totalmente de acuerdo")
    tipo_esc <- "acuerdo"
    n_puntos <- 5L
  }

  if (is.null(instrucciones)) {
    instrucciones <- .generar_instrucciones(tipo_esc, idioma)
  }

  items_pres <- .ordenar_items(items_df, orden = orden, seed = seed)
  items_pres$numero_presentacion <- seq_len(nrow(items_pres))

  seccion_datos <- if (incluir_datos) {
    .bloque_datos_demograficos(datos_solicitados, idioma)
  } else NULL

  md <- .construir_md_test(
    nombre_test = nombre_test, subtitulo = subtitulo,
    autor = autor, version = version,
    instrucciones = instrucciones, seccion_datos = seccion_datos,
    anclajes = anclajes, tipo_esc = tipo_esc, n_puntos = n_puntos,
    tabla_respuesta = tabla_respuesta,
    items_pres = items_pres,
    incluir_puntuacion = incluir_puntuacion,
    items_inversos = items_inversos, idioma = idioma
  )

  # Resolver rutas de imagenes si vienen ilustraciones
  imagenes_items <- NULL
  if (!is.null(ilustraciones)) {
    imagenes_items <- .resolver_imagenes_items(ilustraciones,
                                                nrow(items_pres),
                                                verbose)
  }
  if (!is.null(respuesta_imagen) && !file.exists(respuesta_imagen)) {
    warning("La imagen de respuesta no existe: ", respuesta_imagen)
    respuesta_imagen <- NULL
  }

  # Datos estructurados para exportadores nativos (officer/flextable)
  docx_data <- list(
    nombre_test        = nombre_test,
    subtitulo          = subtitulo,
    autor              = autor,
    version            = version,
    instrucciones      = instrucciones,
    datos_solicitados  = if (incluir_datos) datos_solicitados else NULL,
    anclajes           = anclajes,
    tipo_esc           = tipo_esc,
    n_puntos           = n_puntos,
    tabla_respuesta    = tabla_respuesta,
    items_pres         = items_pres,
    incluir_puntuacion = incluir_puntuacion,
    items_inversos     = items_inversos,
    idioma             = idioma,
    imagenes_items     = imagenes_items,      # vector ruta o NA por item
    respuesta_imagen   = respuesta_imagen,    # ruta o NULL
    modo_ilustrado     = !is.null(ilustraciones)
  )

  archivos_generados <- character(0)
  if (!is.null(archivo)) {
    archivos_generados <- .exportar_test(md, archivo, formato, verbose,
                                         docx_data = docx_data)
  }

  if (verbose) {
    cat("\n[ensamblar_test][", etiqueta_forma, "] ", nombre_test,
        " - ", nrow(items_pres), " items.\n", sep = "")
    for (a in archivos_generados) cat("  - ", a, "\n", sep = "")
  }

  resultado <- list(
    forma              = etiqueta_forma,
    nombre_test        = nombre_test,
    subtitulo          = subtitulo,
    autor              = autor,
    version            = version,
    idioma             = idioma,
    instrucciones      = instrucciones,
    datos_demograficos = datos_solicitados,
    anclajes           = anclajes,
    tipo_escala        = tipo_esc,
    n_puntos           = n_puntos,
    orden              = orden,
    items_presentacion = items_pres,
    items_inversos     = items_inversos,
    texto_md           = md,
    archivos           = archivos_generados
  )
  class(resultado) <- c("semilla_test", "list")
  resultado
}


# =============================================================================
# Construir escala corta (usando forma_corta() o seleccion propia)
# =============================================================================

#' @keywords internal
.construir_escala_corta <- function(escala, forma_corta_obj = NULL,
                                    n_items_corta = 16L,
                                    por_dimension = TRUE,
                                    verbose = TRUE) {

  if (!is.null(forma_corta_obj)) {
    items_corta <- if (is.list(forma_corta_obj) && !is.null(forma_corta_obj$items)) {
      forma_corta_obj$items
    } else if (is.data.frame(forma_corta_obj)) {
      forma_corta_obj
    } else {
      stop("'forma_corta_obj' no tiene la estructura esperada.")
    }
  } else {
    # Intentar usar forma_corta() del paquete
    if (exists("forma_corta", mode = "function")) {
      fc <- tryCatch(
        do.call("forma_corta",
                list(x = escala, n_items = n_items_corta,
                     por_dimension = por_dimension)),
        error = function(e) {
          if (verbose) cat("  [!] forma_corta() fallo: ",
                           conditionMessage(e),
                           ". Seleccionando items por muestreo.\n", sep = "")
          NULL
        }
      )
      if (!is.null(fc) && !is.null(fc$items)) {
        items_corta <- fc$items
      } else {
        items_corta <- .seleccion_simple_corta(escala, n_items_corta,
                                               por_dimension)
      }
    } else {
      items_corta <- .seleccion_simple_corta(escala, n_items_corta,
                                             por_dimension)
    }
  }

  # Construir un objeto escala nuevo preservando metadatos
  escala_corta <- if (is.list(escala)) escala else list()
  escala_corta$items <- items_corta
  class(escala_corta) <- c("semilla", "list")
  escala_corta
}


#' @keywords internal
.seleccion_simple_corta <- function(escala, n_items, por_dimension = TRUE) {
  items_df <- .extraer_df_items(escala)
  if (por_dimension && "dimension" %in% names(items_df)) {
    dims <- unique(items_df$dimension)
    por_dim <- max(1, floor(n_items / length(dims)))
    partes <- lapply(dims, function(d) {
      sub <- items_df[items_df$dimension == d, , drop = FALSE]
      sub[seq_len(min(por_dim, nrow(sub))), , drop = FALSE]
    })
    do.call(rbind, partes)
  } else {
    items_df[seq_len(min(n_items, nrow(items_df))), , drop = FALSE]
  }
}


# =============================================================================
# HELPERS INTERNOS
# =============================================================================

#' @keywords internal
.extraer_df_items <- function(x) {
  if (inherits(x, "semilla") || inherits(x, "semilla_items")) {
    if (!is.null(x$items) && is.data.frame(x$items)) return(x$items)
  }
  if (is.data.frame(x) && "item" %in% names(x)) return(x)
  NULL
}


#' @keywords internal
.extraer_concepto_str <- function(x) {
  if (inherits(x, "semilla") || inherits(x, "semilla_items")) {
    if (!is.null(x$concepto)) {
      if (is.list(x$concepto)) {
        return(x$concepto$concepto %||%
               x$concepto$nombre %||%
               x$concepto$definicion %||% "constructo")
      } else {
        return(as.character(x$concepto))
      }
    }
  }
  "Constructo"
}


#' @keywords internal
.capitalizar <- function(s) {
  s <- trimws(as.character(s))
  if (nchar(s) == 0) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}


#' @keywords internal
.generar_instrucciones <- function(tipo_esc, idioma = "es") {

  if (idioma == "es") {
    switch(tipo_esc,
      "frecuencia" = paste(
        "A continuaci\u00F3n encontrar\u00E1 afirmaciones que describen comportamientos",
        "y emociones. Indique con qu\u00E9 FRECUENCIA cada afirmaci\u00F3n refleja su",
        "experiencia cotidiana en los \u00FAltimos seis meses. No hay respuestas",
        "correctas o incorrectas. Marque UNA SOLA opci\u00F3n por cada afirmaci\u00F3n."
      ),
      "intensidad" = paste(
        "A continuaci\u00F3n encontrar\u00E1 afirmaciones. Indique con qu\u00E9 INTENSIDAD",
        "cada afirmaci\u00F3n describe su experiencia actual. No hay respuestas",
        "correctas o incorrectas. Marque UNA SOLA opci\u00F3n por cada afirmaci\u00F3n."
      ),
      "acuerdo" = paste(
        "A continuaci\u00F3n encontrar\u00E1 afirmaciones. Indique su grado de ACUERDO",
        "con cada una. No hay respuestas correctas o incorrectas.",
        "Marque UNA SOLA opci\u00F3n por cada afirmaci\u00F3n."
      ),
      "preferencia" = paste(
        "A continuaci\u00F3n encontrar\u00E1 afirmaciones. Indique qu\u00E9 tan probable es",
        "que usted act\u00FAe de esa manera. No hay respuestas correctas o",
        "incorrectas. Marque UNA SOLA opci\u00F3n por cada afirmaci\u00F3n."
      ),
      paste(
        "A continuaci\u00F3n encontrar\u00E1 afirmaciones. Lea cada una y marque la",
        "opci\u00F3n que mejor describa su experiencia. No hay respuestas correctas",
        "o incorrectas. Marque UNA SOLA opci\u00F3n por cada afirmaci\u00F3n."
      )
    )
  } else if (idioma == "en") {
    "Please read each statement and mark the option that best describes your experience. There are no right or wrong answers."
  } else {
    "Por favor lea cada afirmacao e marque a opcao que melhor descreve sua experiencia."
  }
}


#' @keywords internal
.ordenar_items <- function(items_df, orden = "intercalado", seed = NULL) {

  if (orden == "original") return(items_df)

  if (orden == "aleatorio") {
    if (!is.null(seed)) set.seed(seed)
    return(items_df[sample(nrow(items_df)), , drop = FALSE])
  }

  # Intercalado: rotar dimensiones
  if ("dimension" %in% names(items_df)) {
    dims <- unique(items_df$dimension)
    grupos <- split(items_df, factor(items_df$dimension, levels = dims))
    max_n <- max(vapply(grupos, nrow, integer(1)))

    orden_list <- list()
    for (i in seq_len(max_n)) {
      for (d in dims) {
        g <- grupos[[d]]
        if (i <= nrow(g)) {
          orden_list[[length(orden_list) + 1]] <- g[i, , drop = FALSE]
        }
      }
    }
    return(do.call(rbind, orden_list))
  }

  items_df
}


#' @keywords internal
.etiquetas_demograficas <- function(campos, idioma = "es") {

  etiquetas_es <- c(
    codigo           = "C\u00F3digo de participante:",
    edad             = "Edad (a\u00F1os):",
    sexo             = "Sexo: [ ] Mujer  [ ] Hombre  [ ] Otro  [ ] Prefiero no decirlo",
    nivel_educativo  = "Nivel educativo: [ ] Primaria  [ ] Secundaria  [ ] T\u00E9cnico  [ ] Universitario  [ ] Postgrado",
    estado_civil     = "Estado civil: [ ] Soltero/a  [ ] Casado/a  [ ] Conviviente  [ ] Divorciado/a  [ ] Viudo/a",
    n_hijos          = "N\u00FAmero de hijos/as:",
    edad_hijo        = "Edad de su hijo/a (en quien piensa al responder):",
    ocupacion        = "Ocupaci\u00F3n:",
    pais             = "Pa\u00EDs de residencia:",
    fecha            = "Fecha de aplicaci\u00F3n:"
  )

  etiquetas_en <- c(
    codigo           = "Participant code:",
    edad             = "Age (years):",
    sexo             = "Sex: [ ] Female  [ ] Male  [ ] Other  [ ] Prefer not to say",
    nivel_educativo  = "Education: [ ] Primary  [ ] Secondary  [ ] Technical  [ ] University  [ ] Postgraduate",
    estado_civil     = "Marital status: [ ] Single  [ ] Married  [ ] Cohabiting  [ ] Divorced  [ ] Widowed",
    n_hijos          = "Number of children:",
    edad_hijo        = "Age of the child you have in mind while answering:",
    ocupacion        = "Occupation:",
    pais             = "Country of residence:",
    fecha            = "Date:"
  )

  labels <- if (idioma == "en") etiquetas_en else etiquetas_es
  items  <- labels[campos]
  items[!is.na(items)]
}


#' @keywords internal
.bloque_datos_demograficos <- function(campos, idioma = "es") {
  items  <- .etiquetas_demograficas(campos, idioma)
  lineas <- paste0("- ", items, "  \n  _______________________________")
  paste(lineas, collapse = "\n")
}


#' @keywords internal
.construir_md_test <- function(nombre_test, subtitulo, autor, version,
                               instrucciones, seccion_datos, anclajes,
                               tipo_esc, n_puntos, tabla_respuesta,
                               items_pres, incluir_puntuacion,
                               items_inversos, idioma = "es") {

  L <- list(
    anclajes_encab = if (idioma == "en") "## Response options"
                     else "## Opciones de respuesta",
    datos_encab    = if (idioma == "en") "## Participant information"
                     else "## Datos del participante",
    instr_encab    = if (idioma == "en") "## Instructions"
                     else "## Instrucciones",
    items_encab    = if (idioma == "en") "## Items"
                     else "## \u00CDtems",
    valor          = if (idioma == "en") "Value"   else "Valor",
    opcion         = if (idioma == "en") "Option"  else "Opci\u00F3n",
    version_txt    = if (idioma == "en") "Version" else "Versi\u00F3n",
    autor_txt      = if (idioma == "en") "Author"  else "Autor",
    recordatorio   = if (idioma == "en")
                       "Use the following response scale for all items:"
                     else
                       "Use la siguiente escala de respuesta para todos los \u00EDtems:"
  )

  out <- character(0)

  # Encabezado
  out <- c(out, paste0("# ", nombre_test))
  if (!is.null(subtitulo)) out <- c(out, paste0("### ", subtitulo))
  out <- c(out, "")
  meta <- character(0)
  if (!is.null(autor))    meta <- c(meta, paste0("**", L$autor_txt, ":** ", autor))
  if (!is.null(version))  meta <- c(meta, paste0("**", L$version_txt, ":** ", version))
  if (length(meta) > 0) {
    out <- c(out, paste(meta, collapse = " \u00B7 "), "")
  }
  out <- c(out, "---", "")

  # Datos del participante
  if (!is.null(seccion_datos)) {
    out <- c(out, L$datos_encab, "", seccion_datos, "", "---", "")
  }

  # Instrucciones
  out <- c(out, L$instr_encab, "", instrucciones, "", "---", "")

  # Tabla de anclajes
  if (tabla_respuesta) {
    out <- c(out, L$anclajes_encab, "", L$recordatorio, "")
    out <- c(out,
             paste0("| ", L$valor, " | ", L$opcion, " |"),
             "|:-----:|----------|")
    for (k in names(anclajes)) {
      out <- c(out, paste0("| **", k, "** | ", anclajes[[k]], " |"))
    }
    out <- c(out, "", "---", "")
  }

  # Items
  out <- c(out, L$items_encab, "")

  # Recordatorio breve encima de la tabla de items
  ancla_inline <- paste(
    paste0(names(anclajes), " = ", unname(anclajes)),
    collapse = " \u00B7 "
  )
  out <- c(out, paste0("*", ancla_inline, "*"), "")

  # Cabecera de tabla de items con columnas para cada valor
  cols_val <- paste0(" ", names(anclajes), " ")
  encab <- paste0("| N | ", if (idioma == "en") "Statement" else "Afirmacion",
                  " | ", paste(cols_val, collapse = " | "), " |")
  sep   <- paste0("|:--:|---", strrep("|:-:", length(anclajes)), "|")
  out <- c(out, encab, sep)

  for (i in seq_len(nrow(items_pres))) {
    texto <- items_pres$item[i]
    out <- c(out, paste0("| ", i, " | ", texto, " | ",
                         paste(rep("( )", length(anclajes)), collapse = " | "),
                         " |"))
  }
  out <- c(out, "", "---", "")

  # Clave de puntuacion (opcional, solo para el desarrollador)
  if (incluir_puntuacion) {
    out <- c(out,
             if (idioma == "en") "## Scoring key (NOT for respondents)"
             else "## Clave de puntuaci\u00F3n (NO imprimir al respondiente)",
             "")
    if ("dimension" %in% names(items_pres)) {
      out <- c(out, paste0("| N | ",
                           if (idioma == "en") "Dimension" else "Dimensi\u00F3n",
                           " | ",
                           if (idioma == "en") "Reverse" else "Inverso",
                           " |"))
      out <- c(out, "|:--:|----------|:-:|")
      for (i in seq_len(nrow(items_pres))) {
        inv <- if (!is.null(items_inversos) &&
                   (i %in% items_inversos ||
                    items_pres$item[i] %in% items_inversos)) "R" else ""
        out <- c(out, paste0("| ", i, " | ", items_pres$dimension[i],
                             " | ", inv, " |"))
      }
      out <- c(out, "")
    }
    out <- c(out,
             if (idioma == "en") {
               "Score each dimension as the mean of its items (reverse-coded items first)."
             } else {
               "Puntaje por dimensi\u00F3n = promedio de sus \u00EDtems (aplicar recodificaci\u00F3n inversa previamente)."
             },
             "")
  }

  paste(out, collapse = "\n")
}


#' @keywords internal
.exportar_test <- function(md, archivo, formato, verbose = TRUE,
                           docx_data = NULL) {

  archivos <- character(0)
  dir_salida <- dirname(archivo)
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

  base_md <- paste0(archivo, ".md")

  # Siempre escribir MD como fuente (sirve a fallback rmarkdown)
  writeLines(md, base_md, useBytes = TRUE)
  if ("md" %in% formato) archivos <- c(archivos, base_md)

  # TXT: version sin marcado
  if ("txt" %in% formato) {
    txt <- md
    txt <- gsub("^#+\\s+", "", txt, perl = TRUE)
    txt <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", txt)
    txt <- gsub("\\*([^*]+)\\*", "\\1", txt)
    txt <- gsub("\\|", " ", txt)
    ruta_txt <- paste0(archivo, ".txt")
    writeLines(txt, ruta_txt, useBytes = TRUE)
    archivos <- c(archivos, ruta_txt)
  }

  # DOCX: preferir officer + flextable (formato profesional);
  # rmarkdown queda como fallback si officer no esta disponible.
  if ("docx" %in% formato) {
    docx_ok <- FALSE
    if (!is.null(docx_data) &&
        requireNamespace("officer",   quietly = TRUE) &&
        requireNamespace("flextable", quietly = TRUE)) {
      ruta_docx <- tryCatch(
        .exportar_test_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("Generacion DOCX con officer fallo: ",
                  conditionMessage(e),
                  ". Usando rmarkdown como fallback.")
          NULL
        }
      )
      if (!is.null(ruta_docx) && file.exists(ruta_docx)) {
        archivos <- c(archivos, ruta_docx)
        docx_ok  <- TRUE
      }
    }
    if (!docx_ok) {
      ruta_docx <- .exportar_via_rmarkdown(base_md, archivo, "docx", verbose)
      if (!is.null(ruta_docx)) archivos <- c(archivos, ruta_docx)
    }
  }

  # HTML / PDF siguen via rmarkdown
  formatos_rmd <- intersect(formato, c("html", "pdf"))
  for (f in formatos_rmd) {
    ruta_out <- .exportar_via_rmarkdown(base_md, archivo, f, verbose)
    if (!is.null(ruta_out)) archivos <- c(archivos, ruta_out)
  }

  # Si md no estaba en formato solicitado, borrarlo
  if (!("md" %in% formato)) {
    try(file.remove(base_md), silent = TRUE)
  }

  archivos
}


#' @keywords internal
.exportar_via_rmarkdown <- function(base_md, archivo, formato, verbose = TRUE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Se requiere 'rmarkdown' para exportar a ", formato, ".")
    return(NULL)
  }
  out_format <- switch(formato,
    "docx" = "word_document",
    "html" = "html_document",
    "pdf"  = "pdf_document"
  )
  ruta_out   <- paste0(archivo, ".", formato)
  dir_salida <- dirname(archivo)
  ok <- tryCatch({
    rmarkdown::render(
      base_md,
      output_format = out_format,
      output_file   = basename(ruta_out),
      output_dir    = dir_salida,
      quiet         = !verbose
    )
    TRUE
  }, error = function(e) {
    warning("No se pudo generar ", formato, ": ", conditionMessage(e))
    FALSE
  })
  if (ok) ruta_out else NULL
}


# =============================================================================
# Exportador DOCX nativo (officer + flextable)
# =============================================================================
#'
#' Construye un DOCX con tipografia, paleta y tablas profesionales sin pasar
#' por pandoc. Devuelve la ruta del archivo generado o NULL si officer/flextable
#' no estan disponibles.
#'
#' @keywords internal
.exportar_test_docx_officer <- function(archivo, d, verbose = TRUE) {

  off <- function(name) get(name, envir = asNamespace("officer"))
  flx <- function(name) get(name, envir = asNamespace("flextable"))

  fp_par    <- off("fp_par")
  fp_text   <- off("fp_text")
  fp_border <- off("fp_border")
  ftext     <- off("ftext")
  fpar      <- off("fpar")

  # Paleta
  col_titulo  <- "#1F3864"   # azul oscuro academico
  col_subtit  <- "#595959"
  col_seccion <- "#1F3864"
  col_borde   <- "#BFBFBF"
  col_fondo_h <- "#1F3864"   # cabecera tabla
  col_text_h  <- "#FFFFFF"
  col_fondo_a <- "#EAEEF5"   # fila alterna
  col_texto   <- "#262626"

  fuente <- "Calibri"
  es_en  <- identical(d$idioma, "en")

  border_inner <- fp_border(color = col_borde,   width = 0.5, style = "solid")
  border_outer <- fp_border(color = col_seccion, width = 1.0, style = "solid")
  border_sec   <- fp_border(color = col_seccion, width = 1.2, style = "solid")

  # Documento + pagina (A4, margenes 2 cm)
  doc <- off("read_docx")()
  default_section <- off("prop_section")(
    page_size    = off("page_size")(width  = 8.27, height = 11.69,
                                    orient = "portrait"),
    page_margins = off("page_mar")(top = 0.79, bottom = 0.79,
                                    left = 0.79, right = 0.79,
                                    header = 0.5, footer = 0.5, gutter = 0),
    type = "continuous"
  )

  # Helpers locales
  add_par <- function(doc, txt, italic = FALSE, bold = FALSE,
                      size = 11, align = "justify",
                      color = col_texto, pad_top = 4, pad_bot = 4,
                      border_bottom = NULL) {
    par_args <- list(text.align    = align,
                     padding.top   = pad_top,
                     padding.bottom = pad_bot,
                     line_spacing  = 1.15)
    if (!is.null(border_bottom)) par_args$border.bottom <- border_bottom

    off("body_add_fpar")(doc, fpar(
      ftext(txt, fp_text(font.size = size, bold = bold, italic = italic,
                         color = color, font.family = fuente)),
      fp_p = do.call(fp_par, par_args)
    ))
  }

  add_heading <- function(doc, txt) {
    add_par(doc, txt, bold = TRUE, size = 13, color = col_seccion,
            align = "left", pad_top = 10, pad_bot = 4,
            border_bottom = border_sec)
  }

  estilizar_tabla <- function(ft, header = TRUE, alt = TRUE) {
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    if (header) {
      ft <- flx("bg")(ft,    bg = col_fondo_h, part = "header")
      ft <- flx("color")(ft, color = col_text_h, part = "header")
      ft <- flx("bold")(ft,  part = "header")
    }
    ft <- flx("border_outer")(ft, border = border_outer, part = "all")
    ft <- flx("border_inner_h")(ft, border = border_inner, part = "all")
    ft <- flx("border_inner_v")(ft, border = border_inner, part = "all")
    ft <- flx("padding")(ft, padding.top = 4, padding.bottom = 4,
                          padding.left = 6, padding.right = 6)
    ft <- flx("valign")(ft, valign = "center", part = "all")
    if (alt) {
      n <- flx("nrow_part")(ft, part = "body")
      if (!is.null(n) && n >= 2) {
        ft <- flx("bg")(ft, i = seq(2, n, by = 2),
                         bg = col_fondo_a, part = "body")
      }
    }
    ft
  }

  # ---------------- Encabezado ----------------
  doc <- add_par(doc, d$nombre_test, bold = TRUE, size = 18,
                 color = col_titulo, align = "center",
                 pad_top = 0, pad_bot = 4)

  if (!is.null(d$subtitulo)) {
    doc <- add_par(doc, d$subtitulo, italic = TRUE, size = 12,
                   color = col_subtit, align = "center",
                   pad_top = 0, pad_bot = 4)
  }

  meta_chunks <- list()
  if (!is.null(d$autor)) {
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      d$autor,
      fp_text(font.size = 10, color = col_subtit, font.family = fuente)
    )
  }
  if (!is.null(d$version) && nzchar(as.character(d$version))) {
    if (length(meta_chunks) > 0) {
      meta_chunks[[length(meta_chunks) + 1]] <- ftext(
        "  \u00B7  ",
        fp_text(font.size = 10, color = col_subtit, font.family = fuente)
      )
    }
    etiqueta_v <- if (es_en) "Version " else "Versi\u00F3n "
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      paste0(etiqueta_v, d$version),
      fp_text(font.size = 10, color = col_subtit, font.family = fuente)
    )
  }
  if (length(meta_chunks) > 0) {
    doc <- off("body_add_fpar")(doc, do.call(fpar, c(
      meta_chunks,
      list(fp_p = fp_par(text.align = "center",
                         padding.top = 0, padding.bottom = 14))
    )))
  }

  # ---------------- Datos del participante ----------------
  if (!is.null(d$datos_solicitados) && length(d$datos_solicitados) > 0) {
    doc <- add_heading(doc,
      if (es_en) "Participant information" else "Datos del participante")

    labels <- .etiquetas_demograficas(d$datos_solicitados, d$idioma)
    df_demo <- data.frame(
      label = unname(labels),
      linea = strrep("_", 45),
      stringsAsFactors = FALSE
    )
    ft <- flx("flextable")(df_demo)
    ft <- flx("delete_part")(ft, part = "header")
    ft <- flx("border_remove")(ft)
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    ft <- flx("fontsize")(ft, size = 11, part = "all")
    ft <- flx("color")(ft, color = col_texto, part = "all")
    ft <- flx("bold")(ft, j = 1, part = "body")
    ft <- flx("align")(ft, j = 1, align = "left",  part = "body")
    ft <- flx("align")(ft, j = 2, align = "left",  part = "body")
    ft <- flx("padding")(ft, padding.top = 5, padding.bottom = 5)
    ft <- flx("width")(ft, j = 1, width = 3.4)
    ft <- flx("width")(ft, j = 2, width = 3.3)
    doc <- flx("body_add_flextable")(doc, ft, align = "left")
    doc <- off("body_add_par")(doc, "")
  }

  # ---------------- Instrucciones ----------------
  doc <- add_heading(doc,
    if (es_en) "Instructions" else "Instrucciones")
  doc <- add_par(doc, d$instrucciones, align = "justify",
                  pad_top = 2, pad_bot = 6)

  # ---------------- Opciones de respuesta ----------------
  if (isTRUE(d$tabla_respuesta)) {
    doc <- add_heading(doc,
      if (es_en) "Response options" else "Opciones de respuesta")
    doc <- add_par(doc,
      if (es_en) "Use the following response scale for all items:"
      else "Use la siguiente escala de respuesta para todos los \u00EDtems:",
      align = "left", pad_top = 2, pad_bot = 6)

    df_anc <- data.frame(
      Valor  = names(d$anclajes),
      Opcion = unname(d$anclajes),
      stringsAsFactors = FALSE
    )
    names(df_anc) <- if (es_en) c("Value", "Option") else c("Valor", "Opci\u00F3n")

    ft <- flx("flextable")(df_anc)
    ft <- flx("fontsize")(ft, size = 11, part = "all")
    ft <- flx("align")(ft, align = "center", part = "header")
    ft <- flx("align")(ft, j = 1, align = "center", part = "body")
    ft <- flx("align")(ft, j = 2, align = "left",   part = "body")
    ft <- flx("bold")(ft, j = 1, part = "body")
    ft <- flx("color")(ft, color = col_texto, part = "body")
    ft <- flx("width")(ft, j = 1, width = 1.0)
    ft <- flx("width")(ft, j = 2, width = 5.0)
    ft <- estilizar_tabla(ft)
    doc <- flx("body_add_flextable")(doc, ft, align = "center")
    doc <- off("body_add_par")(doc, "")
  }

  # ---------------- Items ----------------
  items_pres <- d$items_pres
  n_items    <- nrow(items_pres)
  vals       <- names(d$anclajes)

  if (isTRUE(d$modo_ilustrado)) {
    # ============== Layout ilustrado estilo Appendix ==============
    # Por cada item: tabla 2x2 con
    #   (1,1) IMAGEN del item (rowspan=2)   (1,2) "N. Texto del item"
    #                                        (2,2) IMAGEN escala respuesta
    # Sin saltos de pagina forzados: Word acomoda 2-3 items por pagina.

    L_falta <- if (es_en) "[image pending: %s]" else "[imagen pendiente: %s]"

    ancho_img_item   <- 2.4   # pulgadas
    alto_img_item    <- 3.0
    ancho_img_resp   <- 4.0
    alto_img_resp    <- 0.9

    for (i in seq_len(n_items)) {
      img_path <- if (!is.null(d$imagenes_items)) d$imagenes_items[i] else NA_character_
      hay_img  <- !is.na(img_path) && file.exists(img_path)
      hay_resp <- !is.null(d$respuesta_imagen) && file.exists(d$respuesta_imagen)

      texto_item <- paste0(i, ". ", items_pres$item[i])

      df_celdas <- data.frame(
        izq    = c("", ""),
        der    = c(texto_item, ""),
        stringsAsFactors = FALSE
      )

      ft <- flx("flextable")(df_celdas)
      ft <- flx("delete_part")(ft, part = "header")
      ft <- flx("font")(ft, fontname = fuente, part = "all")
      ft <- flx("color")(ft, color = col_texto, part = "body")

      # Merge vertical de la columna izquierda (imagen del item)
      ft <- flx("merge_at")(ft, i = 1:2, j = 1)

      # Insertar imagen del item (o placeholder)
      if (hay_img) {
        dims_it <- .img_dims_inch(img_path,
                                   ancho_obj = ancho_img_item,
                                   alto_max  = alto_img_item)
        ft <- flx("compose")(ft, i = 1, j = 1,
                              value = flx("as_paragraph")(
                                flx("as_image")(src = img_path,
                                                 width = dims_it$w,
                                                 height = dims_it$h)
                              ))
      } else {
        ph_txt <- sprintf(L_falta,
                          if (!is.na(img_path)) basename(img_path)
                          else paste0("item_", sprintf("%02d", i), ".png"))
        ft <- flx("compose")(ft, i = 1, j = 1,
                              value = flx("as_paragraph")(
                                flx("as_chunk")(ph_txt,
                                  props = fp_text(font.size = 9,
                                                   italic = TRUE,
                                                   color = col_subtit,
                                                   font.family = fuente))
                              ))
      }

      # Texto del item (celda derecha arriba): bold, alineado izq
      ft <- flx("compose")(ft, i = 1, j = 2,
                            value = flx("as_paragraph")(
                              flx("as_chunk")(texto_item,
                                props = fp_text(font.size = 12,
                                                 bold = TRUE,
                                                 color = col_texto,
                                                 font.family = fuente))
                            ))

      # Imagen escala de respuesta (celda derecha abajo)
      if (hay_resp) {
        dims_re <- .img_dims_inch(d$respuesta_imagen,
                                   ancho_obj = ancho_img_resp,
                                   alto_max  = alto_img_resp)
        ft <- flx("compose")(ft, i = 2, j = 2,
                              value = flx("as_paragraph")(
                                flx("as_image")(src = d$respuesta_imagen,
                                                 width = dims_re$w,
                                                 height = dims_re$h)
                              ))
      }

      ft <- flx("align")(ft, i = 1, j = 1, align = "center", part = "body")
      ft <- flx("align")(ft, i = 1, j = 2, align = "left",   part = "body")
      ft <- flx("align")(ft, i = 2, j = 2, align = "center", part = "body")
      ft <- flx("valign")(ft, valign = "center", part = "body")
      ft <- flx("padding")(ft, padding.top = 6, padding.bottom = 6,
                            padding.left = 6, padding.right = 6, part = "body")
      ft <- flx("border_outer")(ft, border = border_outer, part = "body")
      ft <- flx("border_inner_h")(ft, border = border_inner, part = "body")
      ft <- flx("border_inner_v")(ft, border = border_inner, part = "body")
      ft <- flx("width")(ft, j = 1, width = 2.6)
      ft <- flx("width")(ft, j = 2, width = 4.1)

      doc <- flx("body_add_flextable")(doc, ft, align = "center",
                                        keepnext = FALSE)
      doc <- off("body_add_par")(doc, "")
    }

  } else {
    # ============== Layout clasico: tabla N x Afirmacion x anclajes ==============
    doc <- add_heading(doc, if (es_en) "Items" else "\u00CDtems")

    ancla_inline <- paste(
      paste0(names(d$anclajes), " = ", unname(d$anclajes)),
      collapse = "  \u00B7  "
    )
    doc <- add_par(doc, ancla_inline, italic = TRUE, size = 9.5,
                    color = col_subtit, align = "left",
                    pad_top = 2, pad_bot = 6)

    df_items <- data.frame(N = seq_len(n_items),
                            Afirmacion = items_pres$item,
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
    for (v in vals) df_items[[v]] <- "\u25CB"   # circulo vacio

    names(df_items) <- c("N",
                          if (es_en) "Statement" else "Afirmaci\u00F3n",
                          vals)

    ft <- flx("flextable")(df_items)
    ft <- flx("fontsize")(ft, size = 10, part = "body")
    ft <- flx("fontsize")(ft, size = 11, part = "header")
    ft <- flx("color")(ft, color = col_texto, part = "body")
    ft <- flx("align")(ft, align = "center", part = "header")
    ft <- flx("align")(ft, j = 1, align = "center", part = "body")
    ft <- flx("align")(ft, j = 2, align = "left",   part = "body")
    for (k in seq_along(vals)) {
      ft <- flx("align")(ft, j = 2 + k, align = "center", part = "body")
    }
    ft <- flx("width")(ft, j = 1, width = 0.40)
    ft <- flx("width")(ft, j = 2, width = 4.20)
    for (k in seq_along(vals)) {
      ft <- flx("width")(ft, j = 2 + k, width = 0.42)
    }
    ft <- flx("padding")(ft, padding.top = 6, padding.bottom = 6,
                          padding.left = 5, padding.right = 5)
    ft <- estilizar_tabla(ft)
    doc <- flx("body_add_flextable")(doc, ft, align = "center")
  }

  # ---------------- Clave de puntuacion (opcional) ----------------
  if (isTRUE(d$incluir_puntuacion) && "dimension" %in% names(items_pres)) {
    doc <- off("body_add_break")(doc)
    doc <- add_heading(doc,
      if (es_en) "Scoring key (NOT for respondents)"
      else "Clave de puntuaci\u00F3n (NO imprimir al respondiente)")

    inv_flag <- vapply(seq_len(n_items), function(i) {
      if (!is.null(d$items_inversos) &&
          (i %in% d$items_inversos ||
           items_pres$item[i] %in% d$items_inversos)) "R" else ""
    }, character(1))

    df_key <- data.frame(
      N         = seq_len(n_items),
      Dimension = items_pres$dimension,
      Inverso   = inv_flag,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df_key) <- if (es_en) c("N", "Dimension", "Reverse")
                     else c("N", "Dimensi\u00F3n", "Inverso")

    ft <- flx("flextable")(df_key)
    ft <- flx("fontsize")(ft, size = 10, part = "all")
    ft <- flx("color")(ft, color = col_texto, part = "body")
    ft <- flx("align")(ft, align = "center", part = "header")
    ft <- flx("align")(ft, j = c(1, 3), align = "center", part = "body")
    ft <- flx("align")(ft, j = 2, align = "left", part = "body")
    ft <- flx("width")(ft, j = 1, width = 0.6)
    ft <- flx("width")(ft, j = 2, width = 3.5)
    ft <- flx("width")(ft, j = 3, width = 1.0)
    ft <- estilizar_tabla(ft)
    doc <- flx("body_add_flextable")(doc, ft, align = "center")
    doc <- off("body_add_par")(doc, "")
    doc <- add_par(doc,
      if (es_en)
        "Score each dimension as the mean of its items (reverse-coded items first)."
      else
        paste("Puntaje por dimensi\u00F3n = promedio de sus \u00EDtems",
              "(aplicar recodificaci\u00F3n inversa previamente)."),
      italic = TRUE, size = 10, align = "left", pad_top = 2, pad_bot = 6)
  }

  doc <- off("body_set_default_section")(doc, default_section)

  ruta_docx <- paste0(archivo, ".docx")
  print(doc, target = ruta_docx)
  if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  ruta_docx
}


# =============================================================================
# Helpers para modo ilustrado
# =============================================================================

#' Resuelve la ruta de imagen para cada item.
#'
#' Acepta:
#'  - data.frame con columnas \code{n_item} y \code{ruta} (mapeo explicito)
#'  - vector character de longitud n_items (rutas en el orden de presentacion)
#'  - cadena con la ruta de carpeta: busca item_NN.<ext> con NN cero-padded
#'
#' Devuelve un vector character de longitud n_items con NA donde no exista.
#'
#' @keywords internal
.resolver_imagenes_items <- function(ilustraciones, n_items, verbose = TRUE) {

  rutas <- rep(NA_character_, n_items)

  if (is.data.frame(ilustraciones) &&
      all(c("n_item", "ruta") %in% names(ilustraciones))) {
    idx <- as.integer(ilustraciones$n_item)
    ok  <- idx >= 1 & idx <= n_items
    rutas[idx[ok]] <- as.character(ilustraciones$ruta[ok])

  } else if (is.character(ilustraciones) && length(ilustraciones) == n_items) {
    rutas <- ilustraciones

  } else if (is.character(ilustraciones) && length(ilustraciones) == 1) {
    if (!dir.exists(ilustraciones)) {
      warning("La carpeta de ilustraciones no existe: ", ilustraciones)
      return(rutas)
    }
    exts <- c("png", "jpg", "jpeg", "PNG", "JPG", "JPEG")
    for (i in seq_len(n_items)) {
      base <- sprintf("item_%02d", i)
      cand <- file.path(ilustraciones, paste0(base, ".", exts))
      hit  <- cand[file.exists(cand)]
      if (length(hit) >= 1) rutas[i] <- hit[1]
    }

  } else {
    warning("'ilustraciones' debe ser una carpeta, un vector character o un ",
            "data.frame con columnas (n_item, ruta).")
    return(rutas)
  }

  if (verbose) {
    encontradas <- sum(!is.na(rutas))
    cat("  [ilustraciones] ", encontradas, "/", n_items,
        " imagenes encontradas.\n", sep = "")
  }
  rutas
}


#' Calcula el ancho/alto en pulgadas conservando la relacion de aspecto.
#'
#' @keywords internal
.img_dims_inch <- function(ruta, ancho_obj = 4.5, alto_max = 6.0) {

  dims_default <- list(w = ancho_obj, h = ancho_obj * 1.25)

  info <- tryCatch({
    if (requireNamespace("magick", quietly = TRUE)) {
      img <- magick::image_read(ruta)
      meta <- magick::image_info(img)
      list(w_px = meta$width[1], h_px = meta$height[1])
    } else {
      ext <- tolower(tools::file_ext(ruta))
      if (ext == "png" && requireNamespace("png", quietly = TRUE)) {
        x <- png::readPNG(ruta, native = TRUE, info = TRUE)
        d <- attr(x, "dim")
        list(w_px = d[2], h_px = d[1])
      } else if (ext %in% c("jpg", "jpeg") &&
                 requireNamespace("jpeg", quietly = TRUE)) {
        x <- jpeg::readJPEG(ruta, native = TRUE)
        d <- attr(x, "dim")
        list(w_px = d[2], h_px = d[1])
      } else {
        NULL
      }
    }
  }, error = function(e) NULL)

  if (is.null(info) || is.null(info$w_px) || is.null(info$h_px) ||
      info$w_px == 0 || info$h_px == 0) {
    return(dims_default)
  }

  ratio <- info$h_px / info$w_px
  w <- ancho_obj
  h <- ancho_obj * ratio
  if (h > alto_max) {
    h <- alto_max
    w <- alto_max / ratio
  }
  list(w = w, h = h)
}


# =============================================================================
# METODO S3 PRINT
# =============================================================================

#' @export
print.semilla_test_multi <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test ensamblado - AMBAS formas (SeMiLLa)\n")
  cat("===========================================================\n")
  if (!is.null(x$larga)) {
    cat(".. FORMA LARGA (", nrow(x$larga$items_presentacion), " items)\n",
        sep = "")
    for (a in x$larga$archivos) cat("     - ", a, "\n", sep = "")
  }
  if (!is.null(x$corta)) {
    cat(".. FORMA CORTA (", nrow(x$corta$items_presentacion), " items)\n",
        sep = "")
    for (a in x$corta$archivos) cat("     - ", a, "\n", sep = "")
  }
  cat("===========================================================\n")
  cat("Acceda con: test$larga  o  test$corta\n\n")
  invisible(x)
}


#' @export
print.semilla_test <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test ensamblado (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Nombre     : ", x$nombre_test, "\n", sep = "")
  if (!is.null(x$subtitulo)) cat("  Subtitulo  : ", x$subtitulo, "\n", sep = "")
  if (!is.null(x$autor))     cat("  Autor      : ", x$autor, "\n", sep = "")
  cat("  Version    : ", x$version, "\n", sep = "")
  cat("  Idioma     : ", x$idioma, "\n", sep = "")
  cat("  Tipo escala: ", x$tipo_escala, " (", x$n_puntos, " puntos)\n", sep = "")
  cat("  Orden      : ", x$orden, "\n", sep = "")
  cat("  N items    : ", nrow(x$items_presentacion), "\n", sep = "")
  if (length(x$archivos) > 0) {
    cat("  Archivos   :\n")
    for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  }
  cat("-----------------------------------------------------------\n")
  cat("  Anclajes   : ",
      paste0(names(x$anclajes), "=", x$anclajes, collapse = " | "),
      "\n", sep = "")
  cat("===========================================================\n")
  cat("Use cat(<objeto>$texto_md) para ver el test completo.\n\n")
  invisible(x)
}


# =============================================================================
# DISPATCHER UNIFICADO POR FORMATO DE INSTRUMENTO (SeMiLLa v2.0)
# =============================================================================

#' @title Ensamblar test segun formato del instrumento (interfaz v2.0)
#'
#' @description
#' Dispatcher unificado para ensamblar el cuestionario administrable en
#' cualquiera de los seis formatos soportados. Sustituye a las seis
#' funciones \code{ensamblar_test()}, \code{ensamblar_test_historias()},
#' etc., que siguen disponibles como alias por retrocompatibilidad.
#'
#' @param tipo Tipo de instrumento. Uno de: \code{"likert"} (default),
#'   \code{"historias"}, \code{"guttman"}, \code{"objetiva"},
#'   \code{"cognitivo"}, \code{"forced_choice"}. Se llama \code{tipo} y no
#'   \code{formato} para evitar colision con el parametro \code{formato} de
#'   las funciones subyacentes (que se refiere al formato de archivo de
#'   salida: md/docx/html/pdf).
#' @param ... Argumentos especificos del tipo de instrumento. Vea la
#'   documentacion de la funcion subyacente:
#'   \itemize{
#'     \item likert \code{->} \code{?ensamblar_test}
#'     \item historias \code{->} \code{?ensamblar_test_historias}
#'     \item guttman \code{->} \code{?ensamblar_test_guttman}
#'     \item objetiva \code{->} \code{?ensamblar_prueba_objetiva}
#'     \item cognitivo \code{->} \code{?ensamblar_test_cognitivo}
#'     \item forced_choice \code{->} \code{?ensamblar_test_forcedchoice}
#'   }
#'
#' @return Objeto del tipo correspondiente.
#'
#' @examples
#' \dontrun{
#' # Likert (default), exportar a md y docx
#' test <- ensamblar(tipo = "likert",
#'                    escala = mi_escala,
#'                    escala_respuesta = esc_resp,
#'                    formato = c("md","docx"))
#'
#' # Forced-choice
#' test <- ensamblar(tipo = "forced_choice", escala = mi_escala_fc)
#' }
#'
#' @export
ensamblar <- function(tipo = c("likert","historias","guttman",
                                "objetiva","cognitivo","forced_choice"),
                      ...) {
  tipo <- match.arg(tipo)
  fn <- switch(tipo,
    "likert"        = ensamblar_test,
    "historias"     = ensamblar_test_historias,
    "guttman"       = ensamblar_test_guttman,
    "objetiva"      = ensamblar_prueba_objetiva,
    "cognitivo"     = ensamblar_test_cognitivo,
    "forced_choice" = ensamblar_test_forcedchoice
  )
  fn(...)
}
