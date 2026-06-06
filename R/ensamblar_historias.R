#' @title Ensamblar test basado en historias
#'
#' @description
#' Toma un objeto \code{semilla_historias} y produce el cuestionario listo
#' para administrar. Para cada historia genera un bloque:
#'
#' \enumerate{
#'   \item Introduccion comun
#'   \item Texto de la historia (introduccion + desenlace especifico del factor)
#'   \item Tabla de items de percepcion con escala Likert
#' }
#'
#' Soporta dos modos:
#' \itemize{
#'   \item \code{"una_por_factor"}: genera K archivos, uno por historia.
#'         Cada participante responderia UN solo cuestionario.
#'   \item \code{"compilado"}: un solo archivo con TODAS las historias
#'         (util para piloto / juicio de expertos).
#' }
#'
#' Exporta a Markdown, DOCX (officer + flextable, layout profesional),
#' HTML y TXT.
#'
#' @param escala_h Objeto \code{semilla_historias} devuelto por
#'   \code{generar_escala_historias()}.
#' @param escala_respuesta Objeto \code{semilla_escala_respuesta} con los
#'   anclajes. Si NULL, se usa Likert 1-6 de acuerdo (default del PDVS).
#' @param nombre_test Nombre comercial del instrumento.
#' @param subtitulo Subtitulo opcional.
#' @param autor Nombre del autor.
#' @param version Version del instrumento.
#' @param incluir_datos Logico. Incluir bloque demografico al inicio.
#' @param datos_solicitados Vector de campos demograficos.
#' @param instrucciones Texto custom; si NULL, se autogenera.
#' @param archivo Ruta de salida SIN extension.
#' @param formato Vector con: "md", "docx", "html", "txt".
#' @param modo \code{"una_por_factor"} (default, K archivos) o
#'   \code{"compilado"} (un archivo con todas las historias).
#' @param sufijo_archivo Sufijo a anteponer al nombre del factor en cada
#'   archivo. Default \code{"_"}. Resultado: \code{<archivo>_<factor>.docx}.
#' @param idioma "es", "en", "pt".
#' @param imagenes Lista nombrada (factor -> ruta de imagen PNG) usada en el
#'   modo \code{"completo_horizontal"}. NULL por defecto.
#' @param verbose Mostrar progreso.
#'
#' @return Lista con clase \code{semilla_test_historias_multi} (modo
#'   "una_por_factor") o \code{semilla_test_historias} (modo "compilado").
#'   Incluye rutas de archivos generados.
#'
#' @export
ensamblar_test_historias <- function(
  escala_h,
  escala_respuesta   = NULL,
  nombre_test        = NULL,
  subtitulo          = NULL,
  autor              = NULL,
  version            = "1.0",
  incluir_datos      = TRUE,
  datos_solicitados  = c("codigo", "edad", "sexo", "nivel_educativo",
                          "estado_civil", "fecha"),
  instrucciones      = NULL,
  archivo            = NULL,
  formato            = c("md", "docx"),
  modo               = c("una_por_factor", "compilado", "completo_horizontal"),
  sufijo_archivo     = "_",
  idioma             = NULL,
  imagenes           = NULL,    # named list factor -> ruta png (modo completo)
  verbose            = TRUE
) {

  if (!inherits(escala_h, "semilla_historias"))
    stop("'escala_h' debe ser un objeto semilla_historias.")
  modo    <- match.arg(modo)
  formato <- tolower(formato)
  if (is.null(idioma)) idioma <- escala_h$idioma %||% "es"

  # Anclajes default: Likert 1-5 acuerdo con neutral (alineado con NEQ -
  # Hasking & Boyes, 2018 - escala estandar mas usada en propension NSSI)
  if (!is.null(escala_respuesta) &&
      inherits(escala_respuesta, "semilla_escala_respuesta")) {
    anclajes <- escala_respuesta$anclajes
    tipo_esc <- escala_respuesta$tipo_escala
    n_puntos <- escala_respuesta$n_puntos
  } else {
    if (idioma == "en") {
      anclajes <- c("1" = "Strongly disagree",
                    "2" = "Disagree",
                    "3" = "Neither agree nor disagree",
                    "4" = "Agree",
                    "5" = "Strongly agree")
    } else {
      anclajes <- c("1" = "Totalmente en desacuerdo",
                    "2" = "En desacuerdo",
                    "3" = "Ni de acuerdo ni en desacuerdo",
                    "4" = "De acuerdo",
                    "5" = "Totalmente de acuerdo")
    }
    tipo_esc <- "acuerdo"
    n_puntos <- 5L
  }

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Escala basada en historias - ",
                          .capitalizar(substr(escala_h$concepto, 1, 50)))
  }
  if (is.null(instrucciones)) {
    instrucciones <- .instrucciones_historias(idioma)
  }

  if (modo == "una_por_factor") {
    if (verbose) cat("\n[ensamblar_test_historias] Modo: una por factor (",
                      nrow(escala_h$historias), " archivos)\n", sep = "")

    resultados <- list()
    for (i in seq_len(nrow(escala_h$historias))) {
      factor_i <- escala_h$historias$factor[i]
      texto_i  <- escala_h$historias$texto[i]

      sub_i <- if (is.null(subtitulo))
        paste0(if (idioma == "en") "Story: " else "Historia: ", factor_i)
      else
        paste0(subtitulo, " - ", factor_i)

      arch_i <- if (!is.null(archivo))
        paste0(archivo, sufijo_archivo, .normalizar_nombre(factor_i))
      else NULL

      r <- .ensamblar_una_historia(
        escala_h          = escala_h,
        factor_actual     = factor_i,
        texto_historia    = texto_i,
        nombre_test       = nombre_test,
        subtitulo         = sub_i,
        autor             = autor, version = version,
        incluir_datos     = incluir_datos,
        datos_solicitados = datos_solicitados,
        instrucciones     = instrucciones,
        anclajes          = anclajes,
        tipo_esc          = tipo_esc, n_puntos = n_puntos,
        archivo           = arch_i,
        formato           = formato,
        idioma            = idioma,
        verbose           = verbose
      )
      resultados[[factor_i]] <- r
    }
    class(resultados) <- c("semilla_test_historias_multi", "list")
    return(resultados)
  }

  # modo == "completo_horizontal"
  if (modo == "completo_horizontal") {
    if (verbose) cat("\n[ensamblar_test_historias] Modo: completo_horizontal",
                      " (1 archivo, ", nrow(escala_h$historias),
                      " paginas landscape: historia + imagen + items)\n", sep = "")

    arch_h <- if (!is.null(archivo)) paste0(archivo, "_completo_horizontal") else NULL
    r <- .ensamblar_completo_horizontal(
      escala_h          = escala_h,
      nombre_test       = nombre_test,
      subtitulo         = subtitulo,
      autor             = autor, version = version,
      incluir_datos     = incluir_datos,
      datos_solicitados = datos_solicitados,
      instrucciones     = instrucciones,
      anclajes          = anclajes,
      tipo_esc          = tipo_esc, n_puntos = n_puntos,
      archivo           = arch_h,
      idioma            = idioma,
      imagenes          = imagenes,
      verbose           = verbose
    )
    class(r) <- c("semilla_test_historias_completo", "list")
    return(r)
  }

  # modo == "compilado"
  if (verbose) cat("\n[ensamblar_test_historias] Modo: compilado (1 archivo)\n")

  arch_c <- if (!is.null(archivo)) paste0(archivo, "_compilado") else NULL
  sub_c <- if (is.null(subtitulo))
    paste0(if (idioma == "en") "All stories - " else "Todas las historias - ",
           nrow(escala_h$historias), if (idioma == "en") " stories" else " historias")
  else subtitulo

  r <- .ensamblar_compilado_historias(
    escala_h          = escala_h,
    nombre_test       = nombre_test,
    subtitulo         = sub_c,
    autor             = autor, version = version,
    incluir_datos     = incluir_datos,
    datos_solicitados = datos_solicitados,
    instrucciones     = instrucciones,
    anclajes          = anclajes,
    tipo_esc          = tipo_esc, n_puntos = n_puntos,
    archivo           = arch_c,
    formato           = formato,
    idioma            = idioma,
    verbose           = verbose
  )
  class(r) <- c("semilla_test_historias", "list")
  r
}


# =============================================================================
# Construir markdown para una historia individual
# =============================================================================

#' @keywords internal
.ensamblar_una_historia <- function(escala_h, factor_actual, texto_historia,
                                     nombre_test, subtitulo,
                                     autor, version,
                                     incluir_datos, datos_solicitados,
                                     instrucciones, anclajes, tipo_esc,
                                     n_puntos, archivo, formato, idioma,
                                     verbose) {

  md <- .construir_md_historia(
    nombre_test    = nombre_test,
    subtitulo      = subtitulo,
    autor          = autor, version = version,
    incluir_datos  = incluir_datos,
    datos_solicitados = datos_solicitados,
    instrucciones  = instrucciones,
    introduccion   = escala_h$introduccion,
    factor         = factor_actual,
    texto_historia = texto_historia,
    items_pres     = escala_h$items,
    anclajes       = anclajes,
    idioma         = idioma
  )

  archivos_generados <- character(0)
  if (!is.null(archivo)) {
    docx_data <- list(
      nombre_test       = nombre_test,
      subtitulo         = subtitulo,
      autor             = autor, version = version,
      datos_solicitados = if (incluir_datos) datos_solicitados else NULL,
      instrucciones     = instrucciones,
      introduccion      = escala_h$introduccion,
      factor            = factor_actual,
      texto_historia    = texto_historia,
      items_pres        = escala_h$items,
      anclajes          = anclajes,
      idioma            = idioma,
      historias_compiladas = NULL
    )
    archivos_generados <- .exportar_test_historias(
      md = md, archivo = archivo, formato = formato,
      docx_data = docx_data, verbose = verbose
    )
  }

  resultado <- list(
    factor             = factor_actual,
    nombre_test        = nombre_test,
    subtitulo          = subtitulo,
    introduccion       = escala_h$introduccion,
    texto_historia     = texto_historia,
    items              = escala_h$items,
    anclajes           = anclajes,
    texto_md           = md,
    archivos           = archivos_generados
  )
  class(resultado) <- c("semilla_test_historias", "list")
  resultado
}


#' @keywords internal
.ensamblar_compilado_historias <- function(escala_h, nombre_test, subtitulo,
                                            autor, version, incluir_datos,
                                            datos_solicitados, instrucciones,
                                            anclajes, tipo_esc, n_puntos,
                                            archivo, formato, idioma, verbose) {

  md <- .construir_md_compilado(
    nombre_test    = nombre_test,
    subtitulo      = subtitulo,
    autor          = autor, version = version,
    incluir_datos  = incluir_datos,
    datos_solicitados = datos_solicitados,
    instrucciones  = instrucciones,
    introduccion   = escala_h$introduccion,
    historias      = escala_h$historias,
    items_pres     = escala_h$items,
    anclajes       = anclajes,
    idioma         = idioma
  )

  archivos_generados <- character(0)
  if (!is.null(archivo)) {
    docx_data <- list(
      nombre_test          = nombre_test,
      subtitulo            = subtitulo,
      autor                = autor, version = version,
      datos_solicitados    = if (incluir_datos) datos_solicitados else NULL,
      instrucciones        = instrucciones,
      introduccion         = escala_h$introduccion,
      historias_compiladas = escala_h$historias,
      items_pres           = escala_h$items,
      anclajes             = anclajes,
      idioma               = idioma
    )
    archivos_generados <- .exportar_test_historias(
      md = md, archivo = archivo, formato = formato,
      docx_data = docx_data, verbose = verbose
    )
  }

  list(
    nombre_test       = nombre_test,
    subtitulo         = subtitulo,
    introduccion      = escala_h$introduccion,
    historias         = escala_h$historias,
    items             = escala_h$items,
    anclajes          = anclajes,
    texto_md          = md,
    archivos          = archivos_generados
  )
}


# =============================================================================
# Markdown builders
# =============================================================================

#' @keywords internal
.construir_md_historia <- function(nombre_test, subtitulo, autor, version,
                                    incluir_datos, datos_solicitados,
                                    instrucciones, introduccion,
                                    factor, texto_historia,
                                    items_pres, anclajes, idioma) {

  L <- .labels_historias(idioma)
  out <- character(0)

  out <- c(out, paste0("# ", nombre_test))
  if (!is.null(subtitulo)) out <- c(out, paste0("### ", subtitulo))
  out <- c(out, "")
  meta <- character(0)
  if (!is.null(autor))   meta <- c(meta, paste0("**", L$autor_txt, ":** ", autor))
  if (!is.null(version)) meta <- c(meta, paste0("**", L$version_txt, ":** ", version))
  if (length(meta) > 0)  out <- c(out, paste(meta, collapse = " \u00B7 "), "")
  out <- c(out, "---", "")

  if (incluir_datos) {
    out <- c(out, paste0("## ", L$datos_encab), "",
              .bloque_datos_demograficos(datos_solicitados, idioma),
              "", "---", "")
  }

  out <- c(out, paste0("## ", L$instr_encab), "", instrucciones, "", "---", "")

  out <- c(out, paste0("## ", L$historia_encab),
            paste0("**", L$factor_txt, ":** ", factor), "",
            "*", L$intro_txt, ".*", "", introduccion, "",
            "*", L$desenlace_txt, ":*", "", texto_historia, "", "---", "")

  out <- c(out, paste0("## ", L$items_encab), "")
  ancla_inline <- paste(paste0(names(anclajes), " = ", unname(anclajes)),
                         collapse = " \u00B7 ")
  out <- c(out, paste0("*", ancla_inline, "*"), "")

  cols_val <- paste0(" ", names(anclajes), " ")
  encab <- paste0("| N | ", L$afirmacion, " | ",
                  paste(cols_val, collapse = " | "), " |")
  sep   <- paste0("|:--:|---", strrep("|:-:", length(anclajes)), "|")
  out <- c(out, encab, sep)

  # Filtrar items por factor si la escala usa items_modo='por_historia'
  if ("factor" %in% names(items_pres) && any(!is.na(items_pres$factor))) {
    items_f <- items_pres[!is.na(items_pres$factor) &
                            items_pres$factor == factor, , drop = FALSE]
    if (nrow(items_f) == 0L) items_f <- items_pres   # fallback
  } else {
    items_f <- items_pres
  }

  for (i in seq_len(nrow(items_f))) {
    out <- c(out, paste0("| ", i, " | ", items_f$item[i], " | ",
                          paste(rep("( )", length(anclajes)), collapse = " | "),
                          " |"))
  }
  out <- c(out, "", "---", "")
  paste(out, collapse = "\n")
}


#' @keywords internal
.construir_md_compilado <- function(nombre_test, subtitulo, autor, version,
                                     incluir_datos, datos_solicitados,
                                     instrucciones, introduccion, historias,
                                     items_pres, anclajes, idioma) {

  L <- .labels_historias(idioma)
  out <- character(0)
  out <- c(out, paste0("# ", nombre_test))
  if (!is.null(subtitulo)) out <- c(out, paste0("### ", subtitulo))
  out <- c(out, "")
  meta <- character(0)
  if (!is.null(autor))   meta <- c(meta, paste0("**", L$autor_txt, ":** ", autor))
  if (!is.null(version)) meta <- c(meta, paste0("**", L$version_txt, ":** ", version))
  if (length(meta) > 0)  out <- c(out, paste(meta, collapse = " \u00B7 "), "")
  out <- c(out, "---", "")

  if (incluir_datos) {
    out <- c(out, paste0("## ", L$datos_encab), "",
              .bloque_datos_demograficos(datos_solicitados, idioma),
              "", "---", "")
  }

  out <- c(out, paste0("## ", L$instr_encab), "", instrucciones, "")
  out <- c(out, "*", L$nota_compilado, "*", "", "---", "")

  for (i in seq_len(nrow(historias))) {
    out <- c(out, paste0("## ", L$historia_encab, " ", i, " - ",
                          historias$factor[i]), "",
              "*", L$intro_txt, ".*", "", introduccion, "",
              "*", L$desenlace_txt, ":*", "", historias$texto[i], "", "")

    ancla_inline <- paste(paste0(names(anclajes), " = ", unname(anclajes)),
                           collapse = " \u00B7 ")
    out <- c(out, paste0("*", ancla_inline, "*"), "")

    cols_val <- paste0(" ", names(anclajes), " ")
    encab <- paste0("| N | ", L$afirmacion, " | ",
                    paste(cols_val, collapse = " | "), " |")
    sep   <- paste0("|:--:|---", strrep("|:-:", length(anclajes)), "|")
    out <- c(out, encab, sep)

    # Filtrar items por factor si la escala usa items_modo='por_historia'
    if ("factor" %in% names(items_pres) && any(!is.na(items_pres$factor))) {
      factor_i_actual <- historias$factor[i]
      items_i <- items_pres[!is.na(items_pres$factor) &
                              items_pres$factor == factor_i_actual, , drop = FALSE]
      if (nrow(items_i) == 0L) items_i <- items_pres
    } else {
      items_i <- items_pres
    }

    for (j in seq_len(nrow(items_i))) {
      out <- c(out, paste0("| ", j, " | ", items_i$item[j], " | ",
                            paste(rep("( )", length(anclajes)), collapse = " | "),
                            " |"))
    }
    out <- c(out, "", "---", "")
  }
  paste(out, collapse = "\n")
}


# =============================================================================
# Etiquetas i18n
# =============================================================================

#' @keywords internal
.labels_historias <- function(idioma) {
  if (idioma == "en") {
    list(
      datos_encab    = "Participant information",
      instr_encab    = "Instructions",
      historia_encab = "Story",
      items_encab    = "Items",
      afirmacion     = "Statement",
      autor_txt      = "Author",
      version_txt    = "Version",
      factor_txt     = "Factor",
      intro_txt      = "Introduction (common to all stories)",
      desenlace_txt  = "Specific development",
      nota_compilado = "All stories share the same set of items below."
    )
  } else {
    list(
      datos_encab    = "Datos del participante",
      instr_encab    = "Instrucciones",
      historia_encab = "Historia",
      items_encab    = "\u00CDtems de percepci\u00F3n",
      afirmacion     = "Afirmaci\u00F3n",
      autor_txt      = "Autor",
      version_txt    = "Versi\u00F3n",
      factor_txt     = "Factor",
      intro_txt      = "Introducci\u00F3n (com\u00FAn a todas las historias)",
      desenlace_txt  = "Desenlace espec\u00EDfico",
      nota_compilado = "Todas las historias comparten el mismo bloque de \u00EDtems."
    )
  }
}


#' @keywords internal
.instrucciones_historias <- function(idioma) {
  if (idioma == "en") {
    paste(
      "Below you will read a short STORY about two people in a relationship,",
      "followed by a series of statements about that story. Read the story",
      "first and then indicate the degree to which you agree or disagree with",
      "each statement. There are no right or wrong answers. Mark only ONE",
      "option per statement."
    )
  } else {
    paste(
      "A continuaci\u00F3n leer\u00E1 una HISTORIA breve sobre dos personas en una",
      "relaci\u00F3n de pareja, seguida de una serie de afirmaciones sobre esa",
      "historia. Lea primero la historia y luego indique el grado en que",
      "est\u00E1 de acuerdo o en desacuerdo con cada afirmaci\u00F3n. No hay respuestas",
      "correctas o incorrectas. Marque UNA SOLA opci\u00F3n por afirmaci\u00F3n."
    )
  }
}


#' @keywords internal
.normalizar_nombre <- function(s) {
  s <- tolower(s)
  # quitar tildes basicas
  s <- chartr("\u00E1\u00E9\u00ED\u00F3\u00FA\u00F1\u00FC", "aeiounu", s)
  s <- gsub("[^a-z0-9_]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}


# =============================================================================
# Exportador (md, txt, docx via officer, html via rmarkdown)
# =============================================================================

#' @keywords internal
.exportar_test_historias <- function(md, archivo, formato,
                                      docx_data, verbose = TRUE) {

  archivos <- character(0)
  dir_salida <- dirname(archivo)
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

  base_md <- paste0(archivo, ".md")
  writeLines(md, base_md, useBytes = TRUE)
  if ("md" %in% formato) archivos <- c(archivos, base_md)

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

  if ("docx" %in% formato) {
    docx_ok <- FALSE
    if (requireNamespace("officer",   quietly = TRUE) &&
        requireNamespace("flextable", quietly = TRUE)) {
      ruta_docx <- tryCatch(
        .exportar_historia_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("officer DOCX (historias) fallo: ", conditionMessage(e),
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

  formatos_rmd <- intersect(formato, c("html", "pdf"))
  for (f in formatos_rmd) {
    ruta_out <- .exportar_via_rmarkdown(base_md, archivo, f, verbose)
    if (!is.null(ruta_out)) archivos <- c(archivos, ruta_out)
  }

  if (!("md" %in% formato)) try(file.remove(base_md), silent = TRUE)
  archivos
}


#' @keywords internal
.exportar_historia_docx_officer <- function(archivo, d, verbose) {

  off <- function(name) get(name, envir = asNamespace("officer"))
  flx <- function(name) get(name, envir = asNamespace("flextable"))

  fp_par    <- off("fp_par")
  fp_text   <- off("fp_text")
  fp_border <- off("fp_border")
  ftext     <- off("ftext")
  fpar      <- off("fpar")

  col_titulo  <- "#1F3864"
  col_subtit  <- "#595959"
  col_seccion <- "#1F3864"
  col_borde   <- "#BFBFBF"
  col_fondo_h <- "#1F3864"
  col_text_h  <- "#FFFFFF"
  col_fondo_a <- "#EAEEF5"
  col_texto   <- "#262626"
  fuente <- "Calibri"
  es_en  <- identical(d$idioma, "en")
  L <- .labels_historias(d$idioma %||% "es")

  border_outer <- fp_border(color = col_seccion, width = 1.0, style = "solid")
  border_inner <- fp_border(color = col_borde,   width = 0.5, style = "solid")
  border_sec   <- fp_border(color = col_seccion, width = 1.2, style = "solid")

  doc <- off("read_docx")()
  default_section <- off("prop_section")(
    page_size    = off("page_size")(width = 8.27, height = 11.69, orient = "portrait"),
    page_margins = off("page_mar")(top = 0.79, bottom = 0.79,
                                    left = 0.79, right = 0.79,
                                    header = 0.5, footer = 0.5, gutter = 0),
    type = "continuous"
  )

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

  estilizar_tabla <- function(ft, alt = TRUE) {
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    ft <- flx("bg")(ft,    bg = col_fondo_h, part = "header")
    ft <- flx("color")(ft, color = col_text_h, part = "header")
    ft <- flx("bold")(ft,  part = "header")
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
                  color = col_titulo, align = "center", pad_top = 0, pad_bot = 4)
  if (!is.null(d$subtitulo)) {
    doc <- add_par(doc, d$subtitulo, italic = TRUE, size = 12,
                    color = col_subtit, align = "center", pad_top = 0, pad_bot = 4)
  }
  meta_chunks <- list()
  if (!is.null(d$autor)) {
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      d$autor, fp_text(font.size = 10, color = col_subtit, font.family = fuente))
  }
  if (!is.null(d$version) && nzchar(as.character(d$version))) {
    if (length(meta_chunks) > 0) {
      meta_chunks[[length(meta_chunks) + 1]] <- ftext(
        "  \u00B7  ", fp_text(font.size = 10, color = col_subtit, font.family = fuente))
    }
    etiqueta_v <- if (es_en) "Version " else "Versi\u00F3n "
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      paste0(etiqueta_v, d$version),
      fp_text(font.size = 10, color = col_subtit, font.family = fuente))
  }
  if (length(meta_chunks) > 0) {
    doc <- off("body_add_fpar")(doc, do.call(fpar, c(
      meta_chunks,
      list(fp_p = fp_par(text.align = "center",
                         padding.top = 0, padding.bottom = 14)))))
  }

  # ---------------- Datos demograficos ----------------
  if (!is.null(d$datos_solicitados) && length(d$datos_solicitados) > 0) {
    doc <- add_heading(doc, L$datos_encab)
    labels <- .etiquetas_demograficas(d$datos_solicitados, d$idioma %||% "es")
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
    ft <- flx("padding")(ft, padding.top = 5, padding.bottom = 5)
    ft <- flx("width")(ft, j = 1, width = 3.4)
    ft <- flx("width")(ft, j = 2, width = 3.3)
    doc <- flx("body_add_flextable")(doc, ft, align = "left")
    doc <- off("body_add_par")(doc, "")
  }

  # ---------------- Instrucciones ----------------
  doc <- add_heading(doc, L$instr_encab)
  doc <- add_par(doc, d$instrucciones, align = "justify",
                  pad_top = 2, pad_bot = 6)

  # ---------------- Helper: bloque de UNA historia + items ----------------
  agregar_historia_y_items <- function(doc, encab, intro_txt, factor_label,
                                        desenlace_txt, items_df, anclajes) {
    doc <- add_heading(doc, encab)
    if (!is.null(factor_label) && nzchar(factor_label)) {
      doc <- add_par(doc, paste0(L$factor_txt, ": ", factor_label),
                      bold = TRUE, size = 11, align = "left",
                      pad_top = 0, pad_bot = 6)
    }
    doc <- add_par(doc, paste0(L$intro_txt, "."), italic = TRUE, size = 10,
                    color = col_subtit, align = "left", pad_top = 2, pad_bot = 2)
    doc <- add_par(doc, intro_txt, align = "justify", pad_top = 0, pad_bot = 6)
    doc <- add_par(doc, paste0(L$desenlace_txt, ":"), italic = TRUE, size = 10,
                    color = col_subtit, align = "left", pad_top = 2, pad_bot = 2)
    doc <- add_par(doc, desenlace_txt, align = "justify", pad_top = 0, pad_bot = 10)

    # Recordatorio anclajes
    ancla_inline <- paste(
      paste0(names(anclajes), " = ", unname(anclajes)),
      collapse = "  \u00B7  "
    )
    doc <- add_par(doc, ancla_inline, italic = TRUE, size = 9.5,
                    color = col_subtit, align = "left",
                    pad_top = 2, pad_bot = 6)

    # Tabla de items con columnas Likert
    vals <- names(anclajes)
    df_items <- data.frame(N = seq_len(nrow(items_df)),
                            Afirmacion = items_df$item,
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
    for (v in vals) df_items[[v]] <- "\u25CB"
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
    doc <- off("body_add_par")(doc, "")
    doc
  }

  # Helper: filtrar items por factor si la escala usa items_modo='por_historia'
  filtrar_por_factor <- function(items_df, factor_lbl) {
    if ("factor" %in% names(items_df) && any(!is.na(items_df$factor))) {
      sel <- items_df[!is.na(items_df$factor) &
                        items_df$factor == factor_lbl, , drop = FALSE]
      if (nrow(sel) > 0L) return(sel)
    }
    items_df
  }

  if (is.null(d$historias_compiladas)) {
    # Una sola historia
    doc <- agregar_historia_y_items(
      doc, encab = L$historia_encab,
      intro_txt = d$introduccion,
      factor_label = d$factor,
      desenlace_txt = d$texto_historia,
      items_df = filtrar_por_factor(d$items_pres, d$factor),
      anclajes = d$anclajes
    )
  } else {
    # Compilado
    for (i in seq_len(nrow(d$historias_compiladas))) {
      doc <- off("body_add_break")(doc)
      doc <- agregar_historia_y_items(
        doc,
        encab = paste0(L$historia_encab, " ", i, " - ",
                       d$historias_compiladas$factor[i]),
        intro_txt = d$introduccion,
        factor_label = d$historias_compiladas$factor[i],
        desenlace_txt = d$historias_compiladas$texto[i],
        items_df = filtrar_por_factor(d$items_pres,
                                       d$historias_compiladas$factor[i]),
        anclajes = d$anclajes
      )
    }
  }

  doc <- off("body_set_default_section")(doc, default_section)

  ruta_docx <- paste0(archivo, ".docx")
  print(doc, target = ruta_docx)
  if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  ruta_docx
}


# =============================================================================
# Modo COMPLETO HORIZONTAL: 1 archivo, K paginas landscape, cada una con
# historia + imagen + items
# =============================================================================

#' @keywords internal
.ensamblar_completo_horizontal <- function(escala_h, nombre_test, subtitulo,
                                            autor, version, incluir_datos,
                                            datos_solicitados, instrucciones,
                                            anclajes, tipo_esc, n_puntos,
                                            archivo, idioma, imagenes,
                                            verbose) {

  if (!requireNamespace("officer", quietly = TRUE) ||
      !requireNamespace("flextable", quietly = TRUE)) {
    stop("Se requieren los paquetes 'officer' y 'flextable' para modo ",
         "completo_horizontal. Instalalos con install.packages(c('officer','flextable')).")
  }

  off <- function(name) get(name, envir = asNamespace("officer"))
  flx <- function(name) get(name, envir = asNamespace("flextable"))

  fp_par    <- off("fp_par");   fp_text   <- off("fp_text")
  fp_border <- off("fp_border"); ftext    <- off("ftext")
  fpar      <- off("fpar");     fp_cell   <- off("fp_cell")

  col_titulo  <- "#1F3864"
  col_subtit  <- "#595959"
  col_seccion <- "#1F3864"
  col_borde   <- "#BFBFBF"
  col_fondo_h <- "#1F3864"
  col_text_h  <- "#FFFFFF"
  col_fondo_a <- "#EAEEF5"
  col_texto   <- "#262626"
  fuente <- "Calibri"
  es_en  <- identical(idioma, "en")
  L      <- .labels_historias(idioma %||% "es")

  border_outer <- fp_border(color = col_seccion, width = 1.0, style = "solid")
  border_inner <- fp_border(color = col_borde,   width = 0.5, style = "solid")

  doc <- off("read_docx")()

  add_par <- function(doc, txt, italic = FALSE, bold = FALSE,
                      size = 11, align = "justify",
                      color = col_texto, pad_top = 4, pad_bot = 4) {
    off("body_add_fpar")(doc, fpar(
      ftext(txt, fp_text(font.size = size, bold = bold, italic = italic,
                         color = color, font.family = fuente)),
      fp_p = fp_par(text.align = align, padding.top = pad_top,
                    padding.bottom = pad_bot, line_spacing = 1.15)
    ))
  }

  estilizar_tabla <- function(ft) {
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    ft <- flx("bg")(ft,    bg = col_fondo_h, part = "header")
    ft <- flx("color")(ft, color = col_text_h, part = "header")
    ft <- flx("bold")(ft,  part = "header")
    ft <- flx("border_outer")(ft, border = border_outer, part = "all")
    ft <- flx("border_inner_h")(ft, border = border_inner, part = "all")
    ft <- flx("border_inner_v")(ft, border = border_inner, part = "all")
    ft <- flx("padding")(ft, padding.top = 3, padding.bottom = 3,
                          padding.left = 5, padding.right = 5)
    ft <- flx("valign")(ft, valign = "center", part = "all")
    n <- flx("nrow_part")(ft, part = "body")
    if (!is.null(n) && n >= 2) {
      ft <- flx("bg")(ft, i = seq(2, n, by = 2), bg = col_fondo_a, part = "body")
    }
    ft
  }

  vals <- names(anclajes)

  # Recorrer las K historias - una pagina LANDSCAPE por cada
  for (k in seq_len(nrow(escala_h$historias))) {

    factor_k <- escala_h$historias$factor[k]
    texto_k  <- escala_h$historias$texto[k]

    # Filtrar items por factor (modo por_historia)
    items_pres <- escala_h$items
    if ("factor" %in% names(items_pres) && any(!is.na(items_pres$factor))) {
      items_k <- items_pres[!is.na(items_pres$factor) &
                              items_pres$factor == factor_k, , drop = FALSE]
      if (nrow(items_k) == 0L) items_k <- items_pres
    } else {
      items_k <- items_pres
    }

    # Encabezado de la pagina (formato consistente entre paginas)
    if (k == 1) {
      # Primera pagina: nombre del test + subtitulo (solo aqui)
      doc <- add_par(doc, nombre_test, bold = TRUE, size = 16,
                      color = col_titulo, align = "center",
                      pad_top = 0, pad_bot = 2)
      if (!is.null(subtitulo) && nzchar(subtitulo)) {
        doc <- add_par(doc, subtitulo, italic = TRUE, size = 11,
                        color = col_subtit, align = "center",
                        pad_top = 0, pad_bot = 6)
      }
    }
    # Encabezado de la historia (mismo formato en todas las paginas)
    doc <- add_par(doc, paste0(L$historia_encab, " ", k, ": ", factor_k),
                    bold = TRUE, size = 14, color = col_titulo,
                    align = "center", pad_top = 0, pad_bot = 6)

    # Tabla 2 columnas: historia (izq) + imagen (der)
    df_layout <- data.frame(
      historia = paste(
        if (!is.null(escala_h$introduccion)) paste0(escala_h$introduccion, "\n\n") else "",
        texto_k, sep = ""
      ),
      imagen   = "",
      stringsAsFactors = FALSE
    )
    names(df_layout) <- c(
      if (es_en) "Story" else "Historia",
      if (es_en) "Comic strip image" else "Historieta (imagen)"
    )
    ft_layout <- flx("flextable")(df_layout)
    ft_layout <- flx("font")(ft_layout, fontname = fuente, part = "all")
    ft_layout <- flx("fontsize")(ft_layout, size = 11, part = "body")
    ft_layout <- flx("fontsize")(ft_layout, size = 12, part = "header")
    ft_layout <- flx("bg")(ft_layout, bg = col_fondo_h, part = "header")
    ft_layout <- flx("color")(ft_layout, color = col_text_h, part = "header")
    ft_layout <- flx("bold")(ft_layout, part = "header")
    ft_layout <- flx("align")(ft_layout, align = "center", part = "header")
    ft_layout <- flx("align")(ft_layout, j = 1, align = "justify", part = "body")
    ft_layout <- flx("align")(ft_layout, j = 2, align = "center", part = "body")
    ft_layout <- flx("valign")(ft_layout, valign = "top", part = "body")
    ft_layout <- flx("border_outer")(ft_layout, border = border_outer, part = "all")
    ft_layout <- flx("border_inner_v")(ft_layout, border = border_inner, part = "all")
    ft_layout <- flx("border_inner_h")(ft_layout, border = border_inner, part = "all")
    ft_layout <- flx("padding")(ft_layout, padding.top = 8, padding.bottom = 8,
                                  padding.left = 8, padding.right = 8)
    ft_layout <- flx("width")(ft_layout, j = 1, width = 4.5)
    ft_layout <- flx("width")(ft_layout, j = 2, width = 5.5)
    ft_layout <- flx("height")(ft_layout, height = 3.5, part = "body")

    # Si hay imagen, insertarla; si no, dejar placeholder
    img_path <- NULL
    if (!is.null(imagenes) && factor_k %in% names(imagenes)) {
      img_path <- imagenes[[factor_k]]
    }

    if (!is.null(img_path) && file.exists(img_path)) {
      ft_layout <- flx("compose")(ft_layout, j = 2, i = 1, part = "body",
        value = flx("as_paragraph")(
          flx("as_image")(img_path, width = 5.2, height = 3.3)
        )
      )
    } else {
      placeholder <- if (es_en)
        "[ Paste comic strip image here ]\n(generated from prompts_historieta() prompt)"
      else
        "[ Pega aqui la imagen de la historieta ]\n(generada con prompts_historieta())"
      ft_layout <- flx("compose")(ft_layout, j = 2, i = 1, part = "body",
        value = flx("as_paragraph")(
          flx("as_chunk")(placeholder,
                          props = fp_text(font.size = 10, italic = TRUE,
                                          color = col_subtit,
                                          font.family = fuente))
        )
      )
    }

    doc <- flx("body_add_flextable")(doc, ft_layout, align = "center")
    doc <- off("body_add_par")(doc, "")

    # Tabla de anclajes inline
    ancla_inline <- paste(paste0(names(anclajes), " = ", unname(anclajes)),
                          collapse = "  \u00B7  ")
    doc <- add_par(doc, ancla_inline, italic = TRUE, size = 9.5,
                    color = col_subtit, align = "left",
                    pad_top = 4, pad_bot = 4)

    # Tabla de items con columnas Likert
    df_items <- data.frame(N = seq_len(nrow(items_k)),
                            Afirmacion = items_k$item,
                            stringsAsFactors = FALSE,
                            check.names = FALSE)
    for (v in vals) df_items[[v]] <- "\u25CB"  # circulo vacio
    names(df_items) <- c("N",
                          if (es_en) "Statement" else "Afirmaci\u00F3n",
                          vals)
    ft <- flx("flextable")(df_items)
    ft <- flx("fontsize")(ft, size = 9.5, part = "body")
    ft <- flx("fontsize")(ft, size = 10.5, part = "header")
    ft <- flx("color")(ft, color = col_texto, part = "body")
    ft <- flx("align")(ft, align = "center", part = "header")
    ft <- flx("align")(ft, j = 1, align = "center", part = "body")
    ft <- flx("align")(ft, j = 2, align = "left", part = "body")
    for (idx in seq_along(vals)) {
      ft <- flx("align")(ft, j = 2 + idx, align = "center", part = "body")
    }
    ft <- flx("width")(ft, j = 1, width = 0.35)
    ft <- flx("width")(ft, j = 2, width = 5.5)
    for (idx in seq_along(vals)) {
      ft <- flx("width")(ft, j = 2 + idx, width = 0.55)
    }
    ft <- estilizar_tabla(ft)
    doc <- flx("body_add_flextable")(doc, ft, align = "center")

    # Page break entre historias (excepto la ultima)
    if (k < nrow(escala_h$historias)) {
      doc <- off("body_add_break")(doc)
    }
  }

  # Aplicar seccion landscape global
  default_section <- off("prop_section")(
    page_size    = off("page_size")(width = 11.69, height = 8.27,
                                     orient = "landscape"),
    page_margins = off("page_mar")(top = 0.5, bottom = 0.5,
                                    left = 0.5, right = 0.5,
                                    header = 0.3, footer = 0.3, gutter = 0),
    type = "continuous"
  )
  doc <- off("body_set_default_section")(doc, default_section)

  archivos_generados <- character(0)
  if (!is.null(archivo)) {
    ruta_docx <- paste0(archivo, ".docx")
    print(doc, target = ruta_docx)
    archivos_generados <- ruta_docx
    if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  }

  list(
    nombre_test = nombre_test,
    subtitulo   = subtitulo,
    historias   = escala_h$historias,
    items       = escala_h$items,
    anclajes    = anclajes,
    archivos    = archivos_generados
  )
}


# =============================================================================
# Print methods
# =============================================================================

#' @export
print.semilla_test_historias <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test de historias ensamblado (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Nombre   : ", x$nombre_test, "\n", sep = "")
  if (!is.null(x$subtitulo)) cat("  Subtitulo: ", x$subtitulo, "\n", sep = "")
  if (!is.null(x$factor))    cat("  Factor   : ", x$factor, "\n", sep = "")
  cat("  N items  : ", nrow(x$items), "\n", sep = "")
  if (length(x$archivos) > 0) {
    cat("  Archivos :\n")
    for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}

#' @export
print.semilla_test_historias_completo <- function(x, ...) {
  cat("\n===========================================================\n")
  cat("  Test horizontal completo (historia + imagen + items)\n")
  cat("===========================================================\n")
  cat("  Nombre   : ", x$nombre_test, "\n", sep = "")
  if (!is.null(x$subtitulo)) cat("  Subtitulo: ", x$subtitulo, "\n", sep = "")
  cat("  Historias: ", nrow(x$historias),
      " (1 pagina landscape c/u con historia + imagen + items)\n", sep = "")
  cat("  Items    : ", nrow(x$items), "\n", sep = "")
  if (length(x$archivos) > 0) {
    cat("  Archivos :\n")
    for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}

#' @export
print.semilla_test_historias_multi <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test de historias - K cuestionarios (SeMiLLa)\n")
  cat("===========================================================\n")
  for (nm in names(x)) {
    cat(".. [", nm, "] ", x[[nm]]$nombre_test, "\n", sep = "")
    for (a in x[[nm]]$archivos) cat("     - ", a, "\n", sep = "")
  }
  cat("===========================================================\n")
  cat("Acceda con: test[['", names(x)[1], "']]\n\n", sep = "")
  invisible(x)
}
