#' @title Ensamblar test Guttman
#'
#' @description
#' Toma un objeto \code{semilla_guttman} y produce el cuestionario listo
#' para administrar. Cada item se imprime como un bloque:
#'
#' \preformatted{
#'   1. <Stem>
#'      ( ) Alternativa nivel 0
#'      ( ) Alternativa nivel 1
#'      ( ) Alternativa nivel 2
#'      ( ) Alternativa nivel 3
#'      ( ) Alternativa nivel 4
#' }
#'
#' Soporta exportacion a Markdown y DOCX (officer + flextable). El DOCX
#' usa el mismo estilo profesional de \code{ensamblar_test()} pero con
#' layout de bloque por item (no tabla Likert).
#'
#' @param escala_g Objeto \code{semilla_guttman}.
#' @param nombre_test Nombre del instrumento.
#' @param subtitulo Subtitulo opcional.
#' @param autor Autor.
#' @param version Version.
#' @param incluir_datos Logico. Incluir bloque demografico al inicio.
#' @param datos_solicitados Vector de campos demograficos.
#' @param instrucciones Texto custom; si NULL, autogenerado.
#' @param incluir_construct_map Logico. Si TRUE, anexa al final una tabla
#'   con el construct map (uso del investigador). Default FALSE.
#' @param archivo Ruta SIN extension.
#' @param formato Vector con: "md", "docx".
#' @param idioma "es" o "en".
#' @param verbose Mostrar progreso.
#'
#' @return Lista con clase \code{semilla_test_guttman} con texto markdown
#'   y rutas de archivos generados.
#'
#' @export
ensamblar_test_guttman <- function(
  escala_g,
  nombre_test            = NULL,
  subtitulo              = NULL,
  autor                  = NULL,
  version                = "1.0",
  incluir_datos          = TRUE,
  datos_solicitados      = c("codigo", "edad", "sexo", "nivel_educativo",
                              "fecha"),
  instrucciones          = NULL,
  incluir_construct_map  = FALSE,
  archivo                = NULL,
  formato                = c("md", "docx"),
  idioma                 = NULL,
  verbose                = TRUE
) {

  if (!inherits(escala_g, "semilla_guttman"))
    stop("'escala_g' debe ser un objeto semilla_guttman.")
  formato <- tolower(formato)
  if (is.null(idioma)) idioma <- escala_g$idioma %||% "es"

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Escala formato Guttman - ",
                          .capitalizar(substr(escala_g$concepto, 1, 50)))
  }
  if (is.null(instrucciones)) {
    instrucciones <- .instrucciones_guttman(idioma)
  }

  md <- .construir_md_guttman(
    nombre_test           = nombre_test,
    subtitulo             = subtitulo,
    autor                 = autor, version = version,
    incluir_datos         = incluir_datos,
    datos_solicitados     = datos_solicitados,
    instrucciones         = instrucciones,
    items                 = escala_g$items,
    alternativas          = escala_g$alternativas,
    construct_map         = escala_g$construct_map,
    incluir_construct_map = incluir_construct_map,
    idioma                = idioma
  )

  archivos_generados <- character(0)
  if (!is.null(archivo)) {
    docx_data <- list(
      nombre_test           = nombre_test,
      subtitulo             = subtitulo,
      autor                 = autor, version = version,
      datos_solicitados     = if (incluir_datos) datos_solicitados else NULL,
      instrucciones         = instrucciones,
      items                 = escala_g$items,
      alternativas          = escala_g$alternativas,
      construct_map         = escala_g$construct_map,
      incluir_construct_map = incluir_construct_map,
      idioma                = idioma
    )
    archivos_generados <- .exportar_test_guttman(
      md = md, archivo = archivo, formato = formato,
      docx_data = docx_data, verbose = verbose
    )
  }

  resultado <- list(
    nombre_test       = nombre_test,
    subtitulo         = subtitulo,
    items             = escala_g$items,
    alternativas      = escala_g$alternativas,
    construct_map     = escala_g$construct_map,
    texto_md          = md,
    archivos          = archivos_generados
  )
  class(resultado) <- c("semilla_test_guttman", "list")
  resultado
}


# =============================================================================
# Helpers
# =============================================================================

#' @keywords internal
.instrucciones_guttman <- function(idioma) {
  if (idioma == "en") {
    paste(
      "Below you will find a series of items. For each one, read the",
      "stem and the response options carefully, then select the",
      "**ONE** option that best describes your current situation. The",
      "options are ordered from lower to higher levels."
    )
  } else {
    paste(
      "A continuaci\u00F3n encontrar\u00E1 una serie de \u00EDtems. Para cada uno,",
      "lea con atenci\u00F3n la pregunta y las opciones de respuesta,",
      "luego seleccione la **\u00DANICA** opci\u00F3n que mejor describe su",
      "situaci\u00F3n actual. Las opciones est\u00E1n ordenadas de menor a",
      "mayor nivel."
    )
  }
}


#' @keywords internal
.labels_guttman <- function(idioma) {
  if (idioma == "en") {
    list(
      datos_encab    = "Participant information",
      instr_encab    = "Instructions",
      items_encab    = "Items",
      cm_encab       = "Construct map (researcher reference)",
      autor_txt      = "Author",
      version_txt    = "Version",
      faceta_txt     = "Facet",
      nivel_txt      = "Level",
      descripcion_txt = "Description",
      seleccione     = "Select ONE option:"
    )
  } else {
    list(
      datos_encab    = "Datos del participante",
      instr_encab    = "Instrucciones",
      items_encab    = "\u00CDtems",
      cm_encab       = "Construct map (referencia del investigador)",
      autor_txt      = "Autor",
      version_txt    = "Versi\u00F3n",
      faceta_txt     = "Faceta",
      nivel_txt      = "Nivel",
      descripcion_txt = "Descripci\u00F3n",
      seleccione     = "Seleccione UNA opci\u00F3n:"
    )
  }
}


#' @keywords internal
.construir_md_guttman <- function(nombre_test, subtitulo, autor, version,
                                    incluir_datos, datos_solicitados,
                                    instrucciones, items, alternativas,
                                    construct_map, incluir_construct_map,
                                    idioma) {

  L <- .labels_guttman(idioma)
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

  out <- c(out, paste0("## ", L$items_encab), "")

  for (i in seq_len(nrow(items))) {
    bloque <- character(0)
    fac <- items$faceta[i]
    encab_item <- if (!is.na(fac) && nzchar(fac))
      paste0("**", i, ".** *(", L$faceta_txt, ": ", fac, ")* ", items$stem[i])
    else
      paste0("**", i, ".** ", items$stem[i])
    bloque <- c(bloque, encab_item, "")
    bloque <- c(bloque, paste0("*", L$seleccione, "*"), "")

    alts_i <- alternativas[alternativas$n_item == i, ]
    for (k in seq_len(nrow(alts_i))) {
      bloque <- c(bloque, paste0("- ( ) ", alts_i$alternativa[k]))
    }
    bloque <- c(bloque, "")
    out <- c(out, bloque)
  }

  if (isTRUE(incluir_construct_map)) {
    out <- c(out, "---", "", paste0("## ", L$cm_encab), "")
    out <- c(out, paste0("| ", L$nivel_txt, " | ", L$descripcion_txt, " |"))
    out <- c(out, "|:--:|------|")
    K <- length(construct_map)
    for (k in seq_len(K)) {
      out <- c(out, paste0("| ", names(construct_map)[k], " | ",
                            construct_map[[k]], " |"))
    }
    out <- c(out, "")
  }

  paste(out, collapse = "\n")
}


# =============================================================================
# Exportador
# =============================================================================

#' @keywords internal
.exportar_test_guttman <- function(md, archivo, formato,
                                     docx_data, verbose = TRUE) {
  archivos <- character(0)
  dir_salida <- dirname(archivo)
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

  base_md <- paste0(archivo, ".md")
  writeLines(md, base_md, useBytes = TRUE)
  if ("md" %in% formato) archivos <- c(archivos, base_md)

  if ("docx" %in% formato) {
    docx_ok <- FALSE
    if (requireNamespace("officer",   quietly = TRUE) &&
        requireNamespace("flextable", quietly = TRUE)) {
      ruta_docx <- tryCatch(
        .exportar_guttman_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("officer Guttman fallo: ", conditionMessage(e),
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
.exportar_guttman_docx_officer <- function(archivo, d, verbose) {

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
  L <- .labels_guttman(d$idioma %||% "es")

  border_outer <- fp_border(color = col_seccion, width = 1.0, style = "solid")
  border_inner <- fp_border(color = col_borde,   width = 0.5, style = "solid")
  border_sec   <- fp_border(color = col_seccion, width = 1.2, style = "solid")

  doc <- off("read_docx")()
  default_section <- off("prop_section")(
    page_size = off("page_size")(width = 8.27, height = 11.69, orient = "portrait"),
    page_margins = off("page_mar")(top = 0.79, bottom = 0.79,
                                    left = 0.79, right = 0.79,
                                    header = 0.5, footer = 0.5, gutter = 0),
    type = "continuous"
  )

  add_par <- function(doc, txt, italic = FALSE, bold = FALSE,
                      size = 11, align = "justify",
                      color = col_texto, pad_top = 4, pad_bot = 4,
                      border_bottom = NULL) {
    par_args <- list(text.align = align,
                     padding.top = pad_top, padding.bottom = pad_bot,
                     line_spacing = 1.15)
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
      d$autor, fp_text(font.size = 10, color = col_subtit, font.family = fuente))
  }
  if (!is.null(d$version) && nzchar(as.character(d$version))) {
    if (length(meta_chunks) > 0) {
      meta_chunks[[length(meta_chunks) + 1]] <- ftext(
        "  \u00B7  ", fp_text(font.size = 10, color = col_subtit, font.family = fuente))
    }
    et <- if (es_en) "Version " else "Versi\u00F3n "
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      paste0(et, d$version),
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

  # ---------------- Items (un bloque por item) ----------------
  doc <- add_heading(doc, L$items_encab)

  items        <- d$items
  alternativas <- d$alternativas

  for (i in seq_len(nrow(items))) {
    fac <- items$faceta[i]
    # Linea de item: numero + (faceta opcional) + stem (negrita)
    if (!is.na(fac) && nzchar(fac)) {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext(paste0(i, ". "), fp_text(font.size = 12, bold = TRUE,
                                        color = col_titulo, font.family = fuente)),
        ftext(paste0("(", L$faceta_txt, ": ", fac, ") "),
              fp_text(font.size = 10, italic = TRUE, color = col_subtit,
                      font.family = fuente)),
        ftext(items$stem[i], fp_text(font.size = 12, bold = TRUE,
                                       color = col_texto, font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.top = 8, padding.bottom = 4)
      ))
    } else {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext(paste0(i, ". "), fp_text(font.size = 12, bold = TRUE,
                                        color = col_titulo, font.family = fuente)),
        ftext(items$stem[i], fp_text(font.size = 12, bold = TRUE,
                                       color = col_texto, font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.top = 8, padding.bottom = 4)
      ))
    }

    # Subnota
    doc <- add_par(doc, L$seleccione, italic = TRUE, size = 9.5,
                    color = col_subtit, align = "left",
                    pad_top = 0, pad_bot = 3)

    # Alternativas: una por linea con (  ) al inicio
    alts_i <- alternativas[alternativas$n_item == i, ]
    for (k in seq_len(nrow(alts_i))) {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext("(  )  ", fp_text(font.size = 11, color = col_titulo,
                                 font.family = fuente, bold = TRUE)),
        ftext(alts_i$alternativa[k],
              fp_text(font.size = 11, color = col_texto, font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.left = 24,
                      padding.top = 2, padding.bottom = 2)
      ))
    }
    doc <- off("body_add_par")(doc, "")
  }

  # ---------------- Construct map opcional ----------------
  if (isTRUE(d$incluir_construct_map)) {
    doc <- off("body_add_break")(doc)
    doc <- add_heading(doc, L$cm_encab)
    cm <- d$construct_map
    df_cm <- data.frame(
      Nivel       = names(cm),
      Descripcion = unlist(cm),
      stringsAsFactors = FALSE
    )
    names(df_cm) <- c(L$nivel_txt, L$descripcion_txt)

    ft <- flx("flextable")(df_cm)
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    ft <- flx("bg")(ft,    bg = col_fondo_h, part = "header")
    ft <- flx("color")(ft, color = col_text_h, part = "header")
    ft <- flx("bold")(ft,  part = "header")
    ft <- flx("border_outer")(ft, border = border_outer, part = "all")
    ft <- flx("border_inner_h")(ft, border = border_inner, part = "all")
    ft <- flx("border_inner_v")(ft, border = border_inner, part = "all")
    ft <- flx("padding")(ft, padding.top = 4, padding.bottom = 4,
                          padding.left = 6, padding.right = 6)
    ft <- flx("align")(ft, j = 1, align = "center", part = "body")
    ft <- flx("align")(ft, j = 2, align = "left",   part = "body")
    ft <- flx("width")(ft, j = 1, width = 1.2)
    ft <- flx("width")(ft, j = 2, width = 5.5)
    n <- flx("nrow_part")(ft, part = "body")
    if (!is.null(n) && n >= 2) {
      ft <- flx("bg")(ft, i = seq(2, n, by = 2), bg = col_fondo_a, part = "body")
    }
    doc <- flx("body_add_flextable")(doc, ft, align = "center")
  }

  doc <- off("body_set_default_section")(doc, default_section)

  ruta_docx <- paste0(archivo, ".docx")
  print(doc, target = ruta_docx)
  if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  ruta_docx
}


#' @export
print.semilla_test_guttman <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test Guttman ensamblado (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Nombre  : ", x$nombre_test, "\n", sep = "")
  if (!is.null(x$subtitulo)) cat("  Subtit. : ", x$subtitulo, "\n", sep = "")
  cat("  N items : ", nrow(x$items), "\n", sep = "")
  cat("  Niveles : ", length(x$construct_map), "\n", sep = "")
  if (length(x$archivos) > 0) {
    cat("  Archivos:\n")
    for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}
