#' @title Ensamblar test Forced-Choice
#'
#' @description
#' Toma un objeto \code{semilla_forcedchoice} y produce los entregables:
#' \enumerate{
#'   \item DOCX en papel con bloques (formato MOST/LEAST u otra eleccion).
#'   \item Excel con \code{item_bank}, \code{bloques}, \code{design_matrix} y
#'         validacion de balance.
#' }
#'
#' Layout DOCX (default \code{"most_least"}):
#' \preformatted{
#'   Bloque 1 (de 15)
#'   Marque cual lo describe MEJOR (M) y cual PEOR (P):
#'      M  P
#'      ( ) ( )  (a) Item de la dimension X
#'      ( ) ( )  (b) Item de la dimension Y
#'      ( ) ( )  (c) Item de la dimension Z
#'      ( ) ( )  (d) Item de la dimension W
#' }
#'
#' @param escala_fc Objeto \code{semilla_forcedchoice}.
#' @param nombre_test Nombre del instrumento.
#' @param subtitulo Subtitulo opcional.
#' @param autor Autor.
#' @param version Version.
#' @param incluir_datos Incluir bloque demografico.
#' @param datos_solicitados Vector de campos demograficos.
#' @param instrucciones Texto custom; si NULL, autogenerado segun metodo.
#' @param archivo Ruta SIN extension.
#' @param formato Vector con: "md", "docx", "xlsx".
#' @param idioma "es" o "en".
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_test_forcedchoice}.
#'
#' @export
ensamblar_test_forcedchoice <- function(
  escala_fc,
  nombre_test       = NULL,
  subtitulo         = NULL,
  autor             = NULL,
  version           = "1.0",
  incluir_datos     = TRUE,
  datos_solicitados = c("codigo", "edad", "sexo", "nivel_educativo",
                         "fecha"),
  instrucciones     = NULL,
  archivo           = NULL,
  formato           = c("md", "docx", "xlsx"),
  idioma            = NULL,
  verbose           = TRUE
) {

  if (!inherits(escala_fc, "semilla_forcedchoice"))
    stop("'escala_fc' debe ser un objeto semilla_forcedchoice.")
  formato <- tolower(formato)
  if (is.null(idioma)) idioma <- escala_fc$idioma %||% "es"
  metodo <- escala_fc$config$metodo

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Escala Forced-Choice - ",
                            .capitalizar(substr(escala_fc$concepto, 1, 60)))
  }
  if (is.null(instrucciones)) {
    instrucciones <- .instrucciones_forcedchoice(metodo, idioma)
  }

  archivos <- character(0)

  # ---------- Excel ----------
  if ("xlsx" %in% formato && !is.null(archivo)) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      ruta_xlsx <- paste0(archivo, "_banco.xlsx")
      openxlsx::write.xlsx(
        list(
          item_bank          = escala_fc$item_bank,
          bloques            = escala_fc$bloques,
          design_matrix      = escala_fc$design_matrix,
          validacion_balance = escala_fc$validacion_balance,
          config = data.frame(
            campo = c("concepto", "dimensiones", "n_items_por_dimension",
                       "block_size", "n_bloques", "metodo",
                       "estimar_valencia", "balancear_valencia",
                       "tolerancia_valencia", "seed"),
            valor = c(escala_fc$concepto,
                       paste(escala_fc$dimensiones, collapse = ", "),
                       escala_fc$config$n_items_por_dimension,
                       escala_fc$config$block_size,
                       escala_fc$config$n_bloques,
                       escala_fc$config$metodo,
                       escala_fc$config$estimar_valencia,
                       escala_fc$config$balancear_valencia,
                       escala_fc$config$tolerancia_valencia,
                       escala_fc$metadata$seed),
            stringsAsFactors = FALSE
          )
        ),
        ruta_xlsx, overwrite = TRUE
      )
      archivos <- c(archivos, ruta_xlsx)
      if (verbose) cat("  - ", ruta_xlsx, "\n", sep = "")
    }
  }

  # ---------- DOCX/MD ----------
  if (any(c("md", "docx") %in% formato) && !is.null(archivo)) {
    md <- .construir_md_forcedchoice(
      escala_fc = escala_fc, nombre_test = nombre_test,
      subtitulo = subtitulo, autor = autor, version = version,
      incluir_datos = incluir_datos,
      datos_solicitados = datos_solicitados,
      instrucciones = instrucciones, idioma = idioma
    )
    docx_data <- list(
      escala_fc = escala_fc,
      nombre_test = nombre_test, subtitulo = subtitulo,
      autor = autor, version = version,
      datos_solicitados = if (incluir_datos) datos_solicitados else NULL,
      instrucciones = instrucciones,
      idioma = idioma
    )
    archivos_doc <- .exportar_forcedchoice(
      md = md, archivo = archivo, formato = formato,
      docx_data = docx_data, verbose = verbose
    )
    archivos <- c(archivos, archivos_doc)
  }

  resultado <- list(
    nombre_test = nombre_test,
    archivos    = archivos,
    metodo      = metodo
  )
  class(resultado) <- c("semilla_test_forcedchoice", "list")
  resultado
}


# =============================================================================
# Helpers
# =============================================================================

#' @keywords internal
.instrucciones_forcedchoice <- function(metodo, idioma) {
  if (idioma == "en") {
    switch(metodo,
      "most_least" = paste(
        "For each block of statements, select the one that BEST",
        "describes you (column M) and the one that WORST describes you",
        "(column W). Mark only ONE per column."
      ),
      "ranking" = paste(
        "For each block, RANK the statements from 1 (best describes you)",
        "to K (worst describes you). Use each rank only once."
      ),
      "single_choice" = paste(
        "For each block, select the ONE statement that BEST describes you."
      )
    )
  } else {
    switch(metodo,
      "most_least" = paste(
        "Para cada bloque de afirmaciones, seleccione la que MEJOR lo",
        "describe (columna M) y la que PEOR lo describe (columna P).",
        "Marque SOLO UNA por columna."
      ),
      "ranking" = paste(
        "Para cada bloque, ORDENE las afirmaciones de 1 (mejor lo describe)",
        "a K (peor lo describe). Use cada numero solo una vez."
      ),
      "single_choice" = paste(
        "Para cada bloque, seleccione la afirmacion que MEJOR lo describe."
      )
    )
  }
}


#' @keywords internal
.labels_forcedchoice <- function(idioma) {
  if (idioma == "en") {
    list(datos = "Participant information", instr = "Instructions",
         items = "Blocks", bloque = "Block", de = "of",
         most = "M", least = "W", rank = "Rank",
         best = "Best", worst = "Worst")
  } else {
    list(datos = "Datos del participante", instr = "Instrucciones",
         items = "Bloques", bloque = "Bloque", de = "de",
         most = "M", least = "P", rank = "Orden",
         best = "Mejor", worst = "Peor")
  }
}


#' @keywords internal
.construir_md_forcedchoice <- function(escala_fc, nombre_test, subtitulo,
                                         autor, version, incluir_datos,
                                         datos_solicitados, instrucciones,
                                         idioma) {

  L <- .labels_forcedchoice(idioma)
  metodo <- escala_fc$config$metodo
  out <- character(0)
  out <- c(out, paste0("# ", nombre_test))
  if (!is.null(subtitulo)) out <- c(out, paste0("### ", subtitulo))
  out <- c(out, "")
  meta <- character(0)
  if (!is.null(autor))   meta <- c(meta, paste0("**Autor:** ", autor))
  if (!is.null(version)) meta <- c(meta, paste0("**Versi\u00F3n:** ", version))
  if (length(meta) > 0)  out <- c(out, paste(meta, collapse = " \u00B7 "), "")
  out <- c(out, "---", "")

  if (incluir_datos) {
    out <- c(out, paste0("## ", L$datos), "",
              .bloque_datos_demograficos(datos_solicitados, idioma),
              "", "---", "")
  }

  out <- c(out, paste0("## ", L$instr), "", instrucciones, "", "---", "")

  out <- c(out, paste0("## ", L$items), "")

  bloques  <- escala_fc$bloques
  blocks_unique <- sort(unique(bloques$block_id))
  total_b <- length(blocks_unique)

  for (b in blocks_unique) {
    items_b <- bloques[bloques$block_id == b, ]
    out <- c(out, "",
              paste0("**", L$bloque, " ", b, " ", L$de, " ", total_b, "**"), "")

    if (metodo == "most_least") {
      out <- c(out,
        paste0("| ", L$most, " | ", L$least, " | Afirmacion |"),
        "|:-:|:-:|---|")
      for (k in seq_len(nrow(items_b))) {
        out <- c(out, paste0("| ( ) | ( ) | (",
                              letters[k], ") ", items_b$texto_item[k], " |"))
      }
    } else if (metodo == "ranking") {
      out <- c(out,
        paste0("| ", L$rank, " | Afirmacion |"),
        "|:-:|---|")
      for (k in seq_len(nrow(items_b))) {
        out <- c(out, paste0("| ___ | (",
                              letters[k], ") ", items_b$texto_item[k], " |"))
      }
    } else {  # single_choice
      for (k in seq_len(nrow(items_b))) {
        out <- c(out, paste0("- ( ) ", items_b$texto_item[k]))
      }
    }
    out <- c(out, "")
  }

  paste(out, collapse = "\n")
}


# =============================================================================
# Exportador
# =============================================================================

#' @keywords internal
.exportar_forcedchoice <- function(md, archivo, formato, docx_data,
                                     verbose = TRUE) {
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
        .exportar_forcedchoice_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("officer fc fallo: ",
                  conditionMessage(e), ". Usando rmarkdown."); NULL
        }
      )
      if (!is.null(ruta_docx) && file.exists(ruta_docx)) {
        archivos <- c(archivos, ruta_docx); docx_ok <- TRUE
      }
    }
    if (!docx_ok) {
      ruta_docx <- .exportar_via_rmarkdown(base_md, archivo, "docx", verbose)
      if (!is.null(ruta_docx)) archivos <- c(archivos, ruta_docx)
    }
  }

  if (!("md" %in% formato)) try(file.remove(base_md), silent = TRUE)
  archivos
}


#' @keywords internal
.exportar_forcedchoice_docx_officer <- function(archivo, d, verbose) {

  off <- function(name) get(name, envir = asNamespace("officer"))
  flx <- function(name) get(name, envir = asNamespace("flextable"))

  fp_par <- off("fp_par"); fp_text <- off("fp_text")
  fp_border <- off("fp_border"); ftext <- off("ftext"); fpar <- off("fpar")

  col_titulo  <- "#1F3864"
  col_subtit  <- "#595959"
  col_seccion <- "#1F3864"
  col_borde   <- "#BFBFBF"
  col_fondo_h <- "#1F3864"
  col_text_h  <- "#FFFFFF"
  col_fondo_a <- "#EAEEF5"
  col_texto   <- "#262626"
  fuente <- "Calibri"
  L <- .labels_forcedchoice(d$idioma %||% "es")
  metodo <- d$escala_fc$config$metodo

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

  # Encabezado
  doc <- add_par(doc, d$nombre_test, bold = TRUE, size = 18,
                  color = col_titulo, align = "center",
                  pad_top = 0, pad_bot = 4)
  if (!is.null(d$subtitulo)) {
    doc <- add_par(doc, d$subtitulo, italic = TRUE, size = 12,
                    color = col_subtit, align = "center",
                    pad_top = 0, pad_bot = 4)
  }
  meta_chunks <- list()
  if (!is.null(d$autor)) meta_chunks[[1]] <- ftext(
    d$autor, fp_text(font.size = 10, color = col_subtit, font.family = fuente))
  if (!is.null(d$version) && nzchar(as.character(d$version))) {
    if (length(meta_chunks) > 0) meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      "  \u00B7  ", fp_text(font.size = 10, color = col_subtit, font.family = fuente))
    meta_chunks[[length(meta_chunks) + 1]] <- ftext(
      paste0("Versi\u00F3n ", d$version),
      fp_text(font.size = 10, color = col_subtit, font.family = fuente))
  }
  if (length(meta_chunks) > 0) {
    doc <- off("body_add_fpar")(doc, do.call(fpar, c(
      meta_chunks,
      list(fp_p = fp_par(text.align = "center",
                         padding.top = 0, padding.bottom = 14)))))
  }

  # Datos demograficos
  if (!is.null(d$datos_solicitados) && length(d$datos_solicitados) > 0) {
    doc <- add_heading(doc, L$datos)
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

  # Instrucciones
  doc <- add_heading(doc, L$instr)
  doc <- add_par(doc, d$instrucciones, align = "justify",
                  pad_top = 2, pad_bot = 6)

  # Bloques
  doc <- add_heading(doc, L$items)
  bloques <- d$escala_fc$bloques
  blocks_unique <- sort(unique(bloques$block_id))
  total_b <- length(blocks_unique)

  for (b in blocks_unique) {
    items_b <- bloques[bloques$block_id == b, ]

    # Sub-encabezado
    doc <- add_par(doc, paste0(L$bloque, " ", b, " ", L$de, " ", total_b),
                    bold = TRUE, size = 12, color = col_titulo,
                    align = "left", pad_top = 8, pad_bot = 4)

    if (metodo == "most_least") {
      df_b <- data.frame(
        M = rep("(  )", nrow(items_b)),
        P = rep("(  )", nrow(items_b)),
        Afirmacion = paste0("(", letters[seq_len(nrow(items_b))], ") ",
                              items_b$texto_item),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      names(df_b) <- c(L$most, L$least,
                        if (d$idioma == "en") "Statement" else "Afirmaci\u00F3n")
      ft <- flx("flextable")(df_b)
      ft <- flx("fontsize")(ft, size = 11, part = "all")
      ft <- flx("color")(ft, color = col_texto, part = "body")
      ft <- flx("align")(ft, align = "center", part = "header")
      ft <- flx("align")(ft, j = 1:2, align = "center", part = "body")
      ft <- flx("align")(ft, j = 3, align = "left", part = "body")
      ft <- flx("width")(ft, j = 1, width = 0.5)
      ft <- flx("width")(ft, j = 2, width = 0.5)
      ft <- flx("width")(ft, j = 3, width = 5.7)
      ft <- estilizar_tabla(ft)
      doc <- flx("body_add_flextable")(doc, ft, align = "center")
    } else if (metodo == "ranking") {
      df_b <- data.frame(
        Orden = rep("____", nrow(items_b)),
        Afirmacion = paste0("(", letters[seq_len(nrow(items_b))], ") ",
                              items_b$texto_item),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      names(df_b) <- c(L$rank,
                        if (d$idioma == "en") "Statement" else "Afirmaci\u00F3n")
      ft <- flx("flextable")(df_b)
      ft <- flx("fontsize")(ft, size = 11, part = "all")
      ft <- flx("align")(ft, j = 1, align = "center", part = "all")
      ft <- flx("align")(ft, j = 2, align = "left", part = "body")
      ft <- flx("width")(ft, j = 1, width = 0.7)
      ft <- flx("width")(ft, j = 2, width = 6.0)
      ft <- estilizar_tabla(ft)
      doc <- flx("body_add_flextable")(doc, ft, align = "center")
    } else {  # single_choice
      for (k in seq_len(nrow(items_b))) {
        doc <- off("body_add_fpar")(doc, fpar(
          ftext("(  )  ", fp_text(font.size = 11, color = col_titulo,
                                    font.family = fuente, bold = TRUE)),
          ftext(items_b$texto_item[k],
                fp_text(font.size = 11, color = col_texto,
                        font.family = fuente)),
          fp_p = fp_par(text.align = "left", padding.left = 24,
                        padding.top = 2, padding.bottom = 2)
        ))
      }
    }
    doc <- off("body_add_par")(doc, "")
  }

  doc <- off("body_set_default_section")(doc, default_section)

  ruta_docx <- paste0(archivo, ".docx")
  print(doc, target = ruta_docx)
  if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  ruta_docx
}


#' @export
print.semilla_test_forcedchoice <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test Forced-Choice ensamblado (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Nombre : ", x$nombre_test, "\n", sep = "")
  cat("  Metodo : ", x$metodo, "\n", sep = "")
  cat("  Archivos:\n")
  for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  cat("===========================================================\n\n")
  invisible(x)
}
