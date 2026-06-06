#' @title Ensamblar prueba objetiva
#'
#' @description
#' Toma un objeto \code{semilla_prueba_objetiva} y produce dos archivos:
#' \enumerate{
#'   \item Version \strong{aplicable}: cuestionario con enunciados +
#'         opciones, sin marcar la respuesta correcta.
#'   \item Version \strong{con clave}: anexo con respuestas correctas,
#'         tema y nivel Bloom por item (uso interno del docente/investigador).
#' }
#'
#' @param escala_o Objeto \code{semilla_prueba_objetiva}.
#' @param nombre_test Nombre de la prueba.
#' @param subtitulo Subtitulo opcional.
#' @param autor Autor.
#' @param version Version.
#' @param incluir_datos Incluir bloque demografico.
#' @param datos_solicitados Vector de campos demograficos.
#' @param instrucciones Texto custom; si NULL, autogenerado.
#' @param archivo Ruta SIN extension. Se generaran:
#'   \code{<archivo>_aplicable.docx} y \code{<archivo>_clave.docx}.
#' @param formato Vector con: "md", "docx".
#' @param idioma "es" o "en".
#' @param verbose Mostrar progreso.
#'
#' @return Lista con dos elementos: \code{aplicable} y \code{clave}, cada
#'   uno de clase \code{semilla_test_objetivo}.
#'
#' @export
ensamblar_prueba_objetiva <- function(
  escala_o,
  nombre_test       = NULL,
  subtitulo         = NULL,
  autor             = NULL,
  version           = "1.0",
  incluir_datos     = TRUE,
  datos_solicitados = c("codigo", "edad", "sexo", "nivel_educativo",
                         "fecha"),
  instrucciones     = NULL,
  archivo           = NULL,
  formato           = c("md", "docx"),
  idioma            = NULL,
  verbose           = TRUE
) {

  if (!inherits(escala_o, "semilla_prueba_objetiva"))
    stop("'escala_o' debe ser un objeto semilla_prueba_objetiva.")
  formato <- tolower(formato)
  if (is.null(idioma)) idioma <- escala_o$idioma %||% "es"

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Prueba objetiva - ",
                           .capitalizar(substr(escala_o$dominio, 1, 60)))
  }
  if (is.null(instrucciones)) {
    instrucciones <- .instrucciones_objetivas(idioma)
  }

  # ---------- Version aplicable ----------
  arch_app <- if (!is.null(archivo)) paste0(archivo, "_aplicable") else NULL
  md_app <- .construir_md_objetivas(
    escala_o = escala_o,
    nombre_test = nombre_test,
    subtitulo = subtitulo, autor = autor, version = version,
    incluir_datos = incluir_datos,
    datos_solicitados = datos_solicitados,
    instrucciones = instrucciones,
    incluir_clave = FALSE,
    idioma = idioma
  )
  archivos_app <- character(0)
  if (!is.null(arch_app)) {
    archivos_app <- .exportar_objetivas(
      md = md_app, archivo = arch_app, formato = formato,
      docx_data = list(
        nombre_test = nombre_test, subtitulo = subtitulo,
        autor = autor, version = version,
        datos_solicitados = if (incluir_datos) datos_solicitados else NULL,
        instrucciones = instrucciones,
        escala_o = escala_o, incluir_clave = FALSE,
        idioma = idioma
      ),
      verbose = verbose
    )
  }

  # ---------- Version con clave ----------
  sub_clave <- if (is.null(subtitulo)) "Versi\u00F3n con clave de respuestas" else
    paste0(subtitulo, " \u2014 Versi\u00F3n con clave")
  arch_clv <- if (!is.null(archivo)) paste0(archivo, "_clave") else NULL
  md_clv <- .construir_md_objetivas(
    escala_o = escala_o,
    nombre_test = nombre_test,
    subtitulo = sub_clave, autor = autor, version = version,
    incluir_datos = FALSE,
    datos_solicitados = datos_solicitados,
    instrucciones = instrucciones,
    incluir_clave = TRUE,
    idioma = idioma
  )
  archivos_clv <- character(0)
  if (!is.null(arch_clv)) {
    archivos_clv <- .exportar_objetivas(
      md = md_clv, archivo = arch_clv, formato = formato,
      docx_data = list(
        nombre_test = nombre_test, subtitulo = sub_clave,
        autor = autor, version = version,
        datos_solicitados = NULL,
        instrucciones = instrucciones,
        escala_o = escala_o, incluir_clave = TRUE,
        idioma = idioma
      ),
      verbose = verbose
    )
  }

  resultado <- list(
    aplicable = structure(
      list(nombre_test = nombre_test, texto_md = md_app, archivos = archivos_app),
      class = c("semilla_test_objetivo", "list")),
    clave     = structure(
      list(nombre_test = nombre_test, texto_md = md_clv, archivos = archivos_clv),
      class = c("semilla_test_objetivo", "list"))
  )
  class(resultado) <- c("semilla_test_objetivo_multi", "list")
  resultado
}


# =============================================================================
# Helpers
# =============================================================================

#' @keywords internal
.instrucciones_objetivas <- function(idioma) {
  if (idioma == "en") {
    paste(
      "Below you will find a series of multiple-choice items. For each one,",
      "read the stem and the response options carefully, then mark the",
      "**ONE** option you consider correct. There is only one correct",
      "answer per item."
    )
  } else {
    paste(
      "A continuaci\u00F3n encontrar\u00E1 una serie de \u00EDtems de opci\u00F3n m\u00FAltiple.",
      "Para cada uno, lea con atenci\u00F3n el enunciado y las opciones de",
      "respuesta, y marque la **\u00DANICA** opci\u00F3n que considere correcta.",
      "Cada \u00EDtem tiene una sola respuesta correcta."
    )
  }
}


#' @keywords internal
.labels_objetivas <- function(idioma) {
  if (idioma == "en") {
    list(
      datos_encab = "Participant information",
      instr_encab = "Instructions",
      items_encab = "Items",
      clave_encab = "Answer key",
      nro_txt = "Item",
      tema_txt = "Topic",
      bloom_txt = "Bloom",
      formato_txt = "Format",
      respuesta_txt = "Correct"
    )
  } else {
    list(
      datos_encab = "Datos del participante",
      instr_encab = "Instrucciones",
      items_encab = "\u00CDtems",
      clave_encab = "Clave de respuestas",
      nro_txt = "\u00CDtem",
      tema_txt = "Tema",
      bloom_txt = "Bloom",
      formato_txt = "Formato",
      respuesta_txt = "Correcta"
    )
  }
}


#' @keywords internal
.construir_md_objetivas <- function(escala_o, nombre_test, subtitulo,
                                     autor, version, incluir_datos,
                                     datos_solicitados, instrucciones,
                                     incluir_clave, idioma) {
  L <- .labels_objetivas(idioma)
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
    out <- c(out, paste0("## ", L$datos_encab), "",
              .bloque_datos_demograficos(datos_solicitados, idioma),
              "", "---", "")
  }

  out <- c(out, paste0("## ", L$instr_encab), "", instrucciones, "", "---", "")

  out <- c(out, paste0("## ", L$items_encab), "")

  items     <- escala_o$items
  opciones  <- escala_o$opciones
  empar     <- escala_o$emparejamientos
  contextos <- escala_o$contextos

  for (i in seq_len(nrow(items))) {
    n <- items$n_item[i]
    fmt <- items$formato[i]
    bloque <- character(0)

    encab <- paste0("**", n, ".** ", items$enunciado[i])
    bloque <- c(bloque, encab, "")

    # Contexto-dependiente: mostrar texto base
    if (fmt == "contexto_dependiente" && nrow(contextos) > 0) {
      ctx_i <- contextos[contextos$n_item == n, ]
      if (nrow(ctx_i) > 0) {
        bloque <- c(bloque, "> ", paste0("> ", ctx_i$contexto[1]), "", "")
      }
    }

    if (!is.na(items$instruccion_extra[i]) &&
        nzchar(items$instruccion_extra[i])) {
      bloque <- c(bloque, paste0("*", items$instruccion_extra[i], "*"), "")
    }

    if (fmt == "emparejamiento" && nrow(empar) > 0) {
      e_i <- empar[empar$n_item == n, ]
      if (nrow(e_i) > 0) {
        # Desordenar respuestas para versión aplicable; en clave mostrar correspondencia
        if (!incluir_clave) {
          set.seed(n)
          orden <- sample(seq_len(nrow(e_i)))
          respuestas_vis <- e_i$respuesta[orden]
        } else {
          respuestas_vis <- e_i$respuesta
        }
        bloque <- c(bloque, "| Premisas | Respuestas |", "|---|---|")
        nrow_max <- max(nrow(e_i), length(respuestas_vis))
        for (k in seq_len(nrow_max)) {
          prem <- if (k <= nrow(e_i)) paste0(k, ". ", e_i$premisa[k]) else ""
          resp <- if (k <= length(respuestas_vis))
                    paste0(letters[k], ") ", respuestas_vis[k]) else ""
          bloque <- c(bloque, paste0("| ", prem, " | ", resp, " |"))
        }
        if (incluir_clave) {
          bloque <- c(bloque, "",
                       paste0("*Clave:* ",
                              paste(seq_len(nrow(e_i)),
                                     "\u2194",
                                     letters[seq_len(nrow(e_i))],
                                     collapse = "  \u00B7  ")))
        }
      }
    } else {
      # Formatos con opciones lineales
      ops_i <- opciones[opciones$n_item == n, ]
      for (k in seq_len(nrow(ops_i))) {
        marca <- if (incluir_clave && ops_i$es_correcta[k]) " **\u2713**" else ""
        bloque <- c(bloque, paste0("- ( ) ", ops_i$texto_opcion[k], marca))
      }
    }

    if (incluir_clave) {
      bloque <- c(bloque, "",
                   paste0("*", L$tema_txt, ": ", items$tema[i],
                          " \u00B7 ", L$bloom_txt, ": ", items$nivel_bloom[i],
                          " \u00B7 ", L$formato_txt, ": ", fmt, "*"))
    }
    bloque <- c(bloque, "")
    out <- c(out, bloque)
  }

  paste(out, collapse = "\n")
}


# =============================================================================
# Exportador (md + docx via officer)
# =============================================================================

#' @keywords internal
.exportar_objetivas <- function(md, archivo, formato,
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
        .exportar_objetivas_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("officer objetivas fallo: ", conditionMessage(e),
                  ". Usando rmarkdown."); NULL
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

  formatos_rmd <- intersect(formato, c("html", "pdf"))
  for (f in formatos_rmd) {
    ruta_out <- .exportar_via_rmarkdown(base_md, archivo, f, verbose)
    if (!is.null(ruta_out)) archivos <- c(archivos, ruta_out)
  }
  if (!("md" %in% formato)) try(file.remove(base_md), silent = TRUE)
  archivos
}


#' @keywords internal
.exportar_objetivas_docx_officer <- function(archivo, d, verbose) {

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
  col_correcta <- "#1E8449"  # verde para marcar respuesta correcta
  col_ctx_bg  <- "#F2F4F8"
  fuente <- "Calibri"
  es_en  <- identical(d$idioma, "en")
  L <- .labels_objetivas(d$idioma %||% "es")

  border_outer <- fp_border(color = col_seccion, width = 1.0, style = "solid")
  border_inner <- fp_border(color = col_borde,   width = 0.5, style = "solid")
  border_sec   <- fp_border(color = col_seccion, width = 1.2, style = "solid")
  border_ctx   <- fp_border(color = col_titulo,  width = 2.0, style = "solid")

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
                      pad_left = 0, border_bottom = NULL) {
    par_args <- list(text.align = align,
                     padding.top = pad_top, padding.bottom = pad_bot,
                     padding.left = pad_left,
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

  # Datos demograficos
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

  # Instrucciones
  doc <- add_heading(doc, L$instr_encab)
  doc <- add_par(doc, d$instrucciones, align = "justify",
                  pad_top = 2, pad_bot = 6)

  # Items
  doc <- add_heading(doc, L$items_encab)

  escala_o <- d$escala_o
  items     <- escala_o$items
  opciones  <- escala_o$opciones
  empar     <- escala_o$emparejamientos
  contextos <- escala_o$contextos

  for (i in seq_len(nrow(items))) {
    n <- items$n_item[i]
    fmt <- items$formato[i]

    # Encabezado item: numero + (formato/bloom opcional en clave) + enunciado
    if (isTRUE(d$incluir_clave)) {
      meta_item <- paste0(" [", items$tema[i], " | ",
                            items$nivel_bloom[i], " | ", fmt, "]")
      doc <- off("body_add_fpar")(doc, fpar(
        ftext(paste0(n, ". "),
              fp_text(font.size = 12, bold = TRUE, color = col_titulo,
                      font.family = fuente)),
        ftext(items$enunciado[i],
              fp_text(font.size = 12, bold = TRUE, color = col_texto,
                      font.family = fuente)),
        ftext(meta_item,
              fp_text(font.size = 9, italic = TRUE, color = col_subtit,
                      font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.top = 8, padding.bottom = 4)
      ))
    } else {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext(paste0(n, ". "),
              fp_text(font.size = 12, bold = TRUE, color = col_titulo,
                      font.family = fuente)),
        ftext(items$enunciado[i],
              fp_text(font.size = 12, bold = TRUE, color = col_texto,
                      font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.top = 8, padding.bottom = 4)
      ))
    }

    # Contexto (formato contexto_dependiente)
    if (fmt == "contexto_dependiente" && nrow(contextos) > 0) {
      ctx_i <- contextos[contextos$n_item == n, ]
      if (nrow(ctx_i) > 0) {
        doc <- off("body_add_fpar")(doc, fpar(
          ftext(ctx_i$contexto[1],
                fp_text(font.size = 10, italic = TRUE, color = col_texto,
                        font.family = fuente)),
          fp_p = fp_par(text.align = "justify",
                        padding.left = 18, padding.right = 18,
                        padding.top = 6, padding.bottom = 8,
                        border.left = border_ctx,
                        shading.color = col_ctx_bg)
        ))
      }
    }

    # Instruccion extra (V/F multiple)
    if (!is.na(items$instruccion_extra[i]) &&
        nzchar(items$instruccion_extra[i])) {
      doc <- add_par(doc, items$instruccion_extra[i], italic = TRUE,
                      size = 9.5, color = col_subtit, align = "left",
                      pad_top = 0, pad_bot = 3)
    }

    if (fmt == "emparejamiento") {
      e_i <- empar[empar$n_item == n, ]
      if (nrow(e_i) > 0) {
        if (!isTRUE(d$incluir_clave)) {
          set.seed(n)
          orden <- sample(seq_len(nrow(e_i)))
          respuestas_vis <- e_i$respuesta[orden]
        } else {
          respuestas_vis <- e_i$respuesta
        }
        df_e <- data.frame(
          Premisas   = paste0(seq_len(nrow(e_i)), ". ", e_i$premisa),
          Respuestas = paste0(letters[seq_along(respuestas_vis)], ") ",
                                respuestas_vis),
          stringsAsFactors = FALSE
        )
        names(df_e) <- if (es_en) c("Premises", "Responses") else
                                   c("Premisas", "Respuestas")
        ft <- flx("flextable")(df_e)
        ft <- flx("font")(ft, fontname = fuente, part = "all")
        ft <- flx("bg")(ft, bg = col_fondo_h, part = "header")
        ft <- flx("color")(ft, color = col_text_h, part = "header")
        ft <- flx("bold")(ft, part = "header")
        ft <- flx("border_outer")(ft, border = border_outer, part = "all")
        ft <- flx("border_inner_h")(ft, border = border_inner, part = "all")
        ft <- flx("border_inner_v")(ft, border = border_inner, part = "all")
        ft <- flx("padding")(ft, padding.top = 4, padding.bottom = 4,
                              padding.left = 6, padding.right = 6)
        ft <- flx("fontsize")(ft, size = 10, part = "all")
        ft <- flx("width")(ft, j = 1, width = 3.2)
        ft <- flx("width")(ft, j = 2, width = 3.5)
        doc <- flx("body_add_flextable")(doc, ft, align = "center")

        if (isTRUE(d$incluir_clave)) {
          clave_txt <- paste("Clave:",
            paste(seq_len(nrow(e_i)), "\u2194", letters[seq_len(nrow(e_i))],
                  collapse = "  \u00B7  "))
          doc <- add_par(doc, clave_txt, italic = TRUE, size = 10,
                          color = col_correcta, align = "left",
                          pad_top = 4, pad_bot = 4)
        }
      }
    } else {
      ops_i <- opciones[opciones$n_item == n, ]
      for (k in seq_len(nrow(ops_i))) {
        if (isTRUE(d$incluir_clave) && ops_i$es_correcta[k]) {
          # Resaltar correcta en verde con check
          doc <- off("body_add_fpar")(doc, fpar(
            ftext("(\u2713)  ", fp_text(font.size = 11, color = col_correcta,
                                     font.family = fuente, bold = TRUE)),
            ftext(ops_i$texto_opcion[k],
                  fp_text(font.size = 11, color = col_correcta,
                          font.family = fuente, bold = TRUE)),
            fp_p = fp_par(text.align = "left", padding.left = 24,
                          padding.top = 2, padding.bottom = 2)
          ))
        } else {
          doc <- off("body_add_fpar")(doc, fpar(
            ftext("(  )  ", fp_text(font.size = 11, color = col_titulo,
                                     font.family = fuente, bold = TRUE)),
            ftext(ops_i$texto_opcion[k],
                  fp_text(font.size = 11, color = col_texto,
                          font.family = fuente)),
            fp_p = fp_par(text.align = "left", padding.left = 24,
                          padding.top = 2, padding.bottom = 2)
          ))
        }
      }
    }
    doc <- off("body_add_par")(doc, "")
  }

  # Anexo: clave compacta al final si version_clave
  if (isTRUE(d$incluir_clave)) {
    doc <- off("body_add_break")(doc)
    doc <- add_heading(doc, L$clave_encab)
    df_clave <- .construir_tabla_clave(escala_o, idioma = d$idioma %||% "es")
    ft <- flx("flextable")(df_clave)
    ft <- flx("font")(ft, fontname = fuente, part = "all")
    ft <- flx("bg")(ft, bg = col_fondo_h, part = "header")
    ft <- flx("color")(ft, color = col_text_h, part = "header")
    ft <- flx("bold")(ft, part = "header")
    ft <- flx("border_outer")(ft, border = border_outer, part = "all")
    ft <- flx("border_inner_h")(ft, border = border_inner, part = "all")
    ft <- flx("border_inner_v")(ft, border = border_inner, part = "all")
    ft <- flx("padding")(ft, padding.top = 4, padding.bottom = 4,
                          padding.left = 6, padding.right = 6)
    ft <- flx("fontsize")(ft, size = 10, part = "all")
    n_b <- flx("nrow_part")(ft, part = "body")
    if (!is.null(n_b) && n_b >= 2) {
      ft <- flx("bg")(ft, i = seq(2, n_b, by = 2),
                       bg = col_fondo_a, part = "body")
    }
    doc <- flx("body_add_flextable")(doc, ft, align = "center")
  }

  doc <- off("body_set_default_section")(doc, default_section)

  ruta_docx <- paste0(archivo, ".docx")
  print(doc, target = ruta_docx)
  if (verbose) cat("  - ", ruta_docx, " (officer)\n", sep = "")
  ruta_docx
}


#' @keywords internal
.construir_tabla_clave <- function(escala_o, idioma) {
  L <- .labels_objetivas(idioma)
  items <- escala_o$items
  opciones <- escala_o$opciones
  empar    <- escala_o$emparejamientos

  resp <- character(nrow(items))
  for (i in seq_len(nrow(items))) {
    n <- items$n_item[i]
    fmt <- items$formato[i]
    if (fmt == "emparejamiento") {
      e_i <- empar[empar$n_item == n, ]
      if (nrow(e_i) > 0) {
        resp[i] <- paste(seq_len(nrow(e_i)), "->",
                          letters[seq_len(nrow(e_i))],
                          collapse = " \u00B7 ")
      } else {
        resp[i] <- "-"
      }
    } else if (fmt == "vf_multiple") {
      ops_i <- opciones[opciones$n_item == n, ]
      vf <- ifelse(ops_i$es_correcta, "V", "F")
      resp[i] <- paste(letters[seq_len(nrow(ops_i))], "=", vf,
                        collapse = " \u00B7 ")
    } else {
      ops_i <- opciones[opciones$n_item == n, ]
      idx <- which(ops_i$es_correcta)[1]
      resp[i] <- if (length(idx) > 0 && !is.na(idx)) letters[idx] else "-"
    }
  }
  data.frame(
    `#`             = items$n_item,
    Tema            = items$tema,
    Bloom           = items$nivel_bloom,
    Formato         = items$formato,
    `Respuesta`     = resp,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' @export
print.semilla_test_objetivo_multi <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Prueba objetiva ensamblada (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  APLICABLE:\n")
  for (a in x$aplicable$archivos) cat("    - ", a, "\n", sep = "")
  cat("\n  CON CLAVE:\n")
  for (a in x$clave$archivos) cat("    - ", a, "\n", sep = "")
  cat("===========================================================\n\n")
  invisible(x)
}

#' @export
print.semilla_test_objetivo <- function(x, ...) {
  cat("Test objetivo: ", x$nombre_test, "\n", sep = "")
  for (a in x$archivos) cat("  - ", a, "\n", sep = "")
  invisible(x)
}
