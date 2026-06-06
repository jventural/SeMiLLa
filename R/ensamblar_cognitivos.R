#' @title Ensamblar test cognitivo procedural
#'
#' @description
#' Toma un objeto \code{semilla_test_cognitivo} y produce los entregables:
#' \enumerate{
#'   \item DOCX en papel (versiones \strong{aplicable} y \strong{con clave}).
#'   \item Un script R con plantilla Shiny lista para deployar (opcional).
#'   \item Un Excel con el banco de trials, validacion procedural y tabla
#'         de clustering.
#' }
#'
#' @param escala_c Objeto \code{semilla_test_cognitivo}.
#' @param nombre_test Nombre del instrumento.
#' @param subtitulo Subtitulo opcional.
#' @param autor Autor.
#' @param version Version.
#' @param archivo Ruta SIN extension.
#' @param formato Vector con: "md", "docx", "xlsx", "shiny".
#' @param idioma "es" o "en".
#' @param verbose Mostrar progreso.
#'
#' @return Lista con clase \code{semilla_test_cognitivo_multi}.
#'
#' @export
ensamblar_test_cognitivo <- function(
  escala_c,
  nombre_test = NULL,
  subtitulo   = NULL,
  autor       = NULL,
  version     = "1.0",
  archivo     = NULL,
  formato     = c("md", "docx", "xlsx"),
  idioma      = NULL,
  verbose     = TRUE
) {

  if (!inherits(escala_c, "semilla_test_cognitivo"))
    stop("'escala_c' debe ser un objeto semilla_test_cognitivo.")
  formato <- tolower(formato)
  if (is.null(idioma)) idioma <- escala_c$idioma %||% "es"

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Test cognitivo procedural - ",
                            toupper(escala_c$paradigma))
  }

  archivos_generados <- character(0)

  # ---------- Excel con banco de trials ----------
  if ("xlsx" %in% formato && !is.null(archivo)) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      ruta_xlsx <- paste0(archivo, "_banco.xlsx")
      openxlsx::write.xlsx(
        list(
          trials               = escala_c$trials,
          trials_resumen       = escala_c$trials_resumen,
          validacion_procedural = escala_c$validacion_procedural,
          cluster_summary      = if (!is.null(escala_c$cluster_summary))
                                    cbind(nivel_teorico = rownames(escala_c$cluster_summary),
                                          escala_c$cluster_summary)
                                  else data.frame(),
          config = data.frame(
            campo = c("paradigma", "niveles_dificultad", "n_trials_por_nivel",
                       "complejidad_operacion", "estimulo_memoria",
                       "refinar_estimulos", "cluster_dificultad", "seed"),
            valor = c(escala_c$paradigma,
                       paste(escala_c$niveles_dificultad, collapse = ", "),
                       escala_c$n_trials_por_nivel,
                       escala_c$config$complejidad_operacion,
                       escala_c$config$estimulo_memoria,
                       escala_c$config$refinar_estimulos,
                       escala_c$config$cluster_dificultad,
                       escala_c$metadata$seed),
            stringsAsFactors = FALSE
          )
        ),
        ruta_xlsx, overwrite = TRUE
      )
      archivos_generados <- c(archivos_generados, ruta_xlsx)
      if (verbose) cat("  - ", ruta_xlsx, "\n", sep = "")
    }
  }

  # ---------- DOCX papel: aplicable + clave ----------
  if (any(c("md", "docx") %in% formato) && !is.null(archivo)) {
    md_app <- .construir_md_cognitivo(escala_c, nombre_test, subtitulo,
                                       autor, version, idioma,
                                       incluir_clave = FALSE)
    md_clv <- .construir_md_cognitivo(escala_c, nombre_test,
                                       subtitulo %||% "Versi\u00F3n con clave",
                                       autor, version, idioma,
                                       incluir_clave = TRUE)

    archivos_app <- .exportar_cognitivo(
      md = md_app, archivo = paste0(archivo, "_aplicable"),
      formato = formato,
      docx_data = list(escala_c = escala_c, nombre_test = nombre_test,
                        subtitulo = subtitulo, autor = autor,
                        version = version, incluir_clave = FALSE,
                        idioma = idioma),
      verbose = verbose
    )
    archivos_clv <- .exportar_cognitivo(
      md = md_clv, archivo = paste0(archivo, "_clave"),
      formato = formato,
      docx_data = list(escala_c = escala_c, nombre_test = nombre_test,
                        subtitulo = subtitulo %||% "Versi\u00F3n con clave",
                        autor = autor, version = version,
                        incluir_clave = TRUE, idioma = idioma),
      verbose = verbose
    )
    archivos_generados <- c(archivos_generados, archivos_app, archivos_clv)
  }

  # ---------- Plantilla Shiny ----------
  if ("shiny" %in% formato && !is.null(archivo)) {
    ruta_shiny <- paste0(archivo, "_app.R")
    .generar_plantilla_shiny_ospan(escala_c, ruta_shiny)
    archivos_generados <- c(archivos_generados, ruta_shiny)
    if (verbose) cat("  - ", ruta_shiny, " (plantilla Shiny)\n", sep = "")
  }

  resultado <- list(
    nombre_test = nombre_test,
    archivos    = archivos_generados,
    paradigma   = escala_c$paradigma
  )
  class(resultado) <- c("semilla_test_cognitivo_multi", "list")
  resultado
}


# =============================================================================
# Markdown builder
# =============================================================================

#' @keywords internal
.construir_md_cognitivo <- function(escala_c, nombre_test, subtitulo,
                                      autor, version, idioma, incluir_clave) {

  L <- if (idioma == "en") list(
    instr = "Instructions",
    items = "Trials",
    clave = "Answer key",
    op = "Operation",
    correcta = "Correct",
    estimulo = "Memorize",
    secuencia = "Sequence to recall"
  ) else list(
    instr = "Instrucciones",
    items = "Trials",
    clave = "Clave de respuestas",
    op    = "Operaci\u00F3n",
    correcta = "Correcta",
    estimulo = "Memorizar",
    secuencia = "Secuencia a recordar"
  )

  out <- character(0)
  out <- c(out, paste0("# ", nombre_test))
  if (!is.null(subtitulo)) out <- c(out, paste0("### ", subtitulo))
  out <- c(out, "")
  meta <- character(0)
  if (!is.null(autor))   meta <- c(meta, paste0("**Autor:** ", autor))
  if (!is.null(version)) meta <- c(meta, paste0("**Versi\u00F3n:** ", version))
  if (length(meta) > 0)  out <- c(out, paste(meta, collapse = " \u00B7 "), "")
  out <- c(out, "---", "")

  # Instrucciones (paradigma OSPAN)
  if (idioma == "en") {
    instr_txt <- paste(
      "For each trial, you will see a sequence of pairs (operation + symbol).",
      "For each operation, decide if it is **TRUE** or **FALSE**, then read",
      "the symbol that follows. At the end of the trial you will be asked to",
      "**recall** the symbols **in the order they were presented**."
    )
  } else {
    instr_txt <- paste(
      "Para cada trial, ver\u00E1 una secuencia de pares (operaci\u00F3n + s\u00EDmbolo).",
      "Para cada operaci\u00F3n, decida si es **VERDADERA (V)** o **FALSA (F)** y",
      "luego lea el s\u00EDmbolo que sigue. Al final del trial se le pedir\u00E1 que",
      "**recuerde los s\u00EDmbolos** en el **orden en que fueron presentados**."
    )
  }
  out <- c(out, paste0("## ", L$instr), "", instr_txt, "", "---", "")

  # Trials
  out <- c(out, paste0("## ", L$items), "")
  trials  <- escala_c$trials
  resumen <- escala_c$trials_resumen

  for (i in seq_len(nrow(resumen))) {
    n <- resumen$n_trial[i]
    set_size <- resumen$set_size[i]
    nivel <- resumen$nivel_dificultad[i]

    out <- c(out, paste0("### Trial ", n, " \u00B7 Nivel ", nivel,
                          " (set size = ", set_size, ")"), "")

    t_i <- trials[trials$n_trial == n, ]
    for (k in seq_len(nrow(t_i))) {
      vf_label <- if (incluir_clave) {
        if (t_i$es_verdadera[k]) " *(V)*" else " *(F)*"
      } else " ( V / F )"
      out <- c(out,
        paste0("**Op ", k, ".** ", t_i$operacion_str[k],
               " = ", t_i$valor_dado[k], vf_label,
               "  \u2192  Memorizar: **", t_i$estimulo_memoria[k], "**"))
    }
    if (incluir_clave) {
      out <- c(out, "",
                paste0("*", L$secuencia, ": **",
                        resumen$secuencia_estimulos[i], "***"))
    } else {
      out <- c(out, "",
                paste0("Recuerde la secuencia: ___ ___ ___",
                        if (set_size > 3) paste0(rep(" ___", set_size - 3),
                                                  collapse = "") else ""))
    }
    out <- c(out, "", "---", "")
  }

  if (isTRUE(incluir_clave)) {
    out <- c(out, paste0("## ", L$clave), "")
    out <- c(out, "| Trial | Set | Secuencia a recordar | V/F operaciones |",
              "|---|---|---|---|")
    for (i in seq_len(nrow(resumen))) {
      n <- resumen$n_trial[i]
      t_i <- trials[trials$n_trial == n, ]
      vf <- paste(ifelse(t_i$es_verdadera, "V", "F"), collapse = "")
      out <- c(out, paste0("| ", n, " | ", resumen$set_size[i], " | ",
                            resumen$secuencia_estimulos[i], " | ", vf, " |"))
    }
    out <- c(out, "")
  }

  paste(out, collapse = "\n")
}


# =============================================================================
# Exportador (md + docx via officer)
# =============================================================================

#' @keywords internal
.exportar_cognitivo <- function(md, archivo, formato, docx_data,
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
        .exportar_cognitivo_docx_officer(archivo, docx_data, verbose),
        error = function(e) {
          warning("officer cognitivo fallo: ",
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
.exportar_cognitivo_docx_officer <- function(archivo, d, verbose) {

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
  col_op_v    <- "#1E8449"   # verde para V correctas
  col_op_f    <- "#C0392B"   # rojo para F
  fuente      <- "Calibri"
  L <- if (d$idioma == "en") list(instr = "Instructions",
                                    items = "Trials",
                                    clave = "Answer key",
                                    secuencia = "Sequence to recall")
        else list(instr = "Instrucciones",
                   items = "Trials",
                   clave = "Clave de respuestas",
                   secuencia = "Secuencia a recordar")

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

  # Instrucciones
  doc <- add_heading(doc, L$instr)
  instr_txt <- if (d$idioma == "en")
    "For each trial: solve the operation (TRUE/FALSE), then memorize the symbol. At the end of each trial, recall the symbols in order."
  else
    "Para cada trial: resuelva la operaci\u00F3n (decidir V/F), luego memorice el s\u00EDmbolo. Al final de cada trial, recuerde los s\u00EDmbolos en orden."
  doc <- add_par(doc, instr_txt, align = "justify", pad_bot = 6)

  # Trials
  doc <- add_heading(doc, L$items)

  escala_c <- d$escala_c
  trials  <- escala_c$trials
  resumen <- escala_c$trials_resumen

  for (i in seq_len(nrow(resumen))) {
    n <- resumen$n_trial[i]
    set_size <- resumen$set_size[i]
    nivel <- resumen$nivel_dificultad[i]

    # Sub-encabezado del trial
    doc <- off("body_add_fpar")(doc, fpar(
      ftext(paste0("Trial ", n, "  \u00B7  Nivel ", nivel,
                    "  (set size = ", set_size, ")"),
            fp_text(font.size = 12, bold = TRUE, color = col_titulo,
                    font.family = fuente)),
      fp_p = fp_par(text.align = "left", padding.top = 10, padding.bottom = 4)
    ))

    t_i <- trials[trials$n_trial == n, ]
    for (k in seq_len(nrow(t_i))) {
      # Linea: Op k. <operacion> = <valor_dado>  [V/F]   →  Memorizar: <est>
      paragraph_chunks <- list(
        ftext(paste0("Op ", k, ". "),
              fp_text(font.size = 11, bold = TRUE, color = col_titulo,
                      font.family = fuente)),
        ftext(paste0(t_i$operacion_str[k], " = ", t_i$valor_dado[k], "   "),
              fp_text(font.size = 11, color = col_texto,
                      font.family = fuente))
      )
      if (isTRUE(d$incluir_clave)) {
        col_vf <- if (t_i$es_verdadera[k]) col_op_v else col_op_f
        paragraph_chunks[[length(paragraph_chunks) + 1]] <- ftext(
          paste0("[", ifelse(t_i$es_verdadera[k], "V", "F"), "]"),
          fp_text(font.size = 11, bold = TRUE, color = col_vf,
                  font.family = fuente))
      } else {
        paragraph_chunks[[length(paragraph_chunks) + 1]] <- ftext(
          "[ V / F ]",
          fp_text(font.size = 11, color = col_subtit,
                  font.family = fuente))
      }
      paragraph_chunks[[length(paragraph_chunks) + 1]] <- ftext(
        "    \u2192    Memorizar: ",
        fp_text(font.size = 11, color = col_subtit, font.family = fuente))
      paragraph_chunks[[length(paragraph_chunks) + 1]] <- ftext(
        paste0("[ ", t_i$estimulo_memoria[k], " ]"),
        fp_text(font.size = 14, bold = TRUE, color = col_titulo,
                font.family = fuente))

      doc <- off("body_add_fpar")(doc, do.call(fpar, c(
        paragraph_chunks,
        list(fp_p = fp_par(text.align = "left",
                           padding.left = 18, padding.top = 2,
                           padding.bottom = 2)))))
    }

    # Recuperacion
    if (isTRUE(d$incluir_clave)) {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext(paste0(L$secuencia, ": "),
              fp_text(font.size = 11, italic = TRUE, color = col_subtit,
                      font.family = fuente)),
        ftext(resumen$secuencia_estimulos[i],
              fp_text(font.size = 12, bold = TRUE, color = col_op_v,
                      font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.left = 18,
                      padding.top = 6, padding.bottom = 4)
      ))
    } else {
      doc <- off("body_add_fpar")(doc, fpar(
        ftext("Recuerde la secuencia:  ",
              fp_text(font.size = 11, italic = TRUE, color = col_subtit,
                      font.family = fuente)),
        ftext(paste(rep("___", set_size), collapse = "  "),
              fp_text(font.size = 14, bold = TRUE, color = col_titulo,
                      font.family = fuente)),
        fp_p = fp_par(text.align = "left", padding.left = 18,
                      padding.top = 6, padding.bottom = 4)
      ))
    }
    doc <- off("body_add_par")(doc, "")
  }

  # Anexo: clave compacta al final si version_clave
  if (isTRUE(d$incluir_clave)) {
    doc <- off("body_add_break")(doc)
    doc <- add_heading(doc, L$clave)
    df_clave <- data.frame(
      Trial = resumen$n_trial,
      Set   = resumen$set_size,
      Nivel = resumen$nivel_dificultad,
      `Secuencia a recordar` = resumen$secuencia_estimulos,
      `V/F operaciones` = vapply(seq_len(nrow(resumen)), function(i) {
        n <- resumen$n_trial[i]
        t_i <- trials[trials$n_trial == n, ]
        paste(ifelse(t_i$es_verdadera, "V", "F"), collapse = "")
      }, character(1)),
      check.names = FALSE
    )
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


# =============================================================================
# Plantilla Shiny minima para OSPAN
# =============================================================================

#' @keywords internal
.generar_plantilla_shiny_ospan <- function(escala_c, ruta_salida) {

  trials_json <- jsonlite::toJSON(
    lapply(unique(escala_c$trials_resumen$n_trial), function(n) {
      t_i <- escala_c$trials[escala_c$trials$n_trial == n, ]
      list(
        n_trial = n,
        set_size = unique(t_i$set_size),
        operaciones = lapply(seq_len(nrow(t_i)), function(k) {
          list(
            str  = t_i$operacion_str[k],
            dado = t_i$valor_dado[k],
            es_verdadera = t_i$es_verdadera[k],
            estimulo = t_i$estimulo_memoria[k]
          )
        }),
        secuencia = strsplit(
          escala_c$trials_resumen$secuencia_estimulos[
            escala_c$trials_resumen$n_trial == n], " ")[[1]]
      )
    }),
    auto_unbox = TRUE, pretty = TRUE
  )

  lineas <- c(
    "# =============================================================================",
    "# Plantilla Shiny - OSPAN (generada por SeMiLLa)",
    "# =============================================================================",
    "",
    "library(shiny)",
    "library(jsonlite)",
    "",
    paste0("trials <- fromJSON('", ruta_salida, ".json', ",
            "simplifyVector = FALSE)  # cargar banco de trials"),
    "",
    "ui <- fluidPage(",
    "  titlePanel('Operation Span (OSPAN)'),",
    "  uiOutput('pantalla')",
    ")",
    "",
    "server <- function(input, output, session) {",
    "  estado <- reactiveValues(",
    "    trial = 1, paso = 0,  # 0 = mostrar operacion+letra; 1 = recuperar",
    "    respuestas_op = list(), respuestas_seq = list()",
    "  )",
    "",
    "  output$pantalla <- renderUI({",
    "    # Logica de paginacion: mostrar operacion-letra o recuperacion",
    "    # (esqueleto; el investigador completa segun necesidades)",
    "    h3(paste('Trial', estado$trial))",
    "  })",
    "}",
    "",
    "shinyApp(ui, server)",
    ""
  )
  writeLines(lineas, ruta_salida, useBytes = TRUE)

  # Tambien escribir el JSON con los trials
  writeLines(trials_json, paste0(ruta_salida, ".json"), useBytes = TRUE)
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_test_cognitivo_multi <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Test cognitivo ensamblado (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Paradigma: ", x$paradigma, "\n", sep = "")
  cat("  Nombre   : ", x$nombre_test, "\n", sep = "")
  cat("  Archivos :\n")
  for (a in x$archivos) cat("    - ", a, "\n", sep = "")
  cat("===========================================================\n\n")
  invisible(x)
}
