#' @title Ensamblar prueba objetiva como HTML imprimible (docente / alumno)
#'
#' @description
#' Genera dos archivos HTML autocontenidos (sin dependencias externas, con
#' CSS listo para imprimir o guardar como PDF desde el navegador) a partir
#' de un objeto \code{semilla_prueba_objetiva}:
#'
#' \enumerate{
#'   \item \strong{Version docente} (\code{<archivo>_docente.html}): marca
#'         la opcion correcta, muestra tema/nivel Bloom/formato por item e
#'         incluye al final la tabla de claves en pagina aparte. Si la
#'         prueba paso por \code{\link{verificar_clave}}, los items con
#'         discrepancia se resaltan.
#'   \item \strong{Version alumno} (\code{<archivo>_alumno.html}): solo
#'         enunciados y opciones, con bloque opcional de datos del
#'         participante.
#' }
#'
#' Si se indica una carpeta de \code{ilustraciones} (con archivos
#' \code{item_01.png}, \code{item_02.png}, ...), las imagenes se incrustan
#' en base64 dentro del HTML, de modo que el archivo se puede enviar o
#' mover sin perderlas.
#'
#' @param escala_o Objeto \code{semilla_prueba_objetiva}.
#' @param archivo Ruta SIN extension. Se generaran
#'   \code{<archivo>_docente.html} y \code{<archivo>_alumno.html}.
#' @param nombre_test Nombre de la prueba (si NULL, se autogenera).
#' @param subtitulo Subtitulo opcional.
#' @param autor Autor.
#' @param version Version.
#' @param incluir_datos Incluir bloque demografico (solo version alumno).
#' @param datos_solicitados Vector de campos demograficos.
#' @param instrucciones Texto custom; si NULL, autogenerado.
#' @param ilustraciones Carpeta con imagenes \code{item_XX.png} a incrustar
#'   (opcional).
#' @param idioma "es" o "en". Si NULL, se toma del objeto.
#' @param verbose Mostrar progreso.
#'
#' @return (Invisible) vector con las rutas de los dos HTML generados.
#'
#' @examples
#' \dontrun{
#' ensamblar_prueba_html(
#'   p,
#'   archivo = "salida/prueba_psicometria"
#' )
#' # -> salida/prueba_psicometria_docente.html
#' # -> salida/prueba_psicometria_alumno.html
#' }
#'
#' @seealso \code{\link{ensamblar_prueba_objetiva}} (version DOCX),
#'   \code{\link{verificar_clave}}
#'
#' @export
ensamblar_prueba_html <- function(
  escala_o,
  archivo,
  nombre_test       = NULL,
  subtitulo         = NULL,
  autor             = NULL,
  version           = "1.0",
  incluir_datos     = TRUE,
  datos_solicitados = c("codigo", "edad", "sexo", "nivel_educativo",
                         "fecha"),
  instrucciones     = NULL,
  ilustraciones     = NULL,
  idioma            = NULL,
  verbose           = TRUE
) {

  if (!inherits(escala_o, "semilla_prueba_objetiva"))
    stop("'escala_o' debe ser un objeto semilla_prueba_objetiva.")
  if (missing(archivo) || is.null(archivo) || !nzchar(archivo))
    stop("'archivo' (ruta sin extension) es obligatorio.")
  if (is.null(idioma)) idioma <- escala_o$idioma %||% "es"

  if (is.null(nombre_test)) {
    nombre_test <- paste0("Prueba objetiva - ",
                           .capitalizar(substr(escala_o$dominio, 1, 60)))
  }
  if (is.null(instrucciones)) {
    instrucciones <- .instrucciones_objetivas(idioma)
  }

  dir_salida <- dirname(archivo)
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

  rutas <- character(0)
  for (rol in c("docente", "alumno")) {
    con_clave <- identical(rol, "docente")
    sub_rol <- if (con_clave) {
      if (is.null(subtitulo)) {
        if (idioma == "en") "Teacher version (with answer key)"
        else "Versión docente (con clave de respuestas)"
      } else paste0(subtitulo, " — ",
                    if (idioma == "en") "Teacher version" else
                      "Versión docente")
    } else subtitulo

    html <- .construir_html_objetivas(
      escala_o          = escala_o,
      nombre_test       = nombre_test,
      subtitulo         = sub_rol,
      autor             = autor,
      version           = version,
      incluir_datos     = incluir_datos && !con_clave,
      datos_solicitados = datos_solicitados,
      instrucciones     = instrucciones,
      incluir_clave     = con_clave,
      ilustraciones     = ilustraciones,
      idioma            = idioma
    )

    ruta <- paste0(archivo, "_", rol, ".html")
    writeLines(html, ruta, useBytes = TRUE)
    rutas <- c(rutas, ruta)
    if (verbose) cat("  - ", ruta, "\n", sep = "")
  }

  if (verbose) {
    cat("\n[ensamblar_prueba_html] Listo. Abra los archivos en el navegador",
        "e imprima (Ctrl+P) o guarde como PDF.\n")
  }
  invisible(rutas)
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.html_escapar <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}


# Incrusta una imagen local como data URI (base64). Devuelve "" si el
# archivo no existe. Usa jsonlite (ya en Imports) para el encoding.

#' @keywords internal
.imagen_data_uri <- function(ruta) {
  if (is.null(ruta) || is.na(ruta) || !nzchar(ruta) || !file.exists(ruta))
    return("")
  ext <- tolower(tools::file_ext(ruta))
  mime <- switch(ext,
    "png"  = "image/png",
    "jpg"  = "image/jpeg",
    "jpeg" = "image/jpeg",
    "gif"  = "image/gif",
    "webp" = "image/webp",
    "image/png"
  )
  contenido <- readBin(ruta, what = "raw", n = file.info(ruta)$size)
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(contenido))
  paste0("data:", mime, ";base64,", b64)
}


#' @keywords internal
.css_prueba_html <- function() {
  paste(
    "body { font-family: Calibri, 'Segoe UI', Arial, sans-serif;",
    "       color: #262626; max-width: 820px; margin: 2rem auto;",
    "       padding: 0 1rem; line-height: 1.4; }",
    "h1 { color: #1F3864; text-align: center; margin-bottom: .2rem; }",
    ".subtitulo { color: #595959; text-align: center; font-style: italic;",
    "             margin-top: 0; }",
    ".meta { color: #595959; text-align: center; font-size: .9rem;",
    "        margin-bottom: 1.5rem; }",
    "h2 { color: #1F3864; border-bottom: 2px solid #1F3864;",
    "     padding-bottom: .2rem; margin-top: 1.6rem; }",
    ".item { margin: 1.1rem 0; }",
    ".enunciado { font-weight: bold; font-size: 1.02rem; }",
    ".enunciado .numero { color: #1F3864; }",
    ".meta-item { color: #595959; font-size: .8rem; font-style: italic; }",
    ".contexto { background: #F2F4F8; border-left: 4px solid #1F3864;",
    "            padding: .5rem .8rem; margin: .4rem 0; font-style: italic;",
    "            font-size: .95rem; }",
    ".instr-extra { color: #595959; font-style: italic; font-size: .9rem; }",
    ".opciones { list-style: none; padding-left: 1.4rem; margin: .4rem 0; }",
    ".opciones li { margin: .15rem 0; }",
    ".casilla { color: #1F3864; font-weight: bold; }",
    ".correcta { color: #1E8449; font-weight: bold; }",
    ".alerta-verificacion { color: #A11616; font-weight: bold;",
    "                       font-size: .85rem; }",
    ".ilustracion { text-align: center; margin: .5rem 0; }",
    ".ilustracion img { max-width: 420px; width: 70%; }",
    "table { border-collapse: collapse; margin: .6rem auto; }",
    "th { background: #1F3864; color: #FFFFFF; padding: .35rem .7rem; }",
    "td { border: 1px solid #BFBFBF; padding: .3rem .7rem; }",
    "tr:nth-child(even) td { background: #EAEEF5; }",
    ".datos p { border-bottom: 1px dotted #595959; padding: .35rem 0;",
    "           max-width: 560px; }",
    ".clave-final { }",
    "@media print {",
    "  body { margin: 1.2cm auto; max-width: none; }",
    "  .item { page-break-inside: avoid; }",
    "  .clave-final { page-break-before: always; }",
    "}",
    sep = "\n"
  )
}


#' @keywords internal
.construir_html_objetivas <- function(escala_o, nombre_test, subtitulo,
                                      autor, version, incluir_datos,
                                      datos_solicitados, instrucciones,
                                      incluir_clave, ilustraciones,
                                      idioma) {

  L <- .labels_objetivas(idioma)
  esc <- .html_escapar

  items     <- escala_o$items
  opciones  <- escala_o$opciones
  empar     <- escala_o$emparejamientos
  contextos <- escala_o$contextos
  verif     <- escala_o$verificacion

  html <- c(
    "<!DOCTYPE html>",
    paste0("<html lang='", idioma, "'>"),
    "<head>",
    "<meta charset='utf-8'>",
    paste0("<title>", esc(nombre_test), "</title>"),
    "<style>",
    .css_prueba_html(),
    "</style>",
    "</head>",
    "<body>",
    paste0("<h1>", esc(nombre_test), "</h1>")
  )
  if (!is.null(subtitulo) && nzchar(subtitulo)) {
    html <- c(html, paste0("<p class='subtitulo'>", esc(subtitulo), "</p>"))
  }
  meta <- character(0)
  if (!is.null(autor)) meta <- c(meta, esc(autor))
  if (!is.null(version) && nzchar(as.character(version))) {
    et <- if (idioma == "en") "Version " else "Versión "
    meta <- c(meta, paste0(et, esc(version)))
  }
  if (length(meta) > 0) {
    html <- c(html, paste0("<p class='meta'>",
                           paste(meta, collapse = " · "), "</p>"))
  }

  # Datos del participante
  if (incluir_datos && length(datos_solicitados) > 0) {
    etiquetas <- .etiquetas_demograficas(datos_solicitados, idioma)
    html <- c(html,
      paste0("<h2>", esc(L$datos_encab), "</h2>"),
      "<div class='datos'>",
      paste0("<p>", esc(unname(etiquetas)), "</p>"),
      "</div>")
  }

  # Instrucciones
  html <- c(html,
    paste0("<h2>", esc(L$instr_encab), "</h2>"),
    paste0("<p>", esc(instrucciones), "</p>"))

  # Items
  html <- c(html, paste0("<h2>", esc(L$items_encab), "</h2>"))

  for (i in seq_len(nrow(items))) {
    n   <- items$n_item[i]
    fmt <- items$formato[i]

    html <- c(html, "<div class='item'>")

    meta_item <- if (incluir_clave) {
      paste0(" <span class='meta-item'>[", esc(items$tema[i]), " | ",
             esc(items$nivel_bloom[i]), " | ", esc(fmt), "]</span>")
    } else ""
    html <- c(html, paste0(
      "<p class='enunciado'><span class='numero'>", n, ".</span> ",
      esc(items$enunciado[i]), meta_item, "</p>"))

    # Alerta de verificacion (solo docente)
    if (incluir_clave && !is.null(verif)) {
      v_i <- verif[verif$n_item == n, , drop = FALSE]
      if (nrow(v_i) > 0 && !is.na(v_i$coincide[1]) && !v_i$coincide[1]) {
        et <- if (idioma == "en") {
          paste0("&#9888; Independent LLM solver answered '",
                 esc(v_i$respuesta_llm[1]),
                 "' instead of the declared key. Review this item.")
        } else {
          paste0("&#9888; El examinado LLM respondió '",
                 esc(v_i$respuesta_llm[1]),
                 "' y no la clave declarada. Revise este ítem.")
        }
        html <- c(html, paste0("<p class='alerta-verificacion'>", et, "</p>"))
      }
    }

    # Contexto (formato contexto_dependiente)
    if (fmt == "contexto_dependiente" && nrow(contextos) > 0) {
      ctx_i <- contextos[contextos$n_item == n, ]
      if (nrow(ctx_i) > 0) {
        html <- c(html, paste0("<div class='contexto'>",
                               esc(ctx_i$contexto[1]), "</div>"))
      }
    }

    # Instruccion extra
    if (!is.na(items$instruccion_extra[i]) &&
        nzchar(items$instruccion_extra[i])) {
      html <- c(html, paste0("<p class='instr-extra'>",
                             esc(items$instruccion_extra[i]), "</p>"))
    }

    # Ilustracion incrustada (item_01.png, item_02.png, ...)
    if (!is.null(ilustraciones)) {
      ruta_img <- file.path(ilustraciones, sprintf("item_%02d.png", n))
      uri <- .imagen_data_uri(ruta_img)
      if (nzchar(uri)) {
        html <- c(html, paste0(
          "<div class='ilustracion'><img src='", uri,
          "' alt='Ilustracion item ", n, "'></div>"))
      }
    }

    if (fmt == "emparejamiento" && nrow(empar) > 0) {
      e_i <- empar[empar$n_item == n, ]
      if (nrow(e_i) > 0) {
        # Mismo barajado que el ensamblador DOCX (semilla por item)
        if (!incluir_clave) {
          set.seed(n)
          orden <- sample(seq_len(nrow(e_i)))
          respuestas_vis <- e_i$respuesta[orden]
        } else {
          respuestas_vis <- e_i$respuesta
        }
        enc <- if (idioma == "en") c("Premises", "Responses") else
                 c("Premisas", "Respuestas")
        html <- c(html, "<table>",
          paste0("<tr><th>", enc[1], "</th><th>", enc[2], "</th></tr>"))
        for (k in seq_len(nrow(e_i))) {
          html <- c(html, paste0(
            "<tr><td>", k, ". ", esc(e_i$premisa[k]), "</td><td>",
            letters[k], ") ", esc(respuestas_vis[k]), "</td></tr>"))
        }
        html <- c(html, "</table>")
        if (incluir_clave) {
          clave_txt <- paste(seq_len(nrow(e_i)), "↔",
                             letters[seq_len(nrow(e_i))],
                             collapse = "  ·  ")
          html <- c(html, paste0("<p class='correcta'>",
                                 if (idioma == "en") "Key: " else "Clave: ",
                                 clave_txt, "</p>"))
        }
      }
    } else {
      ops_i <- opciones[opciones$n_item == n, ]
      if (nrow(ops_i) > 0) {
        html <- c(html, "<ul class='opciones'>")
        for (k in seq_len(nrow(ops_i))) {
          if (incluir_clave && ops_i$es_correcta[k]) {
            html <- c(html, paste0(
              "<li class='correcta'>(&#10003;) ",
              esc(ops_i$texto_opcion[k]), "</li>"))
          } else {
            html <- c(html, paste0(
              "<li><span class='casilla'>(&nbsp;&nbsp;)</span> ",
              esc(ops_i$texto_opcion[k]), "</li>"))
          }
        }
        html <- c(html, "</ul>")
      }
    }

    html <- c(html, "</div>")
  }

  # Tabla de claves al final (solo docente), en pagina aparte al imprimir
  if (incluir_clave) {
    df_clave <- .construir_tabla_clave(escala_o, idioma = idioma)
    html <- c(html,
      "<div class='clave-final'>",
      paste0("<h2>", .html_escapar(L$clave_encab), "</h2>"),
      "<table>",
      paste0("<tr>", paste0("<th>", .html_escapar(names(df_clave)),
                            "</th>", collapse = ""), "</tr>"))
    for (r in seq_len(nrow(df_clave))) {
      html <- c(html, paste0(
        "<tr>", paste0("<td>",
                       .html_escapar(unlist(df_clave[r, ], use.names = FALSE)),
                       "</td>", collapse = ""), "</tr>"))
    }
    html <- c(html, "</table>", "</div>")
  }

  c(html, "</body>", "</html>")
}
