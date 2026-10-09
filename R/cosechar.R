#' @title Exportar Escala
#'
#' @description
#' Exporta los items a un archivo Excel (o CSV, si \pkg{openxlsx} no esta
#' instalado) y, opcionalmente, un archivo de texto con la informacion de la
#' escala (definicion, fundamentacion, dimensiones, referencias, estructura e
#' instrucciones de aplicacion). No escribe nada si no se indica \code{archivo}.
#'
#' @param x Objeto \code{semilla}, \code{semilla_items} o lista con
#'   estructura compatible (al menos \code{$items}; se usan tambien
#'   \code{$concepto} y \code{$metadata} si existen).
#' @param archivo Ruta del archivo de salida \strong{sin} extension. Es
#'   obligatoria: no hay valor por defecto para no escribir en el directorio
#'   de trabajo. Use, por ejemplo, \code{file.path(tempdir(), "escala")}.
#' @param formato Formato de exportacion: \code{"csv"}, \code{"excel"} o
#'   \code{"ambos"}. Se conserva por compatibilidad; en la version actual los
#'   items se escriben siempre en \code{.xlsx} si \pkg{openxlsx} esta
#'   disponible y en \code{.csv} si no lo esta.
#' @param incluir_info Logico. Si \code{TRUE} (por defecto), escribe ademas
#'   \code{<archivo>_info.txt} con la informacion de la escala.
#' @param verbose Logico. Si \code{TRUE}, muestra la ruta de cada archivo
#'   creado.
#'
#' @return Vector de caracteres, invisible, con las rutas de los archivos
#'   creados: el de items (\code{.xlsx} o \code{.csv}) y, si
#'   \code{incluir_info = TRUE}, el \code{_info.txt}.
#'
#' @examples
#' f <- file.path(tempdir(), "escala_demo")
#' archivos <- exportar_escala(semilla_demo, archivo = f, verbose = FALSE)
#' basename(archivos)
#' unlink(archivos)
#'
#' @export
exportar_escala <- function(x,
                            archivo,
                            formato = "csv",
                            incluir_info = TRUE,
                            verbose = TRUE) {

  if (missing(archivo) || is.null(archivo) || !nzchar(archivo)) {
    stop("Indica 'archivo' (ruta sin extension), por ejemplo ",
         "file.path(tempdir(), \"escala\").", call. = FALSE)
  }

  # Extraer datos segun tipo
  if (inherits(x, "semilla") || inherits(x, "semilla_items")) {
    items <- x$items
    concepto <- x$concepto
    metadata <- x$metadata
    efa <- .separabilidad(x)
  } else if (is.list(x) && !is.null(x$items)) {
    items <- x$items
    concepto <- x$concepto
    metadata <- x$metadata
    efa <- .separabilidad(x)
  } else {
    stop("Objeto no valido. Usa un objeto semilla, semilla_items, o lista con $items")
  }

  archivos_creados <- c()

  # Exportar items a Excel
  archivo_xlsx <- paste0(archivo, ".xlsx")
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(items, archivo_xlsx)
  } else {
    # Fallback a CSV si openxlsx no esta disponible
    archivo_xlsx <- paste0(archivo, ".csv")
    write.csv(items, archivo_xlsx, row.names = FALSE, fileEncoding = "UTF-8")
  }
  archivos_creados <- c(archivos_creados, archivo_xlsx)

  if (verbose) cat("  ", .color_check(), " Items: ", archivo_xlsx, "\n", sep = "")

  # Exportar informacion
  if (incluir_info) {
    archivo_info <- paste0(archivo, "_info.txt")

    texto <- c(
      .linea("="),
      paste0("  ESCALA: ", toupper(metadata$concepto_original)),
      .linea("="),
      "",
      paste0("Fecha de generacion: ", metadata$fecha),
      paste0("Idioma: ", .nombre_idioma(metadata$idioma)),
      paste0("Poblacion: ", ifelse(is.null(metadata$poblacion), "General", metadata$poblacion)),
      paste0("Modelo: ", metadata$modelo),
      paste0("Items generados: ", metadata$n_items_generados),
      "",
      .linea("-"),
      "DEFINICION:",
      .linea("-"),
      concepto$definicion,
      ""
    )

    # Fundamentacion teorica
    if (!is.null(concepto$fundamentacion_teorica)) {
      texto <- c(texto,
        .linea("-"),
        "FUNDAMENTACION TEORICA:",
        .linea("-"),
        ""
      )

      if (!is.null(concepto$fundamentacion_teorica$teorias_base)) {
        texto <- c(texto, "Teorias base:")
        for (t in concepto$fundamentacion_teorica$teorias_base) {
          texto <- c(texto, paste0("  * ", t))
        }
        texto <- c(texto, "")
      }

      if (!is.null(concepto$fundamentacion_teorica$modelos_referencia)) {
        texto <- c(texto, "Modelos de referencia:")
        for (m in concepto$fundamentacion_teorica$modelos_referencia) {
          texto <- c(texto, paste0("  * ", m))
        }
        texto <- c(texto, "")
      }

      if (!is.null(concepto$fundamentacion_teorica$justificacion)) {
        texto <- c(texto, "Justificacion:", concepto$fundamentacion_teorica$justificacion, "")
      }
    }

    # Dimensiones
    texto <- c(texto,
      .linea("-"),
      "DIMENSIONES:",
      .linea("-"),
      ""
    )

    for (d in names(concepto$dimensiones)) {
      n_items_dim <- sum(items$dimension == d)
      texto <- c(texto,
        paste0("[", n_items_dim, " items] ", toupper(d)),
        concepto$dimensiones[[d]],
        ""
      )
    }

    # Referencias
    if (!is.null(concepto$referencias) && length(concepto$referencias) > 0) {
      texto <- c(texto,
        .linea("-"),
        "REFERENCIAS:",
        .linea("-"),
        ""
      )
      refs <- unlist(concepto$referencias)
      for (i in seq_along(refs)) {
        texto <- c(texto, paste0("[", i, "] ", refs[i]))
      }
      texto <- c(texto, "")
    }

    # EFA
    if (!is.null(efa)) {
      texto <- c(texto,
        .linea("-"),
        "ANALISIS FACTORIAL (EFA):",
        .linea("-"),
        "",
        paste0("Factores extraidos: ", efa$metadata$n_factores),
        paste0("Rotacion: ", efa$metadata$rotacion),
        paste0("Varianza explicada: ", round(sum(efa$varianza$Prop_Var) * 100, 1), "%"),
        ""
      )
    }

    # Instrucciones
    texto <- c(texto,
      .linea("-"),
      "INSTRUCCIONES DE APLICACION:",
      .linea("-"),
      "",
      "A continuacion encontraras una serie de afirmaciones. Por favor, indica",
      "en que medida cada una te describe, usando la siguiente escala:",
      "",
      "1 = Totalmente en desacuerdo",
      "2 = En desacuerdo",
      "3 = Ni de acuerdo ni en desacuerdo",
      "4 = De acuerdo",
      "5 = Totalmente de acuerdo",
      "",
      .linea("="),
      "Generado con SeMiLLa: SEmantic Measurement Items via LLM Assistance",
      .linea("=")
    )

    writeLines(texto, archivo_info, useBytes = TRUE)
    archivos_creados <- c(archivos_creados, archivo_info)

    if (verbose) cat("  ", .color_check(), " Info: ", archivo_info, "\n", sep = "")
  }

  invisible(archivos_creados)
}


#' @title Guardar Objeto SeMiLLa
#'
#' @description
#' Guarda el objeto completo en un archivo \code{.rds} para uso posterior
#' (se recupera con \code{\link{cargar}}).
#'
#' @param semilla Objeto \code{semilla} (o cualquier objeto de R) a guardar.
#' @param archivo Ruta del archivo \strong{sin} la extension \code{.rds}.
#'   Es obligatoria: no hay valor por defecto para no escribir en el
#'   directorio de trabajo. Use, por ejemplo,
#'   \code{file.path(tempdir(), "escala")}.
#' @param verbose Logico. Si \code{TRUE} (por defecto), muestra la ruta del
#'   archivo creado.
#'
#' @return Cadena de caracteres, invisible, con la ruta del archivo
#'   \code{.rds} creado. Se llama por su efecto.
#'
#' @examples
#' f <- file.path(tempdir(), "semilla_demo")
#' ruta <- guardar(semilla_demo, f, verbose = FALSE)
#' x <- cargar(ruta)
#' identical(x$items, semilla_demo$items)
#' unlink(ruta)
#'
#' @export
guardar <- function(semilla, archivo, verbose = TRUE) {

  if (missing(archivo) || is.null(archivo) || !nzchar(archivo)) {
    stop("Indica 'archivo' (ruta sin extension .rds), por ejemplo ",
         "file.path(tempdir(), \"escala\").", call. = FALSE)
  }
  archivo_rds <- paste0(archivo, ".rds")
  saveRDS(semilla, archivo_rds)

  if (verbose) {
    cat("  ", .color_check(), " Guardado: ", archivo_rds, "\n", sep = "")
  }

  invisible(archivo_rds)
}


#' @title Cargar Objeto SeMiLLa
#'
#' @description
#' Carga un objeto SeMiLLa guardado previamente con \code{\link{guardar}}.
#'
#' @param archivo Ruta al archivo \code{.rds}.
#'
#' @return El objeto guardado en el archivo (normalmente de clase
#'   \code{semilla}), tal como lo devuelve \code{readRDS()}.
#'
#' @examples
#' f <- file.path(tempdir(), "semilla_demo")
#' ruta <- guardar(semilla_demo, f, verbose = FALSE)
#' x <- cargar(ruta)
#' class(x)
#' unlink(ruta)
#'
#' @export
cargar <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("Archivo no encontrado: ", archivo)
  }
  readRDS(archivo)
}
