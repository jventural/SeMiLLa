#' @title Detectar que items se benefician de una ilustracion
#'
#' @description
#' Analiza cada item y decide, con una heuristica lexica (sin llamadas al
#' LLM, costo cero), si el item describe una ESCENA DIBUJABLE que se
#' beneficiaria de apoyo visual. Pensada para el flujo de
#' \code{prompts_ilustracion()}: en escalas para ninos o poblaciones con
#' lectura limitada no siempre conviene ilustrar TODOS los items; esta
#' funcion prioriza los que realmente ganan con una imagen.
#'
#' La regla se basa en que una escena ilustrable necesita un actor haciendo
#' algo concreto en un lugar o con alguien. Se buscan cuatro tipos de
#' senales en el texto del item:
#'
#' \enumerate{
#'   \item \strong{Referencia visual explicita} (el item menciona una
#'         imagen, dibujo, figura o pide observar algo): marca directa.
#'   \item \strong{Accion observable} (jugar, abrazar, llorar, pelear,
#'         compartir, gritar, comer...).
#'   \item \strong{Escenario concreto} (aula, recreo, casa, parque, mesa,
#'         calle...).
#'   \item \strong{Interlocutor} (amigos, profesor, mama, papa, hermanos,
#'         companeros...).
#' }
#'
#' Un item se marca como ilustrable si tiene referencia visual explicita, o
#' si reune al menos \code{umbral_senales} de los tipos 2-4. Los items
#' puramente introspectivos (pensar, creer, opinar, sentirse + estado
#' abstracto) sin accion observable se marcan como NO ilustrables aunque
#' mencionen personas, porque la imagen no puede mostrar el contenido
#' mental que mide el item.
#'
#' Cada decision es trazable: se reportan los gatillos encontrados y el
#' criterio aplicado.
#'
#' @param escala Objeto \code{semilla}, \code{semilla_items},
#'   \code{semilla_prueba_objetiva} o data.frame con columna \code{item}.
#' @param umbral_senales Numero minimo de tipos de senal (accion, escenario,
#'   interlocutor) para marcar el item como ilustrable (default 2).
#' @param verbose Mostrar resumen.
#'
#' @return Data.frame de clase \code{semilla_deteccion_ilustracion} con
#'   columnas: \code{n_item}, \code{dimension}, \code{item},
#'   \code{necesita_ilustracion}, \code{criterio}, \code{gatillos}.
#'
#' @examples
#' \dontrun{
#' det <- detectar_necesidad_ilustracion(mi_escala)
#' subset(det, necesita_ilustracion)
#'
#' # Ilustrar solo los items detectados
#' seleccion <- mi_escala$items[det$necesita_ilustracion, ]
#' p <- prompts_ilustracion(seleccion, api_key = api_key)
#' }
#'
#' @seealso \code{prompts_ilustracion()}
#'
#' @export
detectar_necesidad_ilustracion <- function(escala,
                                           umbral_senales = 2L,
                                           verbose        = TRUE) {

  items_df <- .extraer_df_items_ilustrables(escala)
  if (is.null(items_df) || nrow(items_df) == 0) {
    stop("No se encontraron items en 'escala'.")
  }

  lex <- .lexico_ilustracion_es()
  n <- nrow(items_df)

  necesita <- logical(n)
  criterio <- character(n)
  gatillos <- character(n)

  for (i in seq_len(n)) {
    txt <- .normalizar_texto_deteccion(items_df$item[i])

    hits_visual <- .buscar_gatillos(txt, lex$visual)
    hits_accion <- .buscar_gatillos(txt, lex$accion)
    hits_lugar  <- .buscar_gatillos(txt, lex$escenario)
    hits_pers   <- .buscar_gatillos(txt, lex$interlocutor)
    hits_abstr  <- .buscar_gatillos(txt, lex$introspectivo)

    encontrados <- c(hits_visual, hits_accion, hits_lugar, hits_pers)
    gatillos[i] <- if (length(encontrados) > 0)
                     paste(unique(encontrados), collapse = ", ")
                   else NA_character_

    if (length(hits_visual) > 0) {
      necesita[i] <- TRUE
      criterio[i] <- "referencia visual explicita"
      next
    }

    if (length(hits_abstr) > 0 && length(hits_accion) == 0) {
      necesita[i] <- FALSE
      criterio[i] <- paste0("introspectivo sin accion observable (",
                            paste(unique(hits_abstr), collapse = ", "), ")")
      next
    }

    n_senales <- sum(length(hits_accion) > 0,
                     length(hits_lugar)  > 0,
                     length(hits_pers)   > 0)

    if (n_senales >= umbral_senales) {
      necesita[i] <- TRUE
      partes <- c(
        if (length(hits_accion) > 0) "accion",
        if (length(hits_lugar)  > 0) "escenario",
        if (length(hits_pers)   > 0) "interlocutor"
      )
      criterio[i] <- paste(partes, collapse = " + ")
    } else {
      necesita[i] <- FALSE
      criterio[i] <- if (n_senales == 0)
        "sin senales de escena dibujable"
      else
        paste0("senales insuficientes (", n_senales, " de ",
               umbral_senales, " requeridas)")
    }
  }

  resultado <- data.frame(
    n_item    = if ("n_item" %in% names(items_df)) items_df$n_item
                else seq_len(n),
    dimension = if ("dimension" %in% names(items_df))
                  as.character(items_df$dimension) else NA_character_,
    item      = as.character(items_df$item),
    necesita_ilustracion = necesita,
    criterio  = criterio,
    gatillos  = gatillos,
    stringsAsFactors = FALSE
  )
  class(resultado) <- c("semilla_deteccion_ilustracion", "data.frame")

  if (verbose) {
    cat("\n[detectar_necesidad_ilustracion] ", sum(necesita), " de ", n,
        " items marcados como ilustrables.\n", sep = "")
    if (sum(necesita) > 0 && sum(necesita) < n) {
      cat("  Sugerencia: ilustre solo los marcados; el resto no describe",
          "una escena dibujable.\n")
    }
  }

  resultado
}


# =============================================================================
# Helpers internos
# =============================================================================

# Extrae items tambien de pruebas objetivas (enunciados), ademas de los
# objetos que ya cubre .extraer_df_items().

#' @keywords internal
.extraer_df_items_ilustrables <- function(x) {
  if (inherits(x, "semilla_prueba_objetiva")) {
    return(data.frame(
      n_item    = x$items$n_item,
      dimension = x$items$tema,
      item      = x$items$enunciado,
      stringsAsFactors = FALSE
    ))
  }
  .extraer_df_items(x)
}


# Minusculas + sin tildes, para que los patrones (escritos sin tildes)
# alcancen texto real con tildes.

#' @keywords internal
.normalizar_texto_deteccion <- function(s) {
  s <- tolower(as.character(s))
  chartr("áéíóúüñ",
         "aeiouun", s)
}


# Devuelve las raices del lexico presentes en el texto (match por raiz al
# inicio de palabra, para cubrir conjugaciones y plurales).

#' @keywords internal
.buscar_gatillos <- function(txt, raices) {
  presentes <- vapply(raices, function(r) {
    grepl(paste0("\\b", r), txt, perl = TRUE)
  }, logical(1))
  raices[presentes]
}


# Lexico curado para items psicologicos/educativos en espanol. Son RAICES
# (sin tildes): "jueg" cubre juego/juega/juegan; "pele" cubre pelea/peleo.

#' @keywords internal
.lexico_ilustracion_es <- function() {
  list(
    visual = c(
      "imagen", "dibujo", "figura", "foto", "lamina", "escena",
      "observa", "mira la", "mira el", "senala"
    ),
    accion = c(
      "jueg", "jugar", "abraz", "llor", "pele", "pega", "pegan", "grit",
      "compart", "ayud", "corr", "come ", "comen", "comer", "duerm",
      "dormir", "romp", "escond", "salud", "empuj", "burl", "molest",
      "invit", "regal", "acompan", "visit", "salt", "bail", "cant",
      "dibuj", "pint", "lee ", "leen", "leer", "escrib", "levant",
      "camin", "cocin", "limpi", "orden", "cuid", "consuel", "defiend",
      "presta", "prestan", "quita", "quitan", "insult", "amenaz",
      "castig", "felicit", "aplaud"
    ),
    escenario = c(
      "aula", "clase", "colegio", "escuela", "salon", "recreo", "patio",
      "casa", "cuarto", "habitacion", "dormitorio", "sala", "comedor",
      "mesa", "cocina", "parque", "calle", "cancha", "tienda", "iglesia",
      "hospital", "consultorio", "oficina", "trabajo", "bus", "micro",
      "fiesta", "cumplean"
    ),
    interlocutor = c(
      "amig", "companer", "profesor", "maestr", "docente", "mama", "papa",
      "padre", "madre", "herman", "abuel", "ti[oa]s?\\b", "prim[oa]s?\\b",
      "familia", "pareja", "vecin", "nin[oa]s?\\b", "adult", "jefe",
      "compania de otr"
    ),
    introspectivo = c(
      "pienso", "piensa", "creo que", "cree que", "opino", "opina",
      "me parece", "le parece", "considero", "considera", "valoro",
      "valora", "imagino que", "reflexion", "me pregunto", "recuerdo que",
      "confio en", "espero que", "deseo que", "sueno con", "me preocupa",
      "me siento capaz", "estoy satisfech", "soy capaz",
      "tengo la sensacion"
    )
  )
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_deteccion_ilustracion <- function(x, ...) {
  # Si el usuario subseteo columnas, degradar al print de data.frame
  cols_req <- c("n_item", "item", "necesita_ilustracion", "criterio")
  if (!all(cols_req %in% names(x))) {
    return(print.data.frame(x, ...))
  }
  cat("\n")
  cat("===========================================================\n")
  cat("  Deteccion de necesidad de ilustracion (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Items analizados : ", nrow(x), "\n", sep = "")
  cat("  Ilustrables      : ", sum(x$necesita_ilustracion), "\n", sep = "")
  cat("-----------------------------------------------------------\n")
  for (i in seq_len(nrow(x))) {
    marca <- if (x$necesita_ilustracion[i]) "[SI]" else "[  ]"
    cat("  ", marca, " ", sprintf("%2d.", x$n_item[i]), " ",
        substr(x$item[i], 1, 60),
        if (nchar(x$item[i]) > 60) "..." else "", "\n", sep = "")
    cat("        criterio: ", x$criterio[i], "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}
