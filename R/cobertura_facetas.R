# =============================================================================
# SeMiLLa - Cobertura de facetas del constructo
# =============================================================================
#  Por que existe este archivo (2026-08-16):
#
#  Una corrida de estructura_por_consenso(auto_refinar = TRUE) sobre una escala
#  de regulacion emocional (24 items, 4 dimensiones) subio la precision de
#  clasificacion de 66.7% a 95.8% y el ARI de 0.493 a 0.884. Excelente por ese
#  lado. Pero al leer los items reescritos aparecieron DOS facetas declaradas
#  del constructo que se habian quedado en CERO items:
#
#    - "Aceptacion sin juicio" (Atencion Plena): 4 items -> 0
#    - "Cuidado de la salud fisica" (Regulacion Emocional): 1 item -> 0
#
#  La primera es literalmente lo que la definicion de esa dimension dice medir
#  ("sin emitir juicios de valor"). La escala clasificaba mejor porque sus items
#  se habian vuelto casi identicos entre si, que es justo lo que sube el
#  consenso del ensemble y hunde la validez de contenido.
#
#  El paquete no tenia forma de verlo: ninguna funcion comprobaba que las
#  caracteristicas declaradas en $concepto$caracteristicas siguieran cubiertas
#  por algun item. Perder una faceta era invisible.
#
#  Esta compuerta lo hace visible y, enganchada en .compuerta_estructura(),
#  BLOQUEA: una escala a la que le falta una faceta de su propia definicion no
#  puede darse por buena por muy bien que clasifique.
# =============================================================================

#' @title Auditoria de cobertura de facetas del constructo
#'
#' @description
#' Comprueba que cada caracteristica (faceta) declarada en
#' \code{escala$concepto$caracteristicas} siga cubierta por al menos un item.
#' Es el contrapeso de contenido a los indices de estructura: el consenso del
#' ensemble premia que los items de una dimension se parezcan entre si, y la
#' via mas corta para lograrlo es que todos midan la misma faceta. Sin esta
#' comprobacion, esa perdida no deja rastro.
#'
#' @param escala Objeto \code{semilla} con \code{$items} (columnas
#'   \code{dimension} y \code{caracteristica}) y
#'   \code{$concepto$caracteristicas} (lista con nombre por dimension).
#' @param min_items Numero minimo de items por faceta para considerarla
#'   cubierta (por defecto 1).
#' @param verbose Imprimir el resumen.
#'
#' @return Objeto de clase \code{semilla_cobertura} (lista) con:
#' \itemize{
#'   \item \code{tabla}: data.frame con una fila por faceta declarada
#'     (dimension, faceta, n_items, cubierta).
#'   \item \code{huerfanas}: subconjunto de \code{tabla} con las facetas sin
#'     items suficientes.
#'   \item \code{sin_declarar}: etiquetas de \code{caracteristica} presentes en
#'     los items que NO figuran entre las declaradas (sintoma de que el LLM
#'     invento una faceta al reemplazar).
#'   \item \code{n_declaradas}, \code{n_cubiertas}, \code{prop_cubierta}.
#'   \item \code{disponible}: FALSE si la escala no trae caracteristicas
#'     declaradas, en cuyo caso la auditoria no se puede hacer y NO debe
#'     interpretarse como que todo esta bien.
#' }
#'
#' @examples
#' \dontrun{
#' cob <- auditar_cobertura_facetas(escala)
#' cob$huerfanas   # las facetas que se quedaron sin items
#' }
#'
#' @export
auditar_cobertura_facetas <- function(escala, min_items = 1L, verbose = TRUE) {

  vacio <- function(motivo) {
    structure(list(
      tabla = data.frame(dimension = character(0), faceta = character(0),
                         n_items = integer(0), cubierta = logical(0),
                         stringsAsFactors = FALSE),
      huerfanas = data.frame(), sin_declarar = character(0),
      n_declaradas = 0L, n_cubiertas = 0L, prop_cubierta = NA_real_,
      disponible = FALSE, motivo = motivo
    ), class = c("semilla_cobertura", "list"))
  }

  if (is.null(escala) || is.null(escala$items) || !nrow(escala$items))
    return(vacio("la escala no tiene items"))
  cc <- escala$concepto$caracteristicas
  if (is.null(cc) || !length(cc))
    return(vacio("la escala no declara caracteristicas por dimension"))
  if (!"caracteristica" %in% names(escala$items))
    return(vacio("los items no traen la columna 'caracteristica'"))

  it <- escala$items

  filas <- list()
  for (d in names(cc)) {
    declaradas <- unlist(cc[[d]], use.names = FALSE)
    declaradas <- declaradas[nzchar(trimws(as.character(declaradas)))]
    if (!length(declaradas)) next
    en_dim <- it$caracteristica[as.character(it$dimension) == d]
    for (f in declaradas) {
      m <- .casar_faceta(en_dim, f)
      filas[[length(filas) + 1L]] <- data.frame(
        dimension = d, faceta = as.character(f), n_items = as.integer(m$n),
        cubierta = m$n >= min_items, metodo = m$metodo, stringsAsFactors = FALSE)
    }
  }
  if (!length(filas)) return(vacio("ninguna dimension declara caracteristicas"))
  tabla <- do.call(rbind, filas)

  # v2.9.29 -- Guarda contra el falso positivo masivo ------------------------
  #  El LLM devuelve la columna 'caracteristica' en texto libre y a menudo
  #  ACORTA la etiqueta declarada ("Grado de bloqueo cognitivo durante la
  #  resolucion de problemas" por "...en un examen, evaluado por..."). Con un
  #  cruce solo por igualdad exacta, una escala perfectamente cubierta daba
  #  0 de 9 facetas y la compuerta la habria bloqueado entera. Verificado sobre
  #  una escala real de ansiedad estadistica (18 items, 9 facetas declaradas).
  #
  #  Por eso .casar_faceta() cruza en tres niveles (exacto, contencion,
  #  solapamiento de palabras) y aqui se comprueba el resultado global: si NO
  #  casa ninguna, el problema son las etiquetas, no el contenido, y hay que
  #  decirlo asi en vez de declarar 100% de facetas perdidas.
  if (all(tabla$n_items == 0) && any(nzchar(trimws(as.character(it$caracteristica))))) {
    r <- vacio(paste0(
      "las etiquetas de 'caracteristica' de los items no casan con ninguna de ",
      "las ", nrow(tabla), " facetas declaradas: probablemente fueron reescritas ",
      "(el refinamiento cambia items$caracteristica sin tocar ",
      "concepto$caracteristicas). No se puede saber que esta cubierto."))
    r$tabla <- tabla
    r$sin_declarar <- unique(stats::na.omit(as.character(it$caracteristica)))
    if (isTRUE(verbose)) print(r)
    return(r)
  }

  ## Etiquetas que aparecen en los items pero no casan con ninguna declarada
  declaradas_todas <- unlist(cc, use.names = FALSE)
  usadas <- unique(stats::na.omit(as.character(it$caracteristica)))
  sin_declarar <- usadas[!vapply(usadas, function(u)
    any(vapply(declaradas_todas, function(f) .casar_faceta(u, f)$n > 0,
               logical(1), USE.NAMES = FALSE)),
    logical(1), USE.NAMES = FALSE)]

  res <- structure(list(
    tabla         = tabla,
    huerfanas     = tabla[!tabla$cubierta, , drop = FALSE],
    sin_declarar  = sin_declarar,
    n_declaradas  = nrow(tabla),
    n_cubiertas   = sum(tabla$cubierta),
    prop_cubierta = mean(tabla$cubierta),
    disponible    = TRUE,
    motivo        = NA_character_
  ), class = c("semilla_cobertura", "list"))

  if (isTRUE(verbose)) print(res)
  res
}

## Cruza una faceta declarada contra las etiquetas que llevan los items, en tres
## niveles y por ese orden. El cruce por igualdad exacta es demasiado rigido: el
## LLM acorta, alarga y reformula la etiqueta, y con una sola tilde de diferencia
## una faceta cubierta se declaraba perdida.
##   1. exacto     : misma cadena normalizada
##   2. contencion : una es prefijo o subcadena de la otra (el caso tipico:
##                   la etiqueta del item es la declarada truncada)
##   3. solape     : >= 60% de palabras de contenido compartidas
.casar_faceta <- function(etiquetas, faceta, umbral_solape = 0.60) {
  et <- .norm_faceta(etiquetas)
  fa <- .norm_faceta(faceta)
  et <- et[nzchar(et)]
  if (!length(et) || !nzchar(fa)) return(list(n = 0L, metodo = NA_character_))

  # v2.9.31: los tres niveles se evaluan SOBRE TODAS las etiquetas y se suman
  # los items distintos que casan por cualquiera de ellos.
  #
  # Antes esto era una cascada con return anticipado: en cuanto el nivel
  # "exacto" encontraba UNA coincidencia devolvia n = 1 y no llegaba a mirar
  # contencion ni solape, donde casaban las demas. Medido el 2026-08-20 sobre
  # una escala real de 24 items: la faceta "Comparte y explica tradiciones o
  # practicas culturales propias..." la cubrian 6 items -uno con la etiqueta
  # literal y cinco con la misma etiqueta truncada por el LLM- y la funcion
  # reportaba 1. La columna n_items sumaba 13 de 24 y hacia parecer que unas
  # dimensiones tenian mas items que otras cuando las tres tenian 8.
  #
  # El veredicto de faceta huerfana NO cambia (una faceta sin items daba 0 por
  # los tres niveles antes y despues); lo que se corrige es CUANTOS la cubren.
  ex <- et == fa

  cont <- vapply(et, function(e)
    startsWith(e, fa) || startsWith(fa, e) ||
    grepl(e, fa, fixed = TRUE) || grepl(fa, e, fixed = TRUE),
    logical(1), USE.NAMES = FALSE)

  pf <- setdiff(strsplit(fa, " ")[[1]], .VACIAS_FACETA)
  sol <- if (!length(pf)) rep(FALSE, length(et)) else vapply(et, function(e) {
    pe <- setdiff(strsplit(e, " ")[[1]], .VACIAS_FACETA)
    if (!length(pe)) return(FALSE)
    length(intersect(pe, pf)) / min(length(pe), length(pf)) >= umbral_solape
  }, logical(1), USE.NAMES = FALSE)

  casan <- ex | cont | sol
  if (!any(casan)) return(list(n = 0L, metodo = NA_character_))

  # El metodo declarado es el mas fuerte con el que casa alguna etiqueta: sirve
  # para saber si el cruce fue limpio (exacto) o hubo que aflojar (solape).
  metodo <- if (any(ex)) "exacto" else if (any(cont)) "contencion" else "solape"
  list(n = sum(casan), metodo = metodo)
}

.VACIAS_FACETA <- c("de","del","la","el","los","las","en","y","o","a","al",
                    "un","una","por","para","con","sin","que","su","sus","lo")

## Normaliza una etiqueta de faceta para poder cruzarla: minusculas, sin tildes,
## sin puntuacion y sin espacios repetidos. El LLM devuelve texto libre en la
## columna 'caracteristica' y basta una tilde de diferencia para no casar.
.norm_faceta <- function(x) {
  if (!length(x)) return(character(0))
  y <- tolower(trimws(as.character(x)))
  y <- iconv(y, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- ""
  y <- gsub("[^a-z0-9 ]", " ", y)
  gsub("\\s+", " ", trimws(y))
}

#' @export
print.semilla_cobertura <- function(x, ...) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("COBERTURA DE FACETAS DEL CONSTRUCTO\n")
  cat(strrep("=", 60), "\n", sep = "")
  if (!isTRUE(x$disponible)) {
    cat("  No se pudo auditar: ", x$motivo, "\n", sep = "")
    cat("  OJO: esto NO significa que la cobertura sea correcta.\n")
    cat(strrep("=", 60), "\n", sep = "")
    return(invisible(x))
  }
  cat(sprintf("  Facetas declaradas: %d  |  cubiertas: %d (%.0f%%)\n",
              x$n_declaradas, x$n_cubiertas, 100 * x$prop_cubierta))
  if (nrow(x$huerfanas)) {
    cat("\n  [!] FACETAS SIN ITEMS (la escala dejo de medirlas):\n")
    for (i in seq_len(nrow(x$huerfanas)))
      cat(sprintf("      - %s / %s\n", x$huerfanas$dimension[i], x$huerfanas$faceta[i]))
    cat("\n  Una faceta declarada que se queda en 0 items es contenido perdido:\n")
    cat("  la escala ya no mide lo que su propia definicion promete.\n")
  } else {
    cat("\n  Todas las facetas declaradas siguen cubiertas.\n")
  }
  if (length(x$sin_declarar)) {
    cat("\n  [i] Etiquetas presentes en los items que nadie declaro:\n")
    for (s in x$sin_declarar) cat("      - ", s, "\n", sep = "")
    cat("      (suele pasar cuando un reemplazo invento su propia faceta)\n")
  }
  cat(strrep("=", 60), "\n", sep = "")
  invisible(x)
}

#' @title Comparar la cobertura de facetas entre dos momentos
#'
#' @description
#' Cruza la cobertura de dos escalas (por ejemplo, antes y despues de refinar)
#' y devuelve una tabla con lo que se gano y lo que se perdio, faceta a faceta.
#'
#' @param antes,despues Objetos \code{semilla}.
#' @param min_items Minimo de items para dar una faceta por cubierta.
#'
#' @return data.frame con dimension, faceta, n_antes, n_despues y estado
#'   (\code{"se perdio"}, \code{"nueva"}, \code{"baja"}, \code{"sube"},
#'   \code{"igual"}).
#'
#' @export
comparar_cobertura_facetas <- function(antes, despues, min_items = 1L) {
  a <- auditar_cobertura_facetas(antes,   min_items = min_items, verbose = FALSE)
  d <- auditar_cobertura_facetas(despues, min_items = min_items, verbose = FALSE)
  if (!isTRUE(a$disponible) || !isTRUE(d$disponible)) return(NULL)

  clave <- function(t) paste(.norm_faceta(t$dimension), .norm_faceta(t$faceta), sep = "||")
  ta <- a$tabla; td <- d$tabla
  ta$k <- clave(ta); td$k <- clave(td)
  out <- merge(ta[, c("k", "dimension", "faceta", "n_items")],
               td[, c("k", "n_items")], by = "k", all = TRUE,
               suffixes = c("_antes", "_despues"))
  out$n_items_antes[is.na(out$n_items_antes)]     <- 0L
  out$n_items_despues[is.na(out$n_items_despues)] <- 0L
  out$estado <- with(out, ifelse(
    n_items_antes >= min_items & n_items_despues < min_items, "se perdio",
    ifelse(n_items_antes < min_items & n_items_despues >= min_items, "nueva",
    ifelse(n_items_despues < n_items_antes, "baja",
    ifelse(n_items_despues > n_items_antes, "sube", "igual")))))
  out$k <- NULL
  out[order(match(out$estado, c("se perdio", "baja", "igual", "sube", "nueva")),
            out$dimension), ]
}
