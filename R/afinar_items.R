# =============================================================================
# Afinar items uno por uno (SeMiLLa 2.12.4)
# -----------------------------------------------------------------------------
# Lo que se hizo a mano con scripts en la escala de bienestar psicologico
# academico (tercera version, 02-11-2026) queda como funciones:
#   reemplazar_item()               candidatos -> nivel -> jueces repetidos ->
#                                   embeddings -> filtros -> eleccion
#   validez_contenido_repetida()    varias tandas de jueces: V media y rango
#   registrar_descartados()         lista persistente de redacciones rechazadas
#   auditar_ediciones()             que cambio a mano entre dos versiones
#   variedad_redaccion()            primera palabra y marcos repetidos
#   lector_simulado()               prueba de parafraseo con lectores LLM
#   formulario_entrevista_cognitiva() / leer_entrevista_cognitiva()
# =============================================================================


# ---- Ayudantes ---------------------------------------------------------------

# Definicion breve (sin "No incluye" ni "Se manifiesta en") de cada dimension,
# con nombre = dimension.
#' @keywords internal
.definiciones_dimensiones <- function(x) {
  dims <- unique(as.character(x$items$dimension))
  stats::setNames(vapply(dims, function(d)
    sub("(?i)\\s*(no incluye|se manifiesta en)\\s*:.*$", "",
        .definicion_dimension(x, d), perl = TRUE), ""), dims)
}

#' @keywords internal
.primera_palabra <- function(t) {
  t <- tolower(trimws(as.character(t)))
  sub("(*UCP)^\\W*(\\w+).*$", "\\1", t, perl = TRUE)
}

# Texto sin tildes ni signos, para comparar redacciones.
#' @keywords internal
.normalizar_texto <- function(t) {
  t <- tolower(trimws(as.character(t)))
  t <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00e0\u00e8\u00ec\u00f2\u00f9",
              "aeiouunaeiou", t)
  t <- gsub("[^a-z0-9 ]+", " ", t)
  gsub("\\s+", " ", trimws(t))
}

#' @keywords internal
.items_df <- function(x) {
  if (inherits(x, c("semilla", "semilla_items"))) x <- x$items
  if (is.character(x)) x <- data.frame(item = x, stringsAsFactors = FALSE)
  if (!is.data.frame(x) || !"item" %in% names(x))
    stop("Necesito un objeto semilla o un data.frame con la columna 'item'.", call. = FALSE)
  x$item <- as.character(x$item)
  x
}

# Fila del item a reemplazar: por codigo, por numero o por posicion.
#' @keywords internal
.resolver_item <- function(items, item) {
  if (is.character(item) && "codigo" %in% names(items) && item %in% items$codigo)
    return(match(item, items$codigo))
  if (is.character(item) && item %in% items$item) return(match(item, items$item))
  if (is.numeric(item) && "numero" %in% names(items) && item %in% items$numero)
    return(match(item, items$numero))
  if (is.numeric(item) && item >= 1 && item <= nrow(items)) return(as.integer(item))
  stop("No encuentro el item '", item, "' (por codigo, texto, numero ni posicion).",
       call. = FALSE)
}

# Asignacion por similitud: parecido medio del candidato con los items de su
# dimension frente al de la dimension ajena mas cercana. En la tercera version
# de bienestar academico, los candidatos de Dominio que se parecian mas a
# Autonomia o a Proposito eran justamente los que los jueces confundian.
#' @keywords internal
.asignacion_similitud <- function(S, cand, base, dims_base, dim, codigos_base = NULL) {
  codigos_base <- codigos_base %||% paste0("I", seq_along(base))
  r <- do.call(rbind, lapply(cand, function(i) {
    s <- S[i, base]
    m <- tapply(s, dims_base, mean)
    ajenas <- m[names(m) != dim]
    data.frame(sim_max_escala = round(max(s), 3),
               mas_parecido = codigos_base[which.max(s)],
               sim_propia = if (dim %in% names(m)) round(m[[dim]], 3) else NA_real_,
               sim_ajena = if (length(ajenas)) round(max(ajenas), 3) else NA_real_,
               dim_ajena = if (length(ajenas)) names(ajenas)[which.max(ajenas)] else NA_character_,
               stringsAsFactors = FALSE)
  }))
  r$asignacion_ok <- is.na(r$sim_ajena) | (!is.na(r$sim_propia) & r$sim_propia > r$sim_ajena)
  r
}

# Lista de descartados en cualquier forma: ruta a CSV, vector o data.frame.
#' @keywords internal
.leer_descartados <- function(d) {
  vacio <- data.frame(fecha = character(0), dimension = character(0), item = character(0),
                      motivo = character(0), stringsAsFactors = FALSE)
  if (is.null(d)) return(vacio)
  if (is.character(d) && length(d) == 1L && grepl("\\.csv$", d, ignore.case = TRUE))
    return(if (file.exists(d)) leer_descartados(d) else vacio)
  if (is.character(d))
    return(data.frame(fecha = NA_character_, dimension = NA_character_, item = d,
                      motivo = "", stringsAsFactors = FALSE))
  if (is.data.frame(d) && "item" %in% names(d)) {
    for (k in setdiff(names(vacio), names(d))) d[[k]] <- NA_character_
    return(d[, names(vacio)])
  }
  stop("'descartados' debe ser la ruta a un .csv, un vector de textos o un data.frame con 'item'.",
       call. = FALSE)
}


# ---- Jueces repetidos --------------------------------------------------------

#' @title Validez de contenido con varias tandas de jueces
#'
#' @description
#' Repite \code{\link{validez_contenido}} varias veces con semillas distintas
#' y resume, para cada item, la V de Aiken minima (el criterio mas bajo) como
#' media, peor valor y rango entre tandas. Un item cuya V cambia mucho de una
#' tanda a otra se marca como inestable: su V de una sola corrida no basta
#' para elegirlo ni para descartarlo. Caso: en la escala de bienestar
#' psicologico academico, un mismo item obtuvo V minima de .73 en una tanda y
#' de .53 en otra.
#'
#' @param x Objeto \code{semilla} (o lo que acepte \code{validez_contenido}).
#' @param api_key Clave de la API.
#' @param n_rondas Numero de tandas de jueces (por defecto 3).
#' @param criterios Criterios que se juzgan.
#' @param n_jueces Jueces por tanda.
#' @param umbral_rango Rango de V minima a partir del cual el item se marca
#'   como inestable (por defecto .15).
#' @param seed Semilla de la primera tanda; las siguientes usan seed + 1, ...
#' @param modelo Modelo de los jueces.
#' @param verbose Mostrar el avance.
#'
#' @return Lista de clase \code{semilla_validez_repetida} con \code{resumen}
#'   (un data.frame por item con \code{V_min_media}, \code{V_min_peor},
#'   \code{V_min_rango}, la media de cada criterio e \code{inestable}),
#'   \code{rondas} (la tabla \code{v_aiken} de cada tanda), \code{n_rondas} y
#'   \code{umbral_rango}.
#'
#' @seealso \code{\link{validez_contenido}}, \code{\link{reemplazar_item}}
#' @examples
#' \dontrun{
#' vr <- validez_contenido_repetida(escala, api_key = clave, n_rondas = 3)
#' vr$resumen[vr$resumen$inestable, ]
#' }
#' @export
validez_contenido_repetida <- function(x, api_key, n_rondas = 3L,
                                       criterios = c("relevancia", "representatividad", "claridad"),
                                       n_jueces = 10, umbral_rango = 0.15, seed = 1L,
                                       modelo = "gpt-4.1-mini", verbose = TRUE) {
  n_rondas <- as.integer(n_rondas)
  if (is.na(n_rondas) || n_rondas < 1L) stop("'n_rondas' debe ser 1 o mas.", call. = FALSE)
  old <- options(SeMiLLa.seed = getOption("SeMiLLa.seed"))
  on.exit(options(old), add = TRUE)
  rondas <- vector("list", n_rondas)
  for (r in seq_len(n_rondas)) {
    options(SeMiLLa.seed = as.integer(seed %||% 1L) + r - 1L)
    if (verbose) cat(sprintf("Tanda de jueces %d de %d\n", r, n_rondas))
    v <- validez_contenido(x, api_key = api_key, criterios = criterios, n_jueces = n_jueces,
                           modelo = modelo, verbose = FALSE)
    rondas[[r]] <- v$v_aiken
  }
  .resumir_rondas(rondas, criterios, umbral_rango)
}

#' @keywords internal
.resumir_rondas <- function(rondas, criterios, umbral_rango = 0.15) {
  n <- nrow(rondas[[1]])
  base <- rondas[[1]][, intersect(c("numero", "dimension", "item"), names(rondas[[1]])),
                      drop = FALSE]
  vmin <- matrix(vapply(rondas, function(v) as.numeric(.v_minima(v, criterios)), numeric(n)),
                 nrow = n)
  base$V_min_media <- round(rowMeans(vmin, na.rm = TRUE), 3)
  base$V_min_peor <- round(apply(vmin, 1, min, na.rm = TRUE), 3)
  base$V_min_rango <- round(apply(vmin, 1, function(z) diff(range(z, na.rm = TRUE))), 3)
  for (cr in criterios) {
    col <- paste0("V_", cr)
    if (!col %in% names(rondas[[1]])) next
    m <- matrix(vapply(rondas, function(v) as.numeric(v[[col]]), numeric(n)), nrow = n)
    base[[paste0(col, "_media")]] <- round(rowMeans(m, na.rm = TRUE), 3)
  }
  base$inestable <- length(rondas) > 1L & base$V_min_rango > umbral_rango
  out <- list(resumen = base, rondas = rondas, n_rondas = length(rondas),
              umbral_rango = umbral_rango)
  class(out) <- "semilla_validez_repetida"
  out
}

#' @export
print.semilla_validez_repetida <- function(x, ...) {
  r <- x$resumen
  cat(sprintf("Validez de contenido en %d tandas de jueces (%d items)\n", x$n_rondas, nrow(r)))
  cat(sprintf("  V minima media de la escala: %.3f\n", mean(r$V_min_media, na.rm = TRUE)))
  k <- which(r$inestable)
  cat(sprintf("  Items inestables (rango > %.2f): %d\n", x$umbral_rango, length(k)))
  for (i in k) cat(sprintf("    %s  media %.2f, rango %.2f  %s\n", r$numero[i] %||% i,
                           r$V_min_media[i], r$V_min_rango[i], substr(r$item[i], 1, 70)))
  invisible(x)
}


# ---- Reemplazar un item -------------------------------------------------------

#' @title Reemplazar un item con el flujo completo de SeMiLLa
#'
#' @description
#' Busca un reemplazo para un item concreto y lo elige con los mismos
#' controles que una escala entera: (1) genera candidatos para la dimension
#' del item (y suma los que proponga el autor), (2) pasa cada uno por el
#' control de nivel (conducta o tarea, dependencia de terceros, lo que la
#' dimension no incluye, acto cognitivo o valor de la tarea, y si encaja mejor
#' en otra dimension), (3) los juzga junto con el resto de la escala en varias
#' tandas de jueces, (4) calcula su parecido con cada item y con cada
#' dimension, y descarta los que se parecen mas a otra dimension que a la
#' suya, los casi duplicados y los parecidos a redacciones ya descartadas, y
#' (5) elige el de mayor V minima media entre los que pasan. El item saliente
#' se juzga en la misma tanda como referencia.
#'
#' @param x Objeto \code{semilla}.
#' @param item Item a reemplazar: su \code{codigo}, su texto, su \code{numero}
#'   o su posicion.
#' @param api_key Clave de la API.
#' @param n_candidatos Candidatos que genera SeMiLLa (0 para juzgar solo los
#'   manuales).
#' @param candidatos_manuales Vector de redacciones propuestas por el autor.
#'   Se juzgan igual que las generadas; no se les exige primera palabra nueva.
#' @param faceta Faceta del polo alto que el reemplazo debe cubrir.
#' @param motivo Por que se reemplaza (p. ej., lo que salio de
#'   \code{\link{leer_entrevista_cognitiva}}); se le pasa al generador.
#' @param evitar Palabras o expresiones que los candidatos no deben usar.
#' @param descartados Redacciones ya rechazadas: ruta a un CSV de
#'   \code{\link{registrar_descartados}}, vector o data.frame. Si es una ruta,
#'   al terminar se le agregan el item saliente y los candidatos rechazados por
#'   nivel, por asignacion o por V minima media menor que .60.
#' @param n_rondas Tandas de jueces (ver \code{\link{validez_contenido_repetida}}).
#' @param n_jueces Jueces por tanda.
#' @param criterios Criterios que se juzgan.
#' @param umbral_similitud Parecido maximo con un item que se conserva.
#' @param umbral_descartado Parecido maximo con una redaccion descartada.
#' @param umbral_rango Rango de V a partir del cual un candidato es inestable.
#' @param modelo Modelo para generar, controlar el nivel y juzgar.
#' @param seed Semilla.
#' @param verbose Mostrar el avance.
#'
#' @return Lista de clase \code{semilla_reemplazo} con \code{candidatos}
#'   (todas las columnas de cada control y \code{valido}, \code{elegido} y
#'   \code{motivo_rechazo}), \code{elegido} (la fila elegida o \code{NULL}),
#'   \code{actual} (V del item saliente en la misma tanda), \code{escala} (el
#'   objeto con el item reemplazado, o \code{NULL} si ningun candidato pasa;
#'   sus embeddings y similitudes se borran porque ya no corresponden),
#'   \code{validez} y \code{dimension}.
#'
#' @seealso \code{\link{validez_contenido_repetida}},
#'   \code{\link{registrar_descartados}}, \code{\link{auditar_asignacion}}
#' @examples
#' \dontrun{
#' r <- reemplazar_item(escala, "DOM32", api_key = clave,
#'                      candidatos_manuales = "Logro que mis estudios funcionen para mi.",
#'                      faceta = "sentir que uno puede dar forma a su vida academica",
#'                      descartados = "descartados.csv")
#' r
#' escala <- r$escala
#' }
#' @export
reemplazar_item <- function(x, item, api_key, n_candidatos = 12L, candidatos_manuales = NULL,
                            faceta = NULL, motivo = NULL, evitar = NULL, descartados = NULL,
                            n_rondas = 2L, n_jueces = 10,
                            criterios = c("relevancia", "representatividad", "claridad"),
                            umbral_similitud = 0.75, umbral_descartado = 0.85,
                            umbral_rango = 0.15, modelo = "gpt-4.1-mini", seed = NULL,
                            verbose = TRUE) {
  if (!inherits(x, "semilla")) stop("'x' debe ser un objeto semilla.", call. = FALSE)
  it <- x$items
  idx <- .resolver_item(it, item)
  dim <- as.character(it$dimension[idx])
  saliente <- it$item[idx]
  def <- .definicion_dimension(x, dim)
  otras <- .definiciones_dimensiones(x)
  conserva <- it[-idx, , drop = FALSE]
  cod_base <- if ("codigo" %in% names(conserva)) as.character(conserva$codigo)
              else paste0("I", conserva$numero %||% seq_len(nrow(conserva)))
  desc <- .leer_descartados(descartados)
  desc_dim <- desc[is.na(desc$dimension) | desc$dimension == dim, , drop = FALSE]
  if (!is.null(seed)) {
    old <- options(SeMiLLa.seed = seed); on.exit(options(old), add = TRUE)
  }
  openai <- .configurar_openai(api_key, modelo)

  # 1. Candidatos ---------------------------------------------------------------
  usadas <- unique(.primera_palabra(conserva$item))
  estilo <- x$metadata$instrucciones_estilo
  extra <- paste0(
    if (is.character(estilo) && length(estilo) == 1L && nzchar(estilo))
      paste0("ESTILO DE REDACCION (obligatorio): ", estilo, "\n") else "",
    "Escribe items NUEVOS que reemplacen a: \"", saliente, "\".\n",
    if (length(motivo) && nzchar(paste(motivo, collapse = "")))
      paste0("Por que se reemplaza: ", paste(motivo, collapse = " "), "\n") else "",
    if (length(faceta) && nzchar(faceta))
      paste0("FACETA que debe cubrir el item: ", faceta, "\n") else "",
    "No empieces el item con estas palabras, que ya abren otros items: ",
    paste(usadas, collapse = ", "), ".\n",
    if (length(evitar)) paste0("No uses estas palabras o expresiones: ",
                               paste0("'", evitar, "'", collapse = ", "), ".\n") else "",
    if (nrow(desc_dim)) paste0("Estas redacciones ya se descartaron; no las repitas ni las ",
                               "parafrasees:\n", paste0("- \"", desc_dim$item, "\"", collapse = "\n"),
                               "\n") else "",
    "Mantente en el nivel de la dimension (vivencia, percepcion o capacidad), sin conductas, ",
    "tareas ni cosas que dependan de la institucion, y sin parecerte a las otras dimensiones.\n")
  es_inv <- "inverso" %in% names(it) && isTRUE(as.logical(it$inverso[idx]))
  gen <- if (n_candidatos > 0) tryCatch(.generar_items_dimension(
    openai = openai, concepto = .extraer_concepto_str(x), dimension = dim,
    definicion_dim = def,
    caracteristicas = if (is.null(faceta) && !is.null(x$concepto$caracteristicas))
                        x$concepto$caracteristicas[[dim]] else NULL,
    n_items = as.integer(n_candidatos), idioma = x$metadata$idioma %||% "es",
    poblacion = x$metadata$poblacion %||% "", modelo = modelo,
    items_evitar = unique(c(it$item, desc$item)),
    complejidad_linguistica = x$metadata$complejidad_linguistica %||% "intermedio",
    tipo_escala_respuesta = x$metadata$tipo_escala_respuesta %||% "acuerdo",
    max_palabras = x$metadata$max_palabras %||% 18L,
    incluir_inversos = es_inv, instruccion_extra = extra,
    faceta_objetivo = faceta, items_contexto = conserva$item[conserva$dimension == dim],
    otras_dimensiones = x$concepto$dimensiones,
    items_otras = conserva$item[conserva$dimension != dim]),
    error = function(e) { warning("No se generaron candidatos: ", conditionMessage(e)); NULL })
  cand <- rbind(
    if (length(candidatos_manuales))
      data.frame(origen = "manual", item = trimws(candidatos_manuales), stringsAsFactors = FALSE),
    if (is.data.frame(gen) && nrow(gen))
      data.frame(origen = "semilla", item = trimws(as.character(gen$item)), stringsAsFactors = FALSE))
  if (is.null(cand) || !nrow(cand)) stop("No hay candidatos que juzgar.", call. = FALSE)
  norm <- .normalizar_texto(cand$item)
  repetido <- duplicated(norm) | norm %in% .normalizar_texto(it$item) |
    (norm %in% .normalizar_texto(desc$item) & cand$origen != "manual")
  cand <- cand[!repetido & nzchar(cand$item), , drop = FALSE]
  if (!nrow(cand)) stop("Todos los candidatos repiten items o redacciones descartadas.",
                        call. = FALSE)
  cand$candidato <- paste0("C", seq_len(nrow(cand)))
  if (verbose) cat(sprintf("Reemplazo de \"%s\" (%s): %d candidatos\n", saliente, dim, nrow(cand)))

  # 2. Control de nivel -----------------------------------------------------------
  controlar <- .tipo_dimension(dim, def) != "conductual"
  niv <- lapply(cand$item, function(t)
    if (controlar) .nivel_item_llm(openai, modelo, t, dim, def, otras_dimensiones = otras)
    else list(ok = NA, motivo = "", criterio = NA_character_))
  cand$nivel_ok <- vapply(niv, function(z) if (is.null(z$ok)) NA else as.logical(z$ok), logical(1))
  cand$criterio_nivel <- vapply(niv, function(z) as.character(z$criterio %||% NA_character_), "")
  cand$motivo_nivel <- vapply(niv, function(z) as.character(z$motivo %||% ""), "")

  # 3. Jueces: escala conservada + item saliente + candidatos, en la misma tanda --
  plantilla <- it[rep(idx, nrow(cand) + 1L), , drop = FALSE]
  plantilla$item <- c(saliente, cand$item)
  if ("codigo" %in% names(plantilla)) plantilla$codigo <- c("ACTUAL", cand$candidato)
  if ("caracteristica" %in% names(plantilla) && length(faceta) && nzchar(faceta))
    plantilla$caracteristica[-1] <- faceta
  x2 <- x
  x2$items <- rbind(conserva, plantilla)
  x2$items$numero <- seq_len(nrow(x2$items))
  rownames(x2$items) <- NULL
  x2$similitud <- NULL; x2$embeddings <- NULL
  vr <- validez_contenido_repetida(x2, api_key = api_key, n_rondas = n_rondas,
                                   criterios = criterios, n_jueces = n_jueces,
                                   umbral_rango = umbral_rango, seed = seed %||% 1L,
                                   modelo = modelo, verbose = verbose)
  res <- vr$resumen
  fila_actual <- nrow(conserva) + 1L
  filas_cand <- nrow(conserva) + 1L + seq_len(nrow(cand))
  cols_v <- setdiff(names(res), c("numero", "dimension", "item"))
  cand <- cbind(cand, res[filas_cand, cols_v, drop = FALSE])
  actual <- cbind(data.frame(item = saliente, stringsAsFactors = FALSE),
                  res[fila_actual, cols_v, drop = FALSE])

  # 4. Embeddings: asignacion, casi duplicados y parecido con lo descartado -----
  textos <- c(conserva$item, cand$item, desc_dim$item)
  emb <- obtener_embeddings(items = data.frame(item = textos,
                                               dimension = c(as.character(conserva$dimension),
                                                             rep(dim, nrow(cand) + nrow(desc_dim))),
                                               stringsAsFactors = FALSE),
                            api_key = api_key, verbose = FALSE)
  S <- emb$similitud
  i_base <- seq_len(nrow(conserva))
  i_cand <- nrow(conserva) + seq_len(nrow(cand))
  i_desc <- nrow(conserva) + nrow(cand) + seq_len(nrow(desc_dim))
  cand <- cbind(cand, .asignacion_similitud(S, i_cand, i_base, as.character(conserva$dimension),
                                            dim, cod_base))
  cand$sim_descartado <- if (length(i_desc))
    round(apply(S[i_cand, i_desc, drop = FALSE], 1, max), 3) else NA_real_

  # 5. Filtros y eleccion -------------------------------------------------------
  cand$primera_usada <- .primera_palabra(cand$item) %in% usadas
  out <- .elegir_candidato(cand, umbral_similitud, umbral_descartado)
  cand <- out$candidatos

  elegido <- if (any(cand$elegido)) cand[cand$elegido, , drop = FALSE] else NULL
  escala <- NULL
  if (!is.null(elegido)) {
    escala <- x
    escala$items$item[idx] <- elegido$item
    if ("caracteristica" %in% names(escala$items) && length(faceta) && nzchar(faceta))
      escala$items$caracteristica[idx] <- faceta
    escala$similitud <- NULL; escala$embeddings <- NULL
  }

  # 6. Lista persistente de descartados -----------------------------------------
  if (is.character(descartados) && length(descartados) == 1L &&
      grepl("\\.csv$", descartados, ignore.case = TRUE)) {
    malos <- cand[!cand$elegido & (cand$nivel_ok %in% FALSE | !cand$asignacion_ok |
                                     cand$V_min_media < 0.60), , drop = FALSE]
    if (!is.null(elegido))
      registrar_descartados(descartados, saliente, dim, "item reemplazado")
    if (nrow(malos))
      registrar_descartados(descartados, malos$item, dim, malos$motivo_rechazo)
  }

  r <- list(candidatos = cand, elegido = elegido, actual = actual, escala = escala,
            validez = vr, dimension = dim, saliente = saliente)
  class(r) <- "semilla_reemplazo"
  if (verbose) print(r)
  invisible(r)
}

# Filtros de reemplazar_item() y eleccion. Aparte para poder probarla sin API.
#' @keywords internal
.elegir_candidato <- function(cand, umbral_similitud = 0.75, umbral_descartado = 0.85) {
  motivo <- function(i) {
    m <- c(if (isFALSE(cand$nivel_ok[i]))
             paste0("fuera de nivel", if (nzchar(cand$motivo_nivel[i] %||% ""))
                                        paste0(" (", cand$motivo_nivel[i], ")") else ""),
           if (isTRUE(cand$sim_max_escala[i] > umbral_similitud))
             sprintf("casi duplicado de %s (%.2f)", cand$mas_parecido[i], cand$sim_max_escala[i]),
           if (isFALSE(cand$asignacion_ok[i]))
             sprintf("se parece mas a %s (%.3f) que a su dimension (%.3f)",
                     cand$dim_ajena[i], cand$sim_ajena[i], cand$sim_propia[i]),
           if (isTRUE(cand$sim_descartado[i] > umbral_descartado))
             sprintf("parecido a una redaccion descartada (%.2f)", cand$sim_descartado[i]),
           if (isTRUE(cand$primera_usada[i]) && cand$origen[i] != "manual")
             "primera palabra ya usada")
    paste(m, collapse = "; ")
  }
  cand$motivo_rechazo <- vapply(seq_len(nrow(cand)), motivo, "")
  cand$valido <- !nzchar(cand$motivo_rechazo)
  cand$elegido <- FALSE
  k <- which(cand$valido)
  if (length(k)) {
    est <- if ("inestable" %in% names(cand)) cand$inestable[k] else rep(FALSE, length(k))
    vm <- if ("V_min_peor" %in% names(cand)) cand$V_min_peor[k] else cand$V_min_media[k]
    o <- order(est, -cand$V_min_media[k], -vm, cand$sim_max_escala[k])
    cand$elegido[k[o[1]]] <- TRUE
  }
  cand <- cand[order(-cand$elegido, -cand$valido, -cand$V_min_media), , drop = FALSE]
  rownames(cand) <- NULL
  list(candidatos = cand)
}

#' @export
print.semilla_reemplazo <- function(x, ...) {
  cat(sprintf("\nReemplazo en %s\n  Sale: %s (V minima media %.2f)\n", x$dimension,
              x$saliente, x$actual$V_min_media))
  if (is.null(x$elegido)) {
    cat("  Ningun candidato paso los filtros. Motivos:\n")
  } else {
    cat(sprintf("  Entra: %s (V minima media %.2f, rango %.2f, parecido max %.2f)\n",
                x$elegido$item, x$elegido$V_min_media, x$elegido$V_min_rango %||% NA,
                x$elegido$sim_max_escala))
    if (isTRUE(x$elegido$inestable)) cat("  Ojo: su V cambio mucho entre tandas de jueces.\n")
  }
  cc <- x$candidatos
  for (i in seq_len(min(nrow(cc), 15L)))
    cat(sprintf("  %s%-4s V %.2f  %s%s\n", if (cc$elegido[i]) "*" else " ", cc$candidato[i],
                cc$V_min_media[i], substr(cc$item[i], 1, 70),
                if (nzchar(cc$motivo_rechazo[i])) paste0("  [", cc$motivo_rechazo[i], "]") else ""))
  invisible(x)
}


# ---- Descartados persistentes --------------------------------------------------

#' @title Lista persistente de redacciones descartadas
#'
#' @description
#' Guarda en un CSV las redacciones que ya se rechazaron (por los jueces, por
#' el autor o por el control de nivel) para que \code{\link{reemplazar_item}}
#' no vuelva a proponerlas ni a proponer otras muy parecidas. Caso: en la
#' tercera version de bienestar academico, SeMiLLa volvio a elegir "Manejo con
#' soltura los asuntos cotidianos...", una redaccion ya descartada en una
#' ronda anterior.
#'
#' @param archivo Ruta del CSV (se crea si no existe).
#' @param items Vector de redacciones descartadas.
#' @param dimension Dimension de cada redaccion (uno o tantos como items).
#' @param motivo Motivo del descarte (uno o tantos como items).
#'
#' @return \code{registrar_descartados()} devuelve, de forma invisible, la
#'   lista completa tras agregar las nuevas (sin repetir textos);
#'   \code{leer_descartados()} devuelve esa lista como data.frame con
#'   \code{fecha}, \code{dimension}, \code{item} y \code{motivo}.
#'
#' @examples
#' f <- tempfile(fileext = ".csv")
#' registrar_descartados(f, "Manejo con soltura los asuntos cotidianos.",
#'                       "Dominio del entorno", "volvia a una redaccion ya descartada")
#' leer_descartados(f)
#' unlink(f)
#' @export
registrar_descartados <- function(archivo, items, dimension = NA_character_, motivo = "") {
  items <- as.character(items)
  nuevo <- data.frame(fecha = format(Sys.Date()), dimension = rep_len(as.character(dimension),
                                                                       length(items)),
                      item = items, motivo = rep_len(as.character(motivo), length(items)),
                      stringsAsFactors = FALSE)
  prev <- if (file.exists(archivo)) leer_descartados(archivo) else NULL
  if (!is.null(prev))
    nuevo <- nuevo[!.normalizar_texto(nuevo$item) %in% .normalizar_texto(prev$item), , drop = FALSE]
  nuevo <- nuevo[!duplicated(.normalizar_texto(nuevo$item)), , drop = FALSE]
  todo <- rbind(prev, nuevo)
  utils::write.csv(todo, archivo, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(todo)
}

#' @rdname registrar_descartados
#' @export
leer_descartados <- function(archivo) {
  d <- utils::read.csv(archivo, encoding = "UTF-8", fileEncoding = "UTF-8",
                       stringsAsFactors = FALSE, colClasses = "character")
  for (k in c("fecha", "dimension", "item", "motivo")) if (!k %in% names(d)) d[[k]] <- NA_character_
  d[, c("fecha", "dimension", "item", "motivo")]
}


# ---- Ediciones manuales ----------------------------------------------------------

#' @title Auditar las ediciones manuales entre dos versiones de una escala
#'
#' @description
#' Compara dos versiones y lista los items nuevos, editados y retirados. Un
#' item editado a mano no paso por los controles de SeMiLLa: con
#' \code{api_key} se le aplica el control de nivel y se juzga junto a su
#' version anterior en la misma tanda, para ver si la edicion empeoro su V.
#' Los textos de \code{verificados} (por ejemplo, los que eligio
#' \code{\link{reemplazar_item}}) no se marcan.
#'
#' @param anterior,nueva Objeto \code{semilla}, data.frame con \code{item}
#'   (y, si existe, \code{dimension} y \code{codigo}) o vector de textos.
#' @param api_key Clave de la API. Sin ella solo se listan los cambios.
#' @param verificados Textos que ya pasaron por SeMiLLa.
#' @param x Objeto \code{semilla} de la version nueva para los jueces (si
#'   \code{nueva} no lo es).
#' @param n_jueces,criterios,modelo Ver \code{\link{validez_contenido}}.
#' @param verbose Mostrar el avance.
#'
#' @return Data.frame de clase \code{semilla_ediciones} con \code{cambio}
#'   (\code{"editado"}, \code{"nuevo"} o \code{"retirado"}),
#'   \code{dimension}, \code{texto_anterior}, \code{texto_nuevo},
#'   \code{verificado} y \code{requiere_revision}; con \code{api_key}, ademas
#'   \code{nivel_ok}, \code{motivo_nivel}, \code{V_min_anterior},
#'   \code{V_min_nuevo} y \code{empeora}.
#'
#' @examples
#' a <- data.frame(item = c("Me acepto como estudiante.", "Mis cursos me sirven."),
#'                 dimension = c("Aceptacion", "Proposito"))
#' b <- data.frame(item = c("Me acepto como estudiante.", "Mis cursos tienen sentido para mi."),
#'                 dimension = c("Aceptacion", "Proposito"))
#' auditar_ediciones(a, b)
#' @export
auditar_ediciones <- function(anterior, nueva, api_key = NULL, verificados = NULL, x = NULL,
                              n_jueces = 10,
                              criterios = c("relevancia", "representatividad", "claridad"),
                              modelo = "gpt-4.1-mini", verbose = TRUE) {
  if (is.null(x) && inherits(nueva, "semilla")) x <- nueva
  a <- .items_df(anterior); b <- .items_df(nueva)
  if (!"dimension" %in% names(a)) a$dimension <- NA_character_
  if (!"dimension" %in% names(b)) b$dimension <- NA_character_
  na <- .normalizar_texto(a$item); nb <- .normalizar_texto(b$item)
  solo_b <- which(!nb %in% na); solo_a <- which(!na %in% nb)
  libres <- solo_a
  filas <- lapply(solo_b, function(j) {
    cands <- libres[is.na(a$dimension[libres]) | is.na(b$dimension[j]) |
                      a$dimension[libres] == b$dimension[j]]
    k <- if (length(cands)) {
      d <- as.numeric(utils::adist(nb[j], na[cands])) / pmax(nchar(nb[j]), nchar(na[cands]))
      if (min(d) <= 0.85) cands[which.min(d)] else NA_integer_
    } else NA_integer_
    if (!is.na(k)) libres <<- setdiff(libres, k)
    data.frame(cambio = if (is.na(k)) "nuevo" else "editado",
               codigo = if ("codigo" %in% names(b)) as.character(b$codigo[j]) else NA_character_,
               dimension = as.character(b$dimension[j]),
               texto_anterior = if (is.na(k)) NA_character_ else a$item[k],
               texto_nuevo = b$item[j], stringsAsFactors = FALSE)
  })
  ret <- lapply(libres, function(k)
    data.frame(cambio = "retirado",
               codigo = if ("codigo" %in% names(a)) as.character(a$codigo[k]) else NA_character_,
               dimension = as.character(a$dimension[k]), texto_anterior = a$item[k],
               texto_nuevo = NA_character_, stringsAsFactors = FALSE))
  out <- do.call(rbind, c(filas, ret))
  if (is.null(out)) {
    out <- data.frame(cambio = character(0), codigo = character(0), dimension = character(0),
                      texto_anterior = character(0), texto_nuevo = character(0),
                      stringsAsFactors = FALSE)
  }
  out$verificado <- !is.na(out$texto_nuevo) &
    .normalizar_texto(out$texto_nuevo) %in% .normalizar_texto(verificados %||% character(0))
  out$requiere_revision <- out$cambio != "retirado" & !out$verificado

  if (!is.null(api_key) && any(out$requiere_revision)) {
    if (is.null(x) || !inherits(x, "semilla"))
      stop("Para revisar con jueces hace falta la version nueva como objeto semilla ('x').",
           call. = FALSE)
    openai <- .configurar_openai(api_key, modelo)
    otras <- .definiciones_dimensiones(x)
    rev <- which(out$requiere_revision)
    out$nivel_ok <- NA; out$motivo_nivel <- NA_character_
    out$V_min_anterior <- NA_real_; out$V_min_nuevo <- NA_real_
    for (i in rev) {
      d <- out$dimension[i]
      nv <- .nivel_item_llm(openai, modelo, out$texto_nuevo[i], d, .definicion_dimension(x, d),
                            otras_dimensiones = otras)
      out$nivel_ok[i] <- nv$ok; out$motivo_nivel[i] <- nv$motivo
    }
    # Viejo y nuevo en la misma tanda: el cambio no se mezcla con el ruido de jueces.
    con_ant <- rev[!is.na(out$texto_anterior[rev])]
    x2 <- x
    if (length(con_ant)) {
      j0 <- match(.normalizar_texto(out$texto_nuevo[con_ant]), .normalizar_texto(x$items$item))
      extra <- x$items[j0, , drop = FALSE]
      extra$item <- out$texto_anterior[con_ant]
      x2$items <- rbind(x$items, extra)
    }
    x2$items$numero <- seq_len(nrow(x2$items)); rownames(x2$items) <- NULL
    x2$similitud <- NULL; x2$embeddings <- NULL
    v <- validez_contenido(x2, api_key = api_key, criterios = criterios, n_jueces = n_jueces,
                           modelo = modelo, verbose = verbose)$v_aiken
    vmin <- .v_minima(v, criterios)
    pos_nuevo <- match(.normalizar_texto(out$texto_nuevo[rev]),
                       .normalizar_texto(x2$items$item[seq_len(nrow(x$items))]))
    out$V_min_nuevo[rev] <- round(vmin[pos_nuevo], 3)
    if (length(con_ant))
      out$V_min_anterior[con_ant] <- round(vmin[nrow(x$items) + seq_along(con_ant)], 3)
    out$empeora <- !is.na(out$V_min_anterior) & out$V_min_nuevo < out$V_min_anterior
    out$requiere_revision <- out$requiere_revision &
      (out$nivel_ok %in% FALSE | out$empeora | out$V_min_nuevo < 0.70)
  }
  class(out) <- c("semilla_ediciones", "data.frame")
  out
}

#' @export
print.semilla_ediciones <- function(x, ...) {
  d <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  cat(sprintf("Cambios entre versiones: %d editados, %d nuevos, %d retirados; %d por revisar\n",
              sum(d$cambio == "editado"), sum(d$cambio == "nuevo"), sum(d$cambio == "retirado"),
              sum(d$requiere_revision)))
  for (i in seq_len(nrow(d))) {
    cat(sprintf("  [%s]%s %s\n", d$cambio[i], if (d$requiere_revision[i]) " REVISAR" else "",
                if (d$cambio[i] == "retirado") d$texto_anterior[i] else d$texto_nuevo[i]))
    if (d$cambio[i] == "editado") cat(sprintf("      antes: %s\n", d$texto_anterior[i]))
  }
  invisible(x)
}


# ---- Variedad de redaccion --------------------------------------------------------

#' @title Variedad de redaccion de una escala
#'
#' @description
#' Revisa, sin API, que los items no empiecen todos igual y que no repitan
#' marcos sintacticos ("mas alla de", "a pesar de", "como estudiante"). Los
#' marcos repetidos hacen que los items se parezcan entre si por la forma y no
#' por el contenido, y pueden crear varianza de metodo.
#'
#' @param x Objeto \code{semilla}, data.frame con \code{item} o vector.
#' @param max_misma_primera Veces que se tolera la misma primera palabra.
#' @param marcos Marcos que se vigilan desde la segunda aparicion. Por
#'   defecto, una lista de marcos frecuentes en espanol.
#' @param min_repeticiones Apariciones a partir de las cuales cualquier
#'   secuencia de tres palabras se marca como marco repetido.
#'
#' @return Lista de clase \code{semilla_variedad} con \code{primera} (primeras
#'   palabras que pasan del maximo y cuantas veces), \code{marcos} (marcos
#'   repetidos y en cuantos items) y \code{por_item} (un data.frame con la
#'   primera palabra, los marcos de cada item y \code{alerta}).
#'
#' @examples
#' variedad_redaccion(c("Valoro mis logros, mas alla de mis notas.",
#'                      "Valoro lo que aprendo.",
#'                      "Valoro mis cursos, mas alla de aprobarlos."))
#' @export
variedad_redaccion <- function(x, max_misma_primera = 2L, marcos = NULL, min_repeticiones = 3L) {
  d <- .items_df(x)
  if (is.null(marcos))
    marcos <- c("m\u00e1s all\u00e1 de", "a pesar de", "aunque", "como estudiante",
                "me siento", "soy capaz de", "en la universidad", "cada vez")
  norm <- .normalizar_texto(d$item)
  prim <- .primera_palabra(norm)
  tp <- table(prim)
  primera <- sort(tp[tp > max_misma_primera], decreasing = TRUE)
  mn <- .normalizar_texto(marcos)
  en <- lapply(mn, function(m) grepl(paste0("(^| )", m, "( |$)"), norm))
  cuenta_fija <- vapply(en, sum, numeric(1))
  fijos <- stats::setNames(cuenta_fija, marcos)[cuenta_fija >= 2]
  # Secuencias de tres palabras que se repiten (al menos una palabra de 4+ letras).
  tri <- lapply(strsplit(norm, " "), function(w)
    if (length(w) >= 3) unique(paste(w[-c(length(w) - 1, length(w))], w[-c(1, length(w))],
                                     w[-c(1, 2)])) else character(0))
  tt <- table(unlist(tri))
  tt <- tt[tt >= min_repeticiones]
  tt <- tt[vapply(strsplit(names(tt), " "), function(w) any(nchar(w) >= 4), logical(1))]
  tt <- tt[!names(tt) %in% mn]
  marcos_rep <- c(fijos, stats::setNames(as.numeric(tt), names(tt)))
  por_item <- data.frame(item = d$item, primera = prim,
                         n_primera = as.integer(tp[prim]), stringsAsFactors = FALSE)
  por_item$marcos <- vapply(seq_along(norm), function(i) {
    m <- c(marcos[cuenta_fija >= 2 & vapply(en, `[`, logical(1), i)],
           names(tt)[vapply(names(tt), function(t) t %in% tri[[i]], logical(1))])
    paste(m, collapse = "; ")
  }, "")
  por_item$alerta <- por_item$n_primera > max_misma_primera | nzchar(por_item$marcos)
  out <- list(primera = primera, marcos = marcos_rep, por_item = por_item,
              max_misma_primera = max_misma_primera)
  class(out) <- "semilla_variedad"
  out
}

#' @export
print.semilla_variedad <- function(x, ...) {
  cat(sprintf("Variedad de redaccion (%d items)\n", nrow(x$por_item)))
  if (length(x$primera)) {
    cat(sprintf("  Primeras palabras repetidas (mas de %d veces):\n", x$max_misma_primera))
    for (p in names(x$primera)) cat(sprintf("    %-15s %d\n", p, x$primera[[p]]))
  } else cat("  Ninguna primera palabra se repite de mas.\n")
  if (length(x$marcos)) {
    cat("  Marcos repetidos:\n")
    for (m in names(x$marcos)) cat(sprintf("    %-25s %d items\n", m, x$marcos[[m]]))
  } else cat("  Sin marcos repetidos.\n")
  invisible(x)
}


# ---- Lector simulado -----------------------------------------------------------

#' @title Lector simulado: prueba de parafraseo antes del piloto
#'
#' @description
#' Pide a un LLM que lea cada item como varios lectores de la poblacion
#' (por ejemplo, estudiantes de primer ciclo), que digan si se entiende, que
#' lo expliquen con sus palabras y que senalen la palabra dificil. Otro
#' llamado, a temperatura 0, decide si cada parafraseo conserva el sentido del
#' item. Detecta verbos abstractos y frases que se entienden distinto
#' ("Logro acomodar mis estudios a lo que necesito"). No reemplaza la
#' entrevista cognitiva con personas: sirve para llegar a ella con menos
#' items problematicos.
#'
#' @param x Objeto \code{semilla}, data.frame con \code{item} o vector.
#' @param api_key Clave de la API.
#' @param poblacion Quien lee. Por defecto, la poblacion de \code{x}.
#' @param n_lectores Lectores simulados por item.
#' @param umbral Proporcion minima de lectores que entienden y de
#'   parafraseos fieles.
#' @param modelo Modelo.
#' @param verbose Mostrar el avance.
#'
#' @return Data.frame de clase \code{semilla_lector} con, por item,
#'   \code{prop_entiende}, \code{prop_fiel}, \code{palabras_dificiles},
#'   \code{parafraseos} y \code{alerta}.
#'
#' @examples
#' \dontrun{
#' lector_simulado(escala, api_key = clave, poblacion = "estudiantes de primer ciclo")
#' }
#' @export
lector_simulado <- function(x, api_key, poblacion = NULL, n_lectores = 3L, umbral = 0.67,
                            modelo = "gpt-4.1-mini", verbose = TRUE) {
  d <- .items_df(x)
  poblacion <- poblacion %||% (if (inherits(x, "semilla")) x$metadata$poblacion) %||%
    "personas de la poblacion a la que va dirigida la escala"
  openai <- .configurar_openai(api_key, modelo)
  filas <- lapply(seq_len(nrow(d)), function(i) {
    if (verbose) cat(sprintf("  Lector simulado %d/%d\n", i, nrow(d)))
    lec <- .leer_como_poblacion(openai, modelo, d$item[i], poblacion, n_lectores)
    fiel <- .fidelidad_parafraseo(openai, modelo, d$item[i], lec$parafraseo)
    dif <- unique(lec$dificil[nzchar(lec$dificil)])
    data.frame(item = d$item[i],
               prop_entiende = if (nrow(lec)) round(mean(lec$entiende == "si"), 2) else NA_real_,
               prop_fiel = if (length(fiel)) round(mean(fiel, na.rm = TRUE), 2) else NA_real_,
               palabras_dificiles = paste(dif, collapse = "; "),
               parafraseos = paste(lec$parafraseo, collapse = " | "),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, filas)
  if ("dimension" %in% names(d)) out <- cbind(dimension = d$dimension, out)
  out$alerta <- (out$prop_entiende < umbral) %in% TRUE | (out$prop_fiel < umbral) %in% TRUE
  class(out) <- c("semilla_lector", "data.frame")
  out
}

#' @keywords internal
.leer_como_poblacion <- function(openai, modelo, item, poblacion, n_lectores) {
  prompt <- paste0(
    "Simula a ", n_lectores, " lectores distintos de esta poblacion: ", poblacion, ". ",
    "Cada uno lee una sola vez esta frase de un cuestionario, sin ayuda:\n\"", item, "\"\n",
    "Para cada lector indica: si la entiende (\"si\", \"mas o menos\" o \"no\"), que cree que ",
    "le esta preguntando, dicho con sus propias palabras como se lo explicaria a un companero ",
    "(sin copiar la frase), y la palabra o parte que le resulto dificil (vacio si ninguna). ",
    "Lectores realistas: no todos leen con atencion ni conocen palabras poco usadas.\n",
    "Responde solo con un JSON: {\"lectores\": [{\"entiende\": \"si\", \"parafraseo\": \"...\", ",
    "\"dificil\": \"\"}]}")
  r <- tryCatch(.llamar_openai(openai, list(list(role = "user", content = prompt)),
                               modelo = modelo, temperature = 0.9, max_tokens = 900L,
                               razonamiento = "low"), error = function(e) NULL)
  vacio <- data.frame(entiende = character(0), parafraseo = character(0),
                      dificil = character(0), stringsAsFactors = FALSE)
  if (is.null(r)) return(vacio)
  j <- tryCatch(jsonlite::fromJSON(.extraer_json_llm(r)), error = function(e) NULL)
  l <- j$lectores
  if (!is.data.frame(l) || !nrow(l)) return(vacio)
  for (k in c("entiende", "parafraseo", "dificil")) if (!k %in% names(l)) l[[k]] <- ""
  l$entiende <- .normalizar_texto(l$entiende)
  l$entiende <- ifelse(grepl("^si", l$entiende), "si",
                       ifelse(grepl("^mas", l$entiende), "mas o menos", "no"))
  l$dificil[is.na(l$dificil)] <- ""
  l[, c("entiende", "parafraseo", "dificil")]
}

# TRUE si el parafraseo conserva el sentido del item. Se usa para los lectores
# simulados y para las respuestas reales de la entrevista cognitiva.
#' @keywords internal
.fidelidad_parafraseo <- function(openai, modelo, item, parafraseos) {
  parafraseos <- as.character(parafraseos)
  parafraseos <- parafraseos[!is.na(parafraseos) & nzchar(trimws(parafraseos))]
  if (!length(parafraseos)) return(logical(0))
  prompt <- paste0(
    "Item de un cuestionario: \"", item, "\"\n",
    "Parafraseos de personas que lo leyeron:\n",
    paste0(seq_along(parafraseos), ". ", parafraseos, collapse = "\n"), "\n\n",
    "Para cada parafraseo decide si conserva lo esencial de lo que el item pregunta (fiel) ",
    "o si lo entendio como otra cosa, solo repitio palabras sin explicar, o quedo vacio (no fiel). ",
    "Responde solo con un JSON: {\"fiel\": [true, false, ...]} en el mismo orden.")
  r <- tryCatch(.llamar_openai(openai, list(list(role = "user", content = prompt)),
                               modelo = modelo, temperature = 0, max_tokens = 400L,
                               razonamiento = "low"), error = function(e) NULL)
  if (is.null(r)) return(rep(NA, length(parafraseos)))
  j <- tryCatch(jsonlite::fromJSON(.extraer_json_llm(r)), error = function(e) NULL)
  f <- as.logical(unlist(j$fiel))
  if (length(f) != length(parafraseos)) return(rep(NA, length(parafraseos)))
  f
}

#' @export
print.semilla_lector <- function(x, ...) {
  d <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  cat(sprintf("Lector simulado: %d items, %d con alerta\n", nrow(d), sum(d$alerta)))
  for (i in which(d$alerta))
    cat(sprintf("  entiende %.2f, fiel %.2f  %s%s\n", d$prop_entiende[i], d$prop_fiel[i],
                substr(d$item[i], 1, 70),
                if (nzchar(d$palabras_dificiles[i])) paste0("  [", d$palabras_dificiles[i], "]")
                else ""))
  invisible(x)
}


# ---- Entrevista cognitiva -------------------------------------------------------

#' @title Formulario web de entrevista cognitiva
#'
#' @description
#' Arma un formulario web (Google Apps Script) para una entrevista cognitiva
#' por escrito: para cada item pregunta si se entiende, que le esta
#' preguntando con sus palabras (parafraseo) y, si no se entiende del todo,
#' que parte es dificil. Las respuestas se guardan en la hoja de Google que
#' aloja el script (pestanas "Participantes" y "Respuestas"), sin duplicar
#' envios. Es el formulario usado en la escala de bienestar psicologico
#' academico (2026). Se leen con \code{\link{leer_entrevista_cognitiva}}.
#'
#' @param x Objeto \code{semilla}, data.frame con \code{item} (y,
#'   opcionalmente, \code{codigo} y \code{dimension}) o vector, en el orden en
#'   que se presentan.
#' @param carpeta Carpeta donde se escriben los archivos.
#' @param descripcion Una frase sobre el cuestionario para la bienvenida (por
#'   ejemplo, "Estamos preparando un cuestionario sobre como viven los
#'   estudiantes sus estudios en la universidad.").
#' @param minutos Duracion estimada. Por defecto, unos 0.7 minutos por item.
#' @param clave Clave con la que el navegador guarda el avance; cambiela en
#'   cada ronda para no mezclar respuestas. Por defecto, la carpeta y la fecha.
#' @param sobrescribir Reemplazar archivos existentes.
#'
#' @return De forma invisible, las rutas de \code{Index.html},
#'   \code{Codigo.gs}, \code{items_formulario.csv} y \code{LEEME.txt}.
#'
#' @examples
#' d <- file.path(tempdir(), "ec_demo")
#' items <- c("Me acepto como estudiante.", "Mis cursos tienen sentido.")
#' formulario_entrevista_cognitiva(items, d, "Cuestionario sobre la vida universitaria.")
#' unlink(d, recursive = TRUE)
#' @export
formulario_entrevista_cognitiva <- function(x, carpeta, descripcion, minutos = NULL, clave = NULL,
                                            sobrescribir = FALSE) {
  d <- .items_df(x)
  d$n <- seq_len(nrow(d))
  if (anyDuplicated(.normalizar_texto(d$item))) stop("Hay items repetidos.", call. = FALSE)
  plant <- system.file("plantillas", "entrevista_cognitiva.html", package = "SeMiLLa")
  gs <- system.file("plantillas", "entrevista_cognitiva.gs", package = "SeMiLLa")
  if (!nzchar(plant) || !nzchar(gs)) stop("No encuentro las plantillas del formulario.", call. = FALSE)
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  rutas <- file.path(carpeta, c("Index.html", "Codigo.gs", "items_formulario.csv", "LEEME.txt"))
  if (!sobrescribir && any(file.exists(rutas)))
    stop("Ya hay un formulario en esa carpeta; use sobrescribir = TRUE.", call. = FALSE)
  minutos <- minutos %||% max(5, 5 * ceiling(0.7 * nrow(d) / 5))
  clave <- clave %||% paste0("ec_", gsub("[^a-z0-9]+", "_", .normalizar_texto(basename(carpeta))),
                             "_", format(Sys.Date(), "%Y%m%d"))
  esc_html <- function(t) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", t, fixed = TRUE),
                                                 fixed = TRUE), fixed = TRUE)
  html <- readLines(plant, encoding = "UTF-8", warn = FALSE)
  json <- as.character(jsonlite::toJSON(d[, c("n", "item")], auto_unbox = TRUE))
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  html <- sub("/*__ITEMS__*/[]", json, html, fixed = TRUE)
  html <- gsub("__CLAVE__", clave, html, fixed = TRUE)
  html <- gsub("__DESCRIPCION__", esc_html(descripcion), html, fixed = TRUE)
  html <- gsub("__MINUTOS__", as.character(minutos), html, fixed = TRUE)
  .escribir_utf8(html, rutas[1])
  .escribir_utf8(readLines(gs, encoding = "UTF-8", warn = FALSE), rutas[2])
  tabla <- d[, intersect(c("n", "codigo", "dimension", "item"), names(d))]
  utils::write.csv(tabla, rutas[3], row.names = FALSE, fileEncoding = "UTF-8")
  .escribir_utf8(c(
    "Como publicar el formulario de entrevista cognitiva",
    "1. Cree una hoja de Google nueva (sheets.new): ahi llegaran las respuestas.",
    "2. Extensiones > Apps Script. Pegue Codigo.gs en el archivo de codigo.",
    "3. Archivo nuevo de tipo HTML llamado Index; pegue el contenido de Index.html.",
    "4. Implementar > Nueva implementacion > Aplicacion web; ejecutar como usted y",
    "   acceso para cualquier usuario. Copie la URL /exec y compartala.",
    "5. Para cambiar los items, vuelva a correr formulario_entrevista_cognitiva(),",
    "   pegue el Index.html nuevo e implemente una version nueva (misma URL).",
    "6. Descargue la hoja como .xlsx y lea los resultados con leer_entrevista_cognitiva().",
    paste0("Clave del avance en el navegador: ", clave)), rutas[4])
  invisible(rutas)
}

#' @keywords internal
.escribir_utf8 <- function(lineas, ruta) {
  con <- file(ruta, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(lineas, con)
}

#' @title Leer los resultados de la entrevista cognitiva
#'
#' @description
#' Resume, por item, las respuestas del formulario de
#' \code{\link{formulario_entrevista_cognitiva}}: proporcion de personas que
#' lo entendieron, partes senaladas como dificiles y parafraseos. Con
#' \code{api_key} juzga ademas si cada parafraseo conserva el sentido del
#' item. Marca para revisar los items con baja claridad o baja fidelidad y
#' redacta un \code{motivo} que puede pasarse a \code{\link{reemplazar_item}}
#' en la ronda siguiente.
#'
#' @param respuestas Ruta a la hoja descargada (.xlsx con la pestana
#'   "Respuestas", o .csv) o data.frame con \code{n}, \code{item},
#'   \code{claridad}, \code{parafraseo} y \code{confuso}.
#' @param items Data.frame opcional con \code{n} (o \code{item}) y
#'   \code{codigo}/\code{dimension}, para unirlos al resumen (por ejemplo,
#'   \code{items_formulario.csv}).
#' @param umbral_claridad Proporcion minima de "Si, se entiende".
#' @param umbral_fidelidad Proporcion minima de parafraseos fieles.
#' @param api_key Clave de la API (opcional).
#' @param modelo Modelo para juzgar los parafraseos.
#'
#' @return Data.frame de clase \code{semilla_entrevista} con, por item,
#'   \code{n_respuestas}, \code{prop_clara}, \code{prop_mas_menos},
#'   \code{prop_no}, \code{confusos}, \code{parafraseos}, \code{prop_fiel}
#'   (con \code{api_key}), \code{revisar} y \code{motivo}.
#'
#' @examples
#' r <- data.frame(n = c(1, 1, 2, 2), item = rep(c("Me acepto.", "Logro acomodar mis estudios."),
#'                 each = 2),
#'                 claridad = c("Si, se entiende", "Si, se entiende",
#'                              "Mas o menos", "No se entiende"),
#'                 parafraseo = c("Si me quiero", "Si me acepto", "?", "No se"),
#'                 confuso = c("", "", "acomodar", "acomodar"))
#' leer_entrevista_cognitiva(r)
#' @export
leer_entrevista_cognitiva <- function(respuestas, items = NULL, umbral_claridad = 0.80,
                                      umbral_fidelidad = 0.67, api_key = NULL,
                                      modelo = "gpt-4.1-mini") {
  r <- if (is.data.frame(respuestas)) respuestas else if (grepl("\\.xlsx?$", respuestas,
                                                                ignore.case = TRUE)) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      stop("Para leer .xlsx instale openxlsx.", call. = FALSE)
    openxlsx::read.xlsx(respuestas, sheet = "Respuestas")
  } else utils::read.csv(respuestas, encoding = "UTF-8", fileEncoding = "UTF-8",
                         stringsAsFactors = FALSE)
  faltan <- setdiff(c("n", "item", "claridad", "parafraseo"), names(r))
  if (length(faltan)) stop("Faltan columnas: ", paste(faltan, collapse = ", "), call. = FALSE)
  if (!"confuso" %in% names(r)) r$confuso <- ""
  cl <- .normalizar_texto(r$claridad)
  r$nivel <- ifelse(grepl("^si", cl), "clara", ifelse(grepl("^mas", cl), "mas_menos", "no"))
  openai <- if (!is.null(api_key)) .configurar_openai(api_key, modelo)
  ns <- sort(unique(r$n))
  out <- do.call(rbind, lapply(ns, function(k) {
    s <- r[r$n == k, , drop = FALSE]
    conf <- unique(trimws(as.character(s$confuso)))
    conf <- conf[!is.na(conf) & nzchar(conf)]
    par <- as.character(s$parafraseo); par <- par[!is.na(par) & nzchar(trimws(par))]
    fiel <- if (!is.null(openai)) .fidelidad_parafraseo(openai, modelo, s$item[1], par)
    data.frame(n = k, item = s$item[1], n_respuestas = nrow(s),
               prop_clara = round(mean(s$nivel == "clara"), 2),
               prop_mas_menos = round(mean(s$nivel == "mas_menos"), 2),
               prop_no = round(mean(s$nivel == "no"), 2),
               confusos = paste(conf, collapse = " | "),
               parafraseos = paste(par, collapse = " | "),
               prop_fiel = if (length(fiel)) round(mean(fiel, na.rm = TRUE), 2) else NA_real_,
               stringsAsFactors = FALSE)
  }))
  if (!is.null(items)) {
    clave_u <- if ("n" %in% names(items)) "n" else "item"
    extra <- items[, c(clave_u, intersect(c("codigo", "dimension"), names(items))), drop = FALSE]
    out <- merge(extra, out, by = clave_u, all.y = TRUE, sort = FALSE)
    out <- out[order(out$n), , drop = FALSE]
  }
  out$revisar <- out$prop_clara < umbral_claridad | (out$prop_fiel < umbral_fidelidad) %in% TRUE
  out$motivo <- vapply(seq_len(nrow(out)), function(i) {
    if (!out$revisar[i]) return("")
    paste(c(sprintf("en la entrevista cognitiva solo %.0f%% lo entendio con claridad",
                    100 * out$prop_clara[i]),
            if (nzchar(out$confusos[i])) paste0("partes dificiles: ", out$confusos[i]),
            if (isTRUE(out$prop_fiel[i] < umbral_fidelidad))
              sprintf("solo %.0f%% de los parafraseos conserva su sentido", 100 * out$prop_fiel[i])),
          collapse = "; ")
  }, "")
  rownames(out) <- NULL
  class(out) <- c("semilla_entrevista", "data.frame")
  out
}

#' @export
print.semilla_entrevista <- function(x, ...) {
  d <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  cat(sprintf("Entrevista cognitiva: %d items, hasta %d respuestas por item, %d por revisar\n",
              nrow(d), max(d$n_respuestas), sum(d$revisar)))
  for (i in which(d$revisar))
    cat(sprintf("  %2d. clara %.2f  %s\n      %s\n", d$n[i], d$prop_clara[i],
                substr(d$item[i], 1, 70), d$motivo[i]))
  invisible(x)
}
