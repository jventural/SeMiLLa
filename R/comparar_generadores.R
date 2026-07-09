# =============================================================================
# SeMiLLa - Benchmark de modelos LLM como GENERADORES de items
# =============================================================================
#
# Responde con datos la pregunta "que modelo genera mejores items?" cada vez
# que aparece un modelo nuevo, en lugar de asumir que mas nuevo = mejor.
# Diseno pensado para ser reportable en un articulo: mismas instrucciones y
# constructo para todos los modelos, metricas objetivas independientes del
# juicio humano, y doble juez LLM CIEGO y CRUZADO (cada juez evalua todos
# los conjuntos sin saber que modelo los escribio, y los jueces pertenecen
# a familias distintas para neutralizar el sesgo de auto-preferencia).

#' @title Comparar modelos LLM como generadores de items
#'
#' @description
#' Genera el mismo conjunto de items (mismo constructo, dimensiones, reglas
#' de redaccion y poblacion) con varios modelos LLM y los compara con dos
#' familias de criterios:
#'
#' \strong{Metricas objetivas} (independientes de jueces):
#' \itemize{
#'   \item \code{separabilidad}: similitud media intra-dimension menos
#'         inter-dimension en embeddings (mayor = las dimensiones se
#'         distinguen semanticamente).
#'   \item \code{clasif_loo}: proporcion de items mas cercanos al centroide
#'         de su propia dimension que al de la ajena (leave-one-out).
#'   \item \code{pares} y \code{facetas}: redundancia segun
#'         \code{\link{auditar_redundancia}} (umbral adaptativo).
#'   \item \code{muletillas}: formulas literales repetidas en 3+ items.
#'   \item Longitud (mediana, maximo y items que exceden el tope) y tiempo
#'         de generacion.
#' }
#'
#' \strong{Doble juez LLM ciego y cruzado}: cada juez (por defecto uno de la
#' familia GPT-4.1 y otro de la GPT-5) califica TODOS los conjuntos en orden
#' aleatorio, sin conocer el modelo autor, en claridad, especificidad
#' conductual, diversidad y naturalidad (1-10). Usar jueces de familias
#' distintas mitiga el sesgo de auto-preferencia documentado en jueces LLM.
#'
#' @param concepto Nombre del constructo (cadena).
#' @param dimensiones Lista nombrada \code{list(nombre = definicion)} con 2+
#'   dimensiones. Se recomienda que las definiciones describan conductas
#'   distinguibles: la separabilidad se mide entre ellas.
#' @param api_key Clave del proveedor LLM.
#' @param modelos Vector de modelos a comparar (default:
#'   \code{c("gpt-4.1-mini", "gpt-5-nano", "gpt-5-mini")}).
#' @param n_items_por_dimension Items a generar por dimension (default 6).
#' @param poblacion Poblacion objetivo (se inyecta al prompt de todos los
#'   modelos por igual).
#' @param idioma "es", "en" o "pt".
#' @param jueces Vector de 1+ modelos juez (default
#'   \code{c("gpt-4.1-mini", "gpt-5-mini")}). Se recomienda mantener jueces
#'   de familias distintas a las comparadas o, al menos, cruzadas.
#' @param max_palabras Tope de palabras por item (misma regla para todos).
#' @param modelo_embeddings Modelo de embeddings para las metricas
#'   semanticas (default \code{"text-embedding-3-small"}).
#' @param seed Semilla (baraja el orden en que los jueces ven los conjuntos).
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_benchmark_generadores} con:
#' \itemize{
#'   \item \code{tabla}: data.frame comparativo (una fila por modelo).
#'   \item \code{items}: lista de data.frames con los items generados por
#'         cada modelo (para inspeccion cualitativa y material suplementario).
#'   \item \code{parametros}: condiciones del experimento (reproducibilidad).
#' }
#'
#' @examples
#' \dontrun{
#' bm <- comparar_generadores(
#'   concepto = "autorregulacion del aprendizaje",
#'   dimensiones = list(
#'     "Planificacion del estudio" = "Conductas de organizar tiempo y tareas...",
#'     "Manejo de distracciones digitales" = "Conductas de controlar el celular..."
#'   ),
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   modelos = c("gpt-4.1-mini", "gpt-5-nano", "gpt-5-mini", "gpt-5.4-mini")
#' )
#' bm$tabla
#' bm$items[["gpt-5-nano"]]
#' }
#'
#' @seealso \code{\link{auditar_redundancia}}, \code{\link{generar_items}}
#'
#' @export
comparar_generadores <- function(concepto,
                                 dimensiones,
                                 api_key,
                                 modelos = c("gpt-4.1-mini", "gpt-5-nano",
                                             "gpt-5-mini"),
                                 n_items_por_dimension = 6,
                                 poblacion = NULL,
                                 idioma = "es",
                                 jueces = c("gpt-4.1-mini", "gpt-5-mini"),
                                 max_palabras = 14L,
                                 modelo_embeddings = "text-embedding-3-small",
                                 seed = 2026,
                                 verbose = TRUE) {

  if (!is.list(dimensiones) || length(dimensiones) < 2 ||
      is.null(names(dimensiones)))
    stop("'dimensiones' debe ser una lista NOMBRADA con al menos 2 dimensiones.")
  if (length(modelos) < 2)
    stop("Se necesitan al menos 2 modelos para comparar.")
  if (!is.null(seed)) set.seed(seed)

  openai <- .configurar_openai(api_key)
  resultados <- list()
  items_por_modelo <- list()

  # --- 1. Generacion: mismas instrucciones para todos ------------------------
  for (m in modelos) {
    if (verbose) cat("\n[comparar_generadores] Generando con ", m, "...\n", sep = "")
    t0 <- Sys.time()
    items_m <- data.frame()
    for (d in names(dimensiones)) {
      df <- tryCatch(.generar_items_dimension(
        openai = openai, concepto = concepto, dimension = d,
        definicion_dim = dimensiones[[d]], caracteristicas = NULL,
        n_items = n_items_por_dimension, idioma = idioma,
        poblacion = poblacion, modelo = m,
        incluir_inversos = FALSE, max_palabras = max_palabras),
        error = function(e) {
          if (verbose) cat("  ", .color_warning(), " ", m, " fallo en '", d,
                           "': ", conditionMessage(e), "\n", sep = "")
          NULL
        })
      if (!is.null(df) && nrow(df) > 0) {
        items_m <- rbind(items_m,
                         data.frame(dimension = d, item = trimws(df$item),
                                    stringsAsFactors = FALSE))
      }
    }
    resultados[[m]] <- list(
      n = nrow(items_m),
      seg = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    )
    items_por_modelo[[m]] <- items_m
    if (verbose) cat("  ", nrow(items_m), " items en ",
                     round(resultados[[m]]$seg), " seg\n", sep = "")
  }

  n_esperado <- n_items_por_dimension * length(dimensiones)

  # --- 2. Metricas objetivas --------------------------------------------------
  .emb <- function(textos) {
    resp <- openai$embeddings$create(model = modelo_embeddings,
                                     input = as.list(textos))
    E <- do.call(rbind, lapply(resp$data, function(x) as.numeric(x$embedding)))
    E / sqrt(rowSums(E^2))
  }

  for (m in modelos) {
    it <- items_por_modelo[[m]]
    r <- resultados[[m]]
    r$valido <- !is.null(it) && nrow(it) >= 0.7 * n_esperado &&
      length(unique(it$dimension)) == length(dimensiones)
    if (!r$valido) { resultados[[m]] <- r; next }

    np <- vapply(strsplit(it$item, "\\s+"), length, integer(1))
    r$palabras_med <- stats::median(np)
    r$sobre_tope   <- sum(np > max_palabras)
    r$muletillas   <- nrow(.detectar_muletillas(it$item, 3L))

    E <- .emb(it$item)
    S <- E %*% t(E)
    mismo <- outer(it$dimension, it$dimension, "==")
    ut <- upper.tri(S)
    r$sim_intra <- mean(S[ut & mismo])
    r$sim_inter <- mean(S[ut & !mismo])
    r$separabilidad <- r$sim_intra - r$sim_inter

    r$clasif_loo <- mean(vapply(seq_len(nrow(it)), function(i) {
      propios <- it$dimension == it$dimension[i] & seq_len(nrow(it)) != i
      ajenos  <- it$dimension != it$dimension[i]
      if (!any(propios) || !any(ajenos)) return(NA)
      (colMeans(E[propios, , drop = FALSE]) %*% E[i, ]) >
        (colMeans(E[ajenos, , drop = FALSE]) %*% E[i, ])
    }, logical(1)), na.rm = TRUE)

    x <- structure(list(
      similitud = S,
      items = data.frame(codigo = paste0("I", seq_len(nrow(it))),
                         item = it$item, dimension = it$dimension,
                         stringsAsFactors = FALSE)),
      class = c("semilla_embeddings", "list"))
    aud <- tryCatch(suppressWarnings(auditar_redundancia(x)),
                    error = function(e) NULL)
    r$pares   <- if (!is.null(aud)) nrow(aud$pares_redundantes) else NA_integer_
    r$facetas <- if (!is.null(aud)) nrow(aud$facetas_repetidas) else NA_integer_
    resultados[[m]] <- r
  }

  # --- 3. Jueces LLM ciegos y cruzados ---------------------------------------
  .juzgar <- function(items_txt, juez) {
    prompt <- paste0(
      "Eres experto en construccion de escalas psicometricas. Evalua este ",
      "conjunto de ", length(items_txt), " items (constructo: ", concepto,
      "; dimensiones: ", paste(names(dimensiones), collapse = " / "), ").\n\n",
      paste(sprintf("%d. %s", seq_along(items_txt), items_txt), collapse = "\n"),
      "\n\nCALIFICA EL CONJUNTO (no cada item) de 1 a 10 en:\n",
      "- claridad: lenguaje simple, una idea por item\n",
      "- especificidad: conductas observables y situadas, no vaguedades\n",
      "- diversidad: cada item cubre una manifestacion DISTINTA (penaliza parafraseo)\n",
      "- naturalidad: suena a lenguaje cotidiano de la poblacion, no a plantilla\n",
      "Devuelve SOLO JSON: {\"claridad\": n, \"especificidad\": n, ",
      "\"diversidad\": n, \"naturalidad\": n}")
    raw <- tryCatch(.llamar_openai(openai,
      messages = list(list(role = "user", content = prompt)),
      modelo = juez, max_tokens = 300L, temperature = 0,
      razonamiento = "low"), error = function(e) NULL)
    if (is.null(raw)) return(NA_real_)
    p <- tryCatch(jsonlite::fromJSON(.limpiar_json(raw)), error = function(e) NULL)
    if (is.null(p)) return(NA_real_)
    mean(c(p$claridad, p$especificidad, p$diversidad, p$naturalidad))
  }

  if (length(jueces) > 0) {
    if (verbose) cat("\n[comparar_generadores] Jueces ciegos: ",
                     paste(jueces, collapse = ", "), "\n", sep = "")
    for (m in sample(modelos)) {           # orden aleatorio: ciego al autor
      if (!isTRUE(resultados[[m]]$valido)) next
      notas <- vapply(jueces, function(j)
        .juzgar(items_por_modelo[[m]]$item, j), numeric(1))
      resultados[[m]]$notas_jueces <- stats::setNames(notas, jueces)
      resultados[[m]]$juez_prom <- mean(notas, na.rm = TRUE)
      if (verbose) cat("  ", m, ": ",
                       paste(sprintf("%s=%.2f", jueces, notas), collapse = " | "),
                       "\n", sep = "")
    }
  }

  # --- 4. Tabla final ---------------------------------------------------------
  tabla <- do.call(rbind, lapply(modelos, function(m) {
    r <- resultados[[m]]
    if (!isTRUE(r$valido)) {
      return(data.frame(modelo = m, n_items = r$n %||% 0L, valido = FALSE,
                        seg = NA, palabras_med = NA, sobre_tope = NA,
                        muletillas = NA, separabilidad = NA, clasif_loo = NA,
                        pares = NA, facetas = NA, jueces = NA,
                        stringsAsFactors = FALSE))
    }
    data.frame(modelo = m, n_items = r$n, valido = TRUE,
               seg = round(r$seg), palabras_med = r$palabras_med,
               sobre_tope = r$sobre_tope, muletillas = r$muletillas,
               separabilidad = round(r$separabilidad, 3),
               clasif_loo = round(r$clasif_loo, 2),
               pares = r$pares, facetas = r$facetas,
               jueces = round(r$juez_prom %||% NA_real_, 2),
               stringsAsFactors = FALSE)
  }))

  out <- list(
    tabla = tabla,
    items = items_por_modelo,
    detalles = resultados,
    parametros = list(concepto = concepto, dimensiones = dimensiones,
                      modelos = modelos, jueces = jueces,
                      n_items_por_dimension = n_items_por_dimension,
                      poblacion = poblacion, idioma = idioma,
                      max_palabras = max_palabras,
                      modelo_embeddings = modelo_embeddings,
                      seed = seed, fecha = format(Sys.Date()))
  )
  class(out) <- c("semilla_benchmark_generadores", "list")
  if (verbose) print(out)
  invisible(out)
}


#' @export
print.semilla_benchmark_generadores <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Benchmark de generadores LLM (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Constructo : ", x$parametros$concepto, "\n", sep = "")
  cat("  Dimensiones: ", paste(names(x$parametros$dimensiones),
                               collapse = " / "), "\n", sep = "")
  cat("  Jueces     : ", paste(x$parametros$jueces, collapse = ", "),
      " (ciegos, orden aleatorio)\n", sep = "")
  cat("-----------------------------------------------------------\n")
  print(x$tabla, row.names = FALSE)
  cat("-----------------------------------------------------------\n")
  cat("  separabilidad = sim intra - inter (mayor mejor) | clasif_loo =\n")
  cat("  clasificacion leave-one-out (mayor mejor) | pares/facetas/\n")
  cat("  muletillas/sobre_tope (menor mejor). Items en x$items[[modelo]].\n")
  cat("===========================================================\n\n")
  invisible(x)
}
