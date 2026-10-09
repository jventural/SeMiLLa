# =============================================================================
# SeMiLLa - Probabilidad de que cada item pertenezca a su dimension
# =============================================================================
#
# Dos lectores de texto independientes clasifican cada item entre las
# dimensiones y dan la probabilidad de cada una: Jev (modelo de decision) y el
# modelo de juicio (Claude Haiku 5.5 con una clave de OpenRouter). La
# probabilidad de pertenencia es el promedio de los dos.
#
# El consenso del clustering de embeddings NO se promedia: se conserva como
# SEGUNDA LECTURA. Mide otra cosa (la cercania del significado entre items) y
# no depende de un modelo de lenguaje, asi que sirve para marcar los items en
# que las dos lecturas discrepan, como las celdas en ambar de ScanQuiz.
#
# Medido el 09-10-2026 en 9 escalas reales (306 items, n = 1500 por escala;
# verdad = el item queda, en los datos, con los de su dimension teorica):
#   AUC por escala  clustering 0,61 · Jev 0,71 · Haiku 0,74 · Jev+Haiku 0,77
#   Brier           clustering 0,204 · Jev+Haiku 0,125
#   sumar el clustering al promedio NO mejoraba (0,69).
# Ganancia de Jev+Haiku sobre el clustering: +0,20 de AUC (IC 95 % 0,08-0,32).
# =============================================================================

#' @title Probabilidad de que cada item pertenezca a su dimension
#'
#' @description
#' Dos lectores de texto clasifican cada item entre las dimensiones de la escala
#' y devuelven la probabilidad de cada una: Jev (modelo de decision de
#' OpenRouter) y el modelo de juicio (Claude Haiku 5.5 con una clave de
#' OpenRouter; con una clave de OpenAI, el modelo que se pase en \code{modelo}).
#' La probabilidad de pertenencia es el promedio de los lectores disponibles.
#'
#' Si se pasa el resultado de \code{precision_clasificacion(metodo =
#' "ensemble")}, su consenso se agrega como segunda lectura y se marcan los
#' items en que las dos lecturas no coinciden (\code{discrepancia}). El
#' consenso no entra en el promedio: medido en 9 escalas reales, sumarlo
#' empeoraba la prediccion.
#'
#' @param x Objeto \code{semilla} o \code{semilla_items} con \code{$items}
#'   (columnas \code{item} y \code{dimension}). Si trae
#'   \code{$concepto$dimensiones}, las definiciones se dan a los lectores.
#' @param api_key Clave de la API. Jev solo esta disponible con una clave de
#'   OpenRouter (\code{"sk-or-..."}).
#' @param ensemble Opcional: resultado de \code{precision_clasificacion()} con
#'   \code{metodo = "ensemble"}, para la segunda lectura.
#' @param modelo Modelo de chat del lector de juicio cuando no se usa
#'   OpenRouter (con OpenRouter se usa el modelo de juicio de la sesion).
#' @param umbral Probabilidad minima para considerar que el item pertenece.
#' @param umbral_consenso Consenso minimo del clustering para la segunda
#'   lectura.
#' @param verbose Si \code{TRUE}, informa el avance.
#'
#' @return Un \code{data.frame} con una fila por item y las columnas
#'   \code{codigo}, \code{item}, \code{dimension}, \code{prob_pertenencia},
#'   \code{prob_jev}, \code{prob_juez}, \code{dimension_probable},
#'   \code{pertenece}, \code{consenso} y \code{discrepancia}. Los atributos
#'   \code{probabilidades} (matriz item x dimension), \code{lectores} y
#'   \code{costo_jev} documentan el calculo.
#'
#' @examples
#' \dontrun{
#' esc <- generar_escala("Autoeficacia academica", api_key = Sys.getenv("OPENROUTER_API_KEY"))
#' pp <- probabilidad_pertenencia(esc, api_key = Sys.getenv("OPENROUTER_API_KEY"))
#' pp[pp$discrepancia, ]
#' }
#' @export
probabilidad_pertenencia <- function(x, api_key, ensemble = NULL, modelo = "gpt-4.1-mini",
                                     umbral = 0.5, umbral_consenso = 0.667,
                                     verbose = TRUE) {
  items <- x$items
  if (is.null(items) || is.null(items$item) || is.null(items$dimension))
    stop("x debe traer $items con las columnas 'item' y 'dimension'.")
  dims <- unique(as.character(items$dimension))
  p <- nrow(items)
  codigo <- as.character(items$codigo %||% paste0("Item_", seq_len(p)))
  if (length(dims) < 2L) {
    return(data.frame(codigo = codigo, item = items$item, dimension = items$dimension,
                      prob_pertenencia = 1, prob_jev = NA_real_, prob_juez = NA_real_,
                      dimension_probable = items$dimension, pertenece = TRUE,
                      consenso = NA_real_, discrepancia = FALSE, stringsAsFactors = FALSE))
  }
  defs <- x$concepto$dimensiones
  descr <- vapply(dims, function(d) {
    def <- if (is.list(defs)) defs[[d]] else NULL
    nombre <- gsub("_", " ", d)
    if (is.character(def) && nzchar(def)) paste0(nombre, ": ", def) else nombre
  }, character(1))

  lectores <- list()
  costo_jev <- 0

  # --- Lector 1: Jev (solo con clave de OpenRouter) ---------------------------
  if (.decision_disponible(api_key)) {
    if (isTRUE(verbose)) message("  [pertenencia] Jev clasifica ", p, " items...")
    criterios <- as.list(stats::setNames(paste("El item mide", descr), dims))
    preg <- stats::setNames(lapply(seq_len(p), function(i) list(
      type = "choice", criteria = criterios,
      instructions = paste0("Cual de estas dimensiones del constructo mide este item de cuestionario? Item: \"",
                            items$item[i], "\""))), paste0("i", seq_len(p)))
    resp <- .llamar_decisiones(preg, list(task = "Asignar items de cuestionario a la dimension que miden",
                                          dimensiones = as.list(dims)), api_key)
    if (!is.null(resp)) {
      costo_jev <- attr(resp, "costo") %||% 0
      M <- t(vapply(paste0("i", seq_len(p)), function(q)
        if (is.null(resp[[q]])) rep(NA_real_, length(dims)) else .probs_decision(resp[[q]], dims),
        numeric(length(dims))))
      lectores$jev <- M
    }
  }

  # --- Lector 2: el modelo de juicio --------------------------------------------
  if (isTRUE(verbose)) message("  [pertenencia] el modelo de juicio clasifica ", p, " items...")
  M2 <- tryCatch(.pertenencia_chat(items$item, dims, descr, api_key, modelo), error = function(e) NULL)
  if (!is.null(M2)) lectores$juez <- M2

  if (!length(lectores)) stop("Ningun lector pudo clasificar los items (revisa la clave de la API).")
  Pm <- Reduce(`+`, lapply(lectores, function(M) { M[is.na(M)] <- 0; M })) / length(lectores)
  for (M in lectores) Pm[is.na(M)] <- NA
  colnames(Pm) <- dims
  rownames(Pm) <- codigo
  idx <- cbind(seq_len(p), match(as.character(items$dimension), dims))
  propia <- function(M) if (is.null(M)) rep(NA_real_, p) else M[idx]

  res <- data.frame(
    codigo = codigo, item = items$item, dimension = items$dimension,
    prob_pertenencia = round(Pm[idx], 3),
    prob_jev = round(propia(lectores$jev), 3),
    prob_juez = round(propia(lectores$juez), 3),
    dimension_probable = dims[max.col(replace(Pm, is.na(Pm), -1), ties.method = "first")],
    stringsAsFactors = FALSE)
  res$pertenece <- res$prob_pertenencia >= umbral

  # --- Segunda lectura: el consenso del clustering -----------------------------
  res$consenso <- NA_real_
  cons <- ensemble$consenso
  if (!is.null(cons) && !is.null(cons$Consenso)) {
    k <- if (!is.null(cons$Codigo)) match(codigo, as.character(cons$Codigo)) else seq_len(p)
    res$consenso <- cons$Consenso[k]
  }
  res$discrepancia <- !is.na(res$consenso) & !is.na(res$prob_pertenencia) &
    (res$pertenece != (res$consenso >= umbral_consenso))

  attr(res, "probabilidades") <- Pm
  attr(res, "lectores") <- names(lectores)
  attr(res, "costo_jev") <- costo_jev
  class(res) <- c("semilla_pertenencia", "data.frame")
  res
}

# El lector de chat: una sola llamada con todos los items numerados; pide la
# probabilidad de cada dimension. Los modelos suelen devolver abreviaturas o
# nombres sin parentesis, asi que el emparejamiento es tolerante.
#' @keywords internal
#' @noRd
.pertenencia_chat <- function(textos, dims, descr, api_key, modelo) {
  openai <- .configurar_openai(api_key, modelo)
  lista <- paste0(seq_along(textos), ". ", textos, collapse = "\n")
  prompt <- paste0(
    "Estos items de cuestionario miden un constructo con estas dimensiones:\n",
    paste0("- ", descr, collapse = "\n"),
    "\n\nPara CADA item, da la probabilidad (0 a 1, que sume 1) de que mida cada dimension. ",
    "Usa EXACTAMENTE estos nombres de dimension: ", paste0('"', dims, '"', collapse = ", "),
    ".\nResponde SOLO un objeto JSON: {\"1\": {\"<dimension>\": p, ...}, \"2\": {...}, ...}\n\n", lista)
  # Clasificar es un juicio: razonamiento "low" para que responda el modelo de
  # juicio de la sesion (Haiku con OpenRouter).
  txt <- .llamar_openai(openai, list(list(role = "user", content = prompt)), modelo = modelo,
                        max_tokens = max(2000L, 60L * length(textos)), temperature = 0,
                        razonamiento = "low")
  js <- jsonlite::fromJSON(.extraer_json_llm(txt), simplifyVector = FALSE)
  norm <- function(z) tolower(trimws(gsub("\\s*\\(.*?\\)", "", gsub("_", " ", z))))
  M <- t(vapply(seq_along(textos), function(i) {
    pi <- js[[as.character(i)]]
    if (is.null(pi)) return(rep(NA_real_, length(dims)))
    claves <- norm(names(pi))
    v <- vapply(dims, function(d) {
      k <- which(claves == norm(d))
      as.numeric(if (length(k)) pi[[k[1]]] else 0)
    }, numeric(1))
    if (sum(v) > 0) v / sum(v) else rep(NA_real_, length(dims))
  }, numeric(length(dims))))
  M
}

#' @export
print.semilla_pertenencia <- function(x, ...) {
  cat("Probabilidad de pertenencia (lectores: ", paste(attr(x, "lectores"), collapse = " + "), ")\n", sep = "")
  cat("  Items que pertenecen a su dimension: ", sum(x$pertenece, na.rm = TRUE), " de ", nrow(x), "\n", sep = "")
  if (any(!is.na(x$consenso)))
    cat("  Discrepan con el clustering (revisar): ", sum(x$discrepancia), "\n", sep = "")
  print(utils::head(as.data.frame(x)[order(x$prob_pertenencia), c("codigo", "dimension", "prob_pertenencia",
                                                                    "dimension_probable", "consenso", "discrepancia")], 10),
        row.names = FALSE)
  invisible(x)
}
