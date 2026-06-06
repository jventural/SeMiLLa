# =============================================================================
# SeMiLLa - Backend de embeddings de acceso libre (sentence-transformers)
# =============================================================================
# Permite calcular embeddings con modelos abiertos que corren en local (sin API
# ni costo), como alternativa reproducible a los modelos propietarios de OpenAI.
# Requiere reticulate + el modulo Python 'sentence-transformers'.
# =============================================================================

# Modelos de acceso libre curados (multilingues, adecuados para psicometria).
.SEMILLA_MODELOS_LIBRES <- c(
  "paraphrase-multilingual-MiniLM-L12-v2",
  "paraphrase-multilingual-mpnet-base-v2",
  "distiluse-base-multilingual-cased-v2",
  "multilingual-e5-small",
  "multilingual-e5-base",
  "multilingual-e5-large",
  "LaBSE",
  "all-MiniLM-L6-v2",
  "all-mpnet-base-v2"
)

# Decide si un nombre de modelo corresponde a un backend local (libre).
.es_modelo_local <- function(modelo) {
  if (length(modelo) != 1 || is.na(modelo)) return(FALSE)
  if (grepl("^local:", modelo)) return(TRUE)          # prefijo explicito
  if (grepl("/", modelo, fixed = TRUE)) return(TRUE)  # formato HuggingFace org/modelo
  openai <- c("text-embedding-3-small", "text-embedding-3-large", "text-embedding-ada-002")
  if (modelo %in% openai) return(FALSE)
  if (modelo %in% .SEMILLA_MODELOS_LIBRES) return(TRUE)
  FALSE  # nombres desconocidos: se asume OpenAI por compatibilidad
}

# Calcula embeddings con un modelo local de sentence-transformers.
.embeddings_locales <- function(textos, modelo, verbose = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("El backend local requiere el paquete 'reticulate'. Instala con install.packages('reticulate').")
  }
  if (!reticulate::py_module_available("sentence_transformers")) {
    stop("El backend local requiere el modulo Python 'sentence-transformers'.\n",
         "  Instala con: reticulate::py_install('sentence-transformers')")
  }
  nombre <- sub("^local:", "", modelo)
  if (!grepl("/", nombre, fixed = TRUE)) nombre <- paste0("sentence-transformers/", nombre)
  if (verbose) cat("  ", .color_flecha(), " Cargando modelo local: ", nombre, "...\n", sep = "")
  st <- reticulate::import("sentence_transformers", delay_load = TRUE)
  model <- st$SentenceTransformer(nombre)
  emb <- model$encode(textos, show_progress_bar = FALSE)
  as.matrix(emb)
}

#' @title Modelos de embeddings de acceso libre disponibles en SeMiLLa
#'
#' @description
#' Devuelve los nombres de los modelos de embeddings de codigo abierto
#' (multilingues) que SeMiLLa puede usar en local mediante
#' \code{sentence-transformers}, sin clave de API ni costo. Cualquiera de estos
#' nombres puede pasarse al argumento \code{modelo_embedding} de
#' \code{\link{obtener_embeddings}} (o anteponiendo \code{"local:"}). Tambien se
#' acepta cualquier identificador de Hugging Face con formato \code{org/modelo}.
#'
#' @details
#' El backend local requiere \code{reticulate} y el modulo de Python
#' \code{sentence-transformers} (instalable con
#' \code{reticulate::py_install("sentence-transformers")}). La primera vez que se
#' usa un modelo, este se descarga y queda en cache local. Los modelos
#' propietarios de OpenAI (\code{text-embedding-3-small} por defecto) siguen
#' disponibles y no se ven afectados.
#'
#' @return Vector de caracteres con los nombres de los modelos libres curados.
#'
#' @examples
#' \dontrun{
#' modelos_embeddings_libres()
#' # Usar un modelo libre en local
#' emb <- obtener_embeddings(mis_items,
#'                           modelo_embedding = "paraphrase-multilingual-MiniLM-L12-v2")
#' }
#'
#' @seealso \code{\link{obtener_embeddings}}
#' @export
modelos_embeddings_libres <- function() {
  .SEMILLA_MODELOS_LIBRES
}
