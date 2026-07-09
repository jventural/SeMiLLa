# =============================================================================
# SeMiLLa - Proveedores LLM alternativos (endpoints OpenAI-compatibles)
# =============================================================================
#
# SeMiLLa habla el protocolo de chat de OpenAI a traves del cliente Python
# `openai` (reticulate). Ese protocolo lo implementan tambien Groq, el router
# de Hugging Face, Ollama y otros servicios, de modo que basta cambiar la
# base_url del cliente para redirigir la GENERACION DE TEXTO a otro proveedor
# sin tocar el resto del paquete.
#
# Los EMBEDDINGS son aparte: siguen pidiendose a OpenAI (text-embedding-*) o
# a modelos locales via sentence-transformers ("local:..." u "org/modelo",
# ver embeddings_locales.R). Groq y Ollama no sirven los modelos de
# embeddings de OpenAI.

#' @title Redirigir la generacion de texto a otro proveedor LLM
#'
#' @description
#' Fija (para toda la sesion) la URL base del endpoint de chat que usaran las
#' funciones generadoras de SeMiLLa. Todos los proveedores soportados exponen
#' una API compatible con OpenAI, por lo que el resto del flujo (prompts,
#' cache, seed) no cambia.
#'
#' Proveedores predefinidos:
#' \itemize{
#'   \item \code{"openai"}: default; limpia cualquier redireccion previa.
#'   \item \code{"groq"}: \code{https://api.groq.com/openai/v1}. Modelos tipo
#'         \code{"llama-3.3-70b-versatile"}. Requiere api_key de Groq.
#'   \item \code{"huggingface"}: \code{https://router.huggingface.co/v1}.
#'         Modelos en formato \code{"organizacion/modelo"} (p. ej.
#'         \code{"Qwen/Qwen2.5-72B-Instruct"}). Requiere token HF.
#'   \item \code{"ollama"}: \code{http://localhost:11434/v1} (servidor local;
#'         la api_key puede ser cualquier cadena no vacia).
#'   \item \code{"personalizado"}: pasar \code{base_url} explicitamente.
#' }
#'
#' La \code{api_key} que se pasa a las funciones de SeMiLLa debe ser la del
#' proveedor activo (no la de OpenAI, salvo que el proveedor sea OpenAI).
#'
#' @section Embeddings:
#' Esta redireccion afecta SOLO a la generacion de texto (chat). Las
#' funciones de embeddings siguen usando OpenAI; si no deseas depender de
#' OpenAI, usa modelos de embeddings locales (\code{modelos_embeddings_libres()}).
#'
#' @param proveedor Uno de \code{"openai"}, \code{"groq"},
#'   \code{"huggingface"}, \code{"ollama"}, \code{"personalizado"}.
#' @param base_url URL base del endpoint (solo requerido con
#'   \code{proveedor = "personalizado"}; en los demas casos se ignora).
#' @param verbose Mostrar confirmacion.
#'
#' @return (Invisible) la base_url activa, o \code{NULL} si el proveedor es
#'   OpenAI.
#'
#' @examples
#' \dontrun{
#' # Generar la prueba con Llama 3.3 via Groq
#' usar_proveedor("groq")
#' p <- generar_prueba_objetiva(
#'   dominio = "psicometria",
#'   api_key = Sys.getenv("GROQ_API_KEY"),
#'   tabla_especificacion = tabla,
#'   modelo = "llama-3.3-70b-versatile"
#' )
#'
#' # Volver a OpenAI
#' usar_proveedor("openai")
#' }
#'
#' @export
usar_proveedor <- function(
  proveedor = c("openai", "groq", "huggingface", "ollama", "personalizado"),
  base_url  = NULL,
  verbose   = TRUE
) {
  proveedor <- match.arg(proveedor)

  url <- switch(proveedor,
    "openai"        = NULL,
    "groq"          = "https://api.groq.com/openai/v1",
    "huggingface"   = "https://router.huggingface.co/v1",
    "ollama"        = "http://localhost:11434/v1",
    "personalizado" = {
      if (is.null(base_url) || !nzchar(base_url))
        stop("Con proveedor = 'personalizado' debes indicar 'base_url'.")
      base_url
    }
  )

  options(SeMiLLa.base_url = url)

  if (verbose) {
    if (is.null(url)) {
      cat("[usar_proveedor] Generacion de texto: OpenAI (default).\n")
    } else {
      cat("[usar_proveedor] Generacion de texto redirigida a '",
          proveedor, "':\n  ", url, "\n", sep = "")
      cat("  Recuerda: 'api_key' debe ser la clave de ESTE proveedor y\n",
          "  'modelo' un modelo que el proveedor sirva.\n", sep = "")
      cat("  Los embeddings siguen en OpenAI; para independencia total usa\n",
          "  modelos locales (modelos_embeddings_libres()).\n", sep = "")
    }
  }

  invisible(url)
}


# Deduce la base_url a partir del nombre del modelo cuando el usuario no
# configuro nada con usar_proveedor(). Reglas conservadoras:
#   - "org/modelo" (contiene "/")            -> router de Hugging Face
#   - familias abiertas servidas por Groq    -> Groq
#   - todo lo demas (gpt-*, o1/o3/o4, text-embedding-*) -> OpenAI (NULL)
# Devuelve list(base_url, proveedor) o NULL si corresponde a OpenAI.

#' @keywords internal
.inferir_proveedor_por_modelo <- function(modelo) {
  if (is.null(modelo) || !nzchar(modelo)) return(NULL)
  m <- tolower(trimws(modelo))

  if (grepl("/", m, fixed = TRUE)) {
    return(list(base_url = "https://router.huggingface.co/v1",
                proveedor = "huggingface"))
  }
  if (grepl("^(llama|mixtral|gemma|qwen|deepseek)", m)) {
    return(list(base_url = "https://api.groq.com/openai/v1",
                proveedor = "groq"))
  }
  NULL
}
