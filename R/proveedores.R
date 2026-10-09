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
#'   \item \code{"openrouter"}: \code{https://openrouter.ai/api/v1}. Una sola
#'         clave de OpenRouter sirve para el texto Y para los embeddings
#'         (\code{openai/text-embedding-3-small}, el mismo modelo que usa
#'         SeMiLLa por defecto, de modo que los umbrales calibrados siguen
#'         valiendo). Con \code{modelo_generacion} y \code{modelo_juicio} se
#'         eligen modelos mas baratos para toda la sesion (ver Detalles).
#'   \item \code{"personalizado"}: pasar \code{base_url} explicitamente.
#' }
#'
#' La \code{api_key} que se pasa a las funciones de SeMiLLa debe ser la del
#' proveedor activo (no la de OpenAI, salvo que el proveedor sea OpenAI).
#'
#' Con \code{"openrouter"}, los nombres de OpenAI que usan las funciones por
#' defecto (\code{"gpt-4.1-mini"}, \code{"text-embedding-3-small"}) se
#' traducen solos a \code{"openai/..."}. Si se fija \code{modelo_generacion},
#' las llamadas que GENERAN texto (items, definiciones, reemplazos) usan ese
#' modelo; si se fija \code{modelo_juicio}, lo usan las que JUZGAN
#' (deseabilidad, jueces de validez, auditorias), que son las que piden
#' razonamiento. Asi se puede, por ejemplo, generar con
#' \code{"anthropic/claude-haiku-5.5"} y juzgar con \code{"openai/gpt-6-luna"}.
#'
#' Sin llamar a esta funcion, basta pasar una clave de OpenRouter
#' (\code{"sk-or-..."}) como \code{api_key}: SeMiLLa activa OpenRouter sola con
#' GPT-6 Luna para generar y Claude Haiku 5.5 para juzgar (la combinacion que
#' costo un 40 por ciento menos que gpt-4.1-mini con V de Aiken equivalentes).
#' Si se fijo un proveedor con esta funcion, la clave no lo cambia.
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
#' @param modelo_generacion,modelo_juicio Solo con \code{"openrouter"}:
#'   modelo (formato \code{"organizacion/modelo"}) que reemplaza al de las
#'   funciones en las llamadas que generan texto y en las que juzgan. Con
#'   \code{NULL} se usa el modelo que pida cada funcion.
#' @param verbose Si \code{TRUE}, imprime en consola la confirmacion del
#'   proveedor activo.
#'
#' @return Cadena de caracteres (invisible) con la \code{base_url} activa, o
#'   \code{NULL} si el proveedor es OpenAI. Como efecto, fija la opcion
#'   \code{SeMiLLa.base_url} para el resto de la sesion; ese es el proposito de
#'   la funcion, y \code{usar_proveedor("openai")} la devuelve a su estado
#'   por defecto.
#'
#' @examples
#' # Solo fija una opcion de la sesion (no llama a ninguna API)
#' url <- usar_proveedor("groq", verbose = FALSE)
#' url
#' getOption("SeMiLLa.base_url")
#' # Volver al estado por defecto
#' usar_proveedor("openai", verbose = FALSE)
#'
#' # Lo siguiente requiere una clave de API del proveedor (llama a un LLM).
#' # 'tabla' es una tabla de especificaciones (ver ?generar_prueba_objetiva).
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
#'
#' # Todo por OpenRouter con modelos baratos (una sola clave)
#' usar_proveedor("openrouter",
#'                modelo_generacion = "anthropic/claude-haiku-5.5",
#'                modelo_juicio     = "openai/gpt-6-luna")
#' esc <- semilla("Autoeficacia academica", api_key = Sys.getenv("OPENROUTER_API_KEY"))
#' }
#'
#' @export
usar_proveedor <- function(
  proveedor = c("openai", "groq", "huggingface", "ollama", "openrouter",
                "personalizado"),
  base_url  = NULL,
  modelo_generacion = NULL,
  modelo_juicio     = NULL,
  verbose   = TRUE
) {
  proveedor <- match.arg(proveedor)

  url <- switch(proveedor,
    "openai"        = NULL,
    "groq"          = "https://api.groq.com/openai/v1",
    "huggingface"   = "https://router.huggingface.co/v1",
    "ollama"        = "http://localhost:11434/v1",
    "openrouter"    = "https://openrouter.ai/api/v1",
    "personalizado" = {
      if (is.null(base_url) || !nzchar(base_url))
        stop("Con proveedor = 'personalizado' debes indicar 'base_url'.")
      base_url
    }
  )

  options(SeMiLLa.base_url = url, SeMiLLa.proveedor_auto = NULL)
  # Los reemplazos de modelo solo tienen sentido en OpenRouter; cualquier otro
  # proveedor los limpia para que no se arrastren de una configuracion previa.
  es_or <- identical(proveedor, "openrouter")
  options(SeMiLLa.modelo_generacion = if (es_or) modelo_generacion else NULL,
          SeMiLLa.modelo_juicio     = if (es_or) modelo_juicio else NULL)

  if (verbose) {
    if (is.null(url)) {
      cat("[usar_proveedor] Generacion de texto: OpenAI (default).\n")
    } else {
      cat("[usar_proveedor] Generacion de texto redirigida a '",
          proveedor, "':\n  ", url, "\n", sep = "")
      cat("  Recuerda: 'api_key' debe ser la clave de ESTE proveedor y\n",
          "  'modelo' un modelo que el proveedor sirva.\n", sep = "")
      if (es_or) {
        cat("  Los embeddings tambien van por OpenRouter",
            " (openai/text-embedding-3-small).\n", sep = "")
        if (!is.null(modelo_generacion))
          cat("  Generacion: ", modelo_generacion, "\n", sep = "")
        if (!is.null(modelo_juicio))
          cat("  Juicios:    ", modelo_juicio, "\n", sep = "")
      } else {
        cat("  Los embeddings siguen en OpenAI; para independencia total usa\n",
            "  modelos locales (modelos_embeddings_libres()).\n", sep = "")
      }
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
#' @noRd
.inferir_proveedor_por_modelo <- function(modelo) {
  if (is.null(modelo) || !nzchar(modelo)) return(NULL)
  m <- tolower(trimws(modelo))

  # Prefijos que solo sirve OpenRouter (en Hugging Face no existen esas
  # organizaciones con modelos de chat abiertos).
  if (grepl("^~?(anthropic|openai|google|x-ai|typesafe)/", m)) {
    return(list(base_url = "https://openrouter.ai/api/v1",
                proveedor = "openrouter"))
  }
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


# --- OpenRouter: nombres de modelo -------------------------------------------
# OpenRouter nombra los modelos como "organizacion/modelo". Las funciones del
# paquete piden nombres de OpenAI sin prefijo ("gpt-4.1-mini"), asi que aqui se
# traducen, y si el usuario eligio modelos baratos con usar_proveedor() se
# sustituyen: el de juicio para las llamadas que piden razonamiento (las que
# JUZGAN) y el de generacion para el resto.

#' @keywords internal
#' @noRd
.es_openrouter <- function(base_url = getOption("SeMiLLa.base_url", NULL)) {
  !is.null(base_url) && grepl("openrouter\\.ai", base_url)
}

#' @keywords internal
#' @noRd
.resolver_modelo <- function(modelo, razonamiento = NULL) {
  if (!.es_openrouter()) return(modelo)
  juicio <- !is.null(razonamiento) && razonamiento %in% c("low", "medium", "high")
  elegido <- if (juicio) getOption("SeMiLLa.modelo_juicio", NULL)
             else getOption("SeMiLLa.modelo_generacion", NULL)
  if (!is.null(elegido) && nzchar(elegido)) return(elegido)
  if (!grepl("/", modelo, fixed = TRUE)) return(paste0("openai/", modelo))
  modelo
}

#' @keywords internal
#' @noRd
.modelo_embedding_proveedor <- function(modelo) {
  if (.es_openrouter() && !grepl("/", modelo, fixed = TRUE))
    return(paste0("openai/", modelo))
  modelo
}

# --- Clave de OpenRouter reconocida sola --------------------------------------
# Una clave de OpenRouter empieza por "sk-or-". Si llega una y nadie fijo un
# proveedor con usar_proveedor(), se activa OpenRouter con los modelos baratos
# recomendados (GPT-6 Luna genera, Claude Haiku 5.5 juzga; medido el
# 2026-10-09: -40 % de costo frente a gpt-4.1-mini con V de Aiken equivalentes).
# Asi funciona igual en un script, en la app y en los procesos de fondo que la
# app lanza con callr, que no heredan las opciones de la sesion. Si despues
# llega una clave que NO es de OpenRouter, la activacion automatica se deshace
# (en la app, un mismo proceso de R atiende a varios usuarios).

#' @keywords internal
#' @noRd
.auto_proveedor_por_clave <- function(api_key) {
  if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key)) return(invisible())
  es_or <- grepl("^sk-or-", trimws(api_key))
  auto  <- isTRUE(getOption("SeMiLLa.proveedor_auto"))
  if (es_or && is.null(getOption("SeMiLLa.base_url", NULL))) {
    options(SeMiLLa.base_url = "https://openrouter.ai/api/v1",
            SeMiLLa.proveedor_auto = TRUE,
            SeMiLLa.modelo_generacion = getOption("SeMiLLa.modelo_generacion") %||% "openai/gpt-6-luna",
            SeMiLLa.modelo_juicio     = getOption("SeMiLLa.modelo_juicio") %||% "anthropic/claude-haiku-5.5")
  } else if (!es_or && auto) {
    options(SeMiLLa.base_url = NULL, SeMiLLa.proveedor_auto = NULL,
            SeMiLLa.modelo_generacion = NULL, SeMiLLa.modelo_juicio = NULL)
  }
  invisible()
}
