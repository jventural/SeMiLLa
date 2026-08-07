# =============================================================================
#  Embeddings via el router de HuggingFace  (alternativa GRATUITA a OpenAI)
# -----------------------------------------------------------------------------
#  Portado desde la App (R/utils.R) el 2026-08-06. Motivo: la app sabia calcular
#  embeddings con HuggingFace pero el PAQUETE no, asi que el script de analisis
#  local que descarga el usuario no podia reproducirlos. Un estudiante con solo
#  token de HF configuraba todo bien en el navegador, descargaba el script y se
#  encontraba con:
#     openai.AuthenticationError: Error code: 401 - Incorrect API key provided
#  porque el script llamaba a OpenAI para los embeddings, hubiera puesto lo que
#  hubiera puesto.
#
#  No requiere correr el modelo localmente (nada de torch ni
#  sentence-transformers): llama al endpoint de feature-extraction, que devuelve
#  un vector por texto. Pensado para modelos sentence-transformers/*.
# =============================================================================

# Un modelo de embeddings es "de HuggingFace" si viene con el prefijo hf:
# (p. ej. "hf:sentence-transformers/all-MiniLM-L6-v2"). El prefijo es lo que lo
# distingue de un modelo local o de uno de OpenAI.

#' @keywords internal
.es_modelo_hf_emb <- function(modelo) {
  is.character(modelo) && length(modelo) == 1 && grepl("^hf:", modelo)
}

#' @keywords internal
.embeddings_hf <- function(items_texto, modelo, hf_token, verbose = FALSE) {
  if (!is.character(hf_token) || !nzchar(hf_token))
    stop("Para un modelo de embeddings de HuggingFace hace falta tu token. ",
         "Pasalo en api_key (empieza por 'hf_') o define HF_TOKEN en el entorno.",
         call. = FALSE)
  model_id <- sub("^hf:", "", modelo)
  url <- paste0("https://router.huggingface.co/hf-inference/models/",
                model_id, "/pipeline/feature-extraction")
  # Los modelos E5 rinden mejor con el prefijo "query: "
  inputs <- if (grepl("e5", model_id, ignore.case = TRUE))
              paste0("query: ", items_texto) else items_texto
  if (verbose)
    cat("  ", .color_flecha(), " Conectando con HuggingFace (", model_id, ")...\n", sep = "")

  body <- jsonlite::toJSON(list(inputs = inputs), auto_unbox = TRUE)
  resp <- httr::POST(
    url,
    httr::add_headers(Authorization = paste("Bearer", hf_token),
                      `Content-Type` = "application/json"),
    body = body, encode = "raw", httr::timeout(120))
  code <- httr::status_code(resp)
  txt  <- httr::content(resp, "text", encoding = "UTF-8")
  if (code != 200) {
    extra <- if (code == 401)
      "\n  (401 = el token no vale para este modelo: revisa que sea un token hf_ con permiso de inferencia)"
      else if (code == 429)
      "\n  (429 = agotaste la cuota gratuita de HuggingFace por ahora; espera o usa OpenAI)"
      else ""
    stop("HuggingFace API error ", code, ": ", substr(txt, 1, 200), extra,
         call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  # Esperado: matriz n_items x dim. Si viene como lista de vectores, apilar.
  mat <- if (is.matrix(parsed)) parsed
         else if (is.list(parsed))
           do.call(rbind, lapply(parsed, function(v) as.numeric(unlist(v))))
         else matrix(as.numeric(parsed), nrow = length(items_texto))
  mat <- matrix(as.numeric(mat), nrow = length(items_texto))
  if (nrow(mat) != length(items_texto))
    stop("Respuesta de HuggingFace inesperada: ", nrow(mat), " filas para ",
         length(items_texto), " items.", call. = FALSE)
  rownames(mat) <- paste0("item_", seq_len(nrow(mat)))
  mat
}
