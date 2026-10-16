# =============================================================================
#  Llamadas al modelo EN PARALELO
# =============================================================================
#  Las fases que consultan al LLM (deseabilidad, jueces) hacian sus llamadas una
#  detras de otra. Medido el 11-ago-2026 sobre una escala real de 33 items: la
#  compuerta tardo 7.5 minutos, y solo la deseabilidad son 4 pasadas x 6 lotes =
#  24 llamadas estrictamente secuenciales, cada una esperando a la anterior.
#
#  Estas llamadas son ESPERA DE RED, no calculo: el proceso esta parado mirando
#  el socket. Por eso se lanzan en HILOS dentro del mismo interprete de Python
#  que ya tiene cargado el cliente de OpenAI, en vez de abrir procesos de R:
#    - un proceso de R nuevo tendria que inicializar reticulate e importar
#      openai otra vez (segundos por trabajador, y falla si ese proceso no
#      hereda RETICULATE_PYTHON: comprobado);
#    - el cliente de Python es seguro entre hilos y la espera de red libera el
#      GIL, asi que N llamadas concurrentes tardan lo que la mas lenta.
#
#  REPRODUCIBILIDAD: esto NO altera el resultado. Los prompts se construyen
#  antes, en R y en el mismo orden de siempre -asi el generador aleatorio se
#  consume igual que en la version secuencial- y solo se paraleliza el envio.
#  Las respuestas vuelven en el orden en que se pidieron.
# =============================================================================

# Lanza `prompts` (lista de cadenas) contra el modelo y devuelve una lista de
# contenidos de texto, del mismo largo y en el mismo orden. Un elemento vale
# NULL si esa llamada fallo tras los reintentos.
#
# max_paralelo: cuantas peticiones vuelan a la vez. Por defecto 6, que va
# holgado dentro de los limites de tasa habituales; con 1 se comporta
# exactamente como la version secuencial.
.chat_en_paralelo <- function(prompts, api_key, modelo,
                              max_tokens = 300L, temperature = 0.3,
                              razonamiento = "low", max_paralelo = 6L,
                              intentos = 2L, verbose = FALSE,
                              seed = NULL, top_p = NULL) {
  if (length(prompts) == 0L) return(list())
  .chat_en_paralelo_lotes(
    list(list(prompts = prompts, max_tokens = max_tokens,
              temperature = temperature, razonamiento = razonamiento,
              seed = seed, top_p = top_p)),
    api_key = api_key, modelo = modelo, max_paralelo = max_paralelo,
    intentos = intentos, verbose = verbose)[[1]]
}

# Varias TANDAS de prompts, cada una con sus propios parametros (tope de
# tokens, temperatura, razonamiento, seed), enviadas en UN solo grupo de hilos.
# Existe por la compuerta (16-10-2026): sus tres jueces -gemelos, deseabilidad y
# correlacion entre dimensiones- no dependen unos de otros pero corrian uno
# detras de otro, y el de gemelos ademas hacia sus 3 consultas en serie. Asi
# vuelan todas a la vez y la espera total es la de la llamada mas lenta.
#
# lotes: lista de list(prompts, max_tokens, temperature, razonamiento, seed,
# top_p). Devuelve una lista del mismo largo que 'lotes'; cada elemento es la
# lista de respuestas de su tanda, en el orden pedido (NULL = esa llamada fallo).
.chat_en_paralelo_lotes <- function(lotes, api_key, modelo, max_paralelo = 6L,
                                    intentos = 2L, verbose = FALSE) {
  if (!length(lotes)) return(list())
  largos <- vapply(lotes, function(l) length(l$prompts), integer(1))
  vacio  <- lapply(largos, function(k) vector("list", k))
  if (sum(largos) == 0L) return(vacio)
  .auto_proveedor_por_clave(api_key)
  max_paralelo <- max(1L, as.integer(max_paralelo))

  secuencial <- function() lapply(lotes, function(l)
    if (!length(l$prompts)) list() else
      .chat_secuencial(l$prompts, api_key, modelo, l$max_tokens, l$temperature,
                       l$razonamiento, intentos, seed = l$seed, top_p = l$top_p))

  ok <- requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(tryCatch(reticulate::py_available(initialize = TRUE), error = function(e) FALSE))
  if (!ok || max_paralelo == 1L) return(secuencial())

  # Los argumentos del modelo se calculan con la misma funcion que usa la ruta
  # secuencial: el tope de tokens, el esfuerzo de razonamiento y el resto de
  # parametros son identicos y las respuestas, comparables.
  kw_lote <- vapply(lotes, function(l) {
    a <- .args_chat_modelo(modelo, list(list(role = "user", content = "x")),
                           max_tokens = l$max_tokens,
                           temperature = l$temperature,
                           seed = l$seed, top_p = l$top_p,
                           razonamiento = l$razonamiento)
    a$messages <- NULL
    as.character(jsonlite::toJSON(a, auto_unbox = TRUE))
  }, character(1))
  prompts <- unlist(lapply(lotes, function(l) as.character(unlist(l$prompts))))
  kws     <- rep(kw_lote, largos)
  n <- length(prompts)

  py <- tryCatch({
    reticulate::py_run_string("
import json, os
from concurrent.futures import ThreadPoolExecutor

def _semilla_chat_lotes(prompts, kwargs_json, api_key, max_workers, intentos, base_url=None):
    from openai import OpenAI
    cli = OpenAI(api_key=api_key, base_url=base_url) if base_url else OpenAI(api_key=api_key)

    def una(par):
        p, kw = par
        kw = json.loads(kw)
        for _ in range(int(intentos)):
            try:
                r = cli.chat.completions.create(
                    messages=[{'role': 'user', 'content': p}], **kw)
                c = r.choices[0].message.content
                if c:
                    return [c, getattr(r, 'model', None) or '']
            except Exception:
                pass
        return None

    with ThreadPoolExecutor(max_workers=int(max_workers)) as ex:
        return list(ex.map(una, list(zip(prompts, kwargs_json))))
", convert = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(py)) return(secuencial())

  if (isTRUE(verbose))
    message(sprintf("  [paralelo] %d llamadas, %d a la vez", n, max_paralelo))

  res <- tryCatch(
    reticulate::py$`_semilla_chat_lotes`(
      prompts      = as.list(prompts),
      kwargs_json  = as.list(kws),
      api_key      = as.character(api_key),
      max_workers  = as.integer(max_paralelo),
      intentos     = as.integer(intentos),
      # Antes no se pasaba: con Groq, Hugging Face u OpenRouter, la ruta en
      # paralelo seguia llamando a OpenAI con la clave de otro proveedor.
      base_url     = getOption("SeMiLLa.base_url", NULL)),
    error = function(e) e)

  # Si el camino paralelo falla por lo que sea, se cae al secuencial: es mas
  # lento pero nunca deja el analisis a medias.
  if (inherits(res, "error") || length(res) != n) {
    if (isTRUE(verbose))
      message("  [paralelo] no disponible, se usa la ruta secuencial: ",
              if (inherits(res, "error")) conditionMessage(res) else "respuesta incompleta")
    return(secuencial())
  }
  # Hasta 2.12.0 las llamadas en paralelo no pasaban por el registro de
  # modelos (.modelos_usados), asi que el informe no las contaba.
  cont <- lapply(seq_len(n), function(k) {
    z <- res[[k]]
    if (is.null(z) || !length(z) || is.null(z[[1]]) || identical(z[[1]], ""))
      return(NULL)
    real <- if (length(z) > 1 && nzchar(z[[2]] %||% "")) z[[2]] else NULL
    .registrar_llamada(list(model = real), modelo)
    as.character(z[[1]])
  })
  fin <- cumsum(largos)
  lapply(seq_along(largos), function(i)
    if (largos[i] == 0L) list() else cont[(fin[i] - largos[i] + 1L):fin[i]])
}

# Ruta de siempre, una llamada detras de otra. Se conserva como respaldo y para
# poder comparar resultados con la version paralela.
.chat_secuencial <- function(prompts, api_key, modelo, max_tokens, temperature,
                             razonamiento, intentos = 2L, seed = NULL, top_p = NULL) {
  openai <- .configurar_openai(api_key)
  lapply(prompts, function(p) {
    for (i in seq_len(intentos)) {
      r <- tryCatch(do.call(openai$chat$completions$create,
                            .args_chat_modelo(modelo,
                                              list(list(role = "user", content = p)),
                                              max_tokens = max_tokens,
                                              temperature = temperature,
                                              seed = seed, top_p = top_p,
                                              razonamiento = razonamiento)),
                    error = function(e) NULL)
      cont <- if (!is.null(r)) r$choices[[1]]$message$content else NULL
      if (!is.null(cont) && nzchar(cont)) return(cont)
    }
    NULL
  })
}
