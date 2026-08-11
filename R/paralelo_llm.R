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
                              intentos = 2L, verbose = FALSE) {

  n <- length(prompts)
  if (n == 0L) return(list())
  max_paralelo <- max(1L, as.integer(max_paralelo))

  # Los argumentos del modelo se calculan UNA vez con la misma funcion que usa
  # la ruta secuencial: asi el tope de tokens, el esfuerzo de razonamiento y el
  # resto de parametros son identicos y las respuestas comparables.
  args_modelo <- .args_chat_modelo(modelo, list(list(role = "user", content = "x")),
                                   max_tokens = max_tokens,
                                   temperature = temperature,
                                   razonamiento = razonamiento)
  args_modelo$messages <- NULL

  ok <- requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(tryCatch(reticulate::py_available(initialize = TRUE), error = function(e) FALSE))
  if (!ok || max_paralelo == 1L)
    return(.chat_secuencial(prompts, api_key, modelo, max_tokens, temperature,
                            razonamiento, intentos))

  py <- tryCatch({
    reticulate::py_run_string("
import json, os
from concurrent.futures import ThreadPoolExecutor

def _semilla_chat_paralelo(prompts, api_key, kwargs_json, max_workers, intentos):
    from openai import OpenAI
    cli = OpenAI(api_key=api_key)
    kw = json.loads(kwargs_json)

    def una(p):
        for _ in range(int(intentos)):
            try:
                r = cli.chat.completions.create(
                    messages=[{'role': 'user', 'content': p}], **kw)
                c = r.choices[0].message.content
                if c:
                    return c
            except Exception:
                pass
        return None

    with ThreadPoolExecutor(max_workers=int(max_workers)) as ex:
        return list(ex.map(una, list(prompts)))
", convert = TRUE)
    TRUE
  }, error = function(e) FALSE)

  if (!isTRUE(py))
    return(.chat_secuencial(prompts, api_key, modelo, max_tokens, temperature,
                            razonamiento, intentos))

  if (isTRUE(verbose))
    message(sprintf("  [paralelo] %d llamadas, %d a la vez", n, max_paralelo))

  res <- tryCatch(
    reticulate::py$`_semilla_chat_paralelo`(
      prompts      = as.list(as.character(prompts)),
      api_key      = as.character(api_key),
      kwargs_json  = jsonlite::toJSON(args_modelo, auto_unbox = TRUE),
      max_workers  = as.integer(max_paralelo),
      intentos     = as.integer(intentos)),
    error = function(e) e)

  # Si el camino paralelo falla por lo que sea, se cae al secuencial: es mas
  # lento pero nunca deja el analisis a medias.
  if (inherits(res, "error") || length(res) != n) {
    if (isTRUE(verbose))
      message("  [paralelo] no disponible, se usa la ruta secuencial: ",
              if (inherits(res, "error")) conditionMessage(res) else "respuesta incompleta")
    return(.chat_secuencial(prompts, api_key, modelo, max_tokens, temperature,
                            razonamiento, intentos))
  }
  lapply(res, function(z) if (is.null(z) || identical(z, "")) NULL else as.character(z))
}

# Ruta de siempre, una llamada detras de otra. Se conserva como respaldo y para
# poder comparar resultados con la version paralela.
.chat_secuencial <- function(prompts, api_key, modelo, max_tokens, temperature,
                             razonamiento, intentos = 2L) {
  openai <- .configurar_openai(api_key)
  lapply(prompts, function(p) {
    for (i in seq_len(intentos)) {
      r <- tryCatch(do.call(openai$chat$completions$create,
                            .args_chat_modelo(modelo,
                                              list(list(role = "user", content = p)),
                                              max_tokens = max_tokens,
                                              temperature = temperature,
                                              razonamiento = razonamiento)),
                    error = function(e) NULL)
      cont <- if (!is.null(r)) r$choices[[1]]$message$content else NULL
      if (!is.null(cont) && nzchar(cont)) return(cont)
    }
    NULL
  })
}
