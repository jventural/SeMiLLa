# OpenRouter: traduccion de nombres de modelo, modelos baratos por rol y
# razonamiento por defecto. Ninguna prueba llama a la API.

con_proveedor <- function(..., code) {
  viejo <- options(SeMiLLa.base_url = NULL, SeMiLLa.modelo_generacion = NULL,
                   SeMiLLa.modelo_juicio = NULL)
  on.exit(options(viejo))
  usar_proveedor(..., verbose = FALSE)
  force(code)
}

test_that("con OpenRouter los nombres de OpenAI ganan el prefijo openai/", {
  con_proveedor("openrouter", code = {
    expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "openai/gpt-4.1-mini")
    expect_equal(SeMiLLa:::.modelo_embedding_proveedor("text-embedding-3-small"),
                 "openai/text-embedding-3-small")
    expect_equal(SeMiLLa:::.resolver_modelo("anthropic/claude-haiku-5.5"),
                 "anthropic/claude-haiku-5.5")
  })
})

test_that("modelo_generacion y modelo_juicio se aplican segun el rol de la llamada", {
  con_proveedor("openrouter", modelo_generacion = "anthropic/claude-haiku-5.5",
                modelo_juicio = "openai/gpt-6-luna", code = {
    expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "anthropic/claude-haiku-5.5")
    expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini", "minimal"), "anthropic/claude-haiku-5.5")
    expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini", "low"), "openai/gpt-6-luna")
  })
})

test_that("otros proveedores no traducen ni sustituyen modelos", {
  con_proveedor("openrouter", modelo_generacion = "anthropic/claude-haiku-5.5", code = {
    usar_proveedor("groq", verbose = FALSE)
    expect_null(getOption("SeMiLLa.modelo_generacion"))
    expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "gpt-4.1-mini")
  })
  con_proveedor("openai", code =
    expect_equal(SeMiLLa:::.modelo_embedding_proveedor("text-embedding-3-small"),
                 "text-embedding-3-small"))
})

test_that("generar apaga el razonamiento y juzgar lo pide bajo con reserva de tokens", {
  con_proveedor("openrouter", modelo_generacion = "anthropic/claude-haiku-5.5",
                modelo_juicio = "openai/gpt-6-luna", code = {
    msg <- list(list(role = "user", content = "x"))
    gen <- SeMiLLa:::.args_chat_modelo("gpt-4.1-mini", msg, max_tokens = 2000)
    expect_equal(gen$model, "anthropic/claude-haiku-5.5")
    expect_false(gen$extra_body$reasoning$enabled)
    expect_equal(gen$max_tokens, 2000L)
    jui <- SeMiLLa:::.args_chat_modelo("gpt-4.1-mini", msg, max_tokens = 300, razonamiento = "low")
    expect_equal(jui$model, "openai/gpt-6-luna")
    expect_equal(jui$extra_body$reasoning$effort, "low")
    expect_equal(jui$max_tokens, 300L + 1024L)
  })
})

test_that("un razonador de OpenAI por OpenRouter conserva su contrato", {
  con_proveedor("openrouter", code = {
    a <- SeMiLLa:::.args_chat_modelo("gpt-5-mini", list(list(role = "user", content = "x")),
                                     max_tokens = 300, razonamiento = "low")
    expect_equal(a$model, "openai/gpt-5-mini")
    expect_equal(a$reasoning_effort, "low")
    expect_equal(a$max_completion_tokens, 300L + 1024L)
  })
})

test_that("los prefijos de OpenRouter no se confunden con Hugging Face", {
  expect_equal(SeMiLLa:::.inferir_proveedor_por_modelo("anthropic/claude-haiku-5.5")$proveedor, "openrouter")
  expect_equal(SeMiLLa:::.inferir_proveedor_por_modelo("Qwen/Qwen2.5-72B-Instruct")$proveedor, "huggingface")
  expect_null(SeMiLLa:::.inferir_proveedor_por_modelo("gpt-4.1-mini"))
})

test_that("una clave sk-or- activa OpenRouter con Luna y Haiku, y otra clave lo deshace", {
  viejo <- options(SeMiLLa.base_url = NULL, SeMiLLa.modelo_generacion = NULL,
                   SeMiLLa.modelo_juicio = NULL, SeMiLLa.proveedor_auto = NULL)
  on.exit(options(viejo))
  SeMiLLa:::.auto_proveedor_por_clave("sk-or-v1-abc")
  expect_true(SeMiLLa:::.es_openrouter())
  expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "openai/gpt-6-luna")
  expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini", "low"), "anthropic/claude-haiku-5.5")
  SeMiLLa:::.auto_proveedor_por_clave("sk-proj-xyz")
  expect_false(SeMiLLa:::.es_openrouter())
  expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "gpt-4.1-mini")
})

test_that("la deteccion automatica no pisa un proveedor elegido a mano", {
  viejo <- options(SeMiLLa.base_url = NULL, SeMiLLa.modelo_generacion = NULL,
                   SeMiLLa.modelo_juicio = NULL, SeMiLLa.proveedor_auto = NULL)
  on.exit(options(viejo))
  usar_proveedor("openrouter", modelo_generacion = "anthropic/claude-haiku-5.5", verbose = FALSE)
  SeMiLLa:::.auto_proveedor_por_clave("sk-proj-xyz")   # no fue automatica: se respeta
  expect_true(SeMiLLa:::.es_openrouter())
  expect_equal(SeMiLLa:::.resolver_modelo("gpt-4.1-mini"), "anthropic/claude-haiku-5.5")
})
