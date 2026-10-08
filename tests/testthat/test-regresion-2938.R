# Regresiones cerradas en 2.9.38. Ninguna llama a la API: .llamar_openai() y
# las funciones que la rodean se sustituyen con local_mocked_bindings().

# --- 1. Jueces de validez_contenido(): nada de relleno silencioso ------------

test_that("una respuesta ilegible deja el item en NA, no en 2", {
  local_mocked_bindings(.llamar_openai = function(...) "no es un json")
  expect_warning(
    ev <- SeMiLLa:::.evaluar_item_jueces(
      openai = NULL, item = "Me preocupa lo que piensan", dimension = "D1",
      definicion_dimension = "", contexto = list(concepto = "x", definicion = "y"),
      criterios = c("relevancia", "representatividad"), n_jueces = 5,
      modelo = "gpt-4.1-mini"),
    "queda en NA")
  expect_true(isTRUE(attr(ev, "fallida")))
  expect_true(all(is.na(ev$relevancia)))
  expect_false(any(ev$relevancia %in% 2))
})

test_that("se reintenta una vez y se acepta JSON con texto alrededor", {
  n <- 0
  local_mocked_bindings(.llamar_openai = function(...) {
    n <<- n + 1
    if (n == 1) "lo siento" else
      'Aqui va: [{"juez":1,"relevancia":"3","representatividad":2},
                 {"juez":2,"relevancia":4,"representatividad":3}] fin'
  })
  ev <- SeMiLLa:::.evaluar_item_jueces(
    openai = NULL, item = "x", dimension = "D1", definicion_dimension = "",
    contexto = list(concepto = "x", definicion = "y"),
    criterios = c("relevancia", "representatividad"), n_jueces = 2,
    modelo = "gpt-4.1-mini")
  expect_equal(n, 2)
  # "3" en texto pasa a numero; el 4 esta fuera de la escala 0-3 y queda NA
  expect_equal(ev$relevancia, c(3, NA))
  expect_equal(ev$representatividad, c(2, 3))
})

test_that("la V de Aiken usa solo los jueces que respondieron", {
  items_df <- data.frame(numero = 1:3, dimension = "D1",
                         item = c("a", "b", "c"), stringsAsFactors = FALSE)
  completo <- data.frame(juez = 1:4, relevancia = c(3, 3, 2, 3))
  parcial  <- data.frame(juez = 1:2, relevancia = c(3, 3))     # solo 2 jueces
  vacio    <- data.frame(juez = 1:4, relevancia = NA_real_)
  r <- SeMiLLa:::.calcular_v_aiken(list(completo, parcial, vacio), items_df,
                                   "relevancia", n_jueces = 4, confianza = 0.95)
  va <- r$v_aiken
  expect_equal(va$V_promedio[1], round(11 / 12, 3))
  # antes los 2 jueces que faltaban se rellenaban con 2 y daba V = .833
  expect_equal(va$V_promedio[2], 1)
  expect_true(is.na(va$V_promedio[3]))
  expect_equal(va$n_jueces_validos, c(4, 2, 0))
  # con menos jueces el IC es mas ancho
  expect_lt(va$IC_inf[2], SeMiLLa:::.calcular_v_aiken(
    list(data.frame(juez = 1:4, relevancia = 3)), items_df[1, ], "relevancia",
    4, 0.95)$v_aiken$IC_inf)
  # el promedio de la escala ignora el item sin evaluar
  expect_false(is.na(r$v_aiken_escala$V_total))
})

test_that("la auditoria de calidad no inventa un 3 cuando la API falla", {
  local_mocked_bindings(.llamar_openai = function(...) stop("timeout"))
  r <- SeMiLLa:::.evaluar_item_calidad_llm(NULL, "item", "D1", "x",
                                           c("claridad", "simplicidad"), "m")
  expect_true(all(is.na(r$puntuaciones)))
  expect_match(r$recomendacion, "Sin evaluacion")
})


# --- 2. .llamar_openai(): cache y registro del modelo real -------------------

cliente_falso <- function(contenido, fin = "stop", modelo_real = "gpt-x-2026-01-01") {
  list(chat = list(completions = list(create = function(...) list(
    model = modelo_real, system_fingerprint = "fp_1",
    choices = list(list(message = list(content = contenido),
                        finish_reason = fin))))))
}

test_that("no se guarda en cache una respuesta truncada o vacia", {
  dir <- withr::local_tempdir()
  withr::local_options(SeMiLLa.cache_enabled = TRUE, SeMiLLa.cache_dir = dir,
                       SeMiLLa.cache_verbose = FALSE)
  msg <- list(list(role = "user", content = "hola"))

  expect_warning(SeMiLLa:::.llamar_openai(cliente_falso("{\"a\":", "length"), msg,
                                          modelo = "gpt-4.1-mini"),
                 "max_tokens")
  expect_length(list.files(dir), 0)

  SeMiLLa:::.llamar_openai(cliente_falso(""), msg, modelo = "gpt-4.1-mini")
  expect_length(list.files(dir), 0)

  SeMiLLa:::.llamar_openai(cliente_falso("ok"), msg, modelo = "gpt-4.1-mini")
  expect_length(list.files(dir), 1)
})

test_that("el proveedor entra en la clave de cache solo si hay base_url", {
  dir <- withr::local_tempdir()
  withr::local_options(SeMiLLa.cache_enabled = TRUE, SeMiLLa.cache_dir = dir,
                       SeMiLLa.cache_verbose = FALSE)
  msg <- list(list(role = "user", content = "hola"))
  SeMiLLa:::.llamar_openai(cliente_falso("openai"), msg, modelo = "m")
  withr::local_options(SeMiLLa.base_url = "https://api.groq.com/openai/v1")
  r <- SeMiLLa:::.llamar_openai(cliente_falso("groq"), msg, modelo = "m")
  expect_equal(r, "groq")              # no reutiliza la respuesta de OpenAI
  expect_length(list.files(dir), 2)
})

test_that("se registra la version real del modelo", {
  marca <- SeMiLLa:::.marca_registro()
  SeMiLLa:::.llamar_openai(cliente_falso("ok", modelo_real = "gpt-test-2099"),
                           list(list(role = "user", content = "z")), modelo = "gpt-test")
  mu <- SeMiLLa:::.modelos_usados(marca)
  expect_equal(mu$modelo_real, "gpt-test-2099")
  expect_equal(mu$n_llamadas, 1)
})


# --- 3. Modo cientifico: referencias desde los metadatos, no del LLM ---------

xml_pubmed <- paste0(
  '<PubmedArticleSet>\n<PubmedArticle>\n<MedlineCitation>\n<Article>\n',
  '<Journal><Title>Journal of Tests</Title></Journal>\n',
  '<ArticleTitle>Escala de <i>Sj&#xf6;gren</i> en adultos</ArticleTitle>\n',
  '<Abstract><AbstractText Label="BACKGROUND">Fondo uno.</AbstractText>\n',
  '<AbstractText Label="METHODS">Metodo uno.</AbstractText></Abstract>\n',
  '<AuthorList><Author><LastName>Perez</LastName><Initials>JA</Initials></Author>\n',
  '<Author><LastName>Gomez</LastName><Initials>M</Initials></Author></AuthorList>\n',
  '</Article>\n<DateCompleted/>\n</MedlineCitation>\n',
  '<PubmedData><ArticleIdList><ArticleId IdType="doi">10.1/xyz</ArticleId>',
  '</ArticleIdList></PubmedData>\n<PubDate><Year>2021</Year></PubDate>\n</PubmedArticle>\n',
  '<PubmedArticle><MedlineCitation><Article><Journal><Title>Rev B</Title></Journal>',
  '<ArticleTitle>Sin resumen</ArticleTitle></Article></MedlineCitation></PubmedArticle>\n',
  '<PubmedArticle><MedlineCitation><Article><ArticleTitle>Tercero</ArticleTitle>',
  '<Abstract><AbstractText>Resumen tres.</AbstractText></Abstract>',
  '</Article></MedlineCitation></PubmedArticle>\n</PubmedArticleSet>')

test_that("cada resumen de PubMed queda con su propio titulo", {
  a <- SeMiLLa:::.parsear_pubmed_xml(xml_pubmed)
  expect_length(a, 3)
  expect_equal(a[[1]]$titulo, "Escala de Sjögren en adultos")
  expect_equal(a[[1]]$resumen, "Fondo uno. Metodo uno.")
  # antes el segundo AbstractText del articulo 1 se emparejaba con el articulo 2
  expect_equal(a[[2]]$resumen, "")
  expect_equal(a[[3]]$resumen, "Resumen tres.")
  expect_equal(a[[1]]$referencia,
    "Perez, J. A., & Gomez, M. (2021). Escala de Sjögren en adultos. Journal of Tests. https://doi.org/10.1/xyz")
  expect_match(a[[2]]$referencia, "\\(s\\. f\\.\\)")
})

test_that("las referencias de Semantic Scholar salen de sus metadatos", {
  js <- '{"data":[{"title":"RSES","year":2010,"venue":"Psych","authors":[{"name":"Morris Rosenberg"}],"externalIds":{"DOI":"10.2/b"}}]}'
  r <- SeMiLLa:::.referencias_semantic_scholar(jsonlite::fromJSON(js)$data)
  expect_equal(r, "Rosenberg, M. (2010). RSES. Psych. https://doi.org/10.2/b")
})

test_that("con mas de 20 autores la referencia sigue APA 7", {
  aut <- sprintf("Autor%02d, A.", 1:25)
  r <- SeMiLLa:::.formatear_referencia(aut, "2020", "T", "R", "")
  expect_match(r, "Autor19, A\\., \\. \\. \\. Autor25, A\\. \\(2020\\)")
  expect_false(grepl("Autor20|&", r))
})

test_that("el respaldo sin literatura no se etiqueta como cientifico", {
  local_mocked_bindings(.analizar_concepto = function(...)
    list(definicion = "d", dimensiones = list(A = "a"),
         referencias = list("Inventado, X. (2020). Nada.")))
  expect_warning(r <- SeMiLLa:::.respaldo_llm(NULL, "x", "es", NULL, NULL, "m",
                                              "literatura insuficiente"),
                 "llm_respaldo")
  expect_equal(r$fuente, "llm_respaldo")
  expect_null(r$referencias)
})


# --- 4. Reemplazo dirigido: el inverso sigue siendo inverso -------------------

escala_falsa <- function(inversos = TRUE, columna = NULL) {
  items <- data.frame(item = c("Disfruto estar con gente", "Evito las fiestas",
                               "Hablo con desconocidos"),
                      dimension = "Sociabilidad", stringsAsFactors = FALSE)
  if (!is.null(columna)) items$inverso <- columna
  x <- list(items = items, concepto = list(dimensiones = list(Sociabilidad = "def")),
            metadata = list(incluir_inversos = inversos, idioma = "es"))
  class(x) <- c("semilla", "list")
  x
}

test_that("la direccion sale de la columna explicita o del juez", {
  expect_true(SeMiLLa:::.direccion_item(escala_falsa(columna = c(FALSE, TRUE, FALSE)),
                                        2, NULL, "m"))
  # escala sin inversos: directo y sin gastar llamadas
  local_mocked_bindings(.clasificar_direccion_llm = function(...) stop("no debe llamarse"))
  expect_false(SeMiLLa:::.direccion_item(escala_falsa(inversos = FALSE), 2, NULL, "m"))
})

test_that("una escala del usuario (sin incluir_inversos) consulta al juez", {
  local_mocked_bindings(.clasificar_direccion_llm = function(...) TRUE)
  expect_true(SeMiLLa:::.direccion_item(escala_falsa(inversos = NULL), 2, NULL, "m"))
})

test_that("el reemplazo de un inverso se pide y se acepta solo inverso", {
  pedido_inverso <- NULL
  candidatos <- c("Me gusta ir a reuniones", "Prefiero quedarme solo en casa")
  k <- 0
  local_mocked_bindings(
    .direccion_item = function(...) TRUE,
    .generar_items_dimension = function(..., incluir_inversos, instruccion_extra) {
      pedido_inverso <<- incluir_inversos
      k <<- k + 1
      data.frame(item = candidatos[k], stringsAsFactors = FALSE)
    },
    .clasificar_direccion_llm = function(openai, modelo, item, ...)
      grepl("solo", item),
    .verificar_redundancia_item = function(...) list(redundante = FALSE,
                                                     embedding_nuevo = NULL),
    .extraer_concepto_str = function(x) "Sociabilidad"
  )
  r <- SeMiLLa:::.reemplazar_item_dirigido(escala_falsa(), NULL, "m", idx_item = 2,
                                          motivo = "redundante")
  expect_true(pedido_inverso)
  # el primer candidato era directo y se rechazo
  expect_equal(r$item, "Prefiero quedarme solo en casa")
})


# --- 5. Generador: un solo modelo para compuerta y estres ---------------------

test_that("estres_escala y simular_estructura usan los mismos parametros", {
  fs <- formals(simular_estructura); fe <- formals(estres_escala)
  expect_equal(fe$carga_propia, fs$carga_propia)
  expect_equal(fe$phi_teorico, fs$phi_teorico)
})

test_that("phi_teorico como rango simula el modelo principal con el valor central", {
  skip_on_cran()
  dims <- rep(c("A", "B"), each = 4)
  r <- simular_estructura(dims, deseabilidad = rep(0.5, 8),
                          phi_teorico = c(0.30, 0.70), fuerza_deseabilidad = 0.3,
                          n = 200, n_rep = 2, n_nucleos = 1, seed = 1,
                          verbose = FALSE)
  # con la Phi reciclada el escenario principal salia distinto del de phi = .50
  ref <- simular_estructura(dims, deseabilidad = rep(0.5, 8),
                            phi_teorico = 0.50, fuerza_deseabilidad = 0.3,
                            n = 200, n_rep = 2, n_nucleos = 1, seed = 1,
                            verbose = FALSE)
  expect_equal(r$prob_limpia, ref$prob_limpia)
  expect_equal(r$sensibilidad$rmsea_med, ref$sensibilidad$rmsea_med)
})
