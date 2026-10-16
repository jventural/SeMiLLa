# 16-10-2026: compuerta mas rapida (jueces en una tanda, banda central, cluster
# compartido, lavaan sin SE). Los cambios son de velocidad: estos tests
# comprueban que el resultado no se mueve.

test_that("las tandas mixtas devuelven cada respuesta en su lote y en orden", {
  local_mocked_bindings(
    .auto_proveedor_por_clave = function(...) invisible(NULL),
    .chat_secuencial = function(prompts, api_key, modelo, max_tokens, temperature,
                                razonamiento, intentos = 2L, seed = NULL, top_p = NULL)
      lapply(prompts, function(p) paste0(p, "|", max_tokens)),
    .package = "SeMiLLa")
  r <- SeMiLLa:::.chat_en_paralelo_lotes(
    list(list(prompts = c("a", "b"), max_tokens = 10L),
         list(prompts = character(0), max_tokens = 20L),
         list(prompts = "c", max_tokens = 30L)),
    api_key = "x", modelo = "m", max_paralelo = 1L)
  expect_length(r, 3)
  expect_equal(unlist(r[[1]]), c("a|10", "b|10"))
  expect_length(r[[2]], 0)
  expect_equal(unlist(r[[3]]), "c|30")
})

test_that("el juez de gemelos lee igual las respuestas traidas de fuera", {
  items <- data.frame(item = c("Me enojo rapido", "Me irrito enseguida", "Leo mucho"),
                      dimension = c("A", "A", "B"))
  si <- "{\"gemelos\": [{\"items\": [1, 2], \"razon\": \"misma emocion\"}]}"
  no <- "{\"gemelos\": []}"
  g <- SeMiLLa:::.juzgar_parafrasis_votado(items, NULL, n_pasadas = 3,
                                           respuestas = list(si, si, no))
  expect_equal(nrow(g), 1)
  expect_equal(c(g$item1, g$item2), c(1L, 2L))
  expect_equal(g$votos, 2L)
  expect_equal(attr(g, "banda"), c(0L, 1L))
  # una pasada caida cuenta como fallo, no como "sin gemelos"
  g2 <- SeMiLLa:::.juzgar_parafrasis_votado(items, NULL, n_pasadas = 3,
                                            respuestas = list(NULL, NULL, NULL))
  expect_false(is.null(attr(g2, "fallo_llm")))
})

test_that("plan + llenado + resumen dan lo mismo que calificar_deseabilidad()", {
  x <- semilla_demo
  resp_fija <- function(prompts) lapply(prompts, function(p) {
    k <- as.integer(sub(".* con ([0-9]+) numeros.*", "\\1", gsub("\n", " ", p)))
    paste0("{\"d\": [", paste(round(seq(0.2, 0.8, length.out = k), 2), collapse = ","), "]}")
  })
  local_mocked_bindings(
    .chat_en_paralelo = function(prompts, ...) resp_fija(prompts),
    .package = "SeMiLLa")
  a <- calificar_deseabilidad(x, api_key = "x", n_pasadas = 4, seed = 11, verbose = FALSE)
  pl <- SeMiLLa:::.plan_deseabilidad(x, n_pasadas = 4, seed = 11)
  b <- SeMiLLa:::.resumir_deseabilidad(SeMiLLa:::.llenar_deseabilidad(pl, resp_fija(pl$prompts)),
                                       pl$dims, verbose = FALSE)
  expect_equal(a, b)
})

test_that("la banda simula solo la fuerza central y sale igual que antes", {
  x <- semilla_demo
  p <- nrow(x$items)
  pas <- matrix(rep(seq(0.3, 0.8, length.out = p), 4), p, 4)
  llamadas <- list()
  real <- SeMiLLa::simular_estructura
  local_mocked_bindings(
    calificar_deseabilidad = function(x, n_pasadas = 4, ...) {
      d <- rowMeans(pas); pd <- tapply(d, x$items$dimension, mean)
      list(deseabilidad = d, por_dimension = pd, sd_entre_dim = stats::sd(pd),
           sd_intra_dim = 0.1, alerta_intra = FALSE, estabilidad = 0.9,
           n_imputados = 0, uniforme = FALSE, riesgo_halo = FALSE,
           mensaje = "fijo", pasadas = pas)
    },
    .juzgar_parafrasis_votado = function(...)
      data.frame(item1 = integer(0), item2 = integer(0), razon = character(0)),
    .decision_disponible = function(...) FALSE,
    .configurar_openai = function(...) NULL,
    .chat_en_paralelo_lotes = function(...) stop("sin red"),
    simular_estructura = function(...) {
      a <- list(...); llamadas[[length(llamadas) + 1L]] <<- a$fuerza_deseabilidad %||% "todas"
      real(...)
    },
    .package = "SeMiLLa")
  g <- compuerta_pre_aplicacion(x, api_key = "x", n_rep = 10, n_banda = 2,
                                n_nucleos = 1, seed = 2026, verbose = FALSE)
  expect_equal(llamadas[[1]], "todas")              # principal: las 3 fuerzas
  expect_equal(unlist(llamadas[-1]), c(0.6, 0.6))   # banda: solo la central
  # la banda coincide con el escenario central de la simulacion completa
  set.seed(2026 + 1000L + 1L)
  v <- rowMeans(pas[, sample.int(4, 4, replace = TRUE), drop = FALSE])
  s <- real(x, deseabilidad = v, similitud = x$similitud, n = 300, n_rep = 10,
            n_nucleos = 1, seed = 2026, verbose = FALSE)
  expect_equal(g$banda_estructura$probs[1], s$prob_limpia)
})
