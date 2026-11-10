# 2.12.5: fronteras entre dimensiones (generador y blindaje), phi por par con
# banda objetivo, regla de halo condicionada y segunda lectura de la asignacion.

test_that("el bloque de fronteras nombra las otras dimensiones y no la propia", {
  otras <- list(
    Autonomia = "Decidir por cuenta propia. No incluye: dominio del entorno.",
    Dominio   = "Sentir que se maneja el entorno. Se manifiesta en: cumplir plazos.",
    Proposito = "Tener metas que dan direccion.")
  t <- SeMiLLa:::.texto_fronteras("Dominio", otras, c("Elijo por mi cuenta", "Tengo metas"))
  expect_match(t, "FRONTERAS CON LAS OTRAS DIMENSIONES")
  expect_match(t, "- Autonomia: Decidir por cuenta propia\\.")
  expect_match(t, "- Proposito: Tener metas")
  expect_false(grepl("- Dominio:", t, fixed = TRUE))
  # el "No incluye" de otra dimension no viaja: nombra el contenido de esta
  expect_false(grepl("No incluye", t, fixed = TRUE))
  expect_match(t, "\"Elijo por mi cuenta\"", fixed = TRUE)
  # sin otras dimensiones no se agrega nada al prompt
  expect_identical(SeMiLLa:::.texto_fronteras("Dominio", NULL), "")
  expect_identical(SeMiLLa:::.texto_fronteras("Dominio", list(Dominio = "x")), "")
})

test_that("el generador recibe las fronteras en el prompt", {
  visto <- NULL
  local_mocked_bindings(
    .llamar_openai = function(openai, messages, ...) {
      visto <<- messages[[1]]$content
      "{\"items\": [{\"item\": \"Decido por mi cuenta\", \"caracteristica\": \"a\"}]}"
    },
    .package = "SeMiLLa")
  r <- SeMiLLa:::.generar_items_dimension(
    openai = NULL, concepto = "Bienestar", dimension = "Autonomia",
    definicion_dim = "Decidir por cuenta propia", caracteristicas = NULL, n_items = 1,
    idioma = "es", poblacion = NULL, modelo = "m",
    otras_dimensiones = list(Autonomia = "Decidir", Proposito = "Tener metas"),
    items_otras = "Tengo metas claras")
  expect_equal(nrow(r), 1)
  expect_match(visto, "- Proposito: Tener metas", fixed = TRUE)
  expect_match(visto, "\"Tengo metas claras\"", fixed = TRUE)
})

test_that("el juez de fronteras marca el item que se lee como de otra dimension", {
  items <- data.frame(item = c("Decido por mi cuenta", "Tengo metas claras", "Sigo mis metas"),
                      dimension = c("Autonomia", "Proposito", "Autonomia"),
                      stringsAsFactors = FALSE)
  dims <- list(Autonomia = "Decidir por cuenta propia", Proposito = "Tener metas")
  local_mocked_bindings(
    .llamar_openai = function(...) "{\"1\": \"Autonomia\", \"2\": \"proposito\", \"3\": \"Proposito\"}",
    .package = "SeMiLLa")
  f <- SeMiLLa:::.juzgar_frontera_llm(items, dims, NULL, "m")
  expect_equal(f$numero, 3L)
  expect_match(f$problema, "Proposito")
  # una sola dimension, o una llamada caida, no marca nada
  expect_equal(nrow(SeMiLLa:::.juzgar_frontera_llm(items, list(Autonomia = "x"), NULL, "m")), 0)
  local_mocked_bindings(.llamar_openai = function(...) stop("sin red"), .package = "SeMiLLa")
  expect_equal(nrow(SeMiLLa:::.juzgar_frontera_llm(items, dims, NULL, "m")), 0)
})

test_that("la escala de anclas de phi llega a .90 y conserva la de 8 como opcion", {
  a <- SeMiLLa:::.anclas_phi()
  expect_length(a$anclas, length(a$valores))
  expect_gt(max(a$valores), 0.85)
  withr::local_options(SeMiLLa.anclas_phi = "8")
  b <- SeMiLLa:::.anclas_phi()
  expect_length(b$anclas, 8)
  expect_equal(max(b$valores), 0.72)
})

test_that("el prompt de phi usa la poblacion y reparte los items de ejemplo", {
  x <- list(items = data.frame(item = paste("item", 1:24),
                               dimension = rep(c("A", "B"), each = 12),
                               stringsAsFactors = FALSE),
            concepto = list(dimensiones = list(A = "def a", B = "def b")),
            metadata = list(poblacion = "estudiantes universitarios"))
  pp <- SeMiLLa:::.prompts_phi(x, n_ejemplos = 8L)
  expect_length(pp$prompts, 1)
  expect_match(pp$prompts, "muestra grande de estudiantes universitarios")
  expect_match(pp$prompts, "item 12", fixed = TRUE)   # el ultimo de A, no solo los primeros
  expect_false(grepl("item 2 ", pp$prompts, fixed = TRUE))
  x$metadata$poblacion <- NULL
  expect_match(SeMiLLa:::.prompts_phi(x)$prompts, "muestra grande de adultos")
})

test_that("la banda objetivo clasifica cada par de dimensiones", {
  Phi <- matrix(c(1, .80, .10, .80, 1, .45, .10, .45, 1), 3,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  b <- SeMiLLa:::.banda_phi(Phi, c(0.30, 0.65))
  expect_equal(nrow(b), 3)
  expect_equal(b$estado, c("alta", "dentro", "baja"))
  expect_equal(b$dimension_a[1], "A"); expect_equal(b$dimension_b[1], "B")
  expect_null(SeMiLLa:::.banda_phi(NULL, c(0.30, 0.65)))
  expect_null(SeMiLLa:::.banda_phi(Phi, NULL))
})

test_that("el halo solo une un par si su phi estimada llega al umbral pedido", {
  d <- c("A", "B", "C")
  sim <- matrix(0.40, 3, 3, dimnames = list(d, d)); diag(sim) <- 1
  des <- c(A = 0.90, B = 0.88, C = 0.86)
  # regla de siempre: las tres son deseables y parejas -> un solo factor
  m0 <- SeMiLLa:::.mapa_fusion(sim, umbral = 0.65, des_por_dim = des)
  expect_equal(m0$k_esperado, 1)
  est <- matrix(c(1, .70, .30, .70, 1, .35, .30, .35, 1), 3, dimnames = list(d, d))
  m1 <- SeMiLLa:::.mapa_fusion(sim, umbral = 0.65, des_por_dim = des,
                               phi_estimado = est, min_phi_halo = 0.55)
  expect_equal(m1$k_esperado, 2)
  expect_true(any(vapply(m1$grupos, function(g) setequal(g, c("A", "B")), logical(1))))
})

test_that("la compuerta informa la banda y los items puente sin tocar el riesgo", {
  x <- semilla_demo
  p <- nrow(x$items)
  dims <- unique(as.character(x$items$dimension))
  skip_if(length(dims) < 2)
  pas <- matrix(rep(seq(0.3, 0.8, length.out = p), 4), p, 4)
  Phi <- matrix(0.85, length(dims), length(dims), dimnames = list(dims, dims)); diag(Phi) <- 1
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
    .decision_disponible = function(...) TRUE,
    .estimar_phi_pares = function(...) Phi,
    .prompts_phi = function(...) NULL,
    .configurar_openai = function(...) NULL,
    .chat_en_paralelo_lotes = function(...) stop("sin red"),
    probabilidad_pertenencia = function(x, ...) {
      it <- x$items
      data.frame(item = it$item, dimension = it$dimension,
                 prob_pertenencia = c(0.20, rep(0.90, nrow(it) - 1)),
                 dimension_probable = c(setdiff(dims, it$dimension[1])[1],
                                        as.character(it$dimension[-1])),
                 stringsAsFactors = FALSE)
    },
    .package = "SeMiLLa")
  g <- compuerta_pre_aplicacion(x, api_key = "x", n_rep = 6, n_banda = 0,
                                n_nucleos = 1, seed = 1, verbose = FALSE)
  expect_true(all(g$relacion_dimensiones$estado == "alta"))
  expect_true(any(grepl("POR ENCIMA de la banda", g$acciones)))
  expect_equal(g$items_puente, 1L)
  expect_true(g$semaforo$estado[g$semaforo$paso == "asignacion"] %in% c("advertencia", "riesgo"))
  # apagadas, no dejan rastro
  g0 <- compuerta_pre_aplicacion(x, api_key = "x", n_rep = 6, n_banda = 0, n_nucleos = 1,
                                 seed = 1, verbose = FALSE, phi_objetivo = NULL,
                                 pertenencia = FALSE)
  expect_null(g0$relacion_dimensiones)
  expect_null(g0$items_puente)
})
