test_that("ajustar_validez_contenido no toca una escala sin items que corregir", {
  x <- structure(list(
    items = data.frame(dimension = c("A", "A"), item = c("Elijo mis cursos.", "Defino mis metas."),
                       stringsAsFactors = FALSE),
    metadata = list(incluir_inversos = FALSE), embeddings = NULL), class = c("semilla", "list"))
  vc <- list(v_aiken = data.frame(V_promedio = c(0.9, 0.85), V_relevancia = c(0.9, 0.85),
                                  V_representatividad = c(0.9, 0.85), V_claridad = c(0.9, 0.9)))
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, verbose = FALSE)
  expect_s3_class(r, "semilla_ajuste_contenido")
  expect_identical(r$escala$items$item, x$items$item)
  expect_equal(nrow(r$registro), 0)
})

test_that("la similitud se rehace cuando no corresponde a los embeddings", {
  E <- rbind(c(1, 0, 0), c(0.9, 0.1, 0), c(0, 0, 1))
  vieja <- SeMiLLa:::.calcular_similitud_coseno(rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
  x <- structure(list(
    items = data.frame(dimension = c("A", "A", "B"), item = c("Elijo mis cursos.", "Defino mis metas.",
                       "Confio en mis companeros."), stringsAsFactors = FALSE),
    metadata = list(incluir_inversos = FALSE), embeddings = E, similitud = vieja),
    class = c("semilla", "list"))
  y <- SeMiLLa:::.sincronizar_similitud(x)
  expect_equal(unname(y$similitud), unname(SeMiLLa:::.calcular_similitud_coseno(E)))
  expect_warning(SeMiLLa:::.sincronizar_similitud(x, avisar = TRUE), "no correspondia")
  expect_silent(SeMiLLa:::.sincronizar_similitud(y, avisar = TRUE))
  # Sin embeddings no hay con que comparar: el objeto queda igual.
  x$embeddings <- NULL
  expect_identical(SeMiLLa:::.sincronizar_similitud(x), x)
})

test_that("ajustar_validez_contenido devuelve la similitud de los items finales", {
  E <- rbind(c(1, 0), c(0.6, 0.8))
  x <- structure(list(
    items = data.frame(dimension = c("A", "A"), item = c("Elijo mis cursos.", "Defino mis metas."),
                       stringsAsFactors = FALSE),
    metadata = list(incluir_inversos = FALSE), embeddings = E, similitud = diag(2)),
    class = c("semilla", "list"))
  vc <- list(v_aiken = data.frame(V_promedio = c(0.9, 0.85), V_relevancia = c(0.9, 0.85),
                                  V_representatividad = c(0.9, 0.85), V_claridad = c(0.9, 0.9)))
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, verbose = FALSE)
  expect_equal(unname(r$escala$similitud), unname(SeMiLLa:::.calcular_similitud_coseno(E)))
})

test_that("una negacion encubierta se marca aunque la V sea alta", {
  expect_true(SeMiLLa:::.negacion_encubierta("Mis notas no definen mi valor."))
  expect_false(SeMiLLa:::.negacion_encubierta("Defino mis prioridades academicas."))
})

test_that("el item se juzga por su criterio mas bajo", {
  v <- data.frame(V_promedio = c(0.85, 0.9), V_relevancia = c(0.9, 0.9),
                  V_representatividad = c(0.9, 0.9), V_claridad = c(0.7, 0.9))
  expect_equal(SeMiLLa:::.v_minima(v, c("relevancia", "representatividad", "claridad")), c(0.7, 0.9))
  expect_equal(SeMiLLa:::.v_minima(v[, 1:3], c("relevancia", "claridad")), c(0.9, 0.9))
  m <- SeMiLLa:::.motivo_contenido(v[1, ], c("relevancia", "claridad"), 0.8, jerga = TRUE)
  expect_match(m, "claridad baja")
  expect_match(m, "registro abstracto")
})

test_that("el registro abstracto marca la jerga y no los items cotidianos", {
  expect_true(SeMiLLa:::.jerga_abstracta("Puedo ajustar mis estudios a mis necesidades formativas."))
  expect_true(SeMiLLa:::.jerga_abstracta("Acepto mis dificultades académicas como parte de mi identidad estudiantil."))
  expect_false(SeMiLLa:::.jerga_abstracta("Mis cursos cuestionan ideas que daba por seguras."))
  expect_false(SeMiLLa:::.jerga_abstracta("Mi carrera se relaciona con mis metas personales."))
})

test_that("los indicadores de POLEN se leen de la definicion", {
  d <- "Capacidad de decidir. No incluye: metas (Proposito).. Se manifiesta en: Elijo por mi cuenta.; Sostengo mi criterio.; Asumo mis decisiones.."
  expect_equal(SeMiLLa:::.indicadores_definicion(d),
               c("Elijo por mi cuenta", "Sostengo mi criterio", "Asumo mis decisiones"))
  expect_length(SeMiLLa:::.indicadores_definicion("Capacidad de decidir."), 0)
})
