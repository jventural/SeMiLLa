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

test_that("la negacion y el supuesto de tiempo llegan al motivo del generador", {
  v <- data.frame(V_promedio = 0.9, V_relevancia = 0.9, V_representatividad = 0.9, V_claridad = 0.9)
  m <- SeMiLLa:::.motivo_contenido(v, c("relevancia", "claridad"), 0.8, negacion = TRUE, temporal = TRUE)
  expect_match(m, "^defecto de redaccion")
  expect_match(m, "sentido directo")
  expect_match(m, "primer ciclo")
  expect_false(grepl("validez de contenido baja", m))
})

test_that("motivos limita las razones para reescribir", {
  x <- structure(list(
    items = data.frame(dimension = c("A", "A"), item = c("Elijo mis cursos.", "Defino mis metas."),
                       stringsAsFactors = FALSE),
    metadata = list(incluir_inversos = FALSE), embeddings = NULL), class = c("semilla", "list"))
  vc <- list(v_aiken = data.frame(V_promedio = c(0.5, 0.5), V_relevancia = c(0.5, 0.5),
                                  V_representatividad = c(0.5, 0.5), V_claridad = c(0.5, 0.5)))
  # V baja en ambos, pero sin "validez" entre los motivos no se reescribe nada (ni se llama a la API).
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, motivos = c("negacion", "temporal"),
                                 verbose = FALSE)
  expect_identical(r$escala$items$item, x$items$item)
  expect_error(ajustar_validez_contenido(x, api_key = "x", validez = vc, motivos = "otro", verbose = FALSE))
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

# 2.12.3 (2026-11-01): el ajuste repuso en Dominio del entorno items de tareas
# que se habian retirado a proposito.

test_that("las facetas con forma de tarea se detectan solo en dimensiones no conductuales", {
  expect_true(all(SeMiLLa:::.forma_de_tarea(c("Organiza el tiempo de estudio",
    "Gestiona las tareas y los tramites", "Identifica y utiliza los recursos universitarios"))))
  expect_false(any(SeMiLLa:::.forma_de_tarea(c("Sensacion de control sobre las exigencias",
    "Capacidad de aprovechar las oportunidades"))))
  x <- structure(list(
    items = data.frame(dimension = c("Dominio", "Dominio", "Habitos"),
                       item = c("a", "b", "c"), stringsAsFactors = FALSE),
    concepto = list(dimensiones = list(Dominio = "Sentido de competencia para manejar el entorno",
                                       Habitos = "Conducta de estudio"),
                    caracteristicas = list(Dominio = c("Organiza el tiempo", "Gestiona los tramites"),
                                           Habitos = c("Organiza el tiempo", "Planifica"))),
    metadata = list()), class = c("semilla", "list"))
  expect_identical(SeMiLLa:::.facetas_con_forma_de_tarea(x), "Dominio")
  x$concepto$caracteristicas$Dominio <- c("Sensacion de control", "Aprovecha oportunidades que surgen")
  expect_length(SeMiLLa:::.facetas_con_forma_de_tarea(x), 0)
})

escala_dos <- function() structure(list(
  items = data.frame(numero = 1:2, dimension = c("Dominio", "Dominio"),
                     item = c("Tengo control sobre mis estudios.", "Me adapto a lo que me exigen."),
                     stringsAsFactors = FALSE),
  concepto = list(dimensiones = list(Dominio = "Sentido de competencia para manejar el entorno")),
  metadata = list(incluir_inversos = FALSE), embeddings = NULL), class = c("semilla", "list"))

vc_fila <- function(v) data.frame(V_promedio = v, V_relevancia = v, V_representatividad = v, V_claridad = v)

test_that("viejo y nuevo se juzgan en la misma tanda y un empate revierte", {
  x <- escala_dos()
  vc <- list(v_aiken = vc_fila(c(0.6, 0.9)))
  llamadas <- list()
  local_mocked_bindings(
    .configurar_openai = function(...) "cliente",
    .reemplazar_item_dirigido = function(...) list(item = "Siento que puedo con mis estudios.", emb = NULL),
    .nivel_item_llm = function(...) list(ok = TRUE, motivo = ""),
    validez_contenido = function(x, ...) {
      llamadas[[length(llamadas) + 1L]] <<- x$items$item
      list(v_aiken = vc_fila(rep(0.7, nrow(x$items))))   # nuevo = viejo: empate
    })
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, max_rondas = 1, verbose = FALSE)
  # una sola tanda con el nuevo y el viejo juntos
  expect_identical(llamadas[[1]], c("Siento que puedo con mis estudios.", "Tengo control sobre mis estudios."))
  expect_true(r$registro$revertido[1])
  expect_identical(r$escala$items$item, x$items$item)
})

test_that("un candidato fuera de nivel se descarta y el item queda igual", {
  x <- escala_dos()
  vc <- list(v_aiken = vc_fila(c(0.6, 0.9)))
  local_mocked_bindings(
    .configurar_openai = function(...) "cliente",
    .reemplazar_item_dirigido = function(...) list(item = "Ajusto mi espacio de estudio.", emb = NULL),
    .nivel_item_llm = function(...) list(ok = FALSE, motivo = "describe una conducta"),
    validez_contenido = function(...) stop("no deberia juzgar")
  )
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, max_rondas = 1, verbose = FALSE)
  expect_identical(r$escala$items$item, x$items$item)
  expect_match(r$registro$nota[1], "fuera de nivel")
  expect_true(r$registro$revertido[1])
})

test_that("un reemplazo que supera al viejo en la misma tanda se acepta", {
  x <- escala_dos()
  vc <- list(v_aiken = vc_fila(c(0.6, 0.9)))
  local_mocked_bindings(
    .configurar_openai = function(...) "cliente",
    .reemplazar_item_dirigido = function(...) list(item = "Siento que puedo con mis estudios.", emb = NULL),
    .nivel_item_llm = function(...) list(ok = TRUE, motivo = ""),
    validez_contenido = function(x, ...) list(v_aiken = vc_fila(
      if (nrow(x$items) == 2 && x$items$item[1] == "Siento que puedo con mis estudios.") c(0.9, 0.7)
      else rep(0.85, nrow(x$items))))
  )
  r <- ajustar_validez_contenido(x, api_key = "x", validez = vc, max_rondas = 1, verbose = FALSE)
  expect_false(r$registro$revertido[1])
  expect_equal(r$registro$V_antes[1], 0.7)
  expect_identical(r$escala$items$item[1], "Siento que puedo con mis estudios.")
})
