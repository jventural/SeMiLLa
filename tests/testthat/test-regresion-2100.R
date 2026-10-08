# Regresiones del bloque "nombres honestos" (2.10.0). Ninguna llama a la API.

escala_min <- function() {
  x <- list(items = data.frame(numero = 1:4, item = c("a", "b", "c", "d"),
                               dimension = c("A", "A", "B", "B"),
                               stringsAsFactors = FALSE),
            embeddings = diag(4), similitud = diag(4),
            concepto = list(dimensiones = list(A = "a", B = "b")),
            metadata = list(poblacion = "adultos"))
  class(x) <- c("semilla", "list")
  x
}

# --- $separabilidad con $efa como alias ---------------------------------------

test_that("$separabilidad y su alias $efa se escriben y se borran juntos", {
  x <- SeMiLLa:::.fijar_separabilidad(escala_min(), list(precision_global = 75))
  expect_equal(x$separabilidad$precision_global, 75)
  expect_identical(x$efa, x$separabilidad)
  x <- SeMiLLa:::.fijar_separabilidad(x, NULL)
  expect_null(x$separabilidad)
  expect_null(x$efa)
})

test_that("un objeto guardado con solo $efa se sigue leyendo", {
  viejo <- escala_min(); viejo$efa <- list(precision_global = 50)
  expect_equal(SeMiLLa:::.separabilidad(viejo)$precision_global, 50)
})

test_that("el resumen imprime separabilidad, no la varianza de un EFA inexistente", {
  out <- capture.output(SeMiLLa:::.imprimir_separabilidad(
    list(n_clusters = 2, metodo = "ensemble", precision_global = 87.5, ari = 0.71,
         silhouette = 0.2)))
  expect_true(any(grepl("87.5%", out)))
  expect_false(any(grepl("Varianza|Rotacion", out)))
})

test_that("el cluster por item sale del esquema actual y del legado", {
  actual <- list(asignacion_clusters = data.frame(cluster = c(2, 1, 1)))
  expect_equal(SeMiLLa:::.cluster_por_item(actual), c(2, 1, 1))
  legado <- list(asignacion = data.frame(item_num = c(2, 1), factor_EFA = c("F2", "F1")))
  expect_equal(SeMiLLa:::.cluster_por_item(legado), c("F1", "F2"))
  expect_null(SeMiLLa:::.cluster_por_item(NULL))
})

# --- funciones obsoletas -------------------------------------------------------

test_that("efa_regularizado() avisa siempre, tambien con verbose = FALSE", {
  x <- escala_min()
  expect_warning(try(efa_regularizado(x, ejecutar = TRUE, verbose = FALSE),
                     silent = TRUE), "obsoleta")
})

test_that("plot_cargas() explica que un objeto semilla no trae cargas", {
  skip_if_not_installed("ggplot2")
  expect_error(plot_cargas(escala_min()), "plot_sankey")
})

test_that("el codigo archivado ya no se carga con el paquete", {
  expect_false(exists("efa_embeddings", envir = asNamespace("SeMiLLa"),
                      inherits = FALSE))
})

# --- seleccion de la mejor version --------------------------------------------

test_that("una mejora dentro del margen no sustituye a la mejor version", {
  sm <- SeMiLLa:::.supera_mejor
  expect_false(sm(1003, 1000, margen = 4))   # mismo veredicto, +3
  expect_true(sm(1004, 1000, margen = 4))    # mismo veredicto, +4
  expect_true(sm(1950, 1000, margen = 4))    # sube de veredicto
  expect_false(sm(990, 2000, margen = 0))    # baja de veredicto
  expect_true(sm(1000, -Inf))
})

test_that("los pesos del puntaje se pueden declarar por opcion", {
  withr::local_options(SeMiLLa.pesos_score = list(gemelo = 10))
  w <- SeMiLLa:::.pesos_score()
  expect_equal(w$gemelo, 10)
  expect_equal(w$veredicto, 1000)
})

# --- historial de cambios = diferencia real -----------------------------------

test_that(".diferencias_items compara por posicion o por numero", {
  a <- data.frame(numero = 1:3, item = c("x", "y", "z"), dimension = "A")
  b <- a; b$item[2] <- "y2"
  d <- SeMiLLa:::.diferencias_items(a, b)
  expect_equal(d$item, 2L)
  expect_equal(d$item_viejo, "y"); expect_equal(d$item_nuevo, "y2")
  c2 <- b[-1, ]
  d2 <- SeMiLLa:::.diferencias_items(a, c2)
  expect_equal(d2$item, 2L)
  expect_equal(nrow(SeMiLLa:::.diferencias_items(a, a)), 0)
})

test_that("converger_escala informa tambien lo reescrito en vueltas que no ganaron", {
  # 10 items: el bucle reescribe hasta el 40 % por vuelta (4 items).
  x <- escala_min()
  x$items <- data.frame(numero = 1:10, item = letters[1:10],
                        dimension = rep(c("A", "B"), each = 5),
                        stringsAsFactors = FALSE)
  x$embeddings <- x$similitud <- diag(10)
  scores <- c(1000, 999, 1010)    # vuelta 1 empeora, vuelta 2 gana
  marcas <- list(1:4, 5:8, 9:10)
  k <- 0
  local_mocked_bindings(
    .configurar_openai = function(...) NULL,
    .diagnostico_convergencia = function(...) {
      k <<- k + 1
      list(compuerta = list(veredicto = "NO APLICAR TODAVIA", escenario = "x",
                            redaccion = list(parametros = list(umbral_sem = 0.7)),
                            deseabilidad = list()),
           marcados = data.frame(idx = marcas[[k]], motivo = paste0("m", k)),
           aiken = NULL, score = scores[k])
    },
    .score_convergencia = function(...) scores[k],
    .reemplazar_item_dirigido = function(x, ..., idx_item)
      list(item = paste0(x$items$item[idx_item], "_nuevo"), emb = NULL),
    .texto_mal_formado = function(...) NA,
    obtener_embeddings = function(x, ...) list(embeddings = diag(10), similitud = diag(10))
  )
  r <- converger_escala(x, api_key = "x", max_iteraciones = 2L,
                        paciencia = 5L, formato_si_falla = FALSE, verbose = FALSE)
  # gano la vuelta 2, construida sobre la 1: la escala entregada trae los
  # 8 items reescritos. Antes $cambios solo listaba los 4 de la vuelta 2.
  expect_equal(r$escala$items$item[1:8], paste0(letters[1:8], "_nuevo"))
  expect_setequal(r$cambios$item, 1:8)
  expect_equal(r$cambios$iteracion[order(r$cambios$item)], rep(1:2, each = 4))
  expect_true(all(c("item_viejo", "item_nuevo", "motivo") %in% names(r$cambios)))
  expect_equal(nrow(r$registro_cambios), 8)
})
