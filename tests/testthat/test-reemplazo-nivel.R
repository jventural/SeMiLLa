# Reemplazos de la optimizacion guiada por la compuerta (caso 2026-10-14):
# deben respetar el estilo del usuario, el nivel de la definicion y, en una
# escala sin inversos, no colar items del polo opuesto.

escala_directa <- function() {
  items <- data.frame(item = c("Decido por criterio propio", "Manejo mis demandas academicas"),
                      dimension = "Dominio", stringsAsFactors = FALSE)
  x <- list(items = items,
            concepto = list(dimensiones = list(Dominio = paste(
              "Competencia para manejar las demandas academicas. No incluye: metas.",
              "Se manifiesta en: Siento que manejo bien mi tiempo."))),
            metadata = list(incluir_inversos = FALSE, idioma = "es",
                            instrucciones_estilo = "ESTILO-DE-PRUEBA"))
  class(x) <- c("semilla", "list")
  x
}

test_that("el tipo de dimension ignora indicadores y exclusiones", {
  d <- escala_directa()$concepto$dimensiones$Dominio
  expect_equal(SeMiLLa:::.tipo_dimension("Dominio", d), "general")
  expect_equal(SeMiLLa:::.tipo_dimension("Ansiedad", "Reaccion emocional ante examenes"),
               "afectiva")
})

test_that("en una escala sin inversos el reemplazo del polo opuesto se rechaza", {
  extra <- NULL
  candidatos <- c("Me abruma la cantidad de pendientes", "Me siento capaz de organizar mis demandas")
  k <- 0
  local_mocked_bindings(
    .direccion_item = function(...) FALSE,
    .generar_items_dimension = function(..., incluir_inversos, instruccion_extra) {
      extra <<- instruccion_extra
      k <<- k + 1
      data.frame(item = candidatos[k], stringsAsFactors = FALSE)
    },
    .clasificar_direccion_llm = function(openai, modelo, item, ...) grepl("abruma", item),
    .verificar_redundancia_item = function(...) list(redundante = FALSE, embedding_nuevo = NULL),
    .extraer_concepto_str = function(x) "Dominio"
  )
  r <- SeMiLLa:::.reemplazar_item_dirigido(escala_directa(), NULL, "m", idx_item = 2,
                                          motivo = "halo", anti_halo = TRUE,
                                          items_dimension_conservados = "Decido por criterio propio")
  expect_equal(r$item, "Me siento capaz de organizar mis demandas")
  expect_match(extra, "ESTILO-DE-PRUEBA", fixed = TRUE)
  expect_match(extra, "mismo nivel", ignore.case = TRUE)
  expect_false(grepl("reaccion fisica o un pensamiento", extra, fixed = TRUE))
  expect_false(grepl("costo personal", extra, fixed = TRUE))
})

test_that("no_empeorar relaja el umbral hasta la redundancia del item sustituido", {
  x <- escala_directa()
  emb <- rbind(c(1, 0), c(0.8, 0.6))   # coseno entre los dos items = 0.8
  umbral_visto <- NULL
  local_mocked_bindings(
    .direccion_item = function(...) FALSE,
    .generar_items_dimension = function(...) data.frame(item = "Me siento capaz de organizar mis demandas"),
    .clasificar_direccion_llm = function(...) FALSE,
    .verificar_redundancia_item = function(..., umbral) {
      umbral_visto <<- umbral
      list(redundante = FALSE, embedding_nuevo = NULL)
    },
    .extraer_concepto_str = function(x) "Dominio"
  )
  SeMiLLa:::.reemplazar_item_dirigido(x, NULL, "m", idx_item = 2, motivo = "V baja",
                                     umbral_redundancia = 0.59, embeddings_existentes = emb,
                                     no_empeorar = TRUE)
  expect_equal(umbral_visto, 0.8, tolerance = 1e-8)
  SeMiLLa:::.reemplazar_item_dirigido(x, NULL, "m", idx_item = 2, motivo = "redundante",
                                     umbral_redundancia = 0.59, embeddings_existentes = emb)
  expect_equal(umbral_visto, 0.59)
})
