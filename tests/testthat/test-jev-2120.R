# Modelo de decision (Jev) y probabilidad de pertenencia. Ninguna prueba llama
# a la API: .llamar_decisiones() y el lector de chat se sustituyen.

esc_prueba <- function() {
  list(items = data.frame(codigo = paste0("Item_", 1:4),
                          item = c("Me siento triste", "Nada me alegra",
                                   "Me tiemblan las manos", "Siento panico"),
                          dimension = c("Depresion", "Depresion", "Ansiedad", "Ansiedad"),
                          stringsAsFactors = FALSE))
}

test_that("Jev solo se usa con clave de OpenRouter y se puede apagar", {
  expect_true(SeMiLLa:::.decision_disponible("sk-or-v1-abc"))
  expect_false(SeMiLLa:::.decision_disponible("sk-proj-abc"))
  expect_false(SeMiLLa:::.decision_disponible(NA_character_))
  viejo <- options(SeMiLLa.usar_jev = FALSE); on.exit(options(viejo))
  expect_false(SeMiLLa:::.decision_disponible("sk-or-v1-abc"))
})

test_that(".probs_decision ordena y normaliza las probabilidades", {
  a <- list(probabilities = list(Ansiedad = 0.2, Depresion = 0.6))
  expect_equal(unname(SeMiLLa:::.probs_decision(a, c("Depresion", "Ansiedad", "Estres"))),
               c(0.75, 0.25, 0))
})

test_that("la pertenencia promedia Jev y el juez, y el consenso solo marca discrepancias", {
  local_mocked_bindings(
    .llamar_decisiones = function(preguntas, ...) {
      r <- lapply(seq_along(preguntas), function(i)
        list(probabilities = if (i == 2) list(Depresion = 0.2, Ansiedad = 0.8)
                             else if (i <= 2) list(Depresion = 0.9, Ansiedad = 0.1)
                             else list(Depresion = 0.1, Ansiedad = 0.9)))
      names(r) <- names(preguntas); attr(r, "costo") <- 0; r
    },
    .pertenencia_chat = function(textos, dims, ...) {
      M <- matrix(c(1, 0, 0.4, 0.6, 0, 1, 0, 1), ncol = 2, byrow = TRUE); M
    })
  ens <- list(consenso = data.frame(Codigo = paste0("Item_", 1:4), Consenso = c(1, 1, 0.3, 1)))
  pp <- probabilidad_pertenencia(esc_prueba(), api_key = "sk-or-v1-x", ensemble = ens, verbose = FALSE)
  expect_equal(pp$prob_pertenencia, c(0.95, 0.3, 0.95, 0.95))
  expect_equal(pp$pertenece, c(TRUE, FALSE, TRUE, TRUE))
  # item 2: los lectores dicen que no pertenece y el clustering que si -> discrepancia
  # item 3: los lectores dicen que si y el clustering que no -> discrepancia
  expect_equal(pp$discrepancia, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(pp$dimension_probable[2], "Ansiedad")
  expect_setequal(attr(pp, "lectores"), c("jev", "juez"))
})

test_that("sin clave de OpenRouter la pertenencia usa solo el lector de chat", {
  local_mocked_bindings(
    .llamar_decisiones = function(...) stop("no deberia llamarse"),
    .pertenencia_chat = function(textos, dims, ...) matrix(c(1, 0, 1, 0, 0, 1, 0, 1), ncol = 2, byrow = TRUE))
  pp <- probabilidad_pertenencia(esc_prueba(), api_key = "sk-proj-x", verbose = FALSE)
  expect_equal(attr(pp, "lectores"), "juez")
  expect_true(all(pp$pertenece))
})

test_that("la matriz de phi estimada se corrige a definida positiva con diagonal 1", {
  Phi <- matrix(c(1, .7, .05, .7, 1, .7, .05, .7, 1), 3)
  M <- SeMiLLa:::.phi_definida_positiva(Phi)
  expect_gte(min(eigen(M, symmetric = TRUE)$values), 0.049)
  expect_equal(unname(diag(M)), c(1, 1, 1))
  expect_identical(SeMiLLa:::.phi_definida_positiva(diag(3)), diag(3))
})
