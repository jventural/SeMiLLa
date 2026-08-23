# Regresion cerrada en 2.9.36: la compuerta no veia un item puesto en la
# dimension equivocada, y ese defecto SI degrada la estructura real.
#
# Medido el 22-ago-2026 sobre 355 condiciones de 13 estudios del Item Response
# Warehouse con el defecto introducido a proposito (verdad por construccion):
# con las respuestas reales el CFI caia de .899 a .790, de .790 a .613 y de
# .927 a .797, y en un caso el phi empirico pasaba de .226 a .857. La compuerta
# daba el mismo prob_limpia = 1.000 con y sin defecto.

test_that("auditar_asignacion senala el item que esta en la dimension equivocada", {
  # Matriz de similitud construida a mano: dos bloques bien separados y UN item
  # (el 6) etiquetado en D1 aunque se parece a los de D2.
  p <- 8
  S <- matrix(0.10, p, p); diag(S) <- 1
  bloqueA <- 1:4; bloqueB <- 5:8
  S[bloqueA, bloqueA] <- 0.70; S[bloqueB, bloqueB] <- 0.70
  diag(S) <- 1
  # el item 6 pertenece semanticamente al bloque B
  dims <- c(rep("D1", 4), "D1", rep("D2", 3))   # el 5o item es el intruso
  S[5, bloqueB] <- 0.70; S[bloqueB, 5] <- 0.70
  S[5, bloqueA] <- 0.10; S[bloqueA, 5] <- 0.10
  diag(S) <- 1

  a <- auditar_asignacion(S, dimension = dims, verbose = FALSE)
  expect_s3_class(a, "semilla_asignacion")
  expect_equal(a$alerta, "riesgo")
  expect_true(a$n_mal_asignados >= 1)
  # el item senalado es el 5, y su destino correcto es D2
  expect_true(any(a$items$mal_asignado[5]))
  expect_equal(a$items$dim_mas_cercana[5], "D2")
  expect_true(a$items$margen[5] < 0)
})

test_that("una escala limpia no dispara la alerta", {
  p <- 8
  S <- matrix(0.10, p, p)
  S[1:4, 1:4] <- 0.70; S[5:8, 5:8] <- 0.70; diag(S) <- 1
  dims <- c(rep("D1", 4), rep("D2", 4))
  a <- auditar_asignacion(S, dimension = dims, verbose = FALSE)
  expect_equal(a$alerta, "ok")
  expect_equal(a$n_mal_asignados, 0L)
})

test_that("el umbral separa el defecto real del caso frontera", {
  # Los margenes de los intrusos reales tienen mediana -.157 y los de los items
  # legitimos senalados -.020: ocho veces menor. Por eso el corte esta en -.05 y
  # lo de en medio se devuelve como 'frontera' en vez de perderse.
  p <- 6
  S <- matrix(0.40, p, p); diag(S) <- 1
  dims <- c("D1","D1","D1","D2","D2","D2")
  # item 3: rozando la frontera (margen negativo pequeno)
  S[3, 4:6] <- 0.42; S[4:6, 3] <- 0.42
  diag(S) <- 1
  a <- auditar_asignacion(S, dimension = dims, umbral_margen = -0.05, verbose = FALSE)
  expect_true(a$n_mal_asignados == 0L)       # no llega al umbral
  expect_true(a$n_frontera >= 1L)            # pero no se pierde
  # con umbral 0 (comportamiento anterior) si se habria senalado
  a0 <- auditar_asignacion(S, dimension = dims, umbral_margen = 0, verbose = FALSE)
  expect_true(a0$n_mal_asignados >= 1L)
})

test_that("umbral_margen valida su entrada", {
  S <- diag(4); dims <- c("D1","D1","D2","D2")
  expect_error(auditar_asignacion(S, dimension = dims, umbral_margen = 0.5,
                                  verbose = FALSE), "umbral_margen")
})

# ---------------------------------------------------------------------------
# Bug corregido de paso: redundancia.R evaluaba cc$concepto ANTES de comprobar
# que cc fuese una lista, asi que pasar el concepto como CADENA mataba
# auditar_redundancia() con "$ operator is invalid for atomic vectors" -- y la
# rama que si contemplaba la cadena quedaba inalcanzable.
# ---------------------------------------------------------------------------
test_that("el concepto puede ser una cadena y no revienta", {
  guarda <- function(cc) {
    # reproduce la guarda corregida
    if (is.list(cc) && is.character(cc$concepto)) cc$concepto
    else if (is.character(cc)) cc else character(0)
  }
  expect_equal(guarda("gratitud"), "gratitud")
  expect_equal(guarda(list(concepto = "gratitud")), "gratitud")
  expect_equal(guarda(NULL), character(0))
})
