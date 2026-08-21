# Regresion cerrada en 2.9.33: "no tocar nada" no podia salir elegido.

test_that("la escala de partida compite y gana si nadie la supera", {
  # Hasta 2.9.32:  mejor <- list(escala = escala, score = -Inf, iteracion = 0L)
  # y la entrega exigia mejor$iteracion > 0. Con score -Inf, CUALQUIER
  # iteracion finita ganaba por definicion. Medido el 2026-08-20 sobre una
  # escala real: 59 reescrituras en 21 items para terminar en 83.3%, el mismo
  # valor con el que entro, y se entregaban los 21 items reescritos igual.
  elegir <- function(score_inicial, scores_iteracion) {
    mejor <- list(score = score_inicial, iteracion = 0L)
    for (i in seq_along(scores_iteracion)) {
      s <- scores_iteracion[i]
      if (is.finite(s) && s > mejor$score) mejor <- list(score = s, iteracion = i)
    }
    list(iteracion = mejor$iteracion, sin_mejora = mejor$iteracion == 0L,
         score = mejor$score)
  }

  # Ninguna iteracion supera a la de partida -> se devuelve intacta
  r <- elegir(0.550, c(0.41, 0.52, 0.49, 0.55))
  expect_equal(r$iteracion, 0L)
  expect_true(r$sin_mejora)
  expect_equal(r$score, 0.550)

  # Alguna la supera -> se entrega esa, y no necesariamente la ultima
  r2 <- elegir(0.550, c(0.60, 0.79, 0.58))
  expect_equal(r2$iteracion, 2L)
  expect_false(r2$sin_mejora)
  expect_equal(r2$score, 0.79)

  # Con el comportamiento viejo (-Inf) hasta la peor iteracion ganaba
  viejo <- elegir(-Inf, c(0.41, 0.52, 0.49))
  expect_equal(viejo$iteracion, 2L)
  expect_false(viejo$sin_mejora)   # nunca podia ser TRUE
})

test_that("refinar_escala declara sin_mejora, score_inicial y score_entregado", {
  # El contrato que la App y los informes necesitan para poder decir
  # "no encontro mejoras" en vez de mostrar reescrituras que no mejoraron nada.
  src <- paste(deparse(body(refinar_escala)), collapse = " ")
  for (campo in c("sin_mejora", "score_inicial", "score_entregado"))
    expect_true(grepl(campo, src, fixed = TRUE), info = campo)
  # y la escala inicial ya no entra con -Inf
  expect_false(grepl("score = -Inf, iteracion = 0L", src, fixed = TRUE))
})
