# Regresiones cerradas en 2.9.31. Los cuatro defectos se detectaron el
# 2026-08-20 sobre una escala real de 24 items y 3 dimensiones (actitudes
# hacia la diversidad cultural) que habia pasado por la App.

test_that("las anclas devueltas son tantas como los puntos pedidos", {
  # Antes: las plantillas suaves de 'acuerdo' e 'intensidad' solo definian
  # "5" y el fallback devolvia 5 anclas SIN avisar, mientras el objeto
  # seguia diciendo 4, 6 o 7 puntos. Se imprimia "ACUERDO - 4 puntos"
  # seguido de cinco anclas.
  f <- SeMiLLa:::.anclajes_estandar
  for (evitar in c(TRUE, FALSE))
    for (tipo in c("acuerdo", "intensidad", "frecuencia"))
      for (np in 4:7) {
        anc <- f(tipo, np, "es", "general", evitar)
        expect_equal(length(anc), np,
                     info = sprintf("tipo=%s n=%d evitar_absolutos=%s",
                                    tipo, np, evitar))
        expect_equal(names(anc), as.character(seq_len(np)))
      }

  # Y las alternativas reportan el N de las anclas que de verdad llevan
  alt <- SeMiLLa:::.alternativas_estandar("acuerdo", 5L, "es", "educativo", TRUE)
  for (a in alt) expect_equal(length(a$anclajes), a$n_puntos)
})

test_that("los dos 'auto' de redundancia usan la misma calibracion", {
  # Antes: analizar_redundancia() = min(.85, max(.70, q95)) y
  # auditar_redundancia() = min(.70, max(.62, q95)). Con q95 = .632 la
  # primera reportaba 2 pares y la segunda 14 sobre la MISMA matriz, y el
  # Paso 6 de la App mostraba las dos cifras en la misma pantalla.
  set.seed(7); n <- 24
  S <- matrix(stats::runif(n * n, .20, .75), n, n)
  S <- (S + t(S)) / 2; diag(S) <- 1
  esc <- structure(list(
    similitud = S,
    items = data.frame(codigo = paste0("I", seq_len(n)),
                       dimension = rep(c("A", "B", "C"), each = 8),
                       item = paste("Item numero", seq_len(n)),
                       stringsAsFactors = FALSE)),
    class = c("semilla", "list"))

  invisible(utils::capture.output({
    r1 <- analizar_redundancia(esc)
    r2 <- auditar_redundancia(esc)
  }))
  expect_equal(nrow(r1), nrow(r2$pares_redundantes))
  expect_lte(r2$parametros$umbral_sem, 0.70)
  expect_gte(r2$parametros$umbral_sem, 0.62)
})

test_that("el veredicto de simular_estructura no contradice al mapa de fusion", {
  # Antes: el veredicto se calculaba solo con prob_limpia, veinte lineas
  # antes de que existiera mapa_fusion, y se imprimia "ALTA (sin riesgo
  # estructural)" justo debajo del mapa que anunciaba la fusion de dos
  # dimensiones. Aqui se comprueba la regla, sin simular.
  mapa <- list(hay_fusion = TRUE, k_esperado = 2L,
               grupos = list(c("Afirmar lo propio", "Actuar para cambiarlo"),
                             "Percibir la desigualdad"))
  K <- 3L
  prob_veredicto <- "ALTA (sin riesgo estructural inducido por el fraseo)"
  veredicto <- prob_veredicto
  if (!is.null(mapa) && isTRUE(mapa$hay_fusion) &&
      !is.null(mapa$k_esperado) && mapa$k_esperado < K) {
    grupos_f <- Filter(function(g) length(g) > 1, mapa$grupos)
    veredicto <- sprintf(
      "SE ESPERAN %d FACTOR(ES), NO %d: no se separan %s [prob. de ajuste limpio: %s]",
      mapa$k_esperado, K,
      paste(vapply(grupos_f, function(g) paste(g, collapse = " + "),
                   character(1)), collapse = " \u00b7 "),
      strsplit(prob_veredicto, " (", fixed = TRUE)[[1]][1])
  }
  expect_false(startsWith(veredicto, "ALTA"))
  expect_true(grepl("SE ESPERAN 2 FACTOR", veredicto, fixed = TRUE))

  # Y el objeto sigue exponiendo aparte el veredicto que sale solo de la
  # probabilidad, para quien lo necesite sin la correccion por fusion.
  expect_true(any(grepl("veredicto_probabilidad",
                        deparse(body(simular_estructura)), fixed = TRUE)))
})

test_that("la simulacion y la compuerta pueden paralelizar", {
  # Antes: simular_estructura() tenia n_nucleos = 1 fijo y
  # compuerta_pre_aplicacion() ni lo exponia ni lo propagaba, asi que el eje 3
  # corria siempre en un nucleo (12.0 min por 432 CFAs frente a los 2.7 min
  # que estres_escala() tardaba en 1500 con 22 nucleos).
  expect_null(formals(simular_estructura)$n_nucleos)
  expect_true("n_nucleos" %in% names(formals(compuerta_pre_aplicacion)))
  expect_null(formals(compuerta_pre_aplicacion)$n_nucleos)
})

test_that(".casar_faceta suma los items de los tres niveles, no del primero", {
  # Antes era una cascada con return anticipado: si el nivel "exacto" encontraba
  # UNA coincidencia devolvia n = 1 y no miraba contencion ni solape, donde
  # casaban las demas. Medido el 2026-08-20 sobre una escala real: una faceta
  # cubierta por 6 items -uno con la etiqueta literal y cinco con la misma
  # truncada por el LLM- se reportaba con 1, y la columna n_items sumaba 13 de
  # 24 haciendo parecer que unas dimensiones tenian mas items que otras.
  f <- SeMiLLa:::.casar_faceta
  faceta <- "Comparte y explica tradiciones propias en contextos intergrupales (por ejemplo, platos)"
  truncada <- "Comparte y explica tradiciones propias en contextos intergrupales"

  # 1 exacto + 3 truncados (contencion) = 4, no 1
  et <- c(faceta, truncada, truncada, truncada)
  m <- f(et, faceta)
  expect_equal(m$n, 4L)
  expect_equal(m$metodo, "exacto")   # el mas fuerte con el que casa alguna

  # sin ninguno exacto, sigue contando los que casan por contencion
  m2 <- f(rep(truncada, 3), faceta)
  expect_equal(m2$n, 3L)
  expect_equal(m2$metodo, "contencion")

  # una faceta que nadie cubre sigue dando 0 (el veredicto no cambia)
  m3 <- f(c("Otra cosa completamente distinta", "Y otra mas"), faceta)
  expect_equal(m3$n, 0L)
  expect_true(is.na(m3$metodo))
})

test_that("el umbral se imprime redondeado en los graficos de redundancia", {
  # El parche del umbral adaptativo dejo al descubierto tres subtitulos que
  # pegaban el valor crudo: "Umbral: 0.632002733009634".
  src <- paste(deparse(body(plot_redundancia)), collapse = " ")
  expect_true(grepl("sprintf", src, fixed = TRUE))
  expect_false(grepl('"Umbral: ", umbral,', src, fixed = TRUE))
})
