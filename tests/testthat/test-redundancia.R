test_that("auditar_redundancia detecta homogeneidad sintactica", {
  mk <- function(textos) {
    n <- length(textos)
    S <- matrix(0.3, n, n); diag(S) <- 1
    structure(list(
      similitud = S,
      items = data.frame(codigo = paste0("I", seq_len(n)),
                         dimension = "D", item = textos,
                         stringsAsFactors = FALSE)),
      class = c("semilla", "list"))
  }

  # Items con plantilla casi identica -> debe disparar alerta
  homog <- mk(c(
    "Si mi pareja llega tarde, me sentiria molesto",
    "Si mi pareja no responde, me sentiria ansioso",
    "Si mi pareja sale sin avisar, me sentiria inseguro",
    "Si mi pareja mira a otra persona, me sentiria celoso"))
  a1 <- auditar_redundancia(homog)
  expect_s3_class(a1, "semilla_redundancia")
  expect_true(a1$homogeneidad_sintactica$alerta)
  expect_true(a1$homogeneidad_sintactica$prefijo_compartido >= 0.5)

  # Items lexicamente diversos -> no debe disparar alerta
  diverso <- mk(c(
    "Disfruto aprender cosas nuevas cada dia",
    "Prefiero planificar antes de actuar",
    "Me cuesta confiar en desconocidos",
    "Suelo ayudar cuando alguien lo necesita"))
  a2 <- auditar_redundancia(diverso)
  expect_false(a2$homogeneidad_sintactica$alerta)

  # Estructura de la salida
  expect_named(a2, c("similitud_maxima", "pares_redundantes", "ngram_overlap",
                     "homogeneidad_sintactica", "diversidad_lexica", "resumen",
                     "alerta", "parametros"))
  expect_equal(nrow(a2$resumen), 4)
})

test_that("auditar_redundancia valida la entrada", {
  expect_error(auditar_redundancia(list(a = 1)), "no valido")
})
