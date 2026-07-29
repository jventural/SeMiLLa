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

  # Estructura de la salida (incluye facetas_repetidas desde la deteccion de
  # clusters de 3+ items; ver ?auditar_redundancia)
  expect_named(a2, c("similitud_maxima", "pares_redundantes",
                     "facetas_repetidas", "ngram_overlap",
                     "homogeneidad_sintactica", "diversidad_lexica", "resumen",
                     "alerta", "parametros"))
  expect_equal(nrow(a2$resumen), 4)
})

test_that("las similitudes empatadas no rompen el corte del dendrograma", {
  # Regresion: con una matriz de similitud CONSTANTE (o redondeada) las
  # distancias empatan y average linkage devuelve alturas con inversiones de
  # ~1e-16; cutree() las rechazaba ("height component not sorted") y tumbaba la
  # auditoria completa. .hc_monotono las repara (ver R/utils.R).
  mk_const <- function(n, sim) {
    S <- matrix(sim, n, n); diag(S) <- 1
    structure(list(similitud = S,
      items = data.frame(codigo = paste0("I", seq_len(n)), dimension = "D",
                         item = paste("Item de prueba numero", seq_len(n)),
                         stringsAsFactors = FALSE)),
      class = c("semilla", "list"))
  }
  for (sim in c(0.3, 0.45, 0.62)) for (n in c(4L, 6L, 9L)) {
    a <- auditar_redundancia(mk_const(n, sim))
    expect_s3_class(a, "semilla_redundancia")
    expect_equal(nrow(a$resumen), n)
  }

  # .hc_monotono no toca un dendrograma ya monotono ni cambia la particion
  set.seed(11); n <- 20
  S <- matrix(stats::runif(n * n, .1, .9), n, n)
  S <- (S + t(S)) / 2; diag(S) <- 1
  hc <- stats::hclust(stats::as.dist(1 - S), method = "average")
  hc2 <- SeMiLLa:::.hc_monotono(hc)
  expect_identical(hc$height, hc2$height)
  expect_identical(stats::cutree(hc, 4), stats::cutree(hc2, 4))

  # Una inversion REAL (metodo no monotono) se repara pero avisa
  hcm <- stats::hclust(stats::as.dist(1 - S), method = "median")
  expect_true(is.unsorted(hcm$height))
  expect_warning(hh <- SeMiLLa:::.hc_monotono(hcm), "inversiones de altura reales")
  expect_false(is.unsorted(hh$height))
})

test_that("auditar_redundancia valida la entrada", {
  expect_error(auditar_redundancia(list(a = 1)), "no valido")
})
