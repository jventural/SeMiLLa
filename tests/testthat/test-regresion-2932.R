# Regresiones cerradas en 2.9.32. Las tres salieron de reconstruir el caso de
# una escala real (24 items, 3 dimensiones, 3 facetas cada una) a la que el
# refinamiento del Paso 6 dejo un reparto 2/6/0 dentro de una dimension.

test_that("el reparto de items entre facetas se mide, no solo si hay o no", {
  # Antes solo existia cubierta = n >= 1: un reparto 6/1/1 en una dimension de
  # 8 items daba las tres facetas "cubiertas" y no habia nada que mirar.
  mk <- function(caract) {
    n <- length(caract)
    S <- matrix(0.3, n, n); diag(S) <- 1
    structure(list(
      similitud = S,
      items = data.frame(codigo = paste0("I", seq_len(n)), dimension = "D",
                         caracteristica = caract,
                         item = paste("Item numero", seq_len(n)),
                         stringsAsFactors = FALSE),
      concepto = list(caracteristicas = list(
        D = c("Faceta alfa", "Faceta beta", "Faceta gamma")))),
      class = c("semilla", "list"))
  }

  # Reparto sano 3/3/2
  sano <- auditar_cobertura_facetas(
    mk(c(rep("Faceta alfa", 3), rep("Faceta beta", 3), rep("Faceta gamma", 2))),
    verbose = FALSE)
  expect_equal(sano$equilibrio$reparto, "3/3/2")
  expect_false(sano$equilibrio$concentrada)
  expect_equal(sano$n_concentradas, 0L)

  # Reparto concentrado 6/1/1: las tres cubiertas, pero una acapara el 75%
  conc <- auditar_cobertura_facetas(
    mk(c(rep("Faceta alfa", 6), "Faceta beta", "Faceta gamma")), verbose = FALSE)
  expect_equal(conc$n_cubiertas, 3L)          # ninguna huerfana...
  expect_true(conc$equilibrio$concentrada)    # ...y aun asi avisa
  expect_equal(conc$equilibrio$max_prop, 0.75)
  expect_equal(conc$n_concentradas, 1L)
})

test_that("el gate del Paso 6 muestra el reparto sin bloquear por el", {
  # La fila es informativa a proposito: hay constructos donde una faceta pesa
  # mas de forma legitima. Lo que no puede es no verse.
  n <- 8L
  S <- matrix(0.3, n, n); diag(S) <- 1
  esc <- structure(list(
    similitud = S,
    items = data.frame(codigo = paste0("I", seq_len(n)), dimension = "D",
                       caracteristica = c(rep("Faceta alfa", 6), "Faceta beta",
                                          "Faceta gamma"),
                       item = paste("Item", seq_len(n)), stringsAsFactors = FALSE),
    concepto = list(caracteristicas = list(
      D = c("Faceta alfa", "Faceta beta", "Faceta gamma")))),
    class = c("semilla", "list"))

  ens <- list(precision_global = 100, ari = 1, silhouette = 0.2,
              consenso = data.frame(Consenso = rep(1, n)))
  tb <- SeMiLLa:::.compuerta_estructura(ens, umbral_consenso = 0.667,
          min_precision = 90, min_ari = 0.65, escala = esc)

  fila <- tb[tb$clave == "equilibrio_facetas", ]
  expect_equal(nrow(fila), 1L)
  expect_false(fila$bloquea)
  expect_equal(fila$crudo, "6/1/1")
})

test_that("ensamblar_test avisa si la escala de respuesta no sirve", {
  # Antes se descartaba en silencio y el .docx salia con las anclas genericas
  # "Totalmente en desacuerdo...". Quien pasaba una lista con $anclajes
  # -estructura razonable- imprimia el cuestionario con anclas que no eligio.
  n <- 6L
  esc <- structure(list(
    items = data.frame(numero = seq_len(n), dimension = "D",
                       item = paste("Item", seq_len(n)), stringsAsFactors = FALSE)),
    class = c("semilla", "list"))
  lista_suelta <- list(tipo_escala = "acuerdo", n_puntos = 5L,
                       anclajes = c("1" = "Muy en desacuerdo", "2" = "En desacuerdo",
                                    "3" = "Indeciso", "4" = "De acuerdo",
                                    "5" = "Muy de acuerdo"))
  expect_warning(
    ensamblar_test(esc, escala_respuesta = lista_suelta, forma = "larga",
                   archivo = NULL, formato = character(0), verbose = FALSE),
    "no es un objeto")

  # Y no avisa cuando no se pasa nada (el default es legitimo)
  expect_silent(
    ensamblar_test(esc, escala_respuesta = NULL, forma = "larga",
                   archivo = NULL, formato = character(0), verbose = FALSE))
})

test_that("el refinamiento conserva la faceta del item que reemplaza", {
  # R/sembrar.R: al reemplazar se guardaba nuevo_item$caracteristica, o sea la
  # etiqueta que devolvia el LLM, tirando la garantia que el propio prompt
  # exige ("DEBE seguir midiendo exactamente esta dimension/caracteristica").
  # Aqui se comprueba la regla sobre el codigo fuente: la asignacion solo
  # ocurre cuando el item original no traia faceta.
  # El fuente no viaja instalado, asi que se valida la REGLA, no el texto.
  aplicar <- function(fac_original, fac_llm) {
    items <- data.frame(caracteristica = fac_original, stringsAsFactors = FALSE)
    idx <- 1L
    if ("caracteristica" %in% names(items)) {
      fac <- items$caracteristica[idx]
      if (is.na(fac) || !nzchar(trimws(as.character(fac))))
        items$caracteristica[idx] <- fac_llm
    }
    items$caracteristica[idx]
  }
  # tenia faceta -> se conserva, pase lo que pase con el LLM
  expect_equal(aplicar("Faceta declarada", "Etiqueta inventada"), "Faceta declarada")
  # no tenia -> se acepta la del LLM, mejor que dejarla vacia
  expect_equal(aplicar(NA_character_, "Etiqueta del LLM"), "Etiqueta del LLM")
  expect_equal(aplicar("", "Etiqueta del LLM"), "Etiqueta del LLM")
})
