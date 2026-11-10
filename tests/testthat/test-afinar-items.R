# SeMiLLa 2.12.4: funciones para afinar items uno por uno (caso de bienestar
# psicologico academico, tercera version, 02-11-2026). Todo sin API.

escala_afinar <- function() {
  items <- data.frame(
    numero = 1:6,
    codigo = c("AUT1", "AUT2", "DOM1", "DOM2", "PRO1", "PRO2"),
    dimension = rep(c("Autonomia", "Dominio", "Proposito"), each = 2),
    item = c("Decido por mi cuenta.", "Sigo mi criterio en los estudios.",
             "Me siento capaz con mis estudios.", "Manejo con soltura los asuntos cotidianos.",
             "Mis estudios tienen sentido.", "Tengo metas claras en la carrera."),
    stringsAsFactors = FALSE)
  x <- list(items = items,
            concepto = list(concepto = "Bienestar academico",
                            dimensiones = list(
                              Autonomia = "Decidir por uno mismo. No incluye: metas.",
                              Dominio = "Sentir competencia ante la vida academica. No incluye: tramites.",
                              Proposito = "Sentido y direccion de los estudios.")),
            metadata = list(idioma = "es", poblacion = "universitarios"))
  class(x) <- c("semilla", "list")
  x
}

test_that("el control de nivel lee lo que la dimension no incluye y ve las otras dimensiones", {
  visto <- NULL
  local_mocked_bindings(
    .llamar_openai = function(openai, messages, ...) {
      visto <<- messages[[1]]$content
      '{"nivel": "fuera", "criterio": "e", "motivo": "es proposito"}'
    })
  r <- SeMiLLa:::.nivel_item_llm(NULL, "m", "Doy a mis estudios un rumbo.", "Dominio",
                                 "Sentir competencia. No incluye: tramites.",
                                 otras_dimensiones = c(Dominio = "x", Proposito = "Sentido"))
  expect_false(r$ok)
  expect_equal(r$criterio, "e")
  expect_match(visto, "NO incluye: tramites", fixed = TRUE)
  expect_match(visto, "- Proposito: Sentido", fixed = TRUE)
  expect_false(grepl("- Dominio: x", visto, fixed = TRUE))
  expect_match(visto, "acto cognitivo", fixed = TRUE)
})

test_that("las tandas repetidas dan V media, rango e inestables", {
  r1 <- data.frame(numero = 1:3, item = c("a", "b", "c"), V_relevancia = c(.9, .8, .7),
                   V_claridad = c(.9, .73, .7))
  r2 <- r1; r2$V_claridad <- c(.9, .53, .7)
  vr <- SeMiLLa:::.resumir_rondas(list(r1, r2), c("relevancia", "claridad"), 0.15)
  expect_s3_class(vr, "semilla_validez_repetida")
  expect_equal(vr$resumen$V_min_media, c(.9, .63, .7))
  expect_equal(vr$resumen$V_min_rango, c(0, .2, 0))
  expect_equal(vr$resumen$inestable, c(FALSE, TRUE, FALSE))
  expect_output(print(vr), "inestables")
})

test_that("validez_contenido_repetida cambia la semilla en cada tanda", {
  semillas <- integer(0)
  local_mocked_bindings(
    validez_contenido = function(x, ...) {
      semillas <<- c(semillas, getOption("SeMiLLa.seed"))
      list(v_aiken = data.frame(numero = 1, item = "a", V_relevancia = .9))
    })
  old <- options(SeMiLLa.seed = NULL); on.exit(options(old), add = TRUE)
  vr <- validez_contenido_repetida(escala_afinar(), "k", n_rondas = 3, criterios = "relevancia",
                                   seed = 10, verbose = FALSE)
  expect_equal(semillas, 10:12)
  expect_null(getOption("SeMiLLa.seed"))
  expect_equal(vr$n_rondas, 3)
})

test_that("la asignacion por similitud detecta el candidato que se parece a otra dimension", {
  S <- matrix(c(1, .2, .6, .1,
                .2, 1, .2, .7,
                .6, .2, 1, .3,
                .1, .7, .3, 1), 4, byrow = TRUE)
  a <- SeMiLLa:::.asignacion_similitud(S, cand = 3:4, base = 1:2, dims_base = c("Dom", "Pro"),
                                       dim = "Dom", codigos_base = c("D1", "P1"))
  expect_equal(a$asignacion_ok, c(TRUE, FALSE))
  expect_equal(a$dim_ajena, c("Pro", "Pro"))
  expect_equal(a$mas_parecido, c("D1", "P1"))
})

test_that("la eleccion respeta nivel, duplicados, asignacion, descartados y primera palabra", {
  cand <- data.frame(
    candidato = paste0("C", 1:6), origen = c(rep("semilla", 5), "manual"),
    item = paste("item", 1:6), nivel_ok = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    motivo_nivel = c("conducta", "", "", "", "", ""),
    V_min_media = c(.9, .85, .8, .95, .7, .75), V_min_peor = c(.9, .8, .8, .9, .7, .7),
    inestable = FALSE, sim_max_escala = c(.5, .8, .5, .5, .5, .5),
    mas_parecido = "X", asignacion_ok = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    sim_propia = .5, sim_ajena = .4, dim_ajena = "Pro",
    sim_descartado = c(NA, NA, NA, .9, NA, NA),
    primera_usada = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE), stringsAsFactors = FALSE)
  e <- SeMiLLa:::.elegir_candidato(cand)$candidatos
  expect_equal(e$candidato[e$elegido], "C6")      # el manual no paga la primera palabra
  m <- stats::setNames(e$motivo_rechazo, e$candidato)
  expect_match(m[["C1"]], "fuera de nivel")
  expect_match(m[["C2"]], "casi duplicado")
  expect_match(m[["C3"]], "se parece mas a Pro")
  expect_match(m[["C4"]], "descartada")
  expect_match(m[["C5"]], "primera palabra")
})

test_that("un candidato inestable solo se elige si no hay otro estable", {
  cand <- data.frame(candidato = c("C1", "C2"), origen = "semilla", item = c("a", "b"),
                     nivel_ok = TRUE, motivo_nivel = "", V_min_media = c(.9, .8),
                     V_min_peor = c(.7, .8), inestable = c(TRUE, FALSE), sim_max_escala = .5,
                     mas_parecido = "X", asignacion_ok = TRUE, sim_propia = .5, sim_ajena = .4,
                     dim_ajena = "P", sim_descartado = NA, primera_usada = FALSE,
                     stringsAsFactors = FALSE)
  e <- SeMiLLa:::.elegir_candidato(cand)$candidatos
  expect_equal(e$candidato[e$elegido], "C2")
})

test_that("reemplazar_item encadena los pasos y no repite lo descartado", {
  x <- escala_afinar()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  registrar_descartados(f, "Logro acomodar mis estudios a lo que necesito.", "Dominio", "no se entendio")
  instruccion <- NULL
  local_mocked_bindings(
    .configurar_openai = function(...) NULL,
    .generar_items_dimension = function(..., instruccion_extra) {
      instruccion <<- instruccion_extra
      data.frame(item = c("Siento que puedo con mi vida academica.",
                          "Logro acomodar mis estudios a lo que necesito.",
                          "Doy rumbo a mis estudios."))
    },
    .nivel_item_llm = function(openai, modelo, item, ...)
      if (grepl("rumbo", item)) list(ok = FALSE, motivo = "es proposito", criterio = "e")
      else list(ok = TRUE, motivo = "", criterio = NA_character_),
    validez_contenido_repetida = function(x, ...) {
      n <- nrow(x$items)
      SeMiLLa:::.resumir_rondas(list(data.frame(numero = seq_len(n), item = x$items$item,
                                                V_relevancia = .9, V_claridad = .8)),
                                c("relevancia", "claridad"))
    },
    obtener_embeddings = function(items, ...) {
      n <- nrow(items); S <- diag(n)
      S[S == 0] <- 0.3
      dom <- which(items$dimension == "Dominio")
      S[dom, dom] <- 0.6; diag(S) <- 1
      list(similitud = S)
    })
  r <- reemplazar_item(x, "DOM2", api_key = "k", n_candidatos = 3, descartados = f,
                       faceta = "dar forma a la vida academica", verbose = FALSE)
  expect_s3_class(r, "semilla_reemplazo")
  expect_false(any(grepl("acomodar", r$candidatos$item)))   # descartado
  expect_match(instruccion, "acomodar", fixed = TRUE)        # se le avisa al generador
  expect_match(instruccion, "FACETA", fixed = TRUE)
  expect_equal(r$elegido$item, "Siento que puedo con mi vida academica.")
  expect_equal(r$escala$items$item[4], "Siento que puedo con mi vida academica.")
  expect_null(r$escala$similitud)
  d <- leer_descartados(f)
  expect_true("Manejo con soltura los asuntos cotidianos." %in% d$item)   # el saliente
  expect_true("Doy rumbo a mis estudios." %in% d$item)                   # fuera de nivel
})

test_that("registrar_descartados no repite textos", {
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  registrar_descartados(f, c("Uno.", "Dos."), "D", "x")
  d <- registrar_descartados(f, c("uno", "Tres."), "D", "y")
  expect_equal(d$item, c("Uno.", "Dos.", "Tres."))
})

test_that("auditar_ediciones separa editados, nuevos y retirados", {
  a <- data.frame(item = c("Valoro mis fortalezas en los estudios.", "Mis cursos me sirven.",
                           "Ajusto mi espacio de estudio."),
                  dimension = c("Ace", "Pro", "Dom"))
  b <- data.frame(item = c("Valoro mis fortalezas en los estudios, a pesar de mis notas.",
                           "Mis cursos me sirven.", "Siento que puedo con mi vida academica."),
                  dimension = c("Ace", "Pro", "Dom"))
  e <- auditar_ediciones(a, b, verificados = "Siento que puedo con mi vida academica.")
  d <- as.data.frame(unclass(e))
  expect_equal(sort(d$cambio), c("editado", "editado"))
  expect_equal(d$requiere_revision[d$dimension == "Ace"], TRUE)
  expect_equal(d$requiere_revision[d$dimension == "Dom"], FALSE)
  expect_output(print(e), "REVISAR")
})

test_that("variedad_redaccion marca primeras palabras y marcos repetidos", {
  v <- variedad_redaccion(c("Valoro mis logros, m\u00e1s all\u00e1 de mis notas.",
                            "Valoro lo que aprendo.", "Valoro mis cursos.",
                            "Mis cursos tienen sentido, m\u00e1s all\u00e1 de aprobarlos.",
                            "Decido por mi cuenta."))
  expect_equal(names(v$primera), "valoro")
  expect_true("m\u00e1s all\u00e1 de" %in% names(v$marcos))
  expect_equal(v$por_item$alerta, c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("el lector simulado resume comprension y fidelidad", {
  local_mocked_bindings(
    .configurar_openai = function(...) NULL,
    .llamar_openai = function(openai, messages, ..., temperature) {
      if (temperature > 0)
        '{"lectores": [{"entiende": "s\u00ed", "parafraseo": "si me adapto", "dificil": ""},
                       {"entiende": "m\u00e1s o menos", "parafraseo": "no se", "dificil": "acomodar"},
                       {"entiende": "no", "parafraseo": "?", "dificil": "acomodar"}]}'
      else '{"fiel": [true, false, false]}'
    })
  l <- lector_simulado("Logro acomodar mis estudios a lo que necesito.", api_key = "k",
                       poblacion = "universitarios", verbose = FALSE)
  d <- as.data.frame(unclass(l))
  expect_equal(d$prop_entiende, 0.33)
  expect_equal(d$prop_fiel, 0.33)
  expect_equal(d$palabras_dificiles, "acomodar")
  expect_true(d$alerta)
})

test_that("el formulario de entrevista cognitiva se arma desde los items", {
  dd <- file.path(tempdir(), "ec_prueba")
  on.exit(unlink(dd, recursive = TRUE), add = TRUE)
  r <- formulario_entrevista_cognitiva(
    data.frame(codigo = c("A1", "D1"), item = c("Me acepto.", "Siento que puedo <con> todo.")),
    dd, "Cuestionario de prueba.", clave = "ec_prueba_1")
  expect_true(all(file.exists(r)))
  h <- paste(readLines(r[1], encoding = "UTF-8"), collapse = "\n")
  expect_match(h, "ec_prueba_1", fixed = TRUE)
  expect_match(h, "Cuestionario de prueba.", fixed = TRUE)
  expect_match(h, "\"item\":\"Me acepto.\"", fixed = TRUE)
  expect_false(grepl("__ITEMS__|__CLAVE__|__MINUTOS__|__DESCRIPCION__", h))
  expect_error(formulario_entrevista_cognitiva("Otro.", dd, "x"), "sobrescribir")
})

test_that("leer_entrevista_cognitiva marca los items poco claros y redacta el motivo", {
  r <- data.frame(n = c(1, 1, 2, 2), item = rep(c("Me acepto.", "Logro acomodar mis estudios."),
                                                each = 2),
                  claridad = c("S\u00ed, se entiende", "S\u00ed, se entiende",
                               "M\u00e1s o menos", "No se entiende"),
                  parafraseo = c("si me quiero", "si me acepto", "?", "no se"),
                  confuso = c("", "", "acomodar", "acomodar"))
  e <- leer_entrevista_cognitiva(r, items = data.frame(n = 1:2, codigo = c("ACE1", "DOM1")))
  d <- as.data.frame(unclass(e))
  expect_equal(d$codigo, c("ACE1", "DOM1"))
  expect_equal(d$prop_clara, c(1, 0))
  expect_equal(d$revisar, c(FALSE, TRUE))
  expect_match(d$motivo[2], "acomodar")
  expect_output(print(e), "por revisar")
})
