# Control por regex de negaciones y contrastes encubiertos (caso 2026-10-14).

test_that("detecta negaciones y contrastes encubiertos", {
  si <- c("Reconozco que mis calificaciones no definen mi valor",
          "Mantengo la carrera que elijo aunque mi pareja me presione",
          "Acepto mis errores sin culparme",
          "Sigo adelante a pesar de las dificultades",
          "Nunca dudo de mis decisiones")
  expect_true(all(SeMiLLa:::.negacion_encubierta(si)))
})

test_that("no confunde palabras que solo contienen esas letras", {
  no <- c("Elijo cursos que coinciden con mis intereses",
          "Mis decisiones sobre la carrera me pertenecen",
          "Cuento con compañeros que me apoyan",
          "Planifico mis estudios con anticipación",
          "Valoro la sinceridad de mis docentes")
  expect_false(any(SeMiLLa:::.negacion_encubierta(no)))
})

test_that(".supuesto_temporal detecta items que no aplican a quien recien empieza", {
  expect_true(SeMiLLa:::.supuesto_temporal("Cada ciclo descubro nuevas posibilidades."))
  expect_true(SeMiLLa:::.supuesto_temporal("Percibo cómo se amplían mis conocimientos desde que inicié la carrera."))
  expect_false(SeMiLLa:::.supuesto_temporal("Confío en mis compañeros de estudio."))
})
