key <- trimws(readLines("C:/Users/PC/AppData/Local/Temp/.semilla_key", warn = FALSE)[1])
suppressMessages(library(SeMiLLa))
stopifnot("comparar_generadores" %in% getNamespaceExports("SeMiLLa"))

bm <- comparar_generadores(
  concepto = "autorregulacion del aprendizaje",
  dimensiones = list(
    "Planificacion del estudio" = paste(
      "Conductas de organizar el tiempo y las tareas de estudio antes de",
      "ejecutarlas: fijar horarios, dividir el material, preparar el espacio."),
    "Manejo de distracciones digitales" = paste(
      "Conductas de controlar el uso del celular y redes sociales durante",
      "el estudio: silenciar, alejar o bloquear dispositivos y apps.")
  ),
  api_key = key,
  modelos = c("gpt-4.1-mini", "gpt-5-nano", "gpt-5-mini",
              "gpt-5.4-nano", "gpt-5.4-mini", "gpt-5.2"),
  poblacion = "estudiantes universitarios peruanos",
  seed = 2026,
  verbose = TRUE
)

saveRDS(bm, "C:/Users/PC/AppData/Local/Temp/benchmark_v2.rds")

cat("\n\n=== MUESTRA DE ITEMS (3 por modelo, dimension distracciones) ===\n")
for (m in names(bm$items)) {
  it <- bm$items[[m]]
  if (nrow(it) == 0) next
  sub <- it[it$dimension == "Manejo de distracciones digitales", ][1:3, ]
  cat("\n---", m, "---\n")
  for (i in seq_len(nrow(sub))) cat("  -", sub$item[i], "\n")
}
