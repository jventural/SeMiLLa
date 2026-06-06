# =============================================================================
# Re-correr comparar_con_semilla() para Pareja con OMEGA como metrica preferida
# Los empíricos ya tienen omega calculado, solo cambia que el comparator lo usa.
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "\\.R$", full.names = TRUE)) source(f)

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/01_bases_de_datos/fase3_pareja_local"

casos <- c("01_MitosAmor", "02_WAST", "03_SCP", "04_IR", "05_Celos")

resultados <- list()
for (cd in casos) {
  cat("\n=== ", cd, " ===\n")
  dir_caso <- file.path(base, cd)
  emp <- readRDS(file.path(dir_caso, "resultados/empirico_calculado.rds"))
  escala <- readRDS(file.path(dir_caso, "resultados/escala_semilla.rds.rds"))

  # FORZAR preferida = omega
  emp$meta$tipo_metrica_preferida <- "omega"

  if (is.null(escala$efa) || !"precision_por_dimension" %in% names(escala$efa)) {
    nd <- length(unique(escala$items$dimension))
    escala$efa <- precision_clasificacion(escala, metodo="ensemble",
      algoritmos=c("kmeans","ward","pam"), n_replicas=20, n_clusters=nd, verbose=FALSE)
  }

  comp <- comparar_con_semilla(emp, escala, modo="conservador",
    dir_salida = file.path(dir_caso, "resultados"),
    prefijo = "comparacion_omega", verbose = TRUE)
  saveRDS(comp, file.path(dir_caso, "resultados/comparacion_omega.rds"))
  resultados[[cd]] <- comp
}

# Consolidar
df <- do.call(rbind, lapply(names(resultados), function(nm) {
  r <- resultados[[nm]]
  data.frame(
    caso = sub("^[0-9]+_", "", nm),
    n_emp = r$n_empirico, k_items = r$n_items, n_dim = r$n_dimensiones,
    metrica_tipo = r$metrica_emp_tipo,
    omega_emp = r$alpha_mean_emp, omega_sem = r$alpha_mean_sem,
    omega_mae = r$alpha_mae, omega_r = r$alpha_pearson,
    carga_coh_r = r$carga_coh_r, kappa = r$kappa,
    accuracy = r$accuracy, especificidad = r$especificidad,
    stringsAsFactors = FALSE
  )
}))
print(df)
write.csv(df, file.path(base, "consolidado_pareja_omega.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== AGREGADOS (OMEGA) ===\n")
cat(sprintf("omega MAE promedio:  %.3f\n", mean(df$omega_mae, na.rm=TRUE)))
cat(sprintf("omega r promedio:    %+.3f\n", mean(df$omega_r, na.rm=TRUE)))
cat(sprintf("Carga~Coh promedio:  %+.3f\n", mean(df$carga_coh_r, na.rm=TRUE)))
cat(sprintf("Accuracy promedio:   %.3f\n", mean(df$accuracy, na.rm=TRUE)))
