base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/01_bases_de_datos/fase3_pareja_local"
casos <- c(
  "Mitos de Amor" = "datasets/01_MitosAmor/resultados/comparacion_fase2.rds",
  "WAST"          = "datasets/02_WAST/resultados/comparacion_fase2.rds",
  "SCP"           = "datasets/03_SCP/resultados/comparacion_fase2.rds",
  "IR"            = "datasets/04_IR/resultados/comparacion_fase2.rds",
  "Celos"         = "datasets/05_Celos/resultados/comparacion_fase2.rds"
)
df <- do.call(rbind, lapply(names(casos), function(nm) {
  r <- readRDS(file.path(base, casos[[nm]]))
  data.frame(
    caso = nm, n_emp = r$n_empirico, k_items = r$n_items,
    n_dim = r$n_dimensiones,
    alpha_emp = r$alpha_mean_emp, alpha_sem = r$alpha_mean_sem,
    alpha_mae = r$alpha_mae, alpha_r = r$alpha_pearson,
    carga_coh_r = r$carga_coh_r, kappa = r$kappa,
    accuracy = r$accuracy, sensibilidad = r$sensibilidad,
    especificidad = r$especificidad, ppv = r$precision, f1 = r$f1,
    stringsAsFactors = FALSE
  )
}))
print(df)
write.csv(df, file.path(base, "consolidado_pareja.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n=== AGREGADOS ===\n")
cat(sprintf("Casos: %d  | Items: %d  | n_emp promedio: %.0f\n",
            nrow(df), sum(df$k_items), mean(df$n_emp)))
cat(sprintf("alpha MAE promedio:  %.3f\n", mean(df$alpha_mae, na.rm=TRUE)))
cat(sprintf("alpha r promedio:    %+.3f\n", mean(df$alpha_r, na.rm=TRUE)))
cat(sprintf("Carga~Coh promedio:  %+.3f\n", mean(df$carga_coh_r, na.rm=TRUE)))
cat(sprintf("Accuracy promedio:   %.3f\n", mean(df$accuracy, na.rm=TRUE)))
cat(sprintf("Esp promedio:        %.3f\n", mean(df$especificidad, na.rm=TRUE)))
