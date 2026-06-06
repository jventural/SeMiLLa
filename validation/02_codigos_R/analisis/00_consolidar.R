# =============================================================================
# Reporte global consolidado — 5 casos
# =============================================================================
# Junta las filas-resumen de cada caso en una sola tabla cross-paper.
# Calcula metricas agregadas y produce plots comparativos.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario"
dir_rep <- file.path(base, "04_reportes_html")
dir_gr  <- file.path(dir_rep, "graficos")
if (!dir.exists(dir_gr)) dir.create(dir_gr, recursive = TRUE)

# ---- Cargar resumenes de cada caso ----
casos <- list(
  WLEIS     = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/01_WLEIS_Merino_2016/resultados/comparacion_v5.rds"),
  `DASS-21` = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/02_DASS21/resultados/comparacion_v5.rds"),
  `PANAS-C` = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/03_PANAS/resultados/comparacion_v5.rds"),
  SWLS      = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/04_SWLS/resultados/comparacion_v5.rds"),
  `MBI-HSS` = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/05_MBI/resultados/comparacion_v5.rds"),
  RSES      = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/06_RSES/resultados/comparacion_v5.rds"),
  `UWES-9`  = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/07_UWES/resultados/comparacion_v5.rds"),
  EAPESA    = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/08_EAPESA/resultados/comparacion_v5.rds"),
  EOIE      = file.path(base, "01_bases_de_datos/fase1_pdfs/casos/09_EOIE/resultados/comparacion_v5.rds")
)

resultados <- lapply(casos, function(p) {
  if (!file.exists(p)) {
    warning("No encontrado: ", p); return(NULL)
  }
  readRDS(p)
})

# ---- Tabla resumen 1 fila por caso ----
df_global <- do.call(rbind, lapply(seq_along(resultados), function(i) {
  r <- resultados[[i]]
  if (is.null(r)) return(NULL)
  data.frame(
    caso          = names(resultados)[i],
    escala        = r$escala_nombre,
    n_emp         = r$n_empirico,
    k_items       = r$n_items,
    n_dim         = r$n_dimensiones,
    metrica_tipo  = if (is.null(r$metrica_emp_tipo)) "alpha" else r$metrica_emp_tipo,
    umbral_coh    = if (is.null(r$umbral_coherencia)) 0.5 else r$umbral_coherencia,
    alpha_r       = r$alpha_pearson,
    alpha_rho     = r$alpha_spearman,
    alpha_mae     = r$alpha_mae,
    alpha_emp     = r$alpha_mean_emp,
    alpha_sem     = r$alpha_mean_sem,
    tucker_prec_r = r$tucker_precision_r,
    carga_coh_r   = r$carga_coh_r,
    kappa         = r$kappa,
    accuracy      = r$accuracy,
    sensibilidad  = r$sensibilidad,
    especificidad = r$especificidad,
    precision_ppv = r$precision,
    f1            = r$f1,
    stringsAsFactors = FALSE
  )
}))

# ---- Imprimir tabla ----
cat("\n=================================================================\n")
cat("REPORTE GLOBAL — 5 CASOS\n")
cat("=================================================================\n\n")
print(df_global, row.names = FALSE)

write.csv(df_global, file.path(dir_rep, "consolidado.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- Tabla 'fila_resumen' formateada para humanos ----
df_pretty <- df_global
num_cols <- sapply(df_pretty, is.numeric)
df_pretty[num_cols] <- lapply(df_pretty[num_cols], function(v) round(v, 3))
write.csv(df_pretty, file.path(dir_rep, "consolidado_pretty.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- Plot 1: precision/sensibilidad/especificidad por caso ----
df_metrics <- df_global %>%
  select(caso, accuracy, sensibilidad, especificidad, precision_ppv, f1) %>%
  pivot_longer(-caso, names_to = "metrica", values_to = "valor")
df_metrics$caso <- factor(df_metrics$caso, levels = df_global$caso)

p_metrics <- ggplot(df_metrics, aes(x = caso, y = valor, fill = metrica)) +
  geom_col(position = position_dodge(width = .8), width = .75) +
  geom_text(aes(label = ifelse(is.na(valor), "NA", sprintf("%.2f", valor))),
            position = position_dodge(width = .8), vjust = -0.4, size = 2.8) +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 1.05) +
  labs(title = "Deteccion de items problematicos: metricas por caso",
       subtitle = "Gold standard: items marcados como problematicos en el paper empirico",
       x = NULL, y = "Valor", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 10, face = "bold"))
ggsave(file.path(dir_gr, "01_metricas_problematicos.png"),
       p_metrics, width = 10, height = 6, dpi = 150)

# ---- Plot 2: alpha empirico vs alpha semantico cross-paper ----
df_alpha <- df_global %>%
  select(caso, alpha_emp, alpha_sem) %>%
  pivot_longer(-caso, names_to = "tipo", values_to = "alpha")
df_alpha$tipo <- factor(df_alpha$tipo,
                        levels = c("alpha_emp", "alpha_sem"),
                        labels = c("Empirico", "Semantico"))
df_alpha$caso <- factor(df_alpha$caso, levels = df_global$caso)

p_alpha <- ggplot(df_alpha, aes(x = caso, y = alpha, fill = tipo)) +
  geom_col(position = position_dodge(width = .7), width = .65) +
  geom_text(aes(label = sprintf("%.2f", alpha)),
            position = position_dodge(width = .7), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Empirico" = "#2980b9", "Semantico" = "#e67e22")) +
  ylim(0, 1.05) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray40") +
  labs(title = "Alpha promedio: empirico vs semantico por caso",
       subtitle = paste0("MAE promedio cross-caso: ",
                         sprintf("%.3f",
                                 mean(df_global$alpha_mae, na.rm = TRUE))),
       x = NULL, y = "Alpha (promedio por escala)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 10, face = "bold"))
ggsave(file.path(dir_gr, "02_alpha_emp_vs_sem.png"),
       p_alpha, width = 10, height = 6, dpi = 150)

# ---- Plot 3: scatter Carga ~ Coherencia r vs Cohen kappa por caso ----
df_corr <- df_global %>%
  select(caso, carga_coh_r, kappa, accuracy) %>%
  mutate(label = sprintf("%s\n(acc=%.2f)", caso, accuracy))

p_corr <- ggplot(df_corr, aes(x = carga_coh_r, y = kappa, color = caso)) +
  geom_point(size = 4) +
  ggrepel::geom_text_repel(aes(label = label), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_x_continuous(limits = c(-0.6, 1)) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  labs(title = "Concordancia item-level (carga~coherencia) vs Cohen's kappa",
       x = "Pearson r: carga convergente (emp) vs coherencia intra (sem)",
       y = "Cohen's kappa (detec. items problematicos)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
ggsave(file.path(dir_gr, "03_scatter_concordancia.png"),
       p_corr, width = 9, height = 7, dpi = 150)

# ---- Resumen agregado ----
cat("\n\n===== AGREGADOS CROSS-CASO =====\n")
cat(sprintf("Items totales:                 %d\n", sum(df_global$k_items)))
cat(sprintf("Casos:                         %d\n", nrow(df_global)))
cat(sprintf("Alpha MAE promedio:            %.3f\n", mean(df_global$alpha_mae, na.rm = TRUE)))
cat(sprintf("Carga~Coherencia r promedio:   %.3f\n", mean(df_global$carga_coh_r, na.rm = TRUE)))
cat(sprintf("Accuracy promedio:             %.3f\n", mean(df_global$accuracy, na.rm = TRUE)))
cat(sprintf("Sensibilidad promedio (n>0):   %.3f\n",
            mean(df_global$sensibilidad, na.rm = TRUE)))
cat(sprintf("Especificidad promedio:        %.3f\n", mean(df_global$especificidad, na.rm = TRUE)))
cat(sprintf("Kappa promedio:                %.3f\n", mean(df_global$kappa, na.rm = TRUE)))

cat("\nArchivos generados:\n")
cat("  ", file.path(dir_rep, "consolidado.csv"),       "\n")
cat("  ", file.path(dir_rep, "consolidado_pretty.csv"), "\n")
cat("  ", file.path(dir_gr, "01_metricas_problematicos.png"), "\n")
cat("  ", file.path(dir_gr, "02_alpha_emp_vs_sem.png"),       "\n")
cat("  ", file.path(dir_gr, "03_scatter_concordancia.png"),    "\n")
