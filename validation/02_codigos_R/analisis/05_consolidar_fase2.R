# =============================================================================
# Consolidacion Fase 2 + comparacion Fase 1 vs Fase 2
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(ggrepel)
})

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario"

# ---- Cargar resultados Fase 2 ----
casos_f2 <- tibble::tribble(
  ~caso,         ~rds_path,
  "DASS-21",     "01_bases_de_datos/fase2_openpsych_en/01_DASS/resultados/comparacion_fase2.rds",
  "RSES",        "01_bases_de_datos/fase2_openpsych_en/02_RSES/resultados/comparacion_fase2.rds",
  "Big Five",    "01_bases_de_datos/fase2_openpsych_en/03_BigFive/resultados/comparacion_fase2.rds",
  "ECR",         "01_bases_de_datos/fase2_openpsych_en/04_ECR/resultados/comparacion_fase2.rds",
  "NPI",         "01_bases_de_datos/fase2_openpsych_en/05_NPI/resultados/comparacion_fase2.rds"
)

df_f2 <- do.call(rbind, lapply(seq_len(nrow(casos_f2)), function(i) {
  rd <- file.path(base, casos_f2$rds_path[i])
  if (!file.exists(rd)) return(NULL)
  r <- readRDS(rd)
  data.frame(
    fase = "Fase 2", caso = casos_f2$caso[i],
    n_emp = r$n_empirico, k_items = r$n_items, n_dim = r$n_dimensiones,
    alpha_emp = r$alpha_mean_emp, alpha_sem = r$alpha_mean_sem,
    alpha_mae = r$alpha_mae, alpha_r = r$alpha_pearson,
    carga_coh_r = r$carga_coh_r, kappa = r$kappa,
    accuracy = r$accuracy, sensibilidad = r$sensibilidad,
    especificidad = r$especificidad, ppv = r$precision, f1 = r$f1,
    stringsAsFactors = FALSE
  )
}))

# ---- Cargar Fase 1 (todos los 9) ----
casos_f1 <- tibble::tribble(
  ~caso,        ~rds_path,
  "WLEIS",      "01_bases_de_datos/fase1_pdfs/casos/01_WLEIS_Merino_2016/resultados/comparacion_v5.rds",
  "DASS-21",    "01_bases_de_datos/fase1_pdfs/casos/02_DASS21/resultados/comparacion_v5.rds",
  "PANAS-C",    "01_bases_de_datos/fase1_pdfs/casos/03_PANAS/resultados/comparacion_v5.rds",
  "SWLS",       "01_bases_de_datos/fase1_pdfs/casos/04_SWLS/resultados/comparacion_v5.rds",
  "MBI-HSS",    "01_bases_de_datos/fase1_pdfs/casos/05_MBI/resultados/comparacion_v5.rds",
  "RSES",       "01_bases_de_datos/fase1_pdfs/casos/06_RSES/resultados/comparacion_v5.rds",
  "UWES-9",     "01_bases_de_datos/fase1_pdfs/casos/07_UWES/resultados/comparacion_v5.rds",
  "EAPESA",     "01_bases_de_datos/fase1_pdfs/casos/08_EAPESA/resultados/comparacion_v5.rds",
  "EOIE",       "01_bases_de_datos/fase1_pdfs/casos/09_EOIE/resultados/comparacion_v5.rds"
)
df_f1 <- do.call(rbind, lapply(seq_len(nrow(casos_f1)), function(i) {
  rd <- file.path(base, casos_f1$rds_path[i])
  if (!file.exists(rd)) return(NULL)
  r <- readRDS(rd)
  data.frame(
    fase = "Fase 1", caso = casos_f1$caso[i],
    n_emp = r$n_empirico, k_items = r$n_items, n_dim = r$n_dimensiones,
    alpha_emp = r$alpha_mean_emp, alpha_sem = r$alpha_mean_sem,
    alpha_mae = r$alpha_mae, alpha_r = r$alpha_pearson,
    carga_coh_r = r$carga_coh_r, kappa = r$kappa,
    accuracy = r$accuracy, sensibilidad = r$sensibilidad,
    especificidad = r$especificidad, ppv = r$precision, f1 = r$f1,
    stringsAsFactors = FALSE
  )
}))

df_total <- rbind(df_f1, df_f2)

write.csv(df_f2, file.path(base, "04_reportes_html/consolidado_fase2.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_total, file.path(base, "04_reportes_html/consolidado_total.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== FASE 2 (5 datasets crudos) ===\n")
print(df_f2)
cat("\n=== AGREGADOS POR FASE ===\n")
agg <- df_total %>% group_by(fase) %>% summarise(
  n_casos = n(),
  k_items_total = sum(k_items),
  alpha_mae   = mean(alpha_mae, na.rm = TRUE),
  carga_coh_r = mean(carga_coh_r, na.rm = TRUE),
  sens_media  = mean(sensibilidad, na.rm = TRUE),
  esp_media   = mean(especificidad, na.rm = TRUE),
  ppv_media   = mean(ppv, na.rm = TRUE),
  f1_media    = mean(f1, na.rm = TRUE),
  kappa_media = mean(kappa, na.rm = TRUE),
  .groups = "drop"
)
print(agg)
saveRDS(agg, file.path(base, "04_reportes_html/agg_por_fase.rds"))

# ---- Comparacion directa Fase 1 vs Fase 2 para escalas comunes ----
comunes <- intersect(df_f1$caso, df_f2$caso)
cat("\n=== ESCALAS EN AMBAS FASES (comparacion directa) ===\n")
cat("Escalas:", paste(comunes, collapse=", "), "\n\n")
df_comparacion <- df_total[df_total$caso %in% comunes, ]
df_comp_wide <- df_comparacion %>%
  select(caso, fase, alpha_mae, carga_coh_r, especificidad, kappa) %>%
  pivot_wider(id_cols = caso, names_from = fase,
              values_from = c(alpha_mae, carga_coh_r, especificidad, kappa))
print(df_comp_wide)

write.csv(df_comp_wide, file.path(base, "04_reportes_html/comparacion_fase1_fase2.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- Plots ----
dir_gr <- file.path(base, "04_reportes_html/graficos")

# Plot Fase 1 vs Fase 2 — Carga~Coh
df_long <- df_total %>% select(fase, caso, carga_coh_r, alpha_mae, especificidad, kappa)

# Agregar nombre para distinguir si el caso solo está en una fase
df_long$caso_label <- paste(df_long$caso, df_long$fase, sep = "\n")
df_long$caso <- factor(df_long$caso,
                       levels = unique(c(df_f1$caso, df_f2$caso)))

p1 <- ggplot(df_long, aes(x = caso, y = carga_coh_r, fill = fase)) +
  geom_col(position = position_dodge(width = .75), width = .65,
           color = "white", linewidth = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(carga_coh_r), "—",
                               sprintf("%.2f", carga_coh_r))),
            position = position_dodge(width = .75), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Fase 1" = "#7f8c8d", "Fase 2" = "#16a085")) +
  scale_y_continuous(limits = c(-0.6, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Carga ~ Coherencia (item-level): Fase 1 (PDF) vs Fase 2 (datos crudos)",
       subtitle = "Indica qué tan bien la coherencia semántica predice la carga factorial real",
       x = NULL, y = "Pearson r", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"))
ggsave(file.path(dir_gr, "20_fase1_vs_fase2_carga_coh.png"),
       p1, width = 11, height = 5.5, dpi = 150)

# Plot α MAE
p2 <- ggplot(df_long, aes(x = caso, y = alpha_mae, fill = fase)) +
  geom_col(position = position_dodge(width = .75), width = .65,
           color = "white", linewidth = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(alpha_mae), "—",
                               sprintf("%.3f", alpha_mae))),
            position = position_dodge(width = .75), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Fase 1" = "#7f8c8d", "Fase 2" = "#16a085")) +
  labs(title = "MAE α empírico vs semántico: Fase 1 vs Fase 2",
       subtitle = "Menor = mayor concordancia con la fiabilidad real",
       x = NULL, y = "MAE", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"))
ggsave(file.path(dir_gr, "21_fase1_vs_fase2_alpha_mae.png"),
       p2, width = 11, height = 5.5, dpi = 150)

cat("\nArchivos generados en:", dir_gr, "\n")
