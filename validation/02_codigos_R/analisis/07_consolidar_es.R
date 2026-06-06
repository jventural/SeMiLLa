# =============================================================================
# Consolidar 5 datasets en ESPAÑOL y comparar EN vs ES
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(gt)
})

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario"

# Mapeo caso -> rds EN y rds ES (idéntica respuesta empírica)
pares <- tibble::tribble(
  ~caso,         ~rds_en,                                                            ~rds_es,
  "DASS-21",     "01_bases_de_datos/fase2_openpsych_en/01_DASS/resultados/comparacion_fase2.rds",
                 "01_bases_de_datos/fase2_openpsych_es/01_DASS21_es/resultados/comparacion_fase2.rds",
  "RSES",        "01_bases_de_datos/fase2_openpsych_en/02_RSES/resultados/comparacion_fase2.rds",
                 "01_bases_de_datos/fase2_openpsych_es/02_RSES_es/resultados/comparacion_fase2.rds",
  "ECR",         "01_bases_de_datos/fase2_openpsych_en/04_ECR/resultados/comparacion_fase2.rds",
                 "01_bases_de_datos/fase2_openpsych_es/03_ECR_es/resultados/comparacion_fase2.rds",
  "Dirty Dozen", "01_bases_de_datos/fase2_openpsych_en/07_DD/resultados/comparacion_fase2.rds",
                 "01_bases_de_datos/fase2_openpsych_es/04_DD_es/resultados/comparacion_fase2.rds",
  "CFCS",        "01_bases_de_datos/fase2_openpsych_en/08_CFCS/resultados/comparacion_fase2.rds",
                 "01_bases_de_datos/fase2_openpsych_es/05_CFCS_es/resultados/comparacion_fase2.rds"
)

cargar <- function(p) if (file.exists(file.path(base, p))) readRDS(file.path(base, p)) else NULL

df_comp <- do.call(rbind, lapply(seq_len(nrow(pares)), function(i) {
  r_en <- cargar(pares$rds_en[i])
  r_es <- cargar(pares$rds_es[i])
  if (is.null(r_en) || is.null(r_es)) return(NULL)
  data.frame(
    caso = pares$caso[i],
    alpha_mae_en   = r_en$alpha_mae,
    alpha_mae_es   = r_es$alpha_mae,
    alpha_r_en     = r_en$alpha_pearson,
    alpha_r_es     = r_es$alpha_pearson,
    carga_coh_en   = r_en$carga_coh_r,
    carga_coh_es   = r_es$carga_coh_r,
    accuracy_en    = r_en$accuracy,
    accuracy_es    = r_es$accuracy,
    especificidad_en = r_en$especificidad,
    especificidad_es = r_es$especificidad,
    kappa_en       = r_en$kappa,
    kappa_es       = r_es$kappa,
    stringsAsFactors = FALSE
  )
}))

write.csv(df_comp, file.path(base, "04_reportes_html/comparacion_en_vs_es.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== COMPARACION EN vs ES ===\n")
print(df_comp)

# Agregados
agg <- data.frame(
  metrica = c("alpha MAE", "Pearson r alpha", "r(carga~coh)", "Accuracy", "Especificidad", "Kappa"),
  en_promedio = c(
    mean(df_comp$alpha_mae_en, na.rm = TRUE),
    mean(df_comp$alpha_r_en,    na.rm = TRUE),
    mean(df_comp$carga_coh_en,  na.rm = TRUE),
    mean(df_comp$accuracy_en,   na.rm = TRUE),
    mean(df_comp$especificidad_en, na.rm = TRUE),
    mean(df_comp$kappa_en,      na.rm = TRUE)
  ),
  es_promedio = c(
    mean(df_comp$alpha_mae_es, na.rm = TRUE),
    mean(df_comp$alpha_r_es,    na.rm = TRUE),
    mean(df_comp$carga_coh_es,  na.rm = TRUE),
    mean(df_comp$accuracy_es,   na.rm = TRUE),
    mean(df_comp$especificidad_es, na.rm = TRUE),
    mean(df_comp$kappa_es,      na.rm = TRUE)
  )
)
agg$delta <- agg$es_promedio - agg$en_promedio
cat("\n=== AGREGADOS POR IDIOMA ===\n")
print(agg)
write.csv(agg, file.path(base, "04_reportes_html/comparacion_en_vs_es_agg.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Plot
df_long <- df_comp %>%
  select(caso,
         carga_coh_en, carga_coh_es,
         accuracy_en, accuracy_es,
         alpha_mae_en, alpha_mae_es) %>%
  pivot_longer(-caso, names_to = "campo", values_to = "valor") %>%
  separate(campo, into = c("metrica", "idioma"), sep = "_(?=[ei])") %>%
  mutate(idioma = recode(idioma, "en" = "Inglés", "es" = "Español"),
         metrica = recode(metrica,
                          "carga_coh" = "r(carga~coh)",
                          "accuracy"  = "Accuracy",
                          "alpha_mae" = "α MAE"))

df_long$caso <- factor(df_long$caso, levels = df_comp$caso)
df_long$metrica <- factor(df_long$metrica, levels = c("α MAE", "r(carga~coh)", "Accuracy"))

p <- ggplot(df_long, aes(x = caso, y = valor, fill = idioma)) +
  geom_col(position = position_dodge(width = .75), width = .65,
           color = "white", linewidth = 0.2, na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y", ncol = 1) +
  geom_text(aes(label = ifelse(is.na(valor), "—", sprintf("%.2f", valor))),
            position = position_dodge(width = .75), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("Inglés" = "#1f3a5f", "Español" = "#c0392b")) +
  labs(title = "SeMiLLa: items en Inglés vs Español (mismos datos crudos)",
       subtitle = "Las respuestas numéricas son idénticas; solo cambian los embeddings",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"))

ggsave(file.path(base, "04_reportes_html/graficos/30_en_vs_es.png"),
       p, width = 10, height = 8, dpi = 150)
cat("\nGuardado plot: graficos/30_en_vs_es.png\n")
