# =============================================================================
# Grid de umbrales adaptativos: Q15, Q20, Q25, Q30, Q35
# =============================================================================
# Re-corre comparar_con_semilla() para los 9 casos en cada percentil del grid.
# Plot Sens vs Esp para identificar el percentil Pareto-optimo.
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "[.]R$", full.names = TRUE)) source(f)

suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(tidyr) })

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario"

casos <- tibble::tribble(
  ~codigo, ~label,
  "01_WLEIS_Merino_2016", "WLEIS",
  "02_DASS21",             "DASS-21",
  "03_PANAS",              "PANAS-C",
  "04_SWLS",               "SWLS",
  "05_MBI",                "MBI-HSS",
  "06_RSES",               "RSES",
  "07_UWES",               "UWES-9",
  "08_EAPESA",             "EAPESA",
  "09_EOIE",               "EOIE"
)

grid_q <- c(0.15, 0.20, 0.25, 0.30, 0.35)

resultados_grid <- list()
for (i in seq_len(nrow(casos))) {
  lab <- casos$label[i]
  cod <- casos$codigo[i]
  dir_res <- file.path(base, "casos", cod, "resultados")
  emp_path <- file.path(dir_res, "empirico_extraido.rds")
  esc_path <- if (cod == "01_WLEIS_Merino_2016")
    file.path(dir_res, "wleis_semilla.rds.rds") else
    file.path(dir_res, "escala_semilla.rds.rds")
  if (!file.exists(emp_path) || !file.exists(esc_path)) next

  emp <- readRDS(emp_path)
  emp$dimensiones$codigo <- toupper(emp$dimensiones$codigo)
  emp$items$codigo       <- toupper(emp$items$codigo)
  emp$items$dimension    <- toupper(emp$items$dimension)
  emp$dimensiones$codigo[emp$dimensiones$codigo == "UEO"] <- "UOE"
  emp$items$dimension[emp$items$dimension == "UEO"]       <- "UOE"

  escala <- readRDS(esc_path)
  if (is.null(escala$efa) || !"precision_por_dimension" %in% names(escala$efa)) {
    nd <- length(unique(escala$items$dimension))
    escala$efa <- precision_clasificacion(
      escala, metodo = "ensemble",
      algoritmos = c("kmeans","ward","pam"),
      n_replicas = 20, n_clusters = nd, verbose = FALSE
    )
  }

  for (q in grid_q) {
    comp <- suppressWarnings(comparar_con_semilla(
      emp, escala,
      umbral_coherencia = "adaptivo",
      percentil_adaptivo = q,
      verbose = FALSE
    ))
    resultados_grid[[length(resultados_grid)+1]] <- data.frame(
      caso        = lab,
      percentil   = q,
      umbral_efec = comp$umbral_coherencia,
      kappa       = comp$kappa,
      accuracy    = comp$accuracy,
      sens        = comp$sensibilidad,
      esp         = comp$especificidad,
      ppv         = comp$precision,
      f1          = comp$f1
    )
  }
}

df_grid <- do.call(rbind, resultados_grid)
print(df_grid)

# Resumen por percentil
res_q <- df_grid %>%
  group_by(percentil) %>%
  summarise(
    n_casos      = n(),
    sens_media   = mean(sens, na.rm = TRUE),
    esp_media    = mean(esp,  na.rm = TRUE),
    f1_media     = mean(f1,   na.rm = TRUE),
    kappa_media  = mean(kappa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(youden = sens_media + esp_media - 1,
         distancia_optimo = sqrt((1 - sens_media)^2 + (1 - esp_media)^2))

cat("\n=== RESUMEN POR PERCENTIL ===\n")
print(res_q)

# Plot: Sens vs Esp por percentil (curva ROC discreta)
dir_gr <- file.path(base, "04_reportes_html/graficos")
if (!dir.exists(dir_gr)) dir.create(dir_gr, recursive = TRUE)

p1 <- ggplot(res_q, aes(x = 1 - esp_media, y = sens_media)) +
  geom_path(color = "#1f3a5f", linewidth = 1) +
  geom_point(aes(color = factor(percentil)), size = 5) +
  geom_text(aes(label = sprintf("Q%.0f", percentil*100)),
            hjust = -0.4, vjust = -0.3, size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title    = "Curva Sensibilidad vs (1-Especificidad) por percentil del umbral adaptativo",
       subtitle = "Promedio cross-caso (9 escalas). Mayor area-under-curve = mejor calibracion",
       x = "1 - Especificidad (FPR)", y = "Sensibilidad",
       color = "Percentil") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title    = element_text(face = "bold"))

ggsave(file.path(dir_gr, "14_grid_umbrales_roc.png"),
       p1, width = 8, height = 7, dpi = 150)

# Plot: F1 medio por percentil
p2 <- ggplot(res_q, aes(x = percentil, y = f1_media)) +
  geom_line(linewidth = 1, color = "#1f3a5f") +
  geom_point(size = 4, color = "#e67e22") +
  geom_text(aes(label = sprintf("%.2f", f1_media)), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = grid_q,
                     labels = sprintf("Q%.0f", grid_q * 100)) +
  scale_y_continuous(limits = c(0, max(res_q$f1_media, na.rm = TRUE) * 1.2)) +
  labs(title    = "F1 promedio cross-caso por percentil",
       x = "Percentil del umbral adaptativo", y = "F1 promedio") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(dir_gr, "15_grid_umbrales_f1.png"),
       p2, width = 8, height = 5, dpi = 150)

write.csv(df_grid, file.path(base, "04_reportes_html/grid_umbrales.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(res_q,   file.path(base, "04_reportes_html/grid_umbrales_resumen.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== PERCENTIL OPTIMO ===\n")
opt <- res_q[which.max(res_q$f1_media), ]
cat(sprintf("F1 maximo: %.3f en Q%.0f (Sens=%.2f, Esp=%.2f, Youden=%.2f)\n",
            opt$f1_media, opt$percentil * 100,
            opt$sens_media, opt$esp_media, opt$youden))
opt_y <- res_q[which.max(res_q$youden), ]
cat(sprintf("Youden maximo: %.3f en Q%.0f\n", opt_y$youden, opt_y$percentil * 100))
