# =============================================================================
# Re-correr los 9 casos en ambos modos (sensible vs conservador)
# y comparar las metricas
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "[.]R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

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

modos <- c("sensible", "conservador")
filas <- list()

for (i in seq_len(nrow(casos))) {
  lab <- casos$label[i]; cod <- casos$codigo[i]
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
      n_replicas = 20, n_clusters = nd, verbose = FALSE)
  }

  for (m in modos) {
    comp <- suppressWarnings(comparar_con_semilla(
      emp, escala, modo = m, percentil_adaptivo = 0.15, verbose = FALSE))
    saveRDS(comp, file.path(dir_res, paste0("comparacion_v6_", m, ".rds")))

    filas[[length(filas)+1]] <- data.frame(
      caso = lab, modo = m,
      n_items = comp$n_items, n_dim = comp$n_dimensiones,
      kappa = comp$kappa, accuracy = comp$accuracy,
      sens = comp$sensibilidad, esp = comp$especificidad,
      ppv = comp$precision, f1 = comp$f1,
      stringsAsFactors = FALSE
    )
  }
  cat(sprintf("[%s] OK\n", lab))
}

df <- do.call(rbind, filas)
df_wide <- df %>%
  tidyr::pivot_wider(id_cols = c(caso, n_items, n_dim),
                     names_from = modo,
                     values_from = c(kappa, accuracy, sens, esp, ppv, f1))

write.csv(df, file.path(base, "04_reportes_html/comparacion_modos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_wide, file.path(base, "04_reportes_html/comparacion_modos_wide.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== TABLA LARGA ===\n")
print(df)
cat("\n=== AGREGADOS POR MODO ===\n")
df %>% dplyr::group_by(modo) %>%
  dplyr::summarise(
    n_casos    = dplyr::n(),
    sens_media = mean(sens, na.rm = TRUE),
    esp_media  = mean(esp,  na.rm = TRUE),
    ppv_media  = mean(ppv,  na.rm = TRUE),
    f1_media   = mean(f1,   na.rm = TRUE),
    kappa_media= mean(kappa, na.rm = TRUE)
  ) -> agg
print(agg)
saveRDS(agg, file.path(base, "04_reportes_html/comparacion_modos_agg.rds"))
