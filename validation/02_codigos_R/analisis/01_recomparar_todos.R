# =============================================================================
# Re-correr la comparacion en TODOS los casos con el nuevo comparador
# (umbral adaptativo + soporte omega) — NO re-extrae ni re-corre SeMiLLa,
# solo recalcula las metricas de concordancia.
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "\\.R$", full.names = TRUE)) source(f)

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

for (i in seq_len(nrow(casos))) {
  cod <- casos$codigo[i]
  lab <- casos$label[i]
  dir_caso <- file.path(base, "casos", cod)
  dir_res  <- file.path(dir_caso, "resultados")
  emp_path <- file.path(dir_res, "empirico_extraido.rds")
  esc_path <- if (cod == "01_WLEIS_Merino_2016")
                file.path(dir_res, "wleis_semilla.rds.rds")
              else
                file.path(dir_res, "escala_semilla.rds.rds")

  if (!file.exists(emp_path) || !file.exists(esc_path)) {
    cat("\n[ ] ", lab, " (faltan archivos, skip)\n", sep="")
    next
  }

  cat("\n[*] ", lab, "\n", sep="")

  emp <- readRDS(emp_path)
  # Normalizar codigos
  if (!is.null(emp$dimensiones$codigo)) emp$dimensiones$codigo <- toupper(emp$dimensiones$codigo)
  if (!is.null(emp$items$codigo))       emp$items$codigo       <- toupper(emp$items$codigo)
  if (!is.null(emp$items$dimension))    emp$items$dimension    <- toupper(emp$items$dimension)
  # Alias WLEIS UEO->UOE
  if (!is.null(emp$dimensiones$codigo)) {
    emp$dimensiones$codigo[emp$dimensiones$codigo == "UEO"] <- "UOE"
    emp$items$dimension[emp$items$dimension == "UEO"]       <- "UOE"
  }

  escala <- readRDS(esc_path)
  if (is.null(escala$efa) || !"precision_por_dimension" %in% names(escala$efa)) {
    n_dim <- length(unique(escala$items$dimension))
    cat("    Re-corriendo ensemble (n_clusters=", n_dim, ")\n", sep="")
    ens <- precision_clasificacion(escala, metodo = "ensemble",
                                   algoritmos = c("kmeans","ward","pam"),
                                   n_replicas = 20, n_clusters = n_dim, verbose = FALSE)
    escala$efa <- ens
  }

  comp <- comparar_con_semilla(
    empirico        = emp,
    escala          = escala,
    umbral_coherencia = "adaptivo",
    percentil_adaptivo = 0.15,  # v5: Q15 optimo (grid empirico)
    dir_salida      = dir_res,
    prefijo         = "comparacion_v5",
    verbose         = FALSE
  )
  saveRDS(comp, file.path(dir_res, "comparacion_v5.rds"))
  cat(sprintf("    n=%d, k=%d, dim=%d  |  alpha MAE=%s  r(carga~coh)=%s  Kappa=%s  Sens=%s  Esp=%s  Umbral=%.3f\n",
              comp$n_empirico, comp$n_items, comp$n_dimensiones,
              ifelse(is.na(comp$alpha_mae), "NA", sprintf("%.3f", comp$alpha_mae)),
              ifelse(is.na(comp$carga_coh_r), "NA", sprintf("%+.3f", comp$carga_coh_r)),
              ifelse(is.na(comp$kappa), "NA", sprintf("%+.3f", comp$kappa)),
              ifelse(is.na(comp$sensibilidad), "NA", sprintf("%.3f", comp$sensibilidad)),
              ifelse(is.na(comp$especificidad), "NA", sprintf("%.3f", comp$especificidad)),
              comp$umbral_coherencia))
}

cat("\n=== TODOS LOS CASOS RE-COMPARADOS CON v3 ===\n")
