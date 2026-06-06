# =============================================================================
# Tests adicionales sobre los 10 datasets de Fase 2:
#   - estructura factorial (n_factores + asignacion item-factor)
#   - mantel (similitud coseno vs correlacion empirica)
#   - discriminacion (unicidad semantica vs carga empirica)
#   - redundancia (Jaccard entre pares detectados)
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_estructura_factorial.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "\\.R$", full.names = TRUE)) source(f)

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario"

# Necesitamos: (a) respuestas crudas re-cargadas, (b) mapping, (c) escala SeMiLLa
# Como esta info esta en cada run.R, hacemos sourcing limitado o re-cargamos.
# Estrategia simple: cargar el RDS empirico (que tiene items+mapping) y el RDS
# de la escala (que tiene similitud + ensemble) + cargar respuestas desde el
# CSV original.

casos <- tibble::tribble(
  ~caso,     ~dir,                                                          ~data_path,                            ~sep,
  "DASS-21",   "01_bases_de_datos/fase2_openpsych_en/01_DASS",      "DASS_data_21.02.19/data.csv", "\t",
  "RSES",      "01_bases_de_datos/fase2_openpsych_en/02_RSES",      "RSE/data.csv",                  "\t",
  "Big Five",  "01_bases_de_datos/fase2_openpsych_en/03_BigFive",   "IPIP-FFM-data-8Nov2018/data-final.csv", "\t",
  "ECR",       "01_bases_de_datos/fase2_openpsych_en/04_ECR",       "ECR-data-1March2018/data.csv",  ",",
  "NPI-40",    "01_bases_de_datos/fase2_openpsych_en/05_NPI",       "NPI/data.csv",                  ",",
  "SD3",       "01_bases_de_datos/fase2_openpsych_en/06_SD3",       "SD3/data.csv",                  "\t",
  "Dirty Dozen","01_bases_de_datos/fase2_openpsych_en/07_DD",       "HSNS+DD/data.csv",              "\t",
  "CFCS",      "01_bases_de_datos/fase2_openpsych_en/08_CFCS",      "CFCS/data.csv",                 "\t",
  "HEXACO",    "01_bases_de_datos/fase2_openpsych_en/09_HEXACO",    "HEXACO/data.csv",               "\t",
  "RIASEC",    "01_bases_de_datos/fase2_openpsych_en/10_RIASEC",    "RIASEC/data.csv",               "\t"
)

resultados <- list()

for (i in seq_len(nrow(casos))) {
  caso <- casos$caso[i]
  dir_caso <- file.path(base, casos$dir[i])
  cat("\n### ", caso, " ###\n", sep = "")

  # Cargar empirico (tiene mapping)
  emp_rds <- file.path(dir_caso, "resultados/empirico_calculado.rds")
  esc_rds <- file.path(dir_caso, "resultados/escala_semilla.rds.rds")
  if (!file.exists(emp_rds) || !file.exists(esc_rds)) {
    cat("Falta archivo, skip\n"); next
  }
  emp <- readRDS(emp_rds)
  escala <- readRDS(esc_rds)
  if (is.null(escala$efa)) {
    cat("Re-corriendo ensemble...\n")
    nd <- length(unique(escala$items$dimension))
    escala$efa <- precision_clasificacion(escala, metodo = "ensemble",
      algoritmos = c("kmeans","ward","pam"), n_replicas = 20,
      n_clusters = nd, verbose = FALSE)
  }

  # Cargar respuestas crudas
  data_csv <- file.path(dir_caso, casos$data_path[i])
  if (!file.exists(data_csv)) { cat("CSV no encontrado:", data_csv, "\n"); next }
  raw <- tryCatch(read.table(data_csv, header = TRUE, sep = casos$sep[i],
                              stringsAsFactors = FALSE, fill = TRUE, quote = "\""),
                  error = function(e) NULL)
  if (is.null(raw)) { cat("Error leyendo CSV\n"); next }

  # mapping desde items extraidos
  mapping <- data.frame(
    codigo = emp$items$codigo,
    item   = emp$items$texto,
    dimension = emp$items$dimension_nombre %||% emp$items$dimension,
    stringsAsFactors = FALSE
  )
  if (all(is.na(mapping$dimension))) mapping$dimension <- emp$items$dimension

  # Tomar columnas de respuestas
  cods_match <- intersect(mapping$codigo, names(raw))
  if (length(cods_match) < 6) { cat("Pocos items en raw\n"); next }

  resp <- raw[, cods_match, drop = FALSE]
  resp <- as.data.frame(lapply(resp, function(v) suppressWarnings(as.numeric(v))))
  resp[resp == 0 | resp == -1] <- NA
  resp <- resp[stats::complete.cases(resp), , drop = FALSE]

  set.seed(2026)
  resp <- resp[sample(nrow(resp), min(1500, nrow(resp))), , drop = FALSE]

  # ---- COMPARAR ----
  res <- tryCatch(
    comparar_estructura_factorial(resp, mapping, escala, verbose = FALSE),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(res)) next

  resultados[[caso]] <- res
  cat(sprintf("  n_factores: emp=%s sem=%s teorico=%d  |  ARI=%.3f acc=%.2f  |  mantel r=%.3f  |  redund jaccard=%.2f\n",
              res$n_factores_empirico, res$n_factores_semantico,
              res$n_factores_teorico,
              ifelse(is.na(res$ari), 0, res$ari),
              ifelse(is.na(res$accuracy), 0, res$accuracy),
              res$mantel_r,
              ifelse(is.na(res$solap_redund), 0, res$solap_redund)))
}

# ---- Consolidar ----
df <- do.call(rbind, lapply(names(resultados), function(nm) {
  r <- resultados[[nm]]
  data.frame(
    caso = nm,
    n_emp = r$n_factores_empirico, n_sem = r$n_factores_semantico,
    n_teorico = r$n_factores_teorico,
    delta_emp_sem = r$delta_n_emp_vs_sem,
    delta_emp_teor = r$delta_n_emp_vs_teor,
    delta_sem_teor = r$delta_n_sem_vs_teor,
    ari = r$ari, accuracy = r$accuracy, kappa = r$kappa,
    mantel_r = r$mantel_r, discrim_corr = r$discrim_corr,
    n_redund_sem = r$n_redundantes_sem, n_redund_emp = r$n_redundantes_emp,
    solap_redund = r$solap_redund,
    stringsAsFactors = FALSE
  )
}))

write.csv(df, file.path(base, "04_reportes_html/tests_estructura.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(resultados, file.path(base, "04_reportes_html/tests_estructura_full.rds"))

cat("\n=== TABLA CONSOLIDADA ===\n")
print(df)

cat("\n=== AGREGADOS ===\n")
cat(sprintf("Mantel r promedio:               %+.3f\n", mean(df$mantel_r, na.rm=TRUE)))
cat(sprintf("ARI promedio:                    %+.3f\n", mean(df$ari, na.rm=TRUE)))
cat(sprintf("Accuracy promedio:               %.3f\n",  mean(df$accuracy, na.rm=TRUE)))
cat(sprintf("Discriminacion correlacion prom: %+.3f\n", mean(df$discrim_corr, na.rm=TRUE)))
cat(sprintf("Jaccard redundancia promedio:    %.3f\n",  mean(df$solap_redund, na.rm=TRUE)))
cat(sprintf("|n_emp - n_sem| promedio:        %.2f\n",  mean(df$delta_emp_sem, na.rm=TRUE)))
