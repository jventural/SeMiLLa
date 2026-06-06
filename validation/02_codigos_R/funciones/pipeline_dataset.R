# =============================================================================
# pipeline_dataset() — orquesta procesamiento de un dataset de Fase 2
# =============================================================================
# Equivalente a pipeline_caso() de Fase 1 pero usando datos crudos.
# Recibe respuestas + mapping y entrega:
#   - empirico (via calcular_psicometria_empirica) en lugar de extraer_psicometria_pdf
#   - escala (via semilla(fuente='usuario'))
#   - comparacion (via comparar_con_semilla)
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/calcular_psicometria_empirica.R")
source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/extraer_psicometria_pdf.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "\\.R$", full.names = TRUE)) source(f)

pipeline_dataset <- function(dir_dataset,
                             respuestas,
                             mapping,
                             escala_nombre,
                             constructo = escala_nombre,
                             api_key,
                             idioma = "es",
                             poblacion = NULL,
                             umbral_carga = 0.40,
                             umbral_itc   = 0.30,
                             modo_comparacion = "conservador",
                             verbose = TRUE) {

  dir_res <- file.path(dir_dataset, "resultados")
  if (!dir.exists(dir_res)) dir.create(dir_res, recursive = TRUE)

  if (verbose) {
    cat("\n###############################################################\n")
    cat("# Fase 2 dataset: ", basename(dir_dataset), "\n", sep = "")
    cat("###############################################################\n")
  }

  # ---- 1. Empirico (calculado por nosotros) -------------------------------
  emp <- calcular_psicometria_empirica(
    respuestas = respuestas,
    mapping    = mapping,
    escala_nombre = escala_nombre,
    constructo    = constructo,
    idioma        = idioma,
    poblacion     = poblacion,
    umbral_carga_problematico = umbral_carga,
    umbral_itc_problematico   = umbral_itc,
    verbose = verbose
  )
  saveRDS(emp, file.path(dir_res, "empirico_calculado.rds"))

  # ---- 2. Excel para SeMiLLa ----------------------------------------------
  # Generar Excel compatible con leer_escala()
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  archivo_xlsx <- file.path(dir_dataset, "items.xlsx")
  dims_emp <- emp$dimensiones
  items_emp <- emp$items
  # Mapear dimension codigo -> nombre completo
  items_emp$dim_nombre <- dims_emp$nombre[match(items_emp$dimension, dims_emp$codigo)]
  excel_df <- data.frame(
    constructo            = c(emp$constructo$nombre, rep("", nrow(items_emp) - 1)),
    definicion_constructo = c(ifelse(is.null(emp$constructo$definicion), "", emp$constructo$definicion),
                              rep("", nrow(items_emp) - 1)),
    dimension             = items_emp$dim_nombre,
    definicion_dimension  = "",
    codigo                = items_emp$codigo,
    item                  = items_emp$texto,
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(excel_df, archivo_xlsx)

  # ---- 3. SeMiLLa (fuente = "usuario") ------------------------------------
  cache("enable", path = file.path(dir_dataset, ".semilla_cache"))
  set.seed(2026)
  escala <- semilla(
    fuente   = "usuario",
    archivo  = archivo_xlsx,
    api_key  = api_key,
    idioma   = idioma,
    refinar  = FALSE,
    seed     = 2026,
    verbose  = verbose
  )
  n_dim <- length(unique(escala$items$dimension))
  ens <- precision_clasificacion(
    escala, metodo = "ensemble",
    algoritmos = c("kmeans","ward","pam"),
    n_replicas = 20, n_clusters = n_dim,
    verbose = verbose
  )
  escala$efa <- ens
  guardar(escala, file.path(dir_res, "escala_semilla.rds"))

  # ---- 4. Comparacion empirico (crudo) vs semantico -----------------------
  comp <- comparar_con_semilla(
    empirico = emp,
    escala   = escala,
    modo     = modo_comparacion,
    dir_salida = dir_res,
    prefijo    = "comparacion_fase2",
    verbose    = verbose
  )
  print(comp)
  saveRDS(comp, file.path(dir_res, "comparacion_fase2.rds"))

  invisible(list(empirico = emp, escala = escala, comparacion = comp))
}
