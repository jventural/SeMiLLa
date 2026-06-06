# =============================================================================
# pipeline_caso() - Procesar un caso completo end-to-end
# =============================================================================
# Recibe un PDF de validacion psicometrica y entrega: extraccion empirica,
# Excel de items, escala SeMiLLa, y comparacion empirico vs semantico.
# Diseñado para correr 4-5 papers automaticamente.
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/extraer_psicometria_pdf.R")
source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/comparar_con_semilla.R")
for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "\\.R$", full.names = TRUE)) source(f)

pipeline_caso <- function(dir_caso,
                          pdf_archivo,
                          api_key,
                          modelo_llm = "gpt-4.1-mini",
                          paginas_pdf = NULL,
                          n_factores = NULL,
                          verbose = TRUE) {

  dir_res <- file.path(dir_caso, "resultados")
  if (!dir.exists(dir_res)) dir.create(dir_res, recursive = TRUE)

  pdf_path <- file.path(dir_caso, pdf_archivo)
  if (!file.exists(pdf_path)) stop("PDF no encontrado: ", pdf_path)

  caso_name <- basename(dir_caso)
  cat("\n###############################################################\n")
  cat("# CASO: ", caso_name, "\n", sep = "")
  cat("###############################################################\n")

  # ---- 1. EXTRACCION EMPIRICA ----
  cat("\n--- 1/4. Extraccion empirica del PDF ---\n")
  emp <- extraer_psicometria_pdf(
    archivo_pdf  = pdf_path,
    api_key      = api_key,
    modelo       = modelo_llm,
    paginas      = paginas_pdf,
    guardar_json = file.path(dir_res, "empirico_extraido.json"),
    verbose      = verbose
  )
  saveRDS(emp, file.path(dir_res, "empirico_extraido.rds"))

  # Normalizar codigos a uppercase
  emp$dimensiones$codigo <- toupper(emp$dimensiones$codigo)
  emp$items$codigo       <- toupper(emp$items$codigo)
  emp$items$dimension    <- toupper(emp$items$dimension)

  # ---- 2. EXCEL DE ITEMS ----
  cat("\n--- 2/4. Generando Excel de items ---\n")
  archivo_xlsx <- file.path(dir_caso, "items.xlsx")
  psicometria_a_excel(emp, archivo_xlsx)

  # Verificar que no haya placeholders [PENDIENTE: ...]
  test_xlsx <- readxl::read_excel(archivo_xlsx)
  n_pendiente <- sum(grepl("\\[PENDIENTE", test_xlsx$item))
  if (n_pendiente > 0) {
    cat("\nADVERTENCIA:", n_pendiente,
        "items sin texto extraido (placeholders en Excel). Revisa el PDF para llenarlos.\n")
  }

  # ---- 3. SEMILLA (fuente = "usuario") ----
  cat("\n--- 3/4. Pipeline SeMiLLa fuente='usuario' ---\n")
  cache("enable", path = file.path(dir_caso, ".semilla_cache"))

  set.seed(2026)
  escala <- semilla(
    fuente   = "usuario",
    archivo  = archivo_xlsx,
    api_key  = api_key,
    idioma   = ifelse(is.null(emp$escala$idioma), "es", emp$escala$idioma),
    refinar  = FALSE,
    seed     = 2026,
    verbose  = verbose
  )

  n_dim_teorico <- if (is.null(n_factores))
    length(unique(escala$items$dimension)) else n_factores

  # Ensemble (Paso 8)
  ens <- precision_clasificacion(
    escala,
    metodo     = "ensemble",
    algoritmos = c("kmeans", "ward", "pam"),
    n_replicas = 20,
    n_clusters = n_dim_teorico,
    verbose    = verbose
  )
  escala$efa <- ens

  guardar(escala, file.path(dir_res, "escala_semilla.rds"))

  # ---- 4. COMPARACION EMP vs SEM ----
  cat("\n--- 4/4. Comparacion empirico vs semantico ---\n")
  comp <- comparar_con_semilla(
    empirico   = emp,
    escala     = escala,
    dir_salida = dir_res,
    prefijo    = "comparacion",
    verbose    = verbose
  )
  print(comp)
  saveRDS(comp, file.path(dir_res, "comparacion.rds"))

  invisible(list(empirico = emp, escala = escala, comparacion = comp))
}
