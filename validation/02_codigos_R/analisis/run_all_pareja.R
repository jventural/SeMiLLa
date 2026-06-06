# =============================================================================
# Fase 3 — Datos crudos de pareja (Ventura-León 2024)
# 5 escalas con datos reales hispanohablantes (n=315 universitarios)
# =============================================================================

source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/pipeline_dataset.R")
suppressPackageStartupMessages(library(readxl))

api_key <- Sys.getenv("OPENAI_API_KEY")

base <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/01_bases_de_datos/fase3_pareja_local"
df_final <- as.data.frame(read_excel(file.path(base, "datasets/df_final.xlsx")))

procesar_pareja <- function(dir_caso, prefijo_codigo, items_xlsx, dim_default,
                            escala_nombre, constructo,
                            dim_por_factor = NULL) {
  cat("\n###############################################################\n")
  cat("# PAREJA: ", escala_nombre, "\n", sep = "")
  cat("###############################################################\n")

  items_df <- as.data.frame(read_excel(items_xlsx))
  # items_df$Items y items_df$Fraseo son obligatorias
  cols_resp <- items_df$Items
  cols_disp <- intersect(cols_resp, names(df_final))
  if (length(cols_disp) < length(cols_resp))
    warning(sprintf("Faltan items en df_final: %s",
                    paste(setdiff(cols_resp, cols_disp), collapse = ", ")))
  items_df <- items_df[items_df$Items %in% cols_disp, ]

  # Mapping
  if (!is.null(dim_por_factor) && nrow(items_df) == length(dim_por_factor)) {
    # multidim: usa el vector dim_por_factor
    items_df$dimension <- dim_por_factor
  } else if ("Factor" %in% names(items_df)) {
    items_df$dimension <- items_df$Factor
  } else {
    items_df$dimension <- dim_default
  }

  mapping <- data.frame(
    codigo = items_df$Items,
    dimension = items_df$dimension,
    item = items_df$Fraseo,
    stringsAsFactors = FALSE
  )

  resp <- df_final[, mapping$codigo, drop = FALSE]
  resp <- as.data.frame(lapply(resp, function(v) suppressWarnings(as.numeric(v))))
  resp <- resp[stats::complete.cases(resp), ]

  res <- pipeline_dataset(
    dir_dataset = dir_caso, respuestas = resp, mapping = mapping,
    escala_nombre = escala_nombre, constructo = constructo,
    api_key = api_key, idioma = "es",
    poblacion = "Universitarios hispanohablantes (Ventura-Leon 2024, n=315)"
  )
  invisible(res)
}

# 1. Mitos de Amor (multidim segun excel)
procesar_pareja(
  dir_caso = file.path(base, "datasets/01_MitosAmor"),
  prefijo_codigo = "MA",
  items_xlsx = file.path(base, "datasets/01_MitosAmor/items.xlsx"),
  dim_default = "Mitos de Amor",
  escala_nombre = "Mitos de Amor",
  constructo = "Creencias romanticas no realistas"
)

# 2. WAST (1 factor, violencia de pareja)
procesar_pareja(
  dir_caso = file.path(base, "datasets/02_WAST"),
  prefijo_codigo = "WAST",
  items_xlsx = file.path(base, "datasets/02_WAST/items.xlsx"),
  dim_default = "Tamizaje violencia pareja",
  escala_nombre = "WAST",
  constructo = "Violencia en relacion de pareja"
)

# 3. SCP - Comunicacion Peligrosa (1 factor)
procesar_pareja(
  dir_caso = file.path(base, "datasets/03_SCP"),
  prefijo_codigo = "SCP",
  items_xlsx = file.path(base, "datasets/03_SCP/items.xlsx"),
  dim_default = "Comunicacion peligrosa",
  escala_nombre = "SCP",
  constructo = "Comunicacion negativa en pareja"
)

# 4. IR - Involucramiento (1 factor)
procesar_pareja(
  dir_caso = file.path(base, "datasets/04_IR"),
  prefijo_codigo = "IR",
  items_xlsx = file.path(base, "datasets/04_IR/items.xlsx"),
  dim_default = "Involucramiento",
  escala_nombre = "IR",
  constructo = "Involucramiento en la relacion de pareja"
)

# 5. Celos (1 factor)
procesar_pareja(
  dir_caso = file.path(base, "datasets/05_Celos"),
  prefijo_codigo = "C",
  items_xlsx = file.path(base, "datasets/05_Celos/items.xlsx"),
  dim_default = "Celos",
  escala_nombre = "Escala de Celos",
  constructo = "Celos en relacion de pareja"
)

cat("\n=== 5 ESCALAS PAREJA PROCESADAS ===\n")
