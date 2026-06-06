# =============================================================================
# Test sintetico — Adversarial item probing sobre WLEIS
# =============================================================================
# Toma el WLEIS (16 items, 4 dim, ground truth conocida) y le inyecta items
# "envenenados" con etiqueta de dimension MENTIROSA. Verifica si SeMiLLa los
# detecta como problematicos.
#
# Tipos de envenenamiento:
#   A. Cross-dim: item real de SEA marcado como OEA -> debe ser items_mal
#   B. Constructo distinto: item de ansiedad marcado como ROE -> baja coh
#   C. Redaccion alterada: item OEA reescrito como SEA -> cross-loading
#
# Metrica: Sensibilidad real (items_envenenados detectados/total envenenados)
#          Especificidad real (items sanos no flaggeados/total sanos)
# =============================================================================

for (f in list.files("D:/14. LIBRERIAS/SeMiLLa/R", pattern = "[.]R$", full.names = TRUE)) source(f)
source("D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/02_codigos_R/funciones/extraer_psicometria_pdf.R")

api_key <- Sys.getenv("OPENAI_API_KEY")

dir_test <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/43_ART_WLEIS_SeMiLLa_usuario/04_reportes_html/test_envenenamiento"
if (!dir.exists(dir_test)) dir.create(dir_test, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. WLEIS base
# -----------------------------------------------------------------------------
items_sanos <- data.frame(
  codigo    = c("WLEIS1","WLEIS5","WLEIS9","WLEIS13",
                "WLEIS2","WLEIS6","WLEIS10","WLEIS14",
                "WLEIS3","WLEIS7","WLEIS11","WLEIS15",
                "WLEIS4","WLEIS8","WLEIS12","WLEIS16"),
  dimension = c(rep("Valoracion de las propias emociones (SEA)", 4),
                rep("Valoracion de las emociones en otros (OEA)", 4),
                rep("Uso de la emocion (UOE)", 4),
                rep("Regulacion de las propias emociones (ROE)", 4)),
  item = c(
    "La mayor parte del tiempo se por que tengo ciertos sentimientos.",
    "Tengo un buen entendimiento de mis propias emociones.",
    "Realmente comprendo lo que yo siento.",
    "Siempre se si estoy o no estoy feliz.",
    "Siempre puedo identificar las emociones de mis amigos a partir de su conducta.",
    "Soy un buen observador de las emociones de los demas.",
    "Soy sensible a los sentimientos y emociones de otras personas.",
    "Tengo un buen entendimiento de las emociones de las personas a mi alrededor.",
    "Siempre me fijo metas y trato de dar lo mejor de mi para alcanzarlas.",
    "Siempre me digo a mi mismo que soy una persona competente.",
    "Soy una persona automotivada.",
    "Siempre me animo a mi mismo a dar lo mejor.",
    "Soy capaz de controlar mi temperamento y manejar las dificultades de manera racional.",
    "Soy bastante capaz de controlar mis propias emociones.",
    "Cuando estoy muy enojado puedo calmarme rapidamente.",
    "Tengo un buen autocontrol de mis emociones."
  ),
  envenenado = FALSE,
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 2. POOL de items envenenados (6 candidatos)
# -----------------------------------------------------------------------------
items_veneno <- data.frame(
  codigo    = c("POIS_A1", "POIS_A2", "POIS_B1", "POIS_B2", "POIS_C1", "POIS_C2"),
  # Asignados deliberadamente a dimension MENTIROSA
  dimension = c(
    "Uso de la emocion (UOE)",                       # A1: item SEA disfrazado de UOE
    "Regulacion de las propias emociones (ROE)",     # A2: item OEA disfrazado de ROE
    "Regulacion de las propias emociones (ROE)",     # B1: item ansiedad como ROE
    "Uso de la emocion (UOE)",                       # B2: item de motivacion fisica
    "Valoracion de las propias emociones (SEA)",     # C1: item OEA reescrito como SEA
    "Valoracion de las emociones en otros (OEA)"     # C2: item SEA reescrito como OEA
  ),
  item = c(
    # A. Cross-dim (item es SEA pero marcado como UOE)
    "Reconozco con claridad las emociones que estoy sintiendo en cada momento.",
    # A. Cross-dim (item es OEA pero marcado como ROE)
    "Percibo facilmente cuando las personas estan tristes o preocupadas.",
    # B. Constructo distinto (ansiedad)
    "Me preocupa mucho lo que otros piensan de mi.",
    # B. Constructo distinto (motivacion fisica)
    "Hago ejercicio fisico tres veces por semana.",
    # C. Redaccion alterada (referente OEA pero rotulado SEA)
    "Entiendo claramente las emociones de las personas cercanas a mi.",
    # C. Redaccion alterada (referente SEA pero rotulado OEA)
    "Tengo una buena comprension de mis propios sentimientos."
  ),
  envenenado = TRUE,
  tipo_veneno = c("A","A","B","B","C","C"),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 3. CONSTRUIR 4 VERSIONES CON CRECIENTE NIVEL DE ENVENENAMIENTO
# -----------------------------------------------------------------------------
construir_version <- function(items_sanos, items_veneno, indices_veneno) {
  veneno_sel <- items_veneno[indices_veneno, , drop = FALSE]
  veneno_sel$tipo_veneno <- NULL
  rbind(items_sanos, veneno_sel)
}

versiones <- list(
  V1 = construir_version(items_sanos, items_veneno, 1),         # 16+1
  V2 = construir_version(items_sanos, items_veneno, c(1, 3)),   # 16+2
  V3 = construir_version(items_sanos, items_veneno, c(1, 2, 3, 5)), # 16+4
  V4 = construir_version(items_sanos, items_veneno, 1:6)        # 16+6
)

set.seed(2026)
cache("enable", path = file.path(dir_test, ".semilla_cache"))

resultados_test <- list()

for (v in names(versiones)) {
  cat("\n###############################################################\n")
  cat("# VERSION ", v, " (n_items = ", nrow(versiones[[v]]),
      ", envenenados = ", sum(versiones[[v]]$envenenado), ")\n", sep = "")
  cat("###############################################################\n")

  df_v <- versiones[[v]]

  # Pasar al pipeline minimalmente: usar fuente='usuario' con items_df
  escala <- semilla(
    fuente   = "usuario",
    items_df = df_v[, c("codigo","dimension","item")],
    concepto = "Inteligencia emocional con items envenenados",
    definicion = "Test sintetico de adversarial item probing",
    api_key  = api_key,
    seed     = 2026,
    verbose  = FALSE
  )

  ens <- precision_clasificacion(
    escala, metodo = "ensemble",
    algoritmos = c("kmeans","ward","pam"),
    n_replicas = 20,
    n_clusters = 4,
    verbose = FALSE
  )
  escala$efa <- ens

  # Items flagged por SeMiLLa
  items_mal <- ens$items_mal_clasificados$Codigo

  coh <- analizar_coherencia(escala, verbose = FALSE)
  coh_item <- coh$datos_por_item
  umbral_adapt <- as.numeric(quantile(coh_item$Similitud_Media, 0.25, na.rm = TRUE))
  if (umbral_adapt > 0.60) umbral_adapt <- 0.50
  items_lowcoh <- coh_item$Codigo[coh_item$Similitud_Media < umbral_adapt]
  flagged_sem  <- unique(c(items_mal, items_lowcoh))

  # Ground truth controlada
  df_v$flagged_sem  <- df_v$codigo %in% flagged_sem
  df_v$ground_truth <- df_v$envenenado  # TRUE = poisoned (deberia ser flagged)

  # Metricas
  TP <- sum( df_v$ground_truth &  df_v$flagged_sem)
  FN <- sum( df_v$ground_truth & !df_v$flagged_sem)
  FP <- sum(!df_v$ground_truth &  df_v$flagged_sem)
  TN <- sum(!df_v$ground_truth & !df_v$flagged_sem)
  sens <- if ((TP+FN) > 0) TP/(TP+FN) else NA_real_
  esp  <- if ((TN+FP) > 0) TN/(TN+FP) else NA_real_
  ppv  <- if ((TP+FP) > 0) TP/(TP+FP) else NA_real_
  f1   <- if (!is.na(sens) && !is.na(ppv) && (sens+ppv) > 0) 2*sens*ppv/(sens+ppv) else NA_real_

  cat(sprintf("  Umbral coherencia (Q25): %.3f\n", umbral_adapt))
  cat(sprintf("  TP=%d  FN=%d  FP=%d  TN=%d\n", TP, FN, FP, TN))
  cat(sprintf("  Sens=%.2f  Esp=%.2f  PPV=%.2f  F1=%.2f\n", sens, esp, ppv, f1))
  cat("  Items envenenados detectados:\n")
  print(df_v[df_v$envenenado, c("codigo","dimension","flagged_sem")])

  resultados_test[[v]] <- list(
    version   = v,
    n_items   = nrow(df_v),
    n_poison  = sum(df_v$envenenado),
    umbral    = umbral_adapt,
    TP=TP, FN=FN, FP=FP, TN=TN,
    sens=sens, esp=esp, ppv=ppv, f1=f1,
    df_v = df_v
  )
}

# -----------------------------------------------------------------------------
# 4. CONSOLIDAR + GUARDAR
# -----------------------------------------------------------------------------
df_test <- do.call(rbind, lapply(resultados_test, function(r) {
  data.frame(version = r$version, n_items = r$n_items, n_poison = r$n_poison,
             TP=r$TP, FN=r$FN, FP=r$FP, TN=r$TN,
             sensibilidad=r$sens, especificidad=r$esp,
             precision_ppv=r$ppv, f1=r$f1, umbral_coh=r$umbral)
}))

cat("\n=================================================================\n")
cat("RESUMEN — TEST SINTETICO DE ENVENENAMIENTO (WLEIS adversarial)\n")
cat("=================================================================\n")
print(df_test, row.names = FALSE)

write.csv(df_test, file.path(dir_test, "resultados_envenenamiento.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(resultados_test, file.path(dir_test, "resultados_envenenamiento.rds"))

cat("\nGuardado en:", dir_test, "\n")
