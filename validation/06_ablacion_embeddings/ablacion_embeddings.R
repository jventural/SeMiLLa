# =============================================================================
# 06_ablacion_embeddings / ablacion_embeddings.R
# -----------------------------------------------------------------------------
# Responde a la observacion E2 del LLM Council (jun-2026): todo el flujo depende
# de un modelo de embeddings propietario (text-embedding-3-small, OpenAI).
# Ablacion: recalcular los indices semanticos de la Fase 3 con un modelo ABIERTO
# multilingue (sentence-transformers, local) y comparar contra OpenAI y contra
# el empirico. Si los patrones se sostienen -> las conclusiones no dependen del
# proveedor. NO se fabrica ningun valor.
# =============================================================================
suppressMessages({ library(psych); library(reticulate) })
set.seed(2026)

BASE  <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/39_ART_Proyecto_Semilla"
DIR_F3 <- file.path(BASE, "03_Validacion_empirica/01_bases_de_datos/fase3_pareja_local")
DIR_RESP <- file.path(BASE, "04_Benchmark_optimizacion/datos_respuestas")
DIR_OUT <- file.path(BASE, "03_Validacion_empirica/06_ablacion_embeddings/salidas")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

MODELO_OPEN <- "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"

# omega congenerico unidimensional desde una matriz de correlacion/similitud
omega_unidim <- function(R, nobs) {
  fa1 <- psych::fa(R, nfactors = 1, fm = "ml", n.obs = nobs, warnings = FALSE)
  l <- as.numeric(fa1$loadings[, 1]); u <- 1 - l^2
  sum(l)^2 / (sum(l)^2 + sum(u))
}
coseno <- function(E) {
  E <- as.matrix(E); n <- sqrt(rowSums(E^2)); En <- E / n
  S <- En %*% t(En); diag(S) <- 1; S
}
coherencia <- function(S) { diag(S) <- NA; rowMeans(S, na.rm = TRUE) }

cat(">> cargando modelo abierto:", MODELO_OPEN, "\n")
st <- reticulate::import("sentence_transformers")
model <- st$SentenceTransformer(MODELO_OPEN)
cat(">> modelo cargado.\n")

escalas <- list(
  list(id = "Celos",     dir = "05_Celos"),
  list(id = "IR",        dir = "04_IR"),
  list(id = "SCP",       dir = "03_SCP"),
  list(id = "WAST",      dir = "02_WAST"),
  list(id = "MitosAmor", dir = "01_MitosAmor"))

rows <- list()
for (e in escalas) {
  res <- tryCatch({
    sem <- readRDS(file.path(DIR_F3, e$dir, "resultados", "escala_semilla.rds.rds"))
    resp <- readRDS(file.path(DIR_RESP, paste0(e$id, ".rds")))$respuestas
    cod_s <- as.character(sem$items$codigo)
    common <- intersect(cod_s, names(resp))
    R <- resp[, common, drop = FALSE]
    # descartar items sin varianza (p. ej. WAST con un item constante)
    v <- vapply(R, function(x) stats::var(x, na.rm = TRUE), numeric(1))
    keep <- names(R)[is.finite(v) & v > 0]
    common <- common[common %in% keep]
    idx <- match(common, cod_s)
    textos <- as.character(sem$items$item)[idx]
    S_oa <- sem$similitud[idx, idx, drop = FALSE]
    R <- resp[, common, drop = FALSE]
    nobs <- nrow(R)

    # empirico (Pearson)
    emp <- tryCatch({
      Remp <- cor(R, use = "pairwise.complete.obs")
      le <- as.numeric(psych::fa(Remp, nfactors = 1, fm = "ml", n.obs = nobs, warnings = FALSE)$loadings[, 1])
      list(load = le, omega = omega_unidim(Remp, nobs))
    }, error = function(err) list(load = NA, omega = NA_real_))

    # OpenAI
    coh_oa <- coherencia(S_oa); omega_oa <- omega_unidim(S_oa, nobs)
    r_oa <- suppressWarnings(cor(emp$load, coh_oa))

    # abierto
    emb <- model$encode(textos, show_progress_bar = FALSE)
    S_op <- coseno(emb); coh_op <- coherencia(S_op); omega_op <- omega_unidim(S_op, nobs)
    r_op <- suppressWarnings(cor(emp$load, coh_op))

    data.frame(id = e$id, items = length(common), n = nobs,
      omega_emp = round(emp$omega, 3),
      omega_sem_openai = round(omega_oa, 3),
      omega_sem_abierto = round(omega_op, 3),
      r_load_coh_openai = round(r_oa, 3),
      r_load_coh_abierto = round(r_op, 3), stringsAsFactors = FALSE)
  }, error = function(err) {
    cat("  [ERROR]", e$id, ":", conditionMessage(err), "\n"); NULL
  })
  if (!is.null(res)) {
    rows[[e$id]] <- res
    cat(sprintf("[%s] omega emp=%s | sem(OpenAI)=%.3f sem(open)=%.3f | r(OpenAI)=%s r(open)=%s\n",
                e$id, format(res$omega_emp), res$omega_sem_openai, res$omega_sem_abierto,
                format(res$r_load_coh_openai), format(res$r_load_coh_abierto)))
  }
}
df <- do.call(rbind, rows)
write.csv(df, file.path(DIR_OUT, "tabla_E2_ablacion.csv"), row.names = FALSE)
cat("\n================ TABLA E2 (ablacion) ================\n"); print(df)
cat("\nModelo abierto:", MODELO_OPEN, "\n>>> guardado en", DIR_OUT, "\n")
