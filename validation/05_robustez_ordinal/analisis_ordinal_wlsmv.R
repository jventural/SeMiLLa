# =============================================================================
# 05_robustez_ordinal / analisis_ordinal_wlsmv.R
# -----------------------------------------------------------------------------
# Respuesta a observaciones de revisores C3 y C4 (revision mayor, jun-2026):
#   C3: el omega empirico de referencia se calculo con correlaciones de Pearson
#       sobre items Likert ordinales -> recalcular con correlaciones POLICORICAS.
#   C4: faltan indices de ajuste confirmatorio -> ajustar CFA con estimador
#       WLSMV (policorico, ordinal) por escala y reportar CFI/TLI/RMSEA/SRMR.
#
# Datos crudos (fieles, mismas matrices del manuscrito):
#   ../../04_Benchmark_optimizacion/datos_respuestas/<id>.rds
#   = list(id, fase, respuestas[n x items], mapping[codigo, dimension])
#
# Herramientas: lavaan::cfa (WLSMV, ordered), psych (omega policorico),
#               semTools::compRelSEM (omega ordinal del modelo), y
#               PsyMetricTools::EFA_modern (EFA exploratorio WLSMV) como chequeo.
# NO se fabrica ningun valor: se reporta lo que devuelven los modelos.
# =============================================================================

suppressMessages({
  library(lavaan); library(psych); library(semTools); library(PsyMetricTools)
})
set.seed(2026)

BASE     <- "D:/1. INVESTIGACIONES/3. ARTICULOS PENDIENTES/2026/39_ART_Proyecto_Semilla"
DIR_RESP <- file.path(BASE, "04_Benchmark_optimizacion", "datos_respuestas")
DIR_OUT  <- file.path(BASE, "03_Validacion_empirica", "05_robustez_ordinal", "salidas")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

## --- helpers ---------------------------------------------------------------
build_model <- function(inames, dim_of_item) {
  dims <- unique(dim_of_item)
  Fn <- paste0("F", seq_along(dims))
  lines <- vapply(seq_along(dims), function(k) {
    its <- inames[dim_of_item == dims[k]]
    paste0(Fn[k], " =~ ", paste(its, collapse = " + "))
  }, character(1))
  paste(lines, collapse = "\n")
}

# omega congenerico (McDonald) unidimensional desde una matriz de correlacion
omega_unidim <- function(data, poly = FALSE) {
  R  <- if (poly) psych::polychoric(data)$rho else cor(data, use = "pairwise.complete.obs")
  fa1 <- psych::fa(R, nfactors = 1, fm = "ml", n.obs = nrow(data), warnings = FALSE)
  l <- as.numeric(fa1$loadings[, 1]); u <- 1 - l^2
  sum(l)^2 / (sum(l)^2 + sum(u))
}

files <- list.files(DIR_RESP, pattern = "\\.rds$", full.names = TRUE)
cfa_rows <- list(); omega_rows <- list()

for (f in files) {
  x <- readRDS(f); id <- x$id; fase <- x$fase
  resp <- x$respuestas; map <- x$mapping
  items <- map$codigo[map$codigo %in% names(resp)]
  resp2 <- resp[, items, drop = FALSE]
  inames <- paste0("I", seq_along(items)); names(resp2) <- inames
  dim_of_item <- map$dimension[match(items, map$codigo)]
  K <- length(unique(dim_of_item))
  model <- build_model(inames, dim_of_item)

  ## ---- C4: CFA WLSMV (ordinal / policorico) ----
  fit <- tryCatch(
    lavaan::cfa(model, data = resp2, ordered = names(resp2),
                estimator = "WLSMV", std.lv = TRUE),
    error = function(e) e)

  if (inherits(fit, "error")) {
    cfa_rows[[id]] <- data.frame(id, fase, K, n = nrow(resp2), items = length(items),
      CFI = NA, TLI = NA, RMSEA = NA, SRMR = NA, omega_wlsmv = NA,
      conv = FALSE, nota = substr(conditionMessage(fit), 1, 50),
      stringsAsFactors = FALSE)
  } else {
    conv <- isTRUE(tryCatch(lavInspect(fit, "converged"), error = function(e) FALSE))
    fm <- tryCatch(fitMeasures(fit, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr")),
                   error = function(e) setNames(rep(NA,4), c("a","b","c","d")))
    ow <- tryCatch(mean(as.numeric(semTools::compRelSEM(fit)), na.rm = TRUE),
                   error = function(e) NA_real_)
    hey <- tryCatch(any(abs(as.numeric(unlist(lavInspect(fit,"std")$lambda))) > 1, na.rm=TRUE),
                    error = function(e) NA)
    cfa_rows[[id]] <- data.frame(id, fase, K, n = nrow(resp2), items = length(items),
      CFI = unname(fm[1]), TLI = unname(fm[2]), RMSEA = unname(fm[3]), SRMR = unname(fm[4]),
      omega_wlsmv = ow, conv = conv,
      nota = ifelse(isTRUE(hey), "carga>1", ""), stringsAsFactors = FALSE)
  }
  cat(sprintf("[CFA] %-12s K=%d ... listo\n", id, K))

  ## ---- C3: omega Pearson vs policorico (Fase 3, unidimensionales) ----
  if (fase == "F3") {
    op <- tryCatch(omega_unidim(resp2, poly = FALSE), error = function(e) NA_real_)
    oo <- tryCatch(omega_unidim(resp2, poly = TRUE),  error = function(e) NA_real_)
    omega_rows[[id]] <- data.frame(id, n = nrow(resp2), items = length(items),
      omega_pearson = op, omega_policorico = oo, dif = oo - op,
      stringsAsFactors = FALSE)
    cat(sprintf("[OMEGA] %-12s Pearson=%.3f Policorico=%.3f\n", id, op, oo))
  }
}

cfa_df   <- do.call(rbind, cfa_rows)
omega_df <- do.call(rbind, omega_rows)
write.csv(cfa_df,   file.path(DIR_OUT, "tabla_C4_cfa_wlsmv.csv"),               row.names = FALSE)
write.csv(omega_df, file.path(DIR_OUT, "tabla_C3_omega_pearson_vs_policorico.csv"), row.names = FALSE)

cat("\n================ TABLA C4 (CFA WLSMV) ================\n"); print(cfa_df, digits = 3)
cat("\n================ TABLA C3 (omega) ===================\n"); print(omega_df, digits = 3)

## ---- EFA exploratorio WLSMV (PsyMetricTools::EFA_modern) como chequeo ----
## name_items es un PREFIJO: la funcion genera I1..In internamente.
cat("\n================ EFA_modern (WLSMV) chequeo =========\n")
efa_rows <- list()
for (id in c("DASS-21", "DirtyDozen", "SD3")) {
  ff <- file.path(DIR_RESP, paste0(id, ".rds")); if (!file.exists(ff)) next
  x <- readRDS(ff); resp <- x$respuestas; map <- x$mapping
  items <- map$codigo[map$codigo %in% names(resp)]
  resp2 <- resp[, items, drop = FALSE]; names(resp2) <- paste0("I", seq_along(items))
  K <- length(unique(map$dimension[match(items, map$codigo)]))
  ef <- tryCatch(
    PsyMetricTools::EFA_modern(n_factors = K, n_items = length(items),
      name_items = "I", data = resp2,
      apply_threshold = FALSE, estimator = "WLSMV"),
    error = function(e) paste("ERROR:", conditionMessage(e)))
  cat("\n--", id, "(K teorico =", K, ") --\n")
  if (is.character(ef)) { cat(ef, "\n"); next }
  bo <- ef$Bondades_Original
  keep <- intersect(c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"), names(bo))
  bo <- bo[, keep, drop = FALSE]; bo$n_factors <- seq_len(nrow(bo)); bo$id <- id; bo$K_teorico <- K
  efa_rows[[id]] <- bo
  print(round(bo[, c("n_factors", keep)], 3))
}
if (length(efa_rows)) {
  efa_df <- do.call(rbind, efa_rows)
  write.csv(efa_df, file.path(DIR_OUT, "tabla_EFA_modern_wlsmv.csv"), row.names = FALSE)
}
cat("\n>>> Analisis ordinal terminado. CSVs en:", DIR_OUT, "\n")
