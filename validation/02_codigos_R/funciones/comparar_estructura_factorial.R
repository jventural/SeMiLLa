# =============================================================================
# comparar_estructura_factorial() — valida SeMiLLa contra estructura empirica
# =============================================================================
# Para cada dataset crudo, compara:
#   (a) numero de factores: parallel analysis empirico vs semantico
#   (b) asignacion item -> factor: EFA cargas vs ensemble clustering
#   (c) matriz similitud coseno vs matriz correlacion Pearson (mantel-like)
#   (d) discriminacion: unicidad semantica vs carga empirica
# =============================================================================

#' Comparacion completa de estructura factorial empirica vs semantica
#'
#' @param respuestas data.frame con respuestas crudas (filas=sujetos, cols=items)
#' @param mapping data.frame con columnas codigo, item, dimension
#' @param escala objeto semilla con embeddings y similitud (output de
#'        semilla(fuente="usuario", archivo=...))
#' @param verbose Mostrar progreso
#'
#' @return lista con:
#'   - n_factores_empirico: numero estimado por parallel analysis sobre respuestas
#'   - n_factores_semantico: numero estimado por parallel analysis sobre similitud
#'   - n_factores_teorico: dimensiones declaradas en mapping
#'   - delta_n_factores_emp_vs_sem: diferencia absoluta
#'   - asignacion_emp: vector con factor asignado (max loading) por item
#'   - asignacion_sem: vector con cluster asignado (ensemble) por item
#'   - ari_emp_sem: ARI entre las dos asignaciones
#'   - accuracy_emp_sem: % items asignados al mismo factor
#'   - kappa_emp_sem: Cohen kappa
#'   - mantel_r: correlacion entre matriz similitud y matriz correlacion
#'   - discriminacion_corr: correlacion entre unicidad semantica y carga empirica
#'   - n_redundantes_sem: pares de items con sim > .85 segun SeMiLLa
#'   - n_redundantes_emp: pares con correlacion empirica > .65 (umbral homologo)
#'   - solapamiento_redundancia: Jaccard entre los dos conjuntos
#'
#' @export
comparar_estructura_factorial <- function(respuestas, mapping, escala,
                                          umbral_redund_sem = 0.85,
                                          umbral_redund_emp = 0.65,
                                          verbose = TRUE) {
  if (!requireNamespace("psych", quietly = TRUE)) stop("Instala psych")
  if (is.null(escala$similitud) || is.null(escala$embeddings))
    stop("escala debe contener $similitud y $embeddings")

  # ---- Alinear items entre respuestas, mapping y escala ----
  codigos_esc <- escala$items$codigo
  codigos_map <- mapping$codigo
  codigos_resp <- names(respuestas)
  cods_comunes <- intersect(intersect(codigos_esc, codigos_map), codigos_resp)
  if (length(cods_comunes) < 6) stop("Muy pocos items en comun")

  mapping <- mapping[match(cods_comunes, mapping$codigo), ]
  X <- respuestas[, cods_comunes, drop = FALSE]
  X <- as.data.frame(lapply(X, function(v) suppressWarnings(as.numeric(v))))
  X <- X[stats::complete.cases(X), ]

  idx_esc <- match(cods_comunes, escala$items$codigo)
  sim_mat <- escala$similitud[idx_esc, idx_esc, drop = FALSE]
  rownames(sim_mat) <- colnames(sim_mat) <- cods_comunes

  if (verbose) cat("\n[1/4] Parallel analysis empirico (sobre respuestas)...\n")
  pa_emp <- tryCatch(suppressWarnings(suppressMessages(
    psych::fa.parallel(X, fa = "fa", fm = "minres", plot = FALSE,
                       SMC = TRUE, n.iter = 20))), error = function(e) NULL)
  n_fact_emp <- if (!is.null(pa_emp)) pa_emp$nfact else NA_integer_

  if (verbose) cat("[2/4] Parallel analysis semantico (sobre similitud)...\n")
  pa_sem <- tryCatch(suppressWarnings(suppressMessages(
    psych::fa.parallel(sim_mat, n.obs = nrow(X), fa = "fa", fm = "minres",
                       plot = FALSE, SMC = TRUE, n.iter = 20))),
    error = function(e) NULL)
  n_fact_sem <- if (!is.null(pa_sem)) pa_sem$nfact else NA_integer_

  n_fact_teorico <- length(unique(mapping$dimension))
  if (verbose)
    cat(sprintf("    n_emp=%s  n_sem=%s  n_teorico=%d\n",
                n_fact_emp, n_fact_sem, n_fact_teorico))

  # ---- (b) Asignacion item -> factor ----
  if (verbose) cat("[3/4] Asignacion item -> factor (EFA empirico vs ensemble)...\n")
  k <- max(2, n_fact_teorico)  # usar el teorico para que sean comparables

  # EFA empirico con k factores teoricos
  fa_emp <- tryCatch(suppressWarnings(suppressMessages(
    psych::fa(X, nfactors = k, fm = "minres", rotate = "oblimin",
              warnings = FALSE))), error = function(e) NULL)
  if (!is.null(fa_emp)) {
    L <- fa_emp$loadings; L <- as.matrix(L)
    asig_emp <- apply(abs(L), 1, which.max)
    names(asig_emp) <- cods_comunes
  } else {
    asig_emp <- setNames(rep(NA_integer_, length(cods_comunes)), cods_comunes)
  }

  # Asignacion semantica (ensemble): viene de escala$efa$asignacion_clusters
  asig_sem_raw <- if (!is.null(escala$efa) &&
                       !is.null(escala$efa$asignacion_clusters))
    escala$efa$asignacion_clusters else NULL

  if (!is.null(asig_sem_raw)) {
    nm_cluster <- if ("Codigo" %in% names(asig_sem_raw)) asig_sem_raw$Codigo
                  else asig_sem_raw$codigo
    cluster_col <- if ("Cluster_Asignado" %in% names(asig_sem_raw))
      asig_sem_raw$Cluster_Asignado else asig_sem_raw$cluster
    asig_sem <- setNames(as.integer(factor(cluster_col)), nm_cluster)
    asig_sem <- asig_sem[cods_comunes]
  } else {
    asig_sem <- setNames(rep(NA_integer_, length(cods_comunes)), cods_comunes)
  }

  # ARI / kappa / accuracy entre asignaciones
  ari <- NA_real_; acc <- NA_real_; k_cohen <- NA_real_
  if (sum(!is.na(asig_emp) & !is.na(asig_sem)) >= 3) {
    if (requireNamespace("mclust", quietly = TRUE)) {
      ari <- mclust::adjustedRandIndex(asig_emp, asig_sem)
    }
    # Accuracy con permutacion optima (matching hungaro simple)
    tab <- table(emp = asig_emp, sem = asig_sem)
    # Permutacion simple: cada cluster_sem se mapea a su factor_emp mas frecuente
    if (nrow(tab) > 0 && ncol(tab) > 0) {
      asig_sem_remap <- apply(tab, 2, which.max)[as.character(asig_sem)]
      ok <- !is.na(asig_emp) & !is.na(asig_sem_remap)
      acc <- mean(asig_emp[ok] == asig_sem_remap[ok])
      if (requireNamespace("psych", quietly = TRUE)) {
        kt <- table(asig_emp[ok], asig_sem_remap[ok])
        if (nrow(kt) == ncol(kt) && nrow(kt) >= 2) {
          k_cohen <- suppressWarnings(psych::cohen.kappa(kt)$kappa)
        }
      }
    }
  }

  # ---- (c) Mantel: matriz similitud vs matriz correlacion ----
  if (verbose) cat("[4/4] Mantel: similitud coseno vs correlacion empirica...\n")
  cor_mat <- suppressWarnings(stats::cor(X, use = "pairwise.complete.obs"))
  diag_off <- upper.tri(cor_mat)
  v_sim <- sim_mat[diag_off]; v_cor <- cor_mat[diag_off]
  mantel_r <- suppressWarnings(stats::cor(v_sim, v_cor, use = "complete.obs"))

  # ---- (d) Discriminacion: unicidad semantica vs carga empirica ----
  if (!is.null(fa_emp)) {
    # Carga maxima absoluta por item
    cargas_max <- apply(abs(L), 1, max)
    # Unicidad semantica: 1 - similitud media a otros items
    diag(sim_mat) <- NA
    unicidad_sem <- 1 - rowMeans(sim_mat, na.rm = TRUE)
    discrim_corr <- suppressWarnings(stats::cor(unicidad_sem, cargas_max,
                                                use = "complete.obs"))
  } else {
    discrim_corr <- NA_real_
  }

  # ---- (e) Redundancia: pares detectados ----
  pares_sem <- which(sim_mat > umbral_redund_sem & upper.tri(sim_mat), arr.ind = TRUE)
  cor_mat_off <- cor_mat; diag(cor_mat_off) <- NA
  pares_emp <- which(cor_mat_off > umbral_redund_emp & upper.tri(cor_mat_off),
                     arr.ind = TRUE)
  set_sem <- if (nrow(pares_sem) > 0)
    apply(pares_sem, 1, function(r) paste(sort(r), collapse = "-")) else character(0)
  set_emp <- if (nrow(pares_emp) > 0)
    apply(pares_emp, 1, function(r) paste(sort(r), collapse = "-")) else character(0)
  jaccard <- if (length(union(set_sem, set_emp)) > 0)
    length(intersect(set_sem, set_emp)) / length(union(set_sem, set_emp))
    else NA_real_

  list(
    n_factores_empirico   = n_fact_emp,
    n_factores_semantico  = n_fact_sem,
    n_factores_teorico    = n_fact_teorico,
    delta_n_emp_vs_sem    = abs(n_fact_emp - n_fact_sem),
    delta_n_emp_vs_teor   = abs(n_fact_emp - n_fact_teorico),
    delta_n_sem_vs_teor   = abs(n_fact_sem - n_fact_teorico),

    asignacion_emp = asig_emp,
    asignacion_sem = asig_sem,
    ari            = ari,
    accuracy       = acc,
    kappa          = k_cohen,

    mantel_r       = mantel_r,
    discrim_corr   = discrim_corr,

    n_redundantes_sem = length(set_sem),
    n_redundantes_emp = length(set_emp),
    solap_redund      = jaccard
  )
}
