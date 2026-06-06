# =============================================================================
# calcular_psicometria_empirica()
# =============================================================================
# Toma respuestas crudas + mapeo items->dimensiones y calcula psicometria
# clasica: alpha, omega, cargas factoriales (1F-CFA via lavaan o EFA via
# psych::fa), item-total correlation (corregida), reliability-if-deleted.
#
# Devuelve un objeto compatible con comparar_con_semilla() (clase
# 'psicometria_extraida'), igual que el output de extraer_psicometria_pdf().
#
# Esto permite usar el MISMO comparator sin importar si el lado empirico
# viene de un PDF o de datos crudos.
# =============================================================================

#' Calcular psicometria clasica desde datos crudos
#'
#' @param respuestas data.frame con respuestas (filas=sujetos, cols=items).
#'        Las columnas deben coincidir con \code{mapping$codigo}.
#' @param mapping data.frame con columnas obligatorias:
#'   - codigo: nombre de la columna en \code{respuestas}
#'   - item: texto del item
#'   - dimension: dimension teorica
#' @param escala_nombre Nombre de la escala (ej: "WLEIS")
#' @param constructo Nombre del constructo
#' @param definicion_constructo Definicion (opcional)
#' @param idioma Idioma de los items (default "es")
#' @param umbral_carga_problematico Carga factorial bajo la cual marcamos
#'        un item como problematico (default 0.40, criterio uniforme)
#' @param umbral_itc_problematico Item-total correlation corregida bajo la
#'        cual marcamos como problematico (default 0.30)
#' @param tipo_metrica "alpha" o "omega" (default "omega" via McDonald)
#' @param verbose Mostrar progreso
#'
#' @return Objeto S3 \code{psicometria_extraida} con:
#'   - escala, constructo, dimensiones, items, meta
#'   - df_items_full: data.frame con metricas item-level adicionales
#'     (ITC, reliability-if-deleted, carga, problematico_motivo)
#'
#' @export
calcular_psicometria_empirica <- function(respuestas,
                                          mapping,
                                          escala_nombre,
                                          constructo = escala_nombre,
                                          definicion_constructo = NULL,
                                          idioma = "es",
                                          poblacion = NULL,
                                          umbral_carga_problematico = 0.40,
                                          umbral_itc_problematico   = 0.30,
                                          tipo_metrica = c("omega","alpha"),
                                          verbose = TRUE) {

  if (!requireNamespace("psych", quietly = TRUE))
    stop("Instala psych: install.packages('psych')")

  tipo_metrica <- match.arg(tipo_metrica)

  # Validar mapping
  cols_req <- c("codigo","item","dimension")
  if (!all(cols_req %in% names(mapping)))
    stop("mapping debe tener columnas: codigo, item, dimension")

  # Filtrar respuestas a las columnas que estan en mapping
  cols_validas <- intersect(mapping$codigo, names(respuestas))
  if (length(cols_validas) == 0)
    stop("Ninguna columna del mapping aparece en respuestas. Revisa los codigos.")

  if (length(cols_validas) < nrow(mapping)) {
    warning(sprintf("Faltan %d items en respuestas: %s",
                    nrow(mapping) - length(cols_validas),
                    paste(setdiff(mapping$codigo, cols_validas), collapse = ", ")))
    mapping <- mapping[mapping$codigo %in% cols_validas, , drop = FALSE]
  }

  X <- respuestas[, mapping$codigo, drop = FALSE]
  X <- as.data.frame(lapply(X, function(v) suppressWarnings(as.numeric(v))))
  # Eliminar filas con todos NA
  X <- X[rowSums(!is.na(X)) > 0, , drop = FALSE]
  n_sujetos <- nrow(X)

  if (verbose) {
    cat("\n=== PSICOMETRIA EMPIRICA (datos crudos) ===\n")
    cat("Escala:   ", escala_nombre, "\n", sep = "")
    cat("N:        ", n_sujetos, "\n", sep = "")
    cat("Items:    ", nrow(mapping), "\n", sep = "")
    cat("Dimensiones: ", length(unique(mapping$dimension)), "\n", sep = "")
  }

  # ---- Por dimension: alpha/omega + cargas + ITC ----
  dims <- unique(mapping$dimension)
  dim_filas <- list()
  item_filas <- list()

  for (d in dims) {
    cods <- mapping$codigo[mapping$dimension == d]
    Xd <- X[, cods, drop = FALSE]
    Xd_complete <- Xd[stats::complete.cases(Xd), , drop = FALSE]
    n_dim <- nrow(Xd_complete)
    k_dim <- length(cods)

    # Alpha de Cronbach
    alpha_obj <- tryCatch(
      suppressWarnings(psych::alpha(Xd_complete, check.keys = FALSE,
                                    warnings = FALSE)),
      error = function(e) NULL)
    alpha_d <- if (!is.null(alpha_obj)) as.numeric(alpha_obj$total$raw_alpha) else NA_real_

    # Omega: se calcula mas abajo, a mano (McDonald) desde las cargas minres.

    # ITC corregida + reliability-if-deleted
    itc <- if (!is.null(alpha_obj))
      as.numeric(alpha_obj$item.stats$r.drop) else rep(NA_real_, k_dim)
    rel_if_del <- if (!is.null(alpha_obj))
      as.numeric(alpha_obj$alpha.drop$raw_alpha) else rep(NA_real_, k_dim)

    # Cargas factoriales (EFA unifactorial dentro de la dimension)
    cargas <- if (k_dim >= 3) {
      fa_obj <- tryCatch(
        suppressWarnings(suppressMessages(
          psych::fa(Xd_complete, nfactors = 1, fm = "minres",
                    rotate = "none", warnings = FALSE))),
        error = function(e) NULL)
      if (!is.null(fa_obj)) as.numeric(fa_obj$loadings[, 1])
      else rep(NA_real_, k_dim)
    } else if (k_dim == 2) {
      r <- suppressWarnings(stats::cor(Xd_complete[, 1], Xd_complete[, 2],
                                       use = "pairwise.complete.obs"))
      rep(sqrt(abs(r)), 2)
    } else rep(NA_real_, k_dim)

    cargas <- pmin(pmax(cargas, -1), 1)

    # Omega de McDonald a mano desde las cargas (minres): para k>=3 es identico a
    # psych::omega(nfactors=1)$omega.tot. Mismo calculo que el Paso 26 de la app,
    # de modo que el paquete y la app Shiny devuelven el mismo omega empirico.
    omega_d <- if (k_dim >= 3) {
      lf <- cargas[is.finite(cargas)]
      if (length(lf) >= 3) { s <- sum(lf); s^2 / (s^2 + sum(1 - lf^2)) } else NA_real_
    } else NA_real_

    # Marcar items problematicos: carga<umbral O ITC<umbral
    problematicos <- (abs(cargas) < umbral_carga_problematico) |
                     (itc          < umbral_itc_problematico)
    motivos <- character(k_dim)
    motivos[abs(cargas) < umbral_carga_problematico] <- "carga<.40"
    motivos[itc < umbral_itc_problematico] <-
      paste(motivos[itc < umbral_itc_problematico], "itc<.30", sep = ";")
    motivos <- gsub("^;", "", motivos)
    motivos[!nzchar(motivos)] <- NA_character_

    dim_filas[[length(dim_filas)+1]] <- data.frame(
      codigo = .codigo_corto_dim(d),
      nombre = d,
      definicion = NA_character_,
      n_items = k_dim,
      alpha = alpha_d,
      alpha_ic_inf = NA_real_, alpha_ic_sup = NA_real_,
      tucker = NA_real_,
      omega = omega_d,
      stringsAsFactors = FALSE
    )

    items_d <- mapping[mapping$dimension == d, ]
    for (j in seq_len(k_dim)) {
      item_filas[[length(item_filas)+1]] <- data.frame(
        codigo = items_d$codigo[j],
        dimension = .codigo_corto_dim(d),
        dimension_nombre = d,
        texto = items_d$item[j],
        carga_convergente = cargas[j],
        congruencia_tucker = NA_real_,
        itc_corregida = itc[j],
        reliability_if_deleted = rel_if_del[j],
        problematico = unname(problematicos[j]),
        razon_problema = motivos[j],
        stringsAsFactors = FALSE
      )
    }

    if (verbose) {
      cat(sprintf("  [%s] k=%d  alpha=%.3f  omega=%.3f  problematicos=%d\n",
                  d, k_dim,
                  ifelse(is.na(alpha_d), NA_real_, alpha_d),
                  ifelse(is.na(omega_d), NA_real_, omega_d),
                  sum(problematicos, na.rm = TRUE)))
    }
  }

  dims_df <- do.call(rbind, dim_filas)
  items_df <- do.call(rbind, item_filas)

  # ---- Empaquetar como psicometria_extraida ----
  resultado <- list(
    escala = list(
      nombre = escala_nombre,
      autores = "calculo propio",
      ano = as.integer(format(Sys.Date(), "%Y")),
      n_participantes = n_sujetos,
      idioma = idioma,
      poblacion = poblacion
    ),
    constructo = list(
      nombre = constructo,
      definicion = definicion_constructo
    ),
    dimensiones = dims_df,
    items = items_df[, c("codigo","dimension","texto",
                          "carga_convergente","congruencia_tucker",
                          "problematico","razon_problema")],
    cor_latentes = NULL,
    cor_observadas = NULL,
    meta = list(
      metodo_estimacion = "EFA-minres + alpha + omega + ITC",
      software = "R (psych)",
      criterio_problematico = sprintf("carga<%.2f O itc<%.2f",
                                       umbral_carga_problematico,
                                       umbral_itc_problematico),
      tipo_metrica_preferida = tipo_metrica,
      fuente_datos = "datos crudos"
    ),
    df_items_full = items_df,
    raw_json = NULL
  )
  class(resultado) <- c("psicometria_extraida", "list")
  resultado
}


#' @keywords internal
.codigo_corto_dim <- function(d) {
  # Si hay paréntesis con sigla, extraerla: "Foo (XYZ)" -> "XYZ"
  m <- regmatches(d, regexpr("\\(([A-Z]{2,6})\\)", d))
  if (length(m) > 0 && nzchar(m)) return(gsub("[()]", "", m))
  # Si no, primer trigrama en mayusculas
  paste(toupper(substr(gsub("[^A-Za-z]", "", d), 1, 4)), collapse = "")
}
