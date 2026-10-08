# =============================================================================
# EFA SEMANTICO - ARCHIVO DE RESPALDO
# =============================================================================
#
# Este archivo contiene las funciones de Analisis Factorial Exploratorio (EFA)
# aplicado a embeddings semanticos. Estas funciones fueron removidas del flujo
# principal de SeMiLLa en favor del analisis de cluster, pero se conservan
# aqui por si se necesitan en el futuro.
#
# NOTA: Este archivo NO se carga automaticamente con el paquete.
# Para usar estas funciones, cargar manualmente con: source("efa_semantico_archivo.R")
#
# Fecha de archivo: 2024-12-23
# Razon: El analisis de cluster es mas apropiado para validacion semantica
#        que el EFA tradicional aplicado a embeddings.
# =============================================================================


#' @title EFA con Embeddings Semanticos (ARCHIVADO)
#'
#' @description
#' Realiza Analisis Factorial Exploratorio (EFA) usando la matriz de
#' similitud coseno derivada de los embeddings semanticos.
#'
#' NOTA: Esta funcion ha sido archivada. El flujo principal de SeMiLLa
#' ahora usa analisis de cluster (precision_clasificacion) en su lugar.
#'
#' @param embeddings Objeto semilla_embeddings o semilla
#' @param n_factores Numero de factores (NULL = parallel analysis)
#' @param rotacion Rotacion: "oblimin", "varimax", "promax", "none"
#' @param metodo Metodo: "minres", "ml", "pa", "wls"
#' @param verbose Mostrar progreso
#'
#' @return Objeto de clase 'semilla_efa' con resultados del EFA
#'
#' @details
#' El EFA se realiza sobre la matriz de similitud coseno entre embeddings,
#' que actua como matriz de correlacion. El numero de factores se determina
#' mediante Parallel Analysis (Horn, 1965) si no se especifica.
#'
#' LIMITACION: El EFA fue disenado para matrices de covarianza de respuestas
#' humanas, no para representaciones semanticas. El analisis de cluster es
#' conceptualmente mas apropiado para embeddings.
#'
#' @examples
#' \dontrun{
#' # Cargar funciones archivadas
#' source("R/efa_semantico_archivo.R")
#'
#' # EFA automatico
#' efa <- efa_embeddings(embeddings)
#'
#' # EFA con 5 factores
#' efa <- efa_embeddings(embeddings, n_factores = 5)
#'
#' # Ver asignacion
#' efa$asignacion
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement.
#' Nature Human Behaviour, 9(5), 944-954.
#'
#' Horn, J. L. (1965). A rationale and test for the number of factors in
#' factor analysis. Psychometrika, 30(2), 179-185.
#' @noRd
efa_embeddings <- function(embeddings,
                           n_factores = NULL,
                           rotacion = "oblimin",
                           metodo = "minres",
                           verbose = TRUE) {

  # Verificar psych
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Necesitas instalar psych: install.packages('psych')")
  }

  # Extraer datos segun tipo
  if (inherits(embeddings, "semilla")) {
    matriz_cor <- embeddings$similitud
    items_df <- embeddings$items
  } else if (inherits(embeddings, "semilla_embeddings")) {
    matriz_cor <- embeddings$similitud
    items_df <- embeddings$items
  } else {
    stop("Objeto no valido. Usa un objeto semilla o semilla_embeddings")
  }

  n_items <- nrow(matriz_cor)

  if (verbose) {
    cat("  [EFA Semantico - Funcion Archivada]\n")
    cat("  -> Preparando matriz de correlacion...\n")
  }

  # Nombrar matriz
  rownames(matriz_cor) <- paste0("item_", 1:n_items)
  colnames(matriz_cor) <- paste0("item_", 1:n_items)

  # Determinar numero de factores
  if (is.null(n_factores)) {
    if (verbose) cat("  -> Ejecutando Parallel Analysis...\n")

    pa <- psych::fa.parallel(matriz_cor, n.obs = 500, fa = "fa", fm = metodo,
                              plot = FALSE)
    n_factores <- pa$nfact

    if (verbose) cat("    Factores sugeridos: ", n_factores, "\n", sep = "")
  }

  # Validar
  if (n_factores < 1) n_factores <- 1
  if (n_factores > floor(n_items / 3)) {
    n_factores <- floor(n_items / 3)
  }

  # Ejecutar EFA
  if (verbose) {
    cat("  -> Ejecutando EFA (", n_factores, " factores, ",
        rotacion, ")...\n", sep = "")
  }

  fa_result <- tryCatch({
    psych::fa(matriz_cor, nfactors = n_factores, rotate = rotacion,
              fm = metodo, n.obs = 500)
  }, error = function(e) {
    stop("Error en EFA: ", e$message)
  })

  # Procesar resultados
  if (verbose) cat("  -> Procesando resultados...\n")

  # Cargas factoriales
  cargas <- as.data.frame(unclass(fa_result$loadings))
  cargas$item_num <- 1:n_items
  cargas$item_texto <- items_df$item
  cargas$factor_original <- items_df$dimension

  # Factor dominante
  cargas_solo <- cargas[, 1:n_factores, drop = FALSE]
  cargas$factor_efa <- apply(abs(cargas_solo), 1, which.max)
  cargas$carga_max <- apply(abs(cargas_solo), 1, max)
  cargas$comunalidad <- fa_result$communality

  # Ordenar
  cargas <- cargas[order(cargas$factor_efa, -cargas$carga_max), ]

  # Asignacion simple
  asignacion <- data.frame(
    item_num = cargas$item_num,
    factor_original = cargas$factor_original,
    item = cargas$item_texto,
    factor_EFA = paste0("F", cargas$factor_efa),
    carga = round(cargas$carga_max, 3),
    comunalidad = round(cargas$comunalidad, 3),
    stringsAsFactors = FALSE
  )
  rownames(asignacion) <- NULL

  # Varianza
  varianza <- data.frame(
    Factor = paste0("F", 1:n_factores),
    SS_Loading = fa_result$Vaccounted[1, ],
    Prop_Var = fa_result$Vaccounted[2, ],
    Cum_Var = fa_result$Vaccounted[3, ]
  )

  # Correlacion entre factores
  if (rotacion %in% c("oblimin", "promax", "oblique")) {
    cor_factores <- fa_result$Phi
  } else {
    cor_factores <- diag(n_factores)
  }

  # Resultado
  resultado <- list(
    fa = fa_result,
    cargas = cargas,
    asignacion = asignacion,
    varianza = varianza,
    correlacion_factores = cor_factores,
    matriz_cor = matriz_cor,
    metadata = list(
      n_items = n_items,
      n_factores = n_factores,
      rotacion = rotacion,
      metodo = metodo
    )
  )

  class(resultado) <- c("semilla_efa", "list")

  if (verbose) {
    cat("  [OK] EFA completado\n")
    cat("    Varianza explicada: ", round(sum(varianza$Prop_Var) * 100, 1), "%\n", sep = "")
  }

  return(resultado)
}


#' @title Comparar Estructura Teorica vs EFA (ARCHIVADO)
#'
#' @description
#' Compara la asignacion de items entre la estructura teorica y el EFA.
#'
#' NOTA: Esta funcion ha sido archivada junto con efa_embeddings().
#'
#' @param efa Objeto semilla_efa o semilla
#' @param verbose Mostrar resultados
#'
#' @return Dataframe con concordancia
#' @noRd
comparar_estructura <- function(efa, verbose = TRUE) {

  # Extraer EFA segun tipo
  if (inherits(efa, "semilla")) {
    if (is.null(efa$efa)) {
      stop("El objeto semilla no contiene EFA. Usa efa_embeddings() primero.")
    }
    asig <- efa$efa$asignacion
  } else if (inherits(efa, "semilla_efa")) {
    asig <- efa$asignacion
  } else {
    stop("Objeto no valido")
  }

  # Tabla de contingencia
  tabla <- table(asig$factor_original, asig$factor_EFA)

  # Concordancia por factor
  factores_originales <- unique(asig$factor_original)
  concordancia <- data.frame()

  for (fo in factores_originales) {
    items_fo <- asig[asig$factor_original == fo, ]
    factor_efa_dominante <- names(which.max(table(items_fo$factor_EFA)))
    n_concordantes <- sum(items_fo$factor_EFA == factor_efa_dominante)
    n_total <- nrow(items_fo)

    concordancia <- rbind(concordancia, data.frame(
      factor_original = fo,
      factor_EFA = factor_efa_dominante,
      items = n_total,
      concordantes = n_concordantes,
      porcentaje = round(n_concordantes / n_total * 100, 1),
      stringsAsFactors = FALSE
    ))
  }

  if (verbose) {
    cat("\n[COMPARACION: ESTRUCTURA TEORICA vs EFA]\n")
    cat(paste(rep("-", 50), collapse = ""), "\n\n")

    cat("Tabla de contingencia:\n")
    print(tabla)
    cat("\n")

    cat("Concordancia por factor:\n")
    for (i in 1:nrow(concordancia)) {
      cat(sprintf("  %s -> %s: %d/%d (%.1f%%)\n",
                  concordancia$factor_original[i],
                  concordancia$factor_EFA[i],
                  concordancia$concordantes[i],
                  concordancia$items[i],
                  concordancia$porcentaje[i]))
    }

    cat("\n  CONCORDANCIA GLOBAL: ",
        round(mean(concordancia$porcentaje), 1), "%\n\n", sep = "")
  }

  invisible(concordancia)
}


#' @title Imprimir EFA (ARCHIVADO)
#' @param x Objeto semilla_efa
#' @param ... Argumentos adicionales
print.semilla_efa <- function(x, ...) {

  cat("\n[ANALISIS FACTORIAL EXPLORATORIO - Archivado]\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Factores: ", x$metadata$n_factores, "\n", sep = "")
  cat("Rotacion: ", x$metadata$rotacion, "\n", sep = "")
  cat("Metodo: ", x$metadata$metodo, "\n\n", sep = "")

  cat("VARIANZA EXPLICADA:\n")
  for (i in 1:x$metadata$n_factores) {
    cat(sprintf("  F%d: %.1f%% (acum: %.1f%%)\n",
                i,
                x$varianza$Prop_Var[i] * 100,
                x$varianza$Cum_Var[i] * 100))
  }
  cat("\n")

  cat("ASIGNACION DE ITEMS:\n")
  for (f in 1:x$metadata$n_factores) {
    items_f <- x$asignacion[x$asignacion$factor_EFA == paste0("F", f), ]
    cat("\n  Factor ", f, " (", nrow(items_f), " items):\n", sep = "")
    for (j in 1:min(3, nrow(items_f))) {
      cat(sprintf("    #%d (%.2f): %s\n",
                  items_f$item_num[j],
                  items_f$carga[j],
                  substr(items_f$item[j], 1, 50)))
    }
    if (nrow(items_f) > 3) {
      cat("    ... y ", nrow(items_f) - 3, " mas\n", sep = "")
    }
  }
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  invisible(x)
}


# =============================================================================
# FIN DEL ARCHIVO DE RESPALDO
# =============================================================================
cat("EFA Semantico cargado (funciones archivadas)\n")
cat("Funciones disponibles: efa_embeddings(), comparar_estructura()\n")
