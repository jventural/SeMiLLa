#' Exportar el proyecto completo de una escala (carpeta resultados/)
#'
#' Genera, en un solo llamado, toda la estructura de salida de un proyecto
#' SeMiLLa: tablas numeradas (01..13) en Excel, la subcarpeta \code{graficos/}
#' con las figuras del analisis (incluidas las figuras comparativas EFA-cargas y
#' Sankey \emph{sin refinar} vs \emph{refinado}), la escala final exportada, el
#' test administrable en \code{test_aplicacion/} y un \code{00_RESUMEN.txt}.
#' Reproduce la misma coleccion de archivos que el pipeline por lotes, pero a
#' partir de los objetos ya calculados en la sesion (no recalcula ni vuelve a
#' llamar al LLM). Es la funcion que usa el boton "Exportar proyecto completo"
#' de la SeMiLLa App, de modo que la salida desde R y desde la App es identica.
#'
#' @param escala Objeto \code{semilla} final (con items y, idealmente, embeddings/similitud).
#' @param dir Carpeta de salida (se crea junto con \code{graficos/} y \code{test_aplicacion/}).
#' @param abreviatura Prefijo corto para la escala final y el formulario (p. ej. "PM").
#' @param efa Resultado de \code{\link{efa_regularizado}} (tabla 03).
#' @param ensemble Resultado de \code{\link{precision_clasificacion}} (tabla 04 + graficos 08-09).
#' @param refinamiento Resultado de \code{\link{refinar_escala}} (tabla 05).
#' @param cv Resultado de \code{\link{validez_contenido}} (tabla 06 + grafico 03).
#' @param calidad Resultado de \code{\link{auditar_redaccion_items}} (tabla 07).
#' @param fiabilidad Resultado de \code{\link{fiabilidad_semantica}} (tabla 13 + grafico 04).
#' @param omega Resultado de \code{\link{omega_semantico}} (columna omega de la tabla 13).
#' @param discriminacion Resultado de \code{\link{discriminacion_semantica}} (tabla 08 + grafico 05).
#' @param coherencia Resultado de \code{\link{analizar_coherencia}} (graficos 06-07).
#' @param criterio Resultado de \code{\link{validez_criterio_predicha}} (tabla 09).
#' @param forma_corta Resultado de \code{\link{forma_corta}} (tabla 10).
#' @param escala_respuesta Resultado de \code{\link{sugerir_escala_respuesta}} (test administrable).
#' @param escala_sin_refinar Objeto \code{semilla} ANTES de refinar; habilita los
#'   graficos comparativos 10 (EFA sin refinar) y 12 (Sankey sin refinar).
#' @param adaptacion Resultado de \code{\link{adaptar_transcultural}} (tabla 11).
#' @param dif Resultado de \code{\link{detectar_dif_semantico}} (tabla 12).
#' @param k Numero de factores para las figuras EFA-cargas (por defecto, el numero
#'   de dimensiones de la escala).
#' @param nombre_test,autor Metadatos del test ensamblado.
#' @param verbose Mensajes de progreso.
#'
#' @return (Invisible) data.frame con los archivos generados y su estado.
#' @export
exportar_proyecto <- function(escala, dir, abreviatura = "TEST",
                              efa = NULL, ensemble = NULL, refinamiento = NULL,
                              cv = NULL, calidad = NULL, fiabilidad = NULL,
                              omega = NULL, discriminacion = NULL, coherencia = NULL,
                              criterio = NULL, forma_corta = NULL,
                              escala_respuesta = NULL, escala_sin_refinar = NULL,
                              adaptacion = NULL, dif = NULL, k = NULL,
                              nombre_test = NULL, autor = "SeMiLLa", verbose = TRUE) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  if (is.null(escala) || is.null(escala$items))
    stop("exportar_proyecto(): 'escala' debe ser un objeto semilla con $items.")

  RES <- dir
  GR  <- file.path(RES, "graficos")
  TST <- file.path(RES, "test_aplicacion")
  for (d in c(RES, GR, TST)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  msg <- function(...) if (isTRUE(verbose)) message(...)
  generados <- list()
  reg <- function(archivo, ok) generados[[length(generados) + 1]] <<-
    data.frame(archivo = archivo, ok = ok, stringsAsFactors = FALSE)
  wx <- function(x, nombre) {
    f <- file.path(RES, nombre)
    ok <- tryCatch({ openxlsx::write.xlsx(x, f); TRUE },
                   error = function(e) { msg("   [x] ", nombre, ": ", e$message); FALSE })
    reg(nombre, ok)
  }
  gp <- function(p, nombre, w = 10, h = 8) {
    f <- file.path(GR, nombre)
    ok <- tryCatch({ ggplot2::ggsave(f, p, width = w, height = h, dpi = 200, bg = "white"); TRUE },
                   error = function(e) { msg("   [x] graficos/", nombre, ": ", e$message); FALSE })
    reg(file.path("graficos", nombre), ok)
  }

  efa      <- efa %||% escala$efa
  ensemble <- ensemble %||% escala$efa
  k        <- k %||% length(unique(escala$items$dimension))

  msg(">> Exportando tablas...")
  wx(escala$items, "01_items_generados.xlsx")
  if (!is.null(escala$similitud))
    wx(as.data.frame(round(escala$similitud, 3)), "02_matriz_similitud.xlsx")
  if (!is.null(efa) && !is.null(efa$cargas))
    wx(list(cargas = as.data.frame(efa$cargas),
            asignacion = data.frame(Item = rownames(efa$cargas),
                                    Factor = efa$asignacion %||% NA_character_,
                                    stringsAsFactors = FALSE)),
       "03_efa_regularizado.xlsx")
  if (!is.null(ensemble) && !is.null(ensemble$consenso))
    wx(ensemble$consenso, "04_consenso_ensemble.xlsx")
  if (!is.null(refinamiento) && !is.null(refinamiento$historial))
    wx(refinamiento$historial, "05_historial_refinamiento.xlsx")
  if (!is.null(cv) && !is.null(cv$v_aiken))
    wx(cv$v_aiken, "06_v_aiken.xlsx")
  if (!is.null(calidad))
    wx(calidad$evaluacion %||% calidad, "07_auditoria_redaccion.xlsx")
  if (!is.null(discriminacion))
    wx(as.data.frame(discriminacion), "08_discriminacion.xlsx")
  if (!is.null(criterio))
    wx(utils::head(criterio$items_clave %||% as.data.frame(criterio), 20), "09_validez_criterio.xlsx")
  if (!is.null(forma_corta) && !is.null(forma_corta$items))
    wx(forma_corta$items, sprintf("10_forma_corta_%d.xlsx", nrow(forma_corta$items)))
  # 11 adaptacion transcultural ES -> PT
  if (!is.null(adaptacion))
    wx(list(items_origen  = adaptacion$escala_origen$items  %||% data.frame(),
            items_destino = adaptacion$escala_destino$items %||% data.frame(),
            equivalencia  = adaptacion$equivalencia         %||% data.frame(),
            items_problem = adaptacion$items_problematicos  %||% data.frame()),
       "11_adaptacion_es_pt.xlsx")
  # 12 DIF semantico ES vs PT
  if (!is.null(dif))
    wx(list(distancias = dif$distancias %||% data.frame(),
            items_dif  = dif$items_dif  %||% data.frame()),
       "12_dif_semantico_es_pt.xlsx")
  # 13 fiabilidad por factor (alfa + omega), nunca total
  if (!is.null(fiabilidad)) {
    fac <- unique(escala$items$dimension)
    ad  <- fiabilidad$alpha_dimensiones
    fib <- data.frame(
      Factor = fac,
      n_items = vapply(fac, function(d) sum(escala$items$dimension == d), integer(1)),
      alpha_semantico = if (!is.null(ad)) round(ad$alpha_semantico[match(fac, ad$dimension)], 3) else NA_real_,
      omega_semantico = if (!is.null(omega)) round(omega$omega_semantico[match(fac, omega$dimension)], 3) else NA_real_,
      stringsAsFactors = FALSE)
    wx(fib, "13_fiabilidad_por_factor.xlsx")
  } else fib <- NULL

  msg(">> Exportando graficos...")
  try(gp(plot_similitud(escala, ordenar_por = "dimension"), "01_heatmap_similitud.png"), silent = TRUE)
  try(gp(plot_embeddings(escala, metodo = "tsne", colorear_por = "dimension"), "02_tsne_embeddings.png"), silent = TRUE)
  if (!is.null(cv))             try(gp(plot_v_aiken(cv), "03_v_aiken.png", 10, 10), silent = TRUE)
  if (!is.null(fiabilidad))     try(gp(plot_fiabilidad(fiabilidad), "04_fiabilidad.png", 10, 6), silent = TRUE)
  if (!is.null(discriminacion)) try(gp(plot_discriminacion(discriminacion), "05_discriminacion.png"), silent = TRUE)
  if (!is.null(coherencia)) {
    try(gp(plot_coherencia(coherencia, tipo = "boxplot"), "06_coherencia_boxplot.png", 10, 6), silent = TRUE)
    try(gp(plot_coherencia(coherencia, tipo = "violin"),  "07_coherencia_violin.png",  10, 6), silent = TRUE)
  }
  if (!is.null(ensemble)) {
    try(gp(plot_precision(ensemble), "08_precision.png", 10, 6), silent = TRUE)
    try(gp(plot_sankey(ensemble),    "09_sankey_dimensiones.png", 12, 6), silent = TRUE)
  }
  # ---- Figuras comparativas sin refinar vs refinado (10-13) ----
  if (!is.null(escala_sin_refinar)) {
    .fig_efa_cargas(escala_sin_refinar, k, file.path(GR, "10_efa_cargas_sinrefinar.png"),
                    paste0(abreviatura, " (sin refinar)"), gp_reg = reg, msg = msg)
    .fig_sankey_de(escala_sin_refinar, file.path(GR, "12_sankey_sinrefinar.png"),
                   paste0(abreviatura, " (sin refinar)"), gp_reg = reg, msg = msg)
  }
  .fig_efa_cargas(escala, k, file.path(GR, "11_efa_cargas_refinado.png"),
                  paste0(abreviatura, " (refinado)"), gp_reg = reg, msg = msg)
  .fig_sankey_de(escala, file.path(GR, "13_sankey_refinado.png"),
                 paste0(abreviatura, " (refinado)"), gp_reg = reg, msg = msg)

  msg(">> Exportando escala final...")
  base_final <- file.path(RES, paste0(abreviatura, "_escala_final"))
  ok_ef <- tryCatch({ exportar_escala(escala, archivo = base_final, incluir_info = TRUE)
                      guardar(escala, base_final); TRUE },
                    error = function(e) { msg("   [x] escala final: ", e$message); FALSE })
  reg(paste0(abreviatura, "_escala_final.{xlsx,txt,rds}"), ok_ef)

  if (!is.null(escala_respuesta)) {
    msg(">> Ensamblando test administrable...")
    ok_t <- tryCatch({
      ensamblar(tipo = "likert", escala = escala, escala_respuesta = escala_respuesta,
                forma = "ambas", forma_corta_obj = forma_corta,
                nombre_test = nombre_test %||% paste0("Escala ", abreviatura),
                autor = autor, archivo = file.path(TST, paste0(abreviatura, "_formulario")),
                formato = c("md", "docx", "html"), idioma = "es", verbose = FALSE); TRUE
    }, error = function(e) { msg("   [x] ensamblar: ", e$message); FALSE })
    reg(file.path("test_aplicacion", paste0(abreviatura, "_formulario.*")), ok_t)
  }

  L <- c(strrep("=", 60),
         paste0("  ", toupper(nombre_test %||% (escala$concepto$nombre %||% abreviatura)),
                " - RESUMEN DE CONSTRUCCION"),
         strrep("=", 60),
         paste0("  Items finales : ", nrow(escala$items)),
         paste0("  Dimensiones   : ", length(unique(escala$items$dimension))))
  if (!is.null(fib))
    L <- c(L, "  Fiabilidad por factor (alfa | omega):",
           paste(sprintf("    - %-26s alfa=%s | omega=%s", fib$Factor,
                         formatC(fib$alpha_semantico, format = "f", digits = 3),
                         formatC(fib$omega_semantico, format = "f", digits = 3)), collapse = "\n"))
  if (!is.null(cv) && !is.null(cv$v_aiken_escala))
    L <- c(L, paste0("  V de Aiken total : ", sprintf("%.3f", cv$v_aiken_escala$V_total)))
  if (!is.null(ensemble) && !is.null(ensemble$precision_global))
    L <- c(L, paste0("  Precision ensemble : ", sprintf("%.1f%%", ensemble$precision_global)))
  if (!is.null(refinamiento))
    L <- c(L, paste0("  Refinamiento : ", sprintf("%.1f%% -> %.1f%%",
                     refinamiento$precision_inicial, refinamiento$precision_final)))
  L <- c(L, strrep("=", 60))
  writeLines(L, file.path(RES, "00_RESUMEN.txt"))
  reg("00_RESUMEN.txt", TRUE)

  out <- do.call(rbind, generados)
  msg(">> LISTO. ", sum(out$ok), "/", nrow(out), " archivos en: ", RES)
  invisible(out)
}

# --- helpers internos (no exportados) -------------------------------------

.alinear_dims <- function(asign, teor, factores) {
  dims <- unique(teor)
  tab <- table(factor = factor(asign, levels = factores), dim = factor(teor, levels = dims))
  mp <- stats::setNames(rep(NA_character_, length(factores)), factores)
  tm <- matrix(as.numeric(tab), nrow = nrow(tab), dimnames = list(rownames(tab), colnames(tab)))
  for (s in seq_len(min(length(factores), length(dims)))) {
    if (all(tm < 0)) break
    idx <- which(tm == max(tm), arr.ind = TRUE)[1, ]
    if (tm[idx[1], idx[2]] <= 0) break
    mp[rownames(tm)[idx[1]]] <- colnames(tm)[idx[2]]; tm[idx[1], ] <- -1; tm[, idx[2]] <- -1
  }
  mp
}

.fig_efa_cargas <- function(esc, k, file, etiqueta, gp_reg = NULL, msg = message) {
  ok <- tryCatch({
    df <- esc$items
    if (is.null(df$numero)) df$numero <- seq_len(nrow(df))
    if (is.null(df$codigo)) df$codigo <- paste0("I", df$numero)
    efa <- efa_regularizado(esc, n_factores = k, centrado = "double", ejecutar = TRUE, verbose = FALSE)
    L <- as.matrix(efa$cargas)
    if (ncol(L) < 1) stop("cargas vacias")
    if (is.null(colnames(L))) colnames(L) <- paste0("F", seq_len(ncol(L)))
    rownames(L) <- df$codigo
    factores <- colnames(L); asign <- efa$asignacion; teor <- df$dimension
    mp <- .alinear_dims(asign, teor, factores)
    short <- function(s, n = 22) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
    col_lab <- stats::setNames(vapply(factores, function(f) {
      d <- mp[f]; if (is.na(d)) paste0(f, "\n(—)") else paste0(f, "\n", short(d)) }, character(1)), factores)
    long <- expand.grid(codigo = df$codigo, factor = factores, stringsAsFactors = FALSE)
    long$carga <- as.numeric(L[cbind(match(long$codigo, df$codigo), match(long$factor, factores))])
    long$dim_teorica <- df$dimension[match(long$codigo, df$codigo)]
    long$dom <- mapply(function(c, f) !is.na(asign[match(c, df$codigo)]) && asign[match(c, df$codigo)] == f,
                       long$codigo, long$factor)
    ord <- df$codigo[order(match(df$dimension, unique(df$dimension)), df$numero)]
    long$codigo <- factor(long$codigo, levels = rev(ord))
    long$dim_teorica <- factor(long$dim_teorica, levels = unique(df$dimension))
    long$factor <- factor(long$factor, levels = factores)
    p <- ggplot2::ggplot(long, ggplot2::aes(.data$factor, .data$codigo, fill = .data$carga)) +
      ggplot2::geom_tile(color = "grey90") +
      ggplot2::geom_tile(data = subset(long, long$dom), color = "black", linewidth = 0.8, fill = NA) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(abs(.data$carga) >= 0.10, sprintf("%.2f", .data$carga), "")), size = 2.4) +
      ggplot2::scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2E5D33", midpoint = 0) +
      ggplot2::scale_x_discrete(labels = col_lab) +
      ggplot2::facet_grid(dim_teorica ~ ., scales = "free_y", space = "free_y", switch = "y") +
      ggplot2::labs(title = paste0("EFA regularizado — ", etiqueta), x = "Factor empirico", y = "Item", fill = "Carga") +
      ggplot2::theme_minimal(base_size = 9) +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 7),
                     strip.placement = "outside", panel.spacing = ggplot2::unit(2, "pt"),
                     axis.text.x = ggplot2::element_text(size = 7), plot.title = ggplot2::element_text(face = "bold"))
    ggplot2::ggsave(file, p, width = max(7, 1.0 * length(factores) + 3),
                    height = max(6, 0.32 * nrow(df) + 2), dpi = 200, bg = "white")
    TRUE
  }, error = function(e) { msg("   [x] ", basename(file), ": ", e$message); FALSE })
  if (!is.null(gp_reg)) gp_reg(file.path("graficos", basename(file)), ok)
  invisible(ok)
}

.fig_sankey_de <- function(esc, file, etiqueta, gp_reg = NULL, msg = message) {
  ok <- tryCatch({
    pr <- precision_clasificacion(esc, metodo = "ensemble", algoritmos = c("kmeans", "ward"), verbose = FALSE)
    p <- plot_sankey(pr, titulo = paste0("Flujo item -> cluster (", etiqueta, ")"))
    ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 200, bg = "white"); TRUE
  }, error = function(e) { msg("   [x] ", basename(file), ": ", e$message); FALSE })
  if (!is.null(gp_reg)) gp_reg(file.path("graficos", basename(file)), ok)
  invisible(ok)
}
