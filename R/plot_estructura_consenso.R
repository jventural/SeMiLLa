# =============================================================================
#  El grafico de los DOS MOMENTOS de estructura_por_consenso()
# -----------------------------------------------------------------------------
#  Al fusionar el paso 6 y el 7, quien usa la app perderia de vista lo que antes
#  veia en dos pantallas: como salio la estructura y como quedo tras el
#  refinamiento. Este grafico lo devuelve en una sola figura, con la paleta de
#  plots.R:
#
#   Panel A - los INDICES, antes -> despues, con su umbral marcado.
#   Panel B - el CONSENSO ITEM A ITEM, antes -> despues, agrupado por dimension;
#             flecha gris = el item no se toco, flecha naranja = se reescribio.
#
#  Todo en escala 0-1 en el panel A para que los cuatro indices sean legibles en
#  el mismo eje (la precision va dividida entre 100).
#
#  OJO con el nombre: plot_estructura() ya existe en plots.R y es otra cosa (el
#  diagrama de la estructura teorica). Esta se llama plot_estructura_consenso()
#  por eso; con el nombre corto la habria pisado segun el orden de colacion, sin
#  ningun aviso.
# =============================================================================

.COL_ANTES   <- "#B0392E"   # rojo: el momento en que fallaba
.COL_DESPUES <- "#2E7D52"   # verde: el momento en que cumple
.COL_CAMBIO  <- "#C9863A"   # naranja: item reescrito

# -----------------------------------------------------------------------------
#  Panel A - indices
# -----------------------------------------------------------------------------
plot_indices_dos_momentos <- function(x, titulo = "Los indices, antes y despues") {
  g0 <- x$gate_antes
  g1 <- x$gate_despues %||% g0

  d <- data.frame(
    indice  = g0$indice,
    clave   = g0$clave,
    antes   = g0$valor,
    despues = g1$valor,
    crudo0  = g0$crudo,
    crudo1  = g1$crudo,
    umbral  = g0$umbral,
    bloquea = g0$bloquea,
    stringsAsFactors = FALSE
  )
  # La silhouette se queda fuera del panel: vive en otra escala (~0.07) y
  # aplastaria los demas. Se informa en el subtitulo.
  sil <- d[d$clave == "silhouette", ]
  d   <- d[d$clave != "silhouette", ]
  d$indice <- factor(d$indice, levels = rev(d$indice))
  d$mejora <- d$despues - d$antes

  sub <- sprintf("Silhouette (informativa, no bloquea): %s -> %s",
                 sil$crudo0, sil$crudo1)

  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$indice)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$antes, xend = .data$despues,
                                       yend = .data$indice),
                          color = "grey65", linewidth = 1.6,
                          arrow = grid::arrow(length = grid::unit(7, "pt"),
                                              type = "closed")) +
    ggplot2::geom_point(ggplot2::aes(x = .data$antes), color = .COL_ANTES, size = 4.2) +
    ggplot2::geom_point(ggplot2::aes(x = .data$despues), color = .COL_DESPUES, size = 4.2) +
    ggplot2::geom_point(ggplot2::aes(x = .data$umbral), shape = 124, size = 6,
                        color = "grey25", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = .data$antes, label = .data$crudo0),
                       vjust = 2.1, size = 3.1, color = .COL_ANTES) +
    ggplot2::geom_text(ggplot2::aes(x = .data$despues, label = .data$crudo1),
                       vjust = -1.5, size = 3.1, color = .COL_DESPUES,
                       fontface = "bold") +
    ggplot2::scale_x_continuous(limits = c(0, 1.06), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(title = titulo, subtitle = sub,
                  x = "Valor del indice (0-1; la barra vertical es el umbral exigido)",
                  y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
      axis.text.y   = ggplot2::element_text(face = "bold", size = 9.5),
      panel.grid.major.y = ggplot2::element_blank())
  p
}

# -----------------------------------------------------------------------------
#  Panel B - consenso item a item
# -----------------------------------------------------------------------------
plot_consenso_dos_momentos <- function(x, titulo = "El consenso de cada item, antes y despues") {
  u  <- x$parametros$umbral_consenso
  a  <- x$consenso_antes
  b  <- x$consenso_despues %||% a
  d  <- merge(a[, c("numero", "dimension", "consenso")],
              b[, c("numero", "consenso")], by = "numero",
              suffixes = c("_antes", "_despues"))
  d$cambiado <- d$numero %in% x$cambiados
  d$etiqueta <- paste0("I", d$numero)

  dims <- unique(a$dimension)
  d$dimension <- factor(d$dimension, levels = dims)
  d <- d[order(as.integer(d$dimension), d$numero), ]
  d$etiqueta <- factor(d$etiqueta, levels = rev(d$etiqueta))

  n_bajo0 <- sum(d$consenso_antes  < u, na.rm = TRUE)
  n_bajo1 <- sum(d$consenso_despues < u, na.rm = TRUE)
  sub <- sprintf("Por debajo del umbral (%.3f): %d item(s) antes -> %d despues   |   naranja = item reescrito",
                 u, n_bajo0, n_bajo1)

  ggplot2::ggplot(d, ggplot2::aes(y = .data$etiqueta)) +
    ggplot2::geom_vline(xintercept = u, linetype = "dashed",
                        color = "grey40", linewidth = 0.7) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$consenso_antes,
                                       xend = .data$consenso_despues,
                                       yend = .data$etiqueta,
                                       color = .data$cambiado),
                          linewidth = 1.3,
                          arrow = grid::arrow(length = grid::unit(6, "pt"),
                                              type = "closed")) +
    ggplot2::geom_point(ggplot2::aes(x = .data$consenso_antes),
                        color = .COL_ANTES, size = 2.6) +
    ggplot2::geom_point(ggplot2::aes(x = .data$consenso_despues),
                        color = .COL_DESPUES, size = 2.6) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "grey72", `TRUE` = .COL_CAMBIO),
                                labels = c(`FALSE` = "se conservo",
                                           `TRUE`  = "reescrito por el refinamiento"),
                                name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 1.02), breaks = seq(0, 1, 0.2)) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$dimension),
                        scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = titulo, subtitle = sub,
                  x = "Grado de consenso del ensemble", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 7.5),
      strip.placement   = "outside",
      panel.spacing     = ggplot2::unit(4, "pt"),
      legend.position   = "bottom",
      panel.grid.major.y = ggplot2::element_blank())
}

# -----------------------------------------------------------------------------
#  Figura completa (A + B)
# -----------------------------------------------------------------------------
plot_estructura_consenso <- function(x, titulo = NULL) {
  # patchwork esta en Suggests: sin el se devuelven los dos paneles en una lista
  # en vez de fallar (quien llame dibuja el que necesite).
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Instala 'patchwork' para componer la figura de dos momentos. ",
            "Se devuelven los dos paneles por separado.")
    return(list(indices = plot_indices_dos_momentos(x),
                consenso = plot_consenso_dos_momentos(x)))
  }
  cab <- titulo %||% switch(x$veredicto,
    cumple              = "Estructura por consenso: la escala cumplia de entrada",
    falla_sin_refinar   = "Estructura por consenso: no cumple (refinamiento desactivado)",
    refinado_ok         = "Estructura por consenso: no cumplia -> refinada -> cumple",
    refinado_incompleto = "Estructura por consenso: refinada, aun no cumple")
  # El pie NO menciona el seed: .clusterizar() fija sus propias semillas dentro
  # de precision_clasificacion(), asi que el ensemble es determinista y el seed
  # que se le pase por fuera no cambia nada (verificado en 04_verificar.R).
  # Anunciarlo seria dar por control del usuario algo que no controla.
  pie <- sprintf("Ensemble: %s x %d replicas (determinista) | umbral de consenso %.3f%s",
                 paste(x$parametros$algoritmos, collapse = " + "),
                 x$parametros$n_replicas, x$parametros$umbral_consenso,
                 if (isTRUE(x$refinado))
                   sprintf(" | %d item(s) reescrito(s) en %s",
                           length(x$cambiados),
                           if (!is.null(x$ciclos) && nrow(x$ciclos) > 0)
                             sprintf("%d ciclo(s) refinar-blindar-medir", nrow(x$ciclos))
                           else sprintf("%d iteracion(es)", x$refinamiento$iteraciones))
                 else "")

  pa <- plot_indices_dos_momentos(x)
  pb <- plot_consenso_dos_momentos(x)

  patchwork::wrap_plots(pa, pb, ncol = 1, heights = c(1, 2.1)) +
    patchwork::plot_annotation(
      title = cab, caption = pie,
      theme = ggplot2::theme(
        plot.title   = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
        plot.caption = ggplot2::element_text(color = "grey45", size = 8.5, hjust = 0.5)))
}

# -----------------------------------------------------------------------------
#  El recorrido completo: todas las vueltas y, sobre todo, el punto de despues
#  del blindaje
# -----------------------------------------------------------------------------
#  plot_evolucion_precision() dibuja $refinamiento$evolucion, que describe UN
#  ciclo y se detiene ANTES del blindaje de cierre. Con esa curva el proceso
#  siempre parece una subida limpia: el momento en que el blindaje reescribe un
#  item y tumba la estructura queda fuera del grafico. Esta funcion dibuja
#  $evolucion de estructura_por_consenso(), que encadena los ciclos y marca en
#  rombo la medicion posterior al blindaje -la unica que describe la escala que
#  se entrega-.
plot_evolucion_estructura <- function(x, titulo = NULL) {
  d <- x$evolucion
  if (is.null(d) || nrow(d) < 2)
    stop("No hay evolucion que dibujar: este paso no llego a corregir nada.")

  d$etiqueta <- factor(d$etiqueta, levels = d$etiqueta)
  d$es_clave <- d$tipo == "tras_blindaje"
  minp <- x$parametros$min_precision

  fin <- d[nrow(d), ]
  ini <- d[1, ]
  # El minimo exigido se cuenta AQUI y no en el eje: con la frase completa, la
  # etiqueta del eje Y se salia del lienzo y perdia el parentesis final.
  sub <- sprintf(paste("Empezo en %.1f%% y termino en %.1f%%. Los rombos son la escala YA blindada:",
                       "la que de verdad se entrega.\nLa linea discontinua es el minimo exigido (%.0f%%)."),
                 ini$precision, fin$precision, minp)

  ggplot2::ggplot(d, ggplot2::aes(x = .data$etiqueta, y = .data$precision,
                                  group = 1)) +
    ggplot2::geom_hline(yintercept = minp, linetype = "dashed",
                        color = "grey45", linewidth = 0.7) +
    ggplot2::geom_line(color = "grey55", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$es_clave,
                                     color = .data$es_clave,
                                     size  = .data$es_clave)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", .data$precision)),
                       vjust = -1.1, size = 3, color = "grey25") +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18),
                                labels = c(`FALSE` = "vuelta del refinamiento",
                                           `TRUE`  = "tras blindar (escala entregada)"),
                                name = NULL) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "#3E78B2", `TRUE` = "#B0392E"),
                                labels = c(`FALSE` = "vuelta del refinamiento",
                                           `TRUE`  = "tras blindar (escala entregada)"),
                                name = NULL) +
    ggplot2::scale_size_manual(values = c(`FALSE` = 2.8, `TRUE` = 4.6),
                               guide = "none") +
    ggplot2::labs(
      title = titulo %||% "El recorrido completo de la correccion",
      subtitle = sub,
      x = NULL, y = "Precision de clasificacion (%)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
      axis.text.x   = ggplot2::element_text(angle = 35, hjust = 1, size = 8.5),
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank())
}
