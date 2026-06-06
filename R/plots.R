# =============================================================================
# SeMiLLa - FUNCIONES DE VISUALIZACION
# =============================================================================
# Visualizaciones para analisis psicometrico semantico
# =============================================================================


#' @title Visualizar Matriz de Similitud
#'
#' @description
#' Genera un heatmap de la matriz de similitud coseno entre items,
#' agrupados por dimension teorica.
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param ordenar_por Ordenar por: "dimension" (default), "cluster", o "ninguno"
#' @param mostrar_valores Mostrar valores numericos en celdas
#' @param colores Vector de colores para gradiente (bajo, medio, alto)
#' @param titulo Titulo del grafico
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' # Heatmap basico
#' plot_similitud(mi_escala)
#'
#' # Con valores numericos
#' plot_similitud(mi_escala, mostrar_valores = TRUE)
#' }
#'
#' @export
plot_similitud <- function(x,
                           ordenar_por = "dimension",
                           mostrar_valores = FALSE,
                           colores = c("#2C3E50", "#F39C12", "#E74C3C"),
                           titulo = "Matriz de Similitud Semantica") {

  .verificar_ggplot2()

  # Extraer datos
  datos <- .extraer_datos_plot(x)
  similitud <- datos$similitud
  items_df <- datos$items

  n_items <- nrow(similitud)

  # Ordenar items

  if (ordenar_por == "dimension" && !is.null(items_df$dimension)) {
    orden <- order(items_df$dimension)
  } else if (ordenar_por == "cluster" && !is.null(datos$efa)) {
    orden <- order(datos$efa$asignacion$factor_EFA)
  } else {
    orden <- 1:n_items
  }

  similitud_ord <- similitud[orden, orden]
  dims_ord <- items_df$dimension[orden]

  # Preparar datos para ggplot
  sim_df <- expand.grid(
    Item1 = 1:n_items,
    Item2 = 1:n_items
  )
  sim_df$Similitud <- as.vector(similitud_ord)
  sim_df$Item1 <- factor(sim_df$Item1, levels = n_items:1)
  sim_df$Item2 <- factor(sim_df$Item2, levels = 1:n_items)

  # Crear heatmap
  p <- ggplot2::ggplot(sim_df, ggplot2::aes(x = Item2, y = Item1, fill = Similitud)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradient2(
      low = colores[1],
      mid = colores[2],
      high = colores[3],
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Similitud\nCoseno"
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Items agrupados por ", ordenar_por),
      x = "Item",
      y = "Item"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 6),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40")
    ) +
    ggplot2::coord_fixed()

  # Agregar valores si se solicita
  if (mostrar_valores && n_items <= 25) {
    sim_df$label <- sprintf("%.2f", sim_df$Similitud)
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = label),
      size = 2,
      color = ifelse(sim_df$Similitud > 0.6, "white", "black")
    )
  }

  # Agregar lineas de separacion por dimension
  if (ordenar_por == "dimension" && !is.null(dims_ord)) {
    cambios <- which(dims_ord[-1] != dims_ord[-n_items]) + 0.5
    for (cambio in cambios) {
      p <- p +
        ggplot2::geom_hline(yintercept = n_items - cambio + 1, color = "black", linewidth = 0.5) +
        ggplot2::geom_vline(xintercept = cambio, color = "black", linewidth = 0.5)
    }
  }

  return(p)
}


#' @title Visualizar Embeddings en 2D
#'
#' @description
#' Proyecta los embeddings de alta dimension a 2D usando t-SNE o UMAP,
#' coloreando los items por dimension o factor.
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param metodo Metodo de reduccion: "tsne" (default) o "umap"
#' @param colorear_por Variable para colorear: "dimension" o "factor_efa"
#' @param mostrar_etiquetas Mostrar numero de item
#' @param perplexity Perplexity para t-SNE (default: 5 o n/4)
#' @param semilla Semilla para reproducibilidad
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' # Proyeccion t-SNE
#' plot_embeddings(mi_escala)
#'
#' # Colorear por factor EFA
#' plot_embeddings(mi_escala, colorear_por = "factor_efa")
#' }
#'
#' @export
plot_embeddings <- function(x,
                            metodo = "tsne",
                            colorear_por = "dimension",
                            mostrar_etiquetas = TRUE,
                            perplexity = NULL,
                            semilla = 42) {

  .verificar_ggplot2()

  # Extraer datos
  datos <- .extraer_datos_plot(x)
  embeddings <- datos$embeddings
  items_df <- datos$items

  n_items <- nrow(embeddings)

  # Reduccion dimensional
  set.seed(semilla)

  if (metodo == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      stop("Instala Rtsne: install.packages('Rtsne')")
    }
    if (is.null(perplexity)) {
      perplexity <- min(5, floor((n_items - 1) / 3))
    }
    tsne_result <- Rtsne::Rtsne(embeddings, dims = 2, perplexity = perplexity,
                                 check_duplicates = FALSE, verbose = FALSE)
    coords <- as.data.frame(tsne_result$Y)
    names(coords) <- c("Dim1", "Dim2")
    metodo_label <- "t-SNE"

  } else if (metodo == "umap") {
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop("Instala umap: install.packages('umap')")
    }
    umap_result <- umap::umap(embeddings)
    coords <- as.data.frame(umap_result$layout)
    names(coords) <- c("Dim1", "Dim2")
    metodo_label <- "UMAP"

  } else {
    # PCA simple como fallback
    pca <- prcomp(embeddings, scale. = TRUE)
    coords <- as.data.frame(pca$x[, 1:2])
    names(coords) <- c("Dim1", "Dim2")
    metodo_label <- "PCA"
  }

  # Agregar variables de color
  coords$item_num <- 1:n_items
  coords$dimension <- items_df$dimension

  if (colorear_por == "factor_efa" && !is.null(datos$efa)) {
    coords$color_var <- datos$efa$asignacion$factor_EFA[order(datos$efa$asignacion$item_num)]
    color_label <- "Factor EFA"
  } else {
    coords$color_var <- items_df$dimension
    color_label <- "Dimension"
  }

  # Crear plot
  p <- ggplot2::ggplot(coords, ggplot2::aes(x = Dim1, y = Dim2, color = color_var)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::labs(
      title = paste("Proyeccion de Items -", metodo_label),
      subtitle = paste0(n_items, " items en espacio semantico"),
      x = paste(metodo_label, "Dimension 1"),
      y = paste(metodo_label, "Dimension 2"),
      color = color_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "right"
    )

  # Agregar etiquetas
  if (mostrar_etiquetas) {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = item_num),
                                   size = 2.5, vjust = -0.8)
    } else {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = item_num),
                                         size = 2.5, max.overlaps = 20)
    }
  }

  # Agregar elipses por grupo
  p <- p + ggplot2::stat_ellipse(level = 0.68, linetype = "dashed", alpha = 0.5)

  return(p)
}


#' @title Red de Items por Similitud
#'
#' @description
#' Visualiza los items como una red donde las conexiones representan
#' similitud semantica por encima de un umbral.
#'
#' @param x Objeto semilla o semilla_embeddings
#' @param umbral Umbral de similitud para mostrar conexion (default: 0.5)
#' @param colorear_por Variable para colorear nodos
#' @param layout Algoritmo de layout: "fr", "kk", "circle", "star"
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' # Red con umbral 0.6
#' plot_red_items(mi_escala, umbral = 0.6)
#' }
#'
#' @export
plot_red_items <- function(x,
                           umbral = 0.5,
                           colorear_por = "dimension",
                           layout = "fr") {

  .verificar_ggplot2()

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Instala igraph: install.packages('igraph')")
  }

  # Extraer datos
  datos <- .extraer_datos_plot(x)
  similitud <- datos$similitud
  items_df <- datos$items

  n_items <- nrow(similitud)

  # Crear matriz de adyacencia
  adj_matrix <- as.matrix(similitud)
  if (is.null(dimnames(adj_matrix))) {
    nms <- paste0("item_", seq_len(n_items))
    dimnames(adj_matrix) <- list(nms, nms)
  }
  adj_matrix[adj_matrix < umbral] <- 0
  diag(adj_matrix) <- 0

  # Crear grafo
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected",
                                            weighted = TRUE, diag = FALSE)

  # Obtener coordenadas de layout
  coords <- switch(layout,
    "fr" = igraph::layout_with_fr(g),
    "kk" = igraph::layout_with_kk(g),
    "circle" = igraph::layout_in_circle(g),
    "star" = igraph::layout_as_star(g),
    igraph::layout_with_fr(g)
  )

  # Preparar datos para ggplot
  edges <- igraph::as_data_frame(g, what = "edges")
  nodes <- data.frame(
    name = 1:n_items,
    x = coords[, 1],
    y = coords[, 2],
    dimension = items_df$dimension
  )

  if (nrow(edges) > 0) {
    # Convertir nombres de vertices a indices numericos
    vertex_names <- igraph::V(g)$name
    from_idx <- match(edges$from, vertex_names)
    to_idx <- match(edges$to, vertex_names)
    edges$x1 <- coords[from_idx, 1]
    edges$y1 <- coords[from_idx, 2]
    edges$x2 <- coords[to_idx, 1]
    edges$y2 <- coords[to_idx, 2]
  }

  # Crear plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "right"
    )

  # Agregar aristas
  if (nrow(edges) > 0) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, alpha = weight),
      color = "gray60"
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.2, 0.8), guide = "none")
  }

  # Agregar nodos
  p <- p +
    ggplot2::geom_point(data = nodes,
                        ggplot2::aes(x = x, y = y, color = dimension),
                        size = 5) +
    ggplot2::geom_text(data = nodes,
                       ggplot2::aes(x = x, y = y, label = name),
                       size = 2.5, color = "white", fontface = "bold") +
    ggplot2::labs(
      title = "Red de Similitud Semantica",
      subtitle = paste0("Umbral: ", umbral, " | Conexiones: ", nrow(edges)),
      color = "Dimension"
    )

  return(p)
}


#' @title Scree Plot con Parallel Analysis
#'
#' @description
#' Visualiza eigenvalues del EFA junto con la linea de parallel analysis
#' para determinar el numero optimo de factores.
#'
#' @param x Objeto semilla o semilla_efa
#'
#' @return Objeto ggplot2
#'
#' @export
plot_scree <- function(x) {

  .verificar_ggplot2()

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Instala psych: install.packages('psych')")
  }

  # Extraer matriz de correlacion
  if (inherits(x, "semilla")) {
    matriz_cor <- x$similitud
  } else if (inherits(x, "semilla_efa")) {
    matriz_cor <- x$matriz_cor
  } else {
    stop("Objeto no valido")
  }

  # Parallel analysis
  pa <- psych::fa.parallel(matriz_cor, n.obs = 500, fa = "fa", fm = "minres",
                            plot = FALSE, main = "")

  # Preparar datos
  n_factors <- length(pa$fa.values)
  scree_df <- data.frame(
    Factor = 1:n_factors,
    Eigenvalue = pa$fa.values,
    Tipo = "Datos"
  )

  pa_df <- data.frame(
    Factor = 1:n_factors,
    Eigenvalue = pa$fa.sim,
    Tipo = "Parallel Analysis"
  )

  plot_df <- rbind(scree_df, pa_df)

  # Punto de corte
  n_sugerido <- pa$nfact

  # Crear plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Factor, y = Eigenvalue,
                                              color = Tipo, linetype = Tipo)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = n_sugerido + 0.5, linetype = "dashed",
                        color = "red", alpha = 0.7) +
    ggplot2::annotate("text", x = n_sugerido + 0.7, y = max(pa$fa.values) * 0.9,
                      label = paste("Factores\nsugeridos:", n_sugerido),
                      hjust = 0, size = 3, color = "red") +
    ggplot2::scale_color_manual(values = c("Datos" = "#3498DB", "Parallel Analysis" = "#E74C3C")) +
    ggplot2::labs(
      title = "Scree Plot con Parallel Analysis",
      subtitle = paste0("Factores sugeridos: ", n_sugerido),
      x = "Numero de Factor",
      y = "Eigenvalue",
      color = "", linetype = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )

  return(p)
}


#' @title Visualizar Cargas Factoriales
#'
#' @description
#' Muestra las cargas factoriales de cada item como barras horizontales,
#' agrupadas por factor.
#'
#' @param x Objeto semilla o semilla_efa
#' @param ordenar Si TRUE, ordena items por carga dentro de cada factor
#' @param umbral_carga Umbral para resaltar cargas significativas
#'
#' @return Objeto ggplot2
#'
#' @export
plot_cargas <- function(x,
                        ordenar = TRUE,
                        umbral_carga = 0.4) {

  .verificar_ggplot2()

  # Extraer datos
  if (inherits(x, "semilla")) {
    if (is.null(x$efa)) stop("Ejecuta precision_clasificacion() primero")
    asig <- x$efa$asignacion
  } else if (inherits(x, "semilla_efa")) {
    asig <- x$asignacion
  } else {
    stop("Objeto no valido")
  }

  # Preparar datos
  plot_df <- data.frame(
    item_num = asig$item_num,
    item_texto = substr(asig$item, 1, 40),
    factor = asig$factor_EFA,
    carga = asig$carga,
    stringsAsFactors = FALSE
  )

  if (ordenar) {
    plot_df <- plot_df[order(plot_df$factor, -plot_df$carga), ]
  }

  plot_df$item_label <- paste0(plot_df$item_num, ": ", plot_df$item_texto)
  plot_df$item_label <- factor(plot_df$item_label, levels = rev(plot_df$item_label))

  plot_df$significativo <- ifelse(plot_df$carga >= umbral_carga, "Si", "No")

  # Crear plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = carga, y = item_label, fill = factor)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::geom_vline(xintercept = umbral_carga, linetype = "dashed",
                        color = "red", alpha = 0.7) +
    ggplot2::facet_wrap(~ factor, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      title = "Cargas Factoriales por Item",
      subtitle = paste0("Linea roja: umbral ", umbral_carga),
      x = "Carga Factorial",
      y = "",
      fill = "Factor"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      axis.text.y = ggplot2::element_text(size = 7),
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold")
    )

  return(p)
}


#' @title Diagrama de Flujo: Estructura Teorica vs EFA
#'
#' @description
#' Visualiza la correspondencia entre dimensiones teoricas y factores EFA
#' usando un diagrama de flujo (alluvial/Sankey).
#'
#' @param x Objeto semilla con EFA
#'
#' @return Objeto ggplot2
#'
#' @export
plot_estructura <- function(x) {

  .verificar_ggplot2()

  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("Instala ggalluvial: install.packages('ggalluvial')")
  }

  # Extraer datos
  if (!inherits(x, "semilla") || is.null(x$efa)) {
    stop("Necesitas un objeto semilla con EFA")
  }

  asig <- x$efa$asignacion

  # Resolver columnas de origen (teorica) y destino (empirica). Acepta tanto el
  # esquema legado (factor_original/factor_EFA) como la asignacion actual de
  # precision_clasificacion() (dimension/cluster).
  fo <- if (!is.null(asig$factor_original)) asig$factor_original else asig$dimension
  fe <- if (!is.null(asig$factor_EFA))      asig$factor_EFA      else asig$cluster
  if (is.null(fo) || is.null(fe)) {
    stop("La asignacion del EFA no tiene columnas reconocibles ",
         "(factor_original/factor_EFA o dimension/cluster).")
  }
  # Etiquetar clusters numericos como F1, F2, ... para legibilidad
  if (is.numeric(fe) || all(grepl("^[0-9]+$", as.character(fe))))
    fe <- paste0("F", fe)

  # Crear tabla de frecuencias
  flow_df <- as.data.frame(table(Dimension = fo, Factor = fe))
  names(flow_df)[3] <- "n_items"

  flow_df <- flow_df[flow_df$n_items > 0, ]

  # Crear plot
  p <- ggplot2::ggplot(flow_df,
                        ggplot2::aes(axis1 = Dimension, axis2 = Factor, y = n_items)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = Dimension), alpha = 0.7) +
    ggalluvial::geom_stratum(width = 0.3, fill = "gray90", color = "gray40") +
    ggplot2::geom_text(stat = ggalluvial::StatStratum,
                       ggplot2::aes(label = ggplot2::after_stat(stratum)),
                       size = 3) +
    ggplot2::scale_x_discrete(limits = c("Dimension\nTeorica", "Factor\nEFA"),
                               expand = c(0.15, 0.05)) +
    ggplot2::labs(
      title = "Correspondencia: Estructura Teorica vs EFA",
      subtitle = "Flujo de items entre dimensiones originales y factores empiricos",
      y = "Numero de Items",
      fill = "Dimension"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  return(p)
}


#' @title Visualizar V de Aiken por Item
#'
#' @description
#' Grafico lollipop mostrando V de Aiken para cada item con intervalos
#' de confianza y linea de corte.
#'
#' @param cv Objeto semilla_cv de validez_contenido()
#' @param ordenar Si TRUE, ordena por V de Aiken
#' @param corte Valor de corte (default: 0.70)
#'
#' @return Objeto ggplot2
#'
#' @export
plot_v_aiken <- function(cv,
                         ordenar = TRUE,
                         corte = 0.70) {

  .verificar_ggplot2()

  if (!inherits(cv, "semilla_cv")) {
    stop("Necesitas un objeto semilla_cv de validez_contenido()")
  }

  # Preparar datos
  plot_df <- cv$v_aiken
  plot_df$item_label <- paste0(plot_df$numero, ": ", substr(plot_df$item, 1, 35))

  if (ordenar) {
    plot_df <- plot_df[order(-plot_df$V_promedio), ]
  }

  plot_df$item_label <- factor(plot_df$item_label, levels = rev(plot_df$item_label))
  plot_df$status <- ifelse(plot_df$V_promedio >= corte & plot_df$IC_inf >= corte,
                           "Aceptable", "Revisar")

  # Crear plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = V_promedio, y = item_label)) +
    ggplot2::geom_segment(ggplot2::aes(x = IC_inf, xend = IC_sup,
                                        y = item_label, yend = item_label),
                          color = "gray70", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(color = status), size = 3) +
    ggplot2::geom_vline(xintercept = corte, linetype = "dashed",
                        color = "red", linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("Aceptable" = "#27AE60", "Revisar" = "#E74C3C")) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::labs(
      title = "V de Aiken por Item",
      subtitle = paste0("Linea de corte: ", corte, " | IC ", cv$metadata$confianza * 100, "%"),
      x = "V de Aiken",
      y = "",
      color = "Estado"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      axis.text.y = ggplot2::element_text(size = 7),
      legend.position = "bottom"
    )

  return(p)
}


#' @title Visualizar Fiabilidad por Dimension
#'
#' @description
#' Grafico de barras mostrando alpha semantico por dimension con
#' interpretacion de niveles.
#'
#' @param fiab Objeto semilla_fiabilidad de fiabilidad_semantica()
#'
#' @return Objeto ggplot2
#'
#' @export
plot_fiabilidad <- function(fiab) {

  .verificar_ggplot2()

  if (!inherits(fiab, "semilla_fiabilidad")) {
    stop("Necesitas un objeto semilla_fiabilidad de fiabilidad_semantica()")
  }

  # Preparar datos
  plot_df <- fiab$alpha_dimensiones
  plot_df$interpretacion <- sapply(plot_df$alpha_semantico, .interpretar_alpha)
  plot_df$interpretacion <- factor(plot_df$interpretacion,
                                    levels = c("Pobre", "Cuestionable", "Aceptable", "Bueno", "Excelente"))

  # Colores por interpretacion
  colores_interp <- c(
    "Pobre" = "#E74C3C",
    "Cuestionable" = "#E67E22",
    "Aceptable" = "#F1C40F",
    "Bueno" = "#2ECC71",
    "Excelente" = "#27AE60"
  )

  # Crear plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = reorder(dimension, alpha_semantico),
                                              y = alpha_semantico,
                                              fill = interpretacion)) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::geom_hline(yintercept = c(0.6, 0.7, 0.8, 0.9), linetype = "dotted",
                        color = "gray50", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f\n(n=%d)", alpha_semantico, n_items)),
                       vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = colores_interp) +
    ggplot2::scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Alpha Semantico por Dimension",
      subtitle = paste0("Alpha promedio ponderado: ", sprintf("%.3f", fiab$alpha_promedio)),
      x = "",
      y = "Alpha Semantico",
      fill = "Interpretacion"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "right"
    )

  return(p)
}


#' @title Visualizar Discriminacion Semantica
#'
#' @description
#' Scatter plot de unicidad vs similitud media, coloreado por categoria
#' de discriminacion predicha.
#'
#' @param disc Resultado de discriminacion_semantica()
#'
#' @return Objeto ggplot2
#'
#' @export
plot_discriminacion <- function(disc) {

  .verificar_ggplot2()

  if (!is.data.frame(disc) || !"unicidad" %in% names(disc)) {
    stop("Necesitas el resultado de discriminacion_semantica()")
  }

  # Colores por categoria
  colores_disc <- c(
    "alta" = "#27AE60",
    "media" = "#F39C12",
    "baja" = "#E74C3C"
  )

  # Crear plot
  p <- ggplot2::ggplot(disc, ggplot2::aes(x = similitud_media, y = unicidad,
                                           color = discriminacion_predicha)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = c(0.3, 0.5), linetype = "dashed",
                        color = "gray50", alpha = 0.7) +
    ggplot2::scale_color_manual(values = colores_disc) +
    ggplot2::labs(
      title = "Discriminacion Semantica de Items",
      subtitle = "Unicidad alta = Mayor poder discriminativo predicho",
      x = "Similitud Media (dentro de dimension)",
      y = "Unicidad Semantica",
      color = "Discriminacion\nPredicha"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "right"
    )

  # Agregar etiquetas para items extremos
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    extremos <- disc[disc$discriminacion_predicha == "alta" |
                     disc$discriminacion_predicha == "baja", ]
    if (nrow(extremos) <= 10) {
      p <- p + ggplot2::geom_text(data = extremos,
                                   ggplot2::aes(label = numero),
                                   vjust = -1, size = 3)
    }
  } else {
    extremos <- disc[disc$discriminacion_predicha == "alta" |
                     disc$discriminacion_predicha == "baja", ]
    if (nrow(extremos) <= 15) {
      p <- p + ggrepel::geom_text_repel(data = extremos,
                                         ggplot2::aes(label = numero),
                                         size = 3, max.overlaps = 10)
    }
  }

  return(p)
}


#' @title Visualizar Parametros IRT Estimados
#'
#' @description
#' Scatter plot de dificultad vs discriminacion estimados, con tamano
#' proporcional a la informacion del item.
#'
#' @param irt Resultado de predecir_irt()
#' @param mostrar_etiquetas Mostrar numeros de items
#'
#' @return Objeto ggplot2
#'
#' @export
#' @noRd
plot_irt <- function(irt, mostrar_etiquetas = TRUE) {

  .verificar_ggplot2()

  if (!is.data.frame(irt) || !"dificultad_estimada" %in% names(irt)) {
    stop("Necesitas el resultado de predecir_irt()")
  }

  # Crear plot
  p <- ggplot2::ggplot(irt, ggplot2::aes(x = dificultad_estimada,
                                          y = discriminacion_estimada,
                                          size = informacion_estimada,
                                          color = dimension)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::scale_size_continuous(range = c(2, 8), name = "Informacion") +
    ggplot2::labs(
      title = "Parametros IRT Estimados desde Embeddings",
      subtitle = "Tamano = Informacion del item",
      x = "Dificultad Estimada (b)",
      y = "Discriminacion Estimada (a)",
      color = "Dimension"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "right"
    )

  if (mostrar_etiquetas && nrow(irt) <= 30) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = numero),
                                         size = 2.5, max.overlaps = 15)
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = numero),
                                   vjust = -1, size = 2.5)
    }
  }

  return(p)
}


#' @title Visualizar Matriz de Jaccard
#'
#' @description
#' Heatmap de indices Jaccard entre clusters semanticos y factores EFA.
#'
#' @param cs Resultado de cargas_semanticas()
#'
#' @return Objeto ggplot2
#'
#' @export
#' @noRd
plot_jaccard <- function(cs) {

  .verificar_ggplot2()

  if (!inherits(cs, "semilla_cargas_sem")) {
    stop("Necesitas el resultado de cargas_semanticas()")
  }

  # Preparar datos
  jaccard_mat <- cs$jaccard_matrix
  n_clusters <- nrow(jaccard_mat)
  n_factores <- ncol(jaccard_mat)

  plot_df <- expand.grid(
    Cluster = rownames(jaccard_mat),
    Factor = colnames(jaccard_mat)
  )
  plot_df$Jaccard <- as.vector(jaccard_mat)

  # Crear plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Factor, y = Cluster, fill = Jaccard)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Jaccard)),
                       color = ifelse(plot_df$Jaccard > 0.4, "white", "black"),
                       size = 4) +
    ggplot2::scale_fill_gradient2(
      low = "#3498DB",
      mid = "#F1C40F",
      high = "#27AE60",
      midpoint = 0.35,
      limits = c(0, 1),
      name = "Indice\nJaccard"
    ) +
    ggplot2::labs(
      title = "Correspondencia Clusters Semanticos - Factores EFA",
      subtitle = paste0("Jaccard promedio: ", sprintf("%.3f", cs$jaccard_promedio)),
      x = "Factor EFA",
      y = "Cluster Semantico"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed()

  return(p)
}


#' @title Visualizar Forma Corta
#'
#' @description
#' Compara la distribucion de items entre escala original y forma corta.
#'
#' @param fc Resultado de forma_corta()
#' @param escala_original Objeto semilla original (opcional)
#'
#' @return Objeto ggplot2
#'
#' @export
plot_forma_corta <- function(fc, escala_original = NULL) {

  .verificar_ggplot2()

  if (!inherits(fc, "semilla_forma_corta")) {
    stop("Necesitas el resultado de forma_corta()")
  }

  # Preparar datos
  if (!is.null(escala_original) && "dimension" %in% names(escala_original$items)) {
    orig_tabla <- as.data.frame(table(escala_original$items$dimension))
    names(orig_tabla) <- c("Dimension", "Original")
  } else {
    orig_tabla <- data.frame(
      Dimension = unique(fc$items$dimension),
      Original = fc$n_original / length(unique(fc$items$dimension))
    )
  }

  corta_tabla <- as.data.frame(table(fc$items$dimension))
  names(corta_tabla) <- c("Dimension", "Corta")

  plot_df <- merge(orig_tabla, corta_tabla, by = "Dimension", all = TRUE)
  plot_df[is.na(plot_df)] <- 0

  plot_df_long <- data.frame(
    Dimension = rep(plot_df$Dimension, 2),
    Escala = rep(c("Original", "Forma Corta"), each = nrow(plot_df)),
    Items = c(plot_df$Original, plot_df$Corta)
  )

  # Crear plot
  p <- ggplot2::ggplot(plot_df_long, ggplot2::aes(x = Dimension, y = Items, fill = Escala)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Items),
                       position = ggplot2::position_dodge(width = 0.9),
                       vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = c("Original" = "#3498DB", "Forma Corta" = "#E74C3C")) +
    ggplot2::labs(
      title = "Comparacion: Escala Original vs Forma Corta",
      subtitle = paste0("Reduccion: ", fc$n_original, " -> ", fc$n_seleccionados, " items (",
                        round((1 - fc$n_seleccionados/fc$n_original) * 100, 1), "%)"),
      x = "Dimension",
      y = "Numero de Items",
      fill = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


#' @title Dashboard Resumen Psicometrico
#'
#' @description
#' Panel integrado con las metricas psicometricas principales de la escala.
#'
#' @param x Objeto semilla completo
#' @param cv Resultado de validez_contenido() (opcional)
#' @param fiab Resultado de fiabilidad_semantica() (opcional)
#'
#' @return Objeto combinado de ggplot2 (requiere patchwork)
#'
#' @export
plot_resumen <- function(x, cv = NULL, fiab = NULL) {

  .verificar_ggplot2()

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Instala patchwork: install.packages('patchwork')")
  }

  if (!inherits(x, "semilla")) {
    stop("Necesitas un objeto semilla completo")
  }

  # Crear plots individuales
  plots <- list()

  # 1. Heatmap de similitud (simplificado)
  plots$similitud <- plot_similitud(x, mostrar_valores = FALSE) +
    ggplot2::labs(title = "Similitud Semantica") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 10))

  # 2. Proyeccion embeddings
  if (!is.null(x$embeddings)) {
    plots$embeddings <- tryCatch({
      plot_embeddings(x, mostrar_etiquetas = FALSE) +
        ggplot2::labs(title = "Espacio Semantico") +
        ggplot2::theme(legend.position = "none",
                       plot.title = ggplot2::element_text(size = 10))
    }, error = function(e) NULL)
  }

  # 3. Scree plot - ARCHIVADO (era para EFA)
  # Las funciones EFA han sido archivadas en efa_semantico_archivo.R
  # El flujo principal ahora usa clustering semantico


  # 3. V de Aiken si esta disponible
  if (!is.null(cv)) {
    plots$v_aiken <- plot_v_aiken(cv) +
      ggplot2::labs(title = "V de Aiken") +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 10),
                     axis.text.y = ggplot2::element_text(size = 5))
  }

  # 4. Fiabilidad si esta disponible
  if (!is.null(fiab)) {
    plots$fiabilidad <- plot_fiabilidad(fiab) +
      ggplot2::labs(title = "Alpha por Dimension") +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 10))
  }

  # Crear metricas textuales
  metricas_txt <- paste0(
    "RESUMEN PSICOMETRICO\n",
    "====================\n",
    "Items: ", nrow(x$items), "\n",
    "Dimensiones: ", length(unique(x$items$dimension)), "\n"
  )

  if (!is.null(x$efa)) {
    metricas_txt <- paste0(metricas_txt,
      "Clusters: ", x$efa$metadata$n_factores, "\n",
      "Varianza: ", round(sum(x$efa$varianza$Prop_Var) * 100, 1), "%\n"
    )
  }

  if (!is.null(fiab)) {
    metricas_txt <- paste0(metricas_txt,
      "Alpha prom: ", sprintf("%.3f", fiab$alpha_promedio), "\n"
    )
  }

  if (!is.null(cv)) {
    metricas_txt <- paste0(metricas_txt,
      "V Aiken: ", sprintf("%.3f", cv$v_aiken_escala$V_total), "\n"
    )
  }

  # Plot de texto
  plots$metricas <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = metricas_txt,
                      size = 3, hjust = 0.5, family = "mono") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#F8F9FA", color = NA))

  # Combinar con patchwork
  plots_disponibles <- plots[!sapply(plots, is.null)]

  if (length(plots_disponibles) >= 4) {
    combined <- (plots_disponibles[[1]] | plots_disponibles[[2]]) /
                (plots_disponibles[[3]] | plots_disponibles[[4]])
  } else if (length(plots_disponibles) >= 2) {
    combined <- plots_disponibles[[1]] | plots_disponibles[[2]]
  } else {
    combined <- plots_disponibles[[1]]
  }

  combined <- combined +
    patchwork::plot_annotation(
      title = paste("Dashboard Psicometrico:", x$metadata$concepto_original),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14)
      )
    )

  return(combined)
}


# =============================================================================
# FUNCIONES INTERNAS - PLOTS
# =============================================================================

#' @keywords internal
.verificar_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Instala ggplot2: install.packages('ggplot2')")
  }
}

#' @keywords internal
.extraer_datos_plot <- function(x) {
  if (inherits(x, "semilla")) {
    list(
      embeddings = x$embeddings,
      similitud = x$similitud,
      items = x$items,
      efa = x$efa
    )
  } else if (inherits(x, "semilla_embeddings")) {
    list(
      embeddings = x$embeddings,
      similitud = x$similitud,
      items = x$items,
      efa = NULL
    )
  } else {
    stop("Objeto no valido para visualizacion")
  }
}


# =============================================================================
# NUEVOS GRAFICOS: COHERENCIA Y PRECISION
# =============================================================================

#' @title Grafico Box Plot de Coherencia Intra vs Inter
#'
#' @description
#' Genera un box plot comparando la similitud semantica dentro de dimensiones
#' (intra) vs entre dimensiones (inter). Util para evaluar la separabilidad
#' de los factores.
#'
#' @param x Objeto semilla_coherencia (resultado de analizar_coherencia)
#' @param colores Vector de 2 colores para intra e inter (default: azul y rojo)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' coh <- analizar_coherencia(mi_escala)
#' plot_coherencia_boxplot(coh)
#' }
#'
#' @export
plot_coherencia_boxplot <- function(x,
                                     colores = c("#3498db", "#e74c3c"),
                                     titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_coherencia")) {
    stop("x debe ser resultado de analizar_coherencia()")
  }

  # Preparar datos
  datos <- data.frame(
    Tipo = c(rep("Intra-dimension", length(x$similitud_intra)),
             rep("Inter-dimension", length(x$similitud_inter))),
    Similitud = c(x$similitud_intra, x$similitud_inter)
  )

  datos$Tipo <- factor(datos$Tipo, levels = c("Intra-dimension", "Inter-dimension"))

  if (is.null(titulo)) {
    titulo <- paste0("Coherencia: Intra vs Inter Dimension\n",
                     "Diferencia de separabilidad: ", sprintf("%.3f", x$diferencia_separabilidad),
                     " (", x$evaluacion, ")")
  }

  # Crear grafico
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = Tipo, y = Similitud, fill = Tipo)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    ggplot2::stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "white") +
    ggplot2::scale_fill_manual(values = colores) +
    ggplot2::labs(
      title = titulo,
      x = "",
      y = "Similitud Semantica"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::annotate("text",
                      x = 1, y = x$mediana_intra + 0.05,
                      label = sprintf("Mediana: %.3f", x$mediana_intra),
                      size = 3.5, fontface = "bold") +
    ggplot2::annotate("text",
                      x = 2, y = x$mediana_inter + 0.05,
                      label = sprintf("Mediana: %.3f", x$mediana_inter),
                      size = 3.5, fontface = "bold")

  return(p)
}


#' @title Grafico Violin de Coherencia por Dimension
#'
#' @description
#' Genera un violin plot mostrando la distribucion de coherencia interna
#' para cada dimension. Permite identificar dimensiones con items heterogeneos.
#'
#' @param x Objeto semilla con embeddings calculados, o semilla_coherencia
#'   (resultado de analizar_coherencia)
#' @param umbral_minimo Umbral minimo de coherencia (linea de referencia)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' # Desde objeto semilla
#' plot_coherencia_violin(mi_escala)
#'
#' # Desde resultado de analizar_coherencia
#' coherencia <- analizar_coherencia(mi_escala)
#' plot_coherencia_violin(coherencia)
#' }
#'
#' @export
plot_coherencia_violin <- function(x, umbral_minimo = 0.50, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada - acepta semilla o semilla_coherencia
  if (!inherits(x, "semilla") && !inherits(x, "semilla_coherencia")) {
    stop("x debe ser un objeto de clase 'semilla' o 'semilla_coherencia'")
  }

  # Obtener datos segun el tipo de objeto

  if (inherits(x, "semilla_coherencia")) {
    # Usar datos pre-calculados de analizar_coherencia
    datos_violin <- x$datos_por_item
  } else {
    # Calcular desde objeto semilla
    matriz_sim <- x$similitud
    items_df <- x$items

    # Usar 'codigo' si existe, sino usar 'numero'
    if (!"codigo" %in% names(items_df)) {
      items_df$codigo <- paste0("Item_", items_df$numero)
    }

    dimensiones <- unique(items_df$dimension)

    datos_violin <- data.frame(
      Dimension = character(),
      Codigo = character(),
      Similitud_Media = numeric(),
      stringsAsFactors = FALSE
    )

    for (dim in dimensiones) {
      idx_dim <- which(items_df$dimension == dim)

      if (length(idx_dim) > 1) {
        for (i in idx_dim) {
          otros_idx <- setdiff(idx_dim, i)
          sim_media <- mean(matriz_sim[i, otros_idx], na.rm = TRUE)

          datos_violin <- rbind(datos_violin, data.frame(
            Dimension = dim,
            Codigo = items_df$codigo[i],
            Similitud_Media = sim_media,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  if (is.null(titulo)) {
    titulo <- "Coherencia Interna por Dimension"
  }

  # Crear grafico
  p <- ggplot2::ggplot(datos_violin,
                       ggplot2::aes(x = Dimension, y = Similitud_Media, fill = Dimension)) +
    ggplot2::geom_violin(alpha = 0.7, trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 1) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, size = 1.5) +
    ggplot2::geom_hline(yintercept = umbral_minimo, linetype = "dashed",
                        color = "red", linewidth = 1) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Linea roja: umbral minimo (", umbral_minimo, ")"),
      x = "",
      y = "Similitud Media con Dimension"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")

  return(p)
}


#' @title Grafico de Precision por Dimension
#'
#' @description
#' Genera un grafico de barras horizontales mostrando la precision de
#' clasificacion para cada dimension. Incluye colores segun nivel de precision.
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' plot_precision(prec)
#' }
#'
#' @export
plot_precision <- function(x, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  datos <- x$precision_por_dimension

  # Asignar colores segun precision
  datos$Color <- ifelse(datos$Precision >= 90, "Excelente",
                        ifelse(datos$Precision >= 80, "Buena",
                               ifelse(datos$Precision >= 70, "Aceptable", "Baja")))
  datos$Color <- factor(datos$Color, levels = c("Excelente", "Buena", "Aceptable", "Baja"))

  if (is.null(titulo)) {
    titulo <- paste0("Precision de Clasificacion por Dimension\n",
                     "Precision Global: ", sprintf("%.1f", x$precision_global), "% | ",
                     "ARI: ", sprintf("%.3f", x$ari))
  }

  # Ordenar por precision
  datos <- datos[order(datos$Precision, decreasing = TRUE), ]
  datos$Dimension <- factor(datos$Dimension, levels = datos$Dimension)

  colores_precision <- c("Excelente" = "#27ae60", "Buena" = "#3498db",
                         "Aceptable" = "#f39c12", "Baja" = "#e74c3c")

  p <- ggplot2::ggplot(datos,
                       ggplot2::aes(x = Dimension, y = Precision, fill = Color)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(sprintf("%.1f", Precision), "%\n(",
                                                    N_Correctos, "/", N_Items, ")")),
                       hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colores_precision, name = "Evaluacion") +
    ggplot2::scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
    ggplot2::labs(
      title = titulo,
      x = "",
      y = "Precision (%)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 80, linetype = "dashed", color = "gray50")

  return(p)
}


#' @title Grafico de Redundancia
#'
#' @description
#' Visualiza los pares de items redundantes como un grafico de red o heatmap.
#' Muestra que items tienen alta similitud dentro de su dimension.
#'
#' @param x Resultado de \code{analizar_redundancia()} (data.frame de pares
#'   redundantes con columnas \code{item1_num/item1/dim1/item2_num/item2/dim2/
#'   similitud}). Tambien acepta el objeto legado \code{semilla_redundancia}.
#' @param tipo Tipo de grafico: "barras" (pares por dimension, default) o
#'   "pares"/"tabla" (top pares individuales por similitud)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2 (o \code{NULL} invisible si no hay redundancias)
#'
#' @examples
#' \dontrun{
#' red <- analizar_redundancia(mi_escala)
#' plot_redundancia(red)              # barras por dimension
#' plot_redundancia(red, "pares")     # top pares por similitud
#' }
#'
#' @export
plot_redundancia <- function(x, tipo = "barras", titulo = NULL) {

  .verificar_ggplot2()

  # --- Ruta data.frame: salida actual de analizar_redundancia() -------------
  if (is.data.frame(x)) {
    if (nrow(x) == 0) {
      message("No hay redundancias para graficar.")
      return(invisible(NULL))
    }
    umbral <- attr(x, "umbral")
    if (is.null(umbral)) umbral <- min(x$similitud, na.rm = TRUE)
    if (is.null(titulo)) {
      titulo <- paste0("Analisis de Redundancia\n",
                       "Umbral: ", umbral, " | Pares encontrados: ", nrow(x))
    }

    tiene_dim <- all(c("dim1", "dim2") %in% names(x)) &&
      any(!is.na(x$dim1) | !is.na(x$dim2))

    if (tipo == "barras" && tiene_dim) {
      # Cada par cuenta una vez por cada dimension que involucra
      dims_por_par <- lapply(seq_len(nrow(x)),
                             function(k) unique(c(x$dim1[k], x$dim2[k])))
      dims_vec <- unlist(dims_por_par)
      dims_vec <- dims_vec[!is.na(dims_vec)]
      datos <- as.data.frame(table(Dimension = dims_vec),
                             stringsAsFactors = FALSE)
      names(datos)[2] <- "N_Redundancias"

      p <- ggplot2::ggplot(datos,
                           ggplot2::aes(x = stats::reorder(Dimension, N_Redundancias),
                                        y = N_Redundancias, fill = Dimension)) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = N_Redundancias),
                           hjust = -0.3, size = 4, fontface = "bold") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = titulo, x = "",
                      y = "Numero de Pares Redundantes") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "none",
                       plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
        ggplot2::scale_fill_brewer(palette = "Set2")
      return(p)
    }

    # tipo = "pares"/"tabla" (o "barras" sin info de dimension): top pares
    datos <- x[order(-x$similitud), , drop = FALSE]
    datos <- datos[seq_len(min(12, nrow(datos))), , drop = FALSE]
    datos$Par <- paste0("#", datos$item1_num, " vs #", datos$item2_num)
    datos$Par <- factor(datos$Par, levels = rev(datos$Par))
    fill_var <- if (tiene_dim) ifelse(datos$dim1 == datos$dim2 & !is.na(datos$dim1),
                                      datos$dim1, "entre dimensiones") else "par"
    datos$Grupo <- fill_var

    p <- ggplot2::ggplot(datos,
                         ggplot2::aes(x = Par, y = similitud, fill = Grupo)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", similitud)),
                         hjust = -0.1, size = 3) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 1.1)) +
      ggplot2::labs(title = titulo, x = "Pares de Items", y = "Similitud",
                    fill = "") +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                     legend.position = "bottom") +
      ggplot2::geom_hline(yintercept = umbral, linetype = "dashed", color = "red")
    return(p)
  }

  # --- Ruta legado: objeto semilla_redundancia ------------------------------
  if (!inherits(x, "semilla_redundancia")) {
    stop("x debe ser resultado de analizar_redundancia()")
  }

  if (x$n_redundancias == 0) {
    message("No hay redundancias para graficar.")
    return(invisible(NULL))
  }

  if (is.null(titulo)) {
    titulo <- paste0("Analisis de Redundancia\n",
                     "Umbral: ", x$umbral, " | Pares encontrados: ", x$n_redundancias)
  }

  if (tipo == "barras") {
    # Grafico de barras por dimension
    datos <- x$redundancias_por_dimension

    p <- ggplot2::ggplot(datos,
                         ggplot2::aes(x = reorder(Dimension, N_Redundancias),
                                      y = N_Redundancias, fill = Dimension)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = N_Redundancias),
                         hjust = -0.3, size = 4, fontface = "bold") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = titulo,
        x = "",
        y = "Numero de Pares Redundantes"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2")

  } else {
    # Grafico de pares individuales
    datos <- x$pares_redundantes[1:min(10, nrow(x$pares_redundantes)), ]
    datos$Par <- paste0(datos$Item1_Codigo, " - ", datos$Item2_Codigo)
    datos$Par <- factor(datos$Par, levels = rev(datos$Par))

    p <- ggplot2::ggplot(datos,
                         ggplot2::aes(x = Par, y = Similitud, fill = Dimension)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Similitud)),
                         hjust = -0.1, size = 3) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 1.1)) +
      ggplot2::labs(
        title = titulo,
        x = "Pares de Items",
        y = "Similitud"
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      ggplot2::geom_hline(yintercept = x$umbral, linetype = "dashed", color = "red")
  }

  return(p)
}


#' @title Grafico de Evolucion de Precision (Refinamiento)
#'
#' @description
#' Muestra la evolucion de la precision a traves de iteraciones de refinamiento.
#' Util para documentar el proceso de mejora de la escala.
#'
#' @param datos_iteraciones Dataframe con columnas: Iteracion, Precision
#' @param objetivo Precision objetivo (linea de referencia)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' # Despues de refinamiento iterativo
#' datos <- data.frame(
#'   Iteracion = 1:3,
#'   Precision = c(80, 85, 95)
#' )
#' plot_evolucion_precision(datos, objetivo = 90)
#' }
#'
#' @export
plot_evolucion_precision <- function(datos_iteraciones,
                                      objetivo = 90,
                                      titulo = NULL) {

  .verificar_ggplot2()

  if (!all(c("Iteracion", "Precision") %in% names(datos_iteraciones))) {
    stop("datos_iteraciones debe tener columnas 'Iteracion' y 'Precision'")
  }

  if (is.null(titulo)) {
    precision_final <- datos_iteraciones$Precision[nrow(datos_iteraciones)]
    titulo <- paste0("Evolucion de Precision en Refinamiento\n",
                     "Precision Final: ", sprintf("%.1f", precision_final), "%")
  }

  p <- ggplot2::ggplot(datos_iteraciones,
                       ggplot2::aes(x = Iteracion, y = Precision)) +
    ggplot2::geom_line(color = "#3498db", linewidth = 1.5) +
    ggplot2::geom_point(color = "#2980b9", size = 4) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(sprintf("%.1f", Precision), "%")),
                       vjust = -1, size = 3.5, fontface = "bold") +
    ggplot2::geom_hline(yintercept = objetivo, linetype = "dashed",
                        color = "#27ae60", linewidth = 1) +
    ggplot2::annotate("text",
                      x = max(datos_iteraciones$Iteracion),
                      y = objetivo + 2,
                      label = paste0("Objetivo: ", objetivo, "%"),
                      color = "#27ae60", size = 3.5, hjust = 1) +
    ggplot2::scale_x_continuous(breaks = datos_iteraciones$Iteracion) +
    ggplot2::scale_y_continuous(limits = c(min(datos_iteraciones$Precision) - 10,
                                           max(datos_iteraciones$Precision, objetivo) + 10)) +
    ggplot2::labs(
      title = titulo,
      x = "Iteracion",
      y = "Precision (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}


#' @title Grafico Sankey/Alluvial de Flujo de Items
#'
#' @description
#' Genera un diagrama de flujo (alluvial) mostrando como los items de cada dimension
#' teorica se asignan a los clusters semanticos. Los flujos se colorean segun si
#' la clasificacion fue correcta (verde) o incorrecta (rojo).
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' plot_sankey(prec)
#' }
#'
#' @export
plot_sankey <- function(x, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  asig <- x$asignacion_clusters
  correctos <- x$precision_por_dimension

  # Crear mapeo de cluster a nombre de dimension
  cluster_a_dimension <- setNames(correctos$Dimension, correctos$Cluster_Asignado)

  # Para clusters sin mapeo, determinar dimension dominante
  todos_clusters <- unique(asig$cluster)
  for (cl in todos_clusters) {
    if (!(cl %in% names(cluster_a_dimension))) {
      items_en_cluster <- asig[asig$cluster == cl, ]
      if (nrow(items_en_cluster) > 0) {
        dim_freq <- table(items_en_cluster$dimension)
        cluster_a_dimension[cl] <- names(which.max(dim_freq))
      }
    }
  }

  # Agregar nombre de dimension del cluster asignado
  asig$cluster_nombre <- cluster_a_dimension[asig$cluster]
  asig$cluster_nombre <- ifelse(is.na(asig$cluster_nombre), asig$cluster, asig$cluster_nombre)

  # Determinar estado (correcto/incorrecto)
  asig$Estado <- mapply(function(dim, clust) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0 && clust == expected) "Correctamente clasificado" else "Mal clasificado"
  }, asig$dimension, asig$cluster)

  # Preparar datos para alluvial
  flujos <- as.data.frame(table(asig$dimension, asig$cluster_nombre, asig$Estado))
  names(flujos) <- c("Factor_Teorico", "Cluster_Asignado", "Estado", "Freq")
  flujos <- flujos[flujos$Freq > 0, ]

  # Verificar ggalluvial
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    message("Para diagrama alluvial instala: install.packages('ggalluvial')")
    message("Generando grafico de barras alternativo...")

    p <- ggplot2::ggplot(flujos,
                         ggplot2::aes(x = Factor_Teorico, y = Freq, fill = Estado)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
      ggplot2::scale_fill_manual(values = c("Correctamente clasificado" = "#66c2a5",
                                            "Mal clasificado" = "#fc8d62")) +
      ggplot2::labs(
        title = if(is.null(titulo)) "Flujo de Items: Factor Teorico -> Cluster Asignado" else titulo,
        x = "Factor Teorico",
        y = "Numero de Items"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    return(p)
  }

  if (is.null(titulo)) {
    titulo <- expression(bold("Flujo de Items: Factor Teorico" %->% "Cluster Semantico"))
  }

  # Crear diagrama alluvial
  p <- ggplot2::ggplot(flujos,
                       ggplot2::aes(y = Freq, axis1 = Factor_Teorico, axis2 = Cluster_Asignado)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = Estado), width = 1/8, alpha = 0.75,
                               curve_type = "sigmoid") +
    ggalluvial::geom_stratum(width = 1/8, fill = "grey90", color = "grey30", linewidth = 0.3) +
    ggplot2::geom_text(stat = ggalluvial::StatStratum,
                       ggplot2::aes(label = ggplot2::after_stat(stratum)),
                       size = 3.2) +
    ggplot2::scale_x_discrete(limits = c("Factor Teorico", "Cluster Asignado"),
                               expand = c(0.15, 0.05)) +
    ggplot2::scale_fill_manual(
      values = c("Correctamente clasificado" = "#66c2a5",
                 "Mal clasificado" = "#fc8d62"),
      name = "Estado"
    ) +
    ggplot2::labs(
      title = "Flujo de Items: Factor Teorico \u2192 Cluster Semantico",
      subtitle = "Verde = Clasificacion correcta | Rojo = Mal clasificados"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      legend.position = "bottom",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(p)
}


#' @title Grafico de Flujo de Items Problematicos
#'
#' @description
#' Genera un diagrama de flujo (alluvial) mostrando SOLO los items mal clasificados,
#' visualizando desde que dimension teorica provienen y a que dimension fueron asignados.
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' plot_flujo_problematicos(prec)
#' }
#'
#' @export
#' @noRd
plot_flujo_problematicos <- function(x, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  asig <- x$asignacion_clusters
  correctos <- x$precision_por_dimension

  # Crear mapeo de cluster a nombre de dimension
  cluster_a_dimension <- setNames(correctos$Dimension, correctos$Cluster_Asignado)

  # Para clusters sin mapeo, determinar dimension dominante
  todos_clusters <- unique(asig$cluster)
  for (cl in todos_clusters) {
    if (!(cl %in% names(cluster_a_dimension))) {
      items_en_cluster <- asig[asig$cluster == cl, ]
      if (nrow(items_en_cluster) > 0) {
        dim_freq <- table(items_en_cluster$dimension)
        cluster_a_dimension[cl] <- names(which.max(dim_freq))
      }
    }
  }

  # Agregar nombre de dimension del cluster asignado
  asig$cluster_nombre <- cluster_a_dimension[asig$cluster]
  asig$cluster_nombre <- ifelse(is.na(asig$cluster_nombre), asig$cluster, asig$cluster_nombre)

  # Determinar estado y filtrar solo incorrectos
  asig$Estado <- mapply(function(dim, clust) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0 && clust == expected) "Correcto" else "Incorrecto"
  }, asig$dimension, asig$cluster)

  problematicos <- asig[asig$Estado == "Incorrecto", ]

  if (nrow(problematicos) == 0) {
    # Grafico con mensaje de exito
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No hay items problematicos\nTodos clasificados correctamente",
                        size = 6, fontface = "bold", color = "#27ae60") +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      ) +
      ggplot2::labs(title = "Flujo de Items Problematicos",
                    subtitle = "Precision: 100%")
    return(p)
  }

  # Preparar datos para alluvial
  flujos <- as.data.frame(table(problematicos$dimension, problematicos$cluster_nombre))
  names(flujos) <- c("Dimension_Original", "Cluster_Asignado", "Freq")
  flujos <- flujos[flujos$Freq > 0, ]

  # Verificar ggalluvial
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    message("Para diagrama alluvial instala: install.packages('ggalluvial')")

    p <- ggplot2::ggplot(flujos,
                         ggplot2::aes(x = Dimension_Original, y = Freq, fill = Cluster_Asignado)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        title = if(is.null(titulo)) "Items Problematicos: Flujo de Reclasificacion" else titulo,
        subtitle = paste0(nrow(problematicos), " items mal clasificados"),
        x = "Dimension Original",
        y = "Numero de Items"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    return(p)
  }

  if (is.null(titulo)) {
    titulo <- "Flujo de Items Problematicos"
  }

  # Colores por dimension de destino
  dims_destino <- unique(flujos$Cluster_Asignado)
  n_dims <- length(dims_destino)
  if (n_dims <= 8) {
    colores <- RColorBrewer::brewer.pal(max(3, n_dims), "Set2")[1:n_dims]
  } else {
    colores <- scales::hue_pal()(n_dims)
  }
  names(colores) <- dims_destino

  n_total <- nrow(asig)
  n_prob <- nrow(problematicos)

  # Crear diagrama alluvial
  p <- ggplot2::ggplot(flujos,
                       ggplot2::aes(y = Freq, axis1 = Dimension_Original, axis2 = Cluster_Asignado)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = Cluster_Asignado), width = 1/8, alpha = 0.75,
                               curve_type = "sigmoid") +
    ggalluvial::geom_stratum(width = 1/8, fill = "grey90", color = "grey30", linewidth = 0.3) +
    ggplot2::geom_text(stat = ggalluvial::StatStratum,
                       ggplot2::aes(label = ggplot2::after_stat(stratum)),
                       size = 3.2) +
    ggplot2::scale_x_discrete(limits = c("Dimension Original", "Cluster Asignado"),
                               expand = c(0.15, 0.05)) +
    ggplot2::scale_fill_manual(values = colores, name = "Dimension\nAsignada") +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0(n_prob, " items mal clasificados de ", n_total, " totales (",
                        round(n_prob/n_total*100, 1), "%)")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "#e74c3c"),
      legend.position = "bottom",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(p)
}


#' @title Grafico de Flujo de Items con Fraseo
#'
#' @description
#' Genera un grafico combinando el fraseo de cada item con lineas de flujo
#' que conectan la dimension teorica con el cluster asignado. Muestra todos
#' los items (o solo los problematicos) con su texto completo.
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param solo_problematicos Si TRUE, muestra solo items mal clasificados (default: FALSE)
#' @param max_chars Maximo de caracteres del item a mostrar (default: 45)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' plot_flujo_items(prec)  # todos los items
#' plot_flujo_items(prec, solo_problematicos = TRUE)  # solo problematicos
#' }
#'
#' @export
#' @noRd
plot_flujo_items <- function(x, solo_problematicos = FALSE, max_chars = 45, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  asig <- x$asignacion_clusters
  correctos <- x$precision_por_dimension

  # Crear mapeo de cluster a nombre de dimension
  cluster_a_dimension <- setNames(correctos$Dimension, correctos$Cluster_Asignado)

  # Para clusters sin mapeo, determinar dimension dominante
  todos_clusters <- unique(asig$cluster)
  for (cl in todos_clusters) {
    if (!(cl %in% names(cluster_a_dimension))) {
      items_en_cluster <- asig[asig$cluster == cl, ]
      if (nrow(items_en_cluster) > 0) {
        dim_freq <- table(items_en_cluster$dimension)
        cluster_a_dimension[cl] <- names(which.max(dim_freq))
      }
    }
  }

  # Agregar nombre de dimension del cluster asignado
  asig$cluster_nombre <- cluster_a_dimension[asig$cluster]
  asig$cluster_nombre <- ifelse(is.na(asig$cluster_nombre), asig$cluster, asig$cluster_nombre)

  # Determinar estado
  asig$Estado <- mapply(function(dim, clust) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0 && clust == expected) "Correcto" else "Incorrecto"
  }, asig$dimension, asig$cluster)

  n_total <- nrow(asig)

  # Filtrar si solo problematicos
 if (solo_problematicos) {
    datos_plot <- asig[asig$Estado == "Incorrecto", ]
    if (nrow(datos_plot) == 0) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Todos los items fueron\nclasificados correctamente",
                          size = 6, fontface = "bold", color = "#27ae60") +
        ggplot2::theme_void() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background = ggplot2::element_rect(fill = "white", color = NA)
        )
      return(p)
    }
  } else {
    datos_plot <- asig
  }

  # Crear etiquetas para items
  if ("codigo" %in% names(datos_plot)) {
    datos_plot$Item_ID <- datos_plot$codigo
  } else if ("numero" %in% names(datos_plot)) {
    datos_plot$Item_ID <- paste0("Item_", datos_plot$numero)
  } else {
    datos_plot$Item_ID <- paste0("Item_", seq_len(nrow(datos_plot)))
  }

  # Truncar texto del item
  datos_plot$Item_Texto <- ifelse(
    nchar(datos_plot$item) > max_chars,
    paste0(substr(datos_plot$item, 1, max_chars - 3), "..."),
    datos_plot$item
  )

  # Ordenar por dimension y estado
  datos_plot <- datos_plot[order(datos_plot$dimension, -as.numeric(datos_plot$Estado == "Incorrecto")), ]
  datos_plot$y_pos <- rev(seq_len(nrow(datos_plot)))

  # Calcular posiciones Y para dimensiones de destino (cluster_nombre)
  dims_destino <- unique(datos_plot$cluster_nombre)
  n_items <- nrow(datos_plot)

  # Distribuir las dimensiones de destino uniformemente en el eje Y
  dim_y_positions <- data.frame(
    cluster_nombre = dims_destino,
    y_destino = seq(from = n_items * 0.9, to = n_items * 0.1, length.out = length(dims_destino))
  )
  datos_plot <- merge(datos_plot, dim_y_positions, by = "cluster_nombre", all.x = TRUE)

  # Reordenar despues del merge
  datos_plot <- datos_plot[order(-datos_plot$y_pos), ]

  # Colores para dimensiones
  all_dims <- unique(c(datos_plot$dimension, datos_plot$cluster_nombre))
  n_dims <- length(all_dims)
  if (n_dims <= 8) {
    colores <- RColorBrewer::brewer.pal(max(3, n_dims), "Set2")
  } else {
    colores <- scales::hue_pal()(n_dims)
  }
  names(colores) <- all_dims

  # Titulo
  if (is.null(titulo)) {
    if (solo_problematicos) {
      titulo <- "Flujo de Items Mal Clasificados"
    } else {
      titulo <- "Flujo de Items: Dimension Original \u2192 Cluster Asignado"
    }
  }

  # Subtitulo
  n_incorrectos <- sum(datos_plot$Estado == "Incorrecto")
  if (solo_problematicos) {
    subtitulo <- paste0(nrow(datos_plot), " items mal clasificados de ", n_total, " totales (",
                        round(nrow(datos_plot)/n_total*100, 1), "%)")
  } else {
    subtitulo <- paste0("Verde = Correctos (", n_total - n_incorrectos, ") | ",
                        "Rojo = Incorrectos (", n_incorrectos, ")")
  }

  # Posiciones X
  x_item <- 0
  x_dim_orig <- 3.5
  x_dim_dest <- 6.5

  # Crear grafico
  p <- ggplot2::ggplot(datos_plot) +
    # Fondo para items
    ggplot2::geom_tile(ggplot2::aes(x = x_item, y = y_pos),
                       fill = "#f8f9fa", width = 3.2, height = 0.85) +
    # Texto del item
    ggplot2::geom_text(ggplot2::aes(x = x_item - 1.5, y = y_pos,
                                     label = paste0(Item_ID, ": ", Item_Texto)),
                       hjust = 0, size = 2.3, color = "grey20") +
    # Caja de dimension original
    ggplot2::geom_tile(ggplot2::aes(x = x_dim_orig, y = y_pos, fill = dimension),
                       width = 1.4, height = 0.8, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(x = x_dim_orig, y = y_pos, label = dimension),
                       size = 2, color = "white", fontface = "bold") +
    # Lineas de flujo (curvas bezier)
    ggplot2::geom_curve(ggplot2::aes(x = x_dim_orig + 0.7, xend = x_dim_dest - 0.7,
                                      y = y_pos, yend = y_destino,
                                      color = Estado),
                        curvature = 0.3, linewidth = 0.6, alpha = 0.7,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"),
                                               type = "closed")) +
    # Cajas de dimension destino (una por cada dimension unica)
    ggplot2::geom_tile(data = dim_y_positions,
                       ggplot2::aes(x = x_dim_dest, y = y_destino),
                       fill = colores[dim_y_positions$cluster_nombre],
                       width = 1.4, height = n_items/length(dims_destino) * 0.8,
                       alpha = 0.9) +
    ggplot2::geom_text(data = dim_y_positions,
                       ggplot2::aes(x = x_dim_dest, y = y_destino, label = cluster_nombre),
                       size = 2.5, color = "white", fontface = "bold") +
    # Colores
    ggplot2::scale_fill_manual(values = colores, guide = "none") +
    ggplot2::scale_color_manual(
      values = c("Correcto" = "#27ae60", "Incorrecto" = "#e74c3c"),
      name = "Estado"
    ) +
    # Ejes
    ggplot2::scale_x_continuous(
      breaks = c(x_item, x_dim_orig, x_dim_dest),
      labels = c("Item", "Dimension\nOriginal", "Cluster\nAsignado"),
      limits = c(-1.7, 7.5)
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      x = "", y = ""
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10,
                                            color = if(solo_problematicos) "#e74c3c" else "grey40"),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 9, face = "bold"),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(p)
}


#' @title Grafico de Items Problematicos con Fraseo
#'
#' @description
#' Genera un grafico mostrando los items mal clasificados con su fraseo,
#' dimension teorica original y cluster al que fueron asignados.
#' Ideal para identificar que items especificos cambiaron de factor.
#'
#' @param x Objeto semilla_precision (resultado de precision_clasificacion)
#' @param max_chars Maximo de caracteres del item a mostrar (default: 50)
#' @param titulo Titulo del grafico (default: auto)
#'
#' @return Objeto ggplot2
#'
#' @examples
#' \dontrun{
#' prec <- precision_clasificacion(mi_escala)
#' plot_items_problematicos(prec)
#' }
#'
#' @export
#' @noRd
plot_items_problematicos <- function(x, max_chars = 50, titulo = NULL) {

  .verificar_ggplot2()

  # Validar entrada
  if (!inherits(x, "semilla_precision")) {
    stop("x debe ser resultado de precision_clasificacion()")
  }

  # Obtener datos
  asig <- x$asignacion_clusters
  correctos <- x$precision_por_dimension

  # Crear columna de estado
  asig$Estado <- mapply(function(dim, clust) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0 && clust == expected) "Correcto" else "Incorrecto"
  }, asig$dimension, asig$cluster)

  n_total <- nrow(asig)

  # Filtrar solo incorrectos
  datos_plot <- asig[asig$Estado == "Incorrecto", ]

  if (nrow(datos_plot) == 0) {
    # Grafico con mensaje de exito
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "Todos los items fueron\nclasificados correctamente",
                        size = 6, fontface = "bold", color = "#27ae60") +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      ) +
      ggplot2::labs(title = "Items Problematicos",
                    subtitle = paste0("Precision: 100% (", n_total, "/", n_total, " items)"))
    return(p)
  }

  # Crear etiquetas para items
  if ("codigo" %in% names(datos_plot)) {
    datos_plot$Item_ID <- datos_plot$codigo
  } else if ("numero" %in% names(datos_plot)) {
    datos_plot$Item_ID <- paste0("Item_", datos_plot$numero)
  } else {
    datos_plot$Item_ID <- paste0("Item_", seq_len(nrow(datos_plot)))
  }

  # Truncar texto del item
  datos_plot$Item_Texto <- ifelse(
    nchar(datos_plot$item) > max_chars,
    paste0(substr(datos_plot$item, 1, max_chars - 3), "..."),
    datos_plot$item
  )

  # Crear etiqueta combinada
  datos_plot$Item_Label <- paste0(datos_plot$Item_ID, ": ", datos_plot$Item_Texto)

  # Obtener cluster esperado
  datos_plot$Cluster_Esperado <- sapply(datos_plot$dimension, function(dim) {
    expected <- correctos$Cluster_Asignado[correctos$Dimension == dim]
    if (length(expected) > 0) expected else NA
  })

  # Crear mapeo de cluster a nombre de dimension
  # Para mostrar el nombre de la dimension en lugar de "Cluster_X"
  # Primero usamos el mapeo de clusters esperados
  cluster_a_dimension <- setNames(correctos$Dimension, correctos$Cluster_Asignado)

  # Para clusters sin mapeo, determinamos la dimension dominante de todos los items
  todos_clusters <- unique(asig$cluster)
  for (cl in todos_clusters) {
    if (!(cl %in% names(cluster_a_dimension))) {
      # Encontrar la dimension mas frecuente en este cluster
      items_en_cluster <- asig[asig$cluster == cl, ]
      if (nrow(items_en_cluster) > 0) {
        dim_freq <- table(items_en_cluster$dimension)
        dim_dominante <- names(which.max(dim_freq))
        cluster_a_dimension[cl] <- dim_dominante
      }
    }
  }

  datos_plot$cluster_nombre <- cluster_a_dimension[datos_plot$cluster]
  # Si aun no hay mapeo, usar el nombre del cluster original
  datos_plot$cluster_nombre <- ifelse(is.na(datos_plot$cluster_nombre),
                                       datos_plot$cluster,
                                       datos_plot$cluster_nombre)

  # Ordenar por dimension
  datos_plot <- datos_plot[order(datos_plot$dimension), ]
  datos_plot$Item_Label <- factor(datos_plot$Item_Label, levels = rev(datos_plot$Item_Label))

  # Titulo
  if (is.null(titulo)) {
    titulo <- "Items Mal Clasificados"
  }

  # Calcular posiciones para las columnas
  # Col 1: Item (texto) | Col 2: Dimension Teorica | Col 3: Flecha | Col 4: Cluster Asignado

  # Crear grafico
  p <- ggplot2::ggplot(datos_plot, ggplot2::aes(y = Item_Label)) +
    # Fondo para el texto del item
    ggplot2::geom_tile(ggplot2::aes(x = 0), fill = "#f8f9fa", width = 3.8, height = 0.9) +
    # Texto del item (alineado a la izquierda)
    ggplot2::geom_text(ggplot2::aes(x = -1.8, label = Item_Label),
                       hjust = 0, size = 2.8, color = "grey20") +
    # Caja de dimension teorica
    ggplot2::geom_tile(ggplot2::aes(x = 2.5, fill = dimension), width = 1.2, height = 0.85) +
    ggplot2::geom_text(ggplot2::aes(x = 2.5, label = dimension),
                       size = 2.2, color = "white", fontface = "bold") +
    # Flecha roja (indicando error)
    ggplot2::geom_segment(ggplot2::aes(x = 3.2, xend = 3.8, yend = Item_Label),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
                          color = "#e74c3c", linewidth = 1.5) +
    # Caja de cluster asignado (mostrando nombre de dimension, no numero de cluster)
    ggplot2::geom_tile(ggplot2::aes(x = 4.5), fill = "#fee2e2", width = 1.2, height = 0.85) +
    ggplot2::geom_text(ggplot2::aes(x = 4.5, label = cluster_nombre),
                       size = 2.2, color = "#991b1b", fontface = "bold") +
    # Escala de colores para dimensiones
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    # Configurar ejes
    ggplot2::scale_x_continuous(
      breaks = c(0, 2.5, 4.5),
      labels = c("Item", "Dimension\nOriginal", "Cluster\nAsignado"),
      limits = c(-2, 5.5)
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0(nrow(datos_plot), " items mal clasificados de ", n_total, " totales (",
                        round(nrow(datos_plot)/n_total*100, 1), "%)"),
      x = "",
      y = ""
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "#e74c3c"),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 9, face = "bold"),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(p)
}
