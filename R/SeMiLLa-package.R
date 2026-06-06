#' @keywords internal
"_PACKAGE"

#' @importFrom stats as.dist as.hclust ave coef cor cutree dist hclust kmeans
#'   mad median prcomp predict qnorm quantile reorder runif sd weighted.mean
#' @importFrom utils adist combn
NULL

## Variables usadas en evaluacion no estandar (aes() de ggplot2, with(), etc.)
## que R CMD check reporta como "no visible binding for global variable".
utils::globalVariables(c(
  "Cluster", "Cluster_Asignado", "Color", "Dim1", "Dim2", "Dimension",
  "Dimension_Original", "Eigenvalue", "Escala", "Estado", "Factor",
  "Factor_Teorico", "Freq", "Grupo", "IC_inf", "IC_sup", "Item1", "Item2",
  "Item_ID", "Item_Label", "Item_Texto", "Items", "Iteracion", "Jaccard",
  "N_Correctos", "N_Items", "N_Redundancias", "Par", "Precision", "Similitud",
  "Similitud_Media", "Tipo", "V_promedio", "alpha_semantico", "carga",
  "cluster_nombre", "color_var", "dificultad_estimada", "dimension",
  "discriminacion_estimada", "discriminacion_predicha", "fill",
  "informacion_estimada", "interpretacion", "item_label", "item_num", "label",
  "n_items", "name", "numero", "similitud", "similitud_media", "status",
  "stratum", "unicidad", "weight", "x", "x1", "x2", "xend", "y", "y1", "y2",
  "y_destino", "y_pos", "yend"
))
