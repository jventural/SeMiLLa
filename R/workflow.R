#' @title Visualizar Flujo de Trabajo SeMiLLa v2.0
#'
#' @description
#' Muestra el flujo de trabajo del paquete (Manual de Usuario v2.0,
#' organizado en 9 fases y 25 pasos). El flujo refleja exactamente el
#' orden recomendado de uso de las funciones, desde la configuracion
#' inicial hasta el entregable final administrable.
#'
#' @param tipo "texto" (default, ASCII en consola) o "grafico"
#'   (genera diagrama con DiagrammeR/ggplot2)
#' @param archivo Si tipo = "grafico", nombre del archivo PNG (sin extension)
#'
#' @return Invisiblemente NULL (texto) u objeto ggplot (grafico).
#'
#' @examples
#' \dontrun{
#' flujo()                                    # texto ASCII en consola
#' flujo(tipo = "grafico", archivo = "wf")    # PNG en disco
#' }
#'
#' @export
flujo <- function(tipo = "texto", archivo = NULL) {

  if (tipo == "texto") {
    .flujo_texto()
  } else if (tipo == "grafico") {
    .flujo_grafico(archivo)
  } else {
    stop("tipo debe ser 'texto' o 'grafico'")
  }

  invisible(NULL)
}


#' @keywords internal
.flujo_texto <- function() {

  cat("\n")
  cat(.linea("="), "\n")
  cat(.color_verde("FLUJO DE TRABAJO SeMiLLa v2.0 (Manual de Usuario)"), "\n")
  cat(.linea("="), "\n\n")

  cat("  ", .color_amarillo("FASE I. ANTES DE EMPEZAR"), "\n", sep = "")
  cat("    Paso 1.  Instalacion y configuracion\n")
  cat("    Paso 2.  cache(action, path)              [cache de llamadas LLM]\n\n")

  cat("  ", .color_verde("FASE II. CONSTRUCCION DE LA ESCALA"), "\n", sep = "")
  cat("    Paso 3.  generar_items(tipo = ...)        [likert/historias/guttman/...]\n")
  cat("    Paso 3b. semilla(fuente = 'usuario')      [salta LLM: el usuario sube items]\n")
  cat("    Paso 4.  ver_items()                      [inspeccion]\n\n")

  cat("  ", .color_azul("FASE III. ANALISIS SEMANTICO"), "\n", sep = "")
  cat("    Paso 5.  obtener_embeddings()             [OpenAI text-embedding-3-small]\n")
  cat("    Paso 6.  analizar_redundancia()           [pares con sim > 0.85]\n")
  cat("    Paso 7.  efa_regularizado()               [Goretzko, 2023]\n")
  cat("    Paso 8.  precision_clasificacion(metodo='ensemble')  [Voss et al., 2026]\n\n")

  cat("  ", .color_amarillo("FASE IV. REFINAMIENTO"), "\n", sep = "")
  cat("    Paso 9.  refinar_escala(criterio = 'ensemble')\n\n")

  cat("  ", .color_azul("FASE V. EVALUACION PSICOMETRICA SIN DATOS"), "\n", sep = "")
  cat("    Paso 10. validez_contenido()              [V de Aiken con LLM]\n")
  cat("    Paso 11. auditar_redaccion_items()        [v2.0 - antes evaluar_calidad_items]\n")
  cat("    Paso 12. fiabilidad_semantica()           [Spearman-Brown]\n")
  cat("    Paso 13. discriminacion_semantica()       [unicidad por item]\n")
  cat("    Paso 14. analizar_coherencia()            [intra vs inter-dim]\n")
  cat("    Paso 15. validez_criterio_predicha()      [Fokkema et al., 2022]\n\n")

  cat("  ", .color_verde("FASE VI. ENTREGABLE FINAL"), "\n", sep = "")
  cat("    Paso 16. forma_corta(x, n_items)\n")
  cat("    Paso 17. sugerir_escala_respuesta(x)\n")
  cat("    Paso 18. ensamblar(tipo = ...)            [v2.0 dispatcher unificado]\n")
  cat("    Paso 19. exportar_escala() / guardar() / cargar()\n\n")

  cat("  ", .color_amarillo("FASE VII. ADAPTACION Y COMPARACION"), "\n", sep = "")
  cat("    Paso 20. adaptar_transcultural()          [Grobelny et al., 2025]\n")
  cat("    Paso 21. detectar_dif_semantico()         [Belzak, 2023]\n")
  cat("    Paso 22. comparar_escalas()\n\n")

  cat("  ", .color_azul("FASE VIII. VISUALIZACION"), "\n", sep = "")
  cat("    Paso 23. plot_*()                         [14 graficos disponibles]\n")
  cat("             plot_coherencia(tipo='boxplot'/'violin')   [v2.0 dispatcher]\n\n")

  cat("  ", .color_verde("FASE IX. MODOS AVANZADOS"), "\n", sep = "")
  cat("    Paso 24. banco_cat()                      [opcional, Gao et al., 2026]\n")
  cat("    Paso 25. crear_plantilla_escala() / leer_escala()\n\n")

  cat(.linea("-"), "\n")
  cat(.color_verde("FUNCION PRINCIPAL:"), " semilla() ejecuta el pipeline central (Paso 3-19)\n")
  cat(.color_verde("FLUJO MINIMO (8 pasos):"), "\n")
  cat("  cache('enable') -> generar_items() -> obtener_embeddings() ->\n")
  cat("  precision_clasificacion(metodo='ensemble') -> refinar_escala() ->\n")
  cat("  validez_contenido() -> forma_corta() -> ensamblar(tipo='likert')\n")
  cat(.linea("="), "\n\n")
}


#' @keywords internal
.flujo_grafico <- function(archivo = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Necesitas instalar ggplot2: install.packages('ggplot2')")
  }

  # 9 fases del Manual v2.0
  pasos <- data.frame(
    y = 9:1,
    label = c(
      "FASE I. ANTES DE EMPEZAR\ncache(action, path)",
      "FASE II. CONSTRUCCION\ngenerar_items(tipo = 'likert')\nver_items()",
      "FASE III. ANALISIS SEMANTICO\nobtener_embeddings()\nprecision_clasificacion(ensemble)\nefa_regularizado()",
      "FASE IV. REFINAMIENTO\nrefinar_escala(criterio = 'ensemble')",
      "FASE V. EVALUACION PSICOMETRICA\nvalidez_contenido() + auditar_redaccion()\nfiabilidad / discriminacion / coherencia\nvalidez_criterio_predicha()",
      "FASE VI. ENTREGABLE FINAL\nforma_corta() + sugerir_escala_respuesta()\nensamblar(tipo = 'likert')\nexportar_escala() + guardar()",
      "FASE VII. ADAPTACION\nadaptar_transcultural()\ndetectar_dif_semantico()\ncomparar_escalas()",
      "FASE VIII. VISUALIZACION\nplot_similitud / plot_v_aiken / plot_sankey\nplot_coherencia(tipo = ...) y 14 mas",
      "FASE IX. MODOS AVANZADOS\nbanco_cat() (opcional)\ncrear_plantilla_escala() + leer_escala()"
    ),
    fill = c(
      "#FFE0B2",  # I  - amarillo claro
      "#C8E6C9",  # II  - verde claro
      "#BBDEFB",  # III - azul claro
      "#FFE0B2",  # IV  - amarillo
      "#BBDEFB",  # V   - azul
      "#C8E6C9",  # VI  - verde
      "#FFE0B2",  # VII - amarillo
      "#BBDEFB",  # VIII- azul
      "#C8E6C9"   # IX  - verde
    ),
    stringsAsFactors = FALSE
  )

  flechas <- data.frame(
    x = 0, xend = 0,
    y    = (9:2) - 0.32,
    yend = (9:2) - 0.68
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = pasos,
      ggplot2::aes(x = 0, y = y, fill = fill),
      width = 4.5, height = 0.85, color = "gray40", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = pasos,
      ggplot2::aes(x = 0, y = y, label = label),
      size = 2.7, lineheight = 0.95
    ) +
    ggplot2::geom_segment(
      data = flechas,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
      color = "gray40", linewidth = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      plot.margin   = ggplot2::margin(8, 8, 8, 8)
    ) +
    ggplot2::labs(
      title = "SeMiLLa v2.0 - Flujo de Trabajo (9 Fases / 25 Pasos)",
      subtitle = "Manual de Usuario - SEmantic Measurement Items via LLM Assistance"
    ) +
    ggplot2::coord_fixed(ratio = 0.45)

  if (!is.null(archivo)) {
    archivo_png <- paste0(archivo, ".png")
    ggplot2::ggsave(archivo_png, p, width = 9, height = 11, dpi = 150)
    cat("  ", .color_check(), " Diagrama guardado: ", archivo_png, "\n", sep = "")
  } else {
    print(p)
  }

  invisible(p)
}


#' @title Resumen de Funciones SeMiLLa
#'
#' @description
#' Muestra un resumen de todas las funciones disponibles.
#'
#' @export
#' @noRd
ayuda <- function() {

  cat("\n")
  cat(.linea("="), "\n")
  cat(.color_verde("SeMiLLa - FUNCIONES DISPONIBLES"), "\n")
  cat(.linea("="), "\n\n")

  cat(.color_azul("FUNCION PRINCIPAL:"), "\n")
  cat("  semilla()            Pipeline completo: concepto -> escala validada\n\n")

  cat(.color_azul("CONCEPTUALIZACION (Item Development):"), "\n")
  cat("  generar_escala()     Genera items desde un constructo psicologico\n")
  cat("  ver_items()          Muestra items como dataframe (factor, item)\n\n")

  cat(.color_azul("REPRESENTACION (Semantic Representation):"), "\n")
  cat("  obtener_embeddings() Calcula embeddings semanticos via OpenAI\n")
  cat("  items_similares()    Encuentra items similares a uno dado\n")
  cat("  analizar_redundancia() Detecta pares de items redundantes\n\n")

  cat(.color_azul("ESTRUCTURA (Clustering Semantico):"), "\n")
  cat("  precision_clasificacion() Clustering y comparacion con teoria\n")
  cat("  refinar_escala()     Refinamiento iterativo de items\n\n")

  cat(.color_azul("EVALUACION (Validity & Reliability):"), "\n")
  cat("  validez_contenido()  Evalua validez de contenido via LLM (CVI)\n")
  cat("  fiabilidad_semantica() Calcula Alpha Semantico (Spearman-Brown)\n\n")

  cat(.color_azul("INTEGRACION (Scale Integration):"), "\n")
  cat("  exportar_escala()    Exporta items a Excel + archivo de info\n")
  cat("  guardar()            Guarda objeto completo (.rds)\n")
  cat("  cargar()             Carga objeto guardado\n\n")

  cat(.color_azul("UTILIDADES:"), "\n")
  cat("  flujo()              Muestra el flujo de trabajo\n")
  cat("  ayuda()              Esta ayuda\n\n")

  cat(.linea("="), "\n")

  cat(.color_azul("REFERENCIA METODOLOGICA:"), "\n")
  cat("  Ferrando, P.J., Morales-Vives, F., Casas, J.M., & Muniz, J. (2025).\n")
  cat("  Likert scales: A practical guide. Psicothema, 37(4), 1-15.\n\n")

  cat("Usa ?nombre_funcion para ver la documentacion completa\n")
  cat(.linea("="), "\n\n")

  invisible(NULL)
}


