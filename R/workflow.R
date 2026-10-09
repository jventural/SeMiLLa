#' @title Visualizar Flujo de Trabajo SeMiLLa v2.0
#'
#' @description
#' Muestra el flujo de trabajo del paquete (Manual de Usuario v2.0,
#' organizado en 9 fases y 25 pasos). El flujo refleja exactamente el
#' orden recomendado de uso de las funciones, desde la configuracion
#' inicial hasta el entregable final administrable.
#'
#' @param tipo \code{"texto"} (por defecto: el flujo en ASCII, emitido como
#'   mensaje) o \code{"grafico"} (diagrama con ggplot2).
#' @param archivo Solo con \code{tipo = "grafico"}: ruta del archivo PNG
#'   \strong{sin} extension. Si es \code{NULL} (por defecto), el diagrama se
#'   dibuja en el dispositivo grafico activo y no se escribe nada en disco.
#'
#' @return \code{NULL} de forma invisible; se llama por su efecto (mostrar el
#'   flujo como mensaje, dibujar el diagrama o guardarlo en \code{archivo}).
#'
#' @examples
#' flujo()
#' f <- file.path(tempdir(), "flujo_semilla")
#' flujo(tipo = "grafico", archivo = f)
#' unlink(paste0(f, ".png"))
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

  # El texto se acumula y se emite como un unico message() (suprimible con
  # suppressMessages()), en lugar de escribirse con cat() en la consola.
  out <- character()
  emit <- function(..., sep = " ") out <<- c(out, paste(..., sep = sep))
  on.exit(message(paste(out, collapse = ""), appendLF = FALSE), add = TRUE)

  emit("\n")
  emit(.linea("="), "\n")
  emit(.color_verde("FLUJO DE TRABAJO SeMiLLa v2.0 (Manual de Usuario)"), "\n")
  emit(.linea("="), "\n\n")

  emit("  ", .color_amarillo("FASE I. ANTES DE EMPEZAR"), "\n", sep = "")
  emit("    Paso 1.  Instalacion y configuracion\n")
  emit("    Paso 2.  cache(action, path)              [cache de llamadas LLM]\n\n")

  emit("  ", .color_verde("FASE II. CONSTRUCCION DE LA ESCALA"), "\n", sep = "")
  emit("    Paso 3.  generar_items(tipo = ...)        [likert/historias/guttman/...]\n")
  emit("    Paso 3b. semilla(fuente = 'usuario')      [salta LLM: el usuario sube items]\n")
  emit("    Paso 4.  ver_items()                      [inspeccion]\n\n")

  emit("  ", .color_azul("FASE III. ANALISIS SEMANTICO"), "\n", sep = "")
  emit("    Paso 5.  obtener_embeddings()             [OpenAI text-embedding-3-small]\n")
  emit("    Paso 6.  analizar_redundancia()           [pares con sim > 0.70]\n")
  emit("    Paso 7.  precision_clasificacion(metodo='ensemble')  [Voss et al., 2026]\n")
  emit("    Paso 8.  (retirado en 2.10.0: efa_regularizado() esta obsoleta)\n\n")

  emit("  ", .color_amarillo("FASE IV. REFINAMIENTO"), "\n", sep = "")
  emit("    Paso 9.  refinar_escala(criterio = 'ensemble')\n\n")

  emit("  ", .color_azul("FASE V. EVALUACION PSICOMETRICA SIN DATOS"), "\n", sep = "")
  emit("    Paso 10. validez_contenido()              [V de Aiken con LLM]\n")
  emit("    Paso 11. auditar_redaccion_items()        [v2.0 - antes evaluar_calidad_items]\n")
  emit("    Paso 12. fiabilidad_semantica()           [Spearman-Brown]\n")
  emit("    Paso 13. discriminacion_semantica()       [unicidad por item]\n")
  emit("    Paso 14. analizar_coherencia()            [intra vs inter-dim]\n")
  emit("    Paso 15. validez_criterio_predicha()      [Fokkema et al., 2022]\n\n")

  emit("  ", .color_amarillo("FASE V-B. COMPUERTA PRE-APLICACION (obligatoria antes de campo)"), "\n", sep = "")
  emit("    Paso 15b. compuerta_pre_aplicacion()      [redaccion + deseabilidad + simulacion]\n")
  emit("     Escenario previsto: LISTA PARA CAMPO / APLICAR CON CAUTELA / NO APLICAR TODAVIA\n")
  emit("              semilla() la ejecuta automaticamente (compuerta = TRUE)\n")
  emit("    Paso 15c. optimizar_para_campo()          [correccion automatica guiada]\n")
  emit("              Poda facetas/pares -> regenera anti-halo -> re-pasa la compuerta\n")
  emit("              semilla() la dispara si el veredicto es NO APLICAR (optimizar = TRUE)\n\n")

  emit("  ", .color_verde("FASE VI. ENTREGABLE FINAL"), "\n", sep = "")
  emit("    Paso 16. forma_corta(x, n_items)\n")
  emit("    Paso 17. sugerir_escala_respuesta(x)\n")
  emit("    Paso 18. ensamblar(tipo = ...)            [v2.0 dispatcher unificado]\n")
  emit("    Paso 19. exportar_escala() / guardar() / cargar()\n\n")

  emit("  ", .color_amarillo("FASE VII. ADAPTACION Y COMPARACION"), "\n", sep = "")
  emit("    Paso 20. adaptar_transcultural()          [Grobelny et al., 2025]\n")
  emit("    Paso 21. detectar_dif_semantico()         [Belzak, 2023]\n")
  emit("    Paso 22. comparar_escalas()\n\n")

  emit("  ", .color_azul("FASE VIII. VISUALIZACION"), "\n", sep = "")
  emit("    Paso 23. plot_*()                         [14 graficos disponibles]\n")
  emit("             plot_coherencia(tipo='boxplot'/'violin')   [v2.0 dispatcher]\n\n")

  emit("  ", .color_verde("FASE IX. MODOS AVANZADOS"), "\n", sep = "")
  emit("    Paso 24. banco_cat()                      [opcional, Gao et al., 2026]\n")
  emit("    Paso 25. crear_plantilla_escala() / leer_escala()\n\n")

  emit(.linea("-"), "\n")
  emit(.color_verde("FUNCION PRINCIPAL:"), " semilla() ejecuta el pipeline central (Paso 3-19),\n")
  emit("  incluida la COMPUERTA PRE-APLICACION (Paso 15b) al cierre.\n")
  emit(.color_verde("FLUJO MINIMO (9 pasos):"), "\n")
  emit("  cache('enable') -> generar_items() -> obtener_embeddings() ->\n")
  emit("  precision_clasificacion(metodo='ensemble') -> refinar_escala() ->\n")
  emit("  validez_contenido() -> compuerta_pre_aplicacion() -> forma_corta() ->\n")
  emit("  ensamblar(tipo='likert')\n")
  emit(.linea("="), "\n\n")
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
      "FASE III. ANALISIS SEMANTICO\nobtener_embeddings()\nprecision_clasificacion(ensemble)",
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
    message("  ", .color_check(), " Diagrama guardado: ", archivo_png)
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
#' @noRd
ayuda <- function() {

  out <- character()
  emit <- function(..., sep = " ") out <<- c(out, paste(..., sep = sep))
  on.exit(message(paste(out, collapse = ""), appendLF = FALSE), add = TRUE)

  emit("\n")
  emit(.linea("="), "\n")
  emit(.color_verde("SeMiLLa - FUNCIONES DISPONIBLES"), "\n")
  emit(.linea("="), "\n\n")

  emit(.color_azul("FUNCION PRINCIPAL:"), "\n")
  emit("  semilla()            Pipeline completo: concepto -> escala validada\n\n")

  emit(.color_azul("CONCEPTUALIZACION (Item Development):"), "\n")
  emit("  generar_escala()     Genera items desde un constructo psicologico\n")
  emit("  ver_items()          Muestra items como dataframe (factor, item)\n\n")

  emit(.color_azul("REPRESENTACION (Semantic Representation):"), "\n")
  emit("  obtener_embeddings() Calcula embeddings semanticos via OpenAI\n")
  emit("  items_similares()    Encuentra items similares a uno dado\n")
  emit("  analizar_redundancia() Detecta pares de items redundantes\n\n")

  emit(.color_azul("ESTRUCTURA (Clustering Semantico):"), "\n")
  emit("  precision_clasificacion() Clustering y comparacion con teoria\n")
  emit("  refinar_escala()     Refinamiento iterativo de items\n\n")

  emit(.color_azul("EVALUACION (Validity & Reliability):"), "\n")
  emit("  validez_contenido()  Evalua validez de contenido via LLM (CVI)\n")
  emit("  fiabilidad_semantica() Calcula Alpha Semantico (Spearman-Brown)\n")
  emit("  compuerta_pre_aplicacion() Auditoria integral antes de ir a campo\n\n")

  emit(.color_azul("INTEGRACION (Scale Integration):"), "\n")
  emit("  exportar_escala()    Exporta items a Excel + archivo de info\n")
  emit("  guardar()            Guarda objeto completo (.rds)\n")
  emit("  cargar()             Carga objeto guardado\n\n")

  emit(.color_azul("UTILIDADES:"), "\n")
  emit("  flujo()              Muestra el flujo de trabajo\n")
  emit("  ayuda()              Esta ayuda\n\n")

  emit(.linea("="), "\n")

  emit(.color_azul("REFERENCIA METODOLOGICA:"), "\n")
  emit("  Ferrando, P.J., Morales-Vives, F., Casas, J.M., & Muniz, J. (2025).\n")
  emit("  Likert scales: A practical guide. Psicothema, 37(4), 1-15.\n\n")

  emit("Usa ?nombre_funcion para ver la documentacion completa\n")
  emit(.linea("="), "\n\n")

  invisible(NULL)
}


