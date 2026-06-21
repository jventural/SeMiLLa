# =============================================================================
# SeMiLLa - Control de lenguaje por etapa evolutiva y nivel socioeconomico
#
# Helper interno que devuelve un bloque de instrucciones para el LLM,
# adaptando el VOCABULARIO, la complejidad sintactica y el registro del
# texto generado segun:
#   - etapa_evolutiva (ninez, adolescencia_temprana, ...)
#   - nivel_socioeconomico (alto, medio, bajo, medio_bajo)
#
# Usado por generar_escala_historias() y prompts_historieta() para evitar
# que el LLM produzca textos con lexico literario o academico inadecuado
# para la poblacion lectora real.
#
# Autor: Dr. Jose Ventura-Leon
# Fecha: 2026-05-03
# =============================================================================


#' @title Bloque de restricciones lexicas segun etapa evolutiva y NSE
#'
#' @description
#' Construye un bloque de instrucciones para el prompt del LLM, restringiendo
#' el vocabulario y la sintaxis al nivel de comprension lectora real de la
#' poblacion objetivo. Util en escalas dirigidas a niños o adolescentes,
#' especialmente de NSE medio o bajo, donde los modelos LLM tienden a
#' generar textos con lexico literario inadecuado.
#'
#' @param etapa_evolutiva Cadena: `"ninez"` (6-11), `"adolescencia_temprana"`
#'   (12-14), `"adolescencia_media"` (15-17), `"adolescencia_tardia"`
#'   (18-21), `"adultez_emergente"` (22-29), `"adultez"` (30-59),
#'   `"adulto_mayor"` (60+), o `"auto"` (sin restriccion).
#' @param nivel_socioeconomico Cadena: `"alto"`, `"medio"`, `"medio_bajo"`,
#'   `"bajo"`, o `"auto"` (sin restriccion).
#' @param idioma `"es"`, `"en"` o `"pt"`.
#' @param variante_regional Variante de castellano para ajustar registro y
#'   vocabulario regional: `"auto"` (default, sin ajuste),
#'   `"selva_peru"` (alias: `"selva"`, `"amazonia_peru"`, `"amazonico"`) para
#'   castellano amazonico simple (lector adolescente a menudo bilingue), o una
#'   cadena libre que se inyecta como guia de registro.
#'
#' @return Cadena con un bloque de texto listo para inyectar en sys_msg.
#'   Devuelve cadena vacia si ambos parametros son `"auto"`.
#'
#' @export
contexto_lenguaje <- function(etapa_evolutiva = "auto",
                              nivel_socioeconomico = "auto",
                              idioma = "es",
                              variante_regional = "auto") {

  # Si todo es auto, no inyectar nada
  if (etapa_evolutiva == "auto" && nivel_socioeconomico == "auto" &&
      identical(variante_regional, "auto")) {
    return("")
  }

  # ---- Bloque por etapa evolutiva (idioma es) ----
  bloque_etapa_es <- switch(etapa_evolutiva,
    "ninez" = paste(
      "ETAPA EVOLUTIVA: NI\u00D1EZ (6-11 a\u00F1os).",
      "- Oraciones cortas (8-12 palabras maximo).",
      "- Vocabulario concreto, palabras que un ni\u00F1o usa en casa o en la",
      "  escuela primaria.",
      "- EVITAR palabras abstractas: 'desregulacion', 'angustia', 'rumiar',",
      "  'intensificar', 'absorber', 'invadir', 'envolver', 'fugazmente'.",
      "- USAR: 'me siento mal', 'me da rabia', 'no s\u00E9 qu\u00E9 hacer', 'me",
      "  asust\u00E9', 'me puse triste', 'me doli\u00F3'.",
      "- Tiempos verbales simples (presente, pasado simple).",
      "- Sin metaforas literarias."
    ),
    "adolescencia_temprana" = paste(
      "ETAPA EVOLUTIVA: ADOLESCENCIA TEMPRANA (12-14 a\u00F1os).",
      "- Oraciones cortas a medianas (12-16 palabras maximo).",
      "- Vocabulario cotidiano de un adolescente de secundaria.",
      "- EVITAR lexico academico o literario: 'fugazmente', 'mezclada',",
      "  'absorber', 'invadir', 'envolver', 'abrumar', 'intensificar',",
      "  'consternada', 'incapaz', 'compungida', 'tormento'.",
      "- USAR registro coloquial: 'rapido', 'lleno', 'fuerte por dentro',",
      "  'no podia decir nada', 'me sentia mal', 'tenia rabia', 'estaba",
      "  cansada'.",
      "- Pensamientos en comillas, simples y directos: '\u00BFPor que a mi?',",
      "  'No puedo mas', 'Algo esta mal en mi'.",
      "- Sin metaforas elaboradas ('tormenta interna', 'alma rota')."
    ),
    "adolescencia_media" = paste(
      "ETAPA EVOLUTIVA: ADOLESCENCIA MEDIA (15-17 a\u00F1os).",
      "- Oraciones medianas (15-20 palabras maximo).",
      "- Vocabulario adolescente de secundaria final, con algo mas de",
      "  abstraccion permitida.",
      "- EVITAR lexico literario poco usado: 'fugazmente', 'consternada',",
      "  'tormento', 'compungida'.",
      "- ACEPTABLE: 'angustia', 'ansiedad', 'soledad', 'culpa' (palabras",
      "  emocionales de uso adolescente comun).",
      "- Pensamientos en comillas con frases coloquiales."
    ),
    "adolescencia_tardia" = paste(
      "ETAPA EVOLUTIVA: ADOLESCENCIA TARDIA / JOVENES ADULTOS (18-21 a\u00F1os).",
      "- Vocabulario emocional permitido (angustia, ansiedad, vacio,",
      "  desregulacion).",
      "- EVITAR solo lexico arcaico o muy literario."
    ),
    "adultez_emergente" = "ETAPA EVOLUTIVA: ADULTEZ EMERGENTE (22-29). Vocabulario adulto general.",
    "adultez"           = "ETAPA EVOLUTIVA: ADULTEZ (30-59). Vocabulario adulto general.",
    "adulto_mayor"      = paste(
      "ETAPA EVOLUTIVA: ADULTO MAYOR (60+).",
      "- Oraciones claras, sin neologismos ni jerga juvenil.",
      "- Evitar referencias a redes sociales sin contexto."
    ),
    "auto"              = "",
    ""
  )

  # ---- Bloque por NSE (idioma es) ----
  bloque_nse_es <- switch(nivel_socioeconomico,
    "bajo" = paste(
      "NIVEL SOCIOECONOMICO: BAJO.",
      "- Lenguaje muy directo y concreto, registro popular.",
      "- EVITAR cultismos, latinismos, lexico cientifico.",
      "- Imaginar a un lector con escolaridad incompleta o lectura limitada.",
      "- Contextos cotidianos (casa peque\u00F1a, barrio, escuela publica)."
    ),
    "medio_bajo" = paste(
      "NIVEL SOCIOECONOMICO: MEDIO-BAJO.",
      "- Lenguaje cotidiano, accesible a un lector de educacion basica.",
      "- EVITAR cultismos: 'fugazmente', 'intensificar', 'consternada',",
      "  'tormento'.",
      "- Contextos cotidianos: barrio de clase trabajadora, colegio publico,",
      "  familia con recursos limitados, transporte publico, etc."
    ),
    "medio" = paste(
      "NIVEL SOCIOECONOMICO: MEDIO.",
      "- Lenguaje estandar, accesible a lector con secundaria completa.",
      "- Permitido vocabulario emocional y abstracto comun."
    ),
    "alto" = "NIVEL SOCIOECONOMICO: ALTO. Lenguaje estandar sin restricciones especiales.",
    "auto" = "",
    ""
  )

  # ---- Bloque por VARIANTE REGIONAL (idioma es) ----
  vr <- tolower(trimws(variante_regional))
  bloque_region_es <- if (vr %in% c("selva_peru", "selva", "amazonia_peru",
                                     "amazonia", "amazonico")) {
    paste(
      "VARIANTE REGIONAL: CASTELLANO AMAZONICO (SELVA DEL PERU).",
      "- Lector tipico: adolescente de un colegio o comunidad de la selva",
      "  peruana, muchas veces BILINGUE (castellano como segunda lengua).",
      "- Castellano MUY simple, oral y concreto. UNA sola idea por oracion.",
      "- Oraciones CORTAS pero COMPLETAS y CLARAS (idealmente 12-16 palabras):",
      "  NUNCA recortes articulos, preposiciones ni conectores, ni dejes frases",
      "  telegraficas o entrecortadas ('perder control emocional', 'no calmarme",
      "  facil'); escribe 'perder el control de mis emociones', 'no calmarme",
      "  facilmente'. Si una palabra mas hace la frase mas clara, agregala: la",
      "  CLARIDAD a la primera lectura tiene PRIORIDAD sobre la brevedad.",
      "- PROHIBIDO encadenar muchas subordinadas o usar dobles condicionales",
      "  ('si... entonces si...', 'aunque... podria... cuando...'). Si hace",
      "  falta, partir en una frase mas simple, pero siempre gramaticalmente",
      "  completa.",
      "- EVITAR palabras cultas o abstractas: 'angustia', 'ansiedad',",
      "  'desconectado', 'saldar', 'expiar', 'reparar', 'vulnerabilidad',",
      "  'desbordar(se)', 'tentacion', 'fallas', 'autocastigo', 'indiferencia'.",
      "- USAR palabras concretas de uso diario: 'hacerme dano', 'lastimarme',",
      "  'me siento mal', 'me da rabia', 'nervios', 'miedo', 'estoy triste',",
      "  'me cuesta calmarme', 'me dan ganas de...', 'pienso que...', 'me siento solo'.",
      "- EVITAR jerga urbana limeña y modismos costeños.",
      "- El sentido del item debe entenderse a la PRIMERA lectura, en voz alta.",
      "- PRUEBA: si un adolescente de la selva con castellano basico tuviera que",
      "  releer la frase o preguntar que significa una palabra, simplificarla."
    )
  } else if (!identical(variante_regional, "auto") && nzchar(vr)) {
    # variante libre: inyectar el texto tal cual como guia de registro
    paste0("VARIANTE REGIONAL / REGISTRO: ", variante_regional)
  } else ""

  # ---- Concatenar ----
  partes <- c(
    "RESTRICCIONES DE LENGUAJE (basadas en la poblacion lectora):",
    bloque_etapa_es,
    bloque_nse_es,
    bloque_region_es,
    "",
    "CRITERIO DE PRUEBA: cualquier frase debe ser comprensible y CLARA a la",
    "primera lectura para un estudiante de la edad, NSE y variante indicados,",
    "leida en voz alta sin pausas para buscar significado de palabras."
  )
  bloque <- paste(partes[nzchar(partes)], collapse = "\n")

  # Idiomas en/pt: traduccion basica (placeholder)
  if (idioma == "en") {
    bloque <- gsub("EVITAR", "AVOID", bloque)
    bloque <- gsub("USAR", "USE", bloque)
    # No traduccion completa: el grueso del prompt sigue siendo en en
  }

  bloque
}


#' @keywords internal
.lista_palabras_evitar_adolescente <- function() {
  # Lista de palabras frecuentes en LLM espanol que NO usa un adolescente
  c("fugazmente", "fugaz", "intensificar", "intensificarse", "absorber",
    "absorbida", "invadir", "envolver", "envolverla", "abrumar", "abrumada",
    "consternada", "compungida", "tormento", "atormentada", "incapaz",
    "mezclada", "mezcla turbia", "denso silencio", "hondo silencio",
    "trasluciendo", "rumiacion", "rumiar", "subyacente", "soterrada",
    "vehemencia", "indolencia", "padecer", "padecimiento", "tropel",
    "perplejidad", "perpleja", "alma rota", "tormenta interna",
    "tormenta emocional", "vacio profundo", "vacio insondable",
    "abismo interior")
}
