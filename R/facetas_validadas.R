# =============================================================================
# SeMiLLa - Biblioteca de FACETAS VALIDADAS para constructos clinicos
#
# Cuando el usuario pide items con `items_modo = "por_historia"` y selecciona
# nombres de facetas que coinciden con esta biblioteca, SeMiLLa enriquece el
# prompt al LLM con:
#   (a) la definicion operacional anclada en literatura
#   (b) la cita del o los instrumentos de los que proviene la faceta
#   (c) ejemplos de items basados en los instrumentos originales
#
# Esto permite generar escalas con anclaje empirico (no facetas ad hoc),
# siguiendo el flujo metodologico:
#   1. Revisar instrumentos validados del constructo
#   2. Extraer dimensiones compartidas como antecedentes (no conducta)
#   3. Definir facetas segun ese mapa empirico
#
# Estructura: lista anidada por DOMINIO -> FACETA -> {definicion, fuentes,
# instrumentos, ejemplos, persona_recomendada, polaridad}.
#
# Autor: Dr. Jose Ventura-Leon
# Fecha: 2026-05-02
# =============================================================================

#' @keywords internal
.facetas_validadas_NSSI_propension <- function() {
  list(

    "urgencia_anticipatoria" = list(
      definicion = paste(
        "Frecuencia, intensidad, intrusividad y dificultad para resistir",
        "impulsos breves de hacerse dano fisico, en personas que aun NO",
        "han ejecutado la conducta. Es el unico antecedente PROSPECTIVO",
        "PURO de NSSI (mide el impulso PRE-acto, no funciones autoatribuidas",
        "retrospectivamente). FRAMING PROYECTIVO OBLIGATORIO: el item DEBE",
        "anclarse en el contenido de la historia que el respondiente acaba",
        "de leer ('Yo tambien he sentido eso de...', 'A mi tambien me ha",
        "pasado lo que vive Lucia cuando...', 'Si yo estuviera como Lucia,",
        "podria sentir el impulso de...'). NO uso de items free-standing",
        "tipo 'A veces siento impulsos repentinos' sin referencia a la",
        "historia."
      ),
      instrumento_principal = "ABUSI - Alexian Brothers Urge to Self-Injure Scale (5 items unidim.)",
      fuentes = paste(
        "Washburn, J. J., Juzwin, K. R., Styer, D. M., & Aldridge, D.",
        "(2010). Measuring the urge to self-injure: Preliminary data from",
        "a clinical sample. Psychiatry Research, 178(3), 540-544."
      ),
      ejemplos_originales = c(
        "Yo tambien he sentido eso de tener impulsos breves como los que vive Lucia",
        "A mi tambien me ha pasado lo que siente Lucia, esas ganas que vienen y se van",
        "Si yo estuviera como Lucia en esta situacion, podria sentir el impulso de hacerme dano"
      ),
      persona_recomendada = "1a persona",
      polaridad = "directa",
      n_factor_origen = 1L
    ),

    "expectativa_alivio" = list(
      definicion = paste(
        "Creencia de que el dolor fisico autoinfligido aliviaria estados",
        "emocionales aversivos (tristeza, rabia, vacio, ansiedad). Es la",
        "expectativa cognitiva con mayor saturacion empirica (4 de 6",
        "instrumentos NSSI revisados convergen en esta dimension).",
        "FRAMING PROYECTIVO OBLIGATORIO: el item DEBE conectar con la",
        "historia leida ('Yo tambien he creido eso de que el dolor fisico",
        "aliviaria...', 'A mi tambien se me ha cruzado la idea de que",
        "lastimarme calmaria...'). NO usar items descriptivos sobre Lucia",
        "que solo repiten el contenido de la historia."
      ),
      instrumento_principal = paste(
        "NEQ - NSSI Expectancy Questionnaire (Hasking & Boyes, 2018,",
        "factor 'Affect regulation') + ISAS (Klonsky & Glenn, 2009,",
        "funcion 'affect regulation') + OSI (Nixon et al., 2015,",
        "'regulacion emocional interna') + FASM (Lloyd-Richardson et al.,",
        "2007, 'refuerzo automatico negativo')"
      ),
      fuentes = paste(
        "Hasking, P., & Boyes, M. (2018). The Non-Suicidal Self-Injury",
        "Expectancy Questionnaire: Factor structure and initial validation.",
        "Clinical Psychologist, 22(2), 251-261."
      ),
      ejemplos_originales = c(
        "Yo tambien he creido eso de que el dolor fisico podria calmar lo que siente Lucia",
        "A mi tambien se me ha pasado por la cabeza que lastimarse aliviaria emociones como las suyas",
        "Lo que cree Lucia sobre el alivio fisico, yo tambien lo he pensado en momentos asi"
      ),
      persona_recomendada = "mixta",
      polaridad = "directa",
      n_factor_origen = 4L
    ),

    "expectativa_autocastigo_corporal" = list(
      definicion = paste(
        "Creencia de que el cuerpo merece sufrir o ser castigado fisicamente",
        "como pago por errores, fallas o defectos morales. Es mas especifica",
        "que la autocritica general: el sufrimiento debe ser CORPORAL (no",
        "solo emocional o psicologico). Apoyada en 2 de 6 instrumentos.",
        "FRAMING 3a PERSONA OBLIGATORIO: el item DEBE juzgar la situacion",
        "de Lucia desde fuera, NUNCA en 1a persona. El respondiente",
        "proyecta sus propias cogniciones sobre Lucia sin auto-revelar.",
        "Patrones permitidos: 'Es comprensible que Lucia [...]', 'Lo que",
        "piensa Lucia sobre castigarse fisicamente refleja [...]', 'Tiene",
        "logica que Lucia crea que [...]', 'La situacion de Lucia muestra",
        "que [...]'."
      ),
      instrumento_principal = paste(
        "ISAS (Klonsky & Glenn, 2009, funcion 'self-punishment') + NEQ",
        "(Hasking & Boyes, 2018, factor 'Negative self-beliefs')"
      ),
      fuentes = paste(
        "Klonsky, E. D., & Glenn, C. R. (2009). Assessing the functions",
        "of non-suicidal self-injury: Psychometric properties of the",
        "Inventory of Statements About Self-Injury (ISAS). Journal of",
        "Psychopathology and Behavioral Assessment, 31(3), 215-219."
      ),
      ejemplos_originales = c(
        "Es comprensible que Lucia crea que su cuerpo deberia pagar por sus errores",
        "Lo que piensa Lucia sobre castigarse fisicamente refleja un dolor real que merece atencion",
        "Tiene logica que Lucia sienta que su cuerpo debe sufrir cuando ella falla"
      ),
      persona_recomendada = "3a persona",
      polaridad = "directa",
      n_factor_origen = 2L
    ),

    "expectativa_comunicacion_interpersonal" = list(
      definicion = paste(
        "Creencia de que el dano fisico autoinfligido serviria para expresar",
        "dolor a otros, pedir ayuda indirectamente, o cambiar la respuesta",
        "de los demas (familia, pares, pareja). Maxima saturacion empirica",
        "(4 de 6 instrumentos). FRAMING 3a PERSONA OBLIGATORIO: el item DEBE",
        "juzgar la situacion de Lucia desde fuera, NUNCA en 1a persona.",
        "Patrones permitidos: 'Es comprensible que Lucia [...]', 'Lo que",
        "piensa Lucia sobre lastimarse [...] tiene sentido [...]', 'Para",
        "Lucia, mostrar marcas seria [...]'."
      ),
      instrumento_principal = paste(
        "NEQ (Hasking & Boyes, 2018, factor 'Communication') + ISAS",
        "(Klonsky & Glenn, 2009, funcion 'interpersonal influence') + OSI",
        "(Nixon et al., 2015, 'influencia social') + FASM (Lloyd-Richardson",
        "et al., 2007, 'refuerzo social positivo')"
      ),
      fuentes = paste(
        "Hasking, P., & Boyes, M. (2018). The Non-Suicidal Self-Injury",
        "Expectancy Questionnaire. Clinical Psychologist, 22(2), 251-261."
      ),
      ejemplos_originales = c(
        "Es comprensible que Lucia crea que lastimarse haria que otros vean lo que siente",
        "Para Lucia, el dolor fisico tendria sentido como una forma de pedir ayuda sin palabras",
        "Lo que piensa Lucia sobre mostrar su sufrimiento con marcas refleja un intento de comunicar su dolor"
      ),
      persona_recomendada = "3a persona",
      polaridad = "directa",
      n_factor_origen = 4L
    )

  )
}


#' @title Sugerir facetas validadas para un constructo
#'
#' @description
#' Devuelve la biblioteca de facetas con anclaje empirico para un constructo
#' clinico. Actualmente soporta: NSSI propension. Cada faceta incluye
#' definicion operacional, instrumento(s) de origen, cita APA, ejemplos y
#' recomendacion de persona (1a / 3a / mixta).
#'
#' @param constructo Nombre del constructo. Actualmente: "NSSI_propension".
#' @param verbose Mostrar tabla resumen.
#'
#' @return Lista nombrada con metadatos de cada faceta.
#' @export
sugerir_facetas <- function(constructo = "NSSI_propension", verbose = TRUE) {
  facetas <- switch(constructo,
    "NSSI_propension" = .facetas_validadas_NSSI_propension(),
    "nssi_propension" = .facetas_validadas_NSSI_propension(),
    NULL
  )

  if (is.null(facetas)) {
    stop("Constructo no soportado: ", constructo,
         ". Disponibles: 'NSSI_propension'.")
  }

  if (isTRUE(verbose)) {
    cat("\n=== FACETAS VALIDADAS PARA: ", toupper(constructo), " ===\n", sep = "")
    cat(rep("-", 70), "\n", sep = "")
    for (nm in names(facetas)) {
      f <- facetas[[nm]]
      cat("\n[", nm, "]\n", sep = "")
      cat("  Persona recomendada: ", f$persona_recomendada,
          "  | Polaridad: ", f$polaridad, "\n", sep = "")
      cat("  Instrumento(s):      ", f$instrumento_principal, "\n", sep = "")
      cat("  Definicion:          ",
          substr(f$definicion, 1, 120), "...\n", sep = "")
    }
    cat("\n")
  }

  invisible(facetas)
}


#' @keywords internal
.faceta_validada_a_prompt <- function(nombre_faceta, victima) {
  todas <- .facetas_validadas_NSSI_propension()
  f <- todas[[tolower(nombre_faceta)]]
  if (is.null(f)) return(NULL)

  ejemplos_txt <- paste0(seq_along(f$ejemplos_originales), ". ",
                          f$ejemplos_originales, collapse = "\n")
  # Sustituir "Lucia" por el nombre real de la victima en los ejemplos
  ejemplos_txt <- gsub("Lucia", victima, ejemplos_txt, fixed = TRUE)

  list(
    definicion = f$definicion,
    instrumento = f$instrumento_principal,
    fuentes = f$fuentes,
    ejemplos = ejemplos_txt,
    persona = f$persona_recomendada,
    polaridad = f$polaridad
  )
}
