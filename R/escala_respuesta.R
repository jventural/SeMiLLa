#' @title Sugerir escala de respuesta optima
#'
#' @description
#' Recomienda el formato de respuesta mas apropiado para una escala (tipo,
#' numero de puntos, polaridad, anclajes verbales y punto neutral) basandose
#' en el contenido semantico de los items. Implementa dos estrategias:
#'
#' \itemize{
#'   \item \strong{heuristica}: analisis lingueistico por conteo de marcadores
#'         (verbos de frecuencia, intensidad, acuerdo, preferencia). Rapida,
#'         sin costo, reproducible.
#'   \item \strong{llm}: consulta a un modelo de lenguaje que lee el constructo,
#'         la poblacion y los items para recomendar el formato, con
#'         justificacion teorica.
#' }
#'
#' La version 2 incluye detecion automatica de contextos sensibles
#' (parental, clinico, adicciones) y adapta los anclajes para evitar
#' efectos piso/techo por deseabilidad social (Krosnick, 1999;
#' Weijters et al., 2010).
#'
#' @param x Objeto semilla, semilla_items, o dataframe con columnas
#'   'item' y 'dimension'.
#' @param metodo Estrategia: "heuristica" (default) o "llm".
#' @param api_key API key de OpenAI (requerida solo para metodo = "llm").
#' @param modelo Modelo LLM. Default "gpt-4.1-mini".
#' @param idioma Idioma de los anclajes: "es" (default), "en", "pt".
#' @param n_puntos Forzar un numero de puntos (4, 5, 6, 7). NULL = automatico.
#' @param contexto Dominio: "auto" (default), "parental", "clinico",
#'   "adicciones", "educativo", "laboral" o "general". Afecta tanto a la
#'   heuristica como a las instrucciones que recibe el LLM.
#' @param evitar_absolutos Logico. Si TRUE (default cuando el contexto es
#'   sensible), sustituye "Nunca"/"Siempre" por cuantificadores relativos
#'   ("Casi nunca"/"Casi siempre", "En pocas ocasiones"/"En la mayoria de las
#'   ocasiones") para reducir efectos piso/techo.
#' @param anclajes_contextuales Logico. Solo aplica a metodo = "llm". Si TRUE
#'   (default), pide al LLM que genere anclajes especificos al dominio en
#'   lugar de usar plantillas genericas.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_escala_respuesta} con:
#' \itemize{
#'   \item \code{tipo_escala}, \code{n_puntos}, \code{polaridad},
#'         \code{punto_neutral}, \code{anclajes}, \code{justificacion},
#'         \code{alternativas}
#'   \item \code{contexto}: dominio detectado
#'   \item \code{deseabilidad_social}: puntaje 0-1 estimado por contenido
#'   \item \code{evitar_absolutos}: decision final
#'   \item \code{diagnostico}: conteos y proporciones por tipo (heuristica)
#'   \item \code{metodo}: metodo empleado
#' }
#'
#' @examples
#' \dontrun{
#' # Heuristica con deteccion automatica de contexto
#' esc <- sugerir_escala_respuesta(mi_escala)
#'
#' # Forzar dominio parental (evita absolutos)
#' esc <- sugerir_escala_respuesta(mi_escala, contexto = "parental")
#'
#' # LLM con anclajes a medida del dominio
#' esc <- sugerir_escala_respuesta(
#'   mi_escala,
#'   metodo  = "llm",
#'   api_key = Sys.getenv("OPENAI_API_KEY"),
#'   contexto = "parental"
#' )
#' }
#'
#' @references
#' Ferrando, P.J., Morales-Vives, F., Casas, J.M., & Muniz, J. (2025).
#' Likert scales: A practical guide. Psicothema, 37(4), 1-15.
#'
#' Krosnick, J.A. (1999). Survey research. Annual Review of Psychology, 50,
#' 537-567.
#'
#' Weijters, B., Cabooter, E., & Schillewaert, N. (2010). The effect of rating
#' scale format on response styles. International Journal of Research in
#' Marketing, 27(3), 236-247.
#'
#' @export
sugerir_escala_respuesta <- function(x,
                                     metodo                = c("heuristica", "llm"),
                                     api_key               = NULL,
                                     modelo                = "gpt-4.1-mini",
                                     idioma                = "es",
                                     n_puntos              = NULL,
                                     contexto              = "auto",
                                     evitar_absolutos      = NULL,
                                     anclajes_contextuales = TRUE,
                                     verbose               = TRUE) {

  metodo   <- match.arg(metodo)
  contexto <- match.arg(
    contexto,
    c("auto", "parental", "clinico", "adicciones",
      "educativo", "laboral", "general")
  )

  info <- .extraer_info_escala_resp(x)
  items_txt <- info$items
  poblacion <- info$poblacion
  concepto  <- info$concepto

  if (length(items_txt) == 0) {
    stop("No se encontraron items en el objeto. Verifica x$items$item o columna 'item'.")
  }

  # ----- Deteccion de contexto si es auto -----
  if (contexto == "auto") {
    contexto <- .detectar_contexto(items_txt, concepto, poblacion)
    if (verbose) cat("[contexto] detectado = '", contexto, "'\n", sep = "")
  }

  # ----- Estimacion de deseabilidad social -----
  ds_score <- .estimar_deseabilidad_social(items_txt, contexto)

  # ----- Decision evitar_absolutos -----
  if (is.null(evitar_absolutos)) {
    evitar_absolutos <- contexto %in% c("parental", "clinico", "adicciones") ||
                        ds_score > 0.35
  }

  if (verbose) {
    cat("[sugerir_escala_respuesta] ", length(items_txt),
        " items | contexto=", contexto,
        " | deseabilidad=", sprintf("%.2f", ds_score),
        " | evitar_absolutos=", evitar_absolutos,
        "\n", sep = "")
  }

  # ----- Dispatch -----
  if (metodo == "heuristica") {
    resultado <- .sugerir_heuristica(
      items_txt        = items_txt,
      idioma           = idioma,
      n_puntos         = n_puntos,
      contexto         = contexto,
      evitar_absolutos = evitar_absolutos,
      verbose          = verbose
    )
  } else {
    if (is.null(api_key) || nchar(api_key) < 20) {
      stop("metodo = 'llm' requiere una 'api_key' valida de OpenAI.")
    }
    resultado <- .sugerir_llm(
      items_txt             = items_txt,
      concepto              = concepto,
      poblacion             = poblacion,
      api_key               = api_key,
      modelo                = modelo,
      idioma                = idioma,
      n_puntos              = n_puntos,
      contexto              = contexto,
      evitar_absolutos      = evitar_absolutos,
      anclajes_contextuales = anclajes_contextuales,
      deseabilidad          = ds_score,
      verbose               = verbose
    )
  }

  resultado$metodo              <- metodo
  resultado$contexto            <- contexto
  resultado$deseabilidad_social <- round(ds_score, 3)
  resultado$evitar_absolutos    <- evitar_absolutos
  class(resultado) <- c("semilla_escala_respuesta", "list")

  if (verbose) {
    cat("[sugerir_escala_respuesta] Recomendacion: ",
        resultado$tipo_escala, " - ", resultado$n_puntos, " puntos (",
        resultado$polaridad, ")\n\n", sep = "")
  }

  resultado
}


# =============================================================================
# DETECCION DE CONTEXTO
# =============================================================================

#' @keywords internal
.detectar_contexto <- function(items_txt, concepto, poblacion) {

  texto_completo <- tolower(paste(
    paste(items_txt, collapse = " "),
    as.character(concepto),
    as.character(poblacion)
  ))

  # Normalizar tildes
  texto_completo <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1",
                          "aeioun", texto_completo)

  contar <- function(palabras) {
    sum(vapply(palabras, function(p) grepl(p, texto_completo, fixed = TRUE),
               logical(1)))
  }

  contextos <- list(
    parental = c(
      "hijo", "hija", "padre", "madre", "crianza", "parental",
      "paterno", "materno", "mi nino", "mi nina", "familia",
      "educar", "apego", "vinculo"
    ),
    clinico = c(
      "sintoma", "depresion", "ansiedad", "trauma", "terapia",
      "paciente", "diagnostico", "trastorno", "clinico", "psicopatologia",
      "autolesion", "suicid", "panico", "obsesion"
    ),
    adicciones = c(
      "alcohol", "droga", "adiccion", "consumo", "sustancia",
      "bebida", "cigarril", "tabaco", "apuestas"
    ),
    educativo = c(
      "estudiante", "alumno", "clase", "profesor", "docente",
      "aprendizaje", "academico", "estudio", "universidad", "escuela"
    ),
    laboral = c(
      "trabajo", "empleo", "empresa", "empleado", "jefe",
      "colaborador", "equipo de trabajo", "organizacion"
    )
  )

  puntajes <- vapply(contextos, contar, integer(1))

  if (max(puntajes) < 2) return("general")
  names(puntajes)[which.max(puntajes)]
}


# =============================================================================
# ESTIMACION DE DESEABILIDAD SOCIAL
# =============================================================================

#' @keywords internal
.estimar_deseabilidad_social <- function(items_txt, contexto) {

  normalizar <- function(s) {
    s <- tolower(s)
    chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1", "aeioun", s)
  }

  texto <- vapply(items_txt, normalizar, character(1))

  # Marcadores de contenido con alta deseabilidad social
  marcadores_ds <- c(
    # Relaciones interpersonales (roles valorados)
    "hijo", "hija", "pareja", "familia", "amigo",
    # Afecto y cuidado
    "afecto", "consuelo", "carino", "cuidado", "amor",
    # Emociones negativas socialmente reprobadas
    "rechazo", "hostilidad", "enojo", "enfado", "grito",
    "castigo", "ignoro", "alejo", "golpeo",
    # Comportamientos sensibles
    "maltrato", "violencia", "agresivo", "miento",
    # Omisiones valoradas negativamente
    "no escucho", "no respondo", "minimizo", "descuido"
  )

  n <- length(texto)
  hits <- sum(vapply(texto, function(it) {
    any(vapply(marcadores_ds, function(m) grepl(m, it, fixed = TRUE),
               logical(1)))
  }, logical(1)))

  base <- hits / n

  # Bonus por contexto
  bonus <- switch(contexto,
    "parental"   = 0.25,
    "clinico"    = 0.20,
    "adicciones" = 0.20,
    0
  )

  min(1, base + bonus)
}


# =============================================================================
# METODO 1: HEURISTICO
# =============================================================================

#' @keywords internal
.sugerir_heuristica <- function(items_txt, idioma = "es",
                                n_puntos = NULL,
                                contexto = "general",
                                evitar_absolutos = FALSE,
                                verbose = TRUE) {

  marcadores <- list(
    frecuencia = c(
      "reviso", "mantengo", "alterno", "superviso", "expreso",
      "intervengo", "valido", "respondo", "consuelo", "acerco",
      "minimizo", "rechazo", "paralizo", "reacciono", "reparo",
      "apoyo", "pongo", "comparto", "comento", "observo",
      "a veces", "a menudo", "frecuentemente", "repetidamente",
      "constantemente", "hay momentos", "varian", "cuando mi hijo"
    ),
    intensidad = c(
      "me siento", "me resulta", "me incomoda", "me preocupa",
      "me cuesta", "me asusto", "me agota", "me desborda",
      "me duele", "me inquieta", "me abruma", "siento miedo",
      "siento rechazo", "siento culpa", "necesito", "es dificil",
      "resulta dificil", "no puedo", "puedo"
    ),
    acuerdo = c(
      "creo que", "considero que", "pienso que", "opino que",
      "valoro", "me parece", "es importante", "debe", "deberia",
      "es mejor", "estoy convencido", "estoy segura", "estoy seguro"
    ),
    preferencia = c("prefiero", "elijo", "evito", "busco", "tiendo a")
  )

  normalizar <- function(s) {
    s <- tolower(s)
    chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1", "aeioun", s)
  }

  items_norm <- vapply(items_txt, normalizar, character(1))

  contar_marcadores <- function(items, palabras) {
    palabras_norm <- vapply(palabras, normalizar, character(1))
    sum(vapply(items, function(it) {
      any(vapply(palabras_norm, function(p) grepl(p, it, fixed = TRUE),
                 logical(1)))
    }, logical(1)))
  }

  conteos <- c(
    frecuencia  = contar_marcadores(items_norm, marcadores$frecuencia),
    intensidad  = contar_marcadores(items_norm, marcadores$intensidad),
    acuerdo     = contar_marcadores(items_norm, marcadores$acuerdo),
    preferencia = contar_marcadores(items_norm, marcadores$preferencia)
  )

  n_total <- length(items_txt)
  proporciones <- conteos / n_total

  orden_desempate <- c("frecuencia", "intensidad", "acuerdo", "preferencia")
  tipo_escala <- orden_desempate[which.max(conteos[orden_desempate])]
  if (max(proporciones) < 0.20) tipo_escala <- "acuerdo"

  if (is.null(n_puntos)) n_puntos <- 5L

  polaridad <- if (tipo_escala == "acuerdo") "bipolar" else "unipolar"
  punto_neutral <- (n_puntos %% 2 == 1)

  anclajes <- .anclajes_estandar(
    tipo = tipo_escala,
    n_puntos = n_puntos,
    idioma = idioma,
    contexto = contexto,
    evitar_absolutos = evitar_absolutos
  )

  # Evaluar discriminabilidad de la recomendacion principal.
  # Si es baja y n_puntos era 5, degradar a 4 puntos (sin neutral).
  discr <- .evaluar_discriminabilidad(anclajes)
  if (is.null(n_puntos) && discr < 0.70 && n_puntos != 4L) {
    n_puntos <- 4L
    anclajes <- .anclajes_estandar(tipo_escala, n_puntos, idioma,
                                   contexto, evitar_absolutos)
    discr <- .evaluar_discriminabilidad(anclajes)
    punto_neutral <- FALSE
  }

  just <- paste0(
    "Analisis heuristico de ", n_total, " items. ",
    "Contexto: ", contexto, ". ",
    "Marcadores dominantes: frecuencia=", conteos["frecuencia"], " (",
    sprintf("%.0f%%", proporciones["frecuencia"] * 100), "), ",
    "intensidad=", conteos["intensidad"], " (",
    sprintf("%.0f%%", proporciones["intensidad"] * 100), "), ",
    "acuerdo=", conteos["acuerdo"], " (",
    sprintf("%.0f%%", proporciones["acuerdo"] * 100), "). ",
    if (evitar_absolutos)
      paste0("Se evitan anclajes absolutos ('Nunca'/'Siempre') porque el ",
             "contenido describe comportamientos con carga de deseabilidad ",
             "social. Se usan cuantificadores relativos. ")
    else "",
    "Escala recomendada: ", tipo_escala, " de ", n_puntos, " puntos."
  )

  alternativas <- .alternativas_estandar(
    tipo_principal   = tipo_escala,
    n_puntos         = n_puntos,
    idioma           = idioma,
    contexto         = contexto,
    evitar_absolutos = evitar_absolutos
  )

  list(
    tipo_escala       = tipo_escala,
    n_puntos          = n_puntos,
    polaridad         = polaridad,
    punto_neutral     = punto_neutral,
    anclajes          = anclajes,
    discriminabilidad = round(discr, 2),
    justificacion     = just,
    alternativas      = alternativas,
    diagnostico       = data.frame(
      tipo       = names(conteos),
      conteo     = as.integer(conteos),
      proporcion = round(as.numeric(proporciones), 3),
      stringsAsFactors = FALSE
    )
  )
}


# =============================================================================
# METODO 2: LLM
# =============================================================================

#' @keywords internal
.sugerir_llm <- function(items_txt, concepto, poblacion,
                         api_key, modelo = "gpt-4.1-mini",
                         idioma = "es", n_puntos = NULL,
                         contexto = "general",
                         evitar_absolutos = FALSE,
                         anclajes_contextuales = TRUE,
                         deseabilidad = 0,
                         verbose = TRUE) {

  openai <- .configurar_openai(api_key)

  idioma_txt <- switch(idioma,
    "es" = "Responde SIEMPRE en espanol. Los anclajes deben estar en espanol natural y claro.",
    "en" = "Respond ALWAYS in English. Anchors in clear English.",
    "pt" = "Responda SEMPRE em portugues. Anclas em portugues claro."
  )

  restriccion_n <- if (!is.null(n_puntos)) {
    paste0("RESTRICCION OBLIGATORIA: usar exactamente ", n_puntos, " puntos.")
  } else {
    "Puedes elegir entre 4, 5, 6 o 7 puntos segun convenga."
  }

  # --- Reglas psicometricas segun contexto ---
  reglas_contexto <- switch(contexto,
    "parental" = paste(
      "CONTEXTO: comportamiento parental hacia hijos. Los padres son reticentes",
      "a afirmar conductas negativas extremas y tienden a sobreestimar positivas.",
      "Usa anclajes RELATIVOS y suavizados que reduzcan la deseabilidad social."
    ),
    "clinico" = paste(
      "CONTEXTO: sintomatologia clinica. Los pacientes minimizan sintomas.",
      "Usa anclajes que permitan reportes graduales sin etiquetar extremos."
    ),
    "adicciones" = paste(
      "CONTEXTO: comportamientos de consumo. Hay fuerte minimizacion por estigma.",
      "Usa anclajes neutros de frecuencia en lugar de juicio moral."
    ),
    paste("CONTEXTO: dominio general. Selecciona anclajes apropiados al contenido.")
  )

  regla_absolutos <- if (evitar_absolutos) paste(
    "REGLA CRITICA: NO uses 'Nunca' ni 'Siempre' como anclajes extremos.",
    "Sustitutye por cuantificadores relativos: 'Casi nunca', 'Rara vez',",
    "'En pocas ocasiones' / 'Casi siempre', 'La mayoria del tiempo',",
    "'En la mayoria de las ocasiones'.",
    "Los extremos absolutos generan efectos piso/techo en contextos con",
    "alta deseabilidad social. Prefiere expresiones suavizadas y graduables."
  ) else ""

  regla_contextuales <- if (anclajes_contextuales) paste(
    "Los anclajes deben ser NATURALES al dominio (no plantillas genericas).",
    "Considera el vocabulario tipico que usaria la poblacion objetivo.",
    "Evita jerga academica; usa lenguaje cotidiano."
  ) else ""

  muestra_items <- paste0(seq_along(items_txt), ". ", items_txt,
                          collapse = "\n")

  system_msg <- paste(
    "Eres un psicometra experto con 20 anos de experiencia en diseno de escalas Likert.",
    "Tu tarea es recomendar el formato de respuesta optimo considerando:",
    "(1) el contenido semantico de los items,",
    "(2) el dominio y la poblacion,",
    "(3) sesgos de respuesta (deseabilidad social, aquiescencia, tendencia central),",
    "(4) la granularidad psicometrica apropiada.",
    "Sigue a Ferrando et al. (2025), Krosnick (1999), Weijters et al. (2010).",
    idioma_txt,
    "Responde EXCLUSIVAMENTE en formato JSON valido, sin texto adicional."
  )

  regla_discriminabilidad <- paste(
    "REGLA DE DISCRIMINABILIDAD SEMANTICA:",
    "Evalua si las anclas adyacentes son claramente distinguibles para un",
    "hispanohablante promedio. Pares como 'A menudo' vs 'Casi siempre' o",
    "'Rara vez' vs 'Casi nunca' suelen confundirse y reducen la varianza.",
    "Si al suavizar absolutos los anclajes se apilan en un polo (p. ej. 4",
    "y 5 parecen casi sinonimos), PREFIERE 4 puntos en lugar de 5. No",
    "sacrifiques claridad por granularidad."
  )

  user_msg <- paste0(
    "CONSTRUCTO: ", concepto, "\n\n",
    "POBLACION: ", poblacion, "\n\n",
    "ITEMS (", length(items_txt), "):\n", muestra_items, "\n\n",
    reglas_contexto, "\n\n",
    "INDICADOR DE DESEABILIDAD SOCIAL DEL CONTENIDO: ",
    sprintf("%.2f (0-1)", deseabilidad), "\n\n",
    regla_absolutos, "\n\n",
    regla_discriminabilidad, "\n\n",
    regla_contextuales, "\n\n",
    restriccion_n, "\n\n",
    "Devuelve SOLO un objeto JSON con esta estructura EXACTA. ",
    "DEBES proponer EXACTAMENTE 5 ALTERNATIVAS distintas cubriendo variaciones ",
    "de numero de puntos (4, 5, 6, 7) y al menos un cambio de tipo de escala. ",
    "Para cada alternativa evalua 'discriminabilidad' en escala 0-1 segun cuan ",
    "distinguibles son las anclas adyacentes para el respondiente promedio.\n\n",
    "{\n",
    '  "tipo_escala": "frecuencia|acuerdo|intensidad|preferencia|descripcion",\n',
    '  "n_puntos": 5,\n',
    '  "polaridad": "unipolar|bipolar",\n',
    '  "punto_neutral": true,\n',
    '  "anclajes": {"1": "Texto ancla 1", "2": "Texto ancla 2", "3": "...", "4": "...", "5": "..."},\n',
    '  "discriminabilidad": 0.85,\n',
    '  "justificacion": "Razonamiento psicometrico: por que ese tipo, por que ese N, por que esos anclajes especificos al dominio, como mitigan la deseabilidad social, y por que las anclas adyacentes son distinguibles.",\n',
    '  "consideraciones_sesgo": "Sesgos esperables y como se mitigan.",\n',
    '  "alternativas": [\n',
    '    {"tipo_escala": "...", "n_puntos": 4, "anclajes": {"1": "...", "2": "...", "3": "...", "4": "..."}, "discriminabilidad": 0.9, "ventaja": "...", "desventaja": "..."},\n',
    '    {"tipo_escala": "...", "n_puntos": 6, "anclajes": {"1": "...", "...": "..."}, "discriminabilidad": 0.7, "ventaja": "...", "desventaja": "..."},\n',
    '    {"tipo_escala": "...", "n_puntos": 7, "anclajes": {"1": "...", "...": "..."}, "discriminabilidad": 0.6, "ventaja": "...", "desventaja": "..."},\n',
    '    {"tipo_escala": "otro_tipo", "n_puntos": 5, "anclajes": {"1": "...", "...": "..."}, "discriminabilidad": 0.85, "ventaja": "...", "desventaja": "..."},\n',
    '    {"tipo_escala": "otro_tipo_2", "n_puntos": 5, "anclajes": {"1": "...", "...": "..."}, "discriminabilidad": 0.8, "ventaja": "...", "desventaja": "..."}\n',
    "  ]\n",
    "}"
  )

  messages <- list(
    list(role = "system", content = system_msg),
    list(role = "user",   content = user_msg)
  )

  if (verbose) cat("[LLM] consultando ", modelo, " (contexto=",
                   contexto, ")...\n", sep = "")

  contenido <- .llamar_openai(openai, messages, modelo = modelo,
                              max_tokens = 3000L)

  # Algunos wrappers retornan la respuesta cruda; otros ya el texto.
  if (is.list(contenido) && !is.null(contenido$choices)) {
    contenido <- contenido$choices[[1]]$message$content
  }

  contenido <- gsub("```json", "", contenido, fixed = TRUE)
  contenido <- gsub("```",     "", contenido, fixed = TRUE)
  contenido <- trimws(contenido)

  parsed <- tryCatch(
    jsonlite::fromJSON(contenido, simplifyVector = FALSE),
    error = function(e) {
      warning("No se pudo parsear JSON del LLM. Fallback a heuristica.")
      NULL
    }
  )

  if (is.null(parsed)) {
    return(.sugerir_heuristica(items_txt, idioma, n_puntos,
                               contexto, evitar_absolutos, verbose))
  }

  # Validacion post-hoc: si se pidio evitar absolutos, revisar que no haya
  # aparecido "Nunca" o "Siempre" como extremos
  if (evitar_absolutos) {
    parsed$anclajes <- .suavizar_absolutos(parsed$anclajes, idioma)
  }

  anclajes_vec <- unlist(parsed$anclajes)
  names(anclajes_vec) <- names(parsed$anclajes)

  if (!is.null(parsed$alternativas) && length(parsed$alternativas) > 0) {
    alternativas <- lapply(parsed$alternativas, function(a) {
      anc <- unlist(a$anclajes)
      if (evitar_absolutos) {
        anc_lista <- as.list(anc); names(anc_lista) <- names(anc)
        anc_lista <- .suavizar_absolutos(anc_lista, idioma)
        anc <- unlist(anc_lista); names(anc) <- names(anc_lista)
      }
      discr <- if (!is.null(a$discriminabilidad))
                 as.numeric(a$discriminabilidad)
               else .evaluar_discriminabilidad(anc)
      list(
        tipo_escala       = a$tipo_escala,
        n_puntos          = as.integer(a$n_puntos),
        anclajes          = anc,
        ventaja           = a$ventaja,
        desventaja        = a$desventaja,
        discriminabilidad = round(discr, 2)
      )
    })
  } else {
    alternativas <- list()
  }

  # Garantizar 5 alternativas (completar con heuristica si el LLM devuelve menos)
  if (length(alternativas) < 5) {
    extras <- .alternativas_estandar(parsed$tipo_escala,
                                     as.integer(parsed$n_puntos),
                                     idioma, contexto, evitar_absolutos)
    alternativas <- c(alternativas, extras)[seq_len(5)]
  } else if (length(alternativas) > 5) {
    alternativas <- alternativas[seq_len(5)]
  }

  discr_principal <- if (!is.null(parsed$discriminabilidad))
                       as.numeric(parsed$discriminabilidad)
                     else .evaluar_discriminabilidad(anclajes_vec)

  list(
    tipo_escala           = parsed$tipo_escala,
    n_puntos              = as.integer(parsed$n_puntos),
    polaridad             = parsed$polaridad,
    punto_neutral         = isTRUE(parsed$punto_neutral),
    anclajes              = anclajes_vec,
    discriminabilidad     = round(discr_principal, 2),
    justificacion         = parsed$justificacion,
    consideraciones_sesgo = parsed$consideraciones_sesgo,
    alternativas          = alternativas,
    diagnostico           = NULL
  )
}


# =============================================================================
# UTILIDADES INTERNAS
# =============================================================================

#' @keywords internal
.suavizar_absolutos <- function(anclajes_list, idioma = "es") {

  reemplazos <- list(
    es = c(
      "nunca"   = "Casi nunca",
      "Nunca"   = "Casi nunca",
      "NUNCA"   = "Casi nunca",
      "siempre" = "Casi siempre",
      "Siempre" = "Casi siempre",
      "SIEMPRE" = "Casi siempre"
    ),
    en = c(
      "never"  = "Almost never",
      "Never"  = "Almost never",
      "always" = "Almost always",
      "Always" = "Almost always"
    ),
    pt = c(
      "nunca"   = "Quase nunca",
      "Nunca"   = "Quase nunca",
      "sempre"  = "Quase sempre",
      "Sempre"  = "Quase sempre"
    )
  )

  rep <- reemplazos[[idioma]] %||% reemplazos$es

  lapply(anclajes_list, function(valor) {
    valor_chr <- as.character(valor)
    if (valor_chr %in% names(rep)) rep[[valor_chr]] else valor_chr
  })
}


#' @keywords internal
.extraer_info_escala_resp <- function(x) {

  items_txt <- character(0)
  poblacion <- "poblacion general"
  concepto  <- "constructo sin especificar"

  if (inherits(x, "semilla") || inherits(x, "semilla_items") || is.list(x)) {
    if (!is.null(x$items)) {
      if (is.data.frame(x$items) && "item" %in% names(x$items)) {
        items_txt <- as.character(x$items$item)
      } else if (is.data.frame(x$items) && "texto" %in% names(x$items)) {
        items_txt <- as.character(x$items$texto)
      }
    }

    if (!is.null(x$concepto)) {
      if (is.list(x$concepto)) {
        concepto <- x$concepto$definicion %||% x$concepto$nombre %||% "constructo"
      } else if (is.character(x$concepto)) {
        concepto <- x$concepto
      }
    }

    if (!is.null(x$metadata$poblacion)) poblacion <- x$metadata$poblacion
  }

  if (is.data.frame(x) && "item" %in% names(x)) {
    items_txt <- as.character(x$item)
  }

  list(items = items_txt, poblacion = poblacion, concepto = concepto)
}


#' @keywords internal
.anclajes_estandar <- function(tipo, n_puntos, idioma = "es",
                               contexto = "general",
                               evitar_absolutos = FALSE) {

  # Plantillas suavizadas (evitar_absolutos = TRUE)
  plantillas_suaves <- list(
    es = list(
      frecuencia = list(
        "4" = c("1" = "Casi nunca", "2" = "En pocas ocasiones",
                "3" = "En muchas ocasiones", "4" = "Casi siempre"),
        "5" = c("1" = "Casi nunca", "2" = "Rara vez", "3" = "A veces",
                "4" = "A menudo", "5" = "Casi siempre"),
        "6" = c("1" = "Casi nunca", "2" = "Rara vez", "3" = "Pocas veces",
                "4" = "Varias veces", "5" = "A menudo",
                "6" = "Casi siempre"),
        "7" = c("1" = "Casi nunca", "2" = "Muy rara vez", "3" = "Rara vez",
                "4" = "A veces", "5" = "A menudo",
                "6" = "Con mucha frecuencia", "7" = "Casi siempre")
      ),
      intensidad = list(
        "5" = c("1" = "Muy poco", "2" = "Poco", "3" = "Algo",
                "4" = "Bastante", "5" = "Mucho")
      ),
      acuerdo = list(
        "5" = c("1" = "Muy en desacuerdo", "2" = "En desacuerdo",
                "3" = "Ni de acuerdo ni en desacuerdo",
                "4" = "De acuerdo", "5" = "Muy de acuerdo")
      )
    )
  )

  # Plantillas clasicas (evitar_absolutos = FALSE)
  plantillas_clasicas <- list(
    es = list(
      frecuencia = list(
        "4" = c("1" = "Nunca", "2" = "A veces",
                "3" = "Frecuentemente", "4" = "Siempre"),
        "5" = c("1" = "Nunca", "2" = "Casi nunca", "3" = "A veces",
                "4" = "Casi siempre", "5" = "Siempre"),
        "6" = c("1" = "Nunca", "2" = "Rara vez", "3" = "A veces",
                "4" = "Con frecuencia", "5" = "Casi siempre", "6" = "Siempre"),
        "7" = c("1" = "Nunca", "2" = "Casi nunca", "3" = "Rara vez",
                "4" = "A veces", "5" = "Con frecuencia",
                "6" = "Casi siempre", "7" = "Siempre")
      ),
      intensidad = list(
        "4" = c("1" = "Nada", "2" = "Poco", "3" = "Bastante", "4" = "Mucho"),
        "5" = c("1" = "Nada", "2" = "Poco", "3" = "Algo",
                "4" = "Bastante", "5" = "Mucho"),
        "7" = c("1" = "Nada", "2" = "Casi nada", "3" = "Poco",
                "4" = "Algo", "5" = "Bastante", "6" = "Mucho",
                "7" = "Muchisimo")
      ),
      acuerdo = list(
        "4" = c("1" = "Totalmente en desacuerdo", "2" = "En desacuerdo",
                "3" = "De acuerdo", "4" = "Totalmente de acuerdo"),
        "5" = c("1" = "Totalmente en desacuerdo", "2" = "En desacuerdo",
                "3" = "Ni de acuerdo ni en desacuerdo",
                "4" = "De acuerdo", "5" = "Totalmente de acuerdo"),
        "7" = c("1" = "Totalmente en desacuerdo", "2" = "En desacuerdo",
                "3" = "Algo en desacuerdo", "4" = "Ni de acuerdo ni en desacuerdo",
                "5" = "Algo de acuerdo", "6" = "De acuerdo",
                "7" = "Totalmente de acuerdo")
      ),
      preferencia = list(
        "5" = c("1" = "Definitivamente no", "2" = "Probablemente no",
                "3" = "No lo se", "4" = "Probablemente si",
                "5" = "Definitivamente si")
      )
    )
  )

  plantillas <- if (evitar_absolutos) plantillas_suaves else plantillas_clasicas

  idioma_sel <- plantillas[[idioma]] %||% plantillas$es
  escala_tipo <- idioma_sel[[tipo]] %||% idioma_sel$acuerdo %||%
                 list("5" = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"))
  anc <- escala_tipo[[as.character(n_puntos)]]
  if (is.null(anc)) anc <- escala_tipo[["5"]]
  if (is.null(anc)) anc <- c("1" = "1", "2" = "2", "3" = "3",
                             "4" = "4", "5" = "5")
  anc
}


#' @keywords internal
#' Genera siempre 5 alternativas combinando distintos N de puntos y tipos.
#' La lista cubre: mismo tipo con menos puntos, mismo tipo con mas puntos,
#' variante dicotomica ampliada (4 sin neutral), tipo alternativo 1 y 2.
.alternativas_estandar <- function(tipo_principal, n_puntos,
                                   idioma = "es",
                                   contexto = "general",
                                   evitar_absolutos = FALSE) {

  otros_tipos <- setdiff(c("frecuencia", "intensidad", "acuerdo"),
                         tipo_principal)

  # Diseno fijo: 5 alternativas balanceadas
  candidatos <- list(
    list(tipo = tipo_principal,  n = 4L,
         nota = "Mismo tipo, 4 puntos (sin neutral). Fuerza a posicionarse y",
         nota2 = "reduce solapamiento semantico entre anclas adyacentes."),
    list(tipo = tipo_principal,  n = 6L,
         nota = "Mismo tipo, 6 puntos (sin neutral). Mayor discriminacion",
         nota2 = "manteniendo equilibrio entre polos."),
    list(tipo = tipo_principal,  n = 7L,
         nota = "Mismo tipo, 7 puntos (con neutral). Maxima granularidad IRT,",
         nota2 = "util si la poblacion tiene alta capacidad verbal."),
    list(tipo = otros_tipos[1],  n = 5L,
         nota = paste0("Cambio de tipo a ", otros_tipos[1],
                       ", 5 puntos. Util si el contenido se interpreta"),
         nota2 = "mejor en terminos cualitativos distintos."),
    list(tipo = otros_tipos[2],  n = 5L,
         nota = paste0("Cambio de tipo a ", otros_tipos[2],
                       ", 5 puntos. Opcion secundaria de reencuadre"),
         nota2 = "del eje de respuesta.")
  )

  lapply(candidatos, function(c) {
    anc <- .anclajes_estandar(c$tipo, c$n, idioma, contexto,
                              evitar_absolutos)
    discriminabilidad <- .evaluar_discriminabilidad(anc)

    mismo_tipo <- identical(c$tipo, tipo_principal)

    ventaja <- if (c$n < n_puntos && mismo_tipo) {
      "Menor carga cognitiva; mayor claridad entre opciones."
    } else if (c$n > n_puntos && mismo_tipo) {
      "Mayor granularidad psicometrica."
    } else {
      "Reencuadre conceptual del eje de respuesta."
    }

    desventaja <- if (c$n < n_puntos && mismo_tipo) {
      "Menor precision IRT."
    } else if (c$n > n_puntos && mismo_tipo) {
      if (discriminabilidad < 0.6)
        "Anclas adyacentes pueden confundirse (baja discriminabilidad semantica)."
      else "Mayor fatiga y tiempo de respuesta."
    } else {
      "Puede no capturar la variabilidad del contenido original."
    }

    list(
      tipo_escala       = c$tipo,
      n_puntos          = c$n,
      anclajes          = anc,
      ventaja           = ventaja,
      desventaja        = desventaja,
      discriminabilidad = round(discriminabilidad, 2),
      nota              = paste(c$nota, c$nota2)
    )
  })
}


#' @keywords internal
#' Evalua cuan distinguibles son semanticamente las anclas adyacentes.
#' Penaliza solapamientos como "a menudo" vs "casi siempre".
#' Retorna un indice 0-1 donde 1 = alta discriminabilidad.
.evaluar_discriminabilidad <- function(anclajes) {

  # Pares problematicos conocidos (baja discriminabilidad)
  pares_confusos <- list(
    c("a menudo",       "casi siempre"),
    c("con frecuencia", "casi siempre"),
    c("con frecuencia", "a menudo"),
    c("rara vez",       "casi nunca"),
    c("muy rara vez",   "casi nunca"),
    c("pocas veces",    "rara vez"),
    c("varias veces",   "a menudo"),
    c("bastante",       "mucho"),
    c("algo",           "poco"),
    c("poco",           "muy poco"),
    c("casi nada",      "nada"),
    c("algo de acuerdo","de acuerdo")
  )

  normalizar <- function(s) {
    s <- tolower(as.character(s))
    s <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1", "aeioun", s)
    trimws(s)
  }

  anc_norm <- vapply(anclajes, normalizar, character(1))
  n <- length(anc_norm)
  if (n < 2) return(1)

  # Contar pares adyacentes problematicos
  conflictos <- 0
  for (i in seq_len(n - 1)) {
    a <- anc_norm[i]; b <- anc_norm[i + 1]
    for (par in pares_confusos) {
      if ((a == par[1] && b == par[2]) || (a == par[2] && b == par[1])) {
        conflictos <- conflictos + 1
        break
      }
    }
  }

  max(0, 1 - conflictos / (n - 1))
}


# Helper
`%||%` <- function(a, b) if (is.null(a)) b else a


# =============================================================================
# METODO S3: PRINT
# =============================================================================

#' @export
print.semilla_escala_respuesta <- function(x, ...) {
  cat("\n")
  cat("=====================================================\n")
  cat("  Escala de respuesta recomendada (SeMiLLa)         \n")
  cat("=====================================================\n")
  cat("  Metodo              : ", x$metodo, "\n", sep = "")
  cat("  Contexto            : ", x$contexto, "\n", sep = "")
  cat("  Deseabilidad social : ", sprintf("%.2f", x$deseabilidad_social),
      "\n", sep = "")
  cat("  Evitar absolutos    : ", ifelse(x$evitar_absolutos, "si", "no"),
      "\n", sep = "")
  cat("-----------------------------------------------------\n")
  cat("  RECOMENDACION PRINCIPAL\n")
  cat("-----------------------------------------------------\n")
  cat("  Tipo                : ", x$tipo_escala, "\n", sep = "")
  cat("  N de puntos         : ", x$n_puntos, "\n", sep = "")
  cat("  Polaridad           : ", x$polaridad, "\n", sep = "")
  cat("  Punto neutral       : ", ifelse(x$punto_neutral, "si", "no"),
      "\n", sep = "")
  if (!is.null(x$discriminabilidad))
    cat("  Discriminabilidad   : ", sprintf("%.2f", x$discriminabilidad),
        " (0=baja, 1=alta)\n", sep = "")
  cat("-----------------------------------------------------\n")
  cat("  Anclajes:\n")
  for (k in names(x$anclajes)) {
    cat("    ", k, " = ", x$anclajes[[k]], "\n", sep = "")
  }
  cat("-----------------------------------------------------\n")
  cat("  Justificacion:\n")
  cat("  ", strwrap(x$justificacion, width = 70, prefix = "  "), sep = "\n")

  if (!is.null(x$consideraciones_sesgo) && nchar(x$consideraciones_sesgo) > 0) {
    cat("\n  Consideraciones de sesgo:\n")
    cat("  ", strwrap(x$consideraciones_sesgo, width = 70, prefix = "  "),
        sep = "\n")
  }

  if (!is.null(x$alternativas) && length(x$alternativas) > 0) {
    cat("\n=====================================================\n")
    cat("  5 ALTERNATIVAS CONSIDERADAS\n")
    cat("=====================================================\n")
    for (i in seq_along(x$alternativas)) {
      a <- x$alternativas[[i]]
      cat("\n  [", i, "] ", toupper(a$tipo_escala), " - ", a$n_puntos,
          " puntos", sep = "")
      if (!is.null(a$discriminabilidad))
        cat(" | discriminabilidad = ", sprintf("%.2f", a$discriminabilidad),
            sep = "")
      cat("\n")
      if (!is.null(a$anclajes)) {
        cat("      Anclas: ",
            paste(a$anclajes, collapse = " / "), "\n", sep = "")
      }
      if (!is.null(a$ventaja))    cat("      (+) ", a$ventaja, "\n", sep = "")
      if (!is.null(a$desventaja)) cat("      (-) ", a$desventaja, "\n", sep = "")
    }
  }

  if (!is.null(x$diagnostico)) {
    cat("\n-----------------------------------------------------\n")
    cat("  Diagnostico (conteo de marcadores):\n")
    print(x$diagnostico, row.names = FALSE)
  }
  cat("=====================================================\n\n")
  invisible(x)
}
