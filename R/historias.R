#' @title Generar escala basada en historias (vignette-based scale)
#'
#' @description
#' Genera un instrumento donde el respondiente lee una HISTORIA breve sobre
#' dos personajes y luego responde N items de PERCEPCION/JUICIO sobre lo
#' que ocurre en esa historia (estilo Perceptions of Dating Violence Scale,
#' Toplu-Demirtas et al., 2020).
#'
#' La estructura tiene tres componentes:
#' \enumerate{
#'   \item \strong{Introduccion comun}: presenta a los personajes y el
#'         contexto de la relacion. Es identica en todas las historias.
#'   \item \strong{K historias} (una por factor): la introduccion + un
#'         desenlace especifico que representa el factor (p. ej. una historia
#'         de control, otra de celos, otra de humillacion).
#'   \item \strong{N items de percepcion} (transversales): los MISMOS items
#'         se aplican a CADA historia. Cubren M facetas de juicio
#'         (severidad, justificacion, normalizacion, culpa de la victima).
#' }
#'
#' Cada llamada al LLM se cachea via \code{habilitar_cache()}.
#'
#' @param concepto Cadena con la definicion del constructo a evaluar.
#' @param api_key Clave de OpenAI.
#' @param idioma Idioma de salida ("es", "en", "pt").
#' @param poblacion Cadena describiendo la poblacion objetivo.
#' @param factores Vector character con los nombres de los factores. Una
#'   historia por factor.
#' @param descripcion_factores Vector character con la descripcion de cada
#'   factor (mismo largo que \code{factores}). Si NULL, el LLM se gua sole
#'   por el nombre del factor.
#' @param agresor Nombre del personaje que ejerce la conducta. Editable.
#' @param victima Nombre del personaje que la recibe. Editable.
#' @param contexto_relacion Texto que situa la relacion (e.g.
#'   "estudiantes universitarios en una relacion de noviazgo de mas de un
#'   año"). Se incluye en la introduccion comun.
#' @param n_items Numero TOTAL de items de percepcion (transversales).
#'   Default 16. Se distribuyen lo mas uniformemente posible entre las
#'   facetas.
#' @param facetas_percepcion Vector character con las facetas conceptuales
#'   de los items. Default 4: severidad, justificacion, normalizacion,
#'   culpa_victima. Cada item se asigna a una.
#' @param tipo_escala_respuesta \code{"acuerdo"} (default), \code{"frecuencia"}
#'   o \code{"intensidad"}. Solo orienta al LLM en la redaccion.
#' @param balance_polaridad Logico. Si TRUE (default), aproximadamente la
#'   mitad de los items se redactan con polaridad inversa (mas acuerdo =
#'   menos normalizacion) para mitigar aquiescencia.
#' @param max_palabras_introduccion Limite de palabras para la introduccion.
#' @param max_palabras_historia Limite de palabras para cada desenlace
#'   especifico (sin contar la introduccion).
#' @param max_palabras_item Limite de palabras por item.
#' @param agresor_colectivo Logico. Si TRUE, el personaje agresor se trata como
#'   colectivo (grupo) en lugar de individual. Default FALSE.
#' @param genero_victima Genero del personaje victima: "auto" (default),
#'   "masculino", "femenino" o "neutro".
#' @param genero_agresor Genero del personaje agresor: "auto" (default),
#'   "masculino", "femenino" o "neutro".
#' @param items_modo Generacion de items: "transversal" (default, comunes a
#'   todas las historias) o "por_historia" (especificos de cada historia).
#' @param contexto_propension Texto opcional que orienta la propension del
#'   contexto. NULL por defecto.
#' @param etapa_evolutiva Etapa evolutiva que ajusta el lenguaje: "auto"
#'   (default), "ninez", "adolescencia_temprana", "adolescencia_media",
#'   "adolescencia_tardia", "adultez_emergente", "adultez" o "adulto_mayor".
#' @param nivel_socioeconomico Nivel socioeconomico que ajusta el lenguaje:
#'   "auto" (default), "alto", "medio", "medio_bajo" o "bajo".
#' @param modelo Modelo OpenAI. Default \code{"gpt-4.1-mini-2025-04-14"}.
#' @param seed Semilla para reproducibilidad.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_historias} (lista) con:
#' \itemize{
#'   \item \code{introduccion}: cadena, parrafo comun.
#'   \item \code{historias}: data.frame (factor, descripcion, texto).
#'   \item \code{items}: data.frame (n_item, item, faceta, polaridad).
#'   \item \code{personajes}: lista (agresor, victima).
#'   \item \code{concepto}, \code{poblacion}, \code{idioma}.
#'   \item \code{metadata}: lista con modelo, seed, fecha, n_items, etc.
#' }
#'
#' @examples
#' \dontrun{
#' h <- generar_escala_historias(
#'   concepto = "normalizacion de violencia psicologica en pareja",
#'   api_key  = api_key,
#'   factores = c("Control", "Celos", "Humillacion", "Aislamiento"),
#'   agresor  = "Diego",
#'   victima  = "Camila",
#'   n_items  = 16
#' )
#' print(h)
#' }
#'
#' @export
generar_escala_historias <- function(
  concepto,
  api_key,
  idioma                  = c("es", "en", "pt"),
  poblacion               = "adultos en general",
  factores,
  descripcion_factores    = NULL,
  agresor                 = "Daniel",
  victima                 = "Sofia",
  agresor_colectivo       = FALSE,
  genero_victima          = c("auto", "masculino", "femenino", "neutro"),
  genero_agresor          = c("auto", "masculino", "femenino", "neutro"),
  contexto_relacion       = NULL,
  n_items                 = 16L,
  facetas_percepcion      = c("severidad", "justificacion",
                              "normalizacion", "culpa_victima"),
  tipo_escala_respuesta   = c("acuerdo", "frecuencia", "intensidad"),
  balance_polaridad       = TRUE,
  max_palabras_introduccion = 60L,
  max_palabras_historia     = 110L,
  max_palabras_item         = 18L,
  modelo                  = "gpt-4.1-mini-2025-04-14",
  seed                    = 2026,
  verbose                 = TRUE,
  # --- v2: items modo (transversal vs por_historia) ---
  items_modo              = c("transversal", "por_historia"),
  contexto_propension     = NULL,
  # --- v3: control de lenguaje por etapa evolutiva y NSE ---
  etapa_evolutiva         = c("auto", "ninez", "adolescencia_temprana",
                               "adolescencia_media", "adolescencia_tardia",
                               "adultez_emergente", "adultez", "adulto_mayor"),
  nivel_socioeconomico    = c("auto", "alto", "medio", "medio_bajo",
                               "bajo")
) {

  items_modo <- match.arg(items_modo)
  etapa_evolutiva <- match.arg(etapa_evolutiva)
  nivel_socioeconomico <- match.arg(nivel_socioeconomico)

  # Construir bloque de restricciones lexicas (vacio si auto/auto)
  bloque_lenguaje <- contexto_lenguaje(etapa_evolutiva,
                                        nivel_socioeconomico,
                                        idioma = idioma)
  options(SeMiLLa.bloque_lenguaje = bloque_lenguaje)
  on.exit(options(SeMiLLa.bloque_lenguaje = NULL), add = TRUE)

  idioma         <- match.arg(idioma)
  tipo_escala_respuesta <- match.arg(tipo_escala_respuesta)
  genero_victima <- match.arg(genero_victima)
  genero_agresor <- match.arg(genero_agresor)

  if (length(factores) < 2L)
    stop("Necesitas al menos 2 factores (= 2 historias).")
  if (!is.null(descripcion_factores) &&
      length(descripcion_factores) != length(factores))
    stop("'descripcion_factores' debe tener el mismo largo que 'factores'.")

  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  # ---- Auto-hint: si la poblacion incluye adolescentes/ninos, sugerir
  # prompts_historieta() para ilustrar las historias como comic strip ----
  if (isTRUE(verbose) && !is.null(poblacion)) {
    pop_lower <- tolower(poblacion)
    pat_jovenes <- paste0(
      "ni\u00f1[oa]s?|ninos?|ninas?|infantil|infantes?|adolescent[ea]s?|",
      "adolescenc|primaria|secundaria|preescolar|inicial|kinder|",
      "children|kids?|child|teen|teenager|youth|adolescent|elementary"
    )
    if (grepl(pat_jovenes, pop_lower, perl = TRUE)) {
      message(
        "\n[hint] Poblacion juvenil detectada en `poblacion = '", poblacion, "'`.",
        "\n       Considera generar HISTORIETAS visuales (comic strip) con:",
        "\n         prompts_historieta(escala_h, api_key, n_panels = 6, paleta = 'color')",
        "\n       cada historia se segmenta en N paneles con bocadillos y se obtiene",
        "\n       un prompt listo para Gemini/ChatGPT/Midjourney.\n"
      )
    }
  }

  if (verbose) {
    cat("\n[generar_escala_historias] Configurando cliente OpenAI...\n")
  }
  openai <- .configurar_openai(api_key)

  # ---------- 1. Introduccion comun ----------
  if (verbose) cat("[1/3] Generando introduccion comun...\n")
  introduccion <- .generar_introduccion_historias(
    openai = openai, modelo = modelo,
    agresor = agresor, victima = victima,
    agresor_colectivo = agresor_colectivo,
    genero_victima = genero_victima, genero_agresor = genero_agresor,
    contexto_relacion = contexto_relacion,
    poblacion = poblacion,
    max_palabras = max_palabras_introduccion,
    idioma = idioma
  )
  if (verbose) cat("  > ", introduccion, "\n", sep = "")

  # ---------- 2. K historias (una por factor) ----------
  if (verbose) cat("[2/3] Generando ", length(factores), " historias...\n",
                    sep = "")
  historias_df <- .generar_historias_por_factor(
    openai = openai, modelo = modelo,
    introduccion = introduccion,
    agresor = agresor, victima = victima,
    agresor_colectivo = agresor_colectivo,
    genero_victima = genero_victima, genero_agresor = genero_agresor,
    factores = factores,
    descripcion_factores = descripcion_factores,
    concepto = concepto,
    max_palabras = max_palabras_historia,
    idioma = idioma,
    verbose = verbose
  )

  # ---------- 3. Items de percepcion ----------
  if (items_modo == "transversal") {
    if (verbose) cat("[3/3] Generando ", n_items,
                      " items de percepcion (TRANSVERSALES, mismos para cada historia)...\n",
                      sep = "")
    items_df <- .generar_items_percepcion(
      openai = openai, modelo = modelo,
      n_items = n_items,
      facetas = facetas_percepcion,
      agresor = agresor, victima = victima,
      agresor_colectivo = agresor_colectivo,
      genero_victima = genero_victima, genero_agresor = genero_agresor,
      concepto = concepto,
      tipo_escala_respuesta = tipo_escala_respuesta,
      balance_polaridad = balance_polaridad,
      max_palabras = max_palabras_item,
      idioma = idioma,
      verbose = verbose
    )
    items_df$factor <- NA_character_
  } else {
    # POR HISTORIA: n_items items especificos para cada historia con framing
    # de propension (auto-identificacion, riesgo, normalizacion, prediccion)
    if (verbose) cat("[3/3] Generando ", n_items,
                      " items POR HISTORIA (framing de propension, ",
                      length(factores), " x ", n_items, " = ",
                      length(factores) * n_items, " items totales)...\n",
                      sep = "")
    items_lista <- list()
    for (i in seq_len(nrow(historias_df))) {
      f_i <- historias_df$factor[i]
      d_i <- historias_df$descripcion[i]
      t_i <- historias_df$texto[i]
      if (verbose) cat("  [", i, "/", nrow(historias_df), "] ", f_i, "\n", sep = "")
      items_i <- .generar_items_propension_por_historia(
        openai = openai, modelo = modelo,
        n_items = n_items,
        facetas = facetas_percepcion,
        victima = victima,
        factor_actual = f_i,
        descripcion_factor = d_i,
        texto_historia = t_i,
        concepto = concepto,
        contexto_propension = contexto_propension,
        tipo_escala_respuesta = tipo_escala_respuesta,
        max_palabras = max_palabras_item,
        idioma = idioma,
        verbose = verbose
      )
      items_i$factor <- f_i
      items_lista[[f_i]] <- items_i
    }
    items_df <- do.call(rbind, items_lista)
    rownames(items_df) <- NULL
    items_df$n_item <- seq_len(nrow(items_df))
  }

  resultado <- list(
    introduccion = introduccion,
    historias    = historias_df,
    items        = items_df,
    personajes   = list(agresor = agresor, victima = victima),
    concepto     = concepto,
    poblacion    = poblacion,
    idioma       = idioma,
    metadata     = list(
      modelo               = modelo,
      seed                 = seed,
      fecha                = format(Sys.Date()),
      n_factores           = length(factores),
      n_items              = nrow(items_df),
      facetas              = facetas_percepcion,
      tipo_escala_respuesta = tipo_escala_respuesta,
      balance_polaridad     = balance_polaridad
    )
  )
  class(resultado) <- c("semilla_historias", "list")

  if (verbose) {
    cat("\n[OK] Escala de historias generada:\n")
    cat("     Factores: ", paste(factores, collapse = ", "), "\n")
    cat("     Items   : ", nrow(items_df), " (",
        paste(table(items_df$faceta), collapse = " / "), " por faceta)\n",
        sep = "")
  }
  resultado
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.generar_introduccion_historias <- function(openai, modelo,
                                            agresor, victima,
                                            agresor_colectivo = FALSE,
                                            genero_victima = "auto",
                                            genero_agresor = "auto",
                                            contexto_relacion,
                                            poblacion,
                                            max_palabras,
                                            idioma) {

  gen_v <- .label_genero_es(genero_victima)
  gen_a <- .label_genero_es(genero_agresor)
  gen_v_concord <- .nota_concordancia_es(genero_victima, victima)

  if (idioma == "es") {
    bloque_leng <- getOption("SeMiLLa.bloque_lenguaje", "")
    bloque_leng_msg <- if (nzchar(bloque_leng))
      paste0("\n\n", bloque_leng) else ""

    sys_msg <- if (isTRUE(agresor_colectivo)) paste0(paste(
      "Eres un redactor experto en instrumentos psicometricos basados en",
      "historias breves. Generas una INTRODUCCION narrativa que presenta",
      "UNICAMENTE a la persona PROTAGONISTA y su contexto de vida. NO",
      "menciones ningun antagonista, conflicto, situacion negativa ni",
      "etiqueta moral. NO digas 'agresor', 'discriminador', 'victima',",
      "'incidente' ni nada parecido. La introduccion debe leerse como un",
      "PERFIL CORTO Y NEUTRO de la protagonista, suficiente para que luego",
      "varias historias distintas la situen en escenas concretas.",
      "Tercera persona, presente, espanol claro nivel basico-intermedio."
    ), bloque_leng_msg) else paste0(paste(
      "Eres un redactor experto en instrumentos psicometricos basados en",
      "historias breves (vignette-based scales). Generas una INTRODUCCION",
      "narrativa comun que presenta a dos personajes en una relacion y",
      "el contexto de esa relacion. La introduccion debe ser NEUTRA",
      "(sin describir ningun conflicto), terminar antes del incidente,",
      "y servir como prologo para varias historias distintas que comparten",
      "los mismos personajes. Usa tercera persona, presente o presente",
      "perfecto, espanol claro de nivel basico-intermedio."
    ), bloque_leng_msg)
    contexto_txt <- if (!is.null(contexto_relacion))
      paste0("Contexto: ", contexto_relacion, ".\n") else ""
    user_msg <- if (isTRUE(agresor_colectivo)) paste0(
      "Protagonista unica: ", victima, " (genero: ", gen_v, ").\n",
      contexto_txt,
      "Poblacion lectora: ", poblacion, ".\n",
      "Limite: ", max_palabras, " palabras.\n",
      gen_v_concord, "\n",
      "Devuelve SOLO el parrafo de introduccion, sin titulo ni comillas.",
      " NO menciones a nadie mas que a ", victima, "."
    ) else paste0(
      "Personajes: ", agresor, " (genero: ", gen_a, ") y ", victima,
      " (genero: ", gen_v, ").\n",
      contexto_txt,
      "Poblacion lectora: ", poblacion, ".\n",
      "Limite: ", max_palabras, " palabras.\n",
      gen_v_concord, "\n",
      "Devuelve UNICAMENTE el parrafo de introduccion, sin titulo ni comillas."
    )
  } else if (idioma == "en") {
    sys_msg <- paste(
      "You are an expert writer of vignette-based psychometric instruments.",
      "You produce a NEUTRAL narrative INTRODUCTION presenting two characters",
      "in a relationship and the context of that relationship. The intro must",
      "stop BEFORE any incident and serve as a shared prologue for several",
      "different short stories with the same characters."
    )
    contexto_txt <- if (!is.null(contexto_relacion))
      paste0("Relationship context: ", contexto_relacion, ".\n") else ""
    user_msg <- paste0(
      "Characters: ", agresor, " (male) and ", victima, " (female).\n",
      contexto_txt,
      "Target readers: ", poblacion, ".\n",
      "Word limit: ", max_palabras, ".\n",
      "Return ONLY the introduction paragraph, no title or quotes."
    )
  } else {
    sys_msg <- "Voce e um redator de instrumentos psicometricos baseados em historias."
    user_msg <- paste0(
      "Personagens: ", agresor, " e ", victima, ". Contexto: ",
      contexto_relacion %||% "", ". Limite: ", max_palabras, " palavras."
    )
  }

  txt <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 400L, temperature = 0.4
  )
  trimws(gsub("\\s+", " ", txt))
}


#' @keywords internal
.generar_historias_por_factor <- function(openai, modelo,
                                          introduccion,
                                          agresor, victima,
                                          agresor_colectivo = FALSE,
                                          genero_victima = "auto",
                                          genero_agresor = "auto",
                                          factores, descripcion_factores,
                                          concepto, max_palabras,
                                          idioma, verbose) {

  gen_v_concord <- .nota_concordancia_es(genero_victima, victima)

  if (is.null(descripcion_factores)) descripcion_factores <- factores

  textos <- character(length(factores))
  for (i in seq_along(factores)) {
    f    <- factores[i]
    desc <- descripcion_factores[i]

    if (idioma == "es") {
      bloque_leng <- getOption("SeMiLLa.bloque_lenguaje", "")
      bloque_leng_msg <- if (nzchar(bloque_leng))
        paste0("\n\n", bloque_leng) else ""

      sys_msg <- if (isTRUE(agresor_colectivo)) paste0(paste(
        "Eres un redactor experto en historias breves para instrumentos",
        "psicometricos. A partir de una INTRODUCCION ya dada (que solo",
        "presenta a la protagonista), generas el DESENLACE especifico que",
        "representa un FACTOR concreto del constructo. El desenlace debe",
        "describir CONDUCTAS OBSERVABLES de personajes secundarios CON",
        "NOMBRES PROPIOS PERUANOS NEUTROS (Sebasti\u00E1n, Carlos, Luc\u00EDa, Andr\u00E9s,",
        "etc.) propios del contexto de cada escena. NO uses etiquetas",
        "morales ('discriminacion', 'racismo', 'agresor', 'victima',",
        "'maltrato'). NO digas que es discriminacion: deja que el lector lo",
        "JUZGUE por las conductas. Tercera persona, tiempo pasado."
      ), bloque_leng_msg) else paste0(paste(
        "Eres un redactor experto en historias breves para instrumentos",
        "psicometricos. A partir de una INTRODUCCION dada, generas el",
        "DESENLACE especifico que representa un FACTOR concreto del",
        "constructo a medir. El desenlace debe describir conductas",
        "OBSERVABLES (no etiquetas) que ilustren claramente el factor,",
        "sin moralizar ni juzgar, sin usar las palabras 'violencia',",
        "'abuso' o 'maltrato', en tercera persona y tiempo pasado.",
        "Espanol claro nivel basico-intermedio."
      ), bloque_leng_msg)
      user_msg <- if (isTRUE(agresor_colectivo)) paste0(
        "Constructo (NO mencionarlo en el texto): ", concepto, ".\n",
        "Protagonista: ", victima, ".\n",
        gen_v_concord, "\n",
        "Introduccion ya escrita (NO repetirla): \"", introduccion, "\"\n",
        "FACTOR a representar: ", f, ".\n",
        "Definicion del factor: ", desc, ".\n",
        "Limite del DESENLACE: ", max_palabras, " palabras.\n",
        "Escribe SOLO el desenlace, con personajes secundarios con nombres",
        " propios apropiados al contexto. NO etiquetes lo que ocurre como",
        " discriminacion o racismo. Empieza con una accion concreta."
      ) else paste0(
        "Constructo: ", concepto, ".\n",
        "Personajes: ", agresor, " (agresor) y ", victima, " (receptora).\n",
        gen_v_concord, "\n",
        "Introduccion ya escrita (NO repetirla): \"", introduccion, "\"\n",
        "FACTOR a representar: ", f, ".\n",
        "Definicion del factor: ", desc, ".\n",
        "Limite del DESENLACE: ", max_palabras, " palabras.\n",
        "Escribe SOLO el desenlace (lo que sigue despues de la introduccion).",
        " No incluyas titulo, etiquetas ni comillas. Empieza con una accion",
        " concreta."
      )
    } else if (idioma == "en") {
      sys_msg <- paste(
        "You write short scenes for vignette-based psychometric instruments.",
        "Given a shared INTRODUCTION, you produce the SPECIFIC DEVELOPMENT",
        "that represents one factor of the construct. Describe OBSERVABLE",
        "behaviors (no labels), do not use 'violence', 'abuse' or 'mistreatment',",
        "third person past tense."
      )
      user_msg <- paste0(
        "Construct: ", concepto, ".\n",
        "Characters: ", agresor, " (perpetrator) and ", victima, " (receiver).\n",
        "Existing introduction (DO NOT repeat): \"", introduccion, "\"\n",
        "FACTOR: ", f, ".\n",
        "Factor description: ", desc, ".\n",
        "Word limit for the DEVELOPMENT: ", max_palabras, ".\n",
        "Return ONLY the development. Start with a concrete action."
      )
    } else {
      sys_msg <- "Redator de cenas para instrumentos psicometricos baseados em historias."
      user_msg <- paste0("Construto: ", concepto,
                          ". Fator: ", f, ". Limite: ", max_palabras, ".")
    }

    txt <- .llamar_openai(
      openai = openai,
      messages = list(
        list(role = "system", content = sys_msg),
        list(role = "user",   content = user_msg)
      ),
      modelo = modelo, max_tokens = 600L, temperature = 0.5
    )
    textos[i] <- trimws(gsub("\\s+", " ", txt))
    if (verbose) cat("  ", sprintf("%d/%d", i, length(factores)),
                      " [", f, "] ", substr(textos[i], 1, 70),
                      if (nchar(textos[i]) > 70) "..." else "", "\n", sep = "")
  }

  data.frame(
    factor      = factores,
    descripcion = descripcion_factores,
    texto       = textos,
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
.generar_items_percepcion <- function(openai, modelo,
                                       n_items, facetas,
                                       agresor, victima, concepto,
                                       tipo_escala_respuesta,
                                       balance_polaridad,
                                       max_palabras, idioma, verbose,
                                       agresor_colectivo = FALSE,
                                       genero_victima = "auto",
                                       genero_agresor = "auto") {

  # Si agresor_colectivo, sustituye el nombre del agresor en los items por
  # referencias neutras ("los demas", "la situacion", "lo ocurrido")
  ref_agresor   <- if (isTRUE(agresor_colectivo)) "los demas" else agresor
  gen_v_concord <- .nota_concordancia_es(genero_victima, victima)

  # Distribuir items entre facetas (lo mas uniforme posible)
  base <- n_items %/% length(facetas)
  resto <- n_items - base * length(facetas)
  por_faceta <- rep(base, length(facetas)) + c(rep(1, resto),
                                                 rep(0, length(facetas) - resto))
  names(por_faceta) <- facetas

  # Definir polaridad esperada por faceta:
  # "directa": acuerdo alto = postura SANA (reconoce, valida, percibe gravedad)
  # "inversa": acuerdo alto = postura PROBLEMATICA (justifica, minimiza, culpa)
  facetas_dir <- c("severidad", "responsabilidad_agresor",
                    "reconocimiento", "validacion_victima", "intervencion")
  # Las facetas inversas: justificacion, normalizacion, culpa_victima,
  # minimizacion (que combina minimizar+justificar).

  out_items   <- character(n_items)
  out_faceta  <- character(n_items)
  out_polar   <- character(n_items)
  k <- 1L

  for (faceta in facetas) {
    n_f <- por_faceta[[faceta]]
    if (n_f == 0L) next

    polaridad_natural <- if (faceta %in% facetas_dir) "directa" else "inversa"

    if (idioma == "es") {
      regla_colectiva <- if (isTRUE(agresor_colectivo)) paste(
        "",
        "REGLA EXTRA DE CAMUFLAJE: NO nombres al agresor (no inventes nombres",
        "para quienes hicieron la accion, no digas 'el grupo', 'el grupo",
        "discriminador', 'el agresor', 'los agresores'). Usa SOLO referencias",
        "neutras: 'lo ocurrido', 'la situacion', 'lo que paso', 'los demas',",
        "'las personas presentes', 'el trato recibido'. Asi el item no",
        "prejuzga moralmente y deja que el respondiente decida si fue",
        "discriminacion. El UNICO nombre propio que puede aparecer es el de",
        "la receptora (", victima, ")."
      ) else ""
      sys_msg <- paste(
        "Eres un redactor experto en instrumentos psicometricos basados",
        "en historias. Generas items de PERCEPCION en TERCERA PERSONA que",
        "el respondiente contestara DESPUES de leer UNA de varias historias",
        "posibles sobre los mismos personajes.",
        "",
        "REGLA CRITICA DE TRANSVERSALIDAD: los mismos items se aplicaran",
        "a CUALQUIER historia del set. Por lo tanto los items DEBEN ser",
        "ABSTRACTOS y referirse a 'lo que hizo X', 'la conducta de X',",
        "'lo ocurrido', 'su reaccion', 'su comportamiento', 'la situacion'.",
        "NUNCA mencionen la conducta o el contexto especifico de una",
        "historia particular (no digas 'controlar', 'revisar el celular',",
        "'celarla', 'humillar', 'seguirla', 'monitorear', 'comentario sobre",
        "la apariencia', 'guardia de seguridad', 'reunion virtual', etc.).",
        "Si tu item solo tiene sentido para una historia particular, esta",
        "MAL escrito. Reescribelo en abstracto.",
        regla_colectiva,
        "",
        "Espanol claro, una sola oracion declarativa por item, sin signos",
        "de pregunta. NO uses 'violencia', 'abuso' ni 'maltrato'."
      )
      faceta_def <- switch(faceta,
        "severidad"      = "Que tan grave o serio fue LO QUE HIZO el agresor (en abstracto, sin nombrar la conducta).",
        "justificacion"  = "Si el agresor tenia derecho o razones legitimas para actuar COMO ACTUO (en abstracto).",
        "normalizacion"  = "Si LO OCURRIDO es algo normal, comun o esperable entre parejas / en la sociedad.",
        "culpa_victima"  = "Si la victima provoco o es responsable de la SITUACION/REACCION del agresor.",
        "responsabilidad_agresor" = "Si el agresor es responsable de la SITUACION/SU PROPIA REACCION.",
        "reconocimiento" = "Si la situacion descrita constituye discriminacion / un acto injusto / un acto problematico (capacidad de identificar lo ocurrido como tal).",
        "minimizacion"   = "Si lo ocurrido NO es importante, NO es para tanto, fue exagerado o malinterpretado por la victima (combina minimizar + justificar).",
        "validacion_victima" = "Si la incomodidad, molestia o queja de la VICTIMA es legitima, comprensible y razonable.",
        "intervencion"   = "Si las personas presentes (incluida la victima) deberian intervenir, denunciar, corregir o apoyar tras lo ocurrido.",
        paste("Faceta:", faceta)
      )
      polaridad_msg <- if (polaridad_natural == "directa")
        "Polaridad: ACUERDO alto significa MAYOR percepcion sana (e.g. mayor gravedad)."
      else
        "Polaridad: ACUERDO alto significa MAYOR justificacion / normalizacion / culpabilizacion (postura PROBLEMATICA hacia el agresor o hacia la victima)."

      ejemplos_buenos <- switch(faceta,
        "severidad"      = paste0("Lo que hizo ", ref_agresor, " es algo serio que afecta a ", victima, ". | La conducta de ", ref_agresor, " es preocupante. | ", victima, " tendria motivos para preocuparse por lo ocurrido."),
        "justificacion"  = paste0(ref_agresor, " tenia derecho a actuar como lo hizo. | Es comprensible que ", ref_agresor, " haya reaccionado asi. | Lo que hizo ", ref_agresor, " se justifica por el contexto."),
        "normalizacion"  = paste0("Lo ocurrido es algo normal entre personas. | Otras personas pasan por situaciones similares sin problema. | Lo que hizo ", ref_agresor, " no deberia sorprender a nadie."),
        "culpa_victima"  = paste0(victima, " contribuyo a lo ocurrido por su forma de comportarse. | ", victima, " tiene parte de responsabilidad por la reaccion de ", ref_agresor, ". | Si ", victima, " se hubiera comportado distinto, esto no habria pasado."),
        "responsabilidad_agresor" = paste0(ref_agresor, " son los unicos responsables de lo ocurrido. | ", ref_agresor, " deberian asumir la responsabilidad de su reaccion."),
        "reconocimiento" = paste0("La situacion descrita constituye un acto injusto hacia ", victima, ". | Lo ocurrido refleja un trato problematico hacia ", victima, ". | El trato recibido por ", victima, " es injusto."),
        "minimizacion"   = paste0(victima, " esta exagerando con lo ocurrido. | Lo que paso no es para tanto. | Lo ocurrido fue solo un malentendido. | ", victima, " esta interpretando mal la situacion."),
        "validacion_victima" = paste0("La incomodidad de ", victima, " esta justificada. | ", victima, " tiene derecho a sentirse afectado/a por lo ocurrido. | Es comprensible que ", victima, " considere lo ocurrido como problematico."),
        "intervencion"   = paste0("Las personas presentes debieron intervenir o corregir la situacion. | Lo ocurrido deberia ser reportado a una autoridad responsable. | Alguien debio apoyar a ", victima, "."),
        ""
      )

      uso_nombres_msg <- if (isTRUE(agresor_colectivo))
        paste0("usando SOLO el nombre ", victima,
                " y referencias neutras como 'lo ocurrido', 'la situacion',",
                " 'los demas' o 'las personas presentes' (NO inventes nombres",
                " para el agresor)")
      else
        paste0("usando los nombres ", agresor, " y/o ", victima)

      user_msg <- paste0(
        "Personajes: agresor = ", ref_agresor, ", receptora = ", victima, "\n",
        gen_v_concord, "\n",
        "Faceta a evaluar: ", faceta, " - ", faceta_def, "\n",
        polaridad_msg, "\n",
        "Tipo de escala de respuesta: ", tipo_escala_respuesta, "\n\n",
        "Ejemplos del estilo correcto (abstracto, transversal):\n",
        ejemplos_buenos, "\n\n",
        "Ejemplos INCORRECTOS (NO hagas esto):\n",
        "- '", ref_agresor, " actuo de manera grave al revisar el celular' (menciona 'revisar celular')\n",
        "- '", ref_agresor, " tiene razon al sentirse celoso' (menciona 'celoso')\n",
        "- 'Es comprensible que ", ref_agresor, " controle a ", victima, "' (menciona 'controlar')\n\n",
        "Genera EXACTAMENTE ", n_f, " items distintos para esta faceta.\n",
        "Cada item: una sola oracion declarativa, maximo ", max_palabras,
        " palabras, en tercera persona, ", uso_nombres_msg,
        ", SIN nombrar conductas especificas.\n",
        "Devuelve cada item en una linea numerada (1., 2., ...). Sin titulos ni comillas."
      )
    } else if (idioma == "en") {
      sys_msg <- paste(
        "You write perception items for vignette-based psychometric scales.",
        "Items express the respondent's JUDGMENT about ONE of several possible",
        "stories about the SAME characters.",
        "",
        "CRITICAL TRANSVERSALITY RULE: the same items will be applied to ANY",
        "story (control, jealousy, humiliation, isolation, etc.). Items MUST",
        "be ABSTRACT and refer to 'what X did', 'X's behavior', 'what happened',",
        "'X's reaction'. NEVER mention the specific behavior (do not say",
        "'control', 'check the phone', 'jealous', 'humiliate', 'isolate', etc.).",
        "If your item only makes sense for one specific story, it is WRONG.",
        "",
        "One declarative sentence per item, third person, no questions.",
        "Do not use 'violence', 'abuse', 'mistreatment'."
      )
      faceta_def <- switch(faceta,
        "severidad"     = "How serious WHAT THE PERPETRATOR DID was (abstract).",
        "justificacion" = "Whether the perpetrator had the right or reasons to act THE WAY HE DID (abstract).",
        "normalizacion" = "Whether WHAT HAPPENED is normal or expected in a relationship.",
        "culpa_victima" = "Whether the victim caused or is responsible for the SITUATION / the perpetrator's REACTION.",
        "responsabilidad_agresor" = "Whether the perpetrator is responsible for the situation/his own reaction.",
        paste("Facet:", faceta)
      )
      user_msg <- paste0(
        "Characters: perpetrator = ", agresor, ", receiver = ", victima, "\n",
        "Facet: ", faceta, " - ", faceta_def, "\n",
        "Response scale: ", tipo_escala_respuesta, "\n",
        "Generate EXACTLY ", n_f, " distinct items for this facet.\n",
        "Each item: one declarative sentence, max ", max_palabras, " words,",
        " third person, using the names ", agresor, " and/or ", victima, ",",
        " WITHOUT naming specific behaviors.\n",
        "Return numbered lines (1., 2., ...). No titles, no quotes."
      )
    } else {
      sys_msg <- "Redator de itens de percepcao para escalas baseadas em historias."
      user_msg <- paste0("Construto: ", concepto, ". Faceta: ", faceta,
                          ". Quantidade: ", n_f, ".")
    }

    raw <- .llamar_openai(
      openai = openai,
      messages = list(
        list(role = "system", content = sys_msg),
        list(role = "user",   content = user_msg)
      ),
      modelo = modelo, max_tokens = 700L, temperature = 0.5
    )

    # Parsear lineas numeradas: "1. xxx", "2. xxx", ...
    lineas <- unlist(strsplit(raw, "\n", fixed = TRUE))
    lineas <- trimws(lineas)
    lineas <- lineas[nzchar(lineas)]
    lineas <- sub("^[0-9]+[.\\)]\\s*", "", lineas, perl = TRUE)
    lineas <- sub("^[-*]\\s*", "", lineas)
    lineas <- lineas[nzchar(lineas)]

    if (length(lineas) < n_f) {
      warning("La faceta '", faceta, "' devolvio ", length(lineas),
              " items (esperados ", n_f, "). Se usa lo disponible.")
      n_eff <- length(lineas)
    } else {
      n_eff <- n_f
      lineas <- lineas[seq_len(n_f)]
    }

    if (n_eff > 0L) {
      idx <- seq.int(k, length.out = n_eff)
      out_items[idx]  <- lineas
      out_faceta[idx] <- faceta
      out_polar[idx]  <- polaridad_natural
      k <- k + n_eff
    }
    if (verbose) cat("  faceta '", faceta, "': ", n_eff,
                      " items generados\n", sep = "")
  }

  # Recortar si quedo algun slot vacio por warnings
  if (k <= n_items) {
    out_items  <- out_items[seq_len(k - 1L)]
    out_faceta <- out_faceta[seq_len(k - 1L)]
    out_polar  <- out_polar[seq_len(k - 1L)]
  }

  data.frame(
    n_item    = seq_along(out_items),
    item      = out_items,
    faceta    = out_faceta,
    polaridad = out_polar,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_historias <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Escala basada en HISTORIAS (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Idioma     : ", x$idioma, "\n", sep = "")
  cat("  Personajes : ", x$personajes$agresor, " (agresor) / ",
      x$personajes$victima, " (receptora)\n", sep = "")
  cat("  Factores   : ", paste(x$historias$factor, collapse = ", "), "\n",
      sep = "")
  cat("  N items    : ", nrow(x$items), " (",
      paste(table(x$items$faceta), collapse = " / "), " por faceta)\n",
      sep = "")
  cat("-----------------------------------------------------------\n")
  cat("  Introduccion:\n")
  cat("    ", x$introduccion, "\n", sep = "")
  cat("\n  Historias:\n")
  for (i in seq_len(nrow(x$historias))) {
    cat("    [", x$historias$factor[i], "] ",
        substr(x$historias$texto[i], 1, 90),
        if (nchar(x$historias$texto[i]) > 90) "..." else "", "\n", sep = "")
  }
  cat("\n  Items (primeros 5):\n")
  for (i in seq_len(min(5, nrow(x$items)))) {
    cat("    ", sprintf("%2d", x$items$n_item[i]),
        " [", x$items$faceta[i], "/", substr(x$items$polaridad[i], 1, 3), "] ",
        x$items$item[i], "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}


# =============================================================================
# Helpers de genero (concordancia gramatical en espanol)
# =============================================================================

#' @keywords internal
.label_genero_es <- function(g) {
  switch(g,
    "auto"       = "inferir del nombre",
    "masculino"  = "MASCULINO",
    "femenino"   = "FEMENINO",
    "neutro"     = "no binario / neutro",
    g
  )
}

#' @keywords internal
.nota_concordancia_es <- function(genero, nombre) {
  if (genero == "auto") return("")
  if (genero == "masculino") {
    paste0("CONCORDANCIA GRAMATICAL: ", nombre, " es MASCULINO. Usa siempre",
           " 'el', 'lo', 'al', 'su', 'afectado', 'incomodo', 'comprensible",
           " para el', 'sentirse afectado', 'incomodo', 'molesto'. NO uses",
           " 'la', 'ella', 'afectada', 'incomoda', 'molesta', 'sentirse",
           " afectada'. NUNCA uses formas con barra ('afectado/a'); decide",
           " masculino siempre.")
  } else if (genero == "femenino") {
    paste0("CONCORDANCIA GRAMATICAL: ", nombre, " es FEMENINO. Usa siempre",
           " 'la', 'ella', 'su', 'afectada', 'incomoda', 'comprensible",
           " para ella', 'sentirse afectada', 'incomoda', 'molesta'. NO",
           " uses 'el', 'lo', 'afectado', 'incomodo', 'molesto'. NUNCA uses",
           " formas con barra ('afectado/a'); decide femenino siempre.")
  } else {  # neutro
    paste0("CONCORDANCIA GRAMATICAL: ", nombre, " es no binario. Usa lenguaje",
           " neutro: 'sentirse afectado/a', 'incomodo/a', 'su',",
           " 'comprensible para esta persona'.")
  }
}


# =============================================================================
# .generar_items_propension_por_historia
#   Genera items perceptivos NSSI-ESPECIFICOS por historia, con framing
#   proyectivo (auto-identificacion, riesgo, normalizacion, prediccion).
#   A diferencia de .generar_items_percepcion (PDVS, transversal), aqui los
#   items hacen referencia al CONTENIDO ESPECIFICO de la historia y al
#   constructo de propension, no a un agresor externo.
# =============================================================================

#' @keywords internal
.generar_items_propension_por_historia <- function(
    openai, modelo,
    n_items, facetas,
    victima, factor_actual, descripcion_factor, texto_historia,
    concepto, contexto_propension = NULL,
    tipo_escala_respuesta = "acuerdo",
    max_palabras = 18L,
    idioma = "es",
    verbose = TRUE) {

  # Distribuir n_items entre facetas (lo mas uniforme posible)
  base       <- n_items %/% length(facetas)
  resto      <- n_items - base * length(facetas)
  por_faceta <- rep(base, length(facetas)) +
                  c(rep(1, resto), rep(0, length(facetas) - resto))
  names(por_faceta) <- facetas

  # Polaridad por faceta (NSSI propension framing)
  facetas_inv <- c("normalizacion", "normalizacion_pares", "minimizacion")

  out_items  <- character(0)
  out_faceta <- character(0)
  out_polar  <- character(0)

  ctx_prop <- if (!is.null(contexto_propension)) paste0(
    "\nCONTEXTO DEL CONSTRUCTO DE PROPENSION:\n", contexto_propension, "\n"
  ) else ""

  for (faceta in facetas) {
    n_f <- por_faceta[[faceta]]
    if (n_f == 0L) next

    polaridad <- if (faceta %in% facetas_inv) "inversa" else "directa"

    if (idioma == "es") {

      bloque_leng <- getOption("SeMiLLa.bloque_lenguaje", "")
      bloque_leng_msg <- if (nzchar(bloque_leng))
        paste0("\n\n", bloque_leng) else ""

      sys_msg <- paste(
        "Eres un redactor experto en items perceptivos para escalas",
        "psicometricas en formato de historias / vinetas, con framing",
        "PROYECTIVO. El respondiente ha leido una historia breve sobre",
        "una protagonista (", victima, ") en una situacion concreta. Tu",
        "tarea es generar items que el respondiente contestara DESPUES",
        "de leer ESA historia particular.",
        "",
        "FRAMING PROYECTIVO (IMPORTANTE):",
        "Los items NO miden juicios morales sobre un agresor externo.",
        "Miden la PROYECCION del respondiente sobre la protagonista, su",
        "auto-identificacion con lo que ella siente o piensa, su prediccion",
        "sobre lo que podria pasarle a ella, y el grado en que normaliza la",
        "situacion. Algunos items invitan al respondiente a hablar de SI",
        "MISMO ('yo me he sentido asi'); otros lo invitan a juzgar la",
        "situacion de ", victima, " en abstracto.",
        "",
        "REGLA DE ESPECIFICIDAD: los items DEBEN referirse al contenido",
        "concreto de la historia que se acaba de leer (estados emocionales,",
        "pensamientos o exposiciones particulares). Pueden retomar ideas",
        "de la historia (vacio, rabia contenida, autocastigo, marcas, redes",
        "sociales, etc.) pero SIN copiar frases textuales.",
        "",
        "REGLA DE LENGUAJE: tercera persona para juicios sobre ", victima,
        ", primera persona ('yo', 'me', 'mi') para items de auto-",
        "identificacion. Espanol claro, una sola oracion declarativa por",
        "item, sin signos de pregunta.",
        "",
        "EVITAR: terminos clinicos ('NSSI', 'autolesion', 'cutting'). En",
        "lugar de 'autolesion' usar 'hacerse dano', 'lastimarse', 'cortarse'.",
        "Las facetas de auto-identificacion y prediccion conductual SI",
        "pueden mencionar dano fisico/cortes; las de empatia y normalizacion",
        "deben mantenerse mas generales.",
        bloque_leng_msg
      )

      # ---- Buscar anclaje empirico en biblioteca de facetas validadas ----
      faceta_validada <- .faceta_validada_a_prompt(faceta, victima)

      faceta_def <- switch(faceta,
        "auto_identificacion"   = paste0("Grado en que el respondiente se",
            " identifica personalmente con lo que siente o piensa ", victima,
            " en esta historia. Items en PRIMERA PERSONA: 'A mi me ha pasado',",
            " 'Yo tambien siento', 'Yo me he sentido asi'."),
        "validacion_victima"    = paste0("Empatia y comprension hacia ",
            victima, ". El respondiente reconoce que lo que siente ", victima,
            " es valido o comprensible. Items en TERCERA PERSONA sobre ",
            victima, "."),
        "reconocimiento_riesgo" = paste0("Reconocimiento de que la situacion",
            " de ", victima, " constituye una senal de alerta que podria",
            " llevarla a hacerse dano fisico. Items en tercera persona,",
            " mencionando explicitamente la posibilidad de dano fisico."),
        "reconocimiento"        = paste0("Reconocimiento de que la situacion",
            " de ", victima, " constituye una senal de alerta que podria",
            " llevarla a hacerse dano fisico. Items en tercera persona."),
        "normalizacion_pares"   = paste0("(POLARIDAD INVERSA) Grado en que",
            " el respondiente percibe que lo que vive ", victima, " es comun",
            " o normal entre adolescentes de su edad. Items en tercera",
            " persona, sin mencionar dano fisico explicito."),
        "normalizacion"         = paste0("(POLARIDAD INVERSA) Grado en que",
            " el respondiente percibe que lo que vive ", victima, " es comun",
            " o normal entre adolescentes de su edad."),
        "prediccion_conductual" = paste0("Anticipacion de que ", victima,
            " (o el propio respondiente en su lugar) podria terminar",
            " haciendose dano fisico si la situacion sigue. Items mixtos:",
            " sobre ", victima, " y sobre 'yo en su lugar'."),
        "intervencion"          = paste0("Necesidad de que alguien acompane",
            ", apoye o intervenga para proteger a ", victima, " de hacerse",
            " dano. Items en tercera persona."),
        paste("Faceta:", faceta)
      )

      polaridad_msg <- if (polaridad == "directa")
        paste0("Polaridad: ACUERDO alto = MAYOR identificacion / mayor",
               " reconocimiento de riesgo / mayor prediccion de dano /",
               " mayor empatia (postura clinicamente relevante de propension).")
      else
        paste0("Polaridad: ACUERDO alto = MAYOR normalizacion / minimizacion",
               " (postura de desensibilizacion social, tambien clinicamente",
               " relevante).")

      # Si la faceta esta en la biblioteca validada, sobrescribir definicion
      if (!is.null(faceta_validada)) {
        faceta_def <- paste0(
          faceta_validada$definicion,
          "\n\nINSTRUMENTO(S) DE ORIGEN: ", faceta_validada$instrumento,
          "\nFUENTE: ", faceta_validada$fuentes,
          "\nPERSONA RECOMENDADA: ", faceta_validada$persona,
          ".\nPOLARIDAD: ", faceta_validada$polaridad, "."
        )
        polaridad <- faceta_validada$polaridad
        persona_f <- faceta_validada$persona

        # Regla de framing condicional segun persona recomendada
        if (grepl("3a", persona_f, ignore.case = TRUE) ||
            grepl("tercera", persona_f, ignore.case = TRUE)) {
          # Tercera persona pura - juicio empatico sobre la protagonista
          sys_msg <- paste(
            sys_msg,
            "",
            "REGLA CRITICA DE PERSPECTIVA - 3a PERSONA (faceta validada):",
            "Esta faceta DEBE generar items en TERCERA PERSONA. El",
            "respondiente proyecta sus propias cogniciones sobre ", victima,
            " sin auto-revelar. NUNCA usar 'yo tambien', 'a mi tambien',",
            "'si yo estuviera'. Los items deben juzgar la situacion de ",
            victima, " desde fuera.",
            "",
            "Patrones permitidos:",
            "  (a) 'Es comprensible que ", victima, " [crea/sienta/piense] [X]'",
            "  (b) 'Lo que [piensa/siente/cree] ", victima, " sobre [X] refleja [Y]'",
            "  (c) 'Tiene logica / sentido que ", victima, " [X]'",
            "  (d) 'Para ", victima, ", [X] seria [Y]'",
            "",
            "EJEMPLO INCORRECTO (1a persona en faceta de 3a):",
            "  'Yo tambien he sentido eso de que mi cuerpo deberia pagar'",
            "EJEMPLO CORRECTO (3a persona empatica, anclada en historia):",
            "  'Es comprensible que ", victima, " crea que su cuerpo deberia",
            "  pagar por sus errores'",
            "",
            "Cada item DEBE retomar UN elemento concreto de la historia.",
            "El item NO debe ser una simple repeticion de lo que ya dice la",
            "historia: debe agregar un juicio (comprension, validez,",
            "logica, sentido)."
          )
        } else {
          # Primera persona o mixta - proyeccion auto-referencial
          sys_msg <- paste(
            sys_msg,
            "",
            "REGLA CRITICA DE PERSPECTIVA - 1a PERSONA / PROYECTIVA",
            "(faceta validada): Esta faceta DEBE generar items en PRIMERA",
            "PERSONA con conexion proyectiva al contenido de la historia.",
            "NUNCA usar items free-standing tipo 'a veces siento X',",
            "'tengo impulsos de Y'. NUNCA usar items descriptivos sobre ",
            victima, ".",
            "",
            "Patrones permitidos:",
            "  (a) 'Yo tambien he sentido / pensado / creido eso de [X]'",
            "      donde X retoma un contenido CONCRETO de la historia.",
            "  (b) 'A mi tambien me ha pasado / se me ha cruzado / lo he",
            "      pensado igual' refiriendose a algo de la historia.",
            "  (c) 'Si yo estuviera como ", victima, " en esta situacion,",
            "      podria sentir / pensar / creer [X]'.",
            "",
            "EJEMPLO INCORRECTO (free-standing, sin proyeccion):",
            "  'A veces siento impulsos repentinos de apretar mi piel'",
            "EJEMPLO CORRECTO (proyectivo, anclado en historia):",
            "  'Yo tambien he sentido eso de apretar mi piel con fuerza',",
            "",
            "Cada item DEBE retomar UN elemento concreto de la historia."
          )
        }
      }

      ejemplos <- switch(faceta,
        "auto_identificacion" = paste0(
          "1. Yo me he sentido como se siente ", victima, " en esta situacion.\n",
          "2. A mi me han pasado cosas parecidas a las de ", victima, ".\n",
          "3. Lo que piensa ", victima, " aqui me resulta familiar."),
        "validacion_victima" = paste0(
          "1. Es comprensible que ", victima, " se sienta asi en esta situacion.\n",
          "2. Lo que siente ", victima, " tiene sentido despues de lo vivido.\n",
          "3. Tiene logica que ", victima, " reaccione de esta manera."),
        "reconocimiento_riesgo" = paste0(
          "1. Lo que vive ", victima, " podria llevarla a hacerse dano fisico.\n",
          "2. Esta situacion es una senal de alerta para ", victima, ".\n",
          "3. Lo que siente ", victima, " puede terminar en hacerse dano."),
        "reconocimiento" = paste0(
          "1. Lo que vive ", victima, " podria llevarla a hacerse dano fisico.\n",
          "2. Esta situacion es una senal de alerta para ", victima, "."),
        "normalizacion_pares" = paste0(
          "1. Lo que le pasa a ", victima, " es comun entre adolescentes de su edad.\n",
          "2. Esto que vive ", victima, " le pasa a muchos chicos y chicas.\n",
          "3. Es normal sentirse como ", victima, " a esa edad."),
        "normalizacion" = paste0(
          "1. Lo que le pasa a ", victima, " es comun entre adolescentes.\n",
          "2. Es normal sentirse asi a esa edad."),
        "prediccion_conductual" = paste0(
          "1. Si ", victima, " sigue sintiendose asi, podria terminar haciendose dano.\n",
          "2. Estando en el lugar de ", victima, ", yo podria pensar lo mismo.\n",
          "3. Es probable que ", victima, " termine lastimandose si nadie la ayuda."),
        "intervencion" = paste0(
          "1. Alguien deberia acompanar a ", victima, " despues de esta situacion.\n",
          "2. Seria importante que alguien apoye a ", victima, " ahora."),
        ""
      )

      # Si la faceta es de biblioteca validada, usar SUS ejemplos originales
      if (!is.null(faceta_validada)) {
        ejemplos <- faceta_validada$ejemplos
      }

      user_msg <- paste0(
        "PROTAGONISTA: ", victima, "\n",
        "FACTOR DE PROPENSION QUE REPRESENTA ESTA HISTORIA: ", factor_actual, "\n",
        "DESCRIPCION DEL FACTOR: ", descripcion_factor, "\n\n",
        "TEXTO DE LA HISTORIA QUE EL RESPONDIENTE ACABA DE LEER:\n",
        texto_historia, "\n",
        ctx_prop,
        "\nFACETA A EVALUAR: ", faceta, " - ", faceta_def, "\n",
        polaridad_msg, "\n",
        "Tipo de escala de respuesta: ", tipo_escala_respuesta, "\n\n",
        "EJEMPLOS DEL ESTILO CORRECTO:\n", ejemplos, "\n\n",
        "Genera EXACTAMENTE ", n_f, " items distintos para esta faceta,",
        " referidos al CONTENIDO ESPECIFICO de la historia anterior.\n",
        "Cada item: una sola oracion declarativa, maximo ", max_palabras,
        " palabras.\n",
        "Devuelve cada item en una linea numerada (1., 2., ...). Sin",
        " titulos, sin comillas, sin explicaciones adicionales."
      )

    } else {
      stop("Idioma no soportado en items_modo='por_historia'. Usa idioma='es'.")
    }

    raw <- .llamar_openai(
      openai = openai,
      messages = list(
        list(role = "system", content = sys_msg),
        list(role = "user",   content = user_msg)
      ),
      modelo = modelo, max_tokens = 700L, temperature = 0.5
    )

    # Parsear lineas numeradas (mismo patron que .generar_items_percepcion)
    lineas <- unlist(strsplit(raw, "\n", fixed = TRUE))
    lineas <- trimws(lineas)
    lineas <- lineas[nzchar(lineas)]
    lineas <- sub("^[0-9]+[.\\)]\\s*", "", lineas, perl = TRUE)
    lineas <- sub("^[-*]\\s*", "", lineas)
    lineas <- lineas[nzchar(lineas)]

    if (length(lineas) < n_f) {
      warning("La faceta '", faceta, "' devolvio ", length(lineas),
              " items (esperados ", n_f, "). Se usa lo disponible.")
    } else {
      lineas <- lineas[seq_len(n_f)]
    }

    out_items  <- c(out_items,  lineas)
    out_faceta <- c(out_faceta, rep(faceta, length(lineas)))
    out_polar  <- c(out_polar,  rep(polaridad, length(lineas)))

    if (verbose) cat("       faceta '", faceta, "': ", length(lineas),
                     " items generados\n", sep = "")
  }

  data.frame(
    n_item    = seq_along(out_items),
    item      = out_items,
    faceta    = out_faceta,
    polaridad = out_polar,
    stringsAsFactors = FALSE
  )
}
