# =============================================================================
# SeMiLLa - prompts_historieta()
#
# Convierte cada historia de un objeto `semilla_historias` en un prompt
# listo para pegar en un modelo texto-a-imagen (Gemini, ChatGPT, Midjourney,
# Stable Diffusion). El prompt construye una HISTORIETA / COMIC STRIP de N
# paneles con la protagonista recurrente, titulos por panel, dialogos y
# pensamientos en bocadillos, y escalada emocional progresiva.
#
# Solo cobra LLM (no imagen): el LLM segmenta la narrativa en N paneles y
# describe cada uno; el usuario corre el prompt en su modelo visual preferido.
#
# Autor: Dr. Jose Ventura-Leon
# Fecha: 2026-05-03
# =============================================================================

#' @title Generar prompts para historietas (comic strip) de cada historia
#'
#' @description
#' Para cada historia de un objeto `semilla_historias`, construye un prompt de
#' historieta de N paneles listo para pegar en un modelo texto-a-imagen.
#' Cada panel incluye: titulo en caja, descripcion visual de la escena,
#' bocadillos de dialogo y de pensamiento. La protagonista mantiene
#' consistencia visual a lo largo de los paneles. Util para tamizajes
#' proyectivos con adolescentes con baja comprension lectora o para
#' versiones ilustradas de instrumentos basados en historias (vignette tests).
#'
#' Solo cobra LLM: el LLM segmenta la historia en paneles. Las imagenes
#' finales las genera el USUARIO copiando los prompts en Gemini/ChatGPT/etc.
#'
#' @param escala_h Objeto de clase `semilla_historias`.
#' @param api_key Clave de OpenAI.
#' @param n_panels Numero de paneles por historia (default 6, rango 4-9).
#' @param layout Disposicion: `"2x3"` (default, 2 filas x 3 columnas),
#'   `"3x2"`, `"3x3"`, `"1xN"` (una fila), `"Nx1"` (una columna).
#' @param paleta `"color"` (default, acuarela calida estilo Gemini) o `"bn"`
#'   (line art blanco y negro).
#' @param estilo Cadena con el estilo visual. NULL = default segun paleta.
#' @param protagonista_visual Ficha visual del personaje. NULL = se construye
#'   automaticamente desde `escala_h$personajes` y `escala_h$poblacion`.
#' @param idioma_prompts `"en"` (default, mejor calidad en modelos visuales)
#'   o `"es"`.
#' @param idioma_bocadillos `"es"` (default) o `"en"`. Idioma del texto que
#'   aparece DENTRO de los bocadillos en la imagen.
#' @param modelo Modelo OpenAI para segmentacion narrativa
#'   (default `"gpt-4.1-mini-2025-04-14"`).
#' @param seed Semilla para reproducibilidad.
#' @param archivo Ruta SIN extension. Si se pasa, escribe Excel con prompts
#'   y instrucciones de uso.
#' @param verbose Mostrar progreso.
#' @param safe_mode `TRUE` (default) activa contingencias anti-bloqueo de
#'   modelos texto-a-imagen (Gemini, Imagen, ChatGPT, etc.) cuando el
#'   contenido aborda temas sensibles. Inyecta un bloque "Intent" educativo,
#'   refuerza manga larga / sin marcas en piel / sin objetos cortantes en la
#'   ficha de protagonista y reglas de consistencia, y sanitiza descripciones
#'   visuales que mencionen cicatrices, cortes, sangre, heridas u objetos
#'   peligrosos. `FALSE` desactiva todas estas restricciones (comportamiento
#'   v1).
#' @param tema_sensible Vector de temas sensibles activados (`"autolesion"`,
#'   `"suicidio"`, `"violencia"`, `"abuso_sexual"`, `"trastorno_alimentario"`,
#'   `"consumo"`). Si es `NULL` (default), se auto-detecta a partir de los
#'   factores y textos de las historias. Solo tiene efecto si `safe_mode = TRUE`.
#' @param intent_clinico Lógico. Si es `TRUE` inserta un bloque "Intent /
#'   Intencion" al inicio del prompt declarando uso clinico/educativo y la
#'   lista explicita de elementos visuales prohibidos. `NULL` (default) =
#'   autodetectar: `TRUE` cuando se detecto al menos un tema sensible.
#'
#' @return Data.frame con clase `semilla_prompts_historieta` y columnas:
#'   `factor`, `historia_texto`, `n_panels`, `prompt`. Cada fila es una
#'   historia con su prompt de historieta completo.
#'
#' @examples
#' \dontrun{
#' h <- prompts_historieta(
#'   escala_h        = mi_escala_h,
#'   api_key         = api_key,
#'   n_panels        = 6,
#'   paleta          = "color",
#'   idioma_prompts  = "en",
#'   archivo         = "historietas_EPNA"
#' )
#' cat(h$prompt[1])  # primer prompt listo para pegar en Gemini
#' }
#'
#' @export
prompts_historieta <- function(escala_h,
                                api_key,
                                n_panels             = 6L,
                                layout               = c("2x3","3x2","3x3","1xN","Nx1"),
                                paleta               = c("color","bn"),
                                estilo               = NULL,
                                protagonista_visual  = NULL,
                                idioma_prompts       = c("en","es"),
                                idioma_bocadillos    = c("es","en"),
                                modelo               = "gpt-4.1-mini-2025-04-14",
                                seed                 = 2026,
                                archivo              = NULL,
                                verbose              = TRUE,
                                safe_mode            = TRUE,
                                tema_sensible        = NULL,
                                intent_clinico       = NULL) {

  if (!inherits(escala_h, "semilla_historias")) {
    stop("`escala_h` debe ser un objeto semilla_historias.")
  }
  layout            <- match.arg(layout)
  paleta            <- match.arg(paleta)
  idioma_prompts    <- match.arg(idioma_prompts)
  idioma_bocadillos <- match.arg(idioma_bocadillos)
  if (n_panels < 4L || n_panels > 9L) {
    stop("n_panels debe estar entre 4 y 9.")
  }

  # ---- 0. Modo seguro: deteccion de tema sensible y bandera de intent -------

  if (isTRUE(safe_mode)) {
    if (is.null(tema_sensible)) {
      tema_sensible <- .detectar_tema_sensible(escala_h)
    }
    if (is.null(intent_clinico)) {
      intent_clinico <- length(tema_sensible) > 0L
    }
    if (verbose && length(tema_sensible) > 0L) {
      cat("[prompts_historieta] safe_mode=TRUE | temas sensibles detectados: ",
          paste(tema_sensible, collapse = ", "), "\n", sep = "")
    }
  } else {
    tema_sensible <- character(0)
    if (is.null(intent_clinico)) intent_clinico <- FALSE
  }

  # ---- 1. Defaults visuales (estilo y protagonista) --------------------------

  victima <- escala_h$personajes$victima %||% "the protagonist"
  pop     <- escala_h$poblacion          %||% "adolescent"

  if (is.null(protagonista_visual)) {
    edad_en <- if (isTRUE(safe_mode)) {
      "a stylized cartoon student of high-school age, depicted with non-photorealistic, generic teen proportions"
    } else {
      "a recurring teenage girl around 14 years old"
    }
    edad_es <- if (isTRUE(safe_mode)) {
      "una estudiante caricaturesca de secundaria, dibujada con proporciones adolescentes genericas y no fotorrealistas"
    } else {
      "una adolescente de unos 14 anos"
    }
    extra_safe_en <- if (isTRUE(safe_mode)) {
      paste(" Long-sleeved hoodie at all times; arms remain covered (no",
            "bare skin shown). Modest school clothing. No scars, no skin",
            "marks, no blood, no sharp objects in any panel.")
    } else ""
    extra_safe_es <- if (isTRUE(safe_mode)) {
      paste(" Sudadera de manga larga en todo momento; los brazos siempre",
            "permanecen cubiertos (no se muestra piel descubierta). Ropa",
            "escolar modesta. Sin cicatrices, marcas en piel, sangre ni",
            "objetos cortantes en ningun panel.")
    } else ""

    protagonista_visual <- if (idioma_prompts == "en") {
      if (paleta == "color") {
        sprintf(paste(
          "%s, %s, shoulder-length dark brown wavy hair, soft round face",
          "with expressive almond-shaped eyes, wearing a casual",
          "long-sleeved purple-and-grey striped hoodie and jeans, school",
          "backpack. Same character must appear consistently in every",
          "panel (same hair, same clothes, same proportions). Population",
          "context: %s.%s"
        ), victima, edad_en, pop, extra_safe_en)
      } else {
        sprintf(paste(
          "%s, %s, shoulder-length wavy hair, expressive eyes, plain",
          "long-sleeved hoodie and jeans, school backpack. Same character",
          "must appear consistently in every panel.%s"
        ), victima, edad_en, extra_safe_en)
      }
    } else {
      if (paleta == "color") {
        sprintf(paste(
          "%s, %s, cabello casta\u00F1o ondulado a los hombros, rostro",
          "redondeado con ojos almendrados expresivos, viste una sudadera",
          "de manga larga a rayas violetas y grises con jean, mochila",
          "escolar. Debe aparecer consistente en cada panel (mismo",
          "cabello, misma ropa, mismas proporciones). Poblacion: %s.%s"
        ), victima, edad_es, pop, extra_safe_es)
      } else {
        sprintf(paste(
          "%s, %s, cabello ondulado a los hombros, ojos expresivos,",
          "sudadera de manga larga y jean lisos. Misma persona",
          "consistente en todos los paneles.%s"
        ), victima, edad_es, extra_safe_es)
      }
    }
  }

  if (is.null(estilo)) {
    estilo <- if (idioma_prompts == "en") {
      if (paleta == "color") {
        paste("Comic strip / graphic novel page, warm watercolor style with",
              "bold black ink outlines and cross-hatched shading, soft",
              "ambient light, expressive cartoon faces with realistic",
              "proportions, single page divided into clearly separated panels",
              "with thin black borders. Each panel has a YELLOW caption box",
              "in the upper-left corner with a SHORT TITLE in uppercase",
              "Spanish. Speech is shown in YELLOW rounded speech bubbles",
              "with tail; thoughts and whispers in LIGHT BLUE / LAVENDER",
              "rounded bubbles without tail. NO text outside the bubbles",
              "or caption boxes.")
      } else {
        paste("Black and white comic strip page, clean bold ink outlines on",
              "white background, NO color, NO grayscale shading, single page",
              "divided into clearly separated panels with thin black",
              "borders. Each panel has a caption box in the upper-left",
              "corner with a short title; speech in rounded speech bubbles",
              "with tail; thoughts in cloud-shaped bubbles.")
      }
    } else {
      if (paleta == "color") {
        paste("Pagina de historieta / comic, estilo acuarela calida con",
              "contornos negros marcados y sombreado de tramas, luz suave,",
              "rostros caricaturescos pero con proporciones realistas,",
              "una pagina dividida en paneles claramente separados con",
              "bordes negros delgados. Cada panel lleva una CAJA AMARILLA",
              "en la esquina superior izquierda con un TITULO CORTO en",
              "MAYUSCULAS. Los dialogos se muestran en BOCADILLOS",
              "AMARILLOS redondeados con cola; los pensamientos y susurros",
              "en BOCADILLOS CELESTES o LAVANDA sin cola. NINGUN texto",
              "fuera de los bocadillos o cajas de titulo.")
      } else {
        paste("Pagina de historieta blanco y negro, contornos negros",
              "definidos sobre fondo blanco, SIN color, SIN sombreado en",
              "grises, una pagina dividida en paneles claramente separados.",
              "Cada panel tiene una caja de titulo en la esquina superior",
              "izquierda; dialogos en bocadillos con cola; pensamientos en",
              "bocadillos tipo nube.")
      }
    }
  }

  # ---- 2. Cliente OpenAI para segmentacion narrativa ------------------------

  openai <- .configurar_openai(api_key)
  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  if (verbose) cat("\n[prompts_historieta] Procesando ",
                    nrow(escala_h$historias), " historias en ",
                    n_panels, " paneles cada una...\n", sep = "")

  # ---- 3. Loop por historia: pedir al LLM segmentacion en paneles -----------

  # Bloque de seguridad inyectado al system message del LLM segmentador
  safety_sys_en <- if (isTRUE(safe_mode)) paste(
    "",
    "CONTENT-SAFETY RULES (MANDATORY, applies to every visual description):",
    "- The protagonist and all characters wear LONG SLEEVES at all times.",
    "  Arms are NEVER shown bare. Skin marks are NEVER shown.",
    "- DO NOT describe in any visual: scars, cuts, wounds, blood, bruises,",
    "  skin marks, sharp objects (knife, blade, razor, scissors), the act of",
    "  cutting/self-harm, lifting a sleeve to reveal anything, or any",
    "  graphic/photorealistic harm.",
    "- If the source story mentions self-harm, scars, or marks, render that",
    "  meaning ONLY through facial expression, posture, thought bubbles, or",
    "  a stylized blurred silhouette inside a thought cloud (no anatomical",
    "  detail, no body part close-ups).",
    "- Replace 'shows scars / lifts sleeve / marks on arm' with safer",
    "  equivalents like 'speaks with seriousness, sleeves down, conveying",
    "  a difficult past through expression'.",
    "- No close-ups of arms, wrists, hands holding sharp objects, or any",
    "  body part where harm could be implied.",
    ""
  ) else ""

  safety_sys_es <- if (isTRUE(safe_mode)) paste(
    "",
    "REGLAS DE SEGURIDAD DE CONTENIDO (OBLIGATORIAS para CADA descripcion visual):",
    "- La protagonista y todos los personajes llevan MANGA LARGA en todo",
    "  momento. Los brazos NUNCA se muestran descubiertos. NUNCA se muestran",
    "  marcas en la piel.",
    "- NO describas en ninguna escena: cicatrices, cortes, heridas, sangre,",
    "  moretones, marcas en la piel, objetos cortantes (cuchillo, cuchilla,",
    "  navaja, tijeras), el acto de cortarse o autolesionarse, levantar la",
    "  manga para mostrar algo, ni representaciones graficas/fotorrealistas.",
    "- Si el relato fuente menciona autolesion, cicatrices o marcas, expresa",
    "  ese significado SOLO mediante expresion facial, postura, bocadillos",
    "  de pensamiento o una silueta borrosa estilizada dentro de una nube",
    "  de pensamiento (sin detalle anatomico, sin primeros planos del cuerpo).",
    "- Sustituye 'muestra cicatrices / levanta la manga / marcas en el brazo'",
    "  por equivalentes seguros como 'habla con seriedad, manga larga abajo,",
    "  transmitiendo un pasado dificil mediante la expresion'.",
    "- Sin primeros planos de brazos, mu\u00F1ecas, manos sujetando objetos",
    "  cortantes, ni partes del cuerpo donde se pueda inferir da\u00F1o.",
    ""
  ) else ""

  out <- vector("list", nrow(escala_h$historias))

  for (i in seq_len(nrow(escala_h$historias))) {
    factor_i <- escala_h$historias$factor[i]
    texto_i  <- escala_h$historias$texto[i]
    if (verbose) cat("  [", i, "/", nrow(escala_h$historias), "] ",
                      factor_i, "...\n", sep = "")

    # Sub-prompt: pedir paneles en JSON estructurado
    sys_msg <- if (idioma_prompts == "en") paste(
      "You are a graphic-novel scriptwriter. Given a short story about a",
      "teenage protagonist, segment it into exactly", n_panels, "panels of a",
      "comic strip. Each panel must have:",
      "(1) a SHORT title in uppercase Spanish (max 4 words);",
      "(2) a VISUAL description of what is happening (1-2 sentences;",
      "character pose, facial expression, environment, key objects, AND",
      "WHO IS PRESENT IN THE PANEL with the protagonist);",
      "(3) a list of BUBBLES, each labeled as 'speech', 'thought' or",
      "'whisper' with text in", idioma_bocadillos, ".",
      "",
      "CRITICAL CLASSIFICATION RULES FOR BUBBLES:",
      "- 'thought' = INTERNAL self-talk, cognitions, things the protagonist",
      "  thinks but does NOT say out loud. Includes phrases the protagonist",
      "  'tells herself', 'thinks', 'feels in her head', or 'self-talk'.",
      "  Even when the source story uses 'pensa', 'se dijo a si misma', or",
      "  shows quoted thoughts inside the protagonist's mind, label as",
      "  'thought'. Thought bubbles must be drawn as cloud/rounded with",
      "  NO TAIL, never pointing at another character.",
      "- 'speech' = ONLY when the source story EXPLICITLY says the",
      "  protagonist SAID something OUT LOUD to another person (verbs:",
      "  'dijo', 'le contesto', 'le respondio', 'le pregunto', 'hablo en",
      "  voz alta', 'comento', 'le dijo a [otra persona]'). Speech bubbles",
      "  have a tail pointing toward the protagonist's mouth.",
      "- 'whisper' = the protagonist whispered, murmured or said something",
      "  in voz baja that another person could partially hear. Drawn as a",
      "  small bubble with dotted/wavy outline.",
      "",
      "DEFAULT WHEN AMBIGUOUS: label as 'thought'. NEVER turn an internal",
      "self-talk into a speech bubble pointing at another character.",
      "",
      "The visual description MUST mention if the protagonist is ALONE in",
      "the panel (only her, internal scene) or with OTHERS (and who).",
      "If she is alone, every bubble must be 'thought' (no speech possible).",
      "",
      "Panels must show progressive emotional escalation from start to",
      "end. Do not invent new plot elements; stay faithful to the source",
      "story.",
      safety_sys_en,
      "Output ONLY valid JSON:",
      '{"panels": [{"title": "...", "visual": "...", "bubbles": [{"type": "speech|thought|whisper", "text": "..."}]}]}'
    ) else paste(
      "Eres guionista de novela grafica. Dado un relato breve sobre una",
      "adolescente protagonista, segmentalo en exactamente", n_panels,
      "paneles de historieta. Cada panel debe tener:",
      "(1) un TITULO corto en MAYUSCULAS en", idioma_bocadillos,
      "(maximo 4 palabras);",
      "(2) una DESCRIPCION visual (1-2 oraciones; pose, expresion,",
      "entorno, objetos clave, Y QUIENES ESTAN PRESENTES en el panel",
      "junto a la protagonista);",
      "(3) una lista de BOCADILLOS rotulados como 'speech', 'thought' o",
      "'whisper' con el texto en", idioma_bocadillos, ".",
      "",
      "REGLAS CRITICAS DE CLASIFICACION DE BOCADILLOS:",
      "- 'thought' = autoconversacion INTERNA, cogniciones, cosas que la",
      "  protagonista PIENSA pero NO dice en voz alta. Incluye frases que",
      "  ella 'se dice a si misma', 'piensa', 'siente en su cabeza' o",
      "  'autoconversacion'. AUNQUE el relato use 'penso', 'se dijo a si",
      "  misma' o muestre la frase entre comillas como pensamiento, debe",
      "  etiquetarse como 'thought'. Los bocadillos de pensamiento se",
      "  dibujan como nube/redondeados SIN COLA, nunca apuntan a otro",
      "  personaje.",
      "- 'speech' = SOLO cuando el relato fuente dice EXPLICITAMENTE que",
      "  la protagonista DIJO algo EN VOZ ALTA a otra persona (verbos",
      "  'dijo', 'le contesto', 'le respondio', 'le pregunto', 'hablo en",
      "  voz alta', 'comento', 'le dijo a [otro]'). Los bocadillos de",
      "  habla tienen cola que apunta a la boca de la protagonista.",
      "- 'whisper' = la protagonista susurro, murmuro o hablo en voz baja",
      "  que otra persona pudo oir parcialmente. Bocadillo pequeno con",
      "  borde punteado.",
      "",
      "POR DEFECTO ANTE AMBIGUEDAD: etiquetar como 'thought'. NUNCA",
      "convertir una autoconversacion en un bocadillo de habla apuntando a",
      "otro personaje.",
      "",
      "La descripcion visual DEBE indicar si la protagonista esta SOLA en",
      "el panel (solo ella, escena interna) o ACOMPA\u00D1ADA (y por quien).",
      "Si esta sola, todos los bocadillos deben ser 'thought' (no es",
      "posible 'speech').",
      "",
      "EJEMPLOS DE CLASIFICACION CORRECTA:",
      "- Relato: 'penso: Mi cuerpo deberia pagar' -> bubble 'thought'",
      "- Relato: 'se dijo a si misma: nadie me entiende' -> 'thought'",
      "- Relato: 'le dijo a Mariana en voz baja: no es tu culpa' -> 'speech'",
      "  (porque hay verbo 'dijo' + destinatario explicito).",
      "- Relato: 'imagino que le decia a su madre' -> 'thought' (imaginar",
      "  no es decir; sigue siendo internal).",
      "",
      "La progresion emocional escala del primer al ultimo panel. No",
      "inventes elementos.",
      safety_sys_es,
      "Devuelve SOLO JSON valido:",
      '{"panels": [{"title": "...", "visual": "...", "bubbles": [{"type": "speech|thought|whisper", "text": "..."}]}]}'
    )

    user_msg <- paste0(
      if (idioma_prompts == "en") "Source story:\n" else "Relato fuente:\n",
      texto_i
    )

    raw <- tryCatch({
      .llamar_openai(
        openai = openai,
        messages = list(
          list(role = "system", content = sys_msg),
          list(role = "user",   content = user_msg)
        ),
        modelo = modelo, max_tokens = 1200L, temperature = 0.5
      )
    }, error = function(e) {
      warning("Error al segmentar historia ", i, ": ", e$message)
      ""
    })

    paneles <- .parse_paneles_json(raw, n_panels)

    # Sanitizar descripciones visuales si safe_mode esta activo
    if (isTRUE(safe_mode) && length(paneles) > 0L) {
      paneles <- lapply(paneles, function(p) {
        if (!is.null(p$visual)) p$visual <- .sanitizar_visual(p$visual)
        p
      })
    }

    # Construir prompt visual final
    prompt_final <- .construir_prompt_historieta(
      paneles            = paneles,
      n_panels           = length(paneles),
      layout             = layout,
      estilo             = estilo,
      protagonista_visual = protagonista_visual,
      titulo_historia    = factor_i,
      idioma_prompts     = idioma_prompts,
      idioma_bocadillos  = idioma_bocadillos,
      paleta             = paleta,
      intent_clinico     = intent_clinico,
      tema_sensible      = tema_sensible,
      safe_mode          = safe_mode
    )

    out[[i]] <- data.frame(
      factor          = factor_i,
      historia_texto  = texto_i,
      n_panels        = length(paneles),
      paneles_json    = jsonlite::toJSON(paneles, auto_unbox = TRUE),
      prompt          = prompt_final,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, out)
  class(res) <- c("semilla_prompts_historieta", "data.frame")

  # ---- 4. Exportar a Excel (opcional) ---------------------------------------

  if (!is.null(archivo)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      warning("openxlsx no disponible. Instala con install.packages('openxlsx').")
    } else {
      ruta_xlsx <- paste0(archivo, ".xlsx")
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "prompts")
      openxlsx::writeData(wb, "prompts",
                          res[, c("factor","n_panels","prompt")])
      openxlsx::addWorksheet(wb, "info")
      openxlsx::writeData(wb, "info", data.frame(
        clave = c("paleta","layout","n_panels","idioma_prompts","idioma_bocadillos","modelo","seed","fecha"),
        valor = c(paleta, layout, n_panels, idioma_prompts, idioma_bocadillos, modelo,
                  ifelse(is.null(seed),"",seed), as.character(Sys.time()))
      ))
      openxlsx::addWorksheet(wb, "instrucciones")
      openxlsx::writeData(wb, "instrucciones", data.frame(
        paso = 1:5,
        instruccion = c(
          "Copia el contenido de la columna 'prompt' (de la hoja prompts) en un modelo texto-a-imagen (Gemini, ChatGPT con DALL-E, Midjourney, Stable Diffusion).",
          "Genera una imagen por historia. Si la imagen no respeta los bocadillos o titulos, vuelve a generarla con el mismo prompt.",
          "Guarda cada imagen como historia_<factor>.png usando el nombre del factor en el archivo.",
          "Imprime las imagenes y administra junto con los items perceptivos.",
          "Si el modelo respeta mejor el ingles, regenera con idioma_prompts='en' (los bocadillos seguiran en idioma_bocadillos)."
        )
      ))
      openxlsx::saveWorkbook(wb, ruta_xlsx, overwrite = TRUE)
      if (verbose) cat("  [OK] Excel guardado: ", ruta_xlsx, "\n", sep = "")
    }
  }

  if (verbose) cat("[OK] Prompts de historieta generados: ", nrow(res), "\n", sep = "")
  res
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.detectar_tema_sensible <- function(escala_h) {
  textos <- paste(c(
    as.character(escala_h$historias$factor),
    as.character(escala_h$historias$texto)
  ), collapse = " ")
  textos <- tolower(textos)

  patrones <- list(
    autolesion = c("autolesion", "autolesi", "cutting", "cortarse",
                    "self-harm", "self harm", "nssi", "non-suicidal",
                    "self-injury", "cicatric", "cortes en"),
    suicidio = c("suicid", "suicide", "suicidal"),
    violencia = c("violencia", "violence", "abuso fisico", "physical abuse",
                   "agresion", "bullying"),
    abuso_sexual = c("abuso sexual", "violacion", "sexual abuse",
                       "sexual assault"),
    trastorno_alimentario = c("anorexia", "bulimia", "atracon", "purga",
                                "binge", "purg", "eating disorder"),
    consumo = c("alcohol", "drogadic", "marihuana", "cocain", "drug abuse",
                 "consumo de drog")
  )

  temas <- character(0)
  for (nm in names(patrones)) {
    if (any(vapply(patrones[[nm]],
                    function(p) grepl(p, textos, fixed = TRUE),
                    logical(1)))) {
      temas <- c(temas, nm)
    }
  }
  unique(temas)
}


#' @keywords internal
.sanitizar_visual <- function(texto) {
  if (!is.character(texto) || length(texto) == 0L || !nzchar(texto[[1]])) {
    return(texto)
  }
  x <- texto

  # Reemplazos en espanol (frases compuestas primero, terminos sueltos despues)
  reemplazos <- list(
    # ---- ESPANOL: frases compuestas (mas especificas primero) ----
    list("(?i)se levant[ao] la manga[, ]+mostrando (cicatrices|cortes|marcas)( en (su|el) brazo)?",
         "habla con seriedad, con la manga larga bajada cubriendo sus brazos"),
    list("(?i)mostrando (cicatrices|cortes|marcas) en (su|el) brazo",
         "transmitiendo un pasado dificil con expresion seria, manga larga bajada"),
    list("(?i)muestra (cicatrices|cortes|marcas) en (su|el) brazo",
         "habla con seriedad, manga larga sin mostrar piel del brazo"),
    list("(?i)se arremanga( la manga)?",
         "habla con la manga larga bajada"),
    list("(?i)brazo[s]? (descubierto|descubiertos|expuesto|expuestos|al aire)",
         "brazo cubierto por la manga larga"),
    list("(?i)manga arremangada",
         "manga larga bajada"),

    # Cualquier mencion de un brazo asociado a marcas/cicatrices/cortes/heridas
    # (cubre 'una imagen del brazo con marcas', 'un plano de su brazo con cortes', etc.)
    list("(?i)(un[ao]s?|el|la|los|las)?\\s*(imagen|plano|primer plano|vista|visual|escena|representacion)[^.,]{0,80}(propio )?brazo[s]?[^.,]{0,40}(marcas|cicatrices|cortes|heridas|sangre)[^.,]*",
         "una nube de pensamiento estilizada con una silueta borrosa, sin detalle anatomico"),
    list("(?i)(propio )?brazo[s]? con (marcas|cicatrices|cortes|heridas)",
         "una silueta borrosa estilizada en una nube de pensamiento, sin detalle anatomico"),
    list("(?i)(marcas|cicatrices|cortes|heridas) (imaginari[ao]s?|en (su|el) brazo|en la piel)",
         "expresion seria que sugiere un pasado dificil, sin detalle anatomico"),
    list("(?i)plano mental.*(brazo|cortes|marcas|cicatrices)[^.]*",
         "plano mental: nube de pensamiento estilizada con una silueta borrosa, sin detalle anatomico"),

    # Terminos sueltos (catch-all). Va despues de las frases compuestas.
    list("(?i)\\b(cicatriz|cicatrices)\\b",
         "[expresion seria]"),
    list("(?i)\\bmarcas?\\b(?!\\s+(de\\s+ropa|en\\s+el\\s+suelo))",
         "[expresion seria]"),
    list("(?i)\\b(cortes?|heridas?|sangre|moretones?)\\b",
         "[expresion seria]"),
    list("(?i)\\b(cuchillo|cuchilla|navaja|tijeras)\\b",
         "[objeto omitido]"),

    # ---- INGLES: frases compuestas primero ----
    list("(?i)lifts? (the|her|his) sleeve to (show|reveal) (scars|cuts|marks)",
         "speaks with seriousness, sleeves down covering arms"),
    list("(?i)(showing|revealing) (scars|cuts|marks) on (the|her|his) arm",
         "conveying a difficult past through serious expression, long sleeves down"),
    list("(?i)(image|view|close[- ]?up|mental image|panel)[^.,]{0,80}(own )?arm[^.,]{0,40}(marks|scars|cuts|wounds|blood)[^.,]*",
         "stylized blurred silhouette inside a thought cloud, no anatomical detail"),
    list("(?i)(own )?arm with (marks|scars|cuts|wounds)",
         "stylized blurred silhouette inside a thought cloud, no anatomical detail"),
    list("(?i)imaginary (marks|scars|cuts) on (her|his|the) arm",
         "stylized blurred silhouette inside a thought cloud, no anatomical detail"),
    list("(?i)bare arm[s]?",
         "long-sleeved arm"),
    list("(?i)rolled[- ]?up sleeve[s]?",
         "long sleeve down"),

    # Terminos sueltos en ingles
    list("(?i)\\b(scars?|cuts?|wounds?|blood|bruises?)\\b",
         "[serious expression]"),
    list("(?i)\\b(knife|blade|razor|scissors)\\b",
         "[object omitted]")
  )

  for (r in reemplazos) {
    x <- gsub(r[[1]], r[[2]], x, perl = TRUE)
  }
  x
}


#' @keywords internal
.intent_block <- function(temas, idioma_prompts) {
  if (idioma_prompts == "en") {
    body <- c(
      "## Intent (read first)",
      "Educational graphic-novel page for a peer-reviewed psychometric study",
      "approved by an institutional ethics committee (IRB/CIEI). The page",
      "illustrates the INTERNAL emotional experience of a young student in a",
      "school setting. The narrative refers to sensitive topics through",
      "facial expression, body language and bubble dialogue ONLY.",
      "",
      "DO NOT depict any of the following in any panel:",
      "  - visible wounds, scars, cuts, blood, bruises or skin marks",
      "  - any act of self-harm, violence or self-injury",
      "  - sharp objects (knife, blade, razor, scissors)",
      "  - exposed bare arms with marks; sleeves remain DOWN at all times",
      "  - graphic, photorealistic or close-up depictions of harm",
      "",
      "Style is stylized cartoon / watercolor (non-photorealistic). All",
      "characters are drawn with generic, stylized teen proportions and",
      "modest school clothing. Long sleeves at all times.",
      ""
    )
  } else {
    body <- c(
      "## Intencion (leer primero)",
      "Pagina de novela grafica educativa para un estudio psicometrico con",
      "revision por pares y aprobacion de comite de etica institucional",
      "(CIEI). La pagina ilustra la experiencia emocional INTERNA de una",
      "estudiante en un entorno escolar. El relato alude a temas sensibles",
      "solo a traves de expresion facial, lenguaje corporal y bocadillos.",
      "",
      "NO mostrar en ningun panel:",
      "  - heridas, cicatrices, cortes, sangre, moretones o marcas en la piel",
      "  - actos de autolesion, violencia o agresion",
      "  - objetos cortantes (cuchillo, cuchilla, navaja, tijeras)",
      "  - brazos descubiertos con marcas; las mangas siempre estan bajadas",
      "  - representaciones graficas, fotorrealistas o en primer plano de da\u00F1o",
      "",
      "Estilo caricaturesco/acuarela (no fotorrealista). Todos los personajes",
      "se dibujan con proporciones adolescentes estilizadas y ropa escolar",
      "modesta. Manga larga en todo momento.",
      ""
    )
  }
  body
}


#' @keywords internal
.parse_paneles_json <- function(raw, n_target) {
  if (!nzchar(raw)) return(list())
  # Limpiar markdown / fences
  raw <- gsub("```(json)?\\s*", "", raw, perl = TRUE)
  raw <- gsub("```\\s*$", "", raw, perl = TRUE)
  raw <- trimws(raw)

  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$panels)) {
    warning("No se pudo parsear JSON de paneles. Devolviendo lista vacia.")
    return(list())
  }
  if (length(parsed$panels) != n_target) {
    warning("LLM devolvio ", length(parsed$panels), " paneles (esperados ",
            n_target, "). Usando lo disponible.")
  }
  parsed$panels
}


#' @keywords internal
.construir_prompt_historieta <- function(paneles, n_panels, layout, estilo,
                                          protagonista_visual, titulo_historia,
                                          idioma_prompts, idioma_bocadillos,
                                          paleta,
                                          intent_clinico = FALSE,
                                          tema_sensible  = character(0),
                                          safe_mode      = FALSE) {

  if (length(paneles) == 0) {
    return(paste0("[error] No hay paneles parseados para esta historia."))
  }

  layout_txt <- switch(layout,
    "2x3"  = "2 rows x 3 columns",
    "3x2"  = "3 rows x 2 columns",
    "3x3"  = "3 rows x 3 columns",
    "1xN"  = "1 single horizontal row",
    "Nx1"  = "1 single vertical column",
    "2 rows x 3 columns"
  )

  intent_lines <- if (isTRUE(intent_clinico)) {
    .intent_block(tema_sensible, idioma_prompts)
  } else character(0)

  # Encabezado
  if (idioma_prompts == "en") {
    header <- c(
      paste0("# COMIC STRIP - \"", titulo_historia, "\""),
      "",
      intent_lines,
      "## Visual style",
      estilo,
      "",
      "## Recurring protagonist",
      protagonista_visual,
      "",
      paste0("## Layout: ", layout_txt, " (", n_panels, " panels total)"),
      "",
      paste0("Bubble text language: ", idioma_bocadillos, ". Caption titles in uppercase."),
      "",
      "## Panels",
      ""
    )
  } else {
    header <- c(
      paste0("# HISTORIETA - \"", titulo_historia, "\""),
      "",
      intent_lines,
      "## Estilo visual",
      estilo,
      "",
      "## Protagonista recurrente",
      protagonista_visual,
      "",
      paste0("## Disposicion: ", layout_txt, " (", n_panels, " paneles en total)"),
      "",
      paste0("Idioma de los bocadillos: ", idioma_bocadillos, ". Titulos de panel en mayusculas."),
      "",
      "## Paneles",
      ""
    )
  }

  # Cuerpo: un bloque por panel
  body <- character(0)
  for (k in seq_along(paneles)) {
    p <- paneles[[k]]
    titulo  <- p$title  %||% ""
    visual  <- p$visual %||% ""
    bubbles <- p$bubbles
    bub_txt <- if (length(bubbles) > 0) {
      paste(vapply(bubbles, function(b) {
        sprintf("    - [%s] \"%s\"",
                b$type %||% "speech",
                b$text %||% "")
      }, character(1)), collapse = "\n")
    } else {
      "    (no bubbles)"
    }

    if (idioma_prompts == "en") {
      body <- c(body,
        paste0("PANEL ", k, " - \"", titulo, "\""),
        paste0("  Visual: ", visual),
        "  Bubbles:",
        bub_txt,
        "")
    } else {
      body <- c(body,
        paste0("PANEL ", k, " - \"", titulo, "\""),
        paste0("  Visual: ", visual),
        "  Bocadillos:",
        bub_txt,
        "")
    }
  }

  # Footer con recordatorios
  safety_footer_en <- if (isTRUE(safe_mode)) c(
    "- Long sleeves at all times. No bare arms, no skin marks, no scars,",
    "  no cuts, no blood, no wounds, no sharp objects in any panel.",
    "- Any reference to self-harm or sensitive content stays in expression,",
    "  posture and bubble text only; never depicted graphically."
  ) else character(0)

  safety_footer_es <- if (isTRUE(safe_mode)) c(
    "- Manga larga en todo momento. Sin brazos descubiertos, sin marcas en",
    "  la piel, sin cicatrices, sin cortes, sin sangre, sin heridas, sin",
    "  objetos cortantes en ningun panel.",
    "- Toda referencia a autolesion o contenido sensible se transmite solo",
    "  por expresion, postura y texto en bocadillos; nunca graficamente."
  ) else character(0)

  footer <- if (idioma_prompts == "en") c(
    "## Consistency rules",
    "- The protagonist must look identical in every panel (same face, same hair, same outfit).",
    "- Maintain the same art style across panels (no panel should look like a different artist).",
    paste0("- Caption titles go in YELLOW BOXES top-left of each panel, in uppercase ", idioma_bocadillos, "."),
    "- Speech bubbles: yellow with tail. Thought / whisper bubbles: light blue or lavender, no tail.",
    "- All text inside the image must be in the bubbles or caption boxes only.",
    safety_footer_en
  ) else c(
    "## Reglas de consistencia",
    "- La protagonista debe verse identica en todos los paneles (mismo rostro, cabello, ropa).",
    "- Mantener el mismo estilo artistico entre paneles.",
    "- Titulos en CAJAS AMARILLAS arriba a la izquierda, en mayusculas.",
    "- Bocadillos de dialogo: amarillos con cola. Pensamientos / susurros: celestes o lavanda, sin cola.",
    "- Todo el texto en la imagen va dentro de bocadillos o cajas de titulo.",
    safety_footer_es
  )

  paste(c(header, body, footer), collapse = "\n")
}


#' @export
print.semilla_prompts_historieta <- function(x, ...) {
  cat("\n=== prompts_historieta() : ", nrow(x), " historias ===\n", sep = "")
  for (i in seq_len(nrow(x))) {
    cat("  [", i, "] ", x$factor[i],
        "  (", x$n_panels[i], " paneles, ",
        nchar(x$prompt[i]), " chars)\n", sep = "")
  }
  cat("\nAcceso: x$prompt[1]  (texto listo para Gemini/ChatGPT/Midjourney)\n")
  invisible(x)
}
