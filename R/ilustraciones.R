#' @title Generar prompts para ilustraciones de items (SIN llamar al modelo de imagen)
#'
#' @description
#' Para cada item del objeto \code{semilla} construye un prompt listo para pegar
#' en un modelo texto-a-imagen (Gemini, ChatGPT, Midjourney, etc.). El prompt
#' combina cuatro bloques fijos:
#'
#' \enumerate{
#'   \item Estilo visual (linea negra blanco-y-negro, sin sombreado, fondo blanco).
#'   \item Ficha del personaje recurrente (mismo niño/a en toda la serie).
#'   \item Descripcion de la escena especifica del item (generada por LLM).
#'   \item Recordatorio de consistencia visual entre imagenes.
#' }
#'
#' Solo se llama al LLM para describir la escena de cada item (~30 palabras),
#' lo que es muchisimo mas barato que generar imagenes via API. Las descripciones
#' se cachean usando el sistema de cache de SeMiLLa.
#'
#' Las imagenes finales las genera el USUARIO copiando los prompts en su modelo
#' visual preferido y guardandolas como \code{item_01.png}, \code{item_02.png},
#' etc. Esa carpeta luego se pasa a \code{ensamblar_test(ilustraciones = ...)}.
#'
#' @param escala Objeto \code{semilla}, \code{semilla_items} o data.frame con
#'   columnas \code{item} y \code{dimension}.
#' @param api_key Clave de OpenAI.
#' @param personaje Cadena con la ficha del personaje. Si es NULL, se usa un
#'   default sensato segun \code{idioma_prompts}.
#' @param estilo Cadena con el estilo visual. Si es NULL, se usa un preset
#'   acorde a \code{paleta}: line-art blanco y negro o ilustracion infantil
#'   en color.
#' @param paleta \code{"bn"} (default, blanco y negro tipo coloring page) o
#'   \code{"color"} (ilustracion infantil en color suave). Cuando \code{"bn"},
#'   los defaults de \code{personaje} y \code{estilo} omiten cualquier color
#'   especifico (porque el modelo respetaria un "blue shorts" y romperia el
#'   line-art) y se anade un refuerzo "strict black-and-white only" al prompt.
#' @param idioma_prompts \code{"en"} (default, mejor calidad en modelos visuales)
#'   o \code{"es"}.
#' @param modelo Modelo OpenAI a usar para describir las escenas
#'   (default: \code{"gpt-4.1-mini-2025-04-14"}).
#' @param seed Semilla para reproducibilidad.
#' @param archivo Ruta de salida SIN extension. Si se proporciona, escribe
#'   un Excel con tres hojas: \code{prompts}, \code{info}, \code{instrucciones}.
#' @param verbose Mostrar progreso.
#'
#' @return Data.frame con clase \code{semilla_prompts_ilustracion} y columnas:
#'   \code{n_item}, \code{dimension}, \code{item}, \code{escena}, \code{prompt}.
#'
#' @examples
#' \dontrun{
#' p <- prompts_ilustracion(
#'   escala         = mi_escala,
#'   api_key        = api_key,
#'   idioma_prompts = "en",
#'   archivo        = "test_aplicacion/prompts_ilustracion"
#' )
#' head(p$prompt, 1)
#' }
#'
#' @export
#' @noRd
prompts_ilustracion <- function(escala,
                                api_key,
                                personaje      = NULL,
                                estilo         = NULL,
                                paleta         = c("bn", "color"),
                                idioma_prompts = c("en", "es"),
                                idioma_docs    = c("es", "en"),
                                modelo         = "gpt-4.1-mini-2025-04-14",
                                seed           = 2026,
                                archivo        = NULL,
                                verbose        = TRUE) {

  idioma_prompts <- match.arg(idioma_prompts)
  idioma_docs    <- match.arg(idioma_docs)
  paleta         <- match.arg(paleta)

  items_df <- .extraer_df_items(escala)
  if (is.null(items_df) || nrow(items_df) == 0) {
    stop("No se encontraron items en 'escala'.")
  }

  # ---------- Defaults segun paleta ----------
  # Importante: en blanco y negro NO mencionar colores en la ficha del personaje
  # (e.g. "blue shorts") porque el modelo de imagen los respeta y el dibujo
  # deja de ser line-art puro.
  if (is.null(personaje)) {
    personaje <- if (idioma_prompts == "en") {
      if (paleta == "bn") {
        paste("A single recurring child around 8 years old, short bobbed hair,",
              "wearing a plain t-shirt and shorts (no patterns), round cheeks,",
              "big expressive eyes, friendly cartoon proportions")
      } else {
        paste("A single recurring child around 8 years old, short brown hair,",
              "white t-shirt and blue shorts, round cheeks, big expressive eyes,",
              "friendly cartoon proportions, soft children's book illustration style")
      }
    } else {
      if (paleta == "bn") {
        paste("Un ni\u00F1o o ni\u00F1a de unos 8 a\u00F1os (siempre el mismo personaje),",
              "cabello corto, polera y short lisos sin estampados, mejillas redondas,",
              "ojos grandes y expresivos, proporciones caricaturescas amigables")
      } else {
        paste("Un ni\u00F1o o ni\u00F1a de unos 8 a\u00F1os (siempre el mismo personaje),",
              "cabello corto casta\u00F1o, polera blanca y short azul, mejillas redondas,",
              "ojos grandes y expresivos, proporciones caricaturescas amigables,",
              "estilo de libro ilustrado infantil")
      }
    }
  }

  if (is.null(estilo)) {
    estilo <- if (idioma_prompts == "en") {
      if (paleta == "bn") {
        paste("Black and white line art coloring page, clean bold black outlines",
              "on plain white background, NO color anywhere, NO grayscale shading,",
              "NO fills, vertical portrait orientation (3:4),",
              "child-friendly cartoon style, single clear scene")
      } else {
        paste("Soft children's book illustration, gentle pastel colors,",
              "warm friendly mood, plain background, vertical portrait orientation (3:4),",
              "child-friendly cartoon style, single clear scene")
      }
    } else {
      if (paleta == "bn") {
        paste("Ilustraci\u00F3n blanco y negro para colorear, contornos negros definidos",
              "sobre fondo blanco liso, SIN color en ning\u00FAn elemento,",
              "SIN sombreado en grises, SIN rellenos, orientaci\u00F3n vertical (3:4),",
              "estilo caricaturesco infantil, una sola escena clara")
      } else {
        paste("Ilustraci\u00F3n estilo libro infantil, colores pastel suaves,",
              "ambiente c\u00E1lido y amigable, fondo liso, orientaci\u00F3n vertical (3:4),",
              "estilo caricaturesco infantil, una sola escena clara")
      }
    }
  }

  reforzar_bn <- if (paleta == "bn") {
    if (idioma_prompts == "en") {
      paste("Strict black-and-white only: lines must be pure black on pure white background,",
            "no gray, no color anywhere in the image, including hair, skin and clothing.")
    } else {
      paste("Estricto blanco y negro: l\u00EDneas negras puras sobre fondo blanco puro,",
            "sin grises ni color en ning\u00FAn elemento de la imagen, incluyendo cabello, piel y ropa.")
    }
  } else NULL

  consistency <- if (idioma_prompts == "en") {
    paste("IMPORTANT: The character must remain IDENTICAL across every image of the series",
          "(same hairstyle, same clothing, same face proportions). The setting and other",
          "characters can change per scene, but the protagonist never changes.")
  } else {
    paste("IMPORTANTE: el personaje debe permanecer ID\u00C9NTICO en todas las im\u00E1genes de la serie",
          "(mismo cabello, misma ropa, mismas proporciones faciales). El contexto y los dem\u00E1s",
          "personajes pueden cambiar seg\u00FAn la escena, pero el protagonista nunca cambia.")
  }

  # ---------- Cliente OpenAI ----------
  if (verbose) cat("\n[prompts_ilustracion] Configurando cliente OpenAI...\n")
  openai <- .configurar_openai(api_key)

  if (!is.null(seed)) options(SeMiLLa.seed = as.integer(seed))

  sys_msg <- if (idioma_prompts == "en") paste(
    "You are a visual storyteller for children's psychometric illustrations.",
    "Given a self-report item (rewritten from the child's perspective if needed),",
    "describe in ONE sentence (max 30 words) the visual scene that would illustrate it.",
    "Include explicitly: the concrete action, the emotion shown on the child's face,",
    "the setting (school classroom, bedroom, living room, playground, park, dining table, etc.),",
    "and any other characters present (friends, teacher, parents, siblings).",
    "Use simple present tense. Do NOT include quotation marks, prefixes like 'Scene:'",
    "or any extra commentary. Reply with just the sentence."
  ) else paste(
    "Eres ilustrador visual de tests psicom\u00E9tricos para ni\u00F1os.",
    "Dado un \u00EDtem (reescr\u00EDbelo desde la perspectiva del ni\u00F1o si es necesario),",
    "describe en UNA oraci\u00F3n (m\u00E1ximo 30 palabras) la escena visual que lo ilustrar\u00EDa.",
    "Incluye expl\u00EDcitamente: la acci\u00F3n concreta, la emoci\u00F3n en la cara del ni\u00F1o,",
    "el contexto (aula, dormitorio, sala, patio, parque, mesa del comedor, etc.)",
    "y los otros personajes presentes (amigos, profesor, padres, hermanos).",
    "Usa presente simple. NO incluyas comillas, prefijos como \u00ABEscena:\u00BB",
    "ni comentarios extra. Responde solo la oraci\u00F3n."
  )

  n <- nrow(items_df)
  escenas <- character(n)

  if (verbose) {
    cat("[prompts_ilustracion] Generando descripciones para ", n,
        " items (idioma=", idioma_prompts, ")...\n", sep = "")
  }

  for (i in seq_len(n)) {
    item_text <- as.character(items_df$item[i])
    dim_text  <- if ("dimension" %in% names(items_df))
                   as.character(items_df$dimension[i]) else ""

    user_msg <- if (idioma_prompts == "en") paste0(
      "Item: \"", item_text, "\"\n",
      if (nzchar(dim_text)) paste0("Dimension: ", dim_text, "\n") else "",
      "Scene (one sentence, max 30 words):"
    ) else paste0(
      "\u00CDtem: \u00AB", item_text, "\u00BB\n",
      if (nzchar(dim_text)) paste0("Dimensi\u00F3n: ", dim_text, "\n") else "",
      "Escena (una oraci\u00F3n, m\u00E1x 30 palabras):"
    )

    contenido <- .llamar_openai(
      openai      = openai,
      messages    = list(
        list(role = "system", content = sys_msg),
        list(role = "user",   content = user_msg)
      ),
      modelo      = modelo,
      max_tokens  = 120L,
      temperature = 0.5
    )
    escenas[i] <- trimws(gsub("\\s+", " ", contenido))
    if (verbose) {
      cat("  ", sprintf("%2d/%d", i, n), "  ",
          substr(escenas[i], 1, 78),
          if (nchar(escenas[i]) > 78) "...", "\n", sep = "")
    }
  }

  # ---------- Composicion del prompt final ----------
  L_style  <- if (idioma_prompts == "en") "Style"     else "Estilo"
  L_char   <- if (idioma_prompts == "en") "Character" else "Personaje"
  L_scene  <- if (idioma_prompts == "en") "Scene"     else "Escena"

  prompt_final <- paste0(
    L_style, ": ", estilo, ".\n",
    L_char,  ": ", personaje, ".\n",
    L_scene, ": ", escenas, "\n",
    if (!is.null(reforzar_bn)) paste0(reforzar_bn, "\n") else "",
    consistency
  )

  resultado <- data.frame(
    n_item    = seq_len(n),
    dimension = if ("dimension" %in% names(items_df))
                  as.character(items_df$dimension) else NA_character_,
    item      = as.character(items_df$item),
    escena    = escenas,
    prompt    = prompt_final,
    stringsAsFactors = FALSE
  )

  # ---------- Exportacion ----------
  if (!is.null(archivo)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      warning("Instala openxlsx para exportar a Excel.")
    } else {
      ruta_xlsx <- paste0(archivo, ".xlsx")

      # Etiquetas de la hoja `info`
      paleta_label <- if (paleta == "bn") {
        if (idioma_docs == "es") "blanco y negro (line art)" else "black & white (line art)"
      } else {
        if (idioma_docs == "es") "color (libro infantil)" else "color (children's book)"
      }
      campos_es <- c("Idioma del prompt visual", "Paleta", "N\u00FAmero de \u00EDtems",
                      "Modelo LLM", "Fecha de generaci\u00F3n", "Personaje", "Estilo visual")
      campos_en <- c("Visual prompt language", "Palette", "Number of items",
                      "LLM model", "Generation date", "Character", "Visual style")
      info_df <- data.frame(
        Campo = if (idioma_docs == "es") campos_es else campos_en,
        Valor = c(idioma_prompts, paleta_label, n, modelo, format(Sys.Date()),
                   personaje, estilo),
        stringsAsFactors = FALSE
      )
      if (idioma_docs == "en") names(info_df) <- c("Field", "Value")

      # Hoja `instrucciones`
      pasos_es <- c(
        "Abre la hoja 'prompts' de este archivo. La \u00DANICA columna que se pega en el modelo de imagen es 'prompt'.",
        "Abre Gemini, ChatGPT, Midjourney o tu modelo texto-a-imagen favorito.",
        "Para la fila 1, copia la celda completa de 'prompt' y genera la imagen (vertical 3:4).",
        "Gu\u00E1rdala como 'item_01.png' (con cero). Verifica que el personaje se ve bien.",
        "Para las filas 2..N, pega cada prompt; si tu modelo lo permite, adjunta item_01.png como imagen de referencia para mantener al personaje constante.",
        "Guarda cada una como item_02.png, item_03.png, ... en la misma carpeta.",
        "Llama a ensamblar_test(ilustraciones = 'ruta/carpeta', respuesta_imagen = 'ruta/escala.png', ...)."
      )
      pasos_en <- c(
        "Open the 'prompts' sheet of this file. The ONLY column you copy into the image model is 'prompt'.",
        "Open Gemini, ChatGPT, Midjourney or your favorite text-to-image model.",
        "For row 1, copy the full 'prompt' cell and generate an image (vertical 3:4).",
        "Save it as 'item_01.png' (zero-padded). Confirm the character looks right.",
        "For rows 2..N, paste each prompt; if your model supports it, attach item_01.png as reference image to keep the character consistent.",
        "Save each as item_02.png, item_03.png, ... in the same folder.",
        "Call ensamblar_test(ilustraciones = 'path/to/folder', respuesta_imagen = 'path/to/scale.png', ...)."
      )
      instr_df <- data.frame(
        Paso   = seq_along(pasos_es),
        Accion = if (idioma_docs == "es") pasos_es else pasos_en,
        stringsAsFactors = FALSE
      )
      if (idioma_docs == "es") {
        names(instr_df) <- c("Paso", "Acci\u00F3n")
      } else {
        names(instr_df) <- c("Step", "Action")
      }
      # Hoja principal: solo lo que el usuario pega en el modelo de imagen.
      hoja_principal <- resultado[, c("n_item", "dimension", "item", "prompt")]
      hoja_escenas   <- resultado[, c("n_item", "item", "escena")]

      if (idioma_docs == "es") {
        names(hoja_principal) <- c("N", "Dimensi\u00F3n", "\u00CDtem", "Prompt")
        names(hoja_escenas)   <- c("N", "\u00CDtem", "Escena")
        nombres_hojas <- c("Prompts", "Escenas", "Info", "Instrucciones")
      } else {
        names(hoja_principal) <- c("N", "Dimension", "Item", "Prompt")
        names(hoja_escenas)   <- c("N", "Item", "Scene")
        nombres_hojas <- c("Prompts", "Scenes", "Info", "Instructions")
      }

      lista_hojas <- list(hoja_principal, hoja_escenas, info_df, instr_df)
      names(lista_hojas) <- nombres_hojas
      openxlsx::write.xlsx(lista_hojas, ruta_xlsx, overwrite = TRUE)
      if (verbose) cat("\n[OK] Prompts guardados en: ", ruta_xlsx, "\n", sep = "")
    }
  }

  class(resultado) <- c("semilla_prompts_ilustracion", "data.frame")
  attr(resultado, "personaje") <- personaje
  attr(resultado, "estilo")    <- estilo
  attr(resultado, "paleta")    <- paleta
  attr(resultado, "idioma")    <- idioma_prompts
  resultado
}


#' @export
print.semilla_prompts_ilustracion <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Prompts de ilustraci\u00F3n (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Idioma     : ", attr(x, "idioma"), "\n", sep = "")
  cat("  Paleta     : ", attr(x, "paleta"), "\n", sep = "")
  cat("  N \u00EDtems    : ", nrow(x), "\n", sep = "")
  cat("  Personaje  : ", substr(attr(x, "personaje"), 1, 70),
      if (nchar(attr(x, "personaje")) > 70) "..." else "", "\n", sep = "")
  cat("  Estilo     : ", substr(attr(x, "estilo"),    1, 70),
      if (nchar(attr(x, "estilo"))    > 70) "..." else "", "\n", sep = "")
  cat("-----------------------------------------------------------\n")
  cat("  Ejemplo (item 1):\n")
  cat("    Item   : ", x$item[1], "\n", sep = "")
  cat("    Escena : ", x$escena[1], "\n", sep = "")
  cat("===========================================================\n\n")
  invisible(x)
}
