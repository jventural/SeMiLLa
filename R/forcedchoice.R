#' @title Generar escala con formato Forced-Choice (Thurstoniano)
#'
#' @description
#' Sexto formato de SeMiLLa: items presentados en BLOQUES donde el
#' respondiente debe ELEGIR entre alternativas igualmente deseables (no
#' graduar acuerdo). Combate el sesgo de deseabilidad social y la tendencia
#' central que ni el Likert ni las Historias eliminan completamente.
#'
#' Estructura:
#' \enumerate{
#'   \item Cada \strong{dimension} del constructo aporta varios items al
#'         pool comun.
#'   \item Cada item se etiqueta con su \strong{valencia social}
#'         (deseabilidad, 1-5) estimada por el LLM.
#'   \item Los items se agrupan en \strong{bloques} de tamano K (default 4)
#'         tal que: (a) cada bloque tiene items de K dimensiones distintas,
#'         (b) los items dentro de un bloque tienen valencia social similar
#'         (diferencia <= 1.0). Esto fuerza al respondiente a discriminar
#'         por su rasgo real, no por aparentar bien.
#'   \item En cada bloque el respondiente elige (segun \code{metodo}):
#'         \itemize{
#'           \item \code{"most_least"}: el item que MAS y el que MENOS lo describe (MOLE).
#'           \item \code{"ranking"}: ranking completo del bloque (1 a K).
#'           \item \code{"single_choice"}: solo el item que MEJOR lo describe.
#'         }
#' }
#'
#' Para analizar los datos se usa \strong{Thurstonian IRT} (Brown &
#' Maydeu-Olivares, 2011), implementado en el paquete
#' \pkg{thurstonianIRT}.
#'
#' @section Referencias metodologicas:
#' \itemize{
#'   \item Brown, A., & Maydeu-Olivares, A. (2011). Item response modeling
#'         of forced-choice questionnaires. \emph{Educational and
#'         Psychological Measurement, 71}(3), 460-502.
#'   \item Brown, A., & Maydeu-Olivares, A. (2018). Modelling forced-choice
#'         response formats. En Irwing, P., Booth, T., & Hughes, D. J.
#'         (Eds.) \emph{The Wiley Handbook of Psychometric Testing}.
#'   \item Thurstone, L. L. (1927). A law of comparative judgment.
#'         \emph{Psychological Review, 34}, 273-286.
#' }
#'
#' @param concepto Cadena con la definicion del constructo global.
#' @param api_key Clave de OpenAI.
#' @param dimensiones Vector character con los nombres de las dimensiones
#'   (e.g. los Big Five).
#' @param descripcion_dimensiones Vector character (mismo largo) con la
#'   definicion de cada dimension. Si NULL, el LLM infiere del nombre.
#' @param polaridad_dimensiones Vector character (mismo largo) con la
#'   polaridad de la SUBESCALA: \code{"positiva"} (puntaje alto = mas del
#'   rasgo) o \code{"negativa"} (puntaje alto = menos). Default: todas
#'   positivas. Define la interpretacion teorica del rasgo.
#' @param n_items_por_dimension Numero de items en el pool por dimension
#'   (default 8). Mas pool = mas flexibilidad para armar bloques balanceados.
#' @param proporcion_polo_negativo Proporcion de items que se redactaran en
#'   el POLO NEGATIVO del rasgo (descripciones que reflejan el opuesto al
#'   rasgo, e.g. para "Conciencia": "suelo postergar mis tareas"). Default
#'   0.4 = 40% items en polo negativo + 60% en polo positivo. Mezclar polos
#'   aumenta la variabilidad de valencia social y mejora la calidad de los
#'   bloques balanceados. Pasar 0 reproduce el comportamiento original
#'   (todos los items en polo positivo).
#' @param block_size Tamano del bloque (default 4). Tipicamente 2, 3 o 4.
#'   El paquete \pkg{thurstonianIRT} requiere bloques de tamano consistente.
#' @param n_bloques Numero de bloques en el test final (default 15).
#' @param metodo Metodo de eleccion: \code{"most_least"} (default, MOLE),
#'   \code{"ranking"} o \code{"single_choice"}.
#' @param estimar_valencia Si TRUE (default), llama al LLM para estimar la
#'   valencia social de cada item.
#' @param balancear_valencia Si TRUE (default), arma bloques con items de
#'   valencia similar (diferencia <= \code{tolerancia_valencia}).
#' @param tolerancia_valencia Diferencia maxima de valencia entre items del
#'   mismo bloque (default 1.0 en escala 1-5).
#' @param idioma "es", "en", "pt".
#' @param modelo Modelo OpenAI.
#' @param seed Semilla para reproducibilidad.
#' @param verbose Mostrar progreso.
#'
#' @return Objeto \code{semilla_forcedchoice} con:
#' \itemize{
#'   \item \code{dimensiones}, \code{config}.
#'   \item \code{item_bank}: data.frame con todos los items
#'         (item_id, dimension, polaridad_item, texto_item, valencia_social).
#'   \item \code{bloques}: data.frame en formato largo
#'         (block_id, posicion, item_id, dimension, texto_item, valencia).
#'   \item \code{design_matrix}: matriz Thurstoniana (\code{thurstonianIRT}-ready).
#'   \item \code{validacion_balance}: tabla con metricas de calidad de
#'         los bloques (rango de valencia por bloque, dimensiones distintas).
#'   \item \code{metadata}.
#' }
#'
#' @examples
#' \dontrun{
#' fc <- generar_escala_forcedchoice(
#'   concepto    = "Big Five (modelo de personalidad)",
#'   api_key     = api_key,
#'   dimensiones = c("Apertura", "Conciencia", "Extraversion",
#'                    "Amabilidad", "Estabilidad emocional"),
#'   n_items_por_dimension = 8,
#'   block_size  = 4,
#'   n_bloques   = 15,
#'   metodo      = "most_least"
#' )
#' print(fc)
#' }
#'
#' @export
generar_escala_forcedchoice <- function(
  concepto,
  api_key,
  dimensiones,
  descripcion_dimensiones = NULL,
  polaridad_dimensiones   = NULL,
  n_items_por_dimension   = 8L,
  proporcion_polo_negativo = 0.4,
  block_size              = 4L,
  n_bloques               = 15L,
  metodo                  = c("most_least", "ranking", "single_choice"),
  estimar_valencia        = TRUE,
  balancear_valencia      = TRUE,
  tolerancia_valencia     = 1.0,
  idioma                  = c("es", "en", "pt"),
  modelo                  = "gpt-4.1-mini-2025-04-14",
  seed                    = 2026,
  verbose                 = TRUE
) {

  metodo <- match.arg(metodo)
  idioma <- match.arg(idioma)

  K_dim <- length(dimensiones)
  if (block_size > K_dim)
    stop("'block_size' (", block_size, ") no puede ser mayor que el numero ",
         "de dimensiones (", K_dim, ").")

  if (is.null(polaridad_dimensiones)) {
    polaridad_dimensiones <- rep("positiva", K_dim)
  }
  if (length(polaridad_dimensiones) != K_dim)
    stop("'polaridad_dimensiones' debe tener largo ", K_dim)

  if (!is.null(descripcion_dimensiones) &&
      length(descripcion_dimensiones) != K_dim)
    stop("'descripcion_dimensiones' debe tener largo ", K_dim)
  if (is.null(descripcion_dimensiones))
    descripcion_dimensiones <- dimensiones

  if (!is.null(seed)) {
    options(SeMiLLa.seed = as.integer(seed))
    set.seed(seed)
  }

  # Validar proporcion_polo_negativo
  if (proporcion_polo_negativo < 0 || proporcion_polo_negativo >= 1)
    stop("'proporcion_polo_negativo' debe estar en [0, 1).")
  n_neg_por_dim <- floor(n_items_por_dimension * proporcion_polo_negativo)
  n_pos_por_dim <- n_items_por_dimension - n_neg_por_dim

  if (verbose) {
    cat("\n[generar_escala_forcedchoice] Configurando OpenAI...\n")
    cat("  Constructo: ", concepto, "\n", sep = "")
    cat("  Dimensiones: ", paste(dimensiones, collapse = ", "), "\n", sep = "")
    cat("  Items pool por dimension: ", n_items_por_dimension,
        " (", n_pos_por_dim, " polo+, ", n_neg_por_dim, " polo-)\n", sep = "")
    cat("  Tamano de bloque: ", block_size, "\n", sep = "")
    cat("  N bloques: ", n_bloques, "\n", sep = "")
    cat("  Metodo de eleccion: ", metodo, "\n", sep = "")
  }
  openai <- .configurar_openai(api_key)

  # ---------- 1. Generar pool de items por dimension (mezcla de polos) ----
  if (verbose) cat("\n[1/3] Generando pool de items (",
                    n_items_por_dimension, " por dimension x ",
                    K_dim, " dimensiones = ",
                    n_items_por_dimension * K_dim, " items)...\n", sep = "")

  item_bank_list <- vector("list", K_dim)
  for (d in seq_len(K_dim)) {
    if (verbose) cat("  > [", d, "/", K_dim, "] ", dimensiones[d],
                      "  (", n_pos_por_dim, "+", n_neg_por_dim, "-)\n",
                      sep = "")

    # Items en polo positivo (alta valencia esperada)
    items_pos <- .generar_items_dimension_fc(
      openai = openai, modelo = modelo,
      concepto = concepto,
      dimension = dimensiones[d],
      descripcion = descripcion_dimensiones[d],
      polo_item = "positivo",
      polaridad_dim = polaridad_dimensiones[d],
      n_items = n_pos_por_dim,
      idioma = idioma
    )

    # Items en polo negativo (baja valencia esperada)
    items_neg <- if (n_neg_por_dim > 0L) {
      .generar_items_dimension_fc(
        openai = openai, modelo = modelo,
        concepto = concepto,
        dimension = dimensiones[d],
        descripcion = descripcion_dimensiones[d],
        polo_item = "negativo",
        polaridad_dim = polaridad_dimensiones[d],
        n_items = n_neg_por_dim,
        idioma = idioma
      )
    } else character(0)

    df_pos <- if (length(items_pos) > 0L) {
      data.frame(
        item_id        = paste0(substr(dimensiones[d], 1, 3), "_p",
                                  seq_along(items_pos)),
        dimension      = dimensiones[d],
        polo_item      = "positivo",
        polaridad_dim  = polaridad_dimensiones[d],
        texto_item     = items_pos,
        stringsAsFactors = FALSE
      )
    } else NULL
    df_neg <- if (length(items_neg) > 0L) {
      data.frame(
        item_id        = paste0(substr(dimensiones[d], 1, 3), "_n",
                                  seq_along(items_neg)),
        dimension      = dimensiones[d],
        polo_item      = "negativo",
        polaridad_dim  = polaridad_dimensiones[d],
        texto_item     = items_neg,
        stringsAsFactors = FALSE
      )
    } else NULL
    item_bank_list[[d]] <- rbind(df_pos, df_neg)
  }
  item_bank <- do.call(rbind, item_bank_list)

  # ---------- 2. Estimar valencia social ----------
  if (isTRUE(estimar_valencia)) {
    if (verbose) cat("\n[2/3] Estimando valencia social de ",
                      nrow(item_bank), " items...\n", sep = "")
    item_bank$valencia_social <- .estimar_valencia_social_lote(
      openai = openai, modelo = modelo,
      items = item_bank$texto_item,
      idioma = idioma
    )
  } else {
    item_bank$valencia_social <- 3  # neutro por defecto
  }

  if (verbose) {
    cat("  Distribucion de valencia social:\n")
    print(round(quantile(item_bank$valencia_social,
                          probs = c(0, .25, .5, .75, 1), na.rm = TRUE), 2))
  }

  # ---------- 3. Armar bloques balanceados ----------
  if (verbose) cat("\n[3/3] Armando ", n_bloques, " bloques de tamano ",
                    block_size, " (balanceando valencia)...\n", sep = "")
  bloques_df <- .armar_bloques_fc(
    item_bank = item_bank,
    block_size = block_size,
    n_bloques = n_bloques,
    balancear_valencia = balancear_valencia,
    tolerancia_valencia = tolerancia_valencia,
    verbose = verbose
  )

  # Validacion de balance
  validacion <- .validar_bloques_fc(bloques_df, item_bank, block_size)
  if (verbose) {
    cat("\n[Validacion de bloques]\n")
    print(validacion)
  }

  # Matriz de diseño Thurstoniana (formato thurstonianIRT)
  design_matrix <- .construir_design_matrix_fc(bloques_df, block_size)

  resultado <- list(
    concepto             = concepto,
    dimensiones          = dimensiones,
    config               = list(
      n_items_por_dimension = n_items_por_dimension,
      block_size            = block_size,
      n_bloques             = n_bloques,
      metodo                = metodo,
      estimar_valencia      = estimar_valencia,
      balancear_valencia    = balancear_valencia,
      tolerancia_valencia   = tolerancia_valencia,
      polaridad_dimensiones = polaridad_dimensiones
    ),
    item_bank            = item_bank,
    bloques              = bloques_df,
    design_matrix        = design_matrix,
    validacion_balance   = validacion,
    idioma               = idioma,
    metadata             = list(
      modelo  = modelo,
      seed    = seed,
      fecha   = format(Sys.Date()),
      n_items_total = nrow(item_bank),
      n_bloques     = n_bloques
    )
  )
  class(resultado) <- c("semilla_forcedchoice", "list")
  resultado
}


# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
.generar_items_dimension_fc <- function(openai, modelo, concepto, dimension,
                                          descripcion, polo_item,
                                          polaridad_dim, n_items,
                                          idioma) {

  if (n_items <= 0L) return(character(0))

  if (idioma == "es") {
    sys_msg <- paste(
      "Eres un experto en construccion de items para escalas de personalidad",
      "en formato FORCED-CHOICE (Brown & Maydeu-Olivares, 2018). Tus items:",
      "1) Son afirmaciones autodescriptivas en PRIMERA PERSONA.",
      "2) Cada item describe UNA conducta o disposicion concreta.",
      "3) NO uses cuantificadores absolutos ('siempre', 'nunca').",
      "4) Vocabulario simple, una sola oracion (max 14 palabras).",
      "5) El item debe poder agruparse con items de otras dimensiones",
      "   en bloques de valencia social similar (mas adelante)."
    )

    if (polo_item == "positivo") {
      polo_msg <- paste(
        "Genera items en el POLO POSITIVO/ALTO del rasgo. Estos items",
        "describen la conducta o disposicion CARACTERISTICA del rasgo en",
        "su nivel ALTO. Ejemplo para 'Conciencia': 'Cumplo con mis tareas",
        "a tiempo'. Ejemplo para 'Apertura': 'Disfruto explorar ideas",
        "nuevas'. Endorsar = mas del rasgo. Tienden a tener valencia social",
        "alta (4-5)."
      )
    } else {
      polo_msg <- paste(
        "Genera items en el POLO NEGATIVO/BAJO del rasgo. Estos items",
        "describen la conducta o disposicion CONTRARIA al rasgo (su polo",
        "bajo). Ejemplo para 'Conciencia': 'Suelo postergar mis",
        "obligaciones'. Ejemplo para 'Apertura': 'Prefiero rutinas",
        "conocidas a probar cosas nuevas'. Endorsar = menos del rasgo.",
        "Tienden a tener valencia social baja a media (1-3). NO uses",
        "etiquetas patologicas ni juicios morales: describe conductas",
        "concretas y observables, no valoraciones."
      )
    }

    user_msg <- paste0(
      "Constructo global: ", concepto, "\n",
      "Dimension: ", dimension, " - ", descripcion, "\n",
      "Polaridad teorica de la subescala: ", polaridad_dim, "\n",
      "Polo del ITEM solicitado: ", toupper(polo_item), "\n\n",
      polo_msg, "\n\n",
      "Genera EXACTAMENTE ", n_items, " items distintos para esta",
      " dimension en el polo ", polo_item, ". Cada item: una sola",
      " oracion declarativa en primera persona, max 14 palabras.",
      " Devuelve cada item en una linea numerada (1., 2., ...). Sin",
      " titulos ni comillas."
    )
  } else {
    sys_msg <- "You build forced-choice personality items, first-person, single sentence."
    user_msg <- paste0("Construct: ", concepto, ". Dimension: ", dimension,
                        ". Pole: ", polo_item, ". Generate ", n_items, " items.")
  }

  raw <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 700L, temperature = 0.5
  )

  lineas <- unlist(strsplit(raw, "\n", fixed = TRUE))
  lineas <- trimws(lineas)
  lineas <- lineas[nzchar(lineas)]
  lineas <- sub("^[0-9]+[.\\)]\\s*", "", lineas, perl = TRUE)
  lineas <- sub("^[-*]\\s*", "", lineas)
  lineas <- lineas[nzchar(lineas)]

  if (length(lineas) < n_items) {
    warning("Solo se obtuvieron ", length(lineas), "/", n_items,
            " items para ", dimension, ". Se rellena con duplicados.")
    lineas <- c(lineas, rep(lineas[1], n_items - length(lineas)))
  }
  lineas[seq_len(n_items)]
}


#' @keywords internal
.estimar_valencia_social_lote <- function(openai, modelo, items, idioma) {

  # Lote de items numerados
  items_numerados <- paste0(seq_along(items), ". ", items, collapse = "\n")

  if (idioma == "es") {
    sys_msg <- paste(
      "Eres un juez experto en deseabilidad social (Edwards, 1957;",
      "Crowne & Marlowe, 1960). Recibes una lista de items autodescriptivos",
      "y devuelves la VALENCIA SOCIAL de cada uno en escala 1-5 donde:",
      "1 = altamente INDESEABLE socialmente (la mayoria evitaria endorsarlo)",
      "2 = algo indeseable",
      "3 = neutro (igualmente endorsable que no)",
      "4 = algo deseable",
      "5 = altamente DESEABLE socialmente (la mayoria querria endorsarlo)",
      "",
      "Devuelve SOLO un objeto JSON con un array 'valencias' de N enteros,",
      "uno por item, en el mismo orden. Sin texto adicional, sin markdown."
    )
    user_msg <- paste0(
      "Items a evaluar (", length(items), " en total):\n\n",
      items_numerados, "\n\n",
      "Devuelve: {\"valencias\": [3, 4, 2, ...]} (un valor por item)."
    )
  } else {
    sys_msg <- "Rate social desirability of each item from 1 (highly undesirable) to 5 (highly desirable). Return JSON {valencias: [...]}."
    user_msg <- paste0("Items:\n", items_numerados)
  }

  raw <- .llamar_openai(
    openai = openai,
    messages = list(
      list(role = "system", content = sys_msg),
      list(role = "user",   content = user_msg)
    ),
    modelo = modelo, max_tokens = 1500L, temperature = 0.2
  )

  raw_clean <- .limpiar_json(raw)
  parsed <- tryCatch(jsonlite::fromJSON(raw_clean, simplifyVector = TRUE),
                     error = function(e) NULL)

  if (is.null(parsed) || is.null(parsed$valencias)) {
    warning("No se pudo parsear valencias. Asignando 3 (neutro) a todos.")
    return(rep(3, length(items)))
  }
  v <- as.numeric(parsed$valencias)
  if (length(v) != length(items)) {
    warning("Valencias devueltas (", length(v), ") != items (",
            length(items), "). Se ajusta con 3 (neutro).")
    v <- c(v, rep(3, length(items)))[seq_along(items)]
  }
  v
}


#' @keywords internal
.armar_bloques_fc <- function(item_bank, block_size, n_bloques,
                                balancear_valencia, tolerancia_valencia,
                                verbose) {

  bloques_list  <- list()
  items_usados <- rep(0L, nrow(item_bank))
  names(items_usados) <- item_bank$item_id

  # Estrategia: estratificar items por valencia y por dimension; en cada
  # bloque tomar 1 item de block_size dimensiones distintas, con valencia
  # similar.
  dims_unicas <- unique(item_bank$dimension)

  if (block_size > length(dims_unicas))
    stop("Block size (", block_size, ") > numero de dimensiones (",
         length(dims_unicas), ").")

  for (b in seq_len(n_bloques)) {
    bloque <- .armar_un_bloque_fc(
      item_bank, items_usados, block_size,
      dims_unicas, balancear_valencia, tolerancia_valencia
    )
    if (is.null(bloque)) {
      warning("No se pudo armar el bloque ", b,
              " con la tolerancia de valencia. Se rebaja temporalmente.")
      bloque <- .armar_un_bloque_fc(
        item_bank, items_usados, block_size,
        dims_unicas, balancear_valencia,
        tolerancia_valencia = tolerancia_valencia + 1.5
      )
      if (is.null(bloque)) {
        stop("Imposible armar el bloque ", b,
             ". Aumenta n_items_por_dimension o reduce n_bloques.")
      }
    }
    items_usados[bloque$item_id] <- items_usados[bloque$item_id] + 1L
    bloque$block_id <- b
    bloque$posicion <- seq_len(nrow(bloque))
    bloques_list[[b]] <- bloque
  }

  bloques_df <- do.call(rbind, bloques_list)
  # Reorganizar columnas
  bloques_df[, c("block_id", "posicion", "item_id", "dimension",
                  "polo_item", "polaridad_dim", "texto_item",
                  "valencia_social")]
}


#' @keywords internal
.armar_un_bloque_fc <- function(item_bank, items_usados, block_size,
                                  dims_unicas, balancear_valencia,
                                  tolerancia_valencia) {
  # Elegir block_size dimensiones (preferentemente las menos usadas)
  uso_por_dim <- vapply(dims_unicas, function(d) {
    sum(items_usados[item_bank$dimension == d])
  }, integer(1))
  dims_orden <- dims_unicas[order(uso_por_dim, runif(length(dims_unicas)))]
  dims_bloque <- dims_orden[seq_len(block_size)]

  if (!balancear_valencia) {
    # Tomar item al azar de cada dimension entre los menos usados
    bloque <- do.call(rbind, lapply(dims_bloque, function(d) {
      cand <- item_bank[item_bank$dimension == d, ]
      cand$uso <- items_usados[cand$item_id]
      cand <- cand[cand$uso == min(cand$uso), ]
      cand[sample(seq_len(nrow(cand)), 1L), ]
    }))
    return(bloque)
  }

  # Estrategia balanceada: elegir un valor central de valencia y tomar items
  # de cada dimension cuya valencia este dentro de [centro - tol, centro + tol]
  # Probar varios centros aleatorios.
  centros <- sample(seq(2, 4, by = 0.5))
  for (centro in centros) {
    bloque_intento <- vector("list", block_size)
    ok <- TRUE
    for (i in seq_len(block_size)) {
      d <- dims_bloque[i]
      cand <- item_bank[item_bank$dimension == d &
                          abs(item_bank$valencia_social - centro) <=
                            tolerancia_valencia, ]
      cand$uso <- items_usados[cand$item_id]
      if (nrow(cand) == 0L) { ok <- FALSE; break }
      cand <- cand[cand$uso == min(cand$uso), ]
      bloque_intento[[i]] <- cand[sample(seq_len(nrow(cand)), 1L), ]
    }
    if (ok) {
      return(do.call(rbind, bloque_intento))
    }
  }
  NULL
}


#' @keywords internal
.validar_bloques_fc <- function(bloques_df, item_bank, block_size) {
  blocks_unique <- unique(bloques_df$block_id)
  metricas <- vapply(blocks_unique, function(b) {
    bloque <- bloques_df[bloques_df$block_id == b, ]
    rango_valencia <- max(bloque$valencia_social) -
                       min(bloque$valencia_social)
    n_dims_distintas <- length(unique(bloque$dimension))
    c(rango = rango_valencia, dims = n_dims_distintas)
  }, numeric(2))

  # Mezcla de polos en el pool y en los bloques
  pct_pos_pool <- mean(item_bank$polo_item == "positivo") * 100
  pct_pos_bloq <- mean(bloques_df$polo_item == "positivo") * 100

  # Bloques con mezcla de polos (mas robustos contra deseabilidad)
  bloques_con_mezcla <- vapply(blocks_unique, function(b) {
    bloque <- bloques_df[bloques_df$block_id == b, ]
    length(unique(bloque$polo_item)) > 1L
  }, logical(1))

  data.frame(
    metrica = c(
      "Numero de bloques",
      "Tamano de bloque",
      "Bloques con K dimensiones distintas (%)",
      "Rango medio de valencia por bloque",
      "Rango maximo de valencia",
      "Items unicos usados (% del pool)",
      "Items reutilizados (en >=2 bloques)",
      "Items polo+ en pool (%)",
      "Items polo+ usados en bloques (%)",
      "Bloques con MEZCLA de polos (%)"
    ),
    valor = c(
      length(blocks_unique),
      block_size,
      round(mean(metricas["dims", ] == block_size) * 100, 1),
      round(mean(metricas["rango", ]), 2),
      round(max(metricas["rango", ]), 2),
      round(length(unique(bloques_df$item_id)) /
              nrow(item_bank) * 100, 1),
      sum(table(bloques_df$item_id) >= 2L),
      round(pct_pos_pool, 1),
      round(pct_pos_bloq, 1),
      round(mean(bloques_con_mezcla) * 100, 1)
    ),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
.construir_design_matrix_fc <- function(bloques_df, block_size) {
  # Genera todos los pares dentro de cada bloque (formato 'pairwise' que
  # usa thurstonianIRT::makeTIRTfit). Incluye polo del item para que el
  # modelo Thurstoniano use cargas con signo correcto.
  blocks_unique <- unique(bloques_df$block_id)
  pares_list <- list()
  par_idx <- 0L
  for (b in blocks_unique) {
    items_b <- bloques_df[bloques_df$block_id == b, ]
    pairs <- t(combn(seq_len(nrow(items_b)), 2L))
    for (p in seq_len(nrow(pairs))) {
      par_idx <- par_idx + 1L
      i <- pairs[p, 1L]; j <- pairs[p, 2L]
      pares_list[[par_idx]] <- data.frame(
        block_id    = b,
        pair_id     = par_idx,
        item_left   = items_b$item_id[i],
        item_right  = items_b$item_id[j],
        dim_left    = items_b$dimension[i],
        dim_right   = items_b$dimension[j],
        polo_left   = items_b$polo_item[i],
        polo_right  = items_b$polo_item[j],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, pares_list)
}


# =============================================================================
# Print method
# =============================================================================

#' @export
print.semilla_forcedchoice <- function(x, ...) {
  cat("\n")
  cat("===========================================================\n")
  cat("  Escala FORCED-CHOICE / Thurstoniana (SeMiLLa)\n")
  cat("===========================================================\n")
  cat("  Constructo  : ", x$concepto, "\n", sep = "")
  cat("  Dimensiones : ", paste(x$dimensiones, collapse = ", "), "\n",
      sep = "")
  cat("  Pool items  : ", nrow(x$item_bank), " (",
      x$config$n_items_por_dimension, " por dimension)\n", sep = "")
  cat("  Bloques     : ", x$config$n_bloques, " (tamano = ",
      x$config$block_size, ", metodo = ", x$config$metodo, ")\n", sep = "")
  cat("  Pares total : ", nrow(x$design_matrix),
      " (para Thurstonian IRT)\n", sep = "")
  cat("-----------------------------------------------------------\n")
  cat("  Validacion de bloques:\n")
  print(x$validacion_balance, row.names = FALSE)
  cat("-----------------------------------------------------------\n")
  cat("  Primer bloque (ejemplo):\n")
  b1 <- x$bloques[x$bloques$block_id == 1, ]
  for (k in seq_len(nrow(b1))) {
    polo_marca <- if (b1$polo_item[k] == "positivo") "(+)" else "(-)"
    cat("    (", letters[k], ") [", b1$dimension[k], " ", polo_marca,
        ", val=", b1$valencia_social[k], "] ",
        b1$texto_item[k], "\n", sep = "")
  }
  cat("===========================================================\n\n")
  invisible(x)
}
