#' @title Obtener Embeddings de Items
#'
#' @description
#' Calcula embeddings (representaciones vectoriales) de los items usando OpenAI.
#' Los embeddings permiten analizar la similitud semantica entre items.
#'
#' @param items Objeto semilla_items, semilla, o dataframe con columna 'item'
#' @param api_key Tu API key de OpenAI
#' @param modelo_embedding Modelo: "text-embedding-3-small" (default),
#'        "text-embedding-3-large", o "text-embedding-ada-002"
#' @param verbose Mostrar progreso
#'
#' @return Objeto de clase 'semilla_embeddings' con:
#' \itemize{
#'   \item \code{embeddings}: Matriz de embeddings (items x dimensiones)
#'   \item \code{items}: Dataframe de items
#'   \item \code{similitud}: Matriz de similitud coseno
#' }
#'
#' @details
#' Los embeddings son representaciones vectoriales densas que capturan el
#' significado semantico del texto. La similitud coseno entre embeddings
#' predice correlaciones empiricas entre items (Wulff & Mata, 2025).
#'
#' El modelo text-embedding-3-small genera vectores de 1536 dimensiones
#' optimizados para tareas de similitud semantica.
#'
#' @examples
#' \dontrun{
#' # Calcular embeddings
#' emb <- obtener_embeddings(items_generados, api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Ver matriz de similitud
#' View(emb$similitud)
#' }
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement.
#' Nature Human Behaviour, 9(5), 944-954.
#' https://doi.org/10.1038/s41562-024-02089-y
#'
#' OpenAI (2024). Embeddings - OpenAI API Documentation.
#' https://platform.openai.com/docs/guides/embeddings
#'
#' @export
obtener_embeddings <- function(items,
                               api_key,
                               modelo_embedding = "text-embedding-3-small",
                               verbose = TRUE) {

  # Extraer items segun tipo de objeto
  if (inherits(items, "semilla")) {
    items_df <- items$items
  } else if (inherits(items, "semilla_items")) {
    items_df <- items$items
  } else if (is.data.frame(items) && "item" %in% names(items)) {
    items_df <- items
  } else {
    # Antes se respondia siempre lo mismo -"Objeto no valido. Usa un objeto
    # semilla, semilla_items, o dataframe con columna 'item'"- y quien llegaba
    # aqui con una prueba objetiva no tenia forma de entender que su
    # instrumento, sencillamente, no se analiza asi. Caso real del 11-ago-2026:
    # una prueba objetiva de 20 items cuyos enunciados viven en la columna
    # 'enunciado'. Ahora se reconoce el objeto y se explica el porque.
    tipado <- c(semilla_prueba_objetiva = "prueba objetiva",
                semilla_historias       = "escala de historias",
                semilla_guttman         = "escala de Guttman",
                semilla_test_cognitivo  = "test cognitivo",
                semilla_forcedchoice    = "escala de eleccion forzada")
    cual <- tipado[intersect(class(items), names(tipado))]
    if (length(cual)) {
      stop(sprintf(paste0(
        "Los embeddings son para escalas tipo Likert y esto es una %s.\n",
        "  El analisis semantico compara el PARECIDO entre los textos de los items ",
        "para ver si se agrupan como dice la teoria. En una %s la estructura la fija ",
        "la tabla de especificaciones, y sus propiedades se estudian con las ",
        "respuestas de las personas (dificultad, discriminacion, funcionamiento de ",
        "los distractores), no con el parecido entre enunciados.\n",
        "  Tus items ya estan completos: pasa directamente a ensamblar el ",
        "entregable con ensamblar_test()."), cual[[1]], cual[[1]]), call. = FALSE)
    }
    # Un data.frame con los items en otra columna: se dice cual falta y cuales hay.
    if (is.data.frame(items)) {
      stop(sprintf(paste0(
        "El data.frame no tiene columna 'item'. Columnas encontradas: %s.\n",
        "  Renombra a 'item' la que contiene el texto de los items."),
        paste(names(items), collapse = ", ")), call. = FALSE)
    }
    if (is.list(items) && is.data.frame(items$items)) {
      stop(sprintf(paste0(
        "El objeto trae $items pero sin columna 'item'. Columnas: %s.\n",
        "  Si es un instrumento que no es una escala Likert, los embeddings no ",
        "se le aplican."), paste(names(items$items), collapse = ", ")), call. = FALSE)
    }
    stop("Objeto no valido. Usa un objeto semilla, semilla_items, o dataframe con columna 'item'",
         call. = FALSE)
  }

  items_texto <- items_df$item
  n_items <- length(items_texto)

  # Determinar el backend: OpenAI (remoto) o sentence-transformers (local, libre)
  usar_local <- .es_modelo_local(modelo_embedding)
  # Modelos "hf:..." van por el router de HuggingFace: ni son locales ni pasan
  # por el cliente de OpenAI, asi que no deben validarse como clave sk-.
  usar_hf_emb <- .es_modelo_hf_emb(modelo_embedding)

  # Validar API key solo si se usa un proveedor remoto (OpenAI)
  openai <- NULL
  if (!usar_local && !usar_hf_emb) {
    if (!missing(api_key)) {
      .validar_api_key(api_key)
      openai <- .configurar_openai(api_key)
    } else {
      # Intentar obtener de variable de entorno
      api_key <- Sys.getenv("OPENAI_API_KEY")
      if (nchar(api_key) < 10) {
        stop("API key no proporcionada. Usa api_key = 'tu-key' o configura OPENAI_API_KEY")
      }
      openai <- .configurar_openai(api_key)
    }
  }

  if (verbose) {
    cat("  ", .color_flecha(), " Procesando ", n_items, " items...\n", sep = "")
  }

  # Las dimensiones se determinan a partir de la matriz obtenida (mas abajo),
  # de modo que el codigo funcione tanto con OpenAI como con modelos locales.

  # ---------------------------------------------------------------------------
  # CACHE: hashear el vector (ordenado) de items + modelo. Los embeddings de
  # OpenAI son deterministicos (mismo input -> mismo output), pero cachearlos
  # ahorra costos y hace la reproducibilidad totalmente independiente de la API.
  # ---------------------------------------------------------------------------
  cache_path <- NULL
  embeddings_matrix <- NULL
  if (.cache_enabled()) {
    payload <- list(
      tipo = "embeddings",
      input = items_texto,
      modelo = modelo_embedding
    )
    cache_path <- .cache_key("embeddings", payload)
    cached <- .cache_get(cache_path)
    if (!is.null(cached)) {
      .cache_msg_hit("embeddings")
      embeddings_matrix <- cached
    } else {
      .cache_msg_miss("embeddings")
    }
  }

  if (is.null(embeddings_matrix)) {
    if (usar_hf_emb) {
      # Backend GRATUITO por el router de HuggingFace: la api_key que llega
      # aqui es el token hf_ (o se toma de HF_TOKEN).
      tok <- if (!missing(api_key) && is.character(api_key) && nzchar(api_key))
               api_key else Sys.getenv("HF_TOKEN", "")
      embeddings_matrix <- .embeddings_hf(items_texto, modelo_embedding,
                                          hf_token = tok, verbose = verbose)
    } else if (usar_local) {
      # Backend local de acceso libre (sentence-transformers via reticulate)
      embeddings_matrix <- .embeddings_locales(items_texto, modelo_embedding, verbose = verbose)
    } else {
      if (verbose) cat("  ", .color_flecha(), " Conectando con OpenAI...\n", sep = "")

      respuesta <- tryCatch({
        openai$embeddings$create(
          model = modelo_embedding,
          input = items_texto
        )
      }, error = function(e) {
        stop("Error al obtener embeddings: ", e$message)
      })

      # Extraer matriz
      embeddings_matrix <- do.call(rbind, lapply(respuesta$data, function(x) x$embedding))
    }

    # Guardar en cache
    if (!is.null(cache_path)) {
      .cache_set(cache_path, embeddings_matrix)
    }
  }

  rownames(embeddings_matrix) <- paste0("item_", 1:n_items)
  dimensiones <- ncol(embeddings_matrix)

  if (verbose) {
    cat("  ", .color_flecha(), " Embeddings: ", nrow(embeddings_matrix), " x ",
        ncol(embeddings_matrix), "\n", sep = "")
  }

  # Calcular similitud coseno
  if (verbose) cat("  ", .color_flecha(), " Calculando similitud...\n", sep = "")

  similitud <- .calcular_similitud_coseno(embeddings_matrix)
  rownames(similitud) <- paste0("item_", 1:n_items)
  colnames(similitud) <- paste0("item_", 1:n_items)

  # Resultado
  resultado <- list(
    embeddings = embeddings_matrix,
    items = items_df,
    similitud = similitud,
    metadata = list(
      n_items = n_items,
      modelo = modelo_embedding,
      dimensiones = dimensiones,
      fecha = Sys.time()
    )
  )

  class(resultado) <- c("semilla_embeddings", "list")

  if (verbose) {
    cat("  ", .color_check(), " Embeddings calculados\n", sep = "")
  }

  return(resultado)
}


#' @title Buscar Items Similares
#'
#' @description
#' Encuentra items semanticamente similares a un item dado.
#'
#' @param embeddings Objeto semilla_embeddings o semilla
#' @param item Numero del item o texto
#' @param top Numero de items similares (default: 5)
#'
#' @return Dataframe con items similares
#'
#' @examples
#' \dontrun{
#' # Items similares al item 1
#' items_similares(emb, item = 1, top = 5)
#' }
#'
#' @export
items_similares <- function(embeddings, item, top = 5) {

  # Extraer embeddings segun tipo
  if (inherits(embeddings, "semilla")) {
    emb <- list(
      similitud = embeddings$similitud,
      items = embeddings$items
    )
  } else if (inherits(embeddings, "semilla_embeddings")) {
    emb <- embeddings
  } else {
    stop("Objeto no valido. Usa un objeto semilla o semilla_embeddings")
  }

  # Determinar indice
  if (is.numeric(item)) {
    idx <- item
  } else {
    idx <- which(emb$items$item == item)
    if (length(idx) == 0) stop("Item no encontrado")
    idx <- idx[1]
  }

  # Obtener similitudes
  sims <- emb$similitud[idx, ]
  orden <- order(sims, decreasing = TRUE)
  orden <- orden[orden != idx]
  top_idx <- orden[1:min(top, length(orden))]

  resultado <- data.frame(
    item_num = top_idx,
    factor = emb$items$dimension[top_idx],
    item = emb$items$item[top_idx],
    similitud = round(sims[top_idx], 4),
    stringsAsFactors = FALSE
  )

  # Mostrar
  cat("\n", .color_verde("Item de referencia"), " (#", idx, "):\n", sep = "")
  cat("  [", emb$items$dimension[idx], "] ", emb$items$item[idx], "\n\n", sep = "")
  cat(.color_verde("Items mas similares:"), "\n")
  cat(.linea("-"), "\n")

  for (i in 1:nrow(resultado)) {
    cat(sprintf("  #%d (%.1f%%) [%s] %s\n",
                resultado$item_num[i],
                resultado$similitud[i] * 100,
                resultado$factor[i],
                resultado$item[i]))
  }
  cat("\n")

  invisible(resultado)
}


#' @title Analizar Redundancia de Items
#'
#' @description
#' Identifica pares de items potencialmente redundantes.
#'
#' @param embeddings Objeto semilla_embeddings o semilla
#' @param umbral Umbral de similitud. Default \code{"auto"} (v2.7.0):
#'   cuantil .95 de las similitudes de la propia escala, acotado a
#'   [0.70, 0.85]. Calibrado con dos escalas reales (n=280): un umbral FIJO
#'   no sirve — 0.85 no capturo ninguna parafrasis danina en PM (vivian en
#'   .56-.78) y 0.70 sobre-alarmaba en ACO (actitud hacia un objeto unico,
#'   linea base de similitud alta). Puede fijarse un numero. Para clusters
#'   de faceta repetida use \code{\link{auditar_redundancia}}.
#'
#' @return Dataframe con pares redundantes
#'
#' @export
analizar_redundancia <- function(embeddings, umbral = "auto") {

  # Extraer embeddings segun tipo
  if (inherits(embeddings, "semilla")) {
    emb <- list(
      similitud = embeddings$similitud,
      items = embeddings$items
    )
  } else if (inherits(embeddings, "semilla_embeddings")) {
    emb <- embeddings
  } else {
    stop("Objeto no valido")
  }

  n <- nrow(emb$similitud)
  redundantes <- data.frame()

  # Umbral adaptativo a la linea base de similitud de la escala
  #
  # v2.9.31: MISMA calibracion que auditar_redundancia(). Hasta 2.9.30 esta
  # funcion usaba min(0.85, max(0.70, q95)) y auditar_redundancia() usaba
  # min(0.70, max(0.62, q95)): dos "auto" con rangos practicamente disjuntos
  # sobre la MISMA matriz. Medido el 2026-08-20 sobre una escala real de 24
  # items (q95 = .632): esta funcion reportaba 2 pares redundantes y
  # auditar_redundancia() reportaba 14. El Paso 6 de la App llama a las dos y
  # las muestra en la misma pantalla, asi que el usuario leia las dos cifras
  # juntas sin forma de saber cual valia.
  #
  # Se conserva la de auditar_redundancia() porque es la calibrada contra datos
  # reales: en la bateria policial (n=280) los pares con r policorica >= .70
  # -dependencia local de verdad- viven en coseno .43-.78, de modo que un
  # umbral de .80-.85 no detecta NINGUNO. El techo .70 es la linea de
  # deteccion; por debajo, quien detecta de forma fiable es el juez LLM de
  # parafrasis (blindar_escala()).
  if (identical(umbral, "auto")) {
    ut <- upper.tri(emb$similitud)
    umbral <- min(0.70, max(0.62,
      as.numeric(stats::quantile(emb$similitud[ut], 0.95, na.rm = TRUE))))
  }

  # Dimension de cada item (si esta disponible) para agregados por dimension
  dim_vec <- if (!is.null(emb$items) && "dimension" %in% names(emb$items))
    as.character(emb$items$dimension) else rep(NA_character_, n)

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (emb$similitud[i, j] >= umbral) {
        redundantes <- rbind(redundantes, data.frame(
          item1_num = i,
          item1 = emb$items$item[i],
          dim1 = dim_vec[i],
          item2_num = j,
          item2 = emb$items$item[j],
          dim2 = dim_vec[j],
          similitud = round(emb$similitud[i, j], 4),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  if (nrow(redundantes) > 0) {
    redundantes <- redundantes[order(-redundantes$similitud), ]
    rownames(redundantes) <- NULL
  }

  # v2.9.31: redondeado. Con el umbral fijo salia "70%"; con el adaptativo
  # imprimia "63.20027%".
  cat("
", .color_verde("ANALISIS DE REDUNDANCIA"),
      sprintf(" (umbral: %.0f%%)
", umbral * 100), sep = "")
  cat(.linea("-"), "\n")

  if (nrow(redundantes) == 0) {
    cat("No se encontraron items redundantes.\n\n")
  } else {
    cat("Pares redundantes: ", nrow(redundantes), "\n\n", sep = "")
    for (i in 1:min(5, nrow(redundantes))) {
      cat(sprintf("  %.1f%%: #%d vs #%d\n",
                  redundantes$similitud[i] * 100,
                  redundantes$item1_num[i],
                  redundantes$item2_num[i]))
    }
    cat("\n")
  }

  # Guardar el umbral usado para que plot_redundancia() pueda anotarlo
  attr(redundantes, "umbral") <- umbral

  invisible(redundantes)
}
