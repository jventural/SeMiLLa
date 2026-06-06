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
    stop("Objeto no valido. Usa un objeto semilla, semilla_items, o dataframe con columna 'item'")
  }

  items_texto <- items_df$item
  n_items <- length(items_texto)

  # Determinar el backend: OpenAI (remoto) o sentence-transformers (local, libre)
  usar_local <- .es_modelo_local(modelo_embedding)

  # Validar API key solo si se usa un proveedor remoto (OpenAI)
  openai <- NULL
  if (!usar_local) {
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
    if (usar_local) {
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
#' @param umbral Umbral de similitud (default: 0.85)
#'
#' @return Dataframe con pares redundantes
#'
#' @export
analizar_redundancia <- function(embeddings, umbral = 0.85) {

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

  cat("\n", .color_verde("ANALISIS DE REDUNDANCIA"), " (umbral: ", umbral * 100, "%)\n", sep = "")
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
