# =============================================================================
# analizar_tolerancia() -- prueba de estres pre-empirica en UN comando.
# Envuelve el flujo del metodo (deseabilidad LLM -> similitud embeddings ->
# pronostico -> prueba de estres) y lo narra paso a paso en lenguaje llano.
# =============================================================================

#' Analisis de tolerancia de una escala: prueba de estres pre-empirica en un comando
#'
#' Envoltura de alto nivel que ejecuta, en una sola llamada, el metodo completo de
#' prueba de estres pre-empirica sobre UNA escala: (1) mide la deseabilidad social
#' de cada item con un modelo de lenguaje; (2) mide la similitud semantica entre
#' items con embeddings; (3) pronostica si la estructura factorial prevista
#' sobrevivira; y (4) somete la escala a dosis crecientes de seis sesgos de
#' respuesta hasta hallar el punto de quiebre y los items que caen primero. Con
#' \code{verbose = TRUE} (por defecto) narra cada paso y explica como leer el
#' resultado, de modo que el usuario entienda el proceso sin conocer el detalle
#' interno de \code{\link{calificar_deseabilidad}}, \code{\link{simular_estructura}}
#' y \code{\link{estres_escala}}, que son las funciones que orquesta.
#'
#' El pronostico se interpreta como una CLASE CUALITATIVA (alta/media/baja
#' probabilidad de estructura limpia) acompanada de su rango entre escenarios, no
#' como una probabilidad puntual. Un pronostico optimista indica ausencia de
#' riesgo INDUCIDO POR EL FRASEO; no es un aval de validez ni sustituye al estudio
#' de campo.
#'
#' @param x \code{data.frame} con columnas \code{item} (texto del enunciado) y
#'   \code{dimension} (a que factor pertenece), u objeto \code{semilla}/lista con
#'   \code{$items}.
#' @param concepto Nombre del constructo que mide la escala (mejora la
#'   calificacion de deseabilidad). Opcional.
#' @param poblacion Poblacion objetivo, p. ej. \code{"adultos de poblacion
#'   general"}. Opcional.
#' @param deseabilidad Vector 0-1 por item ya medido (opcional; si se entrega, se
#'   omite la llamada al LLM del paso 1).
#' @param similitud Matriz de similitud de embeddings ya medida (opcional; si se
#'   entrega, se omite la llamada del paso 2).
#' @param api_key Clave OpenAI. Solo se necesita si hay que medir deseabilidad o
#'   embeddings (es decir, si no se entregan como argumentos).
#' @param modelo Modelo de lenguaje para la deseabilidad (por defecto
#'   \code{"gpt-4.1-mini"}).
#' @param modelo_embedding Modelo de embeddings para la similitud (por defecto
#'   \code{"text-embedding-3-small"}).
#' @param k_cat Numero de categorias de respuesta de la escala (2 = dicotomica,
#'   5 = Likert de 5, etc.).
#' @param n Respondientes sinteticos por replica (por defecto 300).
#' @param n_rep Replicas Monte Carlo por celda de la prueba de estres (por
#'   defecto 60).
#' @param n_nucleos Nucleos a usar. Por defecto (\code{NULL}) todos menos uno.
#' @param seed Semilla global (streams por replica derivados de ella; el resultado
#'   es identico con 1 o N nucleos).
#' @param solo_pronostico Si \code{TRUE}, corre solo hasta el paso 3 (pronostico)
#'   y omite la prueba de estres, que es la parte mas lenta.
#' @param dl_juez,dl_adyacencia Canales adicionales de dependencia local de la
#'   prueba de estres (v2.9.5): juez LLM de parafrasis y adyacencia posicional.
#'   Se pasan tal cual a \code{\link{estres_escala}}; con \code{"escenario"}
#'   (default del juez) la linea base se recalcula CON los pares del canal y se
#'   reporta como escenario del EJE 1 (estructura de partida), sin darlos por
#'   hecho. \code{"off"} los apaga; \code{"on"} los inyecta al pronostico base.
#' @param pares_juez,modelo_jueces,g_juez,g_ady Ajustes finos de esos canales
#'   (pares ya juzgados, modelo del juez y fuerzas de inyeccion); ver
#'   \code{\link{estres_escala}}.
#' @param optimizar_estres Si \code{TRUE}, permite que la prueba de estres
#'   reescriba iterativamente los items mas fragiles (consume API). Por defecto
#'   \code{FALSE}: \code{analizar_tolerancia()} es un analisis DIAGNOSTICO.
#' @param verbose Si \code{TRUE} (por defecto) narra el proceso paso a paso y
#'   explica como leer el resultado.
#' @return Objeto \code{semilla_tolerancia} (lista) con: \code{escala} (metadatos),
#'   \code{deseabilidad}, \code{sd_entre}, \code{similitud}, \code{pares_ld},
#'   \code{pronostico} (salida de \code{\link{simular_estructura}}), \code{estres}
#'   (salida de \code{\link{estres_escala}}, o \code{NULL}) y \code{seed}. Tiene
#'   metodo \code{print()} con un resumen legible y accionable.
#' @seealso \code{\link{calificar_deseabilidad}}, \code{\link{obtener_embeddings}},
#'   \code{\link{simular_estructura}}, \code{\link{estres_escala}}
#' @examples
#' \dontrun{
#' items <- data.frame(
#'   item = c("Me siento capaz de resolver problemas dificiles",
#'            "Confio en mis habilidades ante retos nuevos",
#'            "Me pongo nervioso con facilidad",
#'            "Me preocupo por cosas sin importancia"),
#'   dimension = c("Autoeficacia", "Autoeficacia", "Ansiedad", "Ansiedad"))
#' res <- analizar_tolerancia(items, concepto = "Autoeficacia y ansiedad",
#'                            k_cat = 5)   # narra el proceso y devuelve el resumen
#' }
#' @export
analizar_tolerancia <- function(x, concepto = NULL, poblacion = NULL,
                                deseabilidad = NULL, similitud = NULL,
                                api_key = Sys.getenv("OPENAI_API_KEY"),
                                modelo = "gpt-4.1-mini",
                                modelo_embedding = "text-embedding-3-small",
                                k_cat = 5, n = 300, n_rep = 60,
                                n_nucleos = NULL, seed = 2026,
                                solo_pronostico = FALSE,
                                dl_juez = c("escenario", "off", "on"),
                                dl_adyacencia = c("off", "escenario", "on"),
                                pares_juez = NULL,
                                modelo_jueces = "gpt-4.1-mini",
                                g_juez = 0.35, g_ady = 0.25,
                                optimizar_estres = FALSE, verbose = TRUE) {
  dl_juez <- match.arg(dl_juez)
  dl_adyacencia <- match.arg(dl_adyacencia)

  or_def <- function(a, b) if (is.null(a)) b else a

  # ---- Normalizar la entrada a la lista 'x' que consume el motor -------------
  if (is.data.frame(x)) {
    if (!all(c("item", "dimension") %in% names(x)))
      stop("El data.frame debe tener columnas 'item' y 'dimension'.")
    items <- data.frame(item = as.character(x$item),
                        dimension = as.character(x$dimension),
                        stringsAsFactors = FALSE)
    xx <- list(items = items,
               concepto = or_def(concepto, "constructo psicologico"),
               metadata = list(poblacion = or_def(poblacion, "poblacion general adulta")))
  } else if (is.list(x) && !is.null(x$items)) {
    xx <- x
    if (!is.null(concepto)) xx$concepto <- concepto
    if (!is.null(poblacion)) { xx$metadata <- or_def(xx$metadata, list()); xx$metadata$poblacion <- poblacion }
    items <- data.frame(item = as.character(xx$items$item),
                        dimension = as.character(xx$items$dimension),
                        stringsAsFactors = FALSE)
  } else stop("x debe ser un data.frame con columnas 'item' y 'dimension', o un objeto con $items.")

  p <- nrow(items); dims <- unique(items$dimension); K <- length(dims)
  necesita_llm <- is.null(deseabilidad) || is.null(similitud)
  if (necesita_llm && (is.null(api_key) || !nzchar(api_key)))
    stop("Falta la clave del modelo de lenguaje (api_key). Pasa 'deseabilidad' y ",
         "'similitud' ya medidas, o define OPENAI_API_KEY.")

  nuc <- if (is.null(n_nucleos)) max(1L, parallel::detectCores() - 1L) else as.integer(n_nucleos)
  say <- function(...) if (isTRUE(verbose)) cat(...)
  hr  <- function(ch = "-") if (isTRUE(verbose)) cat(strrep(ch, 72), "\n")

  if (verbose) {
    cat("\n"); hr("=")
    cat("ANALISIS DE TOLERANCIA DE LA ESCALA  (prueba de estres pre-empirica)\n")
    hr("=")
    cat(sprintf("Escala: %d items en %d dimension%s | categorias de respuesta: %d\n",
                p, K, if (K == 1) "" else "es", k_cat))
    cat("Idea del metodo: sin recolectar datos, se lee el TEXTO de los items para\n")
    cat("estimar cuanto sesgo tolera la estructura prevista antes de romperse.\n")
    hr()
  }

  # ---- PASO 1: deseabilidad --------------------------------------------------
  if (is.null(deseabilidad)) {
    say("\n[PASO 1/4] Deseabilidad social de cada item\n")
    say("  El modelo ", modelo, " lee el texto de cada item y estima cuan deseable\n")
    say("  es socialmente responder que si, tal como lo haria un panel de jueces.\n")
    des <- calificar_deseabilidad(xx, api_key = api_key, modelo = modelo,
                                  poblacion = xx$metadata$poblacion,
                                  seed = seed, verbose = FALSE)
    deseabilidad <- if (is.list(des) && !is.null(des$deseabilidad)) des$deseabilidad else des
  } else {
    say("\n[PASO 1/4] Deseabilidad: se usa el vector ya proporcionado (sin costo LLM).\n")
  }
  medias <- tapply(deseabilidad, items$dimension, mean)
  sd_entre <- if (K > 1) stats::sd(medias) else NA_real_
  say(sprintf("  -> deseabilidad media = %.2f  (rango %.2f a %.2f)\n",
              mean(deseabilidad), min(deseabilidad), max(deseabilidad)))
  if (K > 1) {
    aviso_halo <- !is.na(sd_entre) && sd_entre < 0.05
    say(sprintf("  -> SD de deseabilidad entre dimensiones = %.3f%s\n", sd_entre,
                if (aviso_halo)
                  "  <== AVISO: < 0.05 => riesgo de HALO: las dimensiones son igual de\n     deseables y tienden a fundirse bajo un factor comun de 'responder bien'." else ""))
  }

  # ---- PASO 2: similitud (embeddings) ----------------------------------------
  if (is.null(similitud)) {
    say("\n[PASO 2/4] Similitud semantica entre items (embeddings)\n")
    say("  Se buscan pares casi PARAFRASEADOS (redaccion muy parecida): comparten\n")
    say("  varianza por su texto y, en un AFC real, degradan el ajuste.\n")
    emb <- obtener_embeddings(items, api_key = api_key,
                              modelo_embedding = modelo_embedding, verbose = FALSE)
    E <- if (is.list(emb) && !is.null(emb$embeddings)) emb$embeddings else NULL
    if (!is.null(E)) { En <- E / sqrt(rowSums(E^2)); similitud <- En %*% t(En) }
    else similitud <- as.matrix(emb$similitud)
  } else {
    say("\n[PASO 2/4] Similitud: se usa la matriz ya proporcionada (sin costo LLM).\n")
  }
  ut <- upper.tri(similitud)
  n_par <- sum(similitud[ut] > 0.80)
  say(sprintf("  -> %d par%s con similitud > 0.80 (disparan dependencia local)\n",
              n_par, if (n_par == 1) "" else "es"))

  # ---- PASO 3: pronostico ----------------------------------------------------
  say("\n[PASO 3/4] Pronostico: sobrevivira la estructura prevista?\n")
  say("  Se generan respuestas sinteticas bajo 3 escenarios de deseabilidad\n")
  say("  (debil/media/fuerte) y se ajusta el AFC en cada replica.\n")
  pron <- simular_estructura(xx, deseabilidad = deseabilidad, similitud = similitud,
                             carga_propia = 0.60, k_cat = k_cat, n = n,
                             n_nucleos = nuc, seed = seed, verbose = FALSE)
  say(sprintf("  -> probabilidad de estructura limpia (escenario central) = %.2f\n",
              pron$prob_limpia))
  say(sprintf("  -> veredicto del pronostico: %s\n", pron$veredicto))
  if (!is.null(pron$mapa_fusion) && isTRUE(pron$mapa_fusion$hay_fusion))
    say("  -> AVISO: el mapa de fusion senala dimensiones que tienden a fundirse.\n")

  # ---- PASO 4: prueba de estres ---------------------------------------------
  estres <- NULL
  if (!solo_pronostico) {
    say("\n[PASO 4/4] Prueba de estres: cuanto sesgo tolera y por donde se rompe?\n")
    say("  Se aplican dosis crecientes de 6 sesgos (aquiescencia, deseabilidad,\n")
    say("  estilo extremo, punto medio, descuido, lineas rectas) hasta el quiebre.\n")
    say("  (esta es la parte mas lenta; usa solo_pronostico = TRUE para saltarla)\n")
    estres <- estres_escala(xx, deseabilidad = deseabilidad, similitud = similitud,
                            carga_propia = 0.60, k_cat = k_cat, n = n, n_rep = n_rep,
                            dl_juez = dl_juez, dl_adyacencia = dl_adyacencia,
                            pares_juez = pares_juez, modelo_jueces = modelo_jueces,
                            g_juez = g_juez, g_ady = g_ady,
                            optimizar_estres = optimizar_estres,
                            api_key = api_key,
                            n_nucleos = nuc, seed = seed, verbose = FALSE)
    if (!is.null(estres$origen))
      say(sprintf("  -> estructura de partida (eje 1): %s\n",
                  sub(" .*", "", sub(":.*", "", estres$origen$etiqueta))))
    say(sprintf("  -> veredicto de resistencia (eje 2): %s\n", estres$veredicto))
    say(sprintf("  -> indice de fragilidad global = %.2f\n", estres$indice_global))
  } else {
    say("\n[PASO 4/4] Prueba de estres OMITIDA (solo_pronostico = TRUE).\n")
  }

  res <- list(escala = list(p = p, K = K, k_cat = k_cat, dimensiones = dims),
              deseabilidad = deseabilidad, sd_entre = sd_entre,
              similitud = similitud, pares_ld = n_par,
              pronostico = pron, estres = estres, seed = seed)
  class(res) <- "semilla_tolerancia"
  if (verbose) { cat("\n"); print(res) }
  invisible(res)
}

#' Imprime el resumen de un analisis de tolerancia
#'
#' @param x Objeto \code{semilla_tolerancia} de \code{\link{analizar_tolerancia}}.
#' @param ... Ignorado.
#' @return Devuelve \code{x} de forma invisible.
#' @export
print.semilla_tolerancia <- function(x, ...) {
  hr <- function(ch = "=") cat(strrep(ch, 72), "\n")
  hr()
  cat("RESUMEN DEL ANALISIS DE TOLERANCIA\n")
  hr()
  cat(sprintf("Escala: %d items, %d dimension(es), %d categorias de respuesta.\n",
              x$escala$p, x$escala$K, x$escala$k_cat))
  pr <- x$pronostico$prob_limpia
  cat(sprintf("Pronostico (prob. de estructura limpia): %.2f  ->  %s\n",
              pr, sub(" .*", "", x$pronostico$veredicto)))
  cat("Como leerlo:\n")
  if (pr >= 0.80) {
    cat("  OPTIMISTA. El fraseo NO anade un riesgo estructural detectable. Aun asi,\n")
    cat("  un pronostico optimista no es un aval de validez: el estudio de campo\n")
    cat("  sigue siendo necesario para confirmar la estructura.\n")
  } else if (pr < 0.40) {
    cat("  PESIMISTA. El fraseo induce un riesgo estructural real. Conviene reescribir\n")
    cat("  los items fragiles y reconsiderar la estructura ANTES de gastar una muestra.\n")
  } else {
    cat("  INTERMEDIO. Riesgo moderado; leer el rango entre escenarios y revisar los\n")
    cat("  items fragiles antes del campo.\n")
  }
  if (!is.null(x$sd_entre) && !is.na(x$sd_entre) && x$sd_entre < 0.05)
    cat("  NOTA: riesgo de HALO (dimensiones igual de deseables que tienden a fundirse).\n")

  if (!is.null(x$estres)) {
    if (!is.null(x$estres$origen)) {
      cat(sprintf("\nEstructura de partida (eje 1): %s\n", x$estres$origen$etiqueta))
      esc_or <- x$estres$origen$escenarios
      if (!is.null(esc_or) && nrow(esc_or)) {
        cat("Escenarios de origen (linea base con los pares de cada canal):\n")
        for (i in seq_len(nrow(esc_or)))
          cat(sprintf("  - canal %-11s (%d par%s): prob. limpia %.0f%%  [%s]\n",
                      esc_or$canal[i], esc_or$n_pares[i],
                      if (esc_or$n_pares[i] == 1) "" else "es",
                      100 * esc_or$prob_limpia[i], esc_or$pares[i]))
      }
    }
    cat(sprintf("\nResistencia (eje 2): %s  (indice de fragilidad global = %.2f).\n",
                sub(" .*", "", x$estres$veredicto), x$estres$indice_global))
    q <- x$estres$quiebres
    if (!is.null(q) && nrow(q)) {
      cat("Punto de quiebre por sesgo (dosis a la que la estructura cae):\n")
      for (i in seq_len(nrow(q)))
        cat(sprintf("  - %-15s %s\n", q$sesgo[i],
                    if (is.na(q$dosis_quiebre[i])) "no quiebra dentro de la grilla"
                    else sprintf("%.2f", q$dosis_quiebre[i])))
    }
    fi <- x$estres$fragilidad_items
    if (!is.null(fi) && nrow(fi)) {
      fi <- fi[order(-fi$caida_max), ]
      top <- utils::head(fi, 3)
      cat("Items que caen primero (candidatos a reescritura antes del campo):\n")
      for (i in seq_len(nrow(top)))
        cat(sprintf("  - %s  [%s]  por %s\n",
                    or_na(top$codigo[i], "?"), or_na(top$dimension[i], "?"),
                    or_na(top$sesgo_critico[i], "?")))
    }
  } else {
    cat("\n(Prueba de estres no ejecutada: solo_pronostico = TRUE.)\n")
  }
  hr()
  cat("Recuerda: el pronostico es una clase cualitativa + rango, no una probabilidad\n")
  cat("puntual, y solo cubre el riesgo INDUCIDO POR EL FRASEO.\n")
  invisible(x)
}

or_na <- function(v, alt) if (is.null(v) || length(v) == 0 || is.na(v)) alt else v
