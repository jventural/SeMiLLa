# =============================================================================
# AUDITORIA DE ASIGNACION ITEM -> DIMENSION  (2.9.36)
# =============================================================================
# POR QUE EXISTE
#   Medido el 22-ago-2026 con un experimento de contaminacion controlada sobre
#   datos del Item Response Warehouse (3 estudios, las mismas personas en varias
#   subescalas, defecto INTRODUCIDO -> verdad por construccion).
#
#   Ninguno de los tres ejes de la compuerta ve que un item este en la dimension
#   equivocada. El eje 3 no puede: simular_estructura() arma el modelo generador
#   DESDE las etiquetas (F_k =~ items de esa dimension) y con carga uniforme
#   .695, asi que el item mal puesto se simula cargando .695 justo donde se le
#   puso, y el CFA sobre esos datos ajusta perfecto. Con la deseabilidad fijada
#   constante, mover 4 items de D2 a D1 dejaba prob_limpia en 1.000, igual que
#   la escala limpia; peor aun, en las 36 comparaciones con k = 1, 2 y 3 la
#   escala contaminada salia MEJOR o igual que el control del mismo tamano
#   (AUC .461, por debajo del azar), porque el simulador solo ve una dimension
#   con mas indicadores.
#
#   Lo unico que reaccionaba era la deseabilidad juzgada por el LLM, y no por la
#   razon correcta: detectaba que el intruso tuviera OTRA deseabilidad, no que
#   midiera otro constructo. Solo funciono donde la brecha era enorme
#   (flexibilidad psicologica .87 frente a estigma del suicidio .15); entre dos
#   dimensiones de un mismo instrumento la brecha es de .07-.08 y no veia nada.
#
#   Mientras tanto el defecto ERA real: con las respuestas reales el CFI caia de
#   forma monotona (.899 -> .790, .790 -> .613, .927 -> .797) y en un caso el phi
#   empirico pasaba de .226 a .857.
#
# QUE HACE
#   Compara, item por item, si se parece mas a los items de SU dimension o a los
#   de otra. No simula ni llama al LLM: usa la matriz de similitud que la
#   compuerta ya tiene. Por eso es determinista entre corridas.
#
# RENDIMIENTO MEDIDO (355 condiciones de 13 estudios IRW, ground truth exacto)
#   Con el umbral por defecto (-0.05), sobre el defecto realista (un item de
#   otra dimension DECLARADA):
#     sensibilidad  .902  (322 de 357 items intrusos)
#     alertan       .993  (146 de 147 condiciones contaminadas)
#     especificidad .844  (135 de 160 condiciones sin defecto quedan limpias)
#
# DONDE APARECEN LOS AVISOS EN ESCALAS SIN DEFECTO, y por que NO son ruido
#   Los 158 senalamientos legitimos se concentran en 3 estudios de 13; los
#   otros 10 dan CERO. Son tres situaciones distintas, y en las tres el aviso
#   dice algo verdadero:
#     (a) DIMENSION HETEROGENEA. 'sexual' pone 100 items de autoconcepto sexual
#         en una sola dimension: tiene tantas facetas dentro que muchos de sus
#         items se parecen mas a la otra dimension que al promedio de la suya.
#         103 de los 158 avisos salen de ahi.
#     (b) DOS INSTRUMENTOS DEL MISMO CONSTRUCTO. 'gratitud' enfrenta GART con
#         GQ-6: los dos miden gratitud, asi que la frontera no existe. 46 avisos.
#     (c) CONSTRUCTOS SOLAPADOS. Indefension aprendida frente a Autoestima. 9.
#   Subir el umbral no los quita sin arrasar la sensibilidad (-0.09 -> .737 con
#   52 avisos), porque no son ruido de calibracion: son la estructura real.
#   Leerlos como "esta dimension no esta bien delimitada" es mas util que
#   silenciarlos.
#
# FUERA DEL INGLES: funciona. 'lys_2020_rape_3' esta EN POLACO (sexismo
#   ambivalente frente a mitos sobre la violacion) y da sensibilidad .800 con
#   CERO falsos positivos. Dato relevante porque SeMiLLa trabaja en espanol.
#   Con umbral 0 (senalar cualquier margen negativo): sensibilidad 0.991 pero
#   261 falsos positivos. La primera medicion, sobre solo 3 estudios, daba
#   especificidad 1.000 con umbral 0; al ampliar el banco aparecieron dos pares
#   de constructos SOLAPADOS -Autism Quotient con Alexitimia, e Indefension
#   aprendida con Autoestima- donde muchos items legitimos estan de verdad mas
#   cerca del centroide vecino. No era un fallo del algoritmo: el banco pequeno
#   no contenia pares proximos.
#
# POR QUE EL UMBRAL EN -0.05
#   Los margenes separan bien: los intrusos reales tienen mediana -0.157
#   (Q25 -0.245, Q75 -0.100) y los falsos positivos -0.020 (Q25 -0.029,
#   Q75 -0.012), ocho veces menor. Barrido medido:
#      0.00 -> sensibilidad .991, 261 falsos positivos
#     -0.03 -> sensibilidad .939,  61
#     -0.05 -> sensibilidad .921,   9   <- elegido
#     -0.08 -> sensibilidad .838,   0
#   El umbral esta en escala de coseno y calibrado con text-embedding-3-small:
#   si se cambia de modelo de embeddings hay que recalibrarlo. Por eso es un
#   parametro y no una constante.
#   Los items con margen negativo pero por encima del umbral NO se descartan:
#   se devuelven como 'frontera' para que se puedan mirar.
#
# LIMITE MEDIDO, y hay que decirlo
#   Detecta "este item pertenece a OTRA de las dimensiones declaradas". NO
#   detecta bien un item ajeno al instrumento entero (sensibilidad .28 sobre el
#   banco de 8 estudios; .17-.33 en el banco inicial de 3): si
#   viene de un tercer constructo no se parece a NINGUNA dimension, asi que su
#   similitud propia baja pero la ajena tambien, y el margen sigue positivo. Se
#   probo una segunda señal por outlier bajo (IQR) para cubrirlo: subia esa
#   sensibilidad a .39-.50 pero metia 28 falsos positivos donde antes habia
#   cero. Con el historial de falsos "no aplicar" de la compuerta, ese cambio
#   empeora mas de lo que arregla. Se descarto a proposito.
# =============================================================================

#' @title Auditar la asignacion de cada item a su dimension
#'
#' @description
#' Comprueba, para cada item, si se parece mas a los items de su propia
#' dimension o a los de otra. Un item con margen negativo esta, semanticamente,
#' en la dimension equivocada.
#'
#' A diferencia de los tres ejes de \code{\link{compuerta_pre_aplicacion}}, este
#' control NO simula nada y NO consulta al LLM: lee la matriz de similitud.
#'
#' @param x Objeto \code{semilla} (o lista) con \code{$items$dimension} y
#'   \code{$similitud}. Tambien acepta una matriz de similitud directa, en cuyo
#'   caso hay que pasar \code{dimension}.
#' @param dimension Vector de dimensiones, si \code{x} es una matriz.
#' @param umbral_margen Margen por debajo del cual un item se declara mal
#'   asignado (negativo; por defecto \code{-0.05}). Los items con margen
#'   negativo pero por encima del umbral se devuelven como \code{frontera}.
#'   Calibrado con \code{text-embedding-3-small}: ver el encabezado del archivo.
#' @param verbose Si \code{TRUE}, imprime el resumen.
#'
#' @return Lista de clase \code{semilla_asignacion} con:
#'   \item{items}{data.frame por item: sim_propia, sim_ajena, dim_mas_cercana,
#'     margen, mal_asignado}
#'   \item{mal_asignados}{data.frame solo con los items senalados}
#'   \item{n_mal_asignados}{entero}
#'   \item{frontera}{items con margen negativo pero por encima del umbral}
#'   \item{n_frontera}{entero}
#'   \item{umbral_margen}{el umbral usado}
#'   \item{alerta}{"ok" o "riesgo"}
#'
#' @seealso \code{\link{compuerta_pre_aplicacion}}
#' @export
auditar_asignacion <- function(x, dimension = NULL, umbral_margen = -0.05,
                               verbose = TRUE) {
  if (!is.numeric(umbral_margen) || length(umbral_margen) != 1 || umbral_margen > 0)
    stop("umbral_margen debe ser un numero <= 0.")
  if (is.matrix(x) || inherits(x, "dist")) {
    S <- as.matrix(x); dims <- dimension
    textos <- rep(NA_character_, length(dims))
  } else {
    if (is.null(x$similitud))
      stop("El objeto no tiene $similitud. Ejecuta antes obtener_embeddings().")
    if (is.null(x$items) || is.null(x$items$dimension))
      stop("El objeto no tiene $items$dimension.")
    S <- as.matrix(x$similitud)
    dims <- as.character(x$items$dimension)
    textos <- as.character(x$items$item)
  }
  if (is.null(dims)) stop("Falta el vector de dimensiones.")
  p <- length(dims)
  if (nrow(S) != p)
    stop(sprintf("La similitud es %dx%d pero hay %d items.", nrow(S), ncol(S), p))

  ds <- unique(dims)
  if (length(ds) < 2) {
    if (verbose) cat("Solo hay una dimension: no hay asignacion que auditar.\n")
    return(invisible(structure(
      list(items = NULL, mal_asignados = NULL, n_mal_asignados = 0L,
           alerta = "ok"), class = "semilla_asignacion")))
  }

  sim_propia <- sim_ajena <- numeric(p)
  dim_cerca <- character(p)
  for (i in seq_len(p)) {
    m <- vapply(ds, function(d) {
      j <- which(dims == d); j <- j[j != i]
      if (!length(j)) NA_real_ else mean(S[i, j], na.rm = TRUE)
    }, numeric(1))
    sim_propia[i] <- m[[dims[i]]]
    otras <- m[names(m) != dims[i]]
    otras <- otras[is.finite(otras)]
    if (!length(otras)) {
      sim_ajena[i] <- NA_real_; dim_cerca[i] <- NA_character_
    } else {
      sim_ajena[i] <- max(otras)
      dim_cerca[i] <- names(otras)[which.max(otras)]
    }
  }
  margen <- sim_propia - sim_ajena

  it <- data.frame(
    item = textos, dimension = dims,
    sim_propia = round(sim_propia, 4), sim_ajena = round(sim_ajena, 4),
    dim_mas_cercana = dim_cerca, margen = round(margen, 4),
    mal_asignado = !is.na(margen) & margen < umbral_margen,
    frontera = !is.na(margen) & margen < 0 & margen >= umbral_margen,
    stringsAsFactors = FALSE)
  mal <- it[it$mal_asignado, , drop = FALSE]
  mal <- mal[order(mal$margen), , drop = FALSE]
  fro <- it[it$frontera, , drop = FALSE]
  fro <- fro[order(fro$margen), , drop = FALSE]

  out <- structure(
    list(items = it, mal_asignados = mal, n_mal_asignados = nrow(mal),
         frontera = fro, n_frontera = nrow(fro), umbral_margen = umbral_margen,
         alerta = if (nrow(mal) > 0) "riesgo" else "ok"),
    class = "semilla_asignacion")
  if (verbose) print(out)
  invisible(out)
}

#' @title Imprimir la auditoria de asignacion
#' @param x Objeto \code{semilla_asignacion}.
#' @param ... Sin uso.
#' @return El objeto, invisible.
#' @export
print.semilla_asignacion <- function(x, ...) {
  cat("\n============================================================\n")
  cat(" ASIGNACION DE LOS ITEMS A SUS DIMENSIONES\n")
  cat("============================================================\n")
  if (is.null(x$items)) {
    cat("  (una sola dimension: nada que auditar)\n")
    cat("============================================================\n\n")
    return(invisible(x))
  }
  cat(sprintf("  Items: %d  |  en la dimension equivocada: %d  |  frontera: %d\n",
              nrow(x$items), x$n_mal_asignados,
              if (is.null(x$n_frontera)) 0L else x$n_frontera))
  cat(sprintf("  (umbral de margen: %+.3f)\n",
              if (is.null(x$umbral_margen)) -0.05 else x$umbral_margen))
  if (x$n_mal_asignados == 0) {
    cat("\n  Cada item se parece mas a los de su propia dimension que a los de\n")
    cat("  cualquier otra. No hay nada que reasignar.\n")
  } else {
    cat("\n  Estos items se parecen MAS a otra dimension que a la suya:\n\n")
    for (i in seq_len(nrow(x$mal_asignados))) {
      r <- x$mal_asignados[i, ]
      cat(sprintf("   - [%s  ->  %s]   margen %+.3f\n",
                  r$dimension, r$dim_mas_cercana, r$margen))
      if (!is.na(r$item))
        cat(sprintf("     %s\n", substr(r$item, 1, 88)))
    }
    cat("\n  Que hacer: reasignarlos a la dimension que se indica, reescribirlos\n")
    cat("  para que expresen el contenido de la suya, o retirarlos.\n")
  }
  if (!is.null(x$frontera) && nrow(x$frontera) > 0) {
    cat("\n  En la FRONTERA (margen negativo pero pequeno; mirar, no\n")
    cat("  corregir en automatico):\n")
    for (i in seq_len(min(4L, nrow(x$frontera)))) {
      r <- x$frontera[i, ]
      cat(sprintf("   - [%s ~ %s] margen %+.3f", r$dimension,
                  r$dim_mas_cercana, r$margen))
      if (!is.na(r$item)) cat(sprintf(" | %s", substr(r$item, 1, 50)))
      cat("\n")
    }
    if (nrow(x$frontera) > 4)
      cat("     ... y ", nrow(x$frontera) - 4, " mas\n", sep = "")
  }
  cat("------------------------------------------------------------\n")
  cat("  Medido sobre 355 condiciones de 13 estudios con defectos\n")
  cat("  introducidos a proposito: sensibilidad .902, especificidad\n")
  cat("  .844. NO detecta bien un item ajeno al instrumento entero\n")
  cat("  (sensibilidad .28).\n")
  cat("  Si una dimension es muy heterogenea por dentro, o si las dos\n")
  cat("  miden casi lo mismo, habra mas avisos: ahi el mensaje es que\n")
  cat("  la dimension no esta bien delimitada, no que sobren items.\n")
  cat("============================================================\n\n")
  invisible(x)
}
