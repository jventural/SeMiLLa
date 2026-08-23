# ---------------------------------------------------------------------------
#  INFORME DE LA COMPUERTA EN TEXTO PLANO
#
#  La pantalla del Paso 5b es la lectura del momento: se sobrescribe en cuanto
#  se vuelve a correr. Quien esta ajustando una escala hace varios intentos y
#  necesita compararlos, y para eso hace falta que cada corrida quede en un
#  archivo aparte. De ahi el nombre con fecha y hora: nunca pisa al anterior.
#
#  Se vuelca TODO lo que hay en la seccion, no un resumen: los cuatro controles
#  con sus numeros, la estructura completa, la prueba de resistencia y los
#  items con su deseabilidad. Al final va una linea con separadores para pegar
#  en una hoja de calculo y ver la evolucion entre intentos.
# ---------------------------------------------------------------------------

`%|N|%` <- function(a, b) if (is.null(a) || length(a) == 0 ||
                             (length(a) == 1 && is.na(a))) b else a

# Varios campos llegan como lista (parametros anidados, resultados por
# escenario). Se aplanan antes de convertir: as.numeric() sobre una lista aborta
# la generacion entera del informe.
.inf_num <- function(x, d = 2) {
  x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(x) == 0 || all(is.na(x))) "-" else
    paste(formatC(x[seq_len(min(6L, length(x)))], format = "f", digits = d),
          collapse = " · ")
}
.inf_pct <- function(x, d = 1) {
  x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(x) == 0 || is.na(x[1])) "-" else sprintf(paste0("%.", d, "f%%"), 100 * x[1])
}
.inf_sino <- function(x) if (isTRUE(x)) "SI" else if (isFALSE(x)) "no" else "-"
.inf_regla <- function(t = "", ch = "-") {
  if (!nzchar(t)) strrep(ch, 74) else
    paste0(strrep(ch, 74), "\n ", t, "\n", strrep(ch, 74))
}
.inf_campo <- function(et, v, ancho = 22) {
  paste0("  ", formatC(et, width = -ancho), ": ", paste(v, collapse = ""))
}
# Envuelve texto largo respetando la sangria, para que el .txt se lea en
# cualquier editor sin scroll horizontal.
.inf_parrafo <- function(txt, sangria = "  ") {
  txt <- gsub("\\s+", " ", trimws(paste(txt, collapse = " ")))
  if (!nzchar(txt)) return(character(0))
  paste0(sangria, strwrap(txt, width = 72))
}
# Tabla de ancho fijo a partir de un data.frame.
.inf_tabla <- function(df, cols = names(df), max_ancho = 46) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return("  (ninguno)")
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return("  (sin columnas reconocibles)")
  d <- df[, cols, drop = FALSE]
  d[] <- lapply(d, function(col) {
    if (is.list(col)) col <- vapply(col, function(z)
      paste(format(unlist(z)), collapse = ","), character(1))
    if (is.numeric(col)) {
      # Los conteos y numeros de item salen como "1.000" si se formatea todo
      # igual; solo llevan decimales las columnas que de verdad los tienen.
      if (all(is.na(col) | col == round(col))) formatC(col, format = "d")
      else formatC(col, format = "f", digits = 3)
    } else substr(as.character(col), 1, max_ancho)
  })
  anchos <- vapply(seq_along(cols), function(j)
    max(nchar(c(cols[j], d[[j]])), na.rm = TRUE), integer(1))
  linea <- function(vals) paste0("  ", paste(mapply(function(v, a)
    formatC(v, width = -a), vals, anchos), collapse = "  "))
  c(linea(cols), paste0("  ", strrep("-", sum(anchos) + 2 * (length(cols) - 1))),
    vapply(seq_len(nrow(d)), function(i) linea(unlist(d[i, ])), character(1)))
}

#  escala : objeto semilla (items, compuerta, metadata)
#  local  : el .rds del analisis local, o NULL si solo se corrio en el navegador
#  origen : "navegador" | "computadora"
.cmp_informe_txt <- function(escala, local = NULL, origen = NULL) {
  g  <- escala$compuerta
  it <- escala$items
  d  <- g$deseabilidad
  e3 <- g$estructura
  L  <- character(0)
  add <- function(...) L <<- c(L, ...)

  # ---- Cabecera -----------------------------------------------------------
  add(strrep("=", 74),
      " SeMiLLa · COMPUERTA PRE-APLICACION",
      strrep("=", 74))
  add(.inf_campo("Descargado", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  add(.inf_campo("Escala", escala$metadata$concepto_original %|N|% "(sin nombre)"))
  add(.inf_campo("Poblacion", escala$metadata$poblacion %|N|% "-"))
  if (!is.null(it))
    add(.inf_campo("Items", sprintf("%d en %d dimensiones", nrow(it),
                                    length(unique(it$dimension)))))
  # De donde sale CADA parte. Importa porque los dos caminos conviven: se puede
  # hacer una lectura rapida aqui y despues subir el .rds del analisis completo.
  # Este informe recoge siempre lo ULTIMO, y lo dice pieza por pieza en vez de
  # etiquetar la corrida entera con un solo origen, que seria falso en el caso
  # mixto (compuerta del navegador + estructura y resistencia del .rds).
  hay_local <- !is.null(local)
  add(.inf_campo("Controles 1 y 2", paste0(
    switch(origen %|N|% "navegador",
           "computadora" = "analisis completo, corrido en tu computadora",
           "lectura rapida, corrida en el navegador"))))
  add(.inf_campo("Control 3 y resistencia", if (hay_local)
    "analisis completo, corrido en tu computadora" else
    "lectura rapida, corrida en el navegador (sin prueba de resistencia)"))
  if (hay_local) {
    add(.inf_campo("Generado el", local$generado %|N|% "-"))
    add(.inf_campo("Nucleos usados", local$config$nucleos %|N|% "-"))
    add(.inf_campo("Identificador", substr(local$id_escala %|N|% "-", 1, 12)))
    add(.inf_parrafo(paste("Este informe recoge el analisis COMPLETO que subiste.",
      "Si antes hiciste una lectura rapida en el navegador, queda sustituida:",
      "manda siempre lo ultimo que se corrio."), "  "))
  } else {
    add(.inf_parrafo(paste("Solo lectura rapida. Para publicar, corre el",
      "analisis completo en tu computadora y sube el .rds: entonces esta misma",
      "descarga incluira la estructura con mas replicas y la prueba de",
      "resistencia."), "  "))
  }
  p <- g$parametros
  if (!is.null(p))
    add(.inf_campo("Parametros", sprintf(
      "%s respondientes · %s replicas · umbral de parecido %s · %s",
      p$n %|N|% "?", p$n_rep %|N|% "?", p$umbral_sem %|N|% "?", p$fecha %|N|% "?")))

  # ---- Titular ------------------------------------------------------------
  add("", .inf_regla("TITULAR", "="))
  add(.inf_campo("Escenario", g$escenario %|N|% "-"))
  add(.inf_parrafo(g$escenario_detalle %|N|% "", "      "))
  add(.inf_campo("Redaccion", g$calidad_redaccion %|N|% "-"))
  add(.inf_campo("Veredicto (clasico)", g$veredicto %|N|% "-"))
  if (!is.null(g$escenario_fusion))
    add(.inf_campo("Fusion prevista", paste(g$escenario_fusion, collapse = " | ")))
  if (length(g$avisos_diseno %|N|% character(0)) > 0) {
    add("", "  Avisos de diseno:")
    for (a in g$avisos_diseno) add(.inf_parrafo(paste("-", a), "    "))
  }

  # ---- Control 0: asignacion (2.9.36) ------------------------------------
  # Va PRIMERO porque su defecto invalida lo que miden los demas: si hay items
  # en la dimension equivocada, la estructura que el control 3 simula no es la
  # que se diseno.
  asg <- g$asignacion
  add("", .inf_regla("CONTROL 1 . SI CADA ITEM ESTA EN SU DIMENSION"))
  if (is.null(asg) || is.null(asg$items)) {
    add(.inf_parrafo("No evaluado en esta corrida.", "  "))
  } else if (asg$n_mal_asignados == 0) {
    add(.inf_parrafo(paste0("Cada item se parece mas a los de su propia ",
      "dimension que a los de cualquier otra. No hay nada que reasignar."), "  "))
  } else {
    add(.inf_parrafo(sprintf(paste0("%d item(s) estan en la dimension ",
      "equivocada: por su contenido se parecen mas a otra dimension que a la ",
      "suya. Corregir esto ANTES que lo demas."), asg$n_mal_asignados), "  "))
    add("")
    for (i in seq_len(nrow(asg$mal_asignados))) {
      r <- asg$mal_asignados[i, ]
      add(sprintf("   %-22s -> %-22s  margen %+.3f", substr(r$dimension, 1, 22),
                  substr(r$dim_mas_cercana, 1, 22), r$margen))
      if (!is.na(r$item)) add(sprintf("     %s", substr(r$item, 1, 90)))
    }
  }
  if (!is.null(asg) && !is.null(asg$frontera) && nrow(asg$frontera) > 0) {
    add("")
    add(.inf_campo("En la frontera", sprintf("%d item(s) con margen negativo pero pequeno (mirar, no corregir en automatico)", nrow(asg$frontera))))
  }
  add("")
  add(.inf_parrafo(paste0("Medido sobre 355 situaciones de 13 estudios con el ",
    "defecto introducido a proposito: sensibilidad .902, especificidad .844. ",
    "No detecta bien un item ajeno a todo el instrumento (.28)."), "  "))

  # ---- Control 1: redaccion ----------------------------------------------
  add("", .inf_regla("CONTROL 2 · COMO ESTAN ESCRITOS LOS ITEMS"))
  rd <- g$redaccion
  # Casi todos estos campos son LISTAS con varios componentes (global/media/
  # por_item), no escalares: hay que nombrar el componente o el informe escupe
  # el vector entero de 15 numeros y se vuelve ilegible.
  add(.inf_parrafo(rd$alerta %|N|% "", "  "))
  add("")
  add(.inf_campo("Similitud maxima", paste0(
    .inf_num(rd$similitud_maxima$global, 3), "   (media entre pares: ",
    .inf_num(rd$similitud_maxima$media, 3), ")")))
  add(.inf_campo("Umbral aplicado", paste0(
    .inf_num(rd$parametros$umbral_sem, 3),
    if (isTRUE(rd$parametros$umbral_sem_auto)) "  (automatico)" else "  (fijado a mano)")))
  add(.inf_campo("Umbral de faceta", .inf_num(rd$parametros$umbral_faceta, 3)))
  add(.inf_campo("Solape de n-gramas", sprintf(
    "medio %s · maximo %s   (n = %s)", .inf_num(rd$ngram_overlap$media, 3),
    .inf_num(rd$ngram_overlap$maxima, 3), .inf_num(rd$ngram_overlap$n_gram, 0))))
  add(.inf_campo("Homogeneidad sintact.", sprintf(
    "indice %s · prefijo compartido %s · alerta %s",
    .inf_num(rd$homogeneidad_sintactica$indice, 3),
    .inf_num(rd$homogeneidad_sintactica$prefijo_compartido, 3),
    .inf_sino(rd$homogeneidad_sintactica$alerta))))
  add(.inf_campo("Diversidad lexica", paste0(
    "TTR global ", .inf_num(rd$diversidad_lexica$ttr_global, 3))))
  add("", "  Pares de items parecidos (por encima del umbral):")
  add(.inf_tabla(rd$pares_redundantes,
                 c("item1", "item2", "similitud", "codigo1", "codigo2", "razon")))
  add("", "  Grupos con la misma plantilla:")
  add(.inf_tabla(rd$facetas_repetidas,
                 c("cluster", "nucleo_lexico", "codigos", "n_items",
                   "sim_media", "inter_dimension")))
  # Detalle por item: es lo que permite ver CUAL arrastra la similitud.
  if (is.data.frame(rd$resumen) && nrow(rd$resumen) > 0) {
    add("", "  Detalle por item (similitud maxima, diversidad, prefijo):")
    add(.inf_tabla(rd$resumen, c("num", "codigo", "sim_max", "ttr", "prefijo"),
                   max_ancho = 30))
  }

  # ---- Control 2: deseabilidad -------------------------------------------
  add("", .inf_regla("CONTROL 3 · QUE TAN COMPROMETEDOR ES RESPONDER"))
  if (is.null(d)) {
    add("  NO SE PUDO EVALUAR en esta corrida.",
        "  Los numeros de estructura se calcularon con deseabilidad imputada",
        "  a 0.5 (el escenario mas favorable): no valen como prueba.")
  } else {
    add(.inf_parrafo(d$mensaje %|N|% "", "  "))
    add(.inf_campo("Media", .inf_num(mean(as.numeric(d$deseabilidad), na.rm = TRUE))))
    add(.inf_campo("Diferencia ENTRE dim.", .inf_num(d$sd_entre_dim, 3)))
    add(.inf_campo("Diferencia DENTRO dim.", .inf_num(d$sd_intra_dim, 3)))
    add(.inf_campo("Estabilidad del juicio",
                   paste0(.inf_num(d$estabilidad, 3),
                          if (isTRUE(as.numeric(d$estabilidad) < 0.70))
                            "   <-- por debajo de 0.70: los valores por ITEM no son fiables (la media por dimension si)" else "")))
    add(.inf_campo("Uniforme entre dim.", .inf_sino(d$uniforme)))
    add(.inf_campo("Riesgo de halo", .inf_sino(d$riesgo_halo)))
    add(.inf_campo("Desbalance intra dim.", .inf_sino(d$alerta_intra)))
    add(.inf_campo("Items sin calificar", d$n_imputados %|N|% "-"))
    M <- d$pasadas
    if (!is.null(M) && !is.null(dim(M))) {
      add(.inf_campo("Pasadas del juez", sprintf(
        "%d (con %s%% de puntuaciones vacias)", ncol(M),
        formatC(100 * mean(is.na(M)), format = "f", digits = 0))))
    }
    if (!is.null(d$por_dimension)) {
      add("", "  Deseabilidad por dimension:")
      pd <- d$por_dimension
      if (is.data.frame(pd)) add(.inf_tabla(pd)) else
        for (n in names(pd)) add(.inf_campo(paste0("  ", n), .inf_num(pd[[n]])))
    }
  }

  # ---- Control 3: estructura ---------------------------------------------
  add("", .inf_regla("CONTROL 4 · COMO SE COMPORTARA LA ESTRUCTURA"))
  s <- local$sim %|N|% e3
  if (is.null(s)) add("  (sin resultados de simulacion)") else {
    add(.inf_campo("Veredicto", s$veredicto %|N|% "-"))
    add(.inf_campo("Estructura limpia", paste0(
      .inf_pct(s$prob_limpia),
      if (!is.null(s$prob_ic)) sprintf("   (IC %s a %s)",
        .inf_pct(s$prob_ic[1]), .inf_pct(s$prob_ic[2])) else "")))
    if (!is.null(s$prob_min))
      add(.inf_campo("Rango entre escenarios",
                     sprintf("%s a %s", .inf_pct(s$prob_min), .inf_pct(s$prob_max))))
    # Los umbrales se leen del OBJETO, no se escriben a mano: si alguien corre
    # simular_estructura() con otros valores, el informe los sigue.
    # (Los defaults cubren los resultados guardados antes de que
    # simular_estructura() empezara a devolverlos, que no los traen.)
    u_phi <- s$umbral_phi %|N|% 0.70
    u_fus <- s$umbral_fusion %|N|% 0.65
    # phi_med es el PROMEDIO de los pares, y al promedio se le aplica u_phi.
    # Aqui figuraba "(umbral de fusion 0.65)", que es el corte de CADA PAR: el
    # informe ponia al lado del promedio un umbral que no se le aplica.
    add(.inf_campo("Correlacion media (phi)", paste0(
      .inf_num(s$phi_med, 3),
      sprintf("   (limite de estructura limpia %s)", .inf_num(u_phi, 2)))))
    add(.inf_campo("RMSEA mediano", .inf_num(s$rmsea_med, 3)))
    add(.inf_campo("Fuerza del factor", .inf_num(s$fuerza_central, 3)))
    add(.inf_campo("Carga estandarizada", .inf_num(e3$carga_estandarizada_media, 3)))
    pp <- s$phi_pares
    if (!is.null(pp)) {
      m <- try(as.matrix(pp), silent = TRUE)
      if (!inherits(m, "try-error") && nrow(m) > 1) {
        add("", sprintf("  Pares de dimensiones (correlacion simulada; >= %s se funden):",
                        .inf_num(u_fus, 2)))
        nn <- rownames(m) %|N|% paste0("dim", seq_len(nrow(m)))
        for (i in 1:(nrow(m) - 1)) for (j in (i + 1):ncol(m))
          add(sprintf("    %-30s <-> %-30s  %s%s",
                      substr(nn[i], 1, 30), substr(nn[j], 1, 30),
                      .inf_num(m[i, j], 3),
                      if (isTRUE(m[i, j] >= u_fus)) "  <-- SE FUNDEN" else ""))
      }
    }
    b <- g$banda_estructura
    if (!is.null(b))
      add("", .inf_campo("Banda entre repeticiones", sprintf(
        "%s a %s (mediana %s, %s repeticiones)",
        .inf_pct(b$min), .inf_pct(b$max), .inf_pct(b$mediana),
        b$n_rep_banda %|N|% "?")))
    if (!is.null(g$estructura_alternativa))
      add("", .inf_parrafo(paste("Estructura alternativa probable:",
        paste(unlist(g$estructura_alternativa), collapse = " · ")), "  "))
  }

  # ---- Resistencia --------------------------------------------------------
  add("", .inf_regla("RESISTENCIA · QUE PASA SI LA GENTE RESPONDE CON SESGO"))
  es <- local$estres
  if (is.null(es)) add("  (no se corrio: requiere el analisis en tu computadora)") else {
    add(.inf_campo("Veredicto", es$veredicto %|N|% "-"))
    add(.inf_campo("Indice global", .inf_num(es$indice_global, 3)))
    add(.inf_campo("Umbral de quiebre", .inf_num(es$umbral_quiebre, 3)))
    add("", "  Por tipo de sesgo (dosis a la que se rompe; NA = no se rompio):")
    add(.inf_tabla(es$quiebres, c("sesgo", "unidad", "dosis_quiebre", "fragilidad")))
    fi <- es$fragilidad_items
    if (!is.null(fi) && nrow(fi) > 0) {
      o <- order(-suppressWarnings(as.numeric(fi$caida_max)))
      add("", "  Items mas fragiles (mayor caida de carga):")
      add(.inf_tabla(fi[o, , drop = FALSE][seq_len(min(8, nrow(fi))), , drop = FALSE],
                     c("codigo", "dimension", "carga_base", "caida_max", "sesgo_critico")))
    }
  }

  # ---- Items --------------------------------------------------------------
  add("", .inf_regla("TUS ITEMS"))
  if (is.null(it)) add("  (sin items)") else {
    tab <- data.frame(n = it$numero, dimension = it$dimension,
                      stringsAsFactors = FALSE)
    if (!is.null(d$deseabilidad) && length(d$deseabilidad) == nrow(it))
      tab$desea <- round(as.numeric(d$deseabilidad), 2)
    tab$item <- it$item
    add(.inf_tabla(tab, names(tab), max_ancho = 90))
  }

  # ---- Mapa de fusion y sensibilidad --------------------------------------
  add("", .inf_regla("MAPA DE FUSION Y SENSIBILIDAD"))
  mf <- g$mapa_fusion %|N|% e3$mapa_fusion
  if (is.null(mf)) add("  (sin mapa de fusion)") else if (is.data.frame(mf))
    add(.inf_tabla(mf)) else add(.inf_parrafo(paste(utils::capture.output(str(mf)), collapse = " "), "  "))
  if (!is.null(e3$sensibilidad)) {
    add("", "  Sensibilidad (como cambia la estructura al mover los supuestos):")
    add(if (is.data.frame(e3$sensibilidad)) .inf_tabla(e3$sensibilidad) else
        paste0("    ", utils::capture.output(str(e3$sensibilidad))))
  }
  if (!is.null(e3$sensibilidad_phi)) {
    add("", "  Sensibilidad de phi:")
    add(if (is.data.frame(e3$sensibilidad_phi)) .inf_tabla(e3$sensibilidad_phi) else
        paste0("    ", utils::capture.output(str(e3$sensibilidad_phi))))
  }
  if (!is.null(g$semaforo))
    add("", .inf_campo("Semaforo", paste(unlist(g$semaforo), collapse = " | ")))

  # ---- Como evoluciono la escala (pestana "Tus items") --------------------
  cv <- local$convergencia
  if (!is.null(cv)) {
    add("", .inf_regla("COMO EVOLUCIONO LA ESCALA (ciclo de mejora)"))
    add(.inf_campo("Minutos", .inf_num(cv$minutos, 1)))
    add(.inf_campo("Objetivo", cv$objetivo %|N|% "-"))
    add(.inf_campo("Objetivo alcanzado", .inf_sino(cv$objetivo_alcanzado)))
    h <- cv$historial
    if (!is.null(h) && nrow(h) > 0) {
      add(.inf_campo("Vueltas dadas", max(h$iteracion, na.rm = TRUE)))
      add("", "  Historial vuelta a vuelta:")
      add(.inf_tabla(h, c("iteracion", "n_marcados", "reemplazos", "score",
                          "escenario", "veredicto")))
    }
    ch <- cv$cambios
    add("", sprintf("  Items reescritos: %d", if (is.null(ch)) 0L else nrow(ch)))
    if (!is.null(ch) && nrow(ch) > 0) {
      # Los nombres varian segun la version que genero el .rds; se prueban en
      # orden y se cae al primero que exista.
      col <- function(fila, opciones) {
        for (o in opciones) if (!is.null(ch[[o]])) {
          v <- as.character(ch[[o]][fila])
          if (!is.na(v) && nzchar(v)) return(v)
        }
        "(no registrado)"
      }
      for (i in seq_len(nrow(ch))) {
        add("", sprintf("   [%s] item %s - %s",
                        i, col(i, c("item", "numero", "codigo")),
                        col(i, c("motivo", "razon"))))
        add(.inf_parrafo(paste("ANTES:", col(i, c("item_viejo", "antes",
                                                  "item_original_texto", "item_original"))), "        "))
        add(.inf_parrafo(paste("AHORA:", col(i, c("item_nuevo", "despues",
                                                  "item_nuevo_texto"))), "        "))
      }
    }
  }

  # ---- Configuracion del analisis -----------------------------------------
  if (!is.null(local$config)) {
    add("", .inf_regla("CONFIGURACION DEL ANALISIS COMPLETO"))
    cfg <- unlist(local$config)
    for (n in names(cfg)) add(.inf_campo(n, cfg[[n]]))
  }
  if (!is.null(es$params)) {
    add("", "  Parametros de la prueba de resistencia:")
    pr <- unlist(es$params)
    for (n in names(pr)) add(.inf_campo(paste0("  ", n), pr[[n]]))
  }
  if (!is.null(es$linea_base))
    add("", .inf_campo("Linea base del estres", .inf_num(es$linea_base, 3)))

  # ---- Acciones -----------------------------------------------------------
  add("", .inf_regla("QUE HACER AHORA"))
  ac <- g$acciones %|N|% character(0)
  if (length(ac) == 0) add("  Sin acciones pendientes.") else
    for (i in seq_along(ac)) add(.inf_parrafo(paste0(i, ". ", ac[i]), "  "))

  # ---- Linea de registro --------------------------------------------------
  #  Una sola linea con separador ';' para pegar en una hoja y comparar los
  #  intentos entre si: es el motivo por el que existe esta descarga.
  add("", .inf_regla("LINEA PARA TU REGISTRO (pegar en una hoja de calculo)"))
  add("  fecha;escenario;redaccion;n_items;desea_media;sd_entre;sd_intra;estabilidad;prob_limpia;phi_med;estres")
  add(paste0("  ", paste(c(
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    g$escenario %|N|% "-", g$calidad_redaccion %|N|% "-",
    if (is.null(it)) "-" else nrow(it),
    if (is.null(d)) "-" else .inf_num(mean(as.numeric(d$deseabilidad), na.rm = TRUE)),
    if (is.null(d)) "-" else .inf_num(d$sd_entre_dim, 3),
    if (is.null(d)) "-" else .inf_num(d$sd_intra_dim, 3),
    if (is.null(d)) "-" else .inf_num(d$estabilidad, 3),
    if (is.null(s)) "-" else .inf_pct(s$prob_limpia),
    if (is.null(s)) "-" else .inf_num(s$phi_med, 3),
    if (is.null(es)) "-" else (es$veredicto %|N|% "-")), collapse = ";")))

  # ---- Apendice: todo lo que no se haya impreso arriba --------------------
  #  Red de seguridad. El paquete gana campos con cada version y el informe se
  #  quedaria atras en silencio: aqui se vuelca AUTOMATICAMENTE cualquier campo
  #  de la compuerta que no se haya cubierto ya, para que "todo el estado"
  #  siga siendo cierto sin tener que acordarse de tocar este archivo.
  cubiertos <- c("escenario", "escenario_fusion", "avisos_diseno",
                 "escenario_detalle", "calidad_redaccion", "semaforo",
                 "veredicto", "acciones", "redaccion", "deseabilidad",
                 "estructura", "mapa_fusion", "estructura_alternativa",
                 "banda_estructura", "parametros")
  resto <- setdiff(names(g), cubiertos)
  sub_resto <- list(
    redaccion    = setdiff(names(g$redaccion), c("similitud_maxima",
                     "pares_redundantes", "facetas_repetidas", "ngram_overlap",
                     "homogeneidad_sintactica", "diversidad_lexica", "resumen",
                     "alerta", "parametros")),
    deseabilidad = setdiff(names(d), c("deseabilidad", "por_dimension",
                     "sd_entre_dim", "sd_intra_dim", "alerta_intra",
                     "estabilidad", "n_imputados", "uniforme", "riesgo_halo",
                     "mensaje", "pasadas")),
    # Los tres umbrales YA se muestran en la seccion de estructura (junto a la
    # cifra a la que se aplica cada uno), asi que se excluyen del apendice: si
    # no, el informe los repite en crudo al final y el mismo dato sale dos veces.
    estructura   = setdiff(names(e3), c("prob_limpia", "prob_ic", "prob_min",
                     "prob_max", "veredicto", "fuerza_central", "rmsea_med",
                     "phi_med", "phi_pares", "mapa_fusion", "sensibilidad",
                     "sensibilidad_phi", "carga_estandarizada_media", "resumen",
                     "umbral_phi", "umbral_fusion", "umbral_rmsea")))
  hay_resto <- length(resto) > 0 || any(lengths(sub_resto) > 0)
  if (hay_resto) {
    add("", .inf_regla("APENDICE · CAMPOS ADICIONALES"))
    add(.inf_parrafo(paste("Campos presentes en el resultado que no tienen",
        "seccion propia en este informe. Se vuelcan tal cual para no perder",
        "nada del estado."), "  "))
    volcar <- function(et, v) {
      add("", paste0("  [", et, "]"))
      if (is.data.frame(v)) add(.inf_tabla(v))
      else if (is.atomic(v) && length(v) <= 40)
        add(.inf_parrafo(paste(format(v), collapse = " · "), "    "))
      else add(paste0("    ", utils::capture.output(str(v, max.level = 2))))
    }
    for (n in resto) volcar(n, g[[n]])
    for (blq in names(sub_resto)) for (n in sub_resto[[blq]])
      volcar(paste0(blq, "$", n), g[[blq]][[n]])
  }

  add("", strrep("=", 74),
      paste0(" Generado por SeMiLLa · ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      strrep("=", 74))
  L
}

# Escribe en UTF-8 con BOM: sin el BOM, el Bloc de notas de Windows abre las
# tildes como simbolos raros y el informe llega ilegible.
.cmp_escribir_txt <- function(lineas, ruta) {
  con <- file(ruta, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(lineas, collapse = "\r\n"), "\r\n"))), con)
  invisible(ruta)
}

# ---------------------------------------------------------------------------
#  API publica
# ---------------------------------------------------------------------------
#' @title Informe de la compuerta, el mismo que muestra la aplicacion
#'
#' @description
#' Devuelve en texto el volcado COMPLETO de la compuerta: los tres controles con
#' sus numeros, el escenario, la estructura, la prueba de resistencia y los items
#' con su deseabilidad. Es exactamente el informe que descarga el Paso 5b de la
#' aplicacion Shiny, con las mismas etiquetas y los mismos veredictos.
#'
#' @details
#' Vive en el paquete, y no en la aplicacion, precisamente para que no puedan
#' divergir: quien trabaje desde R y quien use la aplicacion tienen que leer lo
#' mismo. La aplicacion llama a esta funcion; no guarda una copia propia.
#'
#' Nota sobre las etiquetas: el campo que describe el resultado es
#' \code{$escenario} (ROBUSTA / VULNERABLE / FRAGIL / SOLO PUNTAJE TOTAL / SE
#' ESPERAN k FACTOR(ES), NO K). El antiguo \code{$veredicto} ("NO APLICAR
#' TODAVIA" y companeros) se conserva por compatibilidad, pero daba falsos
#' negativos a instrumentos publicados que funcionan y no debe presentarse como
#' el resultado.
#'
#' @param escala Objeto \code{semilla} con \code{$compuerta} ya calculada.
#' @param local Resultados del analisis local (\code{$sim}, \code{$estres}), si
#'   los hay. Anaden las secciones de estructura completa y resistencia.
#' @param origen Texto libre sobre de donde salio el resultado.
#'
#' @return Un vector de caracteres, una linea por elemento.
#'
#' @examples
#' \dontrun{
#' cat(informe_compuerta(escala), sep = "\n")
#' writeLines(informe_compuerta(escala), "compuerta.txt")
#' }
#' @export
informe_compuerta <- function(escala, local = NULL, origen = NULL) {
  if (is.null(escala) || is.null(escala$compuerta))
    stop("La escala no tiene compuerta calculada. Ejecuta compuerta_pre_aplicacion() primero.")
  .cmp_informe_txt(escala, local = local, origen = origen)
}

#' @title Guardar el informe de la compuerta en un archivo
#' @param lineas Salida de \code{\link{informe_compuerta}}.
#' @param ruta Fichero de destino.
#' @return La ruta, de forma invisible.
#' @export
guardar_informe_compuerta <- function(lineas, ruta) {
  .cmp_escribir_txt(lineas, ruta)
  invisible(ruta)
}
