# SeMiLLa v2.1

## SEmantic Measurement of Items via LLM Assistance

> An R Package and Shiny App for LLM-Assisted Construction and Pre-Empirical
> Semantic Screening of Psychometric Scales

```
   ____       __  __ _ _     _
  / ___|  ___|  \/  (_) |   | |    __ _
  \___ \ / _ \ |\/| | | |   | |   / _` |
   ___) |  __/ |  | | | |___| |__| (_| |
  |____/ \___|_|  |_|_|_____|_____\__,_|
```

Paquete de R para construir y validar escalas psicometricas asistido por
Modelos de Lenguaje (LLM). Genera items, los analiza semanticamente,
diagnostica calidad y exporta el cuestionario administrable.

---

## Instalacion

```r
install.packages("D:/14. LIBRERIAS/SeMiLLa", repos = NULL, type = "source")
library(SeMiLLa)
```

Configuracion inicial: definir `OPENAI_API_KEY` en `.Renviron` y `RETICULATE_PYTHON` apuntando al Python con el SDK `openai` instalado.

---

## Manual de Usuario v2.0 — orden por fases

El paquete sigue un flujo de 25 pasos agrupados en 9 fases. Cada paso corresponde a una accion concreta del usuario.

### Fase I. Antes de empezar

| Paso | Accion | Funcion principal |
|---|---|---|
| 1 | Instalacion y configuracion (API, .Renviron) | — |
| 2 | Cache de llamadas LLM | `cache(action, path)` |

### Fase II. Construccion de la escala

| Paso | Accion | Funcion principal |
|---|---|---|
| 3 | Definir el constructo y generar los items | `generar_items(tipo = ...)` |
| 4 | Inspeccionar y validar la generacion | `ver_items()` |

### Fase III. Analisis semantico

| Paso | Accion | Funcion principal |
|---|---|---|
| 5 | Convertir items a embeddings | `obtener_embeddings()` |
| 6 | Detectar redundancias | `analizar_redundancia()`, `items_similares()` |
| 7 | Estructura factorial alternativa | `efa_regularizado()` |
| 8 | Estructura por consenso ensemble | `precision_clasificacion(metodo = "ensemble")` |

### Fase IV. Refinamiento

| Paso | Accion | Funcion principal |
|---|---|---|
| 9 | Reemplazo iterativo de items problematicos | `refinar_escala(criterio = "ensemble")` |

### Fase V. Evaluacion psicometrica sin datos

| Paso | Accion | Funcion principal |
|---|---|---|
| 10 | Validez de contenido (V de Aiken con LLM) | `validez_contenido()` |
| 11 | Auditoria de redaccion | `auditar_redaccion_items()` |
| 12 | Fiabilidad semantica | `fiabilidad_semantica()` |
| 13 | Discriminacion y unicidad | `discriminacion_semantica()`, `cargas_semanticas()` |
| 14 | Coherencia intra/inter-dimensional | `analizar_coherencia()` |
| 15 | Validez de criterio predicha | `validez_criterio_predicha()` |

### Fase VI. Entregable final

| Paso | Accion | Funcion principal |
|---|---|---|
| 16 | Seleccion de la forma corta | `forma_corta()` |
| 17 | Recomendacion de escala de respuesta | `sugerir_escala_respuesta()` |
| 18 | Ensamblaje del test administrable | `ensamblar(tipo = ...)` |
| 19 | Exportacion | `exportar_escala()`, `guardar()`, `cargar()` |

### Fase VII. Adaptacion y comparacion

| Paso | Accion | Funcion principal |
|---|---|---|
| 20 | Adaptacion transcultural | `adaptar_transcultural()` |
| 21 | Tamizaje de DIF semantico | `detectar_dif_semantico()` |
| 22 | Comparar dos escalas | `comparar_escalas()` |

### Fase VIII. Visualizacion

| Paso | Accion | Funciones |
|---|---|---|
| 23 | Galeria de graficos | 14 funciones `plot_*` (ver seccion abajo) |

### Fase IX. Modos avanzados

| Paso | Accion | Funcion principal |
|---|---|---|
| 24 | Banco amplio para CAT (opcional) | `banco_cat()` |
| 25 | Pipeline manual sin LLM | `crear_plantilla_escala()`, `leer_escala()` |

---

## Flujo minimo (8 funciones obligatorias)

Para validar una escala desde cero (8 pasos):

```r
# 1. Configurar
cache("enable", path = "cache_llm")

# 2. Generar items
escala <- generar_items(
  tipo                    = "likert",
  concepto                = "resiliencia infantil",
  api_key                 = Sys.getenv("OPENAI_API_KEY"),
  n_items                 = 32,
  n_dimensiones           = 4,
  complejidad_linguistica = "basico",
  incluir_inversos        = FALSE,    # recomendado en escalas cortas
  seed                    = 2026
)

# 3. Embeddings y similitud
escala <- obtener_embeddings(escala, api_key = Sys.getenv("OPENAI_API_KEY"))

# 4. Diagnostico estructural por consenso ensemble (Voss et al., 2026)
estructura <- precision_clasificacion(
  escala,
  metodo     = "ensemble",
  algoritmos = c("kmeans","ward","gmm"),
  n_replicas = 10
)

# 5. Refinamiento iterativo basado en consenso
ref <- refinar_escala(
  escala,
  api_key         = Sys.getenv("OPENAI_API_KEY"),
  criterio        = "ensemble",
  umbral_consenso = 0.667,
  max_iteraciones = 8
)
escala <- ref$escala_final

# 6. Validez de contenido (V de Aiken con LLM como panel)
cv <- validez_contenido(escala, api_key = Sys.getenv("OPENAI_API_KEY"))

# 7. Forma corta
fc <- forma_corta(escala, n_items = 16, por_dimension = TRUE)

# 8. Test administrable (tipo = instrumento; formato = archivos de salida)
test <- ensamblar(
  tipo             = "likert",
  escala           = escala,
  forma            = "ambas",
  forma_corta_obj  = fc,
  archivo          = "EEAP_formulario",
  formato          = c("md","docx","html")
)
```

---

## Galeria de graficos disponibles

| Funcion | Cuando usarla |
|---|---|
| `plot_similitud()` | Heatmap de cohesion intra-dimension |
| `plot_embeddings()` | t-SNE / UMAP de la estructura semantica |
| `plot_red_items()` | Red de items con similitud > umbral |
| `plot_estructura()` | Grafo dimension <-> cluster |
| `plot_v_aiken()` | V de Aiken por item con IC 95% |
| `plot_fiabilidad()` | Alpha semantico por dimension |
| `plot_discriminacion()` | Unicidad semantica por item |
| `plot_coherencia(tipo = "boxplot"/"violin")` | Distribucion intra vs inter-dimension |
| `plot_precision()` | Matriz de confusion cluster <-> dimension |
| `plot_evolucion_precision()` | Trayectoria del refinamiento |
| `plot_redundancia()` | Pares de items con similitud > 0.85 |
| `plot_sankey()` | Flujo dimension teorica -> cluster empirico |
| `plot_cargas()` | Heatmap de cargas del EFA regularizado |
| `plot_scree()` | Sedimentacion de eigenvalues |
| `plot_forma_corta()` | Items seleccionados para la forma corta |
| `plot_resumen()` | Reporte agregado (todas las metricas) |

---

## Cambios respecto a v1.x

### Funciones nuevas (interfaz unificada v2.0)

| Funcion v2.0 | Sustituye a |
|---|---|
| `cache(action, path)` | `habilitar_cache()`, `deshabilitar_cache()`, `info_cache()`, `limpiar_cache()` |
| `generar_items(tipo = ...)` | 6 funciones por formato |
| `ensamblar(tipo = ...)` | 6 funciones por formato |
| `plot_coherencia(tipo = ...)` | `plot_coherencia_boxplot()`, `plot_coherencia_violin()` |
| `auditar_redaccion_items()` | `evaluar_calidad_items()` (renombre semantico) |

### Funciones archivadas (no exportadas, accesibles via `SeMiLLa:::`)

- `efa_embeddings()`, `comparar_estructura()` — reemplazadas por `precision_clasificacion()` con `metodo = "ensemble"` y `efa_regularizado()`
- `predecir_irt()`, `plot_irt()` — heuristica sin respaldo empirico
- `plot_jaccard()` — solapa con `plot_sankey()`
- `validar_escala()` — ahora interna, llamada desde `generar_items()`
- `ayuda()` — reemplazada por la documentacion estandar de R

### Compatibilidad

Todos los nombres v1.x siguen funcionando. Los renombres muestran un
warning informativo una vez por sesion.

---

## Decisiones que el usuario debe documentar

1. Polaridad: con o sin items invertidos (recomendacion: sin, en escalas < 50 items)
2. Complejidad linguistica (`minimo`, `basico`, `intermedio`, `avanzado`)
3. Tipo de escala de respuesta (`frecuencia`, `acuerdo`, `intensidad`, `preferencia`)
4. Algoritmos del ensemble (default: kmeans + ward + gmm) y numero de replicas (default: 10)
5. Umbral de consenso para refinamiento (default 0.667)
6. Modelo LLM y `seed` para reproducibilidad

---

## Limitaciones conocidas

1. Embeddings dependen de proveedor cerrado (OpenAI). El paquete soporta
   alternativas pero no por defecto.
2. V de Aiken via LLM puede subestimar items invertidos sin la marca
   apropiada. SeMiLLa los detecta automaticamente con una heuristica
   (caso_a + caso_b + caso_c + caso_d en `evaluar.R`).
3. Adaptacion transcultural amplifica el DIF semantico cuando los items
   son cortos. Conviene validar con panel bilingue humano.
4. EFA regularizado puede colapsar a 1 factor sin double-centering. El
   default `centrado = "double"` previene este problema.

---

## Fundamento cientifico

El paquete se sustenta en evidencia 2022-2026 sobre uso de LLM en
construccion y validacion de instrumentos. Las 12 referencias clave
estan en `inst/REFERENCES.bib`. Listado resumido:

- **Milano et al. (2025)** — embeddings predicen estructura factorial
- **Voss, Wu, Javalagi & Kell (2026)** — clustering ensemble para cargas
- **Barendse & de Vries (2024)** — HEXACO con ChatGPT
- **Hernandez & Nie (2023)** — AI-IP, banco de items LLM
- **Krumm et al. (2024)** — SJT con ChatGPT
- **Goretzko (2023)** — EFA regularizada
- **Belzak (2023)** — regDIF
- **Grobelny et al. (2025)** — adaptacion transcultural con LLM
- **Fokkema et al. (2022)** — ML y prediccion en assessment
- **Gao et al. (2026)** — banco CAT con LLM
- **Dumas, Greiff & Wetzel (2025)** — diez lineamientos AI scoring
- **Wulff & Mata (2025)** — embeddings y jingle-jangle

---

## Licencia

MIT License

## Autor

Dr. Jose Ventura-Leon  
SeMiLLa: SEmantic Measurement of Items via LLM Assistance
