# Validación empírica de SeMiLLa

Material de reproducibilidad del estudio de validación empírica descrito en el
manuscrito *"SeMiLLa: An R Package and Shiny App for LLM-Assisted Construction
and Pre-Empirical Semantic Screening of Psychometric Scales"*.

> **Esta carpeta NO forma parte del paquete instalable.** Está excluida del build
> de R (`.Rbuildignore`), de modo que `R CMD build` / CRAN no la incluyen. Vive
> aquí únicamente para mantener el código del estudio junto al paquete que evalúa.

## Estructura

| Carpeta | Contenido |
|---|---|
| `02_codigos_R/funciones/` | Funciones del estudio: `extraer_psicometria_pdf()`, `calcular_psicometria_empirica()`, `comparar_con_semilla()`, `comparar_estructura_factorial()`, más los pipelines por caso/dataset. |
| `02_codigos_R/analisis/` | Scripts de las tres fases: consolidación, recomparación, test de envenenamiento (*adversarial item probing*), grid de umbrales, comparación de modos y tests de estructura. |
| `03_escalas/` | Ítems de las escalas usadas (xlsx), por fase. |
| `04_reportes_html/` | Reportes autocontenidos (`.qmd` + `.html`) con los resultados completos y los consolidados (`.csv` / `.rds`). |
| `05_robustez_ordinal/` | Análisis de robustez ordinal (ω policórico, CFA WLSMV). |
| `06_ablacion_embeddings/` | Ablación del método de *clustering* (k-centroide vs. metaheurísticas). |

## Funciones clave

Estas funciones son **scripts del estudio**, no funciones exportadas del paquete
SeMiLLa. Se cargan con `source()`:

- `extraer_psicometria_pdf()` — recupera propiedades psicométricas de PDFs publicados (pdftools + `gpt-4.1-mini` con esquema JSON estricto). Fase 1.
- `calcular_psicometria_empirica()` — cómputo uniforme de α, ω, EFA unidimensional e ITC sobre datos crudos. Fases 2 y 3.
- `comparar_con_semilla()` — comparador central: concordancia de fiabilidad (r, ρ, MAE), concordancia ítem-nivel y detección de ítems problemáticos.
- `comparar_estructura_factorial()` — análisis paralelo (Horn, 1965) sobre respuestas vs. matriz de similitud coseno, comparados vía ARI.

## Datos crudos (no incluidos)

Los datos crudos **no se versionan aquí** (≈600 MB y/o restricciones de uso). Para
reproducir el estudio:

- **Fase 1 (9 papers en español):** PDFs públicos en Redalyc y SciELO; las escalas
  y valores extraídos están en `03_escalas/fase1_wleis_y_otros/`.
- **Fase 2 (10 datasets OpenPsychometrics):** descargables desde
  <https://openpsychometrics.org/_rawdata/>. Coloca los CSV en una carpeta
  `01_bases_de_datos/` local antes de correr los scripts.
- **Fase 3 (5 escalas peruanas, n = 315):** datos con consentimiento informado y
  aprobación del comité de ética de la Universidad Privada del Norte; disponibles
  para verificación independiente **a petición** al autor.

## Reproducibilidad

Todos los scripts usan `set.seed(2026)`, caché de embeddings determinista en disco
y `temperature = 0` en las llamadas al LLM. El paquete evaluado es
[`SeMiLLa`](https://github.com/jventural/SeMiLLa).
