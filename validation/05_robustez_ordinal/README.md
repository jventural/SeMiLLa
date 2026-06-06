# 05_robustez_ordinal — Análisis de robustez con tratamiento ordinal (WLSMV)

Responde a las observaciones **C3** y **C4** de la revisión mayor (jun-2026):

- **C3** — La fiabilidad de referencia del manuscrito se calculó con correlaciones de
  Pearson sobre ítems Likert ordinales. Aquí se recalcula con **correlaciones
  policóricas** (ω congenérico unidimensional) en las cinco escalas de Fase 3.
- **C4** — Se añade **CFA con estimador WLSMV** (policórico, ordinal) por escala,
  reportando CFI/TLI/RMSEA/SRMR robustos (*scaled*) y ω ordinal del modelo.
- Verificación exploratoria con `PsyMetricTools::EFA_modern` (extracción WLSMV).

## Archivos

- `analisis_ordinal_wlsmv.R` — script reproducible (set.seed(2026)).
- `salidas/tabla_C3_omega_pearson_vs_policorico.csv`
- `salidas/tabla_C4_cfa_wlsmv.csv`
- `salidas/tabla_EFA_modern_wlsmv.csv`

## Datos de entrada

Matrices de respuesta crudas (las mismas del manuscrito) en
`../../04_Benchmark_optimizacion/datos_respuestas/<id>.rds`
= `list(id, fase, respuestas[n×ítems], mapping[codigo, dimension])`.
El modelo teórico ítem→factor del CFA se toma de `mapping$dimension`.

## Resultados clave

- **ω policórico uniformemente mayor** que el de Pearson en Fase 3
  (Δ de +0,019 a +0,122; máximo en SCP, escala casi binaria). El orden de
  fiabilidad se mantiene → no cambian las conclusiones cualitativas.
- **CFA WLSMV**: ajuste bueno en DASS-21 (CFI 0,971), CFCS (0,964), RSES, SCP;
  aceptable en multidimensionales grandes (CFI 0,82–0,92). Mitos de Amor ajusta
  mal como unidimensional (CFI 0,820 → coherente con estructura bidimensional);
  WAST no converge (ítem sin varianza → justifica el uso de α).
- **EFA WLSMV** reproduce la estructura teórica (DASS-21, Dirty Dozen, SD3: el
  mejor ajuste corresponde al número teórico de factores).

Las dos tablas se integraron en `01_Manuscrito_FUSION/manuscript_SeMiLLa_fusion_*_v3`
(Tablas 10 y 11) y reemplazan el texto de "pendiente" en la sección de Limitaciones.

## Reproducir

```r
# Rscript desde esta carpeta (R 4.4.1, paquetes: lavaan, psych, semTools, PsyMetricTools)
Rscript analisis_ordinal_wlsmv.R
```
