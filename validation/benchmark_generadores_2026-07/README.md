# Benchmark de generadores LLM — 2026-07-09

Primer uso de `comparar_generadores()` (SeMiLLa 2.7.0). Pregunta: ¿los
modelos GPT-5 generan mejores items que gpt-4.1-mini (default historico)?

## Diseno

- Constructo: autorregulacion del aprendizaje (2 dimensiones contrastables:
  Planificacion del estudio / Manejo de distracciones digitales).
- 12 items por modelo (6 x 2), mismas reglas de redaccion del paquete
  (Ferrando, anti-cuantificadores, diversidad, max 14 palabras), poblacion
  universitarios peruanos, seed 2026.
- Metricas objetivas: separabilidad de embeddings, clasificacion
  leave-one-out, pares/facetas redundantes (umbral auto), muletillas,
  longitud, tiempo.
- Doble juez LLM CIEGO y cruzado (gpt-4.1-mini y gpt-5-mini; orden
  aleatorio, sin conocer al autor): claridad, especificidad, diversidad,
  naturalidad (1-10).

## Resultados (tabla completa en resultado.rds / log_completo.txt)

| modelo | clasif_loo | pares | muletillas | jueces (prom.) |
|---|---|---|---|---|
| gpt-4.1-mini | 0.92 | 2 | 3 | 7.12 |
| gpt-5-nano | 1.00 | 0 | 2 | 7.62 |
| **gpt-5-mini** | 1.00 | 0 | 2 | **8.25** |
| gpt-5.4-nano | 1.00 | 4 | 2 | 7.12 |
| gpt-5.4-mini | 1.00 | 4 | 4 | 7.75 |
| gpt-5.2 | 1.00 | 4 | 1 | 7.75 |

## Conclusiones

1. **gpt-5-mini gana**: mejor puntaje de ambos jueces (incluido el juez de
   la familia rival), 0 pares redundantes, clasificacion perfecta. Su
   "separabilidad" baja (.085) es consecuencia de MAYOR diversidad
   intra-dimension (deseable: leccion PM), no un defecto — la clasificacion
   se mantiene en 100%.
2. **gpt-5-nano es el mejor calidad/precio**: supera a gpt-4.1-mini en
   todo (jueces 7.62 vs 7.12; 0 vs 2 pares; 100% vs 92% clasificacion) a
   1/8 del precio de input ($0.05 vs $0.40 por millon).
3. **Mas nuevo NO es mejor**: gpt-5.4-mini/nano y gpt-5.2 producen items
   PLANTILLADOS ("Al estudiar, X" repetido; 4 pares redundantes) e incluso
   con errores gramaticales ("Al estudiar, silencia mi celular",
   "silenciamos"). Su alta "separabilidad" (.188) es artefacto de la
   plantilla compartida intra-dimension.
4. gpt-4.1-mini queda superado como default: unico modelo con error de
   clasificacion y peor puntaje de ambos jueces.

## Decision aplicada

- Default de generacion en SeMiLLa_App: **gpt-5-mini** (mejor calidad),
  con gpt-5-nano etiquetado como opcion mas economica.
- Gotcha de API descubierto en el proceso: gpt-5.1+ rechaza
  reasoning_effort="minimal" (usa "none"); normalizado en
  `.normalizar_razonamiento()`.
