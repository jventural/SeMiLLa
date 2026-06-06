# 06_ablacion_embeddings — Ablación del modelo de embeddings (E2)

Responde a la observación E2 del LLM Council: todo el flujo depende de un modelo
propietario (text-embedding-3-small, OpenAI). Se recalculan los índices semánticos
de la Fase 3 con un modelo ABIERTO multilingüe (sentence-transformers,
paraphrase-multilingual-MiniLM-L12-v2, local vía reticulate/Python 3.11) y se
comparan contra OpenAI y contra el empírico.

## Hallazgo
- El omega semántico y la sobreestimación sistemática son casi idénticos entre
  OpenAI y el modelo abierto (dif <= 0.013) -> robusto al proveedor.
- El fallo de Celos (r carga~coherencia ~ -0.76) se reproduce con ambos modelos
  -> el colapso por homogeneidad sintáctica no depende del modelo.
- La concordancia item-level fina es más variable entre modelos (esperable con
  n=315 e items pocos).

## Archivos
- analisis: `ablacion_embeddings.R`
- salida: `salidas/tabla_E2_ablacion.csv`

Reproducir: Rscript ablacion_embeddings.R (requiere reticulate + sentence-transformers).
