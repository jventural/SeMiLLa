# SeMiLLa 2.1.0 (2026-06-05)

Actualización derivada de la validación empírica del manuscrito de fusión (v4)
y de la revisión por pares (LLM Council).

## Nuevas funciones

* `auditar_redundancia()`: auditoría multi-índice de redundancia de ítems
  (similitud máxima entre pares, pares redundantes, solapamiento de n-gramas,
  homogeneidad sintáctica con alerta, y diversidad léxica). Detecta el patrón
  que colapsa los índices semánticos (caso Escala de Celos, r = -0.73).
* `modelos_embeddings_libres()`: lista de modelos de embeddings de **acceso
  libre** (multilingües) utilizables en local.
* `coherencia_dimensional()` y `homogeneidad_semantica()`: alias descriptivos de
  `omega_semantico()` y `fiabilidad_semantica()` que dejan explícito que son
  proxies pre-empíricos, no fiabilidad poblacional (CTT/IRT).

## Mejoras

* **Backend de embeddings de código abierto**: `obtener_embeddings()` ahora
  acepta modelos locales vía `sentence-transformers` (reticulate), además de
  OpenAI. Basta pasar `modelo_embedding = "paraphrase-multilingual-MiniLM-L12-v2"`
  (o cualquier identificador `org/modelo` de Hugging Face, o el prefijo
  `"local:"`). No requiere clave de API ni costo. La ablación del manuscrito
  mostró que las estimaciones agregadas son robustas al cambio de proveedor.
* `omega_semantico()` y `fiabilidad_semantica()` emiten una **alerta de
  homogeneidad sintáctica** cuando los ítems comparten plantilla, avisando de
  que los índices pueden no ser interpretables como consistencia.

## Notas

* Los nombres `omega_semantico()`/`fiabilidad_semantica()` se conservan por
  compatibilidad; miden cohesión geométrica del texto, no varianza verdadera.
