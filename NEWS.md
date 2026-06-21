# SeMiLLa 2.4.0 (2026-06-18)

## contexto_lenguaje(): variante regional de castellano

- Nuevo argumento **`variante_regional`** en `contexto_lenguaje()` y en
  `generar_escala_historias()` (default `"auto"`). Valor `"selva_peru"` (alias
  `"selva"`, `"amazonia_peru"`, `"amazonico"`) inyecta un bloque de castellano
  amazonico SIMPLE: lector adolescente a menudo bilingue, oraciones cortas
  (~12-14 palabras), una idea por frase, sin subordinadas encadenadas ni dobles
  condicionales, sin cultismos ('saldar', 'expiar', 'reparar', 'vulnerabilidad',
  'tentacion', 'fallas'), claro a la PRIMERA lectura en voz alta. Tambien acepta
  una cadena libre como guia de registro. Se combina con `etapa_evolutiva` y
  `nivel_socioeconomico`.
- Nota practica: los LLM tienden a exceder el limite de palabras en la
  generacion; para registros muy simples, conviene una pasada posterior de
  acortamiento (<=13 palabras/item) verificada (caso EPNA-H selva: media de
  18.9 -> 12.8 palabras).

## generar_escala_historias(): enfoque de items "facetas" vs "historias"

- Nuevo argumento **`enfoque_items`** (solo aplica con
  `items_modo = "por_historia"`):
  - `"facetas"` (default, comportamiento previo): recorre las mismas
    `facetas_percepcion` en cada historia -> items PARALELOS entre historias
    (diseno cruzado faceta x historia). La estructura factorial tiende a
    organizarse por faceta o por perspectiva (1a vs 3a persona), no por historia.
  - `"historias"`: cada historia genera items DISTINTIVOS de su constructo
    dominante (contenido propio, no facetas compartidas). El modelo deriva 4-6
    indicadores del MISMO constructo por historia (variedad sin redundancia),
    todos anclados al texto. Objetivo: que las K historias rindan K factores
    diferenciados y relacionados.
    - ENFOQUE DE RIESGO/PROPENSION (no estado actual): para escalas de cribado
      en poblacion general (la mayoria nunca se ha autolesionado), los items se
      redactan como PRECURSORES/vulnerabilidad con marco hipotetico-condicional
      ('podria', 'me costaria', 'entiendo la tentacion de', 'me identifico con
      la creencia', 'me despertaria curiosidad'). PROHIBIDO presuponer conducta
      o ideacion ACTUAL ('siento ganas de lastimarme', 'me vienen imagenes de
      cortarme', 'aprieto mi piel'): eso mediria a quien YA se autolesiona, no
      el riesgo. Marcos: vulnerabilidad/rasgo, creencias/expectativas (NEQ),
      actitud/identificacion, hipotetico/proyectivo y susceptibilidad social.
    - PERSPECTIVA UNICA: todos los items en PRIMERA PERSONA (autoinforme); no
      se generan items de 3a persona sobre la protagonista (evita el factor de
      metodo por perspectiva y mantiene la pureza de constructo de propension).
    - ANCLAJE A LA HISTORIA pero VARIADO: cada item se ancla a un suceso/elemento
      concreto de su historia (asi la historieta tiene sentido), pero variando la
      forma y posicion del anclaje para no crear plantilla.
    - CLARIDAD > BREVEDAD: los items son cortos pero COMPLETOS y gramaticales;
      prohibido el estilo telegrafico (no se omiten articulos/preposiciones).
    - GUARDIA LEXICA en registro simple/amazonico: si el `bloque_lenguaje` activo
      restringe el registro (selva, NIÑEZ, NSE bajo), los items que aun contengan
      palabras abstractas salientes que el LLM no logra suprimir ('angustia',
      'ansiedad', 'desregulacion', 'desconectado', 'vulnerabilidad') se reescriben
      automaticamente con un equivalente simple ('nervios', 'miedo', 'sentirme muy
      mal'). Garantiza el lexico de forma reproducible (con cache), sin edicion
      manual posterior.
  - `contexto_lenguaje()`: el bloque `variante_regional="selva_peru"` ahora prioriza
    explicitamente la CLARIDAD a la primera lectura sobre la brevedad (oraciones
    cortas pero completas, 12-16 palabras, sin frases entrecortadas).
    - ANTI-FRASEO: se prohiben aperturas plantilladas ('Si yo estuviera', 'A mi
      tambien', 'Yo tambien', 'Es comprensible que', etc.) y se exige que cada
      item arranque distinto; nuevo argumento interno `evitar_arranques` acumula
      las aperturas ya usadas en historias previas para que NINGUN arranque se
      repita ENTRE dimensiones. Asi se elimina el efecto plantilla que, de otro
      modo, induce covarianza de metodo y rompe la estructura por historias.
- Motivacion: con `enfoque_items = "facetas"` los items de una misma faceta
  resultan casi parafrasis entre historias, por lo que la prueba NO se organiza
  en factores alineados con las historias; `"historias"` corrige esto cuando el
  diseno busca que cada historia/historieta sea una dimension.
- Nueva funcion interna `.generar_items_distintivos_por_historia()`.
- `metadata` ahora guarda `items_modo` y `enfoque_items`.
- Compatibilidad: el default `"facetas"` reproduce exactamente el comportamiento
  anterior.

# SeMiLLa 2.3.0 (2026-06-14)

## forma_breve(): modo HIBRIDO con piloto empirico

- `forma_breve()` gana el argumento **`respuestas_piloto`**. Si se pasa una
  matriz de respuestas (una columna por item, en el orden de `x$items`), el corte
  final se hace por **discriminacion empirica** (correlacion item-resto por
  dimension), usando lo semantico solo como guardia anti-redundancia. Sin
  `respuestas_piloto`, mantiene el modo semantico (`repr - beta*cross`).
- Motivacion: lo semantico solo no supera ~69% de coincidencia con la seleccion
  empirica; el dato de respuesta captura varianza/efecto techo que el texto no ve.
- **Validacion (EEAP, vs seleccion empirica top-4 por carga):**
  forma_corta k-means = 69%; forma_breve semantica = 56%;
  forma_breve hibrida: piloto n=50 = 64% (ruidoso), n=70 = 71%, n=90 = 81%,
  n=100 = **88%**. Conclusion: el modo hibrido supera de forma fiable al
  semantico a partir de ~90 respuestas; pilotos muy pequenos son inestables.
- **Guia de uso documentada** en `forma_corta()` y `forma_breve()` (seccion
  "Recomendacion de uso"): **sin datos** (caso habitual) → `forma_corta`
  (default mas robusto, ~69%); **con piloto (~90+)** → `forma_breve` hibrido
  (~88%). El modo hibrido es opcional, nunca el comportamiento por defecto.

# SeMiLLa 2.2.1 (2026-06-14)

## Documentacion: discriminacion_semantica() — evidencia completa

- Se corrige el `@details` para reflejar fielmente a Kilmen & Bulut (2025): el
  vinculo entre unicidad semantica y discriminacion IRT es **moderado y depende
  de la subescala**, no una ley general. Antes se citaba solo r = -.546
  (ansiedad); ahora se documenta tambien el resultado **nulo r = +.036 (n.s.) en
  evitacion**, y la inestabilidad observada en la validacion interna EEAP
  (de ~ -.10 a +.67 entre dimensiones). No cambia el calculo (sigue siendo la
  similitud coseno media intra-subescala, fiel al articulo).
- Se anade referencia a Loevinger (1954) por la paradoja de la atenuacion, base
  conceptual del metodo.

# SeMiLLa 2.2.0 (2026-06-14)

## Nueva funcion: forma_breve() — forma corta por discriminacion semantica neta

- **`forma_breve(x, n_items, por_dimension, beta_discriminante, umbral_redundancia)`**:
  selecciona la forma corta por **representatividad neta de discriminacion**
  (`repr - beta*cross`: similitud media intra-dimension menos similitud media
  cross-dimension) con guardia anti-redundancia. Es una alternativa a
  `forma_corta()` (k-means + centroide), mas alineada con la logica de la carga
  factorial del CFA. Devuelve un objeto compatible (`semilla_forma_corta`) con
  `$puntajes` (repr, cross, score por item).
- **Hallazgo de validacion (EEAP, n = 100)**: la "unicidad" de `discriminacion()`
  correlaciona en sentido **inverso** a la carga empirica (rho ~ -.29);
  `repr - cross` es el mejor predictor semantico intra-dimension (rho ~ +.32)
  pero la concordancia es moderada/inestable. En el caso EEAP, `forma_breve`
  corrigio la seleccion en Apego Seguro/Evitativo (evita items prototipicos con
  efecto techo) pero no supero la coincidencia global de `forma_corta` (56% vs
  69%). Conclusion documentada: la fuerza empirica de un item depende de
  propiedades de respuesta (varianza/techo) que los embeddings no observan; la
  forma corta deberia calibrarse con un piloto empirico.

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
