# SeMiLLa 2.9.15 (2026-08-05)
## El juez de gemelos: menos falsos positivos y resultado reproducible

Dos quejas del autor sobre el eje 1, medidas antes de tocar nada (gpt-4.1-mini,
dos escalas reales de aburrimiento parental, 9 pasadas por condicion).

**(a) No era reproducible, y el `seed` no lo arreglaba.** Misma escala, misma
configuracion:

| Condicion | Resultados distintos en 6 corridas | Algun par en las 6 |
|---|---|---|
| sin seed | 5 de 6 | ninguno |
| con seed | 3 de 6 | ninguno |

El `seed` de OpenAI es best-effort: no garantiza nada. Aparte, **no llegaba**:
`compuerta_pre_aplicacion()` solo lo usaba en `set.seed()` de la simulacion y
nunca fijaba la opcion `SeMiLLa.seed` que `.llamar_openai()` envia a la API. Los
jueces de los ejes 1 y 2 corrian sin semilla mientras el usuario creia haberla
fijado. Ahora se propaga; sirve de poco por si sola, pero deja de mentir.

**(b) Era demasiado severo.** El prompt tenia 5 reglas de "SI son gemelos", todas
con ejemplos concretos, frente a 2 de "NO", genericas y sin ninguno. Los ejemplos
pesan mas que las reglas abstractas, y el juez llegaba inclinado al si:

- "Miro el reloj con frecuencia" vs "digo EN VOZ ALTA cuanto falta": gemelos en
  **8 de 9** pasadas. Una conducta es privada y la otra verbal.
- "miro el telefono" vs "pongo el telefono EN OTRA HABITACION": llego a marcarse,
  y son conductas opuestas.
- La regla "misma conducta con escenario superficialmente distinto" -escrita para
  'devuelvo dinero' vs 'devuelvo objetos'- se aplicaba a "a la hora de dormir
  sigo el mismo procedimiento" vs "cuando salimos propongo los mismos lugares".

### Lo que cambia

- **Prompt reequilibrado.** Una PRUEBA DECISIVA operativa al principio -si una
  persona puede puntuar alto en uno y bajo en el otro sin contradecirse, no son
  gemelos-, que es la definicion misma de dependencia local; las reglas negativas
  pasan a tener ejemplos, como las positivas; y se dice explicitamente que
  cambiar la SITUACION rompe la gemelidad.
- **Votacion por mayoria** (`n_pasadas_gemelos = 3`): un par se acepta si sale en
  la mayoria de las consultas. Los pares legitimos salieron 9 de 9 y los dudosos
  entre 1 y 5, asi que la frecuencia separa unos de otros. La salida incluye
  `votos` y `de`, para distinguir "el juez lo vio siempre" de "lo vio por poco".
  Con `n_pasadas_gemelos = 1` se recupera el comportamiento anterior.
- **`seed` se propaga** a la opcion global que consume `.llamar_openai()`.

### Por que hacen falta las dos cosas

La votacion sola no basta: el falso positivo "miro el reloj" ~ "digo en voz alta
cuanto falta" salia en **8 de 9** pasadas. Ninguna mayoria lo elimina; hace falta
el prompt. Y el prompt solo tampoco: los casos limitrofes siguen oscilando entre
corridas, y ahi es la mayoria la que estabiliza.

### Verificacion

| Escala | Antes | Despues (2.9.15) |
|---|---|---|
| La del autor (13 items) | 4 pares: 2 estables + 2 ruido (5/9 y 3/9) | los 2 legitimos, **4 corridas identicas** |
| Experimento 1 (20 items) | falso positivo en 8/9 + 3 pares de ruido | ninguno, **4 corridas identicas** |
| Control con gemelos plantados | los detecta | **los detecta**: la sensibilidad se conserva |

El control positivo importa: sin el, "no marca nada" seria indistinguible de
"se apago el juez".

# SeMiLLa 2.9.7 (2026-08-03)
## optimizar_para_campo(): no regresion, umbral consistente y balance visible

Corridas reales (escala de ansiedad ante la estadistica, 18 items, 3
dimensiones) mostraron que el bucle podia DEGRADAR la escala mientras la
corregia: elimino los 2 clusters de faceta repetida, pero introdujo 2
parafrasis gemelas confirmadas por el juez LLM y abrio un desbalance de
deseabilidad DENTRO de las dimensiones (DE intra 0.09 -> 0.16) que antes no
existia. El veredicto seguia en "NO APLICAR TODAVIA", ahora por motivos
distintos, y la funcion devolvia esa version degradada.

- **No regresion**: cada version se puntua con `.score_compuerta()` (el
  veredicto manda; dentro del mismo veredicto pesan gemelos, clusters, pares,
  alertas de deseabilidad y probabilidad de estructura limpia) y se devuelve
  la MEJOR version vista, no la ultima. `$optimizacion$revertido` indica si
  hubo reversion y `$optimizacion$historial` incorpora la columna `score`.
- **Umbral de aceptacion consistente**: el filtro que acepta un item nuevo
  usaba coseno 0.70 FIJO mientras `auditar_redundancia()` detecta con un
  umbral adaptativo en [0.62, 0.70]. En esa franja el candidato se aceptaba y
  a la iteracion siguiente reaparecia como par redundante: el bucle perseguia
  su propia cola (5 de 15 reemplazos fueron reemplazos de reemplazos). Ahora
  el filtro toma el umbral efectivo de la compuerta menos un margen de 0.03.
- **Embeddings vivos**: los candidatos se comparan contra una matriz que se
  actualiza dentro de la misma tanda, de modo que dos reemplazos simultaneos
  ya no pueden parecerse entre si. `.verificar_redundancia_item()` devuelve
  ahora `embedding_nuevo` para permitirlo sin re-embeber toda la escala.
- **Polaridad de deseabilidad preservada**: el prompt de reemplazo recibe la
  deseabilidad del item que sustituye y exige que el nuevo cueste admitirlo
  aproximadamente lo mismo. Antes solo habia restriccion anti-halo, y solo si
  el semaforo de deseabilidad ya estaba en amarillo o rojo: por eso el bucle
  CREABA el desbalance intra partiendo de un eje en verde.
- **Facetas ya cubiertas**: el prompt lista los items que SI se conservan en
  esa dimension y exige una manifestacion distinta de todas ellas. Sin esa
  lista, 9 de 15 reemplazos de una misma dimension derivaron a la misma idea
  ("dejar preguntas en blanco").
- **Balance antes/despues**: `$optimizacion$balance` devuelve un data.frame
  con el valor inicial, el final y el sentido del cambio por indicador
  (gemelos, facetas, pares, deseabilidad intra/entre, probabilidad de
  estructura limpia, |Phi|). En consola se imprime al terminar, marcando con
  [EMPEORA] lo que se rompio. La funcion optimiza redundancia semantica y
  puede pagar ese arreglo en otro eje: eso ahora se ve, no se descubre.

# SeMiLLa 2.9.6 (2026-07-29)
## Progreso visible en simular_estructura() y avisos de API accionables

- `simular_estructura()` ahora informa mientras corre, igual que
  `estres_escala()`: encabezado (items, dimensiones, n, escenarios, replicas,
  CFAs totales, nucleos y ETA) y una BARRA DE PROGRESO por escenario con
  replicas completadas y ETA, seguida del resultado del escenario
  (prob. limpia, RMSEA mediano, |Phi| mediano). Antes no imprimia nada hasta
  el final y una corrida de varios minutos parecia colgada.
- Los numeros NO cambian: las replicas se trocean en lotes solo para poder
  dibujar la barra, cada una conserva su stream L'Ecuyer y se apilan en el
  orden original (mismo `seed` -> mismo resultado que en 2.9.5).
- La barra se redibuja sobre si misma (`\r`) en consola interactiva y va
  linea por linea cuando la salida se lee en streaming (Rscript, `callr`,
  el log de la app), donde `\r` nunca se veria.
## Correcciones

- `auditar_redundancia()` abortaba con "the 'height' component of 'tree' is not
  sorted (increasingly)" cuando la matriz de similitud traia valores EMPATADOS
  (matriz constante, similitudes redondeadas, escalas pequenas): el enlace
  promedio devuelve alturas como 0.69999999999999996 seguidas de
  0.69999999999999984 y `cutree()` rechaza esa inversion de 1.2e-16. El nuevo
  `.hc_monotono()` monotoniza las alturas (`cummax`) sin alterar la particion, y
  avisa solo si la inversion excede 1e-8 (senal de un metodo NO monotono). Se
  aplica tambien al clustering ward.D2 y diana de `evaluar_estructura()`, donde
  el mismo fallo dejaba metodos fuera del ensemble en silencio.
- La deteccion de facetas repetidas pasa a ser un subanalisis tolerante a
  fallos: si el clustering no puede correr se avisa y se conservan los demas
  indices, en lugar de perder toda la auditoria (incluida la alerta de
  homogeneidad sintactica).
- `S` se acota a [-1, 1] antes de construir la distancia, para que un coseno
  devuelto como 1 + 1e-16 no genere distancias negativas.
- Se agrega `tests/testthat.R` (faltaba): sin ese archivo `R CMD check` NUNCA
  ejecutaba la suite de tests, que es la razon por la que el fallo anterior y un
  `expect_named` desactualizado pasaron inadvertidos. Nuevo test de regresion
  para similitudes empatadas.
- Los avisos de canal LLM caido dejan de ser una linea criptica
  ("AVISO: canal juez de parafrasis sin resultado (¿API?)"): ahora
  DIAGNOSTICAN la causa (sin clave / falta el entorno Python de reticulate +
  openai / la llamada fallo, con el detalle del error), dicen DONDE poner la
  clave (argumento `api_key`, `Sys.setenv`, `.Renviron`) con el enlace para
  obtenerla, aclaran que se pierde exactamente y como apagar el canal para
  no volver a ver el aviso. Aplica al juez de parafrasis (Eje 1) y al sesgo
  de deseabilidad (Eje 2).


# SeMiLLa 2.9.4 (2026-07-15)
## Motor de escenarios en estres_escala() (anadido 2026-07-16)

- Nuevos canales de dependencia local ademas del coseno >= `umbral_ld`:
  `dl_juez` ("escenario" default | "off" | "on") inyecta los pares que el juez
  LLM de parafrasis detecta (gemelos SEMANTICOS, coseno 0.43-0.78), y
  `dl_adyacencia` ("off" default) inyecta pares consecutivos (efecto de orden).
  En modo "escenario" NO alteran el pronostico base: se recalcula SOLO la
  linea base (dosis 0) con los pares inyectados y se reporta lado a lado.
- Fuerzas calibradas con datos reales: `g_juez = 0.35` (residual ~0.11,
  el observado en el par ASS3~ASS7 con N = 1144) y `g_ady = 0.25`.
- **Veredicto de DOS EJES**: `$origen` (estructura de partida: LIMPIA /
  EN DUDA con pares nombrados / COMPROMETIDA) + `$veredicto` (resistencia
  a sesgos: ROBUSTA / VULNERABLE / FRAGIL, sin cambios de compatibilidad).
- Evidencia de calibracion (por que "escenario" y no "hecho"): el juez
  acierta en escalas recien generadas (bateria policial: sus gemelos si
  tuvieron r policorica >= .70 en campo) pero sobre-alarma en escalas ya
  depuradas (ASS, MitosAmor); y la dependencia local empirica dominante en
  escalas aplicadas en orden fijo es la ADYACENCIA (8/9 pares con MI >= 10
  en escalas de pareja + 3/3 errores correlacionados del articulo de Celos
  fueron items consecutivos), invisible para cualquier analisis del fraseo.
- Cierre de blindaje: ahora tambien consulta el COSENO (pares >= 0.80 se
  marcan como gemelos aunque el juez no los señale) y re-verifica con
  embeddings frescos (hasta 2 pases); `blindar_escala()` y `semilla()` ganan
  `modelo_jueces` (default "gpt-4.1-mini": ~2x mas rapido que gpt-5-mini con
  el mismo criterio en benchmark) y los candidatos rechazados se acumulan en
  el prompt (con temperatura 0 los reintentos eran identicos).


## Blindaje: jueces LLM de contexto y parafrasis DENTRO del pipeline

- Nuevo modulo `R/blindaje.R`: `.juzgar_contexto_llm()` (items que rompen el
  rol/etapa/entorno de la poblacion), `.juzgar_parafrasis_llm()` (pares
  gemelos que miden la misma conducta/creencia/emocion con otras palabras,
  aunque solo cambie un verbo sinonimo, el objeto o el beneficiario) y la
  funcion exportada `blindar_escala()`. Motivacion (bateria policial, n=280):
  los embeddings SUBDETECTAN parafrasis; pares con r policorica >= .70 en
  campo (dependencia local, Phi interfactorial inflado hasta .92) vivian en
  coseno 0.43-0.78.
- `generar_escala(blindaje = TRUE)` (default): FASE 2b tras la generacion.
- CIERRE al final de `semilla()`: la optimizacion de compuerta y el optimizador
  de estres reescriben items por caminos sin jueces; el cierre re-aplica
  jueces + garantia de longitud al objeto FINAL y recalcula embeddings.
- Cada CANDIDATO de reemplazo pasa tres puertas ANTES de aceptarse:
  longitud -> contexto (regex + juez) -> gemelidad (`.es_gemelo_candidato()`).
  Sin esto, los reemplazos reintroducian fugas ("en patrulla") o nacian
  gemelos de otros items y quedaban sin re-juicio al agotarse las rondas.

## Nuevo parametro `contexto_prohibido`

- En `semilla()`, `generar_escala()` y `blindar_escala()`: lista de terminos
  vetados por el usuario. Se inyecta una vez en la descripcion de la poblacion
  (fluye a TODOS los prompts: generacion, FC, optimizacion, estres) y ademas
  se aplica por REGEX deterministico en el blindaje, porque el juez LLM es
  inconsistente en terminos ambiguos (p. ej. "patrulla" para estudiantes de
  escuela policial).

## Otros

- `auditar_redundancia()`: el umbral "auto" ahora tiene techo 0.70 (antes
  0.85, que no detectaba ningun gemelo real).
- Compuerta: juez LLM de parafrasis como segunda capa (gemelos confirmados =
  estado riesgo; se fusionan a `pares_redundantes` y `optimizar_para_campo()`
  los reescribe); alerta nueva cuando |Phi| simulado < .15 (dimensiones casi
  ortogonales).
- `optimizar_para_campo()`: los reemplazos respetan `metadata$max_palabras`
  (antes el rango derivado permitia hasta 16 palabras en escalas "minimo").
- `generar_escala_forcedchoice()` gano `complejidad_linguistica` y
  `max_palabras` (antes tope fijo de 14 palabras).
- Garantia dura de longitud en el blindaje: item que excede `max_palabras`
  se regenera.

## instrucciones_estilo (anadido 2026-07-16 tarde)

- Nuevo parametro `instrucciones_estilo` en `semilla()`, `generar_escala()` y
  `blindar_escala()`: separa QUIEN es la poblacion (que el juez de contexto
  evalua) de COMO deben sonar los items (que va solo a los prompts de
  redaccion/regeneracion). Motivo: meter criterios de estilo dentro de
  `poblacion` volvia al juez de contexto un inquisidor (marco 12/16 items
  sanos como "fuera de contexto" por no calzar con instrucciones de
  redaccion). La App (Paso 3) gano los campos "Terminos vetados" y
  "Instrucciones de estilo", cableados a generar_items() y documentados en el
  script reproducible.

## Mejoras pendientes (anotadas, no implementadas)

- **Juicio por lotes**: hoy cada candidato de reemplazo gasta 2-3 llamadas LLM
  secuenciales (contexto + gemelidad); el cuello de botella del pipeline es la
  latencia de la API, no la CPU (las simulaciones ya usan detectCores()-1).
  Mejora: una sola llamada que genere y juzgue VARIOS candidatos a la vez
  (o juzgue en lote todos los ofensores de una ronda), reduciendo las llamadas
  encadenadas de ~100-200 a ~10-20 por escala.
- Evaluar proveedores rapidos (`usar_proveedor("groq")`) para los JUECES:
  la inferencia LPU baja la latencia por llamada ~10x, pero hay que validar
  que el criterio del juez con modelos abiertos iguale al de gpt-5-mini
  (los embeddings seguirian en OpenAI).

# SeMiLLa 2.9.3 (2026-07-14)

## estres_escala() ahora OPTIMIZA (optimizar_estres), no solo diagnostica

- `estres_escala()` gano `optimizar_estres = TRUE` (default), `max_iteraciones`
  (5), `n_reescribir`, `n_rep_intermedio` (30), `modelo` y `poblacion`. Antes era
  puramente diagnostico: reportaba el punto de quiebre y los items que colapsan
  pero no hacia nada. Ahora, si la escala resulta VULNERABLE/FRAGIL, entra en un
  LOOP: toma los items que colapsan primero (bajo su peor sesgo), los reescribe
  con una instruccion ANTI-SESGO (anti-deseabilidad, anti-aquiescencia, etc.),
  recalcula embeddings + deseabilidad y RE-CORRE el stress test, quedandose con
  la version de menor fragilidad; se detiene si llega a ROBUSTA o no mejora en 2
  vueltas. Devuelve ademas `$optimizacion` (historial iteracion a iteracion) y
  `$escala_final` (la escala mejorada). Con `optimizar_estres = FALSE` se
  mantiene el comportamiento clasico (solo diagnostico). Es generico: sirve para
  cualquier sesgo que salga mal, no solo deseabilidad. Nota honesta: en
  constructos intrinsecamente deseables (moral, valores) la reescritura baja la
  fragilidad pero no la elimina; el camino solido sigue siendo el formato
  (eleccion forzada) o una escala de deseabilidad de control.
- `semilla()` gano `estres = FALSE` (paso pesado, apagado por defecto) y
  `optimizar_estres = TRUE`. Con `estres = TRUE` corre la prueba de estres al
  final y, si optimiza, adopta la escala mejorada. Funciona tambien en el camino
  `fuente = "usuario"`: cargar los items de un articulo (via
  `crear_plantilla_escala()` + Excel), `compuerta = FALSE`, y `estres = TRUE`
  para estresar/optimizar ese test desde R local.


## Fidelidad de contexto a la poblacion en la generacion de items

- El prompt de generacion (`.generar_items_dimension`) reforzo el bloque de
  poblacion: la instruccion generica "Poblacion: X. Adapta el lenguaje y
  contenido" era demasiado blanda y el modelo respetaba el TEMA pero inventaba
  escenarios de otra etapa o rol. Ahora se exige FIDELIDAD DE CONTEXTO con una
  restriccion explicita y NEGATIVA (aplicable a cualquier poblacion): no
  atribuir al respondiente actividades, responsabilidades u objetos que aun no
  le corresponden por su etapa vital/formativa o su rol (p. ej. tareas de un
  profesional en ejercicio cuando la poblacion todavia esta en formacion), y
  anclar cada situacion en su entorno cotidiano real.
  - Motivacion (caso real): la bateria policial de una tesis (cadetes de
    escuela de formacion, varones 17-24) generaba items con "mis patrullas",
    "mi servicio", "mi unidad" o "tramites", escenas de policia en ejercicio
    impropias de un estudiante aun en formacion. La poblacion SI se pasaba al
    prompt; el fallo era la debilidad de la instruccion, no su ausencia.
- `generar_escala_forcedchoice()` ahora acepta `poblacion` y la propaga a
  `.generar_items_dimension_fc()` con la misma fidelidad de contexto (antes el
  generador forced-choice ignoraba por completo la poblacion).

## Fidelidad de contexto: no confundir "escuela de formacion" con colegio

- El bloque de poblacion de `.generar_items_dimension` ahora prohibe
  explicitamente el vocabulario de colegio de menores ("escolar", "colegio",
  "aula", "maestro", "acoso escolar", "salon de clases") cuando el entorno es una
  INSTITUCION ESPECIFICA que no es un colegio, y aclara que el que la institucion
  se llame "escuela" (p. ej. escuela de formacion policial) NO la convierte en un
  colegio de ninos. Ademas advierte contra introducir roles de vida personal no
  pertinentes ("mi pareja", "mi jefe", "mis hijos"). Detectado con la bateria
  policial (cadetes 17-24): items con "voluntariados escolares" y "acoso escolar".

## semilla() expone el nivel de lectura (complejidad_linguistica, max_palabras)

- `semilla()` gano los argumentos `complejidad_linguistica` (default
  "intermedio") y `max_palabras` (default NULL -> derivado del nivel: minimo=10,
  basico=12, intermedio=18, avanzado=25). Antes el punto de entrada principal NO
  permitia controlar el nivel de lectura: `generar_escala()` si lo tenia, pero
  `semilla()` no lo pasaba y usaba siempre "intermedio" (secundaria completa),
  produciendo items largos (media ~14-15 palabras) y con vocabulario sofisticado
  ("visceral", "contraviene", "concienciacion", "desprotecion social"),
  impropios para poblaciones de nivel socioeconomico medio/bajo.
- Ademas, `semilla()` ahora PRESERVA `complejidad_linguistica`, `max_palabras` y
  `tipo_escala_respuesta` en el `metadata` del objeto resultante. Antes los
  descartaba al rearmar su metadata, de modo que `optimizar_para_campo()` (que
  lee `x$metadata$complejidad_linguistica`) volvia al default "intermedio" y
  re-alargaba los items que regeneraba. Ahora la compuerta/optimizacion heredan
  el mismo nivel con el que se genero la escala.

## Ficha demografica por defecto mas neutra

- `ensamblar()` cambio su `datos_solicitados` por defecto de
  `c("edad","sexo","nivel_educativo","n_hijos","edad_hijo")` a
  `c("edad","sexo","nivel_educativo")`. Los campos de crianza ("Numero de
  hijos/as", "Edad de su hijo/a en quien piensa al responder") eran restos de
  una plantilla de escala de crianza y aparecian por defecto en TODOS los
  cuestionarios exportados (incluido `exportar_proyecto()`), impropios para la
  mayoria de poblaciones (p. ej. cadetes de 17-24 anios). Siguen disponibles
  pasandolos explicitamente en `datos_solicitados`.


- Bugfix: `calificar_deseabilidad()` fallaba con escalas UNIDIMENSIONALES
  ("valor ausente donde TRUE/FALSE es necesario"): con K = 1 la DE entre
  dimensiones es NA y el diagnostico de halo intentaba usarla en un if.
  Ahora `uniforme` usa isTRUE() y el mensaje explica que el contraste entre
  dimensiones no aplica con una sola dimension. Detectado con RSES-10
  (OpenPsychometrics) en el lote del articulo de la prueba de estres.

# SeMiLLa 2.9.1 (2026-07-11)

## cargas_por_cohesion(): las cargas del generador en modo suelto

- Nueva funcion exportada `cargas_por_cohesion(similitud, dimension)`: expone
  de forma independiente las cargas que el motor usa con
  `carga_propia = "semantica"` en `simular_estructura()` y `estres_escala()`,
  sin necesidad de un objeto semilla ni de llamadas a la API. Acepta la matriz
  de similitud o directamente la matriz de embeddings (calcula el coseno).
  Devuelve tabla por item, LAMBDA, afinidades y numero de cargas cruzadas,
  con print propio.
- Documentacion con advertencia explicita: son un INSUMO DEL SIMULADOR
  (orden por cohesion en el rango .45-.75), NO una prediccion de cargas
  empiricas (cor similitud-policorica = .21 en la calibracion). No confundir
  con `cargas_semanticas()` (Jaccard clusters vs EFA), que es otra funcion.

## Veredicto de simular_estructura() acotado al fraseo

- El rotulo "ALTA (aplicar con confianza)" pasa a
  "ALTA (sin riesgo estructural inducido por el fraseo)": el pronostico cubre
  riesgo inducido por el TEXTO de los items (halo, parafrasis, estilos de
  respuesta) y no la covariacion sustantiva entre rasgos (caso DASS-21:
  pronostico ALTA correcto en su alcance, con factores empiricamente casi
  fundidos por comorbilidad, |Phi| = .81, que no vive en el fraseo).

# SeMiLLa 2.7.0 (2026-07-09)

## comparar_generadores(): benchmark reproducible de modelos LLM

- Nueva funcion `comparar_generadores()`: genera el mismo conjunto de items
  (constructo, dimensiones, reglas y poblacion identicos) con varios
  modelos LLM y los compara con metricas objetivas (separabilidad
  semantica intra/inter dimension, clasificacion leave-one-out por
  centroides, pares y facetas redundantes, muletillas, longitud, tiempo) y
  con DOBLE JUEZ LLM ciego y cruzado (cada juez evalua todos los conjuntos
  en orden aleatorio sin conocer al autor; jueces de familias distintas
  para mitigar el sesgo de auto-preferencia). Devuelve tabla comparativa +
  items por modelo + parametros del experimento (reportable en un
  articulo). Motivacion: decidir con datos si un modelo nuevo genera
  mejores items, en vez de asumir que mas nuevo = mejor.
- **Gotcha de sub-familias GPT-5** (`.normalizar_razonamiento()`): el
  "razonamiento apagado" cambia de nombre — gpt-5 clasico usa "minimal";
  gpt-5.1+ (5.2, 5.4-mini...) usa "none" y RECHAZA "minimal" (error 400);
  las o-series no tienen apagado (se degrada a "low"). El paquete lo
  normaliza automaticamente.

## Soporte GPT-5 (gpt-5-nano / gpt-5-mini) en todo el paquete

- Nuevo helper interno `.args_chat_modelo()`: TODA llamada de chat pasa por
  el, y construye el contrato correcto segun la familia del modelo. Los
  razonadores (GPT-5, o-series) rechazan `max_tokens` (se mapea a
  `max_completion_tokens`) y solo aceptan `temperature`/`top_p` default
  (se omiten). Con esto, `modelo = "gpt-5-nano"` ($0.05/$0.40 por millon,
  el mas barato) funciona en cualquier funcion del paquete.
- **Gotcha critico resuelto**: sin `reasoning_effort = "minimal"`, gpt-5-nano
  consume TODO el presupuesto de tokens en razonamiento interno y devuelve
  contenido VACIO (verificado: 200/200 tokens a razonamiento, respuesta "").
  El default del paquete es "minimal" (opcion global
  `SeMiLLa.reasoning_effort`).
- **Razonamiento por tipo de tarea**: para GENERACION de items/JSON,
  "minimal" es suficiente y mas barato (probado: items de buena calidad);
  para JUICIOS evaluativos se fuerza "low" (calificar_deseabilidad, jueces
  de validez de contenido, auditoria de redaccion, examinado de
  verificar_clave). Evidencia: con minimal, nano califico un item
  claramente indeseable con deseabilidad 0.68-0.95 e inestabilidad r=.70;
  con "low" lo corrigio a 0.12, igual que gpt-5-mini (0.05) y gpt-4.1-mini
  (0.10), con estabilidad r=1.00.
- Validado en vivo con gpt-5-nano: .llamar_openai + parseo JSON, generacion
  de items, calificar_deseabilidad, y retrocompatibilidad con gpt-4.1-mini.

## Mapa de fusion + hipotesis B pre-registrable (tercer caso real: VP)

La escala de Valores Positivos (32 items, 4F de Schwartz) de la misma
bateria mostro el caso INTERMEDIO: colapso parcial y selectivo — los 3
factores prosociales se fundieron (Phi=.83-.89) pero Apertura al cambio se
separo (Phi=.46-.59); el analisis empirico termino en 2 factores con 15 de
32 items. Tres capacidades nuevas:

- `simular_estructura()` ahora devuelve **`phi_pares`** (matriz |Phi|
  simulada POR PAR de dimensiones, escenario central) y **`mapa_fusion`**:
  componentes conexos de los pares con |Phi| >= umbral_phi, es decir, QUE
  dimensiones se fundiran entre si y cuales sobreviviran (no solo si "algo"
  colapsa). Se imprime como "[SE FUNDEN] A + B + C / [se separa] D".
- `compuerta_pre_aplicacion()` convierte el mapa en una **HIPOTESIS B
  PRE-REGISTRABLE** (`$estructura_alternativa`): la estructura empirica
  esperable con sus dimensiones e items por factor, y la accion "declarar
  el modelo B y contrastarlo como rival del teorico" — el investigador va a
  campo con ambos modelos declarados en vez de descubrir el B a posteriori.
- Las acciones de halo ahora conectan con el modulo que YA existe en el
  paquete: **`generar_escala_forcedchoice()`**. En constructos de
  valores/virtudes (todos los items deseables por definicion) la
  reescritura no basta; el formato comparativo es la salida estandar
  (Schwartz PVQ).

## Calibracion bidireccional con el segundo caso real (ACO, n=280)

La escala de Actitud hacia la Corrupcion (24 items, tripartita) de la misma
bateria policial funciono en campo (3F integrada de 16 items: CFI=.941,
RMSEA=.075) y sirvio como control de ESPECIFICIDAD: la compuerta no debe
bloquear escalas sanas. Cuatro ajustes:

- `compuerta_pre_aplicacion()` ahora pasa `"auto"` a la auditoria de
  redundancia (sus defaults numericos pisaban el umbral adaptativo).
- `.detectar_facetas_repetidas()` distingue DIMENSION TEMATICA LEGITIMA de
  faceta repetida: un cluster que abarca >= 75% de una dimension teorica
  cohesiona porque debe (en ACO los 8 items cognitivos "pienso que la
  corrupcion..." cohesionaban a .65 y funcionaron) y solo se marca si
  ademas comparte plantilla fuerte (sim media >= .70; la plantilla afectiva
  "siento X" llego a .70 y la poda empirica retiro 4 de sus 8 items). Los
  clusters PARCIALES siguen marcandose ("regalos evito aceptar", sim .785:
  la poda empirica retiro 2 de esos 3 items).
- `calificar_deseabilidad(umbral_uniforme)` baja de 0.10 a **0.05**: PM
  colapso con DE entre dimensiones = .031 (Phi=.92) y ACO se separo con
  DE = .059-.077 (Phi=.64-.84); con 0.10 ambas daban "uniforme".
- `simular_estructura(umbral_phi)` sube de 0.50 a **0.70**: factores
  correlacionados .50-.70 son comunes y discriminables; ACO funciono con
  Phi simulado = .58 que el criterio 0.50 marcaba como colapso. PM
  (Phi simulado = .71) sigue correctamente marcada.
- El semaforo de redaccion de la compuerta distingue **facetas FUERTES**
  (sim media >= .65 o 4+ items -> riesgo, bloquea) de **facetas debiles y
  pares sueltos** (-> advertencia, no bloquea): ACO-16 funciono en campo
  con 6 pares de similitud .71-.77 y un cluster debil de 3 items, mientras
  que todos los bloques que si danaron (PM "companeros" 7 items, plantilla
  afectiva ACO .70) cumplen el criterio fuerte. Ademas el cluster cuyo
  nucleo lexico es el OBJETO de la escala entera (presente en >50% de todos
  los items) no se marca salvo plantilla fuerte.
- Validacion final de los tres escenarios: ACO-24 -> NO APLICAR TODAVIA
  (podar los gemelos: lo que la spec search empirica hizo), ACO-16 ->
  APLICAR CON CAUTELA (lo que empiricamente fue), PM-16 -> NO APLICAR
  TODAVIA (halo + facetas fuertes: lo que empiricamente colapso).

## optimizar_para_campo(): la compuerta ya no solo avisa, corrige

- Nueva funcion `optimizar_para_campo()`: bucle de correccion automatica
  guiado por la compuerta. En cada iteracion (a) PODA los clusters de
  faceta repetida conservando los 2 items mas prototipicos, (b) poda el
  miembro mas redundante de cada par, (c) REGENERA los items podados con
  dos restricciones inyectadas al prompt: conducta del cluster PROHIBIDA
  (nucleo lexico + sinonimos) y, si hubo riesgo de halo, exigencia de
  conducta especifica con costo personal real (no autoatribuciones
  halagadoras), (d) verifica redundancia del reemplazo contra TODA la
  escala, recalcula embeddings y RE-PASA la compuerta completa. Termina al
  alcanzar el `veredicto_objetivo` o agotar iteraciones, devolviendo
  historial (`x$optimizacion$historial`) y trazabilidad item viejo -> item
  nuevo con motivo (`x$optimizacion$reemplazos`).
- `semilla()` la dispara automaticamente cuando la compuerta devuelve
  "NO APLICAR TODAVIA" (parametros `optimizar = TRUE`,
  `max_iteraciones_optimizar = 2`), como paso [5b/5].
- `.generar_items_dimension()` gana el parametro `instruccion_extra` (canal
  para restricciones de correccion sin tocar las reglas base).
- Las conductas vetadas se ACUMULAN entre iteraciones y se exige variacion
  sintactica en los reemplazos: sin esa memoria, los reemplazos de una tanda
  derivan hacia una muletilla nueva comun (validado con los items PM: la
  iteracion 1 elimino "ayudo a companeros" y creo "dedico tiempo a
  corregir"; la iteracion 2 la detecto y podo, pero sin acumulacion el bucle
  persigue facetas moviles).
- **Deteccion literal de MULETILLAS** (`.detectar_muletillas`): n-gramas de
  2-4 palabras compartidos por 3+ items ("aunque me incomode", "dedico
  tiempo a") Y verbos iniciales repetidos en 3+ items ("Elijo...",
  "Uso...") entran al plan de reemplazo como hallazgo propio, ademas de
  las facetas semanticas, y la formula queda vetada para los reemplazos
  (que ademas se RECHAZAN si la reintroducen, incluido empezar con el
  verbo vetado).
- **Iteraciones EXTRA automaticas** (`max_iteraciones_extra`, default 2): si
  al agotar las iteraciones planificadas quedan facetas o muletillas
  corregibles, el bucle continua en vez de detenerse con trabajo de
  redaccion pendiente; si el bloqueo restante no es de redaccion (formato,
  deseabilidad estructural), se detiene y lo informa.
- **Homogeneidad de longitud** (`.rango_palabras_escala`): los reemplazos
  reciben un rango obligatorio de palabras (mediana de la escala -3/+4,
  acotado a 6-16) para que no destaquen por largos frente a los items
  conservados (DeVellis: evitar items excepcionalmente largos; la
  especificidad anti-halo debe lograrse gastando palabras en situacion y
  conducta, no en apendices).
- Limite honesto: si el unico riesgo restante no es corregible reescribiendo
  items (p. ej. decision de formato de respuesta, anclas de frecuencia,
  rediseno de dimensiones), el bucle se detiene y lo dice en vez de iterar
  a ciegas.
- **Robustez del juicio de deseabilidad en la compuerta**: si la
  estabilidad entre pasadas del LLM es baja (r < .70), se recalifica
  automaticamente con 4 pasadas antes de emitir el diagnostico de halo
  (observado en PM2: con r = .41 el mismo conjunto de items recibia
  "contrastante" o "halo" segun la corrida, oscilando alrededor del umbral).
- **Correccion por TIPO de dimension** (`.tipo_dimension()`): la
  restriccion anti-halo del optimizador respeta la naturaleza de la
  dimension — en cognitivas exige creencias con matiz discutible (PROHIBIDO
  convertirlas en conductas), en afectivas emociones diferenciadas, y solo
  en conductuales/generales la conducta con costo. Ademas, los clusters de
  faceta INTRA-dimension cognitiva/afectiva solo se podan con plantilla
  fuerte (sim >= .70): las creencias/emociones sobre el mismo objeto
  cohesionan de forma legitima (piloto ACO: cognitivos cohesivos, omega=.72).
  Bug detectado al regenerar ACO2: el optimizador transformo creencias
  buenas ("Enterarme de favoritismos reduce mi confianza...") en conductas
  ("dedico tiempo a recopilar evidencia...").
- **Replicas ADAPTATIVAS** (`n_rep_intermedio = 40`): las compuertas
  intermedias del bucle usan 40 replicas Monte Carlo (solo orientan la
  correccion) y el veredicto final se re-estima con las `n_rep` completas
  (100). Motivacion medida: la simulacion local (300 CFAs ordinales por
  compuerta) domina el tiempo total — generar PM2/ACO2/VP2 tomo 16/38/60+
  minutos con 4-5 compuertas completas cada una; el modo adaptativo recorta
  cerca de la mitad sin perder rigor en el veredicto reportado.

## compuerta_pre_aplicacion(): ninguna escala va a campo sin auditarse

- Nueva funcion `compuerta_pre_aplicacion()`: encadena las tres auditorias
  pre-campo — (1) `auditar_redundancia()` (pares + facetas repetidas),
  (2) `calificar_deseabilidad()` (halo entre/dentro de dimensiones),
  (3) `simular_estructura()` (probabilidad de estructura limpia) — y emite
  un VEREDICTO unico con semaforo por paso y acciones concretas, en
  4 niveles: **LISTA PARA CAMPO / APLICAR CON CAUTELA / APLICAR COMO
  ESCALA GLOBAL / NO APLICAR TODAVIA**.
- El veredicto **"APLICAR COMO ESCALA GLOBAL"** distingue el caso PM: si la
  redaccion ya esta limpia, el ajuste simulado es aceptable (RMSEA <= .08)
  y el unico riesgo restante es que las dimensiones no se separen (Phi alto
  por deseabilidad uniforme u otra causa estructural), la escala ES
  aplicable puntuando el TOTAL (empiricamente en PM: bifactor con ECV=.84 y
  omegaH=.92 avalaron el puntaje unico); "NO APLICAR" seria un falso
  bloqueo. Las acciones indican puntuar total / bifactor-G y reservan la
  separacion de facetas para una decision de diseno (deseabilidad
  contrastante, anclas, formato ipsativo).
- `semilla()` la ejecuta AUTOMATICAMENTE como paso [5/5] del pipeline
  (nuevo parametro `compuerta = TRUE`; con `FALSE` se omite y el print del
  objeto lo recuerda como "pendiente"). El veredicto queda en
  `x$compuerta` y se muestra en el resumen COMPLETADO y en `print(x)`.
- Si un paso falla (p. ej. sin lavaan o sin credito LLM) la compuerta
  degrada a "advertencia: no evaluada" en ese paso en vez de romper el
  pipeline.
- `flujo()` documenta la nueva FASE V-B (Paso 15b) y el flujo minimo pasa
  de 8 a 9 pasos.
- Motivacion: caso PM policial (n=280) — los tres mecanismos que arruinaron
  esa aplicacion eran detectables antes de ir a campo, pero estaban en
  funciones sueltas que habia que acordarse de llamar.

## Anti-redundancia recalibrado con datos reales (caso PM policial, n=280)

Una escala construida con SeMiLLa (Personalidad Moral, 16 items) llego a
aplicacion con 8 pares de items "gemelos" (policorica >= .70) que el filtro
de redundancia NO detecto, causando dependencia local (RMSEA=.122), factores
fundidos (Phi=.92) y fiabilidad inflada. La autopsia con embeddings mostro
tres fallas, todas corregidas:

- **Umbral ADAPTATIVO** (`umbral_sem = "auto"`, default) en
  `analizar_redundancia()` y `auditar_redundancia()`: cuantil .95 de la
  distribucion de similitudes de la propia escala, acotado a [0.70, 0.85]
  (facetas: cuantil .75 acotado a [0.55, 0.70]). La calibracion con DOS
  escalas de la misma bateria (n=280) mostro que ningun umbral fijo sirve:
  en PM (linea base media=.49) las parafrasis daninas vivian en .56-.78 y
  el antiguo 0.85 capturo 0 de 8 pares gemelos; en ACO (actitud hacia UN
  objeto nombrado en cada item, linea base alta) un 0.70 fijo marcaba 28
  pares en una escala que empiricamente FUNCIONO, mientras que los pares
  altos en terminos RELATIVOS (>= ~.78) coincidian con los items que la
  specification search empirica descarto. En `refinar_escala()`,
  `generar_banco_cat()` y el verificador por item (sin matriz completa
  disponible) el default queda en 0.70.
- **`auditar_redundancia()` ahora detecta FACETAS REPETIDAS**: clusters de 3+
  items que parafrasean la misma conducta (clustering jerarquico sobre
  1-similitud + nucleo lexico compartido). El chequeo por pares no ve este
  patron, que es el mas danino: en PM, 7 items de "ayudo a companeros"
  pasaban de a pares y formaron un bloque de dependencia local masiva.
  Validacion retrospectiva: la nueva auditoria encuentra exactamente los 2
  clusters culpables (7 items "companeros", 5 items "siento que ser X") y
  recomienda conservar 1-2 por cluster — la misma solucion que el analisis
  empirico tardo 8 experimentos en derivar. Con parametros v2.6: 0 hallazgos.
- **Comparacion inter-dimension** en `refinar_escala()`: el filtro solo
  comparaba items de la misma dimension; los 2 pares con mayor correlacion
  empirica (r >= .76) cruzaban dimensiones y jamas se compararon.
- **Reglas de DIVERSIDAD DE CONTENIDO en la generacion**: el prompt de
  `.generar_items_dimension()` ahora prohibe explicitamente repetir el par
  verbo+objeto (sinonimos incluidos) y exige que cada item cubra una
  manifestacion distinta (conducta/contexto/destinatario), con
  auto-revision del conjunto antes de responder.
- Limite documentado y honesto: la similitud semantica NO captura la
  correlacion inducida por deseabilidad social uniforme (en PM, el par con
  r=.77 tenia similitud 0.42; correlacion global similitud-policorica = .21).
  Para ese mecanismo estan `calificar_deseabilidad()` y
  `simular_estructura()` (v2.6.0), que ahora la auditoria recomienda como
  compuerta obligatoria pre-aplicacion.

## verificar_clave(): auditoria de la clave con un examinado LLM

- Nueva funcion `verificar_clave()`: un LLM independiente RESUELVE cada item
  de una `semilla_prueba_objetiva` sin ver la clave y su respuesta se compara
  con la declarada. Si el examinado no llega a la clave, el item puede tener
  clave equivocada, mas de una opcion defendible o enunciado ambiguo
  (viola la directriz de "una sola correcta" de Moreno et al., 2004).
- Cubre los seis formatos (usual, alternativa, V/F, V/F multiple,
  emparejamiento con el MISMO barajado que ve el respondiente, y
  contexto-dependiente con su texto base).
- `n_resoluciones > 1` resuelve varias veces (1ra pasada determinista,
  siguientes con temperatura) y decide por voto mayoritario.
- El resultado queda en `x$verificacion` (con print propio que lista
  discrepancias y razonamiento del LLM) y en `metadata$verificacion`;
  `print()` de la prueba ahora muestra la tasa de coincidencia o
  "pendiente" si aun no se verifico.

## ensamblar_prueba_html(): version docente y version alumno

- Nueva funcion `ensamblar_prueba_html()`: exporta la prueba objetiva como
  dos HTML autocontenidos e imprimibles (Ctrl+P / guardar como PDF):
  `_docente.html` (clave marcada en verde, metadatos tema/Bloom/formato,
  tabla de claves en pagina aparte) y `_alumno.html` (limpio, con bloque
  demografico opcional).
- Acepta `ilustraciones = "carpeta"` con `item_XX.png`: las imagenes se
  incrustan en base64, el HTML viaja completo en un solo archivo.
- Si la prueba paso por `verificar_clave()`, la version docente resalta
  los items con discrepancia para revision.

## detectar_necesidad_ilustracion(): que items ganan con imagen

- Nueva funcion `detectar_necesidad_ilustracion()`: heuristica lexica en
  espanol (costo cero, sin LLM) que marca los items que describen una
  escena DIBUJABLE: accion observable + escenario y/o interlocutor, o
  referencia visual explicita. Los items introspectivos sin accion
  observable se excluyen (la imagen no puede mostrar contenido mental).
- Cada decision es trazable (`criterio` y `gatillos` por item). Acepta
  objetos `semilla`, `semilla_items`, data.frames y tambien
  `semilla_prueba_objetiva` (analiza los enunciados).

## prompts_ilustracion(): proteccion de la clave en pruebas objetivas

- `prompts_ilustracion()` ahora acepta `semilla_prueba_objetiva`. En ese
  caso la escena se genera SOLO desde el enunciado (sin resolver el item)
  y el prompt final incorpora reglas de proteccion: la imagen no debe
  representar ni sugerir la respuesta correcta, sin letras a/b/c/d, sin
  textos de opciones ni palabras que sirvan de pista.

## usar_proveedor(): generacion de texto mas alla de OpenAI

- Nueva funcion `usar_proveedor()`: redirige el chat a cualquier endpoint
  OpenAI-compatible con presets para Groq, router de Hugging Face y
  Ollama local, o `base_url` personalizada. La api_key pasa a ser la del
  proveedor activo; prompts, cache y seed no cambian.
- `.configurar_openai()` ademas infiere el proveedor por el nombre del
  modelo (`"org/modelo"` -> HF router; `llama|mixtral|gemma|qwen|deepseek`
  -> Groq) cuando no se configuro nada, avisando con un message.
- Los embeddings siguen en OpenAI o en modelos locales
  (`modelos_embeddings_libres()`); Groq/Ollama no sirven text-embedding-*.

# SeMiLLa 2.6.0 (2026-07-08)

## calificar_deseabilidad(): fiabilidad, barajado y fallo explicito

- Las calificaciones del LLM ahora se promedian sobre **`n_pasadas`**
  (default 2) pasadas independientes y se reporta la correlacion media entre
  pasadas como `estabilidad` (con aviso si r < .70).
- Los items se **barajan** antes de formar los lotes de 6: los lotes
  secuenciales coincidian con las dimensiones y la deriva de escala entre
  lotes sesgaba `sd_entre_dim` (el numero del diagnostico de halo).
- Si mas de **`max_imputados`** (default 25%) de los items quedan sin
  calificar, la funcion **aborta con error** en vez de imputar todo a 0.5
  (antes un fallo de API producia una simulacion sin factor de deseabilidad).
- Nuevo diagnostico de contraste **intra-dimension** (`sd_intra_dim`,
  `alerta_intra`): deseabilidad mixta dentro de un factor genera varianza de
  metodo que lo fragmenta; el contraste favorable es ENTRE dimensiones.
- Al prompt va solo la **definicion** del constructo (antes se inyectaba la
  lista anidada deparseada).

## simular_estructura(): sensibilidad, casos borde y replicas honestas

- **`fuerza_deseabilidad` acepta un vector** y por defecto simula la grilla
  `c(0.3, 0.6, 0.9)`: el veredicto usa el escenario central y `$sensibilidad`
  reporta el rango completo (la fuerza real es un supuesto, no un dato; con
  una escala real el veredicto pasaba de 48% a 0% segun este parametro).
- **Fix**: con escalas **unidimensionales** la funcion abortaba con "valor
  ausente donde TRUE/FALSE es necesario" (|Phi| = NaN); ahora el criterio se
  reduce al RMSEA.
- Las replicas **no convergidas o inadmisibles** (Heywood, matrices no PD via
  `post.check`) ahora cuentan como estructura NO limpia en vez de descartarse
  (descartarlas sesgaba la probabilidad al alza). Se reportan
  `tasa_no_conv` y `tasa_inadmisible` por escenario.
- `n_rep` sube de 40 a **100** y se reporta **`prob_ic`** (IC 95% binomial):
  con 40 replicas el semaforo cambiaba de color por ruido de +-8 puntos.
- El indice latente se **estandariza analiticamente** (los umbrales ordinales
  operan en la metrica prevista) y se reporta
  `carga_estandarizada_media` (la carga efectiva del rasgo tras sumar los
  componentes de metodo es menor que la nominal).
- Salida retrocompatible: `prob_limpia`, `veredicto`, `rmsea_med`, `phi_med`
  y `resumen` se mantienen (ahora referidos al escenario central).

# SeMiLLa 2.5.0 (2026-06-25)

## plot_consenso(): lollipop de consenso del ensemble por item

- Nueva funcion **`plot_consenso()`** que dibuja un grafico lollipop con el
  grado de consenso (0-1) de cada item dentro de su dimension teorica, segun
  el ensemble de clustering. Los items por debajo de `umbral_consenso`
  (default 0.667) se resaltan en rojo (candidatos a refinamiento). Es la
  version "por item" de `plot_sankey()` (flujo global). La misma funcion
  sirve para la escala SIN refinar y para la REFINADA: solo cambia el objeto
  de precision que se le pasa. Acepta un objeto `semilla_precision`
  (metodo = "ensemble") o un `data.frame` con columnas `Dimension` y
  `Consenso`.
- `exportar_proyecto()` ahora genera automaticamente dos figuras nuevas:
  `graficos/14_consenso_sinrefinar.png` y `graficos/15_consenso_refinado.png`
  (helper interno `.fig_consenso_de()`).

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
