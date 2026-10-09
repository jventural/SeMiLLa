# SeMiLLa: mejoras para el post (octubre de 2026)

Registro de lo que cambia entre la version anterior (librería 2.9.37 y app en
Connect Cloud) y la version nueva (librería 2.12.0 en adelante y app en el
VPS, semilla.joseventuraleon.com). Cada punto lleva el dato medido que lo
respalda. Este archivo no viaja en el paquete (esta en `.Rbuildignore`).

## 1. Modelos mas baratos por OpenRouter (librería 2.12.0 y app)

- Una sola clave de OpenRouter (`sk-or-...`) sirve para todo: el texto y los
  embeddings. Los embeddings son los mismos de antes
  (`openai/text-embedding-3-small`), asi que los umbrales ya calibrados
  siguen valiendo.
- GPT-6 Luna genera los items y Claude Haiku 5.5 los juzga. Medido el
  9-oct-2026 con una escala de 18 items y 3 dimensiones (generar, embeddings,
  agrupamiento, deseabilidad y 10 jueces, 2 corridas): con gpt-4.1-mini
  costaba US$ 0,017 y ahora US$ 0,010 (40 % menos), con V de Aiken
  equivalentes (0,84 frente a 0,83). Todo con Luna costaria US$ 0,0055, pero
  Luna como juez es mas severo (V = 0,76).
- La librería reconoce sola una clave `sk-or-`: basta pasarla como `api_key`.
- En la app, cada usuario pega su propia clave de OpenRouter en el Paso 1; la
  app la valida sin gastar saldo y muestra el saldo disponible.
- Corregido: la deseabilidad y los jueces en paralelo ignoraban el proveedor
  elegido y llamaban siempre a OpenAI.

## 2. Probabilidad de pertenencia de cada item (en curso)

Decision del 9-oct-2026:

- La probabilidad de pertenencia que ve el usuario pasaria a salir de Jev +
  Haiku. Es mas precisa y Jev es casi perfectamente estable.
- El consenso de clustering se queda como segunda lectura, igual que en
  ScanQuiz. Mide otra cosa, la cercania del significado entre items, y es
  independiente de los modelos de lenguaje. Por eso sirve para detectar
  cuando el modelo y los embeddings discrepan. Esos items se marcan para
  revisar, y no se promedian.
- Un punto a favor: si Luna genera los items y Jev y Haiku los clasifican,
  quien juzga no es el mismo modelo que escribio. Eso reduce la circularidad
  que el articulo ya declara.

Lo que lo respalda (9 escalas reales de OpenPsychometrics, 306 items, 1500
personas cada una; verdad = el item queda, en los datos reales, junto a los
de su dimension teorica):

| Metodo | Acierta la estructura real (AMI) | Predice que items pertenecen (AUC) | Error de la probabilidad (Brier) |
|---|---|---|---|
| Consenso de clustering (antes) | 0,50 | 0,61 | 0,204 |
| Jev | 0,67 | 0,71 | 0,144 |
| Haiku | 0,75 | 0,74 | 0,123 |
| Jev + Haiku (ahora) | | 0,77 | 0,125 |

Ganancia de Jev + Haiku sobre el clustering: +0,20 de AUC (IC 95 %: 0,08 a
0,32). Jev da la misma respuesta en dos corridas (r = 0,998). Costo de leer
las 9 escalas: Jev US$ 0,002, Haiku US$ 0,01. Cautela: solo 27 de 306 items
"no pertenecen", asi que los margenes son amplios.

## 3. Correlacion entre factores estimada (se informa, no decide)

ACTUALIZACION del mismo dia (ver la seccion 9): la estimacion por par se
acerca mas a la correlacion real, pero usarla en la simulacion empeoro el
veredicto de la compuerta frente a la realidad. Por eso se muestra como
informacion y el veredicto se calcula con el supuesto de siempre.

Antes, el motor de simulacion suponia la misma correlacion (phi = 0,47) para
todos los pares de dimensiones; lo que devolvia era ese supuesto con ruido.
Ahora Jev y Haiku estiman cada par a partir de los nombres de las dos
dimensiones y algunos items de cada una.

Medido el 9-oct-2026 contra la correlacion REAL de 72 pares de dimensiones
(AFC sobre las respuestas de 1500 personas en 9 escalas; |phi| medio real
0,39, DE 0,27):

| Metodo | Error medio | Correlacion con el phi real |
|---|---|---|
| Constante 0,47 (antes) | 0,248 | no aplica |
| Jev | 0,154 | 0,79 |
| Haiku | 0,130 | 0,79 |
| Jev + Haiku (ahora) | 0,135 | 0,83 |

El error baja casi a la mitad. Jev da la misma respuesta en dos corridas
(r = 0,998). Estimar los 72 pares costo menos de un centavo.

## 4. El juez de deseabilidad ya no "salta" (compuerta reproducible)

ACTUALIZACION del mismo dia (ver la seccion 9): Jev es el juez mas estable,
pero NO quedo como juez por defecto de la compuerta, porque frente a la
realidad acerto peor que Haiku. El juez por defecto es Haiku, que ya es
estable (0,97 a 0,99 entre corridas). Lo de abajo sigue siendo cierto como
medicion de estabilidad.

En agosto de 2026, repetir la compuerta sobre la misma escala daba
probabilidades de estructura limpia de 0,87 y luego 0,20: el juez de
deseabilidad (gpt-4.1-mini) calificaba el mismo item con 0,20 o con 0,60 en
llamadas identicas, y el simulador amplificaba ese ruido.

Ahora, con una clave de OpenRouter, el juez es Jev, un modelo de decision: da
la probabilidad de cada nivel de una escala de 7 anclas y se usa el valor
esperado. Medido el 9-oct-2026 en 306 items reales, cada juez corrido dos
veces:

| Juez | Correlacion entre dos corridas | Diferencia media | Diferencia maxima en un item |
|---|---|---|---|
| Haiku (chat, 4 pasadas) | 0,990 | 0,027 | 0,150 |
| Jev | 1,000 | 0,005 | 0,021 |

Jev coincide con Haiku en que items son deseables (r = 0,957); solo deja de
variar.

La compuerta repetida dos veces sobre la misma escala:

| | Probabilidad de estructura limpia | Correlacion entre dimensiones | Estabilidad del juez |
|---|---|---|---|
| Agosto (gpt-4.1-mini) | 0,87 y 0,20 | 0,50 fija | 0,46 a 0,90 |
| Octubre, juez Haiku | 0,06 y 0,04 | 0,50 fija | 0,97 a 0,99 |
| Octubre, Jev + correlacion por par | 0,01 y 0,01 | 0,57 a 0,66 estimada | 0,999 |

Parte de la mejora ya venia del cambio a Haiku; Jev la deja fija.

## 5. V de Aiken con segunda lectura

La V de Aiken sigue saliendo del panel simulado de jueces (ahora Haiku). Con una
clave de OpenRouter, Jev agrega una segunda lectura por item, y se marcan los
items en que las dos se separan mas de 0,30 de V.

Medido el 9-oct-2026 con 168 juicios (84 items reales, cada uno con su
dimension y con una equivocada; dos corridas):

| Juez | Separa relevante de no relevante (AUC) | Correlacion entre corridas | Mayor salto de V en un item | Costo |
|---|---|---|---|---|
| Panel de Haiku | 0,92 | 0,93 | 0,77 | US$ 0,044 |
| Jev | 0,85 | 0,998 | 0,05 | US$ 0,0007 |

El panel distingue mejor pero puede cambiar mucho la V de un item entre
corridas; Jev casi no cambia. Por eso Jev no reemplaza al panel: avisa cuando
el panel dio una V que no se sostiene.

## 6. Lo que se probo y NO se cambio

Se incorpora solo lo que mide mejor que lo que habia.

- Direccion del item (directo o inverso): en 113 items reales con sus 33
  inversos documentados (SD3, ECR y BigFive), Jev y Haiku acertaron el 100 %
  en dos corridas. Jev no mejora: se queda Haiku.
- Items gemelos: con 84 items reales y 15 parafrasis plantadas, Jev no separa
  los gemelos de los demas pares (todos le salen entre 0,5 y 0,8). Se queda
  Haiku, que en una pasada encontro 13 y 8 de 15; SeMiLLa ya vota con 3
  pasadas para compensar esa variacion.

## 7. Cuanto cuesta construir una prueba completa

Medido el 9-oct-2026 con el gasto real de la clave de OpenRouter, recorriendo
los pasos de "Construir" de la app salvo el banco de items: generar,
embeddings, compuerta, estructura con refinamiento, V de Aiken, auditoria de
redaccion, escala de respuesta y ensamblado. Escala de 24 items (4
dimensiones x 6), una corrida por configuracion:

| Configuracion | Costo total | Tiempo | Items reescritos | V de Aiken media |
|---|---|---|---|---|
| Antes (gpt-4.1-mini, sin Jev) | US$ 0,071 | 14 min | 6 | 0,79 |
| Ahora (Luna + Haiku + Jev) | US$ 0,020 | 13 min | 0 | 0,83 |

Construir una prueba lista para aplicar cuesta unos 2 centavos de dolar
(unos 7 centimos de sol): 72 % menos que antes. Cien pruebas, unos US$ 2.
La mayor parte del ahorro viene de la estructura: antes el refinamiento
perseguia al clustering y reescribio 6 items (US$ 0,037); ahora Jev y Haiku
vieron los 24 items en su dimension y no hizo falta reescribir.

## 8. Probado en todos los caminos de la app

Con una clave de OpenRouter, el 9-oct-2026 funcionaron, sin configurar nada
mas: escalas de historias, Guttman, prueba objetiva y forced choice; el
camino Validar sobre una escala real (DASS-21: embeddings, estructura con
pertenencia y compuerta con correlacion estimada por par); y la adaptacion
transcultural al portugues. En la app se comprobo que la clave de OpenRouter
se valida sin gastar saldo, muestra el saldo disponible, rechaza una clave
falsa y que, con una clave de OpenAI, el modelo pasa solo a gpt-5-mini.

Las exportaciones (Excel de la app y exportar_proyecto()) traen ahora la
pertenencia de cada item y la correlacion estimada por par.

## 9. La compuerta frente a la realidad: lo que se mantuvo igual

Se repitio el estudio de agosto con las mismas 9 escalas (AFC real con
n = 1500 como verdad, misma semilla y 30 replicas) para ver si los nuevos
jueces mejoraban el veredicto de la compuerta:

| Juez de deseabilidad + correlacion usada | AUC | Aciertos con el umbral 0,80 |
|---|---|---|
| Agosto: juez anterior + 0,47 | 0,65 | 6 de 9 |
| Haiku + 0,47 (queda por defecto) | 0,575 | 5 de 9 |
| Jev + 0,47 | 0,525 | 4 de 9 |
| Jev + correlacion estimada | 0,375 | 3 de 9 |

Con correlaciones estimadas mas bajas, la simulacion aprobaba escalas que en
campo fallan (ECR, DD, RIASEC): el error caro. Por eso la correlacion
estimada se informa pero no decide, y la compuerta usa a Haiku como juez.
Con 9 escalas, la diferencia entre 0,65 y 0,575 es una sola escala; la
compuerta sigue siendo una orientacion previa, no un pronostico.
