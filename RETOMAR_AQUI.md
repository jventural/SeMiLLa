# SeMiLLa: retomar aqui

Ultima actualizacion: 2026-10-08. Este archivo no viaja en el paquete
(esta en `.Rbuildignore`).

## Donde quedo

- Rama de trabajo: `correcciones-2.9.38`. `main` sigue en la 2.9.37.
- Version en la rama: **2.11.0**, lista para CRAN pero **todavia no enviada**.
- `R CMD check --as-cran` completo (ejemplos, `--run-donttest`, tests y
  manual PDF/HTML): **0 errores, 0 warnings, 2 NOTE**. Las NOTE son "New
  submission" y "unable to verify current time" (esta ultima solo en local).
- Nada se ha subido a GitHub (sin push) ni a CRAN ni a win-builder.

| Commit | Version | Que trae |
|---|---|---|
| `6d7f084` | 2.9.38 | Bloque 1: seis salidas que cambiaban resultados sin avisar |
| `7bf3a97` | 2.10.0 | Bloque 2: nombres que dicen lo que se mide |
| (sin commit) | 2.11.0 | Bloque 3: preparacion para CRAN |

El detalle de cada bloque esta en `NEWS.md`.

## Siguientes pasos, en orden

1. **Commit del bloque 3** (sin Co-Authored-By; sin push salvo que se pida).
2. **Corrida real corta con la API**, por ejemplo
   `validez_contenido(semilla_demo, api_key = Sys.getenv("OPENAI_API_KEY"))`.
   Los bloques 1-3 se probaron sin API (respuestas simuladas); falta ver una
   respuesta real pasar por `.llamar_openai()`, los jueces sin relleno y el
   registro del modelo (`$metadata$modelos_usados`).
3. **Win-builder r-devel** con el tarball final:
   `R CMD build .` y luego
   `curl -T SeMiLLa_2.11.0.tar.gz ftp://win-builder.r-project.org/R-devel/`.
   Meta: `Status: 1 NOTE`. El resultado llega al correo del maintainer.
4. Revisar `cran-comments.md` (falta la linea de win-builder) y **enviar** con
   la skill `submit-r-package-cran` (incluye confirmar el enlace del correo).
5. Unir la rama con `main` cuando el envio este hecho.

## Pendientes conocidos (no bloquean CRAN)

**En la app (`D:/16_Shinys/SeMiLLa_App` y `SeMiLLa_App v2`):**
- Al vaciar la cache el texto ahora sale como `message()`: el panel que lo
  mostraba con `capture.output()` queda en blanco (la cache si se vacia).
- La galeria del paso 23 llama a `plot_scree()` y `plot_cargas()`, que desde
  la 2.10.0 estan obsoletas: `plot_scree()` avisa y `plot_cargas()` se detiene
  con un objeto `semilla`. Quitarlas de la galeria o envolverlas en `tryCatch`.
- `$efa` sigue escrito como alias de `$separabilidad`; la app puede seguir
  leyendo `$efa` hasta la 3.0.
- Instalar la 2.11.0 en el VPS y en Connect Cloud solo despues de revisar lo
  anterior.

**En el paquete:**
- `paralelo_llm.R` ignora `base_url` (con Groq u Ollama las llamadas en
  paralelo fallan en silencio).
- Los embeddings se configuran con el mismo cliente que el chat: con
  `usar_proveedor("groq")` se van a Groq.
- `.auditar_longitud` tiene un prompt fijo que menciona "pareja" y nivel
  "secundaria incompleta" para cualquier constructo.
- `.detectar_item_inverso` usa vocabulario propio de apego.
- La circularidad del refinamiento solo se declara (`$nota_circularidad`); no
  se mide con un criterio independiente (otro modelo de embeddings o items
  reservados).
- Solo se centralizaron los pesos del puntaje (`options(SeMiLLa.pesos_score)`);
  el resto de umbrales sigue repartido por el codigo.
- El umbral de redundancia es relativo: percentil 95 acotado a .62-.70.

## Cosas que conviene recordar

- **NAMESPACE manual.** No tiene cabecera de roxygen. Regenerar solo la
  documentacion con `roxygen2::roxygenise(roclets = "rd")`; los exports e
  imports se editan a mano. `man/informe_compuerta.Rd` y `man/retirar_items.Rd`
  tambien son manuales.
- **Semillas.** Ningun `seed` tiene default numerico (politica CRAN). Los tres
  bucles que comparan versiones sortean una semilla al inicio y la devuelven
  (`$seed`, `$optimizacion$seed`, `$parametros$seed`).
- **`semilla_demo`** se reconstruye con `Rscript data-raw/semilla_demo.R`. Sus
  embeddings son sinteticos: sirve para ejemplos y tests, no para conclusiones.
- **Tests sin API**: `tests/testthat/test-regresion-2938.R` y
  `test-regresion-2100.R` simulan `.llamar_openai()` con
  `local_mocked_bindings()`. Correrlos con
  `NOT_CRAN=true Rscript -e 'devtools::test()'`.
- **Check completo con manual**: hace falta TinyTeX en el PATH
  (`C:/Users/PC/AppData/Roaming/TinyTeX/bin/windows`) y
  `_R_CHECK_FORCE_SUGGESTS_=false` (faltan algunos Suggests locales).
- `validation/` (estudio empirico del articulo) esta excluida del paquete.
