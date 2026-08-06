# SeMiLLa 2.9.16 (2026-08-06)
## converger_escala() habla el mismo idioma que la compuerta

Al subir a la app el resultado del analisis local, la misma escala aparecia
descrita de dos formas en la misma pantalla: arriba "ROBUSTA" (escenario) y
abajo "APLICAR CON CAUTELA" (veredicto). El bucle SIGUE decidiendo con las
cadenas antiguas -ramifica sobre ellas- pero ahora tambien guarda y muestra el
escenario.

- `$historial` gana la columna **`escenario`**, junto a `veredicto`. Quien lea
  el historial puede mostrar el mismo vocabulario que la compuerta en vez de
  traducir, que seria incorrecto: el veredicto es el peor de los tres ejes y el
  escenario describe solo la estructura. No son la misma medida.
- El encabezado de la corrida traduce el objetivo al mostrarlo
  ("APLICAR CON CAUTELA" -> "que deje de ser FRAGIL"), sin tocar el valor
  interno.

