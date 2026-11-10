// Entrevista cognitiva (Google Apps Script), formulario de SeMiLLa::formulario_entrevista_cognitiva().
// Va dentro de la hoja de Google que recibe las respuestas (Extensiones > Apps Script).
// doGet sirve el formulario (archivo Index.html del proyecto); guardar() escribe cada participación en:
//   "Participantes": una fila por estudiante (perfil, tiempos, comentario final).
//   "Respuestas":    una fila por estudiante y frase (claridad, parafraseo, parte confusa, segundos).

function doGet() {
  return HtmlService.createHtmlOutputFromFile('Index')
    .setTitle('Entrevista cognitiva')
    .addMetaTag('viewport', 'width=device-width, initial-scale=1');
}

function hoja_(nombre, encabezado) {
  const libro = SpreadsheetApp.getActiveSpreadsheet();
  const h = libro.getSheetByName(nombre) || libro.insertSheet(nombre);
  if (h.getLastRow() === 0) {
    h.appendRow(encabezado);
    h.getRange(1, 1, 1, encabezado.length).setFontWeight('bold');
    h.setFrozenRows(1);
  }
  return h;
}

function guardar(d) {
  const candado = LockService.getScriptLock();
  candado.waitLock(30000);
  try {
    const recibido = new Date();
    const p = hoja_('Participantes', ['id', 'recibido', 'inicio', 'fin', 'edad', 'sexo', 'carrera', 'ciclo',
                                      'n_frases', 'n_no_claras', 'minutos', 'comentario']);
    // Un reenvío del mismo participante (p. ej., tras un corte de conexión) no se duplica.
    const ids = p.getLastRow() > 1 ? p.getRange(2, 1, p.getLastRow() - 1, 1).getValues().flat() : [];
    if (ids.indexOf(d.id) >= 0) return 'duplicado';
    const r = d.respuestas;
    const segundos = r.reduce((s, x) => s + (Number(x.segundos) || 0), 0);
    p.appendRow([d.id, recibido, d.inicio, d.fin, d.perfil.edad, d.perfil.sexo, d.perfil.carrera, d.perfil.ciclo,
                 r.length, r.filter(x => x.claridad !== 'Sí, se entiende').length,
                 Math.round(segundos / 6) / 10, d.comentario || '']);
    const h = hoja_('Respuestas', ['id', 'n', 'item', 'claridad', 'parafraseo', 'confuso', 'segundos']);
    const filas = r.map(x => [d.id, x.n, x.item, x.claridad, x.parafraseo, x.confuso || '', x.segundos]);
    h.getRange(h.getLastRow() + 1, 1, filas.length, filas[0].length).setValues(filas);
    return 'ok';
  } finally {
    candado.releaseLock();
  }
}
