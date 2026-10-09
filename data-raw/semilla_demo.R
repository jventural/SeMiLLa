# Construye 'semilla_demo': una escala pequena con embeddings SINTETICOS para
# que los ejemplos y los tests corran sin clave de API. Los items estan
# redactados a mano para la demostracion; los embeddings no vienen de ningun
# modelo: cada item es el centroide de su dimension mas ruido, y los items 1 y
# 2 se construyen casi identicos para que la auditoria de redundancia tenga un
# par que senalar. Texto solo ASCII (sin tildes) para que el paquete no lleve
# cadenas no ASCII en data/.

set.seed(20261008)

items <- data.frame(
  numero = 1:15,
  dimension = rep(c("Autoeficacia", "Organizacion", "Ansiedad"), each = 5),
  caracteristica = c(
    "confianza ante tareas", "confianza ante tareas", "persistencia",
    "dominio de contenidos", "afrontamiento de dificultades",
    "planificacion", "gestion del tiempo", "orden del material",
    "metas semanales", "revision periodica",
    "preocupacion previa", "sintomas fisicos", "bloqueo",
    "anticipacion negativa", "evitacion"),
  item = c(
    "Confio en que puedo resolver las tareas del curso",
    "Confio en que puedo completar las tareas del curso",
    "Sigo intentando cuando un problema me resulta dificil",
    "Puedo explicar los temas del curso a un companero",
    "Encuentro la forma de avanzar cuando me trabo en un ejercicio",
    "Hago un plan antes de empezar a estudiar",
    "Reparto mi tiempo de estudio entre las materias",
    "Guardo mis apuntes ordenados por tema",
    "Me pongo metas de estudio para cada semana",
    "Repaso lo avanzado al final de cada semana",
    "Me preocupo varios dias antes de un examen",
    "Siento el corazon acelerado durante un examen",
    "Me quedo en blanco aunque haya estudiado",
    "Pienso que voy a desaprobar antes de empezar el examen",
    "Postergo el estudio porque me pone nervioso pensar en el examen"),
  stringsAsFactors = FALSE)

d <- 64L
centros <- matrix(stats::rnorm(3 * d), 3, d)
memb <- match(items$dimension, unique(items$dimension))
emb <- centros[memb, ] + matrix(stats::rnorm(15 * d, sd = 0.9), 15, d)
emb[2, ] <- emb[1, ] + stats::rnorm(d, sd = 0.15)      # par casi gemelo
emb <- emb / sqrt(rowSums(emb^2))
rownames(emb) <- paste0("I", items$numero)
sim <- emb %*% t(emb)
dimnames(sim) <- list(rownames(emb), rownames(emb))

semilla_demo <- list(
  concepto = list(
    concepto = "Afrontamiento del estudio universitario",
    definicion = paste(
      "Grado en que un estudiante confia en sus capacidades, organiza su",
      "estudio y experimenta ansiedad ante las evaluaciones."),
    dimensiones = list(
      Autoeficacia = "Confianza en la propia capacidad para resolver las tareas del curso.",
      Organizacion = "Planificacion y orden en el estudio.",
      Ansiedad = "Preocupacion y activacion ante las evaluaciones."),
    fuente = "manual",
    referencias = NULL),
  items = items,
  embeddings = emb,
  similitud = sim,
  metadata = list(
    concepto_original = "Afrontamiento del estudio universitario",
    idioma = "es",
    poblacion = "estudiantes universitarios",
    modelo = "ninguno (demostracion)",
    modelo_embedding = "sintetico (64 dimensiones)",
    incluir_inversos = FALSE,
    fuente = "manual",
    sintetico = TRUE))
class(semilla_demo) <- c("semilla", "list")

save(semilla_demo, file = "data/semilla_demo.rda", compress = "xz")
