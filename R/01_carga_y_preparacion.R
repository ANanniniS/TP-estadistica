# 01_carga_y_preparacion.R
# -----------------------------------------------------------------------------
# Carga el dataset de encuestas de satisfacción y prepara las variables
# categóricas como factores (nominales y ordinales), usando las etiquetas
# y escalas definidas en el enunciado del TP.
# -----------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

encuestas <- read.csv(ruta_datos, sep = ";", stringsAsFactors = FALSE)

# --- Variables categóricas nominales ------------------------------------------

encuestas$Genero <- factor(encuestas$Genero,
                            levels = c("F", "M"),
                            labels = c("Femenino", "Masculino"))

encuestas$Pasajero_frecuente <- factor(encuestas$Pasajero_frecuente,
                                        levels = c("SI", "NO"),
                                        labels = c("Frecuente", "No frecuente"))

encuestas$Tipo_viaje <- factor(encuestas$Tipo_viaje,
                                levels = c("Negocios", "Vacaciones"))

encuestas$Satisfaccion <- factor(encuestas$Satisfaccion,
                                  levels = c("satisfecho", "neutral o insatisfecho"),
                                  labels = c("Satisfecho", "Neutral o insatisfecho"))

# --- Variables categóricas ordinales ------------------------------------------

encuestas$Clase <- factor(encuestas$Clase,
                           levels = c("Eco", "Eco Plus", "Business"),
                           labels = c("Económica", "Económica Plus", "Business"),
                           ordered = TRUE)

encuestas$Comodidad <- factor(encuestas$Comodidad, levels = 1:5, ordered = TRUE)

encuestas$Servicio_a_bordo <- factor(encuestas$Servicio_a_bordo, levels = 1:5, ordered = TRUE)

# --- Chequeo rápido de la preparación ------------------------------------------

stopifnot(
  "El dataset quedó vacío tras la carga" = nrow(encuestas) > 0,
  "Quedaron NA al convertir alguna variable a factor" =
    all(complete.cases(encuestas[c("Genero", "Pasajero_frecuente", "Tipo_viaje",
                                    "Clase", "Comodidad", "Servicio_a_bordo",
                                    "Satisfaccion")]))
)
