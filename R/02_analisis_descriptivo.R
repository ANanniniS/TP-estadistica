# 02_analisis_descriptivo.R
# -----------------------------------------------------------------------------
# Punto 1 del TP: análisis descriptivo univariado y bivariado (en relación a
# la variable objetivo "Satisfaccion") de la encuesta de satisfacción de
# pasajeros. Todas las figuras se guardan automáticamente en `figuras/` con
# el mismo tamaño y resolución (ver R/00_config.R).
# -----------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("R", "utils_graficos.R"))
source(here::here("R", "01_carga_y_preparacion.R"))

# =============================================================================
# I. ANÁLISIS UNIVARIADO
# =============================================================================

# -----------------------------------------------------------------------------
# I.I Variables categóricas
# -----------------------------------------------------------------------------

## Género declarado -----------------------------------------------------------

tabla_frecuencias(encuestas, "Genero")

grafico_torta(encuestas, "Genero", colores = paleta_genero,
              titulo = "Distribución de pasajeros según género declarado",
              archivo = "genero_torta.png")

grafico_barras_conteo(encuestas, "Genero", colores = paleta_genero,
                       titulo = "Pasajeros según género declarado",
                       xlab = "Género", archivo = "genero_barras.png")

## Pasajero frecuente ----------------------------------------------------------

tabla_frecuencias(encuestas, "Pasajero_frecuente")

grafico_torta(encuestas, "Pasajero_frecuente", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según programa de pasajero frecuente",
              archivo = "pasajero_frecuente_torta.png")

grafico_barras_conteo(encuestas, "Pasajero_frecuente", colores = paleta_azules_2,
                       titulo = "Pasajeros según programa de pasajero frecuente",
                       xlab = "Tipo de pasajero", archivo = "pasajero_frecuente_barras.png")

## Tipo de viaje ----------------------------------------------------------------

tabla_frecuencias(encuestas, "Tipo_viaje")

grafico_torta(encuestas, "Tipo_viaje", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según motivo del viaje",
              archivo = "tipo_viaje_torta.png")

grafico_barras_conteo(encuestas, "Tipo_viaje", colores = paleta_azules_2,
                       titulo = "Pasajeros según motivo del viaje",
                       xlab = "Motivo del viaje", archivo = "tipo_viaje_barras.png")

## Clase ------------------------------------------------------------------------

tabla_frecuencias(encuestas, "Clase")

grafico_torta(encuestas, "Clase", colores = paleta_azules_3,
              titulo = "Distribución de pasajeros según la clase en la que viajaron",
              archivo = "clase_torta.png")

grafico_barras_conteo(encuestas, "Clase", colores = paleta_azules_3,
                       titulo = "Pasajeros según clase en la que viajaron",
                       xlab = "Clase", archivo = "clase_barras.png")

## Comodidad percibida -----------------------------------------------------------

tabla_frecuencias(encuestas, "Comodidad")

grafico_torta(encuestas, "Comodidad", colores = paleta_azules_5,
              titulo = "Distribución de pasajeros según comodidad percibida",
              archivo = "comodidad_torta.png",
              etiquetas = c("1 (menor comodidad)", "2", "3", "4", "5 (mayor comodidad)"))

grafico_barras_conteo(encuestas, "Comodidad", colores = paleta_azules_5,
                       titulo = "Pasajeros según comodidad percibida",
                       xlab = "Nivel de comodidad", archivo = "comodidad_barras.png")

## Servicio a bordo ---------------------------------------------------------------

tabla_frecuencias(encuestas, "Servicio_a_bordo")

grafico_torta(encuestas, "Servicio_a_bordo", colores = paleta_azules_5,
              titulo = "Distribución de pasajeros según valoración del servicio a bordo",
              archivo = "servicio_a_bordo_torta.png",
              etiquetas = c("1 (menor valoración)", "2", "3", "4", "5 (mayor valoración)"))

grafico_barras_conteo(encuestas, "Servicio_a_bordo", colores = paleta_azules_5,
                       titulo = "Pasajeros según valoración del servicio a bordo",
                       xlab = "Valoración del servicio a bordo",
                       archivo = "servicio_a_bordo_barras.png")

## Satisfacción general -----------------------------------------------------------

tabla_frecuencias(encuestas, "Satisfaccion")

grafico_torta(encuestas, "Satisfaccion", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según su nivel de satisfacción",
              archivo = "satisfaccion_torta.png")

grafico_barras_conteo(encuestas, "Satisfaccion", colores = paleta_azules_2,
                       titulo = "Pasajeros según nivel de satisfacción",
                       xlab = "Nivel de satisfacción", archivo = "satisfaccion_barras.png")

# -----------------------------------------------------------------------------
# I.II Variables numéricas
# -----------------------------------------------------------------------------

## Edad ---------------------------------------------------------------------------

resumen_numerico(encuestas, "Edad")

boxplot_univariado(encuestas, "Edad", ylab = "Edad (años)",
                    titulo = "Distribución de la edad de los pasajeros",
                    archivo = "edad_boxplot.png")

histograma(encuestas, "Edad", xlab = "Edad (años)", archivo = "edad_histograma.png")

## Distancia volada -----------------------------------------------------------------

resumen_numerico(encuestas, "Distancia_volada")

boxplot_univariado(encuestas, "Distancia_volada", ylab = "Distancia (km)",
                    titulo = "Distribución de la distancia volada",
                    archivo = "distancia_boxplot.png")

histograma(encuestas, "Distancia_volada", xlab = "Distancia (km)",
           archivo = "distancia_histograma.png")

## Demora en el despegue --------------------------------------------------------------
# La distribución está fuertemente asimétrica a la derecha (muchos vuelos sin
# demora y una cola larga de demoras grandes), por lo que se agrega también un
# histograma con escala logarítmica en el eje Y para visualizar mejor la cola.

resumen_numerico(encuestas, "Demora_despegue")

boxplot_univariado(encuestas, "Demora_despegue", ylab = "Demora (minutos)",
                    titulo = "Distribución de la demora en el despegue",
                    archivo = "demora_boxplot.png")

histograma(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
           archivo = "demora_histograma.png")

histograma(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
           archivo = "demora_histograma_log.png", escala_log_y = TRUE)

# =============================================================================
# II. ANÁLISIS BIVARIADO (en relación a la variable objetivo Satisfaccion)
# =============================================================================

# -----------------------------------------------------------------------------
# II.I Variables categóricas vs. Satisfacción
# -----------------------------------------------------------------------------

## Género vs. Satisfacción -----------------------------------------------------------

tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "total")
tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "fila")
tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Genero", colores = paleta_genero,
                    xlab = "Género", archivo = "genero_barras_100.png")

## Pasajero frecuente vs. Satisfacción -------------------------------------------------

tabla_contingencia(encuestas, "Pasajero_frecuente", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Pasajero_frecuente", colores = paleta_azules_2,
                    xlab = "Tipo de pasajero",
                    archivo = "pasajero_frecuente_barras_100.png")

## Tipo de viaje vs. Satisfacción -------------------------------------------------------

tabla_contingencia(encuestas, "Tipo_viaje", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Tipo_viaje", colores = paleta_azules_2,
                    xlab = "Motivo del viaje", archivo = "tipo_viaje_barras_100.png")

## Clase vs. Satisfacción -----------------------------------------------------------------

tabla_contingencia(encuestas, "Clase", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Clase", colores = paleta_azules_3,
                    xlab = "Clase", archivo = "clase_barras_100.png")

## Comodidad vs. Satisfacción --------------------------------------------------------------

tabla_contingencia(encuestas, "Comodidad", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Comodidad", colores = paleta_azules_5,
                    xlab = "Nivel de comodidad", archivo = "comodidad_barras_100.png")

## Servicio a bordo vs. Satisfacción --------------------------------------------------------

tabla_contingencia(encuestas, "Servicio_a_bordo", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Servicio_a_bordo", colores = paleta_azules_5,
                    xlab = "Valoración del servicio a bordo",
                    archivo = "servicio_a_bordo_barras_100.png")

# -----------------------------------------------------------------------------
# II.II Variables numéricas vs. Satisfacción
# -----------------------------------------------------------------------------

## Edad vs. Satisfacción -------------------------------------------------------------------

resumen_por_grupo(encuestas, "Edad")

boxplot_comparativo(encuestas, "Edad", ylab = "Edad (años)",
                     titulo = "Distribución de la edad según el nivel de satisfacción",
                     archivo = "edad_boxplot_satisfaccion.png")

histograma_por_grupo(encuestas, "Edad", xlab = "Edad (años)",
                      archivo = "edad_histograma_satisfaccion.png")

## Distancia volada vs. Satisfacción -----------------------------------------------------------

resumen_por_grupo(encuestas, "Distancia_volada")

boxplot_comparativo(encuestas, "Distancia_volada", ylab = "Distancia (km)",
                     titulo = "Distribución de la distancia según el nivel de satisfacción",
                     archivo = "distancia_boxplot_satisfaccion.png")

histograma_por_grupo(encuestas, "Distancia_volada", xlab = "Distancia (km)",
                      archivo = "distancia_histograma_satisfaccion.png")

## Demora en el despegue vs. Satisfacción -------------------------------------------------------

resumen_por_grupo(encuestas, "Demora_despegue")

boxplot_comparativo(encuestas, "Demora_despegue", ylab = "Demora (minutos)",
                     titulo = "Distribución de la demora según el nivel de satisfacción",
                     archivo = "demora_boxplot_satisfaccion.png")

histograma_por_grupo(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
                      archivo = "demora_histograma_satisfaccion.png")
