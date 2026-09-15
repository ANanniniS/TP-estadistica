# 02_analisis_descriptivo.R
# -----------------------------------------------------------------------------
# Punto 1 del TP: análisis descriptivo de la encuesta de satisfacción de
# pasajeros. Para cada variable se presenta primero su análisis univariado y
# luego su análisis bivariado en relación a la variable objetivo
# (Satisfaccion). Las variables se analizan en el mismo orden en que aparecen
# enumeradas en el enunciado del TP. Todas las figuras se guardan
# automáticamente en `figuras/` con el mismo tamaño y resolución (ver
# R/00_config.R).
# -----------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("R", "utils_graficos.R"))
source(here::here("R", "01_carga_y_preparacion.R"))

# =============================================================================
# 1. Género
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Genero")

grafico_torta(encuestas, "Genero", colores = paleta_genero,
              titulo = "Distribución de pasajeros según género declarado",
              archivo = "genero_torta.png")


## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "total")
tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "fila")
tabla_contingencia(encuestas, "Genero", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Genero", colores = paleta_genero,
                    xlab = "Género", archivo = "genero_barras_100.png")

# =============================================================================
# 2. Edad
# =============================================================================

## Univariado -------------------------------------------------------------

resumen_numerico(encuestas, "Edad")

histograma(encuestas, "Edad", xlab = "Edad (años)", archivo = "edad_histograma.png")

## Bivariado vs. Satisfacción -----------------------------------------------

resumen_por_grupo(encuestas, "Edad")

boxplot_comparativo(encuestas, "Edad", ylab = "Edad (años)",
                     titulo = "Distribución de la edad según el nivel de satisfacción",
                     archivo = "edad_boxplot_satisfaccion.png")

# =============================================================================
# 3. Pasajero frecuente
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Pasajero_frecuente")

grafico_torta(encuestas, "Pasajero_frecuente", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según programa de pasajero frecuente",
              archivo = "pasajero_frecuente_torta.png")

## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Pasajero_frecuente", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Pasajero_frecuente", colores = paleta_azules_2,
                    xlab = "Tipo de pasajero",
                    archivo = "pasajero_frecuente_barras_100.png")

# =============================================================================
# 4. Tipo de viaje
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Tipo_viaje")

grafico_torta(encuestas, "Tipo_viaje", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según motivo del viaje",
              archivo = "tipo_viaje_torta.png")

## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Tipo_viaje", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Tipo_viaje", colores = paleta_azules_2,
                    xlab = "Motivo del viaje", archivo = "tipo_viaje_barras_100.png")

# =============================================================================
# 5. Clase
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Clase")

grafico_torta(encuestas, "Clase", colores = paleta_azules_3,
              titulo = "Distribución de pasajeros según la clase en la que viajaron",
              archivo = "clase_torta.png")

grafico_barras_conteo(encuestas, "Clase", colores = paleta_azules_3,
                       titulo = "Pasajeros según clase en la que viajaron",
                       xlab = "Clase", archivo = "clase_barras.png")

## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Clase", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Clase", colores = paleta_azules_3,
                    xlab = "Clase", archivo = "clase_barras_100.png")

# =============================================================================
# 6. Distancia volada
# =============================================================================

## Univariado -------------------------------------------------------------

resumen_numerico(encuestas, "Distancia_volada")


histograma(encuestas, "Distancia_volada", xlab = "Distancia (km)",
           archivo = "distancia_histograma.png")

## Bivariado vs. Satisfacción -----------------------------------------------

resumen_por_grupo(encuestas, "Distancia_volada")

boxplot_comparativo(encuestas, "Distancia_volada", ylab = "Distancia (km)",
                     titulo = "Distribución de la distancia según el nivel de satisfacción",
                     archivo = "distancia_boxplot_satisfaccion.png")

histograma_por_grupo(encuestas, "Distancia_volada", xlab = "Distancia (km)",
                      archivo = "distancia_histograma_satisfaccion.png")

# =============================================================================
# 7. Comodidad
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Comodidad")

grafico_torta(encuestas, "Comodidad", colores = paleta_azules_5,
              titulo = "Distribución de pasajeros según comodidad percibida",
              archivo = "comodidad_torta.png",
              etiquetas = c("1 (menor comodidad)", "2", "3", "4", "5 (mayor comodidad)"))

## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Comodidad", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Comodidad", colores = paleta_azules_5,
                    xlab = "Nivel de comodidad", archivo = "comodidad_barras_100.png")

# =============================================================================
# 8. Servicio a bordo
# =============================================================================

## Univariado -------------------------------------------------------------

tabla_frecuencias(encuestas, "Servicio_a_bordo")

grafico_torta(encuestas, "Servicio_a_bordo", colores = paleta_azules_5,
              titulo = "Distribución de pasajeros según valoración del servicio a bordo",
              archivo = "servicio_a_bordo_torta.png",
              etiquetas = c("1 (menor valoración)", "2", "3", "4", "5 (mayor valoración)"))


## Bivariado vs. Satisfacción -----------------------------------------------

tabla_contingencia(encuestas, "Servicio_a_bordo", tipo_porcentaje = "columna")

grafico_barras_100(encuestas, "Servicio_a_bordo", colores = paleta_azules_5,
                    xlab = "Valoración del servicio a bordo",
                    archivo = "servicio_a_bordo_barras_100.png")

# =============================================================================
# 9. Demora en el despegue
# =============================================================================
# La distribución está fuertemente asimétrica a la derecha (muchos vuelos sin
# demora y una cola larga de demoras grandes), por lo que se agrega también un
# histograma con escala logarítmica en el eje Y para visualizar mejor la cola.

## Univariado -------------------------------------------------------------

resumen_numerico(encuestas, "Demora_despegue")


histograma(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
           archivo = "demora_histograma.png")

histograma(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
           archivo = "demora_histograma_log.png", escala_log_y = TRUE)

## Bivariado vs. Satisfacción -----------------------------------------------

resumen_por_grupo(encuestas, "Demora_despegue")


histograma_por_grupo(encuestas, "Demora_despegue", xlab = "Demora (minutos)",
                      archivo = "demora_histograma_satisfaccion.png")

# =============================================================================
# 10. Satisfacción (variable objetivo)
# =============================================================================
# Es la variable contra la que se compararon todas las anteriores, por lo que
# aquí solo corresponde su análisis univariado.

tabla_frecuencias(encuestas, "Satisfaccion")

grafico_torta(encuestas, "Satisfaccion", colores = paleta_azules_2,
              titulo = "Distribución de pasajeros según su nivel de satisfacción",
              archivo = "satisfaccion_torta.png")

grafico_barras_conteo(encuestas, "Satisfaccion", colores = paleta_azules_2,
                       titulo = "Pasajeros según nivel de satisfacción",
                       xlab = "Nivel de satisfacción", archivo = "satisfaccion_barras.png")



