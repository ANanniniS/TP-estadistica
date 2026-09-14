# 00_config.R
# -----------------------------------------------------------------------------
# Configuración global del análisis: paquetes, rutas, paleta de colores y
# tamaño estándar de las figuras exportadas a la carpeta `figuras/`.
# -----------------------------------------------------------------------------

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

library(here)
library(dplyr)
library(ggplot2)

# --- Rutas -------------------------------------------------------------------

ruta_datos   <- here("data", "Satisfaccion_pasajeros.csv")
ruta_figuras <- here("figuras")

if (!dir.exists(ruta_figuras)) dir.create(ruta_figuras, recursive = TRUE)

# --- Paletas de color ----------------------------------------------------
# Escala secuencial de azules (ColorBrewer "Blues"), reutilizada en todos los
# gráficos para mantener una identidad visual consistente a lo largo del TP.

paleta_azules_2 <- c("#3182BD", "#08519C")
paleta_azules_3 <- c("#BDD7E7", "#3182BD", "#08519C")
paleta_azules_5 <- c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")
paleta_genero   <- c("indianred1", "royalblue")

# --- Tamaño estándar de las figuras exportadas --------------------------------
# Se fija un único ancho/alto/resolución para que todas las figuras del TP
# (ggplot2 y gráficos base R) tengan el mismo tamaño en `figuras/`.

ANCHO_FIGURA_CM <- 16
ALTO_FIGURA_CM  <- 10
RESOLUCION_DPI  <- 300

# --- Tema gráfico compartido (ggplot2) ----------------------------------------

theme_set(theme_minimal(base_size = 12))

# --- Guardado uniforme de figuras ---------------------------------------------
# Acepta un gráfico ggplot (`plot`) o una función que dibuja un gráfico base R
# (`expr_grafico_base`), y lo guarda en `figuras/` con el mismo tamaño y
# resolución en ambos casos.

guardar_figura <- function(nombre_archivo, plot = NULL, expr_grafico_base = NULL) {
  ruta <- file.path(ruta_figuras, nombre_archivo)

  if (!is.null(plot)) {
    ggsave(ruta, plot = plot,
           width = ANCHO_FIGURA_CM, height = ALTO_FIGURA_CM,
           units = "cm", dpi = RESOLUCION_DPI)
  } else if (!is.null(expr_grafico_base)) {
    png(ruta,
        width = ANCHO_FIGURA_CM, height = ALTO_FIGURA_CM,
        units = "cm", res = RESOLUCION_DPI)
    on.exit(dev.off())
    expr_grafico_base()
  } else {
    stop("guardar_figura() requiere `plot` (objeto ggplot) o ",
         "`expr_grafico_base` (función que dibuja el gráfico).")
  }
}

# --- Utilidad genérica ---------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x
