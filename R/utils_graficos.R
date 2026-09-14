# utils_graficos.R
# -----------------------------------------------------------------------------
# Funciones reutilizables para tablas y gráficos del análisis descriptivo.
# Reciben el nombre de la variable como parámetro para evitar repetir la
# misma lógica (y los mismos valores hardcodeados) para cada variable del TP.
# Requiere que R/00_config.R haya sido fuenteado antes (usa guardar_figura,
# las paletas de color y el operador %||%).
# -----------------------------------------------------------------------------

# --- Tablas de frecuencia y contingencia --------------------------------------
# Implementadas con funciones base de R (table/prop.table/addmargins) para
# poder recibir el nombre de variable como parámetro sin depender de
# evaluación no estándar (NSE) de janitor, que no soporta bien nombres de
# columna dinámicos en tablas de dos vías.

tabla_frecuencias <- function(data, variable) {
  frecuencia_absoluta <- table(data[[variable]])
  frecuencia_relativa <- round(prop.table(frecuencia_absoluta) * 100, 2)

  tabla <- data.frame(
    categoria  = names(frecuencia_absoluta),
    n          = as.integer(frecuencia_absoluta),
    porcentaje = as.numeric(frecuencia_relativa)
  )

  rbind(tabla, data.frame(categoria = "Total",
                           n = sum(tabla$n),
                           porcentaje = sum(tabla$porcentaje)))
}

tabla_contingencia <- function(data, variable, tipo_porcentaje = c("total", "fila", "columna"),
                                grupo = "Satisfaccion") {
  tipo_porcentaje <- match.arg(tipo_porcentaje)
  margen <- switch(tipo_porcentaje, total = NULL, fila = 1, columna = 2)

  tabla_n <- table(Categoria = data[[variable]], Satisfaccion = data[[grupo]])
  tabla_pct <- if (is.null(margen)) {
    prop.table(tabla_n) * 100
  } else {
    prop.table(tabla_n, margin = margen) * 100
  }

  list(n = addmargins(tabla_n), porcentaje = round(addmargins(tabla_pct), 2))
}

# --- Gráfico de sectores (pie) ------------------------------------------------
# Por defecto usa los niveles del factor como etiquetas; se puede pasar
# `etiquetas` para variables ordinales numéricas (ej. escalas 1 a 5).

grafico_torta <- function(data, variable, colores, titulo, archivo,
                           etiquetas = NULL) {
  frecuencias   <- table(data[[variable]])
  etiquetas     <- etiquetas %||% levels(data[[variable]])
  porcentajes   <- round(prop.table(frecuencias) * 100, 2)
  etiquetas_pct <- paste0(etiquetas, " (", porcentajes, "%)")

  guardar_figura(archivo, expr_grafico_base = function() {
    pie(as.numeric(porcentajes), etiquetas_pct, col = colores, main = titulo)
  })
}

# --- Gráfico de barras de conteo (univariado) --------------------------------

grafico_barras_conteo <- function(data, variable, colores, titulo, xlab, archivo) {
  frecuencias <- table(data[[variable]])

  guardar_figura(archivo, expr_grafico_base = function() {
    barplot(frecuencias, main = titulo, xlab = xlab,
            ylab = "Cantidad de pasajeros", col = colores, las = 1)
  })
}

# --- Barras 100% apiladas (variable categórica vs. grupo) --------------------

grafico_barras_100 <- function(data, variable, colores, xlab, archivo,
                                titulo = NULL, grupo = "Satisfaccion") {
  tabla <- table(data[[grupo]], data[[variable]])

  guardar_figura(archivo, expr_grafico_base = function() {
    barplot(prop.table(tabla, margin = 2) * 100,
            xlab = xlab, ylab = "Frecuencia relativa (%)", main = titulo,
            col = colores, ylim = c(0, 100),
            legend.text = rownames(tabla),
            args.legend = list(x = "topright", cex = 0.7, inset = c(-0.05, 0)),
            beside = FALSE)
  })
}

# --- Moda ---------------------------------------------------------------
# R no trae una función de moda incorporada. Se calcula como el valor exacto
# más frecuente; si hay empate (multimodal), se informan todos los valores
# modales.

valores_modales <- function(x) {
  frecuencias <- table(x)
  as.numeric(names(frecuencias)[frecuencias == max(frecuencias)])
}

calcular_moda <- function(x) paste(valores_modales(x), collapse = ", ")

# --- Resumen numérico: tendencia central, dispersión y posición --------------

resumen_numerico <- function(data, variable) {
  x <- data[[variable]]
  data.frame(
    promedio = mean(x),
    mediana  = median(x),
    moda     = calcular_moda(x),
    min      = min(x),
    max      = max(x),
    rango    = max(x) - min(x),
    iqr      = IQR(x),
    var      = var(x),
    sd       = sd(x),
    cv_pct   = sd(x) / mean(x) * 100
  )
}

resumen_por_grupo <- function(data, variable, grupo = "Satisfaccion") {
  data %>%
    group_by(.data[[grupo]]) %>%
    summarise(
      promedio = mean(.data[[variable]]),
      mediana  = median(.data[[variable]]),
      moda     = calcular_moda(.data[[variable]]),
      sd       = sd(.data[[variable]]),
      cv_pct   = sd(.data[[variable]]) / mean(.data[[variable]]) * 100,
      iqr      = IQR(.data[[variable]]),
      min      = min(.data[[variable]]),
      max      = max(.data[[variable]]),
      .groups  = "drop"
    )
}

# --- Histograma (ggplot2) -----------------------------------------------
# Superpone líneas verticales con las medidas de tendencia central (media,
# mediana, moda) y de posición (cuartiles Q1 y Q3).

COLORES_MEDIDAS <- c(Media = "#08519C", Mediana = "#E6550D", Moda = "#31A354",
                     Q1 = "#756BB1", Q3 = "#756BB1")
LINETYPES_MEDIDAS <- c(Media = "solid", Mediana = "dashed", Moda = "dotted",
                       Q1 = "dotdash", Q3 = "longdash")

histograma <- function(data, variable, xlab, archivo, escala_log_y = FALSE) {
  x <- data[[variable]]
  moda    <- valores_modales(x)
  cuartiles <- quantile(x, probs = c(0.25, 0.75))

  medidas <- data.frame(
    medida = factor(c("Media", "Mediana", rep("Moda", length(moda)), "Q1", "Q3"),
                     levels = names(COLORES_MEDIDAS)),
    valor  = c(mean(x), median(x), moda, cuartiles[[1]], cuartiles[[2]])
  )

  escala_y <- if (escala_log_y) {
    scale_y_log10(name = "log10(Frecuencia absoluta)")
  } else {
    scale_y_continuous(name = "Frecuencia absoluta")
  }

  p <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(colour = "black", fill = "#6BAED6") +
    geom_vline(data = medidas, aes(xintercept = valor, colour = medida, linetype = medida),
               linewidth = 0.8) +
    scale_colour_manual(name = "Medida", values = COLORES_MEDIDAS) +
    scale_linetype_manual(name = "Medida", values = LINETYPES_MEDIDAS) +
    scale_x_continuous(name = xlab) +
    escala_y

  guardar_figura(archivo, plot = p)
  p
}

# --- Histograma comparativo por grupo (ggplot2, facetado) ---------------------

histograma_por_grupo <- function(data, variable, xlab, archivo,
                                  grupo = "Satisfaccion") {
  p <- ggplot(data, aes(x = .data[[variable]], colour = .data[[grupo]],
                         fill = .data[[grupo]])) +
    geom_histogram(alpha = 0.3) +
    facet_grid(cols = vars(.data[[grupo]])) +
    scale_x_continuous(name = xlab) +
    scale_y_continuous(name = "Frecuencia absoluta") +
    labs(colour = grupo, fill = grupo)

  guardar_figura(archivo, plot = p)
  p
}

# --- Boxplot (base R): univariado o comparativo por grupo --------------------

boxplot_univariado <- function(data, variable, ylab, titulo, archivo,
                                color = paleta_azules_5[4]) {
  guardar_figura(archivo, expr_grafico_base = function() {
    boxplot(data[[variable]], col = color, ylab = ylab, main = titulo)
  })
}

boxplot_comparativo <- function(data, variable, ylab, titulo, archivo,
                                 grupo = "Satisfaccion", colores = paleta_azules_2) {
  formula_grafico <- as.formula(paste(variable, "~", grupo))

  guardar_figura(archivo, expr_grafico_base = function() {
    boxplot(formula_grafico, data = data,
            xlab = "Satisfacción", ylab = ylab, main = titulo, col = colores)
  })
}
