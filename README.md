# Satisfacción de pasajeros de una aerolínea

Trabajo Práctico Final — Maestría en Ciencia de Datos, Universidad Austral.
Asignatura: Estadística. Grupo 3.

## Contexto

Una aerolínea realizó 6.100 encuestas a sus pasajeros para conocer sus
características, evaluar la experiencia de viaje y analizar los factores
asociados a su nivel de satisfacción. El enunciado completo está en
[`Enunciado_Grupo3_Satisfaccion_pasajeros.pdf`](Enunciado_Grupo3_Satisfaccion_pasajeros.pdf)
y plantea 6 consignas:

1. Análisis descriptivo (univariado y bivariado contra `Satisfaccion`).
2. ¿La demora promedio no supera los 15 minutos, como afirma la aerolínea?
3. ¿El desvío estándar de la demora es inferior a 36 minutos?
4. Estimación de la proporción de pasajeros satisfechos (total y por clase).
5. Relación entre género, pasajero frecuente, tipo de viaje y satisfacción.
6. Relación entre demora en la partida y distancia del viaje.

## Estado del repositorio

- **Consigna 1** (análisis descriptivo): resuelta en `R/02_analisis_descriptivo.R`.
- **Consignas 2 a 6**: pendientes (inferencia estadística, proporciones,
  asociación y correlación).

## Estructura

```
TP_final/
├── data/
│   └── Satisfaccion_pasajeros.csv        # dataset original de la encuesta
├── R/
│   ├── 00_config.R                       # rutas, paleta de color, tamaño de figuras
│   ├── utils_graficos.R                  # funciones reutilizables de tablas y gráficos
│   ├── 01_carga_y_preparacion.R          # lectura del csv + variables como factores
│   └── 02_analisis_descriptivo.R         # consigna 1: univariado + bivariado
├── figuras/                              # se generan automáticamente al correr los scripts
├── docs/                                 # conclusiones e informe/presentación final
└── Enunciado_Grupo3_Satisfaccion_pasajeros.pdf
```

## Cómo correrlo

Abrir R/RStudio con el working directory en la raíz del repositorio (`TP_final/`)
y correr:

```r
source("R/02_analisis_descriptivo.R")
```

Esto carga y prepara los datos, y genera automáticamente todas las tablas
(impresas en consola) y figuras (guardadas en `figuras/`, todas con el mismo
tamaño de 16×10 cm a 300 DPI).

### Dependencias

```r
install.packages(c("here", "dplyr", "ggplot2"))
```

## Variables del dataset

| Variable | Descripción |
|---|---|
| `Genero` | Género declarado por el pasajero |
| `Edad` | Edad en años |
| `Pasajero_frecuente` | Pertenencia al programa de pasajero frecuente (Sí/No) |
| `Tipo_viaje` | Motivo del viaje (Negocios/Vacaciones) |
| `Clase` | Clase de viaje (Económica/Económica Plus/Business) |
| `Distancia_volada` | Distancia recorrida, en km |
| `Comodidad` | Valoración de la comodidad, escala 1 a 5 |
| `Servicio_a_bordo` | Valoración del servicio a bordo, escala 1 a 5 |
| `Demora_despegue` | Demora respecto del horario previsto, en minutos |
| `Satisfaccion` | Nivel de satisfacción general (satisfecho / neutral o insatisfecho) |
