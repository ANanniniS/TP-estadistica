#-------------------------------------------------------#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL          #
# ASIGNATURA: ESTADÍSTICA                               #
#                                                       #
# TRABAJO PRÁCTICO FINAL - GRUPO Nº 3                   #
# ESTUDIO: "SATISFACCIÓN DE PASAJEROS DE UNA AEROLÍNEA" #
#                                                       #
# Septiembre 2.026                                       #
#-------------------------------------------------------#


# CONSIGNAS 2 A 6

#----------------------------------------------#
# TAREAS PRELIMINARES                          #
#----------------------------------------------#

# 1. Cargar librerías

library(janitor)
library(dplyr)
library(ggplot2)
library(DescTools) # para intervalos de confianza
library(EnvStats)
library(nortest)

#library(readr)      # Lo dejo comentado porque no lo necesité para abrir el dataset
#library(tidyverse)
#library(stats) # para distribuciones



# 2. Setear Working Directory personal

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Lo setea en la carpeta donde está este script

getwd()   # Consultar el WD final para chequear


# 3. Abrir dataset "Satisfaccion_pasajeros.csv"

encuestas <- read.csv("data/Satisfaccion_pasajeros.csv", sep=";")


# 4. Inspección muy general:

str(encuestas)

summary(encuestas)


# 5. Preparación de las variables categóricas (aplicando Factor): 
#    Genero, Pasajero_frecuente, Tipo_viaje, Clase, Comodidad, Servicio_a_bordo y Satisfacción

#    Categóricas Nominales:

encuestas$Genero <- factor(encuestas$Genero, 
                           levels = c("F", "M"), 
                           labels = c("Femenino", "Masculino"))   # Reemplaza las letras por los nombres
str(encuestas$Genero)
summary(encuestas$Genero)

encuestas$Pasajero_frecuente <- factor(encuestas$Pasajero_frecuente, 
                                       levels = c("SI", "NO"), 
                                       labels = c("Frecuente", "No Frecuente")) # Reemplaza SI/NO por lo que representan,
# ¿Es mejor hacerlo así o no? (lo pienso p/ los gráf.)
str(encuestas$Pasajero_frecuente)
summary(encuestas$Pasajero_frecuente)

encuestas$Tipo_viaje <- factor(encuestas$Tipo_viaje)
str(encuestas$Tipo_viaje)
summary(encuestas$Tipo_viaje)

encuestas$Satisfaccion <- factor(encuestas$Satisfaccion, 
                                 levels = c("satisfecho", "neutral o insatisfecho"), 
                                 labels = c("Satisfecho", "Neutral o Insatisfecho"))   # Para poner 1a letra mayúsc.
str(encuestas$Satisfaccion)
summary(encuestas$Satisfaccion)


#    Categóricas Ordinales:

table(encuestas$Clase)
encuestas$Clase <- factor(encuestas$Clase,
                          levels = c("Eco", "Eco Plus", "Business"),
                          labels = c("Económica", "Económica Plus", "Business"),
                          ordered = TRUE)
str(encuestas$Tipo_viaje)


table(encuestas$Comodidad)
encuestas$Comodidad <- factor(encuestas$Comodidad,
                              levels = c(1, 2, 3, 4, 5),
                              #labels = c(""),            #Lo ideal debe ser indicar visualmente cuál es peor y mejor
                              ordered = TRUE)
str(encuestas$Comodidad)

table(encuestas$Servicio_a_bordo)
encuestas$Servicio_a_bordo <- factor(encuestas$Servicio_a_bordo,
                                     levels = c(1, 2, 3, 4, 5),
                                     #labels = c(""),            #Lo ideal debe ser indicar visualmente cuál es peor y mejor
                                     ordered = TRUE)
str(encuestas$Servicio_a_bordo)


# 6. Ver como quedó en lo general después de los ajustes:

summary(encuestas)




#########################
# CONSIGNA 2            # 
#########################


# ESTIMACIÓN PUNTUAL

mean(encuestas$Demora_despegue)   

# ESTIMACIÓN INTERVALO DE CONFIANZA para mu, sin conocer sigma --> t Student

MeanCI(encuestas$Demora_despegue, sd = NULL, conf.level = 0.95)

# Rta.: Con un nivel de confianza del 95%, se estima que la demora de despegue promedio 
# poblacional se encuentra entre 13,26' y 15,06'.


# Test de hipótesis:

t.test(encuestas$Demora_despegue,
       mu = 15,
       alternative = "less", #cambiado, quiero demostrar que la evidencia demuestra esto.
       conf.level = 0.95)

# Respuesta: Rechazo la hipótesis nula. La evidencia demuestra que el promedio es menor a 15 minutos con un nivel de signifcancia del 0.05



#########################
# CONSIGNA 3            # 
#########################


# ESTIMACIÓN PUNTUAL

var(encuestas$Demora_despegue)
sd(encuestas$Demora_despegue)   

# ESTIMACIÓN INTERVALO DE CONFIANZA para σ (desv. estándar poblacional)
ad.test(encuestas$Demora_despegue) # p-value <3e-16. Se descarta la normalidad

sqrt(VarCI(encuestas$Demora_despegue, method = "bonett", conf.level = 0.95))

# Rta.: Con un nivel de confianza del 95%, se estima que la variabilidad de la demora de despegue 
# poblacional se encuentra entre 32,21' y 40,34'.


# Test de hipótesis:

#H0) Variabilidad demora σ2 > 1296 (1296 = 36^2)  (objetivo)
#H1) Variabilidad demora σ2 <= 1296


varTest(encuestas$Demora_despegue,
        alternative="less",
        sigma.squared = 1296,
        conf.level = 0.95)

# Respuesta: No rechazo la hipótesis nula. No se puede afirmar que el desvio estandar de la demora este acotado superiormente por 36 minutos.



#########################
# CONSIGNA 4            # 
#########################

# Nota: Los gráficos de estas relaciones, los habíamos hecho dentro de la Consigna Nº 1

# Estimación puntual (p̂  =x/n) para el total de la muestra:

encuestas %>% 
  tabyl(Satisfaccion) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  

# Intervalo de confianza para proporcion de Sastisfaccion 
BinomCI(sum(encuestas$Satisfaccion=="Satisfecho"),count(encuestas)$n,conf.level = 0.95)
# Podemos afirmar con un nivel de confianza del 95% que el nivel de satisfaccion se encuentra entre 0.456 y 0.482

# Estimación puntual (p̂  =x/n) por clase:
encuestas %>% 
  tabyl(Clase, Satisfaccion) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


# Intervalo de confianza para proporcion de Sastisfaccion por clase
encuesta_eco = encuestas[encuestas$Clase == "Económica",]
encuesta_ecoplus = encuestas[encuestas$Clase == "Económica Plus",]
encuesta_bus = encuestas[encuestas$Clase == "Business",]

BinomCI(sum(encuesta_eco$Satisfaccion=="Satisfecho"),count(encuesta_eco)$n,conf.level = 0.95)
# Podemos decir con un nivel de confianza del 95% que el nivel de satisfaccion para la clase Economica
# se encuentra entre un 18,22% y 21,30%


BinomCI(sum(encuesta_ecoplus$Satisfaccion=="Satisfecho"),count(encuesta_ecoplus)$n,conf.level = 0.95)
# Podemos decir con un nivel de confianza del 95% que el nivel de satisfaccion para la clase Economica Plus
# se encuentra entre un 25,47% y 33,97%

BinomCI(sum(encuesta_bus$Satisfaccion=="Satisfecho"),count(encuesta_bus)$n,conf.level = 0.95)
# Podemos decir con un nivel de confianza del 95% que el nivel de satisfaccion para la clase Business
# se encuentra entre un 69,88% y 73,05%

#########################
# CONSIGNA 5            # 
#########################

# Nota: Los gráficos de estas relaciones, los habíamos hecho dentro de la Consigna Nº 1

# TIPO DE PASAJERO VS SATISFACCIÓN

# H0) Las variables son independientes
# H1) Las variables no son independientes

tabla1 <- table(encuestas$Pasajero_frecuente, encuestas$Satisfaccion)
#View(tabla1)

chisq.test(tabla1)

# Respuesta: Rechazo H0 = Las variables no son independientes.


# TIPO DE VIAJE VS SATISFACCIÓN

# H0) Las variables son independientes
# H1) Las variables no son independientes

tabla2 <- table(encuestas$Tipo_viaje, encuestas$Satisfaccion)
#View(tabla2)

chisq.test(tabla2)

# Respuesta: Rechazo H0 = Las variables no son independientes.



#########################
# CONSIGNA 6            # 
#########################

# Gráfico de dispersión:

plot (encuestas$Distancia_volada, encuestas$Demora_despegue,    # en plot, 1ra var eje X y 2da var eje Y
      pch=20,                   # plotting character: 20 = círculo pequeño lleno
      col="lightblue", 
      main= "Diagrama de dispersión: Distancia y Demora", 
      xlab="Distancia volada (km)", 
      ylab="Demora en despegue (minutos)")

# Covarianza

cov(encuestas$Distancia_volada, encuestas$Demora_despegue)

# Coeficiente de correlación

cor(encuestas$Distancia_volada, encuestas$Demora_despegue, method="spearman")

# Respuesta: La demora en la partida no está relacionada con la distancia del viaje. 
