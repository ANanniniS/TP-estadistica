#-------------------------------------------------------#
# MAESTRÍA EN CIENCIA DE DATOS - UNIV. AUSTRAL          #
# ASIGNATURA: ESTADÍSTICA                               #
#                                                       #
# TRABAJO PRÁCTICO FINAL - GRUPO Nº 3                   #
# ESTUDIO: "SATISFACCIÓN DE PASAJEROS DE UNA AEROLÍNEA" #
#                                                       #
# Septiembre 2.026                                       #
#-------------------------------------------------------#


# BORRADOR VERO #

#----------------------------------------------#
# TAREAS PRELIMINARES                          #
#----------------------------------------------#

# 1. Cargar librerías

library(janitor)
library(dplyr)
library(ggplot2)


#library(readr)      # Lo dejo comentado porque no lo necesité para abrir el dataset
#library(tidyverse)
#library(stats) # para distribuciones
#library(DescTools) # para intervalos de confianza


# 2. Setear Working Directory personal

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Lo setea en la carpeta donde está este script

getwd()   # Consultar el WD final para chequear


# 3. Abrir dataset "Satisfaccion_pasajeros.csv"

encuestas <- read.csv("Satisfaccion_pasajeros.csv", sep=";")


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


#----------------------------------------------#
# CONSIGNA Nº 1: ANÁLISIS DESCRIPTIVO          #
#----------------------------------------------#
# I. ANÁLISIS UNIVARIADO                       #
#----------------------------------------------#

#----------------------------------------------#
# I.I. VAR. CATEGÓRICAS                        #
#----------------------------------------------#

####### GÉNERO DECLARADO POR LOS PASAJEROS ######

# table(encuestas$Genero)       # Creo que no tiene sentido y con la tercera es mejor, más completo
# tabyl(encuestas, Genero)    # con frec abs y relat (sin totales ni %) - Idem anterior

encuestas %>% 
  tabyl(Genero) %>% 
  adorn_totals %>%                  # Agrega la fila de Totales
  adorn_pct_formatting(digits = 2)  # Transforma las frec relat en %, con 1 decimal


# Gráfico de sectores:
porcentajes <- as.numeric(round(((prop.table(table(encuestas$Genero)))*100),2))
etiquetas <- c("Femenino", "Masculino")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("indianred","royalblue"),
    main = "Gráfico 1. Distribución de pasajeros según género declarado")


# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Genero),
        main="Gráfico 2. Pasajeros según género declarado",
        ylim=c(0,3500)                       #valor más alto para el eje Y
        ylab ="Cantidad de pasajeros",       #rótulo del eje Y
        xlab="Género",                       #rótulo del eje X
        cex.lab=1,                           #tamaño títulos ejes (1 = tam normal)
        cex.names=0.8,                       #tamaño categorías eje X (1 = tam normal)
        font.axis=1,                         #tipo de fuente / estilo (1 = normal)
        col=c("indianred","royalblue"),      #lista de colores de barras
        las=1)                               #alineación de categorías eje X (1 = horiz)

# ¿¿ Es necesario para este agregar el de barras por % ??


###### TIPO DE PASAJERO (PROGRAMA DE PASAJERO FRECUENTE) ######

# table(encuestas$Pasajero_frecuente)       
# tabyl(encuestas, Pasajero_frecuente)    

encuestas %>% 
  tabyl(Pasajero_frecuente) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  


# Gráfico de sectores:
porcentajes <- as.numeric(round(((prop.table(table(encuestas$Pasajero_frecuente)))*100),2))
etiquetas <- c("Pasajero frecuente", "Pasajero no frecuente")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#08519C","#3182BD"),
    main = "Gráfico 3. Distribución de pasajeros según pertenencia a programa de pasajeros frecuentes")


# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Pasajero_frecuente),
        main="Gráfico 4. Pasajeros según pertenencia a programa de pasajeros frecuentes",
        ylim=c(0,6000),
        ylab ="Cantidad de pasajeros",
        xlab="Tipo de pasajero", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#08519C","#3182BD"),               
        las=1)                               

# ¿¿Es necesario agregar Gráfico de barras con %??



###### MOTIVO DEL VIAJE ######

# table(encuestas$Tipo_viaje)       
# tabyl(encuestas, Tipo_viaje)    

encuestas %>% 
  tabyl(Tipo_viaje) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  

# Gráfico de sectores:
porcentajes <- as.numeric(round(((prop.table(table(encuestas$Tipo_viaje)))*100),2))
etiquetas <- c("Negocios", "Vacaciones")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#3182BD", "#08519C"),
    main = "Gráfico 5. Distribución de pasajeros según motivo del viaje")

# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Tipo_viaje),
        main="Gráfico 6. Pasajeros según motivo del viaje",
        ylim=c(0,6000),
        ylab ="Cantidad de pasajeros",
        xlab="Motivo del viaje", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#08519C","#3182BD"),               
        las=1)                      

# ¿¿Es necesario agregar Gráfico de barras con %??



###### NIVEL DE SATISFACCIÓN GENERAL ######

# table(encuestas$Satisfaccion)       
# tabyl(encuestas, Satisfaccion)    

encuestas %>% 
  tabyl(Satisfaccion) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  

# Gráfico de sectores

porcentajes <- as.numeric(round(((prop.table(table(encuestas$Satisfaccion)))*100),2))
etiquetas <- c("Satisfecho", "Neutral o Instatisfecho")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#3182BD", "#08519C"),
    main = "Gráfico 7. Distribución de pasajeros según su nivel de satisfacción")


# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Tipo_viaje),
        main="Gráfico 8. Pasajeros según nivel de satisfacción",
        ylim=c(0,6000),
        ylab ="Cantidad de pasajeros",
        xlab="Nivel de satisfacción", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#08519C","#3182BD"),               
        las=1)                     

# ¿¿Es necesario agregar Gráfico de barras con %??



###### CLASE ######

# table(encuestas$Clase)       
# tabyl(encuestas, Clase)    

encuestas %>% 
  tabyl(Clase) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  

# Gráfico de sectores:
porcentajes <- as.numeric(round(((prop.table(table(encuestas$Clase)))*100),2))
etiquetas <- c("Económica", "Económica Plus", "Business")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#BDD7E7", "#3182BD", "#08519C"),
    main = "Gráfico 9. Distribución de pasajeros según la clase en la que viajaron")


# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Clase),
        main="Gráfico 10. Pasajeros según clase en la que viajaron",
        ylim=c(0,4000),
        ylab ="Cantidad de pasajeros",
        xlab="Clase", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#BDD7E7","#3182BD", "#08519C"),               
        las=1)                     

# ¿¿Es necesario agregar Gráfico de barras con %??




###### VALORACIÓN DE LA COMODIDAD ######

# table(encuestas$Comodidad)       
# tabyl(encuestas, Comodidad)    

encuestas %>% 
  tabyl(Comodidad) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  


# ¿¿¿Gráfico de sectores????  No sé si está bueno incluir este gráfico, 
#                             siendo que los nombres de las categorías son Nros.


porcentajes <- as.numeric(round(((prop.table(table(encuestas$Comodidad)))*100),2))
etiquetas <- c("1 (Menor comodidad)", "2", "3", "4", "5 (Mayor comodidad)")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),
    main = "Gráfico 11. Distribución de pasajeros según comodidad percibida")



# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Comodidad),
        main="Gráfico 12. Pasajeros según comodidad percibida",
        ylim=c(0,2500),
        ylab ="Cantidad de pasajeros",
        xlab="Nivel de comodidad", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),               
        las=1)                     

# ¿¿Es necesario agregar Gráfico de barras con %??



###### VALORACIÓN DEL SERVICIO A BORDO ######

# table(encuestas$Servicio_a_bordo)       
# tabyl(encuestas, Servicio_a_bordo)    

encuestas %>% 
  tabyl(Servicio_a_bordo) %>% 
  adorn_totals %>%                  
  adorn_pct_formatting(digits = 2)  


# ¿¿¿Gráfico de sectores????  No sé si está bueno incluir este gráfico, 
#                             siendo que los nombres de las categorías son Nros.


porcentajes <- as.numeric(round(((prop.table(table(encuestas$Servicio_a_bordo)))*100),2))
etiquetas <- c("1 (Menor valoración)", "2", "3", "4", "5 (Mayor valoración)")
etiquetas_con_porcentajes <- paste(etiquetas, porcentajes, "%")

pie(porcentajes, etiquetas_con_porcentajes,col = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),
    main = "Gráfico 13. Distribución de pasajeros según valoración del servició a bordo")


# Gráfico de barras verticales (por cantidades)

barplot(table(encuestas$Servicio_a_bordo),
        main="Gráfico 14. Pasajeros según valoración del servició a bordo",
        ylim=c(0,2500),
        ylab ="Cantidad de pasajeros",
        xlab="Valoración del servicio a bordo", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                         
        col=c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),               
        las=1)                     


# ¿¿Es necesario agregar Gráfico de barras con %??




#----------------------------------------------#
# CONSIGNA Nº 1: ANÁLISIS DESCRIPTIVO (CONTIN.)#
#----------------------------------------------#
# I. ANÁLISIS UNIVARIADO (CONTINUACIÓN)        #
#----------------------------------------------#

#----------------------------------------------#
# I.II. VAR. NUMÉRICAS                         #
#----------------------------------------------#


###### EDAD DE LOS PASAJEROS #####

# 1) Medidas de tendencia central, dispersión y posición  

encuestas %>%
  summarise(min=min(Edad),
            max=max(Edad),
            rango= max(Edad) - min(Edad),
            iqr= IQR(Edad),
            var=var(Edad),
            sd = sd(Edad),
            cv=(sd(Edad)/mean(Edad))*100)

summary(encuestas$Edad)


# 2) Gráficos:

boxplot(encuestas$Edad, col="#6BAED6", ylab="Edad",main="Distribución de la edad de los pasajeros")

ggplot(encuestas, aes(x=Edad)) + 
  geom_histogram(col="black", fill="#6BAED6") +
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="Edad (años)")



###### DISTANCIA VOLADA #####

# 1) Medidas de tendencia central, dispersión y posición  

encuestas %>%
  summarise(min=min(Distancia_volada),
            max=max(Distancia_volada),
            rango= max(Distancia_volada) - min(Distancia_volada),
            iqr= IQR(Distancia_volada),
            var=var(Distancia_volada),
            sd = sd(Distancia_volada),
            cv=(sd(Distancia_volada)/mean(Distancia_volada))*100)

summary(encuestas$Distancia_volada)


# 2) Gráficos:

boxplot(encuestas$Distancia_volada, col="#6BAED6", ylab="Distancia (km)",main="Distribución de la distancia volada")

ggplot(encuestas, aes(x=Edad)) + 
  geom_histogram(col="black", fill="#6BAED6") +
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="Distancia (km)")




###### DEMORA #####

# 1) Medidas de tendencia central, dispersión y posición  

encuestas %>%
  summarise(min=min(Demora_despegue),
            max=max(Demora_despegue),
            rango= max(Demora_despegue) - min(Demora_despegue),
            iqr= IQR(Demora_despegue),
            var=var(Demora_despegue),
            sd = sd(Demora_despegue),
            cv=(sd(Demora_despegue)/mean(Demora_despegue))*100)

summary(encuestas$Demora_despegue)


# 2) Gráficos:

boxplot(encuestas$Demora_despegue, col="#6BAED6", ylab="Demora (minutos))",main="Distribución de la demora en el despegue")

ggplot(encuestas, aes(x=Demora_despegue)) + 
  geom_histogram(col="black", fill="#6BAED6") +
  scale_y_log10(name="log_10(Frecuencia absoluta)") +
  scale_x_continuous(name="Demora (minutos)")



#----------------------------------------------#
# CONSIGNA Nº 1: ANÁLISIS DESCRIPTIVO          #
#----------------------------------------------#
# II. ANÁLISIS BIVARIADO                       #
#     En relación a la variable objetivo       #
#     (Nivel de satisfacción)                  #
#----------------------------------------------#


#------------------------------------------------#
# II.I. VAR. CATEGÓRICAS VS SATISFACCIÓN (CATEG.)#
#------------------------------------------------#


###### GÉNERO VS SATISFACIÓN #####

# 1) Tabla de contingencia: ¿¿Cuáles mostramos??

#  Tabla   de contigencia - porcentaje sobre el total 
encuestas %>% 
  tabyl(Genero, Satisfaccion) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Tabla   de contigencia - porcentaje filas
encuestas %>% 
  tabyl(Genero, Satisfaccion) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# Tabla   de contigencia - porcentaje col
encuestas %>% 
  tabyl(Genero, Satisfaccion) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


# 2) Gráficos de barras (Acá hice lío!!!):

tabla=table(encuestas$Genero, encuestas$Satisfaccion)
tabla2=table(encuestas$Satisfaccion, encuestas$Genero)


#Barras apiladas - frec absolutas
barplot(tabla,
        xlab = "Nivel de Satisfacción", ylab = "Frecuencia absoluta",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.9, cex.lab=0.9,cex.names =0.9,cex.main=0.8, 
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex=0.9,inset = c(-0.05,0)),
        beside=FALSE)


#Barras agrupadas- - frec absolutas
barplot(tabla,
        xlab = "Nivel de satisfacción", ylab = "Frecuencia absoluta",
        ylim=c(0,150),
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.9, cex.lab=0.9,cex.names =0.9,cex.main=0.8, 
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex=0.9,inset = c(-0.05,0)),
        beside=TRUE)


##Barras apiladas - Frec Relativa
barplot(prop.table(tabla)*100,
        xlab = "Color", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.9, cex.lab=0.9,cex.names =0.9,cex.main=0.8, 
        ylim=c(0,40),
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex=0.6,inset = c(-0.05,0)),
        beside=FALSE)

#Barras agrupadas- - frec relativa
barplot(prop.table(tabla)*100,
        xlab = "Color", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.9, cex.lab=0.9,cex.names =0.9,cex.main=0.8, 
        ylim=c(0,35),
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex=0.6,inset = c(-0.05,0)),
        beside=TRUE)

#barras 100%- Frec Relativa
barplot(prop.table(tabla2, margin=2)*100,
        xlab = "Género", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.7, cex.lab=0.7,cex.names =0.7,cex.main=0.8, 
        ylim=c(0,100),
        legend.text = rownames(tabla2),
        args.legend = list(x = "topright", cex=0.4,inset = c(-0.1,0)),
        beside=FALSE)

barplot(prop.table(tabla, margin=2)*100,
        xlab = "Género", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.7, cex.lab=0.7,cex.names =0.7,cex.main=0.8, 
        ylim=c(0,100),
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex=0.6,inset = c(-0.05,0)),
        beside=FALSE)

#barras 100% - horizontales
barplot(prop.table(tabla, margin=2) * 100,
        xlab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.7, cex.lab = 0.7, cex.names = 0.7, cex.main = 0.8, 
        horiz = TRUE,
        las = 1, # Esto pone los nombres de las categorías en horizontal
        legend.text = rownames(tabla),
        args.legend = list(x = "topright", cex = 0.6, inset = c(0, 0)),
        beside = FALSE)






###### PASAJERO FRECUENTE VS SATISFACIÓN #####




###### MOTIVO DE VIAJE VS SATISFACIÓN #####




###### CLASE VS SATISFACIÓN #####





###### COMODIDAD VS SATISFACIÓN #####



###### SERVICIO A BORDO VS SATISFACIÓN #####




#-----------------------------------------------#
# II.II. VAR. NUMÉRICAS VS SATISFACCIÓN (CATEG.)#
#-----------------------------------------------#


###### EDAD VS SATISFACIÓN #####

# 1) Medidas resumen

encuestas %>%
  group_by(Satisfaccion) %>%      # Antes de calcular agrupa los datos por...
  summarise(media = mean(Edad),      # A partir de acá cada cálculo es por el crit de group
            mediana= median(Edad),
            sd = sd(Edad),
            cv=sd(Edad)/mean(Edad)*100,
            iqr= IQR(Edad),
            min=min(Edad),
            max=max(Edad))


# 2) Boxplot comparativo

boxplot(Edad~ Satisfaccion,          # Edad en fx del nivel de satisfacción
        data = encuestas,
        xlab="Satisfacción", 
        ylab="Edad",
        main="Distribución de la edad según el nivel de satisfacción",
        col = c("#3182BD", "#08519C"))


# 3) Histograma comparativo - Elegir alguna de las opciones (me gusta más el 3)

#opción 1:

ggplot(encuestas, 
       aes(x = Edad, 
           colour=Satisfaccion))  + geom_histogram()

#opción 2 (superpuestos):

ggplot(encuestas, 
       aes(x = Edad)) +
  geom_histogram(aes(color = Satisfaccion, 
                     fill = Satisfaccion), 
                 position = "identity",
                 alpha=0.4)    # alpha controla la transparencia


#opción 3 (Comparativos):

ggplot(encuestas, 
       aes(x = Edad)) +
  geom_histogram(aes(color = Satisfaccion, 
                     fill = Satisfaccion), 
                 alpha=0.3)+
  facet_grid(~Satisfaccion)     



###### DISTANCIA VS SATISFACIÓN #####


# 1) Medidas resumen

encuestas %>%
  group_by(Satisfaccion) %>%      
  summarise(media = mean(Distancia_volada),      
            mediana= median(Distancia_volada),
            sd = sd(Distancia_volada),
            cv=sd(Distancia_volada)/mean(Distancia_volada)*100,
            iqr= IQR(Distancia_volada),
            min=min(Distancia_volada),
            max=max(Distancia_volada))

# 2) Boxplot comparativo

boxplot(Edad~ Satisfaccion,          
        data = encuestas,
        xlab="Satisfacción", 
        ylab="Distancia (km)",
        main="Distribución de la distancia según el nivel de satisfacción",
        col = c("#3182BD", "#08519C"))

# 3) Histograma comparativo (con opc 3)

ggplot(encuestas, 
       aes(x = Distancia_volada)) +
  geom_histogram(aes(color = Satisfaccion, 
                     fill = Satisfaccion), 
                 alpha=0.3)+
  facet_grid(~Satisfaccion) 



###### DEMORA VS SATISFACIÓN #####

# 1) Medidas resumen

encuestas %>%
  group_by(Satisfaccion) %>%      
  summarise(media = mean(Demora_despegue),      
            mediana= median(Demora_despegue),
            sd = sd(Demora_despegue),
            cv=sd(Demora_despegue)/mean(Demora_despegue)*100,
            iqr= IQR(Demora_despegue),
            min=min(Demora_despegue),
            max=max(Demora_despegue))


# 2) Boxplot comparativo

boxplot(Edad~ Satisfaccion,          
        data = encuestas,
        xlab="Satisfacción", 
        ylab="Demora en el despegue (minutos)",
        main="Distribución de la distancia según el nivel de satisfacción",
        col = c("#3182BD", "#08519C"))


# 3) Histograma comparativo (con opc 3)

ggplot(encuestas, 
       aes(x = Demora_despegue)) +
  geom_histogram(aes(color = Satisfaccion, 
                     fill = Satisfaccion), 
                 alpha=0.3)+
  facet_grid(~Satisfaccion) 



