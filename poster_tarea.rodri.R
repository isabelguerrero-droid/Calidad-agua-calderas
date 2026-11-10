#TRABAJO INDIVIDUAL DEL POSTER
# install.packages("modeest")
# install.packages("mice")

library(fdth)
library(modeest)
library(mice)
library(ggplot2)

#creo base de datos 
BDCALDERAS<-read.csv("bdcalderas2026P1OCT2026.prueba1.csv", header = TRUE,
                     sep = ";", dec = ".", na.strings = c("", " ", "No disp",
                                                          "No Hay dato", "no hay dato",
                                                          "20-500", "02/01/1900", "04/01/1900"))

BDCALDERAS$Fecha<-NULL
BDCALDERAS$Alcalinidad.P<-NULL
BDCALDERAS$Alcalinidad.M<-NULL
BDCALDERAS$Dureza.total<-NULL
BDCALDERAS$Molibdato<-NULL
BDCALDERAS$Hierro<-NULL
BDCALDERAS$Sulfito.ppm<-NULL
BDCALDERAS$Alcalinidad.OH<-NULL
BDCALDERAS$Purgas<-NULL
BDCALDERAS$Notas<- NULL
BDCALDERAS$PH<- NULL
str(BDCALDERAS)
summary(BDCALDERAS)

#limites
BDCALDERAS$Sílice<- as.numeric(BDCALDERAS$Sílice)
BDCALDERAS$Conductividad<- as.numeric(BDCALDERAS$Conductividad)

BDCALDERAS$Conductividad[BDCALDERAS$Conductividad>5000]<-NA
BDCALDERAS$Conductividad[BDCALDERAS$Conductividad<0]<-NA
BDCALDERAS$Sílice[BDCALDERAS$Sílice<0]<-NA

unique(BDCALDERAS$Sílice)
unique(BDCALDERAS$Conductividad)
unique(BDCALDERAS$Año)
unique(BDCALDERAS$Destino.de.Agua)
unique(BDCALDERAS$Descripcion.equipo)

CALDERAS <- BDCALDERAS[!is.na(BDCALDERAS$Destino.de.Agua) &
                         BDCALDERAS$Destino.de.Agua == "Calderas", ]
CALDERAS <- CALDERAS[!is.na(CALDERAS$Año) & CALDERAS$Año > 2010, ]
nrow(CALDERAS)      
summary(CALDERAS)
md.pattern(CALDERAS[, c("Conductividad", "Sílice")])

#IMPUTACION 
CALDERASimp<- CALDERAS
CALDERASimp$Sílice<- ifelse(is.na(CALDERASimp$Sílice),
                            median(CALDERASimp$Sílice, na.rm = TRUE), CALDERASimp$Sílice) 
CALDERASimp$Conductividad<- ifelse(is.na(CALDERASimp$Conductividad),
                                   median(CALDERASimp$Conductividad, na.rm = TRUE), CALDERASimp$Conductividad)

summary(CALDERASimp)
mfv(CALDERASimp$Sílice)

md.pattern(CALDERASimp[, c("Conductividad", "Sílice")])

tabla_silice <- fdt(CALDERASimp$Sílice)
tabla_silice

# MEDIDAS: ANTES (2011-2015) CONTRA AHORA (2021-2025)
# (con datos sin imputar)
ini <- CALDERAS$Sílice[CALDERAS$Año <= 2015]
ult <- CALDERAS$Sílice[CALDERAS$Año >= 2021]

mean(ini, na.rm = TRUE);   mean(ult, na.rm = TRUE)
median(ini, na.rm = TRUE); median(ult, na.rm = TRUE)
sd(ini, na.rm = TRUE);     sd(ult, na.rm = TRUE)
quantile(ini, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(ult, c(0.25, 0.5, 0.75), na.rm = TRUE)
IQR(ini, na.rm = TRUE);    IQR(ult, na.rm = TRUE)

# GRÁFICOS (con CALDERAS, sin imputar)
CALDERAS$Periodo <- ifelse(CALDERAS$Año <= 2015, "2011-2015",
                           ifelse(CALDERAS$Año >= 2021, "2021-2025", NA))
PERIODOS <- CALDERAS[!is.na(CALDERAS$Periodo), ]

# boxplot de sílice por año
g1 <- ggplot(CALDERAS, aes(x = factor(Año), y = Sílice)) +
  geom_boxplot(fill = "lightblue", na.rm = TRUE) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  labs(title = "Sílice por año", x = "Año", y = "Sílice (ppm)")
g1

#mediana anual (línea + puntos)
g2 <- ggplot(CALDERAS, aes(x = Año, y = Sílice)) +
  stat_summary(fun = median, geom = "line", color = "steelblue", na.rm = TRUE) +
  stat_summary(fun = median, geom = "point", size = 3, color = "steelblue", na.rm = TRUE) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  labs(title = "Mediana anual de sílice", x = "Año", y = "Sílice (ppm)")
g2

# densidad, antes contra ahora
g3 <- ggplot(PERIODOS, aes(x = Sílice, fill = Periodo)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  labs(title = "Distribución de sílice: antes y ahora", x = "Sílice (ppm)", y = "Densidad")
g3

# histograma con media (roja) y mediana (azul)
g4 <- ggplot(CALDERAS, aes(x = Sílice)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "white", na.rm = TRUE) +
  geom_vline(xintercept = mean(CALDERAS$Sílice, na.rm = TRUE), color = "red") +
  geom_vline(xintercept = median(CALDERAS$Sílice, na.rm = TRUE), color = "blue") +
  labs(title = "Distribución de sílice", x = "Sílice (ppm)", y = "Frecuencia")
g4

#boxplot de conductividad por año
g5 <- ggplot(CALDERAS, aes(x = factor(Año), y = Conductividad)) +
  geom_boxplot(fill = "green", na.rm = TRUE) +
  labs(title = "Conductividad por año", x = "Año", y = "Conductividad")
g5

# coeficiente de variacion
# Sílice
silice_ini <- CALDERAS$Sílice[CALDERAS$Año <= 2015]
silice_ult <- CALDERAS$Sílice[CALDERAS$Año >= 2021]

sd(silice_ini, na.rm = TRUE) / mean(silice_ini, na.rm = TRUE) * 100
sd(silice_ult, na.rm = TRUE) / mean(silice_ult, na.rm = TRUE) * 100

# Conductividad
cond_ini <- CALDERAS$Conductividad[CALDERAS$Año <= 2015]
cond_ult <- CALDERAS$Conductividad[CALDERAS$Año >= 2021]

sd(cond_ini, na.rm = TRUE) / mean(cond_ini, na.rm = TRUE) * 100
sd(cond_ult, na.rm = TRUE) / mean(cond_ult, na.rm = TRUE) * 100
#tabla de comparacion
tabla_cv <- data.frame(
  Variable = c("Sílice", "Conductividad"),
  
  CV_2011_2015 = c(sd(silice_ini, na.rm = TRUE) / mean(silice_ini, na.rm = TRUE) * 100,
                   sd(cond_ini, na.rm = TRUE) / mean(cond_ini, na.rm = TRUE) * 100),
  
  CV_2021_2025 = c(sd(silice_ult, na.rm = TRUE) / mean(silice_ult, na.rm = TRUE) * 100,
                   sd(cond_ult, na.rm = TRUE) / mean(cond_ult, na.rm = TRUE) * 100))

tabla_cv