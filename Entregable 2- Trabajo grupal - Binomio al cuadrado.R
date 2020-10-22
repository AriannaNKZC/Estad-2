

library(readxl)
library(foreign)
library(descr)
library(DescTools)
library(haven)
library(car)
library(psych)
library(PMCMRplus)
library(Rmisc)
library(gplots)

covid <- read_excel("Cuestionario_COVID.xlsx")
########################PRIMERA PARTE

#Variable: 

#Descripción de las variables
#variable de horas_estudiar
horas_estudiar <- covid$tiempo1
str(horas_estudiar)
table(horas_estudiar)
horas_estudiar = as.factor(horas_estudiar)
levels(horas_estudiar)= c("Muy Insuficientes", "Insuficientes", "Regular", "Suficientes", "Muy Suficientes")
table(horas_estudiar)
freq(horas_estudiar)
freq(ordered(horas_estudiar), cex.names=0.7)
pie(table(horas_estudiar))
Mode(horas_estudiar, na.rm = T)
Mode(as.numeric(horas_estudiar), na.rm=T)
Median(as.numeric(horas_estudiar), na.rm=T)
barplot(table(horas_estudiar),col="orange",
        xlab=NULL,
        ylab="Conteo de encuestados",
        main="Gráfico 1: percepción sobre las horas 
                  asignadas al estudio",
        cex.names=0.6)
horas_estudiar=covid$tiempo1
IQR(horas_estudiar)

#variable de horas_recreación
horas_recreación <- covid$tiempo2
str(horas_recreación)
table(horas_recreación)
horas_recreación = as.factor(horas_recreación)
levels(horas_recreación)= c("Muy Insuficientes", "Insuficientes", "Regular", "Suficientes", "Muy Suficientes")
table(horas_recreación)
freq(horas_recreación)
freq(ordered(horas_recreación), cex.names=0.6)
pie(table(horas_recreación))
Mode(horas_recreación, na.rm = T)
Mode(as.numeric(horas_recreación), na.rm=T)
Median(as.numeric(horas_recreación), na.rm=T)
barplot(table(horas_recreación),col="YellowGreen",
        xlab=NULL,
        ylab="Conteo de encuestados",
        main="Gráfico 2: percepción sobre las horas 
                  asignadas a la recreación",
        cex.names=0.6)
horas_recreación = covid$tiempo2
IQR(horas_recreación)

#variable de horas_domesticas
horas_domesticas <- covid$tiempo3
str(horas_domesticas)
table(horas_domesticas)
horas_domesticas= as.factor(horas_domesticas)
levels(horas_domesticas)= c("Muy Insuficientes", "Insuficientes", "Regular", "Suficientes", "Muy Suficientes")
table(horas_domesticas)
freq(horas_domesticas)
freq(ordered(horas_domesticas), cex.names=0.7)
pie(table(horas_domesticas))
Mode(horas_domesticas, na.rm = T)
Mode(as.numeric(horas_domesticas), na.rm=T)
Median(as.numeric(horas_domesticas), na.rm=T)
barplot(table(horas_domesticas),col="Salmon",
        xlab=NULL,
        ylab="Conteo de encuestados",
        main="Gráfico 3: percepción sobre las horas
                 asignadas a las labores domésticas",
        cex.names=0.6)
horas_domesticas = covid$tiempo3
IQR(horas_domesticas)

#variable de horas_dormir
horas_dormir <- covid$tiempo4
str(horas_dormir)
table(horas_dormir)
horas_dormir= as.factor(horas_dormir)
levels(horas_dormir)= c("Muy Insuficientes", "Insuficientes", "Regular", "Suficientes", "Muy Suficientes")
table(horas_dormir)
freq(horas_dormir)
freq(ordered(horas_dormir), cex.names=0.7)
Mode(horas_dormir, na.rm = T)
Mode(as.numeric(horas_dormir), na.rm=T)
Median(as.numeric(horas_dormir), na.rm=T)
barplot(table(horas_dormir),col="Thistle",
        xlab=NULL,
        ylab="Conteo de encuestados",
        main= "Gráfico 4: percepción sobre las horas de sueño",
        cex.names=0.6)
horas_dormir = covid$tiempo4
IQR(horas_dormir)

#################SEGUNDA PARTE

#Variable - recodificación
#horas de estudiar, importa satisfacción

recode_h.est = covid$tiempo1
table(recode_h.est)
recode_h.est <- recode(recode_h.est, "5=4")
recode_h.est <- recode(recode_h.est, "1:3=0; 4=1")


#horas de recreación, importa satisfacción

recode_h.recr = covid$tiempo2
table(recode_h.recr)
recode_h.recr <- recode(recode_h.recr, "5=4")
recode_h.recr <- recode(recode_h.recr, "1:3=0; 4=1")
table(recode_h.recr)

#horas de dormir, importa satisfacción

recode_h.dor = covid$tiempo3
table(recode_h.dor)
recode_h.dor <- recode(recode_h.dor, "5=4")
recode_h.dor <- recode(recode_h.dor, "1:3=0; 4=1")
table(recode_h.dor)

#horas de actividades domésticas, importa satisfacción

recode_h.dom = covid$tiempo4
table(recode_h.dom)
recode_h.dom <- recode(recode_h.dom, "5=4")
recode_h.dom <- recode(recode_h.dom, "1:3=0; 4=1")
table(recode_h.dom)

#sexo

#Variable - sexo
str(covid$sexo)
covid$sexo <- factor(covid$sexo)
levels(covid$sexo) <- c("Mujer", "Hombre")
freq(covid$sexo)

#ingresos
covid$ingresos

ingresos <- covid$ingresos
ingresos <- recode(ingresos, "2=3")
ingresos <- as.factor(ingresos)
levels(ingresos) <- c("Se ha reducido", "No se ha reducido")
table(ingresos)

#años - pucp
###variable años en la pucp
#recodificamos

años_pucp <- covid$pucp
años_pucp=as.factor(años_pucp)
levels(años_pucp)
años_pucp <- recode(años_pucp, "0:2=1")
años_pucp <- recode(años_pucp, "3:6=2")
table(años_pucp)
levels(años_pucp) <- c("Desde ingresante a 2 años", "De 3 a 6 años")
table(años_pucp)
freq(años_pucp)

#####ANÁLISIS DE VARIABLES
###análisis de variable de tiempo 1 
#sexo
#hacer las tablas de contingencia

table(covid$sexo, recode_h.est) 
prop.test(x= c(29, 40), n = c(110,89)) #0.009641

prop.test(29,110)
prop.test(40,89)

plotmeans(recode_h.est ~ covid$sexo , p=0.95, 
          xlab= "Sexo", 
          ylab ="Suficiencia de la distribución de horas para estudiar", 
          col = "brown",
          main="Gráfico 5, IC 95%: Suficiencia de la distribución de horas para estudiar según sexo")

#ingresos

table(ingresos, recode_h.est)
prop.test(x= c(38, 31), n = c(118,81))  # 0.4641

prop.test(38,118)
prop.test(31,81)

plotmeans(recode_h.est ~ ingresos , p=0.95, 
          xlab= "Variación de ingresos durante el contexto del covid-19",
          ylab="Suficiencia de distribución de horas para estudiar",
          col=c("Salmon","Gold","SteelBlue","Thistle"),
          main="Gráfico 6, IC 95%: Suficiencia de la distribución de horas para estudiar según variación de ingresos")


#años en pucp
table(recode_h.est, años_pucp)

prop.test(32,96)
prop.test(37,102)

plotmeans(recode_h.est ~ años_pucp , p=0.95, 
          xlab= "Años en la PUCP", 
          ylab ="Suficiencia de la distribución de horas para estudiar", 
          col = "Salmon",
          main="Gráfico 7, IC 95%: Suficiencia de la distribución de horas para estudiar según años que lleva en la PUCP")

###análisis de variable de tiempo 2

#sexo

table(covid$sexo, recode_h.recr) 
prop.test(x= c(22, 28), n = c(110,89)) #0.09123

prop.test(22,110)
prop.test(28,89)

plotmeans(recode_h.recr ~ covid$sexo , p=0.95, 
          xlab= "Sexo", 
          ylab ="Suficiencia de la distribución de horas en actividades recreativas", 
          col = "orangered3",
          main="Gráfico 8, IC 95%: Suficiencia de la distribución de horas en actividades recreativas según sexo")


#ingresos

table(recode_h.recr, ingresos)

prop.test(x= c(31, 19), n = c(118,81)) #0.7769

prop.test(31,118)
prop.test(19,81)


plotmeans(recode_h.recr ~ ingresos , p=0.95, 
          xlab= "Variación de ingresos durante el contexto del covid-19",
          ylab="Distribución de horas en actividades de recreación",
          col=c("Salmon","Gold","SteelBlue","Thistle"),
          main="Gráfico 9, IC 95%: Suficiencia de la distribución de horas en actividades recreativas según variación de ingresos")


#años en pucp
table(recode_h.recr, años_pucp)

prop.test(23,96)
prop.test(27,102)

plotmeans(recode_h.recr ~ años_pucp , p=0.95, 
          xlab= "Años en la PUCP", 
          ylab ="Suficiencia de la distribución de horas para recreación", 
          col = "Salmon",
          main="Gráfico 10, IC 95%: Suficiencia de la distribución de horas para recreación según años que lleva en la PUCP")

###análisis de variable 3

#sexo

table(covid$sexo, recode_h.dom) 
prop.test(x= c(26, 27), n = c(110,89)) #otra opción: 0.3671

prop.test(26,110)
prop.test(27,89)


plotmeans(recode_h.dom ~ covid$sexo , p=0.95, 
          xlab= "Sexo", 
          ylab ="Suficiencia de la distribución de horas para tareas domésticas", 
          col = "orangered3",
          main="Gráfico 11, IC 95%: Suficiencia de la distribución de horas para tareas domésticas según sexo")



#ingresos
table(recode_h.dom, ingresos)

prop.test(29,118)
prop.test(29,81)

plotmeans(recode_h.dom ~ ingresos , p=0.95, 
          xlab= "Ingresos", 
          ylab ="Suficiencia de la distribución de horas para tareas domésticas", 
          col = "Salmon",
          main="Gráfico 12, IC 95%: Suficiencia de la distribución de horas para las tareas domésticas según ingresos")

#años en la pucp
table(recode_h.dom, años_pucp)

prop.test(24,96)
prop.test(34,102)

plotmeans(recode_h.dom ~ años_pucp , p=0.95, 
          xlab= "Años en la PUCP", 
          ylab ="Suficiencia de la distribución de horas para tareas domésticas", 
          col = "Salmon",
          main="Gráfico 13, IC 95%: Suficiencia de la distribución de horas para tareas domésticas según años que lleva en la PUCP")

###análisis de variable tiempo 4

#sexo

table(covid$sexo, recode_h.dor)
prop.test(x= c(31, 27), n = c(110,89)) #otra opción: 0.8605

prop.test(31,110)
prop.test(27,89)

plotmeans(recode_h.dor ~ covid$sexo , p=0.95, 
          xlab= "Sexo", 
          ylab ="Suficiencia de la distribución de horas para dormir", 
          col = "orangered3",
          main="Gráfico 14, IC 95%: Suficiencia de la distribución de horas para dormir según sexo")

#ingresos
table(recode_h.dor, ingresos)

prop.test(32,118)
prop.test(21,81)

plotmeans(recode_h.dor ~ ingresos , p=0.95, 
          xlab= "Ingresos", 
          ylab ="Suficiencia de la distribución de horas para dormir", 
          col = "Salmon",
          main="Gráfico 15, IC 95%: Suficiencia de la distribución de horas para dormir según ingresos")
#años en la pucp
table(recode_h.dor, años_pucp)

prop.test(30,96)
prop.test(23,102)

plotmeans(recode_h.dor ~ años_pucp , p=0.95, 
          xlab= "Años en la PUCP", 
          ylab ="Suficiencia de la distribución de horas para dormir", 
          col = "Salmon",
          main="Gráfico 16, IC 95%: Suficiencia de la distribución de horas para dormir según años que lleva en la PUCP")


################TERCERA PARTE

intervalo=covid$tiempo1+covid$tiempo2+covid$tiempo3+covid$tiempo4
summary(intervalo)
intervalo=intervalo-5
intervalo=(intervalo/13)
intervalo=intervalo*5

t.test(intervalo~covid$sexo, var.equal=T)
#P-value<0.05: existe diferencia significativa

#ANOVA
#años
pucp1 <- covid$pucp
pucp1=as.factor(pucp1)
levels(pucp1)
levels(pucp1) <- c("Menos de 1 año","1 año","2 años","3 años","4 años","5 años","6 años")
table(pucp1)

summary(aov(intervalo~pucp1))
#ingresos
covid$ingresos=ordered(covid$ingresos)
summary(aov(intervalo~covid$ingresos))
#P-value>0.05: no existe diferencia significativa entre las medias

