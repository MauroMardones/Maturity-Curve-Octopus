#Para ambos sexos de la especie.

library(readxl)
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)
dataoct <- read_excel("C:/Users/arturo.jimenez/Desktop/ARTURO FEMP/Muestreos Biológicos Pulpo/Base de datos muestreos biológicos Octopus vulgaris.xlsx")
View(dataoct)
glimpse(dataoct)


#Para categorizar la fecha en día, mes y año.
dataoct1 <- dataoct %>% 
  mutate(DAY = day(`Fecha pesca`),
         MES= month(`Fecha pesca`),
         ANO= year(`Fecha pesca`))

# seleccionar datos a utilizar.
dataoct2 <- dataoct1 %>% 
  select(c(22, 21, 12, 13, 14, 15, 16, 17, 18)) %>% 
  filter(dataoct1$`Peso (gr)`<4000,
         dataoct1$`Peso (gr)`>10) 

summary(dataoct2)
plot1 <- ggplot(dataoct2,
                aes(dataoct2$`talla (mm)`, dataoct2$`Peso (gr)`, 
                    color=Sexo,
                    group=Sexo))+
  geom_point()+
  geom_smooth(method = "lm")
plot1
#Represento talla peso y sexo con colores, eliminando 2 outlyers.

# tipo de objeto
class(dataoct2)
install.packages("Matrix")
install.packages("sizeMat")
library(sizeMat)

classify_data = classify_mature(dataoct2, varNames = c("Peso (gr)","talla (mm)"), 
                                varSex = "Sexo", 
                                selectSex = NULL, 
                                method = "ld")


par(mfrow = c(2,2))
plot(classify_data)

plot(classify_data, xlab = "talla (mm.)", ylab = "weight (g)", legendPlot = FALSE)
legend("topleft", "Put your legend here", bty = "n")

plot(classify_data, xlab = "talla (mm.)", ylab = "weight (g)", 
     col = c(2, 3), pch = c(5, 6), legendPlot = TRUE)

plot(classify_data, xlab = "talla (mm.)", ylab = "weight (g)", 
     col = c(2, 3), pch = c(5, 6), lty_lines = c(1, 2), lwd_lines = c(1, 3), 
     cex = c(1, 3), main = "Classification")

my_ogive_fq = morph_mature(classify_data, method = "fq", niter = 1000)
print(my_ogive_fq)
#Frequentist regression 

par(mfrow = c(2,2))
plot(my_ogive_fq, xlab = "weight (g)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))
#Plot maturity ogive.
dev.off()
plot(my_ogive_fq, xlab = "weight (g)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"), onlyOgive = TRUE)
#solo ploteo la ogiva 

# seleccionamos solo LOS MACHOS.
library(dplyr)
dataoct3 <- dataoct2 %>% 
  filter(dataoct2$`Sexo`=="1")
dim(dataoct3)
glimpse(dataoct3)

classify_data3 = classify_mature(dataoct3, varNames = c("Peso (gr)","talla (mm)"), 
                                 varSex = "Sexo", selectSex = NULL, method = "ld")


par(mfrow = c(2,2))
plot(classify_data3)

plot(classify_data3, xlab = "weight (g)", ylab = "talla (mm)", legendPlot = FALSE)
legend("topleft", "Put your legend here", bty = "n")

plot(classify_data3, xlab = "weight (g)", ylab = "talla (mm)", 
     col = c(2, 3), pch = c(5, 6), legendPlot = TRUE)

plot(classify_data3, xlab = "weight (g)", ylab = "talla (mm)", 
     col = c(2, 3), pch = c(5, 6), lty_lines = c(1, 2), lwd_lines = c(1, 3), 
     cex = c(1, 3), main = "Classification")

my_ogive_fq = morph_mature(classify_data3, method = "fq", niter = 1000)
print(my_ogive_fq)
#Frequentist regression 

par(mfrow = c(2,2))
plot(my_ogive_fq, xlab = "weight (g)", ylab = "Proportion mature", col = c("blue", "red"))
#Plot maturity ogive.
dev.off()
plot(my_ogive_fq, xlab = "weight (g)", ylab = "Proportion mature", col = c("blue", "red"), onlyOgive = TRUE)
#solo ploteo la ogiva 

# seleccionamos solo LAS HEMBRAS.
dataoct4 <- dataoct2 %>% 
  filter(dataoct2$`Sexo`=="2")
dim(dataoct4)
glimpse(dataoct4)

classify_data4 = classify_mature(dataoct4, varNames = c("Peso (gr)","talla (mm)"), 
                                 varSex = "Sexo", selectSex = NULL, method = "ld")


par(mfrow = c(2,2))
plot(classify_data4)

plot(classify_data, xlab = "weight (g)", ylab = "talla (mm)", legendPlot = FALSE)
legend("topleft", "Put your legend here", bty = "n")

plot(classify_data, xlab = "weight (g)", ylab = "talla (mm)", 
     col = c(2, 3), pch = c(5, 6), legendPlot = TRUE)

plot(classify_data, xlab = "weight (g)", ylab = "talla (mm)", 
     col = c(2, 3), pch = c(5, 6), lty_lines = c(1, 2), lwd_lines = c(1, 3), 
     cex = c(1, 3), main = "Classification")

my_ogive_fq = morph_mature(classify_data, method = "fq", niter = 1000)
print(my_ogive_fq)
#Frequentist regression 

par(mfrow = c(2,2))
plot(my_ogive_fq, xlab = "weight (g)", ylab = "Proportion mature", col = c("blue", "red"))
#Plot maturity ogive.
dev.off()
plot(my_ogive_fq, xlab = "weight (g)", ylab = "Proportion mature", col = c("blue", "red"), onlyOgive = TRUE)
#solo ploteo la ogiva 


#Introducimos la variable maduro/inmaduro.

#machos (dataoct3)
my_ogive_fq = gonad_mature(dataoct3, varNames = c("Peso (gr)", "Maduro/inmaduro"),
                           inmName = "Inmaduro",
                           matName = "Maduro", 
                           method = "fq", 
                           niter = 999)
print(my_ogive_fq)
par(mfrow = c(2,2))
plot(my_ogive_fq, 
     xlab = "Peso (gr)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))

#hembras (dataoct4)
my_ogive_fq = gonad_mature(dataoct4, varNames = c("Peso (gr)", "Maduro/inmaduro"),
                           inmName = "Inmaduro",
                           matName = "Maduro", 
                           method = "fq", 
                           niter = 999)
print(my_ogive_fq)
par(mfrow = c(2,2))
plot(my_ogive_fq, 
     xlab = "Peso (gr)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))

#Para machos cogemos como inmaduros el 1, 2a, 2b y COMO MADUROS 3A Y 3B.
dataoct3$`Madurez nueva`<-as.factor(dataoct3$`Madurez nueva`)
str(dataoct3)
my_ogive_fq = gonad_mature(dataoct3, varNames = c("Peso (gr)", "Madurez nueva"),
                           inmName = c("1","2a", "2b" ),
                           matName = c("3a","3b"),
                           method = "fq", 
                           niter = 999)
print(my_ogive_fq)
par(mfrow = c(2,2))
plot(my_ogive_fq, 
     xlab = "Peso (gr)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))


#Para HEMBRAS cogemos como inmaduros el 1, 2a y COMO MADUROS 2b, 3A Y 3B.
dataoct4$`Madurez nueva`<-as.factor(dataoct4$`Madurez nueva`)
str(dataoct4)
my_ogive_fq = gonad_mature(dataoct4, varNames = c("Peso (gr)", "Madurez nueva"),
                           inmName = c("1","2a"),
                           matName = c("2b","3a","3b"),
                           method = "fq", 
                           niter = 999)
print(my_ogive_fq)
par(mfrow = c(2,2))
plot(my_ogive_fq, 
     xlab = "Peso (gr)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))


#datos machos 2023

dataoctM23 <- dataoct2 %>% 
  filter(dataoct2$`Sexo`=="1",
         ANO==2023)
dim(dataoctM23
)

dataoctM23$`Madurez nueva`<-as.factor(dataoctM23$`Madurez nueva`)
my_ogive_fq = gonad_mature(dataoctM23, varNames = c("Peso (gr)", "Madurez nueva"),
                           inmName = c("1","2a"),
                           matName = c("2b" ,"3a","3b"),
                           method = "fq", 
                           niter = 999)
print(my_ogive_fq)
par(mfrow = c(2,2))
plot(my_ogive_fq, 
     xlab = "Peso (gr)", 
     ylab = "Proportion mature", 
     col = c("blue", "red"))


dataoctM23 <- dataoct2 %>% 
  filter(dataoct2$`Sexo`=="2",
         ANO==2023)

#Para ver como son los datos según los años y los meses.
table(dataoct2$ANO, dataoct2$MES
)