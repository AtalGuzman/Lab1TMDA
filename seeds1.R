require(mclust)
require(Hmisc)
require(ggplot2)

#Funciones utilizadas
QQPLOT <- function(data){
  
  sample <- data[,1:7]
  x <- qnorm(c(0.25,0.75))
  y <- quantile(data[,1:7], c(0.25,0.75))
  m <- diff(y)/diff(x)
  int <- y[1] - m * x[1]
  p <- ggplot(data, aes(sample = data[,1:7]))+
    stat_qq()+
    geom_abline(slope = m, intercept = int,color = "blue")
  p
  }

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\usuario\\Documents\\1.- Universidad\\nivel 10\\Taller de minería de datos avanzada\\Laboratorio 1\\seeds_dataset.txt", 
                      col.names = c("A","P","C","LoK","WoK","AC","LoKG","Class"))

#Limpieza de datos
#   Encontrar datos pérdidos

is.na(seedsCC[,1:8])

#Enriquecimiento
#   Qué se le puede agregar a los datos?


#Codificación
#   Se transforma la clase que es leída como un número

seedType <- c("Kama","Rosa","Canadian")
seedsCC[8] <- factor(seedType)
seedsSC <- seedsCC[,1:7]

#https://web.ua.es/es/lpa/docencia/analisis-estadistico-de-datos-geoquimicos-con-r/estadistica-descriptiva-y-test-de-normalidad.html

#Análisis estadístico
#Descripción básica de los datos

describe(seedsSC)

#Test de normalidad
#Se prueba gráficamente

sample <- seedsSC[,1]
x <- qnorm(c(0.25,0.75))
y <- quantile(sample, c(0.25,0.75))
m <- diff(y)/diff(x)
int <- y[1] - m * x[1]

p <- ggplot(seedsSC, aes(sample = sample))+stat_qq()+geom_abline(slope = m, intercept = int,color = "blue")
