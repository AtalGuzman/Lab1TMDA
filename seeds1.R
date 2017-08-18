require(mclust)
require(Hmisc)
require(ggplots)

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\Natalia\\Google Drive\\2017 - 2\\Minería de Datos Avanzada\\IX.- grupo 4 seeds\\seeds_dataset.txt", 
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
qqline(seedsSC[1:7])

