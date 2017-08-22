library(mclust)
library(psych)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgl)
require(graphics)

#####################################
#     Declaración de funciones      #
#####################################

#Función que recibe como entrada un sistema de información y el índice de la variable a estudiar
#de este modo se genera el gráfico cuantil-cuantil asociado, para verificar gráficamente la normalidad
#data: conjunto de datos
#i: índice del atributo del conjunto de datos
#retorna el gráfico asociado

QQPLOT <- function(data, i){
  colname <-colnames(data[i])
  xlab <- "Distribución teórica"
  ylab <- paste("Muestra de ",colnames(data[i]))
  x <- qnorm(c(0.25,0.75))
  y <- quantile(data[,i],c(0.25,0.75))
  slope <- diff(y)/diff(x)
  intercept <- y[1]-slope*x[1]
  
  p <- ggplot(data = data,aes(sample = data[,i],color = Clase))+stat_qq()+geom_abline(aes(slope =slope, intercept = intercept), color = "blue")
  p <- p + ggtitle(paste("Q-Q PLOT",colnames(data[i]),data$Clase,sep =" "))+ xlab(xlab) + ylab(paste("Muestra de ",colnames(data[i])))
  return(p)
}

#Función que genera los gráficos de caja de los datos ingresados.
#data: conjunto de datos
#i: índice del atributo del conjunto de datos
#retorna el gráfico asociado

BOXPLOT <- function(dato,i){
  p <- ggplot(data = dato,aes(x = Class, y = dato[i],fill = Class))
  p <- p + stat_boxplot()
  p <- p + ggtitle(paste("Box-plot",colnames(dato[i])))+ylab(colnames(dato[i]))
  p <- p+ scale_fill_discrete(name="Clase",
                              breaks=c("1", "2", "3"),
                              labels=c("Canadian", "Kama", "Roma"))
  return(p)
}

#Función que genera los test de normalidad para determinar si es necesario aplicar test paramétricos
#o test no paramétricos 
#Recibe como entrada un vector de datos de alguna variable
NORMALITY <- function(data){
  test <- ks.test(data,"pnorm",mean = mean(data), sd = sd(data))
  return(test)  
}

#Función de libre uso puesta a disposición por el grupo de programadores de R, COOK-BOOK-R
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###############
#     MAIN    #
###############

names <- c("Área","Perímetro","Compacidad","LoK","WoK","Asimetría","LoKG","Class")

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\Natalia\\Google Drive\\2017 - 2\\Minería de Datos Avanzada\\IX.- grupo 4 seeds\\seeds_dataset.txt", 
                      col.names = names)

#seedsCC <- read.table("C:\\Users\\Usuario\\Documents\\1. Universidad\\Nivel 10\\Tópico II - Minería de Datos Avanzados\\Lab1TMDA\\seeds_dataset.txt", 
#                      col.names = names)

#seedsCC <- read.table("C:\\Users\\usuario\\Documents\\1.- Universidad\\nivel 10\\Taller de minería de datos avanzada\\Laboratorio 1\\seeds_dataset.txt", 
     #                 col.names = names)

seedsCC$Class <- as.factor(seedsCC$Class)
seedsCC$Clase[seedsCC$Class == 1] <- "Canadian"
seedsCC$Clase[seedsCC$Class == 2] <- "Kama"
seedsCC$Clase[seedsCC$Class == 3] <- "Roma"


#       "Preprocesamiento"
#Limpieza de datos
#   Encontrar datos pérdidos

is.na(seedsCC[,1:8])

#Enriquecimiento
#   Nada que agregar ni modificar :c

#Codificación
#   Se transforma la clase que es leída como un número

seedType <- c("Kama","Rosa","Canadian")
seedsSC <- seedsCC[,1:7]

#https://web.ua.es/es/lpa/docencia/analisis-estadistico-de-datos-geoquimicos-con-r/estadistica-descriptiva-y-test-de-normalidad.html

#       "Descripción estadística"
#Descripción básica de los datos
cat("\n#####    DESCRIPCIÓN ESTADÍSTICA    #####\n\n")
print(describe(seedsSC))

cat("\n#####    TEST DE NORMALIDAD   #####\n\n")
#       Análisis estadístico e inferencial

#Test de normalidad
#Recordar que si p-value <= alfa rechazar Ho
#             si p-value > alfa aceptar Ho  
#Dicho en palabras un bajo valor de P, significa que la muestra provee suficiente evidencia 
#como para rechaza la hipótesis nula
#Numérico
#Test de normalidad numéricos para cada una de las características

test1.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",1])
test1.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",1])
test1.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",1])

test2.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",2])
test2.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",2])
test2.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",2])

test3.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",3])
test3.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",3])
test3.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",3])

test4.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",4])
test4.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",4])
test4.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",4])

test5.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",5])
test5.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",5])
test5.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",5])

test6.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",6])
test6.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",6])
test6.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",6])

test7.1 <- NORMALITY(seedsCC[seedsCC$Class == "1",7])
test7.2 <- NORMALITY(seedsCC[seedsCC$Class == "2",7])
test7.3 <- NORMALITY(seedsCC[seedsCC$Class == "3",7])

test.Canadian <-  c(test1.1$p.value, test2.1$p.value, test3.1$p.value, test4.1$p.value, test5.1$p.value, test6.1$p.value, test7.1$p.value)
test.Kama <-      c(test1.2$p.value, test2.2$p.value, test3.2$p.value, test4.2$p.value, test5.2$p.value, test6.2$p.value, test7.2$p.value)
test.Rosa <-      c(test1.3$p.value, test2.3$p.value, test3.3$p.value, test4.3$p.value, test5.3$p.value, test6.3$p.value, test7.3$p.value)

normal.test <- data.frame(names[1:7],test.Canadian,test.Kama,test.Rosa, row.names = 1)
result.test <- normal.test
result.test[result.test > 0.05] <- "Normal"
result.test[result.test <= 0.05] <- "No Normal"

print(normal.test)
cat("\n")
print(result.test)


#Gráficos de los datos. QQPLOT, para ver la normalidad gráficamente 
#y boxplot para detectar posibles diferencias que se detectarán con test de diferencias de medias 
#(paramétrica o no parámetrica según sea el caso)

#Área
p1.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],1)
p1.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],1)
p1.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],1)
p1.4 <- BOXPLOT(seedsCC,1)

#Perímetro
p2.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],2)
p2.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],2)
p2.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],2)
p2.4 <- BOXPLOT(seedsCC,2)

#Compacidad
p3.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],3)
p3.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],3)
p3.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],3)
p3.4 <- BOXPLOT(seedsCC,3)

#Largo de Núcleo
p4.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],4)
p4.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],4)
p4.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],4)
p4.4 <- BOXPLOT(seedsCC,4)

#ANcho de Núcleo
p5.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],5)
p5.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],5)
p5.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],5)
p5.4 <- BOXPLOT(seedsCC,5)

#Asimetría
p6.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],6)
p6.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],6)
p6.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],6)
p6.4 <- BOXPLOT(seedsCC,6)

#Largo de Estría del Núcleo
p7.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],7)
p7.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],7)
p7.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],7)
p7.4 <- BOXPLOT(seedsCC,7)

#Realizar ANOVA
#Solo LoKG, necesita una prueba no paramétrica Kruskal-Wallis y con post-hoc de U de mann-withneya
#Son más de 30 datos, entonces es robusto frente a la homocedasticidad
#LOs factores son los tipos de semillas
#Dado que son todas semillas diferentes, se opta por la utilización de un anova entre sujetos (no medidas repetidas)

attach(seedsCC)
aov.area <-       aov(Área~Class, na.action = na.exclude)
aov.perimetro <-  aov(Perímetro~Class, na.action = na.exclude)
aov.compacidad <- aov(Compacidad~Class, na.action = na.exclude)
aov.lok <-        aov(LoK~Class, na.action = na.exclude)
aov.wok <-        aov(WoK~Class, na.action = na.exclude)
aov.asimetria <-  aov(Asimetría~Class, na.action = na.exclude)
aov.lokg <-       kruskal.test(LoKG~Class, na.action = na.exclude)

p.value.aov <- c(anova(aov.area)$`Pr(>F)`[1],anova(aov.perimetro)$`Pr(>F)`[1],anova(aov.compacidad)$`Pr(>F)`[1],
                 anova(aov.lok)$`Pr(>F)`[1],
                 anova(aov.wok)$`Pr(>F)`[1],anova(aov.asimetria)$`Pr(>F)`[1],
                 aov.lokg$p.value)

result.aov <- data.frame(names[1:7],p.value.aov)
result.aov$sig[!result.aov$p.value.aov > 0.05] <- "No hay diferencias significativas"
result.aov$sig[result.aov$p.value.aov <= 0.05] <- "Sí hay diferencias significativas"

cat("\n####   RESULTADOS DE ANÁLISIS DE VARIANZA    ####\n\n")
print(result.aov)

#Realización de test de POST-HOC
TukeyHSD(aov.area)
TukeyHSD(aov.perimetro)
TukeyHSD(aov.compacidad)
TukeyHSD(aov.lok)
TukeyHSD(aov.wok)
TukeyHSD(aov.asimetria)

#Se grafican los datos entregados

cat("\n\n####   GENERACIÓN DE GRÁFICOS   ####\n")
cat("\n####   ESPERE UN MOMENTO   ####\n")
multiplot(p1.1,p1.2,p1.3, p1.4, cols = 2)
multiplot(p2.1,p2.2,p2.3,p2.4,cols = 2)
multiplot(p3.1,p3.2,p3.3,p3.4,cols = 2)
multiplot(p4.1,p4.2,p4.3,p4.4,cols = 2)
multiplot(p5.1,p5.2,p5.3,p5.4,cols = 2)
multiplot(p6.1,p6.2,p6.3,p6.4,cols = 2)
multiplot(p7.1,p7.2,p7.3,p7.4,cols = 2)

###########################################
#     REALIZACIÓN DE AGRUPAMIENTO         #
###########################################

#Función que se encarga de calcular el mclust, dado el grado de claster
#a considerar (g) y el modelo que puede ser "EII", etc.
MCLUST<- function(data, g, model){
  p=Mclust(seedsSC, G=g, prior=priorControl(functionName = "defaultPrior", shrinkage=0.1, modelNames=model))
  return(p)
}

#modelos a evaluar y comparar
model.names=c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EEV", "VEV", "VVV")

#1- Hacer todo variable.
#resumen de loglikehood dado el modelo y numero de clusters
#sirve para comparar entre distintos modelos y distintos grupos.
cruza.datos <- data.frame(Modelo=numeric(), Grupos=numeric(), LogLikehood=numeric(), Bic=numeric())
for(i in 1:10){
  for(j in 2:10){
    m<-MCLUST(seedsSC, g=j, model=model.names[i])
    cruza.datos <-rbind(cruza.datos, 
                        data.frame(
                          Modelo=model.names[i],
                          Grupos=j,
                          LogLikehood=m$loglik,
                          Bic=m$bic))
    #Para ir graficando todas las combinaciones D:
    #plot(m, what="classification")
    #legend("bottomright", legend=1:8, #numero de clusters
    #       col=mclust.options("classPlotColors"),
    #       pch=mclust.options("classPlotColors"),
    #       title="Class labels:")
    }
}
summary(cruza.datos)

#Hacer el gráfico de BIC
#mientras mayor es el resultado de Bic, mejor es la clasificacion
BIC= mclustBIC(seedsCC[,1:7], prior= priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)
summary(BIC) #presentan los mejores valores

#en base al mejor valor segun BIC (los tres mejores)
mejorBIC1 = Mclust(seedsSC, modelNames="EEV", G=3)
summary(mejorBIC1)
plot(mejorBIC1, what="classification")
mejorBIC2 = Mclust(seedsSC, modelNames="VEV", G=3)
summary(mejorBIC2)
plot(mejorBIC2, what="classification")
mejorBIC3 = Mclust(seedsSC, modelNames="VVV", G=3)
summary(mejorBIC3)
plot(mejorBIC3, what="classification")


#comparación de clasificacion 
#la clase que debería ser versus la clase que es
table (seedsCC$Clase, mejorBIC1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, mejorBIC2$classification)
table (seedsCC$Clase, mejorBIC3$classification)

#para comparar con el peor en 3 grupos:
peorBIC1= Mclust(seedsSC, modelNames ="EII", G=3 )
plot(peorBIC1, what="classification")
peorBIC2= Mclust(seedsSC, modelNames ="VII", G=3 )
plot(peorBIC1, what="classification")

table (seedsCC$Clase, peorBIC1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, peorBIC2$classification)

#Comparar con ICL
ICL = mclustICL(seedsSC)
summary(ICL)

mejorICL1 = Mclust(seedsSC, modelNames="VEV", G=5)
summary(mejorICL1)
plot(mejorICL1, what="classification")
mejorICL2 = Mclust(seedsSC, modelNames="EEV", G=5)
summary(mejorICL2)
plot(mejorICL2, what="classification")
mejorICL3 = Mclust(seedsSC, modelNames="EEV", G=3) #igual en bic
summary(mejorICL3)
plot(mejorICL3, what="classification")

table(seedsCC$Clase, mejorICL1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, mejorICL2$classification)
table (seedsCC$Clase, mejorICL3$classification)

#Hacer agrupamiento K-medias
MacQueen <- kmeans(seedsSC, 3, algorithm = "MacQueen")
plot(seedsSC, col = MacQueen$cluster)
points(MacQueen$centers, col = 1:2, pch = 8, cex = 2)

HertiganWong <- kmeans(seedsSC, 3, algorithm = "Hartigan-Wong")
plot(seedsSC, col = HertiganWong$cluster)
points(HertiganWong$centers, col = 1:2, pch = 8, cex = 2)

Lloyd <- kmeans(seedsSC, 3, algorithm = "Lloyd")
plot(seedsSC, col = Lloyd$cluster)
points(Lloyd$centers, col = 1:2, pch = 8, cex = 2)

Forgy <- kmeans(seedsSC, 3, algorithm = "Forgy")
plot(seedsSC, col = Forgy$cluster)
points(Forgy$centers, col = 1:2, pch = 8, cex = 2)

#Comparación entre los distintos algoritmos de k-medias
table(seedsCC$Clase,MacQueen$cluster)
table(seedsCC$Clase,HertiganWong$cluster)
table(seedsCC$Clase,Lloyd$cluster)
table(seedsCC$Clase,Forgy$cluster)



#Analisis de componentes principales
pairs(seedsSC) #correlación gráfica entre diferentes variables
cor(seedsSC) #tabla de correlación

round(cor(seedsSC),2)
comp= princomp(seedsSC, cor=TRUE, score=TRUE)
summary(comp)
plot(comp, type="lines")
biplot(comp)

library(rgl)
plot3d(seedsSC[1:3], col=seedsCC$Class, pch=21)
