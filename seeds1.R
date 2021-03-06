library(mclust)
library(psych)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgl)
require(graphics)

#####################################
#     Declaraci�n de funciones      #
#####################################

#Funci�n que recibe como entrada un sistema de informaci�n y el �ndice de la variable a estudiar
#de este modo se genera el gr�fico cuantil-cuantil asociado, para verificar gr�ficamente la normalidad
#data: conjunto de datos
#i: �ndice del atributo del conjunto de datos
#retorna el gr�fico asociado

QQPLOT <- function(data, i){
  colname <-colnames(data[i])
  xlab <- "Distribuci�n te�rica"
  ylab <- paste("Muestra de ",colnames(data[i]))
  x <- qnorm(c(0.25,0.75))
  y <- quantile(data[,i],c(0.25,0.75))
  slope <- diff(y)/diff(x)
  intercept <- y[1]-slope*x[1]
  
  p <- ggplot(data = data,aes(sample = data[,i],color = Clase))+stat_qq()+geom_abline(aes(slope =slope, intercept = intercept), color = "blue")
  p <- p + ggtitle(paste("Q-Q PLOT",colnames(data[i]),data$Clase,sep =" "))+ xlab(xlab) + ylab(paste("Muestra de ",colnames(data[i])))
  return(p)
}

#Funci�n que genera los gr�ficos de caja de los datos ingresados.
#data: conjunto de datos
#i: �ndice del atributo del conjunto de datos
#retorna el gr�fico asociado

BOXPLOT <- function(dato,i){
  p <- ggplot(data = dato,aes(x = Class, y = dato[i],fill = Class))
  p <- p + stat_boxplot()
  p <- p + ggtitle(paste("Box-plot",colnames(dato[i])))+ylab(colnames(dato[i]))
  p <- p+ scale_fill_discrete(name="Clase",
                              breaks=c("1", "2", "3"),
                              labels=c("Canadian", "Kama", "Roma"))
  return(p)
}

#Funci�n que genera los test de normalidad para determinar si es necesario aplicar test param�tricos
#o test no param�tricos 
#Recibe como entrada un vector de datos de alguna variable
NORMALITY <- function(data){
  test <- ks.test(data,"pnorm",mean = mean(data), sd = sd(data))
  return(test)  
}

#Funci�n de libre uso puesta a disposici�n por el grupo de programadores de R, COOK-BOOK-R
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

names <- c("�rea","Per�metro","Compacidad","LoK","WoK","Asimetr�a","LoKG","Class")

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\Natalia\\Google Drive\\2017 - 2\\Miner�a de Datos Avanzada\\IX.- grupo 4 seeds\\seeds_dataset.txt", 
                      col.names = names)

#seedsCC <- read.table("C:\\Users\\Usuario\\Documents\\1. Universidad\\Nivel 10\\T�pico II - Miner�a de Datos Avanzados\\Lab1TMDA\\seeds_dataset.txt", 
#                      col.names = names)

#seedsCC <- read.table("C:\\Users\\usuario\\Documents\\1.- Universidad\\nivel 10\\Taller de miner�a de datos avanzada\\Laboratorio 1\\seeds_dataset.txt", 
     #                 col.names = names)

seedsCC$Class <- as.factor(seedsCC$Class)
seedsCC$Clase[seedsCC$Class == 1] <- "Canadian"
seedsCC$Clase[seedsCC$Class == 2] <- "Kama"
seedsCC$Clase[seedsCC$Class == 3] <- "Roma"


#       "Preprocesamiento"
#Limpieza de datos
#   Encontrar datos p�rdidos

is.na(seedsCC[,1:8])

#Enriquecimiento
#   Nada que agregar ni modificar :c

#Codificaci�n
#   Se transforma la clase que es le�da como un n�mero

seedType <- c("Kama","Rosa","Canadian")
seedsSC <- seedsCC[,1:7]

#https://web.ua.es/es/lpa/docencia/analisis-estadistico-de-datos-geoquimicos-con-r/estadistica-descriptiva-y-test-de-normalidad.html

#       "Descripci�n estad�stica"
#Descripci�n b�sica de los datos
cat("\n#####    DESCRIPCI�N ESTAD�STICA    #####\n\n")
print(describe(seedsSC))

cat("\n#####    TEST DE NORMALIDAD   #####\n\n")
#       An�lisis estad�stico e inferencial

#Test de normalidad
#Recordar que si p-value <= alfa rechazar Ho
#             si p-value > alfa aceptar Ho  
#Dicho en palabras un bajo valor de P, significa que la muestra provee suficiente evidencia 
#como para rechaza la hip�tesis nula
#Num�rico
#Test de normalidad num�ricos para cada una de las caracter�sticas

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


#Gr�ficos de los datos. QQPLOT, para ver la normalidad gr�ficamente 
#y boxplot para detectar posibles diferencias que se detectar�n con test de diferencias de medias 
#(param�trica o no par�metrica seg�n sea el caso)

#�rea
p1.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],1)
p1.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],1)
p1.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],1)
p1.4 <- BOXPLOT(seedsCC,1)

#Per�metro
p2.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],2)
p2.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],2)
p2.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],2)
p2.4 <- BOXPLOT(seedsCC,2)

#Compacidad
p3.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],3)
p3.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],3)
p3.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],3)
p3.4 <- BOXPLOT(seedsCC,3)

#Largo de N�cleo
p4.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],4)
p4.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],4)
p4.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],4)
p4.4 <- BOXPLOT(seedsCC,4)

#ANcho de N�cleo
p5.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],5)
p5.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],5)
p5.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],5)
p5.4 <- BOXPLOT(seedsCC,5)

#Asimetr�a
p6.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],6)
p6.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],6)
p6.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],6)
p6.4 <- BOXPLOT(seedsCC,6)

#Largo de Estr�a del N�cleo
p7.1 <- QQPLOT(seedsCC[seedsCC$Class == "1",],7)
p7.2 <- QQPLOT(seedsCC[seedsCC$Class == "2",],7)
p7.3 <- QQPLOT(seedsCC[seedsCC$Class == "3",],7)
p7.4 <- BOXPLOT(seedsCC,7)

#Realizar ANOVA
#Solo LoKG, necesita una prueba no param�trica Kruskal-Wallis y con post-hoc de U de mann-withneya
#Son m�s de 30 datos, entonces es robusto frente a la homocedasticidad
#LOs factores son los tipos de semillas
#Dado que son todas semillas diferentes, se opta por la utilizaci�n de un anova entre sujetos (no medidas repetidas)

attach(seedsCC)
aov.area <-       aov(�rea~Class, na.action = na.exclude)
aov.perimetro <-  aov(Per�metro~Class, na.action = na.exclude)
aov.compacidad <- aov(Compacidad~Class, na.action = na.exclude)
aov.lok <-        aov(LoK~Class, na.action = na.exclude)
aov.wok <-        aov(WoK~Class, na.action = na.exclude)
aov.asimetria <-  aov(Asimetr�a~Class, na.action = na.exclude)
aov.lokg <-       kruskal.test(LoKG~Class, na.action = na.exclude)

p.value.aov <- c(anova(aov.area)$`Pr(>F)`[1],anova(aov.perimetro)$`Pr(>F)`[1],anova(aov.compacidad)$`Pr(>F)`[1],
                 anova(aov.lok)$`Pr(>F)`[1],
                 anova(aov.wok)$`Pr(>F)`[1],anova(aov.asimetria)$`Pr(>F)`[1],
                 aov.lokg$p.value)

result.aov <- data.frame(names[1:7],p.value.aov)
result.aov$sig[!result.aov$p.value.aov > 0.05] <- "No hay diferencias significativas"
result.aov$sig[result.aov$p.value.aov <= 0.05] <- "S� hay diferencias significativas"

cat("\n####   RESULTADOS DE AN�LISIS DE VARIANZA    ####\n\n")
print(result.aov)

#Realizaci�n de test de POST-HOC
TukeyHSD(aov.area)
TukeyHSD(aov.perimetro)
TukeyHSD(aov.compacidad)
TukeyHSD(aov.lok)
TukeyHSD(aov.wok)
TukeyHSD(aov.asimetria)

#Se grafican los datos entregados

cat("\n\n####   GENERACI�N DE GR�FICOS   ####\n")
cat("\n####   ESPERE UN MOMENTO   ####\n")
multiplot(p1.1,p1.2,p1.3, p1.4, cols = 2)
multiplot(p2.1,p2.2,p2.3,p2.4,cols = 2)
multiplot(p3.1,p3.2,p3.3,p3.4,cols = 2)
multiplot(p4.1,p4.2,p4.3,p4.4,cols = 2)
multiplot(p5.1,p5.2,p5.3,p5.4,cols = 2)
multiplot(p6.1,p6.2,p6.3,p6.4,cols = 2)
multiplot(p7.1,p7.2,p7.3,p7.4,cols = 2)

###########################################
#     REALIZACI�N DE AGRUPAMIENTO         #
###########################################
cat("\n#####    AGRUPAMIENTO    #####\n\n")

#Funci�n que se encarga de calcular el mclust, dado el grado de claster
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

#Hacer el gr�fico de BIC
#mientras mayor es el resultado de Bic, mejor es la clasificacion
BIC= mclustBIC(seedsCC[,1:7], prior= priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC, main = "") 
title(main="Gr�fico de los BIC por configuraci�n de par�metros")
summary(BIC) #presentan los mejores valores

#en base al mejor valor segun BIC (los tres mejores)
mejorBIC1 = Mclust(seedsSC, modelNames="EEV", G=3)
summary(mejorBIC1)
plot(mejorBIC1, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo EEV")
mejorBIC2 = Mclust(seedsSC, modelNames="VEV", G=3)
summary(mejorBIC2)
plot(mejorBIC2, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo VEV")
mejorBIC3 = Mclust(seedsSC[3:5], modelNames="VVV", G=3)
summary(mejorBIC3)
plot(mejorBIC3, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo VVV")

#comparaci�n de clasificacion 
#la clase que deber�a ser versus la clase que es
table (seedsCC$Clase, mejorBIC1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, mejorBIC2$classification)
table (seedsCC$Clase, mejorBIC3$classification)

#para comparar con el peor en 3 grupos:
peorBIC1= Mclust(seedsSC, modelNames ="EII", G=3 )
plot(peorBIC1, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo EII")
peorBIC2= Mclust(seedsSC[3:5], modelNames ="VII", G=3 )
plot(peorBIC2, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo VII")

table (seedsCC$Clase, peorBIC1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, peorBIC2$classification)

#Comparar con ICL
ICL = mclustICL(seedsSC)
summary(ICL)
plot(ICL)
title(main="Gr�fico de los ICL por configuraci�n de par�metros")


mejorICL1 = Mclust(seedsSC[3:5], modelNames="VEV", G=3)
summary(mejorICL1)
plot(mejorICL1, what="classification")
title(main="Clasificaci�n con 3 grupos y modelo VEV")
mejorICL2 = Mclust(seedsSC, modelNames="EEV", G=5)
summary(mejorICL2)
plot(mejorICL2, what="classification")
title(main="Clasificaci�n con 5 grupos y modelo EEV")
mejorICL3 = Mclust(seedsSC, modelNames="EEV", G=3) #igual en bic
summary(mejorICL3)
plot(mejorICL3, what="classification")
title(main="Clasificaci�n con 3 grupos y modelo EEV")

table(seedsCC$Clase, mejorICL1$classification)#districionde clases por cada grupo.
table (seedsCC$Clase, mejorICL2$classification)
table (seedsCC$Clase, mejorICL3$classification)

#Hacer agrupamiento K-medias
MacQueen <- kmeans(seedsSC[3:5], 3, algorithm = "MacQueen")
plot(seedsSC[3:5], col = MacQueen$cluster)
title(main="Clasificaci�n con k-medias")
plot(seedsSC[3:5], col=seedsCC$Class)
points(MacQueen$centers, col = "blue", pch = 12)
title(main="Clasificaci�n real")
MacQueen$centers
help(points)

HertiganWong <- kmeans(seedsSC, 3, algorithm = "Hartigan-Wong")
plot(seedsSC, col = HertiganWong$cluster)
points(HertiganWong$centers, col = 1:2, pch = 8, cex = 2)
title(main="Clasificaci�n con k-medias, algoritmo Hertigan-Wong")

Lloyd <- kmeans(seedsSC, 3, algorithm = "Lloyd")
plot(seedsSC, col = Lloyd$cluster)
points(Lloyd$centers, col = 1:2, pch = 8, cex = 2)
title(main="Clasificaci�n con k-medias, Lloyd")

Forgy <- kmeans(seedsSC, 3, algorithm = "Forgy")
plot(seedsSC, col = Forgy$cluster)
points(Forgy$centers, col = 1:2, pch = 8, cex = 2)
title(main="Clasificaci�n con k-medias, Forgy")

#Comparaci�n entre los distintos algoritmos de k-medias
table(seedsCC$Clase,MacQueen$cluster)
table(seedsCC$Clase,HertiganWong$cluster)
table(seedsCC$Clase,Lloyd$cluster)
table(seedsCC$Clase,Forgy$cluster)



#Analisis de componentes principales
pairs(seedsSC) #correlaci�n gr�fica entre diferentes variables
cor(seedsSC) #tabla de correlaci�n

round(cor(seedsSC),2)
mejores <-seedsSC[,c(1,6,7)]
round(cor(mejores),2)

#comp= princomp(seedsSC, cor=TRUE, score=TRUE)
#summary(comp)
#plot(comp, type="lines")
#biplot(comp)
attach(mejores)
library(rgl)
plot3d(mejores, col=seedsCC$Class, pch=21,
       xlab="Area", ylab="Asimetria", zlab="LoKG")

BIC= mclustBIC(mejores, prior= priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC, main = "") 
title(main="Gr�fico de los BIC por configuraci�n de par�metros")
summary(BIC) #presentan los mejores valores

mejordelavida= Mclust(mejores, modelNames ="EEE", G=3 )
plot(mejordelavida, what="classification", main="")
title(main="Clasificaci�n con 3 grupos y modelo EEE")
mejordelavida2= Mclust(mejores, modelNames ="EEE", G=3 )
plot(mejordelavida2, what="classification", main="")
title(main="Clasificaci�n con 5 grupos y modelo EEE")

table (seedsCC$Clase, mejordelavida$classification)
table (seedsCC$Clase, mejordelavida$classification)

plot3d(mejores ,col=mejordelavida$classification, pch=21,
       xlab="Area", ylab="Asimetria", zlab="LoKG")
mejordelavida$classification
