library(mclust)
library(psych)
library(ggplot2)
library(grid)
library(gridExtra)
library("fitdistrplus")


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
  p <- ggplot(data = data,aes(sample = data[,i], color = Class))+stat_qq()+geom_abline(aes(slope =slope, intercept = intercept),color ="blue")
  p <- p + ggtitle(paste("Q-Q PLOT",colnames(data[i]),data$Class,sep =" ")) + xlab(xlab) + ylab(paste("Muestra de ",colnames(data[i])))
  return(p) #<- p + geom_point(aes(color = Class))
}

#Funci�n que genera los gr�ficos de caja de los datos ingresados.
#data: conjunto de datos
#i: �ndice del atributo del conjunto de datos
#retorna el gr�fico asociado

BOXPLOT <- function(data,i){
  p <- ggplot(data = data,aes(x = Class, y = data[i],fill = Class))
  p <- p + stat_boxplot()
  p <- p + ggtitle(paste("Box-plot",colnames(data[i])))+ylab(colnames(data[i]))
  return(p)
}

#Funci�n que genera los test de normalidad para determinar si es necesario aplicar test param�tricos
#o test no param�tricos 
#Recibe como entrada un vector de datos de alguna variable
NORMALITY <- function(data){
  n <- length(data)
  test <- ks.test(data,"pnorm",mean = mean(data), sd = sd(data))
  fitdist(data,"norm")
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

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\Usuario\\Documents\\1. Universidad\\Nivel 10\\T�pico II - Miner�a de Datos Avanzados\\Lab1TMDA\\seeds_dataset.txt", 
                      col.names = c("�rea","Per�metro","Compacidad","LoK","WoK","Asimetr�a","LoKG","Class"))


#       "Preprocesamiento"
#Limpieza de datos
#   Encontrar datos p�rdidos

is.na(seedsCC[,1:8])

#Enriquecimiento
#   Nada que agregar ni modificar :c

#Codificaci�n
#   Se transforma la clase que es le�da como un n�mero

seedType <- c("Kama","Rosa","Canadian")
seedsCC[8] <- factor(seedType)
seedsSC <- seedsCC[,1:7]

#https://web.ua.es/es/lpa/docencia/analisis-estadistico-de-datos-geoquimicos-con-r/estadistica-descriptiva-y-test-de-normalidad.html

#       "Descripci�n estad�stica"
#Descripci�n b�sica de los datos
print(describe(seedsSC))

#       An�lisis estad�stico e inferencial

#Test de normalidad
#Recordar que si p-value <= alfa rechazar Ho
#             si p-value > alfa aceptar Ho 
#Num�rico
test1.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],1)
test1.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],1)
test1.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],1)

test2.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],2)
test2.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],2)
test2.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],2)

test3.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],3)
test3.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],3)
test3.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],3)

test4.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],4)
test4.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],4)
test4.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],4)

test5.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],5)
test5.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],5)
test5.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],5)

test6.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],6)
test6.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],6)
test6.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],6)

test7.1 <- NORMALITY(seedsCC[seedsCC$Class == "Canadian",],7)
test7.2 <- NORMALITY(seedsCC[seedsCC$Class == "Kama",],7)
test7.3 <- NORMALITY(seedsCC[seedsCC$Class == "Rosa",],7)

#Gr�ficos de los datos. QQPLOT, para ver la normalidad gr�ficamente 
#y boxplot para detectar posibles diferencias que se detectar�n con test de diferencias de medias 
#(param�trica o no par�metrica seg�n sea el caso)

#�rea
p1.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],1)
p1.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],1)
p1.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],1)
p1.4 <- BOXPLOT(seedsCC,1)

#Per�metro
p2.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],2)
p2.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],2)
p2.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],2)
p2.4 <- BOXPLOT(seedsCC,2)

#Compacidad
p3.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],3)
p3.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],3)
p3.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],3)
p3.4 <- BOXPLOT(seedsCC,3)

#Largo de N�cleo
p4.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],4)
p4.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],4)
p4.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],4)
p4.4 <- BOXPLOT(seedsCC,4)

#ANcho de N�cleo
p5.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],5)
p5.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],5)
p5.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],5)
p5.4 <- BOXPLOT(seedsCC,5)

#Asimetr�a
p6.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],6)
p6.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],6)
p6.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],6)
p6.4 <- BOXPLOT(seedsCC,6)

#Largo de Estr�a del N�cleo
p7.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],7)
p7.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],7)
p7.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],7)
p7.4 <- BOXPLOT(seedsCC,7)

#Se grafican los datos entregados
multiplot(p1.1,p1.2,p1.3, p1.4, cols = 2)
multiplot(p2.1,p2.2,p2.3,p2.4,cols = 2)
multiplot(p3.1,p3.2,p3.3,p3.4,cols = 2)
multiplot(p4.1,p4.2,p4.3,p4.4,cols = 2)
multiplot(p5.1,p5.2,p5.3,p5.4,cols = 2)
multiplot(p6.1,p6.2,p6.3,p6.4,cols = 2)
multiplot(p7.1,p7.2,p7.3,p7.4,cols = 2)

# ANOVA
