library(mclust)
library(psych)
library(ggplot2)
library(grid)
library(gridExtra)

QQPLOT <- function(data, i){
  colname <-colnames(data[i])
  xlab <- "Distribución teórica"
  ylab <- paste("Muestra de ",colnames(data[i]))
  x <- qnorm(c(0.25,0.75))
  y <- quantile(data[,i],c(0.25,0.75))
  slope <- diff(y)/diff(x)
  intercept <- y[1]-slope*x[1]
  p <- ggplot(data = data,aes(sample = data[,i], color = Class))+stat_qq()+geom_abline(aes(slope =slope, intercept = intercept),color ="blue")
  p <- p + ggtitle(paste("Q-Q PLOT",colnames(data[i]),data$Class,sep =" ")) + xlab(xlab) + ylab(paste("Muestra de ",colnames(data[i])))
  return(p) #<- p + geom_point(aes(color = Class))
}

BOXPLOT <- function(data,i){
  p <- ggplot(data,aes(Class, data[i],color = Class))
  p <- p+geom_boxplot()
  p <- p+theme(plot.title = element_text(paste("Gráfico de Cajas y Bigotes para ",colnames(data[i]))),
               axis.title.y = element_text(colnames(data[i])))
  p 
  return(p)
}

NORMALITY <- function(data){
  n <- length(data)
  distribucion.teorica <- rnorm(n,mean = mean(data), sd = sd(data))
  test <- ks.test(data,"pnorm",mean = mean(data), sd = sd(data))
  return  
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

#Lectura del archivo
seedsCC <- read.table("C:\\Users\\Usuario\\Documents\\1. Universidad\\Nivel 10\\Tópico II - Minería de Datos Avanzados\\Lab1TMDA\\seeds_dataset.txt", 
                      col.names = c("Área","Perímetro","Compacidad","Largo de Núcleo","Ancho de núcleo","Asimetría","Largo de Estría del Núcleo","Class"))



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
print(describe(seedsSC))

#Test de normalidad
#Recordar que si p-value <= alfa rechazar Ho
#             si p-value > alfa aceptar Ho 
#Numérico
test1 <- NORMALITY(seedsCC[,1])
#NORMALITY(seedsCC[,2])
#NORMALITY(seedsCC[,3])
#NORMALITY(seedsCC[,4])
#NORMALITY(seedsCC[,5])
#NORMALITY(seedsCC[,6])
#NORMALITY(seedsCC[,7])
 
#Gráfico
#Área
p1.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],1)
p1.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],1)
p1.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],1)
p1.4 <- BOXPLOT(seedsCC,1)

#Perímetro
p2.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],2)
p2.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],2)
p2.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],2)
p2.4 <- BOXPLOT(seedsCC,2)

#Compacidad
p3.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],3)
p3.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],3)
p3.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],3)
p3.4 <- BOXPLOT(seedsCC,3)

#Largo de Núcleo
p4.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],4)
p4.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],4)
p4.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],4)
p4.4 <- BOXPLOT(seedsCC,4)

#ANcho de Núcleo
p5.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],5)
p5.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],5)
p5.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],5)
p5.4 <- BOXPLOT(seedsCC,5)

#Asimetría
p6.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],6)
p6.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],6)
p6.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],6)
p6.4 <- BOXPLOT(seedsCC,6)

#Largo de Estría del Núcleo
p7.1 <- QQPLOT(seedsCC[seedsCC$Class == "Canadian",],7)
p7.2 <- QQPLOT(seedsCC[seedsCC$Class == "Kama",],7)
p7.3 <- QQPLOT(seedsCC[seedsCC$Class == "Rosa",],7)
p7.4 <- BOXPLOT(seedsCC,7)

multiplot(p1.1,p1.2,p1.3, p1.4, cols = 2)

multiplot(p2.1,p2.2,p2.3,p2.4,cols = 2)

multiplot(p3.1,p3.2,p3.3,p3.4cols = 2)

multiplot(p4.1,p4.2,p4.3,p4.4,cols = 2)

multiplot(p5.1,p5.2,p5.3,p5.4,cols = 2)

multiplot(p6.1,p6.2,p6.3,p6.4,cols = 2)

multiplot(p7.1,p7.2,p7.3,p7.4,cols = 2)
