
#### TRABAJO ENTREGABLE FINAL MODULO 5


#############################################################################
#################### CLÚSTER JERARQUICOS ####################################

# Carga de las librerias
library(openxlsx)
library(cluster)
library(usethis)
library(devtools)
library(ggplot2)
library(factoextra)
library(fpc)
library(dplyr)
library(NbClust)

file.choose()

data<-read.xlsx("C:\\Users\\CompuStore\\Desktop\\CURSOS\\CIENCIA DE DATOS\\MODULO 5\\parte 2\\BOL_BP_MAY_ 2017.xlsx",
                sheet = "INDICADORES")

# Eliminamos filas
data<-slice(data, -c(1:3, 5))

#Eliminamos las columnas de bancos de totales
data<-data[,-1]
data<-data[,-6]
data<-data[,-15]
data<-data[,-c(25:30)]


#Seleccionamos las variables a utilizar 

data<-data %>%
  filter(data$INDICADORES.FINANCIEROS=="NOMBRE DEL INDICADOR"|
    data$INDICADORES.FINANCIEROS=="ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS"|
           data$INDICADORES.FINANCIEROS=="MOROSIDAD DE LA CARTERA TOTAL"|
           data$INDICADORES.FINANCIEROS=="GASTOS DE OPERACION  / MARGEN FINANCIERO"|
           data$INDICADORES.FINANCIEROS=="RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO"|
           data$INDICADORES.FINANCIEROS=="FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO")

# Trasponemos nuestra base
data<- data.frame(t(data))

# Convertimos la fila en el nombre de la columna
colnames(data) <- data[1, ]
data <- data[-1, ]

#Guardamos en una variable el nombre de los bancos
nombres<-data$`NOMBRE DEL INDICADOR`


#Ponemos en formato numerico
base<-as.data.frame(lapply(data[,-1], as.numeric))

# Estandarizamos 
base<-data.frame(scale(base))

#Convertimos como indice a los nombres
row.names(base)<-nombres

# Distancias - Euclideana, eclideana^2 , manhattan
# Metodo para separación
# Distintas distancias- distintos dendogramas 

# Cluster Jerarquico - distancia Euclidean y metodo WAR.D

cluster<-hclust(dist(base, method = "euclidean"),
                method = "ward.D")

plot(cluster, hang = 0.01, cex=0.8)

# El dendograma sirve para clasifcar a un grupo dentro de un cluster

# Cambiamos el método 

cluster2<-hclust(dist(base, method = "manhattan"),
                 method = "average")

plot(cluster2, hang = 0.01, cex=0.8)

# En una sola ventana los 2 graficos 

par(mfrow=c(1,2))

cluster<-hclust(dist(base, method = "euclidean"),
                method = "ward.D")

plot(cluster, hang = 0.01, cex=0.8)

cluster2<-hclust(dist(base, method = "manhattan"),
                 method = "average")

plot(cluster2, hang = 0.01, cex=0.8)

## ¿A qué distancia se encuentran los elementos?
## Distancia euclideana 
distancia<- dist(base, method = "euclidean")
distancia

# ¿A qué grupo pertenecen los clúster?

cluster$merge

## ¿A qué distancia se encuentran los elementos?
## Distancia euclideana 
distancia2<- dist(base, method = "manhattan")
distancia2

# ¿A qué grupo pertenecen los clúster?

cluster2$merge

# Realizando cortes

cutree(cluster, k=3)

plot(cluster, hang = 0.01, cex=0.8)
rect.hclust(cluster, k=3, border = "red")

# ¿A que grupo pertenecen?

grupos<-as.data.frame(cutree(cluster, k=3))
grupos

## cluster 

ncluster<-diana(base, metric="euclidean")

par(mfrow=c(1,2))

plot(ncluster)
# Coeficiente de disividalidad 
# mientras mas cercano a 1 modelos bien hechos 


cluster1<- hcut(base, k=3, stand=TRUE,
                hc_metric = "euclidean", 
                hc_method = "ward.D")
fviz_dend(cluster1, rect=1, cex= 0.5,
          k_colors = c("blue", "red", "orange")
)


###############################################################################
###################### CLÚSTER K-MEDIAS #######################################


## cluster
cnj<-kmeans(base,3)
cnj

#Media de cada cluster 

cnj$centers

# Usando agregate 

aggregate(base, by= list(cnj$cluster), FUN = mean)

# Como se ven los grupos 

fviz_cluster(cnj, data=base)

require(cluster)
clusplot(base, cnj$cluster,
         color=T, shade = T,
         labels = 2, lines = 2)

# ¿Cuantos cluster son los optimos?
#Numero de cluster optimos 

clustoptim<-NbClust(base, distance = "euclidea",
                    min.nc = 2, max.nc = 6,
                    method = "ward.D", index = "all")

fviz_nbclust(clustoptim)

# Evaluamos el cluster
cnj2<-kmeans(base,3)
silueta<- silhouette(cnj2$cluster, dist(base, method = "euclidean"))
fviz_silhouette(silueta)

# Permite conocer en promedio el ancho de la clasificacion por cluster 
#evaluar promedio que tan bien esta clasificando en el numero de cluster que uno esta diciendo

# La linea con rojo es el promedio. Primero deben ser positivos 
# Si hay negativos hay que ver los problemas en la base, o el numero de cluster no es el correcto

# La linea roja, las barras deben ser lo mas altas posibles

# Se recomienda probar con mas cluster 
# Revisar valores atipicos 
# Revisar que las variables sean "las correctas"
# Incorporar mas variables al analisis cluster 
# O por ultimo, cambiar el enfoque

