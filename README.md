# MCTC Estimating cost functions with r
Learning how github works
#########################MCTC##################################
#PRÁCTICA: ANÁLISIS DE COSTES EN LAS INFRAESTRUCTURAS##########
######################PORTUARIAS###############################


#¿COSTES O PRODUCCIÓN?

#¿QUÉ REPRESENTA LA FUNCIÓN DE COSTES?

#¿QUÉ PROPIEDADES DEBE CUMPLIR LA FUNCIÓN DE COSTES?

#¿QUÉ INFORMACIÓN DE INTERÉS PODEMOS OBTENER DE LA FUNCIÓN
#DE COSTES?


###############################################################
#                    CARGAR LOS DATOS                         #
###############################################################

getwd()                 #directorio donde va a buscar los datos

#si nuestros datos no están en ese directorio le tenemos que
#decir donde buscarlos.

setwd()

#cargamos los datos, el excel debe estar guardado en csv delimi-
#tado por comas

portdata<-read.csv("portdata.csv", sep=";", header=T)

portdata

#nos muestra los nombre de los elementos que tenemos guardados
#ahora solo tenemos las variables

#para que considere los objetos como variables independientes
attach(portdata)

names(portdata)

#estadísticos descriptivos 

summary(ctr)

summary(portdata)

#Modelo 1: estimar función Cobb-Douglas (¿estimación exacta o aproximada?)

#A) Estimación exacta

#logaritmos

lctr<-log(ctr)
lmtot<-log(mtot)
lpl<-log(pl)
lpk<-log(pk)
lpci<-log(pci)

#A) Estimación exacta

#homogeneidad de grado 1: transformación algebraica

lctrn<-lctr-lpci
lpln<-lpl-lpci
lpkn<-lpk-lpci

#estimamos  modelo
modelo1<-lm(lctrn ~ lpln+lpkn+lmtot)
summary(modelo1)

#calculamos el coeficiente asociado al precio de los consumos intermedios

b2<-modelo1$coefficients[2]
b2
b3<-modelo1$coefficients[3]
b3
b4<-1-b2-b3
b4

#economías de tamaño

by<-modelo1$coefficients[4]
by

#costes medios y  marginales en la media

#costes medios en la media
CME<-mean(ctm)/mean(mtotm)
CME

#coste marginal

CMG<-by*CME
CMG

#economías de escala

ec<-1/by
ec

#demandas óptimas de factores productivos
dl<-b2*(mean(ctr)/mean(pl))
dl
dk<-b3*(mean(ctr)/mean(pk))
dk
dci<-b4*(mean(ctr)/mean(pci))
dci

#B) Forma aproximada

#Desviamos variables en logaritmos con respecto a su media

lctrd<-lctr-mean(lctr)
lmtotd<-lmtot-mean(lmtot)
lpld<-lpl-mean(lpl)
lpkd<-lpk-mean(lpk)
lpcid<-lpci-mean(lpci)

summary lctr lctrd lpl lpld lpk lpkd lpci lpcid lmtot lmtotd

#Modelo 2: estimar Cobb-Douglas con tendencias y efectos fijios

#creamos la tendencia
t<-year-1985
t

#creamos las tendencia al cuadrado
t2<-(1/2)*t^2
t2

#creamos las variables ficticas por autoridad
nt<-length(ctr)
nt

F<-rep(0,2)

for (i in 1:2){
  portdata$di <- factor ( with ( portdata, ifelse ( ( ref ==  i ), 1 , 0 ) ) )
    }

d2


portdata$d_1 <- factor ( with ( portdata, ifelse ( ( ref == 1 ), 1 , 0 ) ) )
portdata$factorref_2 <- factor ( with ( portdata, ifelse ( ( ref == 2 ), 1 , 0 ) ) )
portdata$factorref_3 <- factor ( with ( portdata, ifelse ( ( ref == 3 ), 1 , 0 ) ) )
portdata$factorref_4 <- factor ( with ( portdata, ifelse ( ( ref == 4 ), 1 , 0 ) ) )
portdata$factorref_5 <- factor ( with ( portdata, ifelse ( ( ref == 5 ), 1 , 0 ) ) )
portdata$factorref_6 <- factor ( with ( portdata, ifelse ( ( ref == 6 ), 1 , 0 ) ) )
portdata$factorref_7 <- factor ( with ( portdata, ifelse ( ( ref == 7 ), 1 , 0 ) ) )
portdata$factorref_8 <- factor ( with ( portdata, ifelse ( ( ref == 8 ), 1 , 0 ) ) )
portdata$factorref_9 <- factor ( with ( portdata, ifelse ( ( ref == 9 ), 1 , 0 ) ) )
portdata$factorref_10 <- factor ( with ( portdata, ifelse ( ( ref == 10 ), 1 , 0 ) ) )
portdata$factorref_11 <- factor ( with ( portdata, ifelse ( ( ref == 11 ), 1 , 0 ) ) )
portdata$factorref_12 <- factor ( with ( portdata, ifelse ( ( ref == 12 ), 1 , 0 ) ) )
portdata$factorref_13 <- factor ( with ( portdata, ifelse ( ( ref == 13 ), 1 , 0 ) ) )
portdata$factorref_14 <- factor ( with ( portdata, ifelse ( ( ref == 14 ), 1 , 0 ) ) )
portdata$factorref_15 <- factor ( with ( portdata, ifelse ( ( ref == 15 ), 1 , 0 ) ) )
portdata$factorref_16 <- factor ( with ( portdata, ifelse ( ( ref == 16 ), 1 , 0 ) ) )
portdata$factorref_17 <- factor ( with ( portdata, ifelse ( ( ref == 17 ), 1 , 0 ) ) )
portdata$factorref_18 <- factor ( with ( portdata, ifelse ( ( ref == 18 ), 1 , 0 ) ) )
portdata$factorref_19 <- factor ( with ( portdata, ifelse ( ( ref == 19 ), 1 , 0 ) ) )
portdata$factorref_20 <- factor ( with ( portdata, ifelse ( ( ref == 20 ), 1 , 0 ) ) )
portdata$factorref_21 <- factor ( with ( portdata, ifelse ( ( ref == 21 ), 1 , 0 ) ) )
portdata$factorref_22 <- factor ( with ( portdata, ifelse ( ( ref == 22 ), 1 , 0 ) ) )
portdata$factorref_23 <- factor ( with ( portdata, ifelse ( ( ref == 23 ), 1 , 0 ) ) )
portdata$factorref_24 <- factor ( with ( portdata, ifelse ( ( ref == 24 ), 1 , 0 ) ) )
portdata$factorref_25 <- factor ( with ( portdata, ifelse ( ( ref == 25 ), 1 , 0 ) ) )
portdata$factorref_26 <- factor ( with ( portdata, ifelse ( ( ref == 26 ), 1 , 0 ) ) )
portdata$factorref_27 <- factor ( with ( portdata, ifelse ( ( ref == 27 ), 1 , 0 ) ) )




#estimamos  modelo
modelo2<-lm(lctrn ~ lpln+lpkn+lmtot+t+t2+portdata$factorref_2
+portdata$factorref_3+portdata$factorref_4
+portdata$factorref_5+portdata$factorref_6+portdata$factorref_7
+portdata$factorref_8+portdata$factorref_9+portdata$factorref_10
+portdata$factorref_11
+portdata$factorref_12+portdata$factorref_13+portdata$factorref_14
+portdata$factorref_15+portdata$factorref_16+portdata$factorref_17
+portdata$factorref_18
+portdata$factorref_19+portdata$factorref_20+portdata$factorref_21
+portdata$factorref_22+portdata$factorref_23+portdata$factorref_24
+portdata$factorref_25+portdata$factorref_26+portdata$factorref_27, data=portdata)
summary(modelo2)

#calculamos el coeficiente asociado al precio de los consumos intermedios

b2<-modelo2$coefficients[2]
b2
b3<-modelo2$coefficients[3]
b3
b4<-1-b2-b3
b4

####EFICIENCIA ECONÓMICA ESTOCÁSTICA (MODELO 2)#####

alfacb_i<-modelo2$coefficients[7:32]
alfacb_i

alfacb<-min(alfacb_i)
alfacb

ECcb_i<-1/exp(alfacb_i-alfacb)
ECcb_i

alfacb_1<-0
alfacb_1

ECcb_1<-1/exp(alfacb_1-alfacb)
ECcb_1

rank(c(ECcb_i, ECcb_1))


####EFICIENCIA ECONÓMICA DETERMINÍSTICA (MODELO 1)#####

#buscamos el mínimo residuo
minrcb<-min(modelo1$residuals)
minrcb

lctrhcb<-lctr-modelo1$residuals
lctrhcb

fronteracb<-lctrhcb+minrcb
ECcb_d<-fronteracb/lctr
ECcb_d

plot(ref, ECcb_d)


#comprobación
lctrnh<-lctrn-modelo1$residuals
lctrnh

a<-lctrh-(lctrnh+lpci)
a


#estimación función de costes translog


#creamos los cuadrados de los precios y el output
lpln2<-(1/2)*lpln^2
lpkn2<-(1/2)*lpkn^2
lmtot2<-(1/2)*lmtot^2

#creamos las interacciones entre el output y los precios
lplnlpkn<-lpln*lpkn
lplnlmtot<-lpln*lmtot
lpknlmtot<-lpkn*lmtot




#estimamos el modelo 3

#variables en desviaciones
lplnd<-lpln-mean(lpln)
lpknd<-lpkn-mean(lpkn)
lmtotd<-lmtot-mean(lmtot)
lsupd<-lsup-mean(lsup)
lctrnd<-lctrn-mean(lctn)

#creamos los cuadrados de los precios y el output
lplnd2<-(1/2)*lplnd^2
lpknd2<-(1/2)*lpknd^2
lmtotd2<-(1/2)*lmtotd^2

#creamos las interacciones entre el output y los precios
lplndlpknd<-lplnd*lpknd
lplndlmtotd<-lplnd*lmtotd
lpkndlmtotd<-lpknd*lmtotd


modelo3<-lm(lctrn ~ lplnd+lpknd+lmtotd+lplnd2+lpknd2
+lmtotd2+lplndlpknd+lplndlmtotd+lpkndlmtotd+t)
summary(modelo3)


modelo4<-lm(lctrn ~ lplnd+lpknd+lmtotd+lplnd2+lpknd2
+lmtotd2+lplndlpknd+lplndlmtotd+lpkndlmtotd+t+
+portdata$factorref_2+portdata$factorref_3+portdata$factorref_4
+portdata$factorref_5+portdata$factorref_6+portdata$factorref_7
+portdata$factorref_8+portdata$factorref_9+portdata$factorref_10
+portdata$factorref_11
+portdata$factorref_12+portdata$factorref_13+portdata$factorref_14
+portdata$factorref_15+portdata$factorref_16+portdata$factorref_17
+portdata$factorref_18
+portdata$factorref_19+portdata$factorref_20+portdata$factorref_21
+portdata$factorref_22+portdata$factorref_23+portdata$factorref_24
+portdata$factorref_25+portdata$factorref_26++portdata$factorref_27)


summary(modelo4)


####EFICIENCIA ECONÓMICA ESTOCÁSTICA (MODELO 4)#####

alfat_i<-modelo4$coefficients[12:37]
alfat_i

alfat<-min(alfat_i)
alfat

ECt_i<-1/exp(alfat_i-alfat)
ECt_i

alfat_1<-0
alfat_1

ECt_1<-1/exp(alfat_1-alfat)
ECt_1

rank(c(ECt_i, ECt_1))


####EFICIENCIA ECONÓMICA DETERMINÍSTICA (MODELO 3)#####

#buscamos el mínimo residuo
minrt<-min(modelo3$residuals)
minrt

lctrht<-lctr-modelo3$residuals
lctrht

fronterat<-lctrht+minrt
ECt_d<-fronterat/lctr
ECt_d

plot(ref, ECt_d)

layout(matrix(1:2,2,1))
plot(ref, ECcb_d)
plot(ref, ECt_d)
layout(1)







#otras variables

lsup<-log(sup)


portdata$factoryear_86 <- factor ( with ( portdata, ifelse ( ( year== 86 ), 1 , 0 ) ) )
portdata$factoryear_87 <- factor ( with ( portdata, ifelse ( ( year== 87 ), 1 , 0 ) ) )
portdata$factoryear_88 <- factor ( with ( portdata, ifelse ( ( year== 88 ), 1 , 0 ) ) )
portdata$factoryear_89 <- factor ( with ( portdata, ifelse ( ( year== 89 ), 1 , 0 ) ) )
portdata$factoryear_90 <- factor ( with ( portdata, ifelse ( ( year== 90 ), 1 , 0 ) ) )
portdata$factoryear_91 <- factor ( with ( portdata, ifelse ( ( year== 91 ), 1 , 0 ) ) )
portdata$factoryear_92 <- factor ( with ( portdata, ifelse ( ( year== 92 ), 1 , 0 ) ) )
portdata$factoryear_93 <- factor ( with ( portdata, ifelse ( ( year== 93 ), 1 , 0 ) ) )
portdata$factoryear_94 <- factor ( with ( portdata, ifelse ( ( year== 94 ), 1 , 0 ) ) )
portdata$factoryear_95 <- factor ( with ( portdata, ifelse ( ( year== 95 ), 1 , 0 ) ) )
portdata$factoryear_96 <- factor ( with ( portdata, ifelse ( ( year== 96 ), 1 , 0 ) ) )
portdata$factoryear_97 <- factor ( with ( portdata, ifelse ( ( year== 97 ), 1 , 0 ) ) )
portdata$factoryear_98 <- factor ( with ( portdata, ifelse ( ( year== 98 ), 1 , 0 ) ) )
portdata$factoryear_99 <- factor ( with ( portdata, ifelse ( ( year== 99 ), 1 , 0 ) ) )
portdata$factoryear_00 <- factor ( with ( portdata, ifelse ( ( year== 00 ), 1 , 0 ) ) )
portdata$factoryear_01 <- factor ( with ( portdata, ifelse ( ( year== 01 ), 1 , 0 ) ) )
portdata$factoryear_02 <- factor ( with ( portdata, ifelse ( ( year== 02 ), 1 , 0 ) ) )
portdata$factoryear_03 <- factor ( with ( portdata, ifelse ( ( year== 03 ), 1 , 0 ) ) )
portdata$factoryear_04 <- factor ( with ( portdata, ifelse ( ( year== 04 ), 1 , 0 ) ) )
portdata$factoryear_05 <- factor ( with ( portdata, ifelse ( ( year== 05 ), 1 , 0 ) ) )



