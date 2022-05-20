########Hacemos los vectores para los niveles del factor y la variable respuesta

#####Le llamaremos (trat) al vector de los niveles del factor 
##Ponemos el n?mero de niveles del factor comenzando con 1 y repitiendolo el n?mero de veces como sus repeticiones y terminando con el m?ximo nivel del factor y sus repeticiones

##ejemplo de propagaci?n vegetativa

trat<-c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3)

###Al vector que contiene a la variable respuesta le llamaremos y

y <- c(57,58,60,59,62,60,60,57,59,61,62,66,65,63,64,62,65,65,62,67,75,77,70,75,69,71,68,73,72,70)

####################otra manera de hacer el vector es

trat <- c(rep(1,10),rep(2,10),rep(3,10))
y<-c(57,58,60,59,62,60,60,57,59,61,62,66,65,63,64,62,65,65,62,67,75,77,70,75,69,71,68,73,72,70)

####################otra manera de hacer el vector es poniendo los nombres de cada nivel del factor

trat=rep((c("glucosa","sacarosa","fructuosa")),c(10,10,10))
y <- c(57,58,60,59,62,60,60,57,59,61,62,66,65,63,64,62,65,
       65,62,67,75,77,70,75,69,71,68,73,72,70)

####Checamos los datos
datos=data.frame(cbind(trat,y))
datos

############Espec?fico a R cual es el factor y la variable respuesta
trat <- as.factor(trat)
is.factor(trat)
is.numeric(y)

###########Hago mi modelo para un dise?o unifactorial
# lm le dice a R que ajuste un modelo lineal
# con y como variable de respuesta y
# trat como factor.

m1<-lm(y~trat)
anova(m1)


#########Aplicamos una prueba de comparaci?n m?ltiple, cuando hay diferencias significativas

TukeyHSD(aov(y~trat))

plot(TukeyHSD(aov(y~trat)))
###########Pido las  medias de los tratamientos para interpretar la prueba de Tukey

medias <- tapply(y,trat,mean)
medias
###########ordeno las medias de menor a mayor
ordenando <- sort(medias)
ordenando
######### intervalos de confianza para las medias, en forma gr?fica
#c?lculo de las medias de los tratamientos
xbar <- tapply(y,trat,mean) 
xbar

xbarord <- sort(xbar)
xbarord
n <- tapply(y,trat,length)
n
CME <- anova(m1)["Residuals", "Mean Sq"]
CME
# error est?ndar de las medias(EE)
EE <- sqrt(CME/n)
EE
#grado libertad del error(gle)
gle <- anova(m1)["Residuals", "Df"]
gle
cuantil <- qt(0.975,gle)
cuantil
plot.new()
stripchart(y~trat,pch=16,vert=T)
arrows(1:3,xbar+cuantil*EE,1:3,xbar-cuantil*EE,angle=90,code=3,length=.1)

##########En el curso se usara la prueba de Tukey, pero se les mostrar? otras:

#############Se descargan las paqueter?as (una sola vez): agricolae, multcomp, LSD,  foreign, MASS

###############Se abren las librerias cada vez que se hace la corrida
library(agricolae)
library(foreign)
library(multcomp)
library(LSD)
library(mvtnorm)


##########Prueba de Bonferroni
pairwise.t.test(y,trat,p.adjust="bonferroni")


##########Prueba de Duncan
model<-aov(y~trat)
out<-duncan.test(model,"trat",main="azucar")
#Nivel de confianza al 95%
duncan.test(model,"trat",alpha=0.05,console=TRUE)
#Nivel de confianza al 99%
duncan.test(model,"trat",alpha=0.01,console=TRUE)

# version old duncan.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
MSerror

print(out$groups)

#####################Prueba se Scheffe
comparison <- scheffe.test(model,"trat", group=TRUE,console=TRUE,main="azucar")

#LSD

LSD.test(aov(m1),"trat")
print(LSD.test(aov(m1),"trat"))


#Prueba de Dunnett
#Cargar la paqueteria de DescTools
library(DescTools)
DunnettTest (trat,y)
DunnettTest
help("DunnettTest")
########Verificando los supuestos

residual <- m1$res
residual

############## An?lisis exploratorio de datos

e<-1:30
e
plot(e,residual)
plot(residual)
######### b)   

qqnorm(residual)
qqnorm
qqline(residual)

##########c) histograma de los residuals 
hist(m1$res,col="blue")

#####Prueba de normalidad    
shapiro.test(residual)

#####Prueba de homogeneidad de varianzas
bartlett.test(residual,trat)

##Istalar en paqueter?a tseries
library(tseries)
####Ho: No hay correlacion (prueba para probar correlaciones)
runs.test(as.factor(m1$residual<0))


#En caso de no cumplir con los supuestos,  se transforma la variable respuesta
y
#Calculo la raiz cuadrada de y
y2<-sqrt(y)

y2
m2<-lm(y2~trat)
anova(m2)
#Otra transformaci?n
#Calculo el logaritmo natural de y
y3<-log(y)

#Otra transformaci?n
#Calculo el cuadrado de y

Y4<-y*y

#Hay otra transformaci?n de Box-Cox
library(MASS)
bc<-boxcox(m1)
lambda<-bc$x[which.max(bc$y)]
lambda
#transformamos
ty<-y^lambda
ty

m2a<-lm(ty~trat)
anova(m2a)
