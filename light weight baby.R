#light weight baby
library(PerformanceAnalytics)
library(forecast)
library(fable)
library(quantmod)
library(TSstudio)
library(TSA)
library(dygraphs)
library(fpp2)
library(rugarch)
library(xts)
library(fUnitRoots)
library(ggplot2)
library(tseries)
library(lmtest)
library(TSA)
library(aTSA)
library(Metrics)
library(FitAR)
library(fBasics)
options(warn = - 1)

##--------Obtención Datos:
start<-format(as.Date("2019-07-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-30"),"%Y-%m-%d")

#--------- Parte I: Datos y Modelo de Media
#--------- Función para bajar precios y generar rendimientos:
rend<-function(simbolo,start,end) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo, src = "yahoo", auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  rend<-periodReturn(datos, period="daily", subset=paste(c(start, end), "::", sep=""), type='arithmetic')
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}
#--------Llamar la función para cada activo particular:
rend("AMZN", start, end)
str(AMZN)
## Gráfico:
rends<-merge.xts(AMZN)
colnames(rends)<-c("AMZN")
dygraph(rends, main = "AMZN Rendimientos") %>%
  dyAxis("y", label = "Rend %") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))


######################################################
########################### PARTE 1: ELEMENTOS
#----------Estadisticas básicas 
basicStats(AMZN) ## Resumen estadísticos
mean(AMZN)
var(AMZN)
stdev(AMZN) # Desv Std
t.test(AMZN)  # Prueba que H0: mean return = 0
s3=skewness(AMZN)  #Sesgo
T=length(AMZN) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*pt(abs(t3), T-1, lower=FALSE) # Calcula p-valor, si p valor > alfa, no se rechaza nula y por tanto sesgo de cero 
pp
s4=kurtosis(AMZN)
s4
t4=s4/sqrt(24/T) # Prueba de curtosis, en exceso
t4
pv=2*(1-pnorm(t4)) # p-valor,  si p valor > alfa, no se rechaza nula y por tanto exceso de curtosis de cero 
pv
normalTest(AMZN,method='jb') # Prueba Jaque Bera, H0: Normal

##----------Gráfica Densidad
chart.Histogram(AMZN, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-AMZN" ,"AMZN dist","dnorm AMZN"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)


######################################################
########################### PARTE 2: MODELOS
##Obtenemos precios de AMAZON
AMZN<-getSymbols("AMZN", from="2019-07-01",to="2021-07-30", src = "yahoo", auto.assign = FALSE) #
# Eliminando valores faltantes
AMZN <- na.omit(AMZN)
# Mantenemos columnas con Precios de Cierre  columna 4:
AMZN <- AMZN[,4]
##Podemos graficar:
plot(AMZN, ylab="Precios")
length(AMZN)
##Partimos serie, tomemos el 2% para la prueba
h <- round(length(AMZN)*0.02, digits = 0 )
h
train <- AMZN[1:(nrow(AMZN) - h), ]
test<- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN), ]

############ Modelos ARIMA ############################
###Veamos si la serie es estacionaria:
adfTest(train)

##Como no es estacionaria, la diferenciamos y vemos si ya es estacionaria:
dtrain<-diff(train)[-1,]
adfTest(dtrain)  #con libreria fUnitRoorts

##Gráficas de ambos
par(mfrow=c(2,1))
plot(train, col="red")
plot(dtrain, col="blue")

##Ya estacionaria, definimos candidatos de modelos ARMA
m<-eacf(dtrain, 20,20)  #Seria un ARMA(1,1), pero si deseamos expresarla como ARIMA, sería: ARIMA (1,1,1).

#Definamos otros modelos mediante la función auto.arima()
m2<-auto.arima(train, seasonal = TRUE)
summary(m2)   #Sería arima(0,1,0)

###Modelación:
mod1<-Arima(train, order=c(15,1,9), method = "ML")
mod1
tsdiag(mod1)  ##residuos se ven ok.

mod2<-Arima(train, order=c(0,1,0), method = "ML")
mod2
tsdiag(mod2)   #Residuos se ven ok


### Modelos pronóstico para m1 y m2:
Pron_m1<-forecast(mod1, h)
Pron_m2<- forecast(mod2, h)

##Gráficos
par(mfrow=c(2,1))
plot(Pron_m1, include=50)
plot(Pron_m2, include=50)

#### Midamos el error de pronóstico, RMSE y MAPE:
RMSE_DRIFT<-rmse(test, Pron_m1$mean)
RMSE_1S<-rmse(test, Pron_m2$mean)

MAPE_DRIFT<-mape(test, Pron_m1$mean)
MAPE_1S<-mape(test, Pron_m2$mean)

## Generamos la función de pronóstico. En datos de precios, se deben transformar 
#los datos lambda para tratar que los residuos sean cercanos a homocedásticos.  
nn1 <- nnetar(train, lambda = TRUE)
nn1

autoplot(forecast(nn1,PI=TRUE, h=10), include=50)
fnn1<-forecast(nn1,h=10)

##Cálculo de las méricas de error de pronóstico:
RMSE_nnetar<-rmse(test, fnn1$mean)
MAPE_nnetar<-mape(test, fnn1$mean)


######################################################
########################### PARTE 3: MODELOS AGG
##--------Obtención Datos:
start<-format(as.Date("2019-07-01"),"%Y-%m-%d")
end<-format(as.Date("2021-07-30"),"%Y-%m-%d")

#--------- Parte I: Datos y Modelo de Media
#--------- Función para bajar precios y generar rendimientos:
rend<-function(simbolo,start,end) {
  ##---------Obtener precios de yahoo finance:
  datos<-getSymbols(simbolo, src = "yahoo", auto.assign = FALSE)
  ##---------eliminar datos faltantes:
  datos<-na.omit(datos)
  ##--------Mantener el precio de interés:
  datos<-datos[,4]
  ##--------Rendimientos simples:
  rend<-periodReturn(datos, period="daily", subset=paste(c(start, end), "::", sep=""), type='arithmetic')
  #--------Para hacer dtos accesibles  GLobal ENv:
  assign(simbolo, rend, envir = .GlobalEnv)
}
#--------Llamar la función para cada activo particular:
rend("AMZN", start, end)
str(AMZN)

#--------Gráfico:
colnames(AMZN)<-"AMZN"
dygraph(AMZN, main = "AMZN Rendimientos") %>%
  dyAxis("y", label = "Rend %") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(1, "Set1"))


#-------Modelo de Media de rendimientos, mediante EACF y auto.arima:
AMZN<-as.data.frame(AMZN)
eacf=eacf(AMZN$AMZN,10,10)      # Sería un arma(0,1)
auto.arima(AMZN)               # Sería un arma(0,2) con media diferente de cero
#------ Desarollo modelos:
m1=arima(AMZN,order=c(2,0,1))
m1
tsdiag(m1)
m2=arima(AMZN, order=c(0,0,1))
m2
tsdiag(m2)      #Modelos similares en sus métricas AIC, tomemos el segundo por su simplicidad.

#-------Parte II. Probar Efecto heterocedástico 
#-------Probar efecto ARCH:
pacf(m2$residuals) 
##nivel de ARCH
arch.test(m2)

#--------Estadisticas básicas:
basicStats(AMZN)
s3=skewness(AMZN)  #Sesgo
T=length(AMZN) # tamaño muestra
t3=s3/sqrt(6/T) # Prueba de sesgo
t3
pp=2*(1-pnorm(abs(t3)))
pp

##----------Gráfica Densidad
chart.Histogram(AMZN, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("Hist-AMZN" ,"AMZN dist","dnorm AMZN"), col=c("gray", "blue", "red"), lty=1, cex = 0.7)

#-------- Parte III: Modelamiento
#-------- Modelo de Volatilidad
#Con librería rugarch, pasos:
#1) ugarchspec(): Especifica que modelo de GARCH se desa emplear (media, varianza, distribución de innovaciones)
#2) ugarchfit(): Estima el modelo GARCH en la serie de rendimientos
#3) ugarchforecast():  Empleando el modelo estimado de GARCH, hace predicciones de volatilidad.

## Modelo 1
modelMean1=list(armaOrder = c(0, 2), include.mean = TRUE)
modelVar1=list(model = "sGARCH", garchOrder = c(1, 1))
modelGarch1=ugarchspec(variance.model=modelVar1,mean.model = modelMean1, distribution.model="std")
modelFit1=ugarchfit(spec=modelGarch1,data=AMZN)
modelFit1
modelFit1@fit$coef
#plot(modelFit1,which="all")
#Media rendimientos:
mean1<-fitted(modelFit1)
plot(mean1)
#Volatilidades:
Vol1<-sigma(modelFit1)
plot(Vol1)

#Desv standar largo plazo:
uncvariance(modelFit1) #Vemos la varianza a largo plazo, 
sqrt(uncvariance(modelFit1))  #Su raíz, la desviación std establece que la desv std a largo plazo es del 2.7%

#--------- Pronóstico:
forc1<-ugarchforecast(fitORspec = modelFit1, n.ahead=10)
plot(forc1, which=1)
plot(forc1, which=3)

#---------- Simulación
###Valores de la media rendimientos futuros:
windows(width=10, height=8)
sim = ugarchsim(modelFit1, n.sim = 1000, m.sim = 25, startMethod="sample")  #n.sim: horizonte simulación. m.sim: num simulaciones.
plot(sim, which = 'all', cex=0.05)
##.


"°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
#Correcto

"Posee rendimientos con una media estadísticamente igual a cero, con un sesgo no significativo, pero un comportamiento leptocúrtico, que la traduce en una serie altamente riesgosa por concentrar sus rendimientos marginales en la media y cuando se presentan choques eventuales de precios, estos son muy significativos o extremos lo que hace que las colas de las distribuciones de rendimientos sean anchas y con ello, alta probabilidad de tener rendimientos extremos traducidos en ganancias o pérdidas extremas. Por otro lado, en cuanto al poder de predicción del comportamiento de la media de rendimientos futuros de la serie a una ventana corta, como ejemplo a 11 días, es preferible modelarla con un modelo tipo de red neuronal, en comparación con uno arima. Sin embargo, la serie presenta un efecto ARCH, que se ve soportada por la distribución leptocúrtica previamente identificada, lo que hace recomendable el considerar un modelo de volatilidad heterocedástica para modelar el comportamiento de la serie. Este modelo, por tener efecto ARCH a altos rezagos de los residuales, debiera considerar una distribución de colas anchas, no sesgadas y con la introducción de términos recursivos de volatilidad o, dicho de otra manera, de términos de predicción de volatilidad anteriores, en su modelo; es decir, modelos tipo GARCH con distribuciones tipo “std”. En resumen, si este activo se considerara por el momento como un instrumento de inversión individual, se debiera monitorear su comportamiento a ventanas cortas debido a su comportamiento riesgoso que podrían generar tanto pérdidas o ganancias relevantes."
