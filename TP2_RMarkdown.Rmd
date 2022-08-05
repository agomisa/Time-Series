---
title: "Análisis de Series Temporales"
author: Couste, Ciro; Gir, Alberto; Menzella, Carla; Misaña,	Agostina; Viera, Arnaldo
date: "Diciembre de 2021"
output:
  html_document:
    code_folding: show
    theme: journal
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
subtitle: Tabajo Práctico N° 2
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r paquetes, include=FALSE, eval=TRUE}

suppressPackageStartupMessages({
  library(forecast)
  library(ggplot2)
  library(gridExtra)
  library(tseries)
  library(PASWR2)
  library(dplyr)
  library(psych)
  library(pastecs)
  library(astsa)
  library(lessR)
  library(tseries)
  library(zoo)
  library(xts)
  library(fma)
  library(expsmooth)
  library(lmtest)
  library(Quandl)
  library(fpp)
  library(urca)
  library(AER)
  library(fUnitRoots)
  library(CADFtest)
  library(fpp2)
  library(car)
  library(tseries)
  library(gridExtra)
  library(readxl)
  library(DataExplorer)
  library(data.table)
  library(strucchange)
  library(FitARMA)
  library(reshape)
  library(Rmisc)
  library(fBasics)
  library(gt)
  library(tidyverse)
  library(timetk)
  library(nortest)
})

```


```{r limpio memoria, include=FALSE, eval=TRUE, echo=FALSE}

rm(list = ls())
setwd(getwd())

```

# Resumen ejecutivo:

El presente trabajo tiene como objetivo poner en práctica los conocimientos adquiridos en la materia Análisis de Series temporales de la Maestría en Ciencia de Datos de la Universidad Austral. 
Para ello analizaremos dos series de temporales. La primera es la cantidad de noches que los viajeros no residentes pernoctan en un complejo hotelero dentro de Argentina. Y la segunda serie es la variación porcentual del índice de precios al consumidor de la república Argentina. A diferencia del primer práctico decidimos utilizar la segunda serie en variación porcentual y no en níveles porque pensamos que más allá del nivel de precios, lo importante es analizar la inflación. Y debido al aprendizaje realizado en el primer práctico, pudimos observar que a medida que la serie incrementaba el valor de los niveles de precios se incrementaba el valor de la variación. Dicho dato pierde referencia si no lo comparamos en términos porcentuales. Ejemplificando, una inflación mensual del 2% es comparable en terminos de impacto económico con otro punto en la serie donde haya dado un valor similar, aunque si partimos de niveles de precios diferentes la variación en términos absolutos también será diferente.
La comparación de los resultados de los Criterios de Información en ambas series, contra los obtenidos en el trabajo práctico anterior, nos indican que tanto la consideración de las raíces unitarias así como la introducción de la estacionalidad en los modelos, llevan a mejores resultados. 

# Introducción:

Analizaremos las dos series por separado. Para cada una de ellas verificaremos el estado estacionario de las mismas, con diferentes tests de raíces unitarias. En caso de ser necesario aplicaremos las transformaciones necesarias para llegar a la estacionaridad.
Luego representaremos las mismas a través de modelos ARIMA o SARIMA según corresponda. Para ello es importante detectar si las series de tiempo tienen un comportamiento cíclico.
Una vez representadas estudiaremos el comportamiento de los residuos del modelos para determinar si se comportan como un ruido blanco. O si por el contrario precisan transformaciones.

# Desarrollo

##  Serie de tiempo 1: 

Se Importa el datasets y configura como serie de tiempo:

```{r imp y convierto en sdt, include=FALSE, eval=TRUE, echo=TRUE}

data1 <- read.csv ("serie1-Cantidad de noches de los viajeros no residentes en los establecimientos hoteleros y parahoteleros del país.csv") #Dataframe completa

data11 <- data1[data1$indice_tiempo < 2020, ] #Dataframe hasta 2019


data1$indice_tiempo <- as.Date(data1$indice_tiempo)  

#Verificar tipo de datos 
data.class(data1$indice_tiempo)
data.class(data1$pernoctes_no_residentes)

#Construyo la serie de tiempo
SdT1 <- ts( data1$pernoctes_no_residentes,  
              frequency = 12, 
            start = c(2007,1))

data.class(SdT1)

```

```{r tabla Sdt1}
data1 %>% 
  mutate(pernoctes_no_residentes) %>% 
  head(4) %>% 
  gt() %>% 
  tab_header('Cantidad de noches de los viajeros no residentes en los establecimientos hoteleros y parahoteleros del pais')
```


```{r autoplot sdt1}
autoplot(SdT1,
         main = "Pernoctes no residentes", 
         ylab = "Cantidad de personas",
         xlab = "Años")

```

  En el grafico de SdT1 se observa un comportamiento aparentemente estacional. Hasta 2020, no se observa ninguna tendencia clara a crecer o decrecer. Se ve una fuerte caida a inicios de 2020,lo cual se puede pensar que fue por el inicio de la pandemia. 
Se procede a graficar la descomposicion:


```{r plot descomposicion sdt1}
autoplot(stl(SdT1, s.window = "periodic"), ts.colour="blue" )
```

Se observa un componente de ciclos (seasonal), sin tendencia aparente hasta que decrece en 2020. Se puede pensar en las diversas cuarentenas implementadas por los gobiernos a raíz de la pandemia por Covid 19 como un suceso extremo u outlier. Para ello es interesante poder observar la serie solo hasta diciembre de 2019.

Por lo tanto se construye la serie sin el año 2020:

```{r sdt1 sin 2020}
SdT1sinout <- ts (data11$pernoctes_no_residentes,  
              frequency = 12, 
              start = c(2007,1),
              end = c(2019,12))

data.class(SdT1sinout)

autoplot(SdT1sinout,
         main = "Pernoctes no residentes - serie acotada", 
         ylab = "Cantidad de personas",
         xlab = "Años")

```

Se procede a graficar la descomposicion nuevamente con el rango la serie ajustado:

```{r descm SdT1sinout}
autoplot(stl(SdT1sinout, s.window = "periodic"), ts.colour="blue" )
```

Se puede comprobar que no hay eventos o intervenciones relevantes, se mantiene una estacionalidad y una tendencia variable.


### Analisis exploratorio de la base

```{r} 
gt(psych::describe(SdT1sinout))
hist(SdT1sinout, col = "purple", main = "Cantidad de Pernoctes de No Residentes 2007-2019",
     xlab = "Número de Pernoctes") 
```

En una primera aproximación se obtiene que la distribución de la serie se aproxima a la distribución normal a grandes razgos. De los datos se extrae que la media es levemente superior a la mediana. Lo cual se corrobora con los quantiles. 

*Supuestos de normalidad:*

Se corrobora con Jarque-Bera, Anderson-Darling y Shapiro-Wilk:

```{r test normalidad} 
jarque.bera.test(data11$pernoctes_no_residentes)

ad.test(data11$pernoctes_no_residentes) 

shapiro.test(data11$pernoctes_no_residentes)
```
El p-value del test es mayor a una significancia de 0.05 en todos los casos, por lo que se indica NO RECHAZAR la H0 de normalidad.

### Analisis de la estacionariedad

Se grafican la serie, FAS, FAC y FACP para una primera interpretación de la estacionaridad:

```{r estacionariedad sdt1inout} 
autoplot(SdT1sinout, main="Cantidad de Pernoctes ENE-2007 a DIC-2019")

acf(SdT1sinout, type = "cov",  main="Funcion de Autocovarianza")

#par(mfrow=c(2,1))
g1<-acf(SdT1sinout, main="Funcion de Autocorrelacion")
g2<-pacf(SdT1sinout, main="Funcion de Autocorrelacion Parcial")
```

En la gráfica de FAS se observan lags que se destacan ciclicamente con cambio de signo.
En la gráfica de FAC, se nota un descenso rápido de los coef de correlación generando una forma sinusoidal con varios lags significativos.
Por otro lado, la gráfica de FACP muestra lags significativos que indicarían un AR de ordenes 4, 6, 7, 10 y 13.

#### Estacionaridad en la varianza

Se verifica la estacionaridad en la varianza y ver la necesidad de aplicar una transformación, a través del lambda propuesto por la función "powerTransform":

```{r powerTranform} 
summary(powerTransform(data11$pernoctes_no_residentes))
```

El resultado es un Est power = 0.97 (lambda estimado), se define que no es necesaria una transformación.
El test verifica que el p-value es muy chico cuando se testea lambda=0 (se rechaza la H0: lambda=0 o sea, log-transform) y grande para lambda=1 (no se rechaza la H0: lambda=1, no es necesaria una transformación).

Finalmente se toma como referencia el resultado de Jarque-Bera de normalidad y no se transforma la serie original.


#### Estacionaridad en la media

Se testea la periodicidad presente en la serie:
```{r} 
frequency(SdT1sinout)
```

1) LJUNG - BOX

A partir del test de Ljung-Box, el rechazo de H0 significa que hay coeficientes de autocorrelación distintos a cero, y hay variabilidad en la Variancia.

```{r Ljung-Box} 
Incorrelation <- function(ts, type = c("Ljung-Box","Box-Pierce"), fitdf = 0){
  p_ljung_box = NULL
  s_ljung_box = NULL
  for(i in 0:(length(ts)/4)){
    p_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$p.value
    s_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$statistic
  }
  table = data.frame(j = 1:(length(ts)/4),
                     P_Value = p_ljung_box,
                     Statistic = s_ljung_box)
  return(table)
}

Incorrelation(SdT1sinout,"Ljung-Box") 
inco_wn = Incorrelation(SdT1sinout,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para distintos lags

```

Todos los p-values dan muy bajos, por lo que SE RECHAZA la H0, y se interpreta que la serie tiene coeficientes de autocorrelaci?n distintos a cero.

Tests de Raíces Unitarias para determinar estacionaridad: se prueban varios test para verificar consistencia en las respuestas.

2) DICKEY-FULLER

- H0: RAIZ UNITARIA (NO ESTACIONARIO)
- H1: NO HAY RAIZ UNITARIA (ESTACIONARIO)

```{r DICKEY-FULLER1} 
df<-tseries::adf.test(SdT1sinout)  
df   
```
El test de AUG D-F resulta en un p-value bajo, por lo que se rechaza la H0 y la serie de tiempo sería ESTACIONARIA. Dado que este test tiene un sesgo a rechazar la H0, se aplicarán otros test de hipótesis.

3) Se aplican las pruebas Dickey-Fuller Aumentado con sus tres posibilidades (paquete URCA)

3.1) TIPO 0 - NONE

```{r Dickey-Fuller tipo 0}
urca_none<-urca::ur.df(SdT1sinout, selectlags = "AIC", type = "none")
urca_none
summary(urca_none)
```
Si se toma un nivel de significancia de 0.05, el tau estadistico es mayor al valor crítico y por lo tanto NO se RECHAZA la H0. La serie no es estacionaria cuando no se considera tendencia y/o drift.


3.2) TIPO 1 - DRIFT

```{r urca Drift}
urca_drift<-urca::ur.df(SdT1sinout, selectlags = "AIC", type = "drift")
urca_drift
summary(urca_drift)
```
Si se toma un nivel de significancia de 0.05, el tau estadistico es menor al valor crítico y por lo tanto se RECHAZA la H0. 
Por otro lado el phi1-estadistico es mayor al phi1 crítico (H0: existe raíz unitaria sin drift), lo cual lleva a rechazar ésta H0. La serie es estacionaria con drift.


3.3) TIPO 2 - TREND & DRIFT

```{r urca T&D}
urca_trend <- urca::ur.df(SdT1sinout, selectlags = "AIC", type = "trend")
urca_trend
summary(urca_trend)
```

Si se toma un nivel de significancia de 0.05, el tau estadistico es menor al valor crítico y por lo tanto se RECHAZA la H0. 
Al continuar el análisis, el estadístico para phi2 es mayor que el valor crítico, por lo tanto se rechaza la H0: existe raíz unitaria, sin drift y sin tendencia. 
El estadístico para phi3 es mayor que el valor crítico, por lo que se rechaza la H0: existe raíz unitaria y sin tendencia.
Notar que el sentido de rechazo - no rechazo de la H0 es inverso entre tau y los phi.

La serie es estacionaria con tendencia y drift (pendiente e intercepto).

4) Se compara ahora con un ndiffs con argumentos iguales a este test:

```{r ndiffs1} 
ndiffs(SdT1sinout, alpha = 0.05, test = "adf")
ndiffs(SdT1sinout, alpha = 0.05, test = "kpss")
ndiffs(SdT1sinout, alpha = 0.05, test = "pp")
```
Estos resultados de test indican que la serie no precisa diferenciar para un nivel de significancia de 0.05.


5) TEST KPSS 

- H0 indica que la serie es ESTACIONARIA con tendencia determinística.

```{r test KPSS}
kpss.test(SdT1sinout)
```
Como el p-value es mayor al alpha = 0.05, NO SE RECHAZA la H0, e indica ESTACIONARIEDAD con tendencia determinística.

6) Se prueba con test de Phillips-Perron. Hipotesis nula que existe al menos una Raíz Unitaria

```{r}
pp.test(SdT1sinout)
```
El resultado indica que debe rechazarce la H0, y por lo tanto la serie es estacionaria.

### Cambios estructurales

Ahora se investigan los cambios estructurales mediante el Test de Zivot-Andrews. 

```{r}
SdTsinout.za <- ur.za(SdT1sinout, model="both")
summary(SdTsinout.za)
```

El estadístico de test de la serie es menor al valor crítico (inclusive para alpha = 0.01), por lo que SE RECHAZA LA H0 de existencia de una raíz unitaria por cambio estructural. 
Esto indica que la serie es estacionaria con cambio estructural.
Se plantea un quiebre en el mes 98.
Se verifica la existencia de cambios estructurales en la serie.


```{r} 
plot(SdTsinout.za)
```

#### Cambios estructurales con tendencia ( SdT1sinout ~ timeline1 )

A partir del hallazgo de estacionariedad con tendencia en test anterior, se busca determinar los cambios estructurales y las rectas de regresión lineal asociadas con el siguiente modelado:

```{r recta de tendencia} 
len <- length(SdT1sinout)
timeline1 <- 1:len
trend_fit <- lm(SdT1sinout ~ timeline1)
summary(trend_fit)
```
El p-value del intercepto indica que es significativo (RECHAZA H0), mientras que el p-value de la pendiente no sería significativo (NO SE RECHAZA LA H0), siendo entonces este coeficiente despreciable en la ecuación. Por ello se considera el cambio estructural de nivel.


####  Cambio estructural de nivel/drift ( SdT1sinout ~ 1 )

Se prueban otras ventanas de tiempo, cuyo resultado siempre devuelve coeficientes significativos. 

```{r SdT1sinout ~ 1 }
SdT1sinout ~ 1
summary(lm(SdT1sinout ~ 1))
coeftest(lm(SdT1sinout ~ 1))

plot(SdT1sinout)
lines(ts(fitted(lm(SdT1sinout ~ 1)), start=2007 ,frequency = 1), col = 4)
```

Se buscan los breakpoints del cambio estructural:

```{r brk}
SdT1brk <- breakpoints(SdT1sinout ~ 1, h = 0.1)
summary(SdT1brk)
```
A partir de los resultados de encontrar m = 1 a 9 puntos de ruptura con las fechas asociadas y las métricas {RSS, BIC}, el valor mínimo de BIC ocurre en m = 4. 
Trazamos la salida de la función breakpoint() para obtener una comprensión visual:

```{r}
plot(SdT1brk)
```

```{r SdT1 con break}
plot(SdT1sinout)
lines(fitted(SdT1brk, breaks = 4), col = 4)
lines(confint(SdT1brk, breaks = 4))
```

Se elige optar por un solo punto de quiebre para trabajar con mayor cantidad de datos. Los valores BIC para los primeros 4 breakpoints no son muy diferentes.

**Serie con 1 break**

```{r serie con 1 break}
plot(SdT1sinout)
lines(fitted(SdT1brk, breaks = 1), col = 4)
lines(confint(SdT1brk, breaks = 1))
```

Las fechas de quiebre:

```{r fechas quiebre B1}
breakdates(SdT1brk, breaks = 1)
```

Los coeficientes:

```{r coeft B1}
coef(SdT1brk, breaks = 1)
```
Se generará el modelo de la serie temporal con el intervalo a partir de 04.2012, según el punto de quiebre encontrado.


### Modelos 

Se definen los subconjuntos de train y test para la serie final correspondiente al intervalo definido (04.2012 - 12.2019):

```{r Train y Test BreakPoint1}

serieFinal <- window(SdT1sinout,
                    start = c(2012,04),
                    end = c(2019,12))
serieejTrain <-  window(SdT1sinout, 
                    start = c(2012,03), 
                    end = c(2018,12))
serieejTest <-  window(SdT1sinout, 
                    start = c(2019,01), 
                    end = c(2019,12))
```

Se utiliza la función auto.arima para comprobar los modelos sugeridos por el algoritmo en el intervalo de serie acotado:
```{r autoarima Serie Final}
auto.arima(serieFinal)
```

**Modelo 1 ajustado solo con datos Train **

Modelo ARIMA(0,1,1)(0,1,1)[12]:

```{r Modelo Arima B1}
model_1break <- Arima(serieejTrain, 
                 order = c(0,1,1), 
                 seasonal = list(order = c(0,1,1), period=12),
                 include.mean = TRUE)

summary(model_1break)
```

Se generan los gráficos de serie original, valores ajustados en período Train y predicción en período Test:
```{r Validacion B1}
fit22 <- forecast(model_1break, h=12) 

autoplot (serieFinal, series="data") +
  autolayer(model_1break$fitted,series="SARIMA(0,1,1)(0,1,1)[12]")+
  autolayer(fit22 , series="Prediction")+
  xlab("year") + ylab("noches")+ ggtitle("Pernoctes")+ theme_bw()+ theme (legend.title = element_blank(),legend.position = "bottom")
```

Se generan los criterios de información del modelo para Train y Test:
```{r Accuracy B1}
accuracy(fit22 , serieejTest)
```

### Analisis de residuos del modelo 

Se analizan los residuos del modelo 1 (Train):
```{r}
checkresiduals(model_1break)
LjungBoxTest(residuals(model_1break), k = 2, lag.max = 20)

```
Resultan p-values mayores que nivel de significancia 0.05 (el menor encontrado es 0.084), por lo que NO se rechaza la H0 de Ljung-Box test (H0: Los residuos están distribuidos independientemente).
El gráfico de FAC solo muestra apenas significativo un lag (8), pero se lo considera incluido por el error del intervalo de confianza.


### Predicciónes
```{r Modelo con Serie Final}
model_p <- Arima(serieFinal, 
                 order = c(0,1,1), 
                 seasonal = list(order = c(0,1,1), period=12),
                 include.mean = TRUE)

summary(model_p)
```


```{r Prediccion}

fit2 <- forecast(model_p, h=12) 

autoplot (serieFinal, series="data") +
  autolayer(model_p$fitted,series="SARIMA(0,1,1)(0,1,1)[12]")+
  autolayer(fit2 , series="Prediction")+
  xlab("year") + ylab("noches")+ ggtitle("Pernoctes")+ theme_bw()+ theme (legend.title = element_blank(),legend.position = "bottom")
```


## Serie de tiempo 2:  

Se importa el dataset y se configura como serie de tiempo:

```{r, include=FALSE, eval=TRUE} 

data2 <- read_excel("serie222-Indice de precios al consumidor.xls")
   
data2 %>% 
  mutate(IPC) %>% 
  head(4) %>% 
  gt() %>% 
  tab_header('Indice_precios')

#data21 <- data2[data2$IPC, ]

data2$fecha <- as.Date(data2$fecha)

#Verificar tipo de datos 
data.class(data2$`IPC`)
data.class(data2$fecha)
```

### Analisis exploratorio de la base

```{r} 
psych::describe(data2)
plot_histogram(data2) 
```


```{r SdT2} 
SdT2 <-  ts(data2$`IPC`, 
            frequency = 12, 
            start = c(2017,1))

data.class(SdT2)
```

Se realizan una breve visualización de los datos:

```{r autplot SdT2} 
autoplot(SdT2, main="IPC ENE-2017 a OCT-2021")

acf(SdT2, type = "cov",  main="Funcion de Autocovarianza")

#par(mfrow=c(2,1))
g1<-acf(SdT2, main="Funcion de Autocorrelacion")
g2<-pacf(SdT2, main="Funcion de Autocorrelacion Parcial")
```

Se agrega una visualización de los componentes de la serie de tiempo a partir de la ecuación básica (YT = Ct + St + Tt + Et)

```{r plot descomposicion SdT2}
plot(decompose(SdT2))
```

A priori, se observan períodos con variabilidad cambiante y cambios estructurales en la tendencia. 
También se observan tendencias variantes a lo largo de la serie.
Los coeficientes de autocorrelación decrecen rapidamente en el grafico de FAC (ACF).

Se comprueba el supuesto de normalidad de la serie original data2:

```{r test de normalidad} 
jarque.bera.test(data2$IPC)

ad.test(data2$IPC) 

shapiro.test(data2$IPC)
```

El valor de p-value es mayor al alpha = 0.05, por lo que NO SE RECHAZA la H0 de normalidad de la serie data2 para Jarque-Bera y Anderson-Darling. Sin embargo, el test Shapiro-Wilk indica que se debe RECHAZAR la H0 de normalidad, por lo tanto se analiza la variancia de la serie antes de avanzar con el modelo.


### Analisis de la estacionariedad
#### Estacionariedad en la varianza

Al rechazar normalidad, se define la transformación de potencia adecuada:

```{r Power Transform} 
summary(powerTransform(SdT2))
```

El resultante, indica un p-value significativo para un lambda=0, por lo que se aplica la transformación de potencia a la serie:
```{r Aplica Power Transform} 
lambda = powerTransform(SdT2)

data2$IPC_t <- data2$IPC ^ lambda$lambda

```


Se comprueba el supuesto de normalidad de la serie original data2 transformada:

```{r} 
jarque.bera.test(data2$IPC_t)

ad.test(data2$IPC_t) 

shapiro.test(data2$IPC_t)
```

En este caso, la H0 de normalidad se confirma (NO SE RECHAZA) en todos los test de normalidad.


Se construye ahora la serie de tiempo a partir de la variable transformada:

```{r Serie del tiempo transformada}
SdT2_t <- ts(data2$`IPC_t`, 
            frequency = 12,  
            start = c(2017,1))
```

Se verifica si la serie tiene perioricidad:
```{r}
frequency(SdT2_t)
```

La prueba arroja una frecuencia/estacionalidad de 12 (meses)

#### Estacionariedad en la media 

1) LJUNG - BOX

Si se rechaza H0 significa que hay coeficientes de autocorrelación distintos a cero, y hay variabilidad en la Variancia.

```{r LJUNG - BOX} 
Incorrelation <- function(ts, type = c("Ljung-Box","Box-Pierce"), fitdf = 0){
  p_ljung_box = NULL
  s_ljung_box = NULL
  for(i in 0:(length(ts)/4)){
    p_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$p.value
    s_ljung_box[i] = Box.test(ts,lag = i,type = type,fitdf = fitdf)$statistic
  }
  table = data.frame(j = 1:(length(ts)/4),
                     P_Value = p_ljung_box,
                     Statistic = s_ljung_box)
  return(table)
}

Incorrelation(SdT2_t,"Ljung-Box") # Ver funci?n autocorrelaci?n al incio del script
inco_wn = Incorrelation(SdT2_t,"Ljung-Box")
autoplot(ts(inco_wn$P_Value)) + ggtitle("Test de Ljung-Box", subtitle = "P-Value") + ylab("") # Grafico los p-value para disntitos lags

```

Todos los p-values dan muy bajos, por lo que SE RECHAZA la H0, y la serie tiene coeficientes de autocorrelación distintos a cero.

Tests de Raíces Unitarias para determinar estacionaridad.

2)  DICKEY-FULLER

H0: RAIZ UNITARIA ( NO ESTACIONARIO )
H1: NO HAY RAIZ UNITARIA ( ESTACIONARIO )

```{r DICKEY-FULLER2} 
df<-tseries::adf.test(SdT2_t)  
df   
```
El test de AUG D-F resulta en un p-value alto, por lo que no se rechaza la H0 y la serie de tiempo sería NO ESTACIONARIA.


3) Dickey-Fuller Aumentado 

3.1) TIPO 0 - NONE
```{r}
urca_none<-urca::ur.df(SdT2_t, selectlags = "AIC", type = "none")
summary(urca_none)
```
Si se toma un nivel de significancia de 0.05, el tau estadistico es mayor al valor crítico y por lo tanto NO se RECHAZA la H0. La serie no es estacionaria cuando no consideramos tendencia y drift.


3.2) TIPO 1 - DRIFT
```{r}
urca_drift<-urca::ur.df(SdT2_t, selectlags = "AIC", type = "drift")
summary(urca_drift)
```
Si se toma un nivel de significancia de 0.05, el tau estadistico es menor al valor crítico y por lo tanto se RECHAZA la H0. La serie es estacionaria con drift.
El valor del phi1-estadístico es menor al phi1 crítico para significancia de 0.05 (H0: existe raíz unitaria sin drift), lo cual lleva a no rechazar ésta H0. Esto no coincide con el resultado de tau.

3.3) TIPO 2 - TREND & DRIFT
```{r}
urca_trend<-urca::ur.df(SdT2_t, selectlags = "AIC", type = "trend")
summary(urca_trend)
```
Si se toma un nivel de significancia de 0.05, el tau estadistico es mayor al valor crítico y por lo tanto NO se RECHAZA la H0. La serie NO es estacionaria con tendencia y drift (pendiente e intercepto).
Los valores de phi2 y phi3 indican que NO SE RECHAZA las respectivas H0 para cada caso, en mismo nivel de significancia.
Los resultados de estos tres tests indican que la serie es estacionaria con drift.

4) ndiffs con argumentos iguales a este test:
```{r ndiffs2} 
ndiffs(SdT2_t, alpha = 0.05, test = "adf")
ndiffs(SdT2_t, alpha = 0.05, test = "kpss")
ndiffs(SdT2_t, alpha = 0.05, test = "pp")

```

Con una significancia de 0.05, para los test de Augm Dickey-Fuller y Philips-Perron la serie no precisa diferenciar.
Por otro lado, con test "kpss" el resultado indica que la serie debe ser diferenciada una vez.


5)TEST KPSS - en este caso la H0 indica que la serie es ESTACIONARIA con tendencia determinística.

```{r TEST KPSS}
kpss.test(SdT2_t)
```

Como el p-value es mayor al alpha = 0.05, NO SE RECHAZA la H0, e indica ESTACIONARIEDAD con tendencia determinística. Esto coincide con el resultado de ndiffs.

6) Se prueba con test de Phillips-Perron. Hipotesis nula que existe al menos una Raíz Unitaria:

```{r Phillips-Perron}
pp.test(SdT2_t)
```
El p-value resulta en RECHAZO de la H0, e indica que la serie no necesita ser diferenciada.  Esto coincide con el resultado de ndiffs para P-P.


7) Test de Zivot-Andrews - Como se interpreta de la grafica que puede haber cambios estructurales en la serie, realizamos el test ur.za.

```{r Zivot-Andrews}
SdT2_t.za <- ur.za(SdT2_t, model="both")
summary(SdT2_t.za)
```
El estadístico de test de la serie es menor al valor crítico (inclusive para alpha = 0.01), por lo que SE RECHAZA LA H0 de existencia de una raíz unitaria por cambio estructural. Esto indica que la serie es estacionaria con cambio estructural.

Se plantea un quiebre en el mes 36, equivalente con dic-2019.

```{r} 
plot(SdT2_t.za)
```

Se debe asumir que el test de Zivot-Andrews arroja resultados más confiables con grandes tamaños muestrales. En este caso, no se trataría de una muestra de este tipo.

### Cambios estructurales

##### Cambio estructural por tendencia ( SdT2_t ~ timeline )

Se busca determinar los cambios estructurales y las rectas de regresión lineal asociadas con el siguiente modelado:

```{r SdT2_t ~ timeline} 
len <- length(SdT2_t)
timeline <- 1:len
trend_fit <- lm(SdT2_t ~ timeline)
summary(trend_fit)
```
Los p-values de intercepto y tendencia son muy pequeños, por lo que RECHAZAMOS la H0 que los considera no significativos.

Se identifican los posible puntos de quiebre de la tendencia:

```{r breakpoints} 
SdT2_tbrk <- breakpoints(SdT2_t ~ timeline, h = 0.1)
summary(SdT2_tbrk)
```

```{r}
plot(SdT2_tbrk)
```

El mejor Criterio de información se da para m=1

Se visualiza el punto de corte:

```{r}
plot(SdT2_t)
lines(fitted(SdT2_tbrk, breaks = 1), col = 4)
lines(confint(SdT2_tbrk, breaks = 1))
```

Fecha de corte:

```{r fecha de corte} 
breakdates(SdT2_tbrk, breaks = 1)
```

Coeficientes de corte:

```{r coeficientes de corte} 
coef(SdT2_tbrk, breaks = 1)
```


```{r}
fitted.ts2 <- fitted(SdT2_tbrk, breaks = 1)
autoplot(fitted.ts2)
```

### Modelos

Se dividen los datos de train and test (80-20):

```{r Train and Test SdT2} 
SdT2_ttrain <- window(SdT2_t, 
                    start = c(2017,01), 
                    end = c(2020,10))

SdT2_ttest <- window(SdT2_t, 
                    start = c(2020,11),
                    end = c(2021,10))

```

Se genera el modelo según mejor performance:

```{r} 
auto.arima(SdT2_t, trace = T, stepwise = T)
```

A partir de la mejor sugerencia, se estima el modelo SARIMA: ARIMA(0,1,1)(1,0,0)[12]:
```{r Modelo SARIMA SdT2}
model2_1 <- Arima(SdT2_ttrain, 
                 order = c(0,1,1), 
                 seasonal = list(order = c(1,0,0), period=12),
                 include.mean = TRUE)

summary(model2_1)

fit21 <- forecast(model2_1, h=12)

autoplot (SdT2_t, series="data") +
  autolayer(model2_1$fitted,series="SARIMA(0,1,1)(1,0,0)[12]")+
  autolayer(fit21 , series="Prediction")+
  xlab("year") + ylab("variation")+ ggtitle("IPC_transformada")+ theme_bw()+ theme (legend.title = element_blank(),legend.position = "bottom")

```

Se estiman metricas de performance del modelo seleccionado: 
```{r}
accuracy(fit21, SdT2_ttest)
```

### Analisis de los residuos del modelo

Se analizan los residuos del modelo:

```{r}
checkresiduals(model2_1)
LjungBoxTest(residuals(model2_1), k = 2, lag.max = 20)
```
Resultan p-values mayores que nivel de significancia 0.05 (el menor encontrado es 0.381), por lo que NO se rechaza la H0 de Ljung-Box test (H0: Los residuos están distribuidos independientemente).
El gráfico de FAC no muestra lags significativos.


### Predicciones 

Se generaliza el modelo para toda la serie (Train+Test) y se realiza la predicción para 12 meses:

```{r Modelo final prediccion}

model2_p <- Arima(SdT2_t, 
                 order = c(0,1,1), 
                 seasonal = list(order = c(1,0,0), period=12),
                 include.mean = TRUE)

summary(model2_p)

fit21 <- forecast(model2_p, h=12)

autoplot (SdT2_t, series="data") +
  autolayer(model2_p$fitted,series="SARIMA(0,1,1)(1,0,0)[12]")+
  autolayer(fit21 , series="Prediction")+
  xlab("year") + ylab("variation")+ ggtitle("IPC_transformada")+ theme_bw()+ theme (legend.title = element_blank(),legend.position = "bottom")
```

Se procede luego a des-transformar la serie:

```{r Destranformar} 

fit212_dest <- bxcx(fit21$mean, lambda = lambda$lambda, InverseQ = TRUE, type = "power")
SdT2_dest <- bxcx(model2_p$fitted, lambda = lambda$lambda, InverseQ = TRUE, type = "power")  

```

Se genera una serie de tiempo a partir de los valores predichos de la variable destransformada:

```{r Serie de tiempo} 
fit212_dest <-  ts(fit212_dest, 
            frequency = 12, 
            start = c(2021,11))
```


```{r Plot con serie destrasformada} 
autoplot (SdT2, series="data") +
  autolayer(SdT2_dest,series="SARIMA(0,1,1)(1,0,0)[12]- Destransf.")+
  autolayer(fit212_dest, series="Prediction")+
  xlab("year") + ylab("variation")+ ggtitle("IPC")+ theme_bw()+ theme (legend.title = element_blank(),legend.position = "bottom")

```

