# Time-Series

Se analizan las dos series por separado. El procedimiento se describe a continuación:

a) Identificación

Representar gráficamente la serie, además de su función de autocorrelación simple (ACF) y función de autocorrelación parcial (PACF).
La gráfica de la serie nos indica si la serie es estacionaria o no.
Según los motivos por los que la serie no es estacionaria, tendremos que aplicar los siguientes procedimientos hasta hacerla estacionaria.
- Si tiene tendencia: determinamos si tiene tendencia determinística o estocástica.
- Si la serie tiene estacionalidad: Tomaremos diferencias estacionales hasta que desaparezca el patrón estacional.
- Si es heterocedástica, es decir, no tiene varianza constante, habrá que transformar la serie.

b) Estimación y verificación.

Luego representaremos las mismas a través de modelos ARIMA o SARIMA según corresponda.
Observando las dos gráficas del ACF y PACF de la serie transformada podemos hacernos una idea del modelo que describe nuestra serie, o al menos de cuáles son los primeros candidatos que debemos probar.
Para comprobar analíticamente (no visualmente) un modelo frecuentemente se ajusta varios modelos candidatos ARIMA(p,d,q) y escogeremos como un buen modelo aquel que tenga los residuales semejantes al de un ruido blanco, además que tenga los valores del AIC (Criterio de Información de Akaike) y BIC (Criterio de Información Bayesiana) menores con relación al resto de los modelos candidatos.

c) Predicción.

Una de las razones de la popularidad del proceso de construcción de modelos es su éxito en la predicción. Los modelos son buenos para realizar predicciones a corto plazo. Una vez representadas estudiaremos el comportamiento de los residuos de los modelos para determinar si se comportan como un ruido blanco. O si por el contrario precisan transformaciones.

d) Comparación de los resultados con los modelos determinados en el trabajo anterior.

Una vez obtenidos los Criterios de Información de los modelos de este trabajo práctico (a partir de la función accuracy), los compararemos con los resultados del trabajo práctico anterior, para determinar cuál modelo aplicado tiene una mejor probabilidad de ajuste.
