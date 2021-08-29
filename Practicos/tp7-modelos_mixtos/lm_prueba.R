library(nlme)
library(tidyverse)
library(magrittr)

# Voy a analizar el dataset Rail con tres modelos diferentes. En uno de los
# casos, vimos en clase que lm realizaba un ajuste polínomico. Veamos los tres
# casos

m1.lm <- lm(travel ~ 1, data = Rail)        # promedio de todos
m2.lm <- lm(travel ~ Rail, data = Rail)     # polinomio (es para factores que están ordenados)
m3.lm <- lm(travel ~ Rail - 1, data = Rail) # cada riel por separado

# En el primero, lm realiza un ajuste para obtener la media de todas las
# mediciones obtenidas, sin distinguir entre rieles. En el tercero, obtiene la
# media para cada riel por separado (pero recordemos que también realiza una
# estimación del desvío estándar para todos los rieles, asumiendo que es el
# mismo entre todos ellos). El caso problemático es el segundo, ya que realiza
# un ajuste polinómico que no tiene mucho sentido para estos datos. Esto ocurre
# porque la columna "Rail" del dataset está configurada como una variable 
# "ordenada" (o de ranking), cuando en realidad simplemente es un código
# arbitrario que nos permite identificar a cada riel. Es decir, no hay un primer
# riel, un segundo riel, etc.; cada riel podría estar identificado por un
# símbolo aleatorio y nada cambiaría en el dataset. ¿Por qué esa columna esta
# configurada para representar una variable "de orden", cuando sería más
# apropiado considerarla categórica? No podemos saber realmente, pero podemos
# ver que, para los ejemplos que plantea el libro (modelos m1.lm y m3.lm), usar
# una variable ordenada no trae problemas (lm nos da los modelos que esperamos).
# El problema viene cuando pedimos un ejemplo que no está considerado en el
# libro.

# Para solucionar esto, vamos a redefinir la columna "Rail": obtendremos una
# nueva columna que contiene un código alfanumérico, por lo tanto R va a
# considerar a la misma como categórica automáticamente. Luego, vamos a
# recalcular los modelos 2 y 3, y esperamos que ahora lm nos de lo que queremos:
# - Modelo 2: las diferencias de todos los rieles respecto al primero
# - Modelo 3: la media de cada riel por separado
# El modelo 1 no lo vamos a calcular nuevamente ya que al pedir la media de
# todos los datos, la columna Rail es irrelevante. El modelo 3 podríamos 
# omitirlo también, ya que lm lo calculaba como esperábamos, pero lo incluimos
# para estar seguros que eso ocurre.

Rail %<>% mutate(Rail_str = paste("R", as.character(Rail), sep=""))
m2b.lm <- lm(travel ~ Rail_str, data = Rail)     # diferencias respecto al riel 1
m3b.lm <- lm(travel ~ Rail_str - 1, data = Rail) # cada riel por separado

# Lo que está en el fondo de todo esto es la llamada matriz de diseño: es una
# matriz que vincula las mediciones con los predictores, y permite hallar los
# parámetros (pendientes y ordenadas del modelo). Para poder acceder a la matriz
# de diseño, usamos la función model.matrix, pasándole como argumentos:
# - la fórmula que define a cada modelo
# - el dataset Rail

matrix.m1 <- model.matrix(travel ~ 1, Rail)
# model.matrix(travel ~ 1, Rail %>% filter(Rail==1))
matrix.m2 <- model.matrix(travel ~ Rail, Rail)
matrix.m3 <- model.matrix(travel ~ Rail - 1, Rail)

matrix.m2b <- model.matrix(travel ~ Rail_str, Rail)
matrix.m3b <- model.matrix(travel ~ Rail_str - 1, Rail)

# Estas matrices definen lo que se llama "contrastes": cada parámetro asociado
# a la matriz define una comparación (o contraste) del mismo respecto a otros
# parámetros del problema, o bien respecto al 0. La matriz más sencilla de todas
# corresponde al modelo que calcula una media para todas las mediciones sin
# importar de qué riel provengan. Esta matriz es una columna que tiene todos 1s.
# Lo que significa es sumar todas las mediciones y dividir por la cantidad total
# (esto sale de aplicar álgebra de matrices para despejar los parámetros). La 
# siguiente más sencilla es la que permite obtener la media para cada riel por
# separado (matrix.m3 o matrix.m3b, son idénticas, solo cambia el nombre de la
# variable que identifica a cada riel). Esa matriz tiene 18 filas al igual que
# antes (ya que hay 18 mediciones) y 6 columnas (ya que queremos estimar 6
# parámetros, ya que tenemos 6 rieles). Si miramos cada columna, tiene 0s
# excepto en las filas que corresponden a los datos del riel asociado en el
# dataset. Esto significa, al igual que antes: sumar todas las mediciones de ese
# riel y dividir por la cantidad de mediciones de ese riel (es decir el promedio)
# La siguiente matriz interesante es matrix.m2b (calcula el promedio para uno
# de los rieles y luego las diferencias de los demás respecto a ese). La
# interpretación de esta matriz es un poco más difícil de las anteriores, ya que
# las filas que corresponden a la medición de cada riel tienen valores distintos
# de cero en más de una columna; esto significa que los parámetros asociados a
# ese riel dependen de las mediciones de otros rieles (y esto es esperable ya
# que, salvo para el riel que el modelo toma como referencia, para todos los
# demás obtendremos un parámetro que, justamente, lo compara con el riel
# referencia). Si vemos las primeras 3 filas de matrix.m2b, veremos que solo
# tiene 1s para la primera columna: esto significa "calcula la media de las
# mediciones del primer riel". Si vemos las siguientes 3 filas (que corresponden
# a las mediciones del segundo riel) tienen 1s en la primera y segunda columna.
# Como la primera columna representa al primer riel y la segunda al 2do, esto
# significa "usá la media del primer riel que ya obtuviste y comparala con la
# media de las mediciones del segundo riel". ¿Por qué cambia la interpretación
# respecto a las matrices anteriores en las que mirábamos cada columna? Todo se
# debe al hecho de pedirle al modelo que obtenga parámetros que requieren
# comparar rieles; las cuentas salen de aplicar álgebra de matrices, no son
# complicadas realmente pero sería largo de explicar aquí. Y como estás matrices
# son en general las más comunes, aunque uno no sepa despejarlas algebraicamente,
# son sencillas de identificar si nos explicaron como interpretarlas. Pero eso
# sí, si nos encontramos con matrices ligeramente diferentes, no vamos a saber
# qué modelo representan. El caso final es matrixm2. Vemos que esa matriz tiene
# coeficientes a simple vista arbitrarios. Esos coeficientes se obtienen de
# aplicar términos lineales, cuadráticos, cúbicos, etc. ¿Cómo se usan y obtienen?
# Para no hacerlo largo, les dejo un tutorial que cuenta algunas cosas
# interesantes: https://www.flutterbys.com.au/stats/tut/tut7.1.html

# Por último, ¿por qué lm usa una matriz en un caso y otra en otra? La clave
# está en si considera a los predictores como categóricos o "de orden". Para
# cada caso, tiene configurado un tipo de matriz por defecto. Eso lo podemos ver
# con el siguiente código:

# veamos las opciones
getOption("contrasts")

# para un caso (unordered), está usando contr.treatment, para el otro (ordered)
# contr.poly. Esas son funciones que podemos llamar y nos devuelven matrices
# de diseño según los parámetros que le pidamos. Por ejemplo, si tenemos un
# factor categórico de 3 niveles:

contr.treatment(3)

# nos da una matriz "miniatura" (es decir que no tiene en cuenta las repeticiones
# para cada nivel del factor, y que tampoco contiene la primera columna que tiene
# todos 1s). Podemos probar:

contr.treatment(6)

# Si mentalmente agregamos la primera columna con 1s, y las filas de cada repetición
# respetando las filas originales, recuperamos la matriz de diseño matrix.m2b

# Algo similar ocurre con contr.poly

contr.poly(6)

# nos da algo muy similar a matrix.m2, donde nuevamente tenemos que hacer el laburo
# mental de agregar la primera columna con 1s, y las filas de cada repetición.
# Ah un dato importante en las matrices de diseño: el orden de las filas de la
# matriz no es importante, siempre que las mediciones tengan el mismo orden.
# (El orden de las filas en matrix.m2 es distinto que en contr.poly(6), pero eso
# se debe a que lm consideró un cierto orden de las mediciones para el modelo 
# matrix.m2 que no es el mismo del dataset).


