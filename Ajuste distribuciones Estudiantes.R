#### REferencias ####
# http://www.dma.ulpgc.es/profesores/personal/stat/cursoR4ULPGC/10-distribProbabilidad.html
# http://www.dma.ulpgc.es/profesores/personal/stat/cursoR4ULPGC/9a-graf-Intro.html
# http://www.dma.ulpgc.es/profesores/personal/stat/cursoR4ULPGC/9c-grafHistograma.html
# https://cran.r-project.org/doc/contrib/grafi3.pdf
# https://www.datanalytics.com/libro_r/distribuciones-de-probabilidad.html

#Decisiones bajo incertidumbre en las organizaciones

#Análisis de datos para la toma de una decisión. Ajuste de funciones de
#distribución de probabilidad

#Este ejercicio consiste en ajustar funciones de distribución de probabilidad a un
#conjunto de datos y hacer pruebas de bonda de ajuste

#Paso 1: Importar y cargar las libreríías requeridas

install.packages("MASS")
install.packages("vcd")
library(MASS)
library(vcd)

#Paso 2: Lectura de datosE

#Lea los datos del archivo "Datos" y asignelos a un objeto cuyo nombre sea "datos"

#Sugerencia: Aunque el archivo está en formato .txt, es posible usar la función read.csv. Asegurese de remover el signo $ de la columna Revenue y de
#Escriba el argumento sep="\n" para que R reconozca que cada entrada est? enun rengl?n distinto

datos <- read_csv("~/github/Ajuste-de-distribuciones-de-probabilidad-en-R/Datos distribuciones.txt", 
                                 col_names = FALSE)
View(datos)


#Convierta el data frame en un vector

#Sugerencia: Use el comando as.numeric

v_datos <- as.numeric(datos$X1)

#Una vez tenga el vector de datos, haga un histograma para examinar alguna posible función de distribución
#de probabilidad. Asigne este histograma a un objeto h
h <- hist(v_datos)

#Use la función fitdistr(conjunto de datos, distribucion a ajustar) para explorar algunas posibles funciones de distribución de probabilidad.
#Ensaye con la gamma, lognormal y normal

gamma <- MASS::fitdistr(v_datos,"gamma")
lognormal  <- MASS::fitdistr(v_datos,"lognormal")
normal <- MASS::fitdistr(v_datos,"normal")

#Haga un gráfico de los datos vs cada distribución ajustada

