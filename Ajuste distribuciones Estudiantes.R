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

datos <- read.csv("~/github/Ajuste-de-distribuciones-de-probabilidad-en-R/Datos distribuciones.txt",
                  header = FALSE, 
                  col.names = "X1")
View(datos)


#Convierta el data frame en un vector

#Sugerencia: Use el comando as.numeric

v_datos <- as.numeric(datos$X1)

#Una vez tenga el vector de datos, haga un histograma para examinar alguna posible función de distribución
#de probabilidad. Asigne este histograma a un objeto h


# Histograma de los datos
h <- hist(v_datos, xlab = "Dato", ylab = "Frecuencia", las = 1, main = "")

# Gráfico de densisdad de los datos
plot(density(v_datos), xlab = "Dato", ylab = "Densidad", las = 1, main = "")

# qqnorm(v_datos, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales", las = 1, main = "")
# qqline(v_datos)

#Use la función fitdistr(conjunto de datos, distribucion a ajustar) para explorar algunas posibles funciones de distribución de probabilidad.
#Ensaye con la gamma, lognormal y normal

gamma <- MASS::fitdistr(v_datos,"gamma")
lognormal <- MASS::fitdistr(v_datos,"lognormal")
normal <- MASS::fitdistr(v_datos,"normal")

#Haga un gráfico de los datos vs cada distribución ajustada

# URL's de referencia
# http://www.dma.ulpgc.es/profesores/personal/stat/cursoR4ULPGC/9c-grafHistograma.html
# http://rstudio-pubs-static.s3.amazonaws.com/134452_3e7f07ccc03446129e0d82499209ffe9.html
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/GammaDist.html
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Lognormal.html
# https://www.rdocumentation.org/packages/graphics/versions/3.6.0/topics/curve

# Histograma
hist(v_datos, xlab = "Dato", ylab = "Densidad", las = 1, main = "", freq = FALSE, col = "lightcyan")
# Linea Densidad
lines(density(v_datos), col="red", lwd=2)
# Normal
curve(dnorm(x,mean=normal$estimate[1],sd=normal$estimate[2]), add=TRUE, col="blue", lwd=2)
# Gamma
curve(dgamma(x,shape = gamma$estimate[1], rate = gamma$estimate[2], scale = 1/ gamma$estimate[2]), add=TRUE, col = "green", lwd=2)
# Lognormal
curve(dlnorm(x,meanlog = lognormal$estimate[1],sdlog = lognormal$estimate[2], log = FALSE), add=TRUE, col = "orange", lwd=2)
#Legenda
legend("topleft",
       col=c("red","blue","green","orange"),
       legend =c("Densidad normal datos","Densidad normal estimada","Desindad gamma estimada","Densidad lognormal estimada"),
       lwd=2, bty = "n")


# curve(pgamma(x,shape = gamma$estimate[1], rate = gamma$estimate[2], lower.tail = TRUE, log.p = TRUE))
# curve(pgamma(x,shape = gamma$estimate[1], rate = gamma$estimate[2], scale = 1/ gamma$estimate[2]))
# curve(qgamma(x,shape = gamma$estimate[1], rate = gamma$estimate[2], scale = 1/ gamma$estimate[2]))
# curve(rgamma(x,shape = gamma$estimate[1], rate = gamma$estimate[2], scale = 1/ gamma$estimate[2]))
# curve(plnorm(x,meanlog = lognormal$estimate[1],sdlog = lognormal$estimate[2], lower.tail = TRUE, log.p = TRUE), add=TRUE)