#R cuenta con los datos precargados "mtcars".
datos <- mtcars$mpg

#La media.
media <- mean(datos)
media

#La desviación estándar.
desv <- sd(datos)
desv

#Probabilidad acumulada de un valor específico, dada una media y una desviación estándar.
pnorm(max(datos), media, desv)

#El valor correspondiente a una probabilidad, con una media y una desviación estándar definidas.
#Podemos utilizar vectores para obtener varios valores.
qnorm(c(0.68, 0.95, 0.99), media, desv)

#Genera una secuencia de números dando un inicio, un final y un paso.
seq(1, 3, 0.5)

#Repite un valor un número determinado de veces.
rep(3, 9)

#Muestra lso primeros datos de una secuencia.
head(data, 3)

#Muestra los últimos valores de una secuencia.
tail(data, 1)

#Redondeo.
round(sqrt(2), 2)

#Redonde hacía arriba.
ceiling(sqrt(2))

#Creamos un vector con 5 elementos.
x <- c(5, 2, 8, 4, 6)
#Lo ordenamos de manera ascendente.
sort(x, decreasing = FALSE)

#Indica las posiciones que ocuparía cada valor si se ordena
rank(x)

#Gráfica
datos <- mtcars
media <- mean(datos$qsec)
desv <- sd(datos$qsec)

random <- rnorm(100 * length(datos$qsec), media, desv)
sd(random)
histograma <- hist(random, breaks = 50)
plot(histograma)
title(main="Gráfica")

qnorm(0.68, media, desv)

#Después del descanso

#Podemos quitar variables utilizando el comando rm
#rm(histogram)
#Podemos borrar todo con
#rm(list=ls()) 

#***Estructuras de control***

#if
calificacion <- 8
if (calificacion >= 6) {
    print("El alumno aprobó")
}

#if else
calificacion <- 5
if (calificacion >= 6) {
    print("El alumno aprobó")
} else {
    print("El alumno reprobó")
}

#ifelse (Realiza lo mismo que lo anterior, pero es más compacto).
calificacion <- 8
ifelse(calificacion >=6, print("El alumno aprobó"), print("El alumno reprobó"))

#for
cuadrado <- rep(NA, 10) #Repetimos NA 10 veces y lo guardamos en la variable.
print("Antes:")
print(cuadrado)
#Iteramos del 1 al 10.
for (i in 1:10) {
    cuadrado[i] <- i^2
}
print("Después:")
print(cuadrado)

#while
monto <- 100
objetivo <- 150

while(monto < objetivo){
    monto <- monto + 10
    print(monto)
}

#repeat
#Repite una pieza de código de manera infinita (o hasta que se encuentra un break).
monto <- 100
objetivo <- 150
repeat{
    monto <- monto +10
    print(monto)
    if (monto == objetivo) break
}

#***Creación de funciones***
#@daredliuth: Sí hay scope en las funciones.
MiFuncion <- function(param1, param2){
    #Hacemos algo
    return()
}

Chicharronera <- function(a,b,c){
    x1 <- (-b + sqrt(b^2 - (4 * a * c))) / (2 * a)
    x2 <- (-b - sqrt(b^2 - (4 * a * c))) / (2 * a)
    return(c(x1,x2))
}

#a=-1, b=7, c=-10
print(Chicharronera(-1,7,-10))

ChicharroneraConSalsa <- function(a,b,c){
    disc <- b^2 - 4 * a * c
    if(disc < 0){
        stop("La ecuación no tiene soluciones reales.")#Detenemos la ejecución del programa y mostramos un mensaje de error.
    }
    
    x1 <- (-b + sqrt(b^2 - (4 * a * c))) / (2 * a)
    x2 <- (-b - sqrt(b^2 - (4 * a * c))) / (2 * a)

    f <- function(x) a * x^2 + b * x + c

    x_vals <- seq(min(x1,x2) - 2, max(x1, x2 + 2), length.out = 200)
    y_vals <- f(x_vals)
    
    plot(x_vals, y_vals, type = "l", main = "Parábola y soluciones", xlab = "x", ylab = "y", lwd = 2)
    abline(h = 0, lty = 2, col = "blue")
    points(c(x1, x2), c(0, 0), pch = 19, col = "red")
    text(x1, 0, labels = "x1", pos = 3)
    text(x2, 0, labels = "x2", pos = 3)
    cat("x1 =", round(x1, 2), "\n")
    return(cat("x2 =", round(x2, 2), "\n"))
}

ChicharroneraConSalsa(-1,7,-10)