#***Variables***
#Las variables en R son el objeto más sencillo.
a <- 3

#Con la función class() podemos comprobar el tipo de la variable.
class(a)    #[1] "numeric"

#Con la función length() podemos ver la longitud de la variable.
length(a)   #[1] 1

#Cadenas de caracteres.
nombre <- "Pepe"

#Fecha como una cadena de caracteres.
fecha <- "2025-06-19"

#Fecha como un tipo fecha.
fecha <- as.Date(fecha)

#Las operaciones son sobre días.
print(fecha-365)

#***Vectores***
#Los vectores son objetos que almacenan un grupo ordenado de datos del mismo tipo.
edades <- c(16, 23, 32, 27, 15)
deportes <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
nombres <- c("Carlos", "Manuel", "Laura", "Pepe", "Miguel")

#Accedemos al elemento 3 del vector edades (No empieza en 0).
edades[3]

#Accedemos a los elementos 2 y 5 del vector nombres.
nombres[c(2, 5)]

#Accedemos a todos los elementos del vector deportes, menos el 2.
deportes[-2]

#***Matrices***
#Creamos una matriz de 3(filas)x4(columnas) con números del 1 al 12, byrow=FALSE los ordena por columna.
matriz <- matrix(1:12, nrow = 3, ncol = 4, byrow = FALSE)
#Accedemos al elemento en la fila 3 y columna 2.
matriz[3, 2]
#Accedemos a todos los elementos de la fila 2.
matriz[2, ]
#Accedemos a todos los elementos de la columna 3.
matriz[ ,3]
#Accedemos a todos los elementos de la matriz, excepto las columnas 2 y 3.
matriz[ , -c(2, 3)]
#Accedemos a todos los elementos de la matriz, excepto la columna 1 y el renglón 3.
matriz[-3,-1]

#***Arreglos***
#Creamos un arreglo de 3 dimensiones con 3 elementos cada una.
arreglo <- array(c(1:9, 11:19, 21:29), c(3,3,3))
#Accedemos al elemento [1,2,3].
arreglo[1,2,3]
#Extraemos todos los datos del elemento [,,2]
arreglo[,,2]
#Extraemos todos los datos de la coluumna 3 de los arreglos.
arreglo[,3,]

#***Data frames***
df <- data.frame(nombres, edades, deportes)
#Extraemos todos los elementos del campo edad.
df$edades
#Extraemos todos los elementos del primer campo.
df[,1]
#Extraemos todos los elementos del campo deporte.
df[["deporte"]]
#Extraemos los datos que cumplan con la condición.
subset(df, edades >= 18)
subset(df, deportes == TRUE)
subset(df, deportes == TRUE & edades >= 18)

#***Listas***
#Las listas pueden almacenar otros objetos.
lista <- list(E1 = matriz, E2 = df, E3 = arreglo)
#Accedemos a los elementos E2 y E1 de la lista.
lista$E2
lista[[1]]

#funciones
RaizCuadradaInversaRapida <- function(numero) {
    x2 <- numero*0.5
    #y <- numero
    #i <- *y #Apuntador a y.
    #i <- 0x5f3759df - ( i >> 1 )
    #y <- i #Accedemos al valor de i (deapuntar).
    #y <- y * ( 1.5 - ( x2 * y * y ) )
    #y <- y * ( 1.5 - ( x2 * y * y ) ) #Segunda iteración
    y <- ( 1.5 - ( x2 * y * y ) )
    return(y)
}

PotenciaRecursiva <- function(numero, potencia){
    if (potencia == 0) {
       return(1)
    }
    return(numero * PotenciaRecursiva(numero, potencia-1))
}

print(PotenciaRecursiva(2,8))