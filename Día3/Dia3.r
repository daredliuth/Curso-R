library(readxl) #Pa leer archivos de excel.
library(ggplot2) #Para gráficas más pro.
library(quantmod) #Pa calcular  rendimientos.

ruta <- "/home/daredliuth/Documentos/Cursos/Curso-R/Día3/Walmex.xlsx" #Ubicación del archivo a leer.
walmex <- read_excel(ruta)

par(bg = "lightsteelblue2")
plot(walmex$Date, walmex$Close, type = "l", lwd = 2,
     col = "blue3", main = "Precios de Walmex", cex.main = 2,
     col.main = "yellow", cex.lab = 0.8, ylab = "Precio",
     xlab = "Fecha", col.lab = "steelblue4", cex.axis = 0.8,
     col.axis = "steelblue", cex.sub = 0.7,
     sub = "Precios del 1 de enero al 31 de diciembre de 2024"
    )

abline(h = seq(50, 75, 5), lty = 2, col = "steelblue2")
abline(v = seq(min(walmex$Date), max(walmex$Date),
     length.out = 6), lty = 2, col = "steelblue2"
    )
#***Cálculo de rendimientos***
almex <- xts(as.numeric(walmex$Close), order.by = as.Date(walmex$Date))
rendimientos <- dailyReturn(walmex)
plot(rendimientos)

hist(rendimientos)
hist(rendimientos, breaks = 20, border = "white",
     main = "Histograma de rendimientos", xlab = "Rendimiento",
     ylab = "Frecuencia", cex.lab = 0.8, cex.axis = 0.7)
     curve(dnorm(x, mean = mean(rendimientos), sd = sd(rendimientos)), add = T,
     lwd = 2, col = "firebrick", lty = 3)

abline(v = mean(rendimientos), lty = 2, lwd = 2)
legend("topleft", c("Rendimientos", "Distribución normal",
         paste0("Promedio = ", round(mean(rendimientos), 3))
        ),
     fill = c("grey", "firebrick", "black"), bg = NA, border = NA, cex = 0.8
    )