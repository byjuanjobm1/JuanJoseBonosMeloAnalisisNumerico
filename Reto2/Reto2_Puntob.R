# Reto 2b
# Juan Jose Bolaños Melo
# David Santiago Saavedra
# David Andres Duarte

library(pracma)

ruta_csv_Itatira <- "C:\\Users\\aulasingenieria\\Desktop\\Retob\\ItatiraF.csv"


ruta_csv_SantaQuiteira <- "C:\\Users\\aulasingenieria\\Desktop\\Retob\\SantaF.csv"



#Lectura de los datos
datosEstacionItatira = read.csv(file = ruta_csv_Itatira ,header = TRUE,sep = ";")

datosEstacionSantaQuiteira = read.csv(file = ruta_csv_SantaQuiteira ,header = TRUE, sep = ";")


#Se declaro varaible incio
inicio <- 1
#Se declaro varaible hasta
hasta <- 720
#Se declaro varaible indice
indice <- 1

coordenadax = seq(inicio, hasta, indice)


# Leemos los archivos de extension .csv

# columna $temp

coordenaday = datosEstacionItatira$Temp

# columna $dia

dias = datosEstacionItatira$Dia

# columna $Hora

horas = datosEstacionItatira$Hora

#Se declaro varaible indicador

indicador <- coordenadax


vect = rep(1, 720)

contadox = 1

contadory = 1

eliminar = sample.int(720,720*0.2)
for (j in eliminar) {
  vect[j] = 0
}


nuevaCoordenadaX <- c()

nuevaCoordenadaY <- c()



#Validacion de datos adecuados

for (e in vect)
{
  if(e == 1)
  {
    nuevaCoordenadaX[contadox] = coordenadax[contadory]
    nuevaCoordenadaY[contadox] = coordenaday[contadory]
    contadox = contadox + 1
  }
  contadory = contadory +1
}


#Se grafica interpolacion estacioness

#Llamado a la funcion plot

plot(coordenadax,coordenaday,type='l', ylab = "Temp estacion Itatira", xlab = "Indicador")

#Llamado a la funcion plot

lines(spline(nuevaCoordenadaX,nuevaCoordenadaY,n=200),col=4)


interpolacion <- splinefun(nuevaCoordenadaX,nuevaCoordenadaY)


vectInterpolacion = c()

contador <- 1

er = c()

for(z in coordenadax)
{
  erY = interpolacion(z)
  
  aux = abs((coordenaday[contador] - erY)/coordenaday[contador])
  er = c(er, abs((coordenaday[contador] - erY)/coordenaday[contador]))
  contador = contador + 1
}

#Llamado a la funcion max
max(er)

#Llamado a la funcion min
min(er)

#Llamado a la funcion mean
mean(er)

#Imprimir
print(er)



vectCalculo = c()

for (j in 1:length(datosEstacionSantaQuiteira$Dia))
{
  
  Dia2 = datosEstacionSantaQuiteira$Dia[j]
  Hora2 = datosEstacionSantaQuiteira$Hora[j]
  
  for(k in 1:720)
  {
    
    if((dias[k] == Dia2) && (horas[k] == Hora2))
    {
      vectCalculo = c(vectCalculo,indicador[k])
    }
  }
}


estaciony = c()

erEstaciony = c()

w = 1

for (fx in vectCalculo)
{
  estaciony = c(estaciony, interpolacion(fx))
  
  erEstaciony = c(erEstaciony, abs((datosEstacionSantaQuiteira$Temp[w] - estaciony[w])/datosEstacionSantaQuiteira$Temp[w]))
  w = w + 1
}


#Llamado a la funcion plot

plot(vectCalculo,datosEstacionSantaQuiteira$Temp, ylab = "Temperaturas estacion Santa Quteira", xlab = "Indicador", type = 'l')

#Llamado a la funcion lines
lines(vectCalculo, estaciony, col = 6)

#Imprimir
print(erEstaciony)

max <- 0

mean <- 0

for (er in erEstaciony) {
  
  if(er > max)
    max = er
  
  mean = mean + er
  
}

min <- 200

for (er in erEstaciony) {
  
  if(er < min)
    min = er
  
}

print(min)


qqnorm(erEstaciony)

qqline(erEstaciony)

print(mean/length(erEstaciony))
print(max)