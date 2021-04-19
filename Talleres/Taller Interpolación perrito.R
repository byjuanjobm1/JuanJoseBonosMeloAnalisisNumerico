#Punto Numero 5
#5. Utilice la interpolación de splines cubicos para el problema del contorno del perrito que esta en el libro: Numerical Analysis,Ninth Edition.Richard L. Burden and J. Douglas Faires (Chapter 3 pg 164,exercise 32), este debe incluir la parte inferior del perrito.
#Juan Jose Bolaños - David Andres Duarte - David Saveedra


#ParteSuperiorPerrito
x=c(1,   2,   5,   6,   7,  8,   10,  13, 17, 20, 24, 25, 27,  27.5, 27.7,  28,  29, 30)
y=c(3, 3.7, 3.9, 4.2, 5.7, 6.6, 7.1, 6.7, 4.4,  7,  6.1,  5.6, 5.8,  5.2,  4.1,   4.3, 4.1, 3)
x=x*1.2
# ParteInferiorPerrito 1
xi1=c(1, 5.3,   6, 5.7, 7.1, 8.8,  11,  12.2,  11.5,   10, 11.5, 8.3,   6)
yi1=c(3, 2.9, 3.4, 2.5,   2, 2.3, 2.1,   2.4,     3,  3.4,  4.5, 5.7, 4.5)
xi1= xi1*1.2
# ParteInferiorPerrito 2
xi2=c( 15,  17,  20, 24.5, 25.5, 26.5,  26, 24.4, 19.8)
yi2=c(2.4, 2.9, 2.8,    3,  3.2,  2.9, 2.4,  2.4, 2.4)
xi2= xi2*1.2
# ParteInferiorPerrito 3
xi3=c(12.2, 15, 19.8)
yi3=c(2.4, 2.4, 2.4)
xi3= xi3*1.2
# ParteInferiorPerrito 4
xi4=c( 26, 30)
yi4=c(2.4, 3)
xi4= xi4*1.2



y1 = y[1:4] ; x1=x[1:4]
y2 = y[4:6] ; x2 = x[4:6]
y3 = y[6:9] ; x3 = x[6:9]
y4 = y[9:12] ; x4 = x[9:12]
y5 = y[12:15] ; x5 = x[12:15]
y6 = y[15:18] ; x6 = x[15:18]
y7 = yi1 [1:2] ; x7 = xi1 [1:2]
y8 = yi1 [2:3] ; x8 = xi1 [2:3]
y9 = yi1 [3:4] ; x9 = xi1 [3:4]
y10 = yi1 [4:6] ; x10 = xi1 [4:6]
y11 = yi1 [6:8] ; x11 = xi1 [6:8]
y12 = yi1 [8:10] ; x12 = xi1 [8:10]
y13 = yi1 [10:11] ; x13 = xi1 [10:11]
y14 = yi1 [11:13] ; x14 = xi1 [11:13]
y18 = yi2[1:2] ; x18 = xi2 [1:2]
y19 = yi2 [2:5] ; x19 = xi2 [2:5]
y20 = yi2 [5:6] ; x20 = xi2 [5:6]
y21 = yi2 [6:8] ; x21 = xi2 [6:8]
y22 = yi2 [8:9] ; x22 = xi2 [8:9]
y23 = yi3 [1:3]  ; x23 = xi3 [1:3]

y24 = yi4 [1:2] ; x24 = xi4 [1:2]


plot(x, # Grafica del ejercicio.
     y,
     main = "Silueta del perro",
     xlab = "X",
     ylab = "Y",
     xlim=c(0,36),
     ylim=c(1,8.5))
par(new = TRUE)
plot(xi1,
     yi1,
     xlab = "",
     ylab = "",
     xlim=c(0,36),
     ylim=c(1,8.5))
par(new = TRUE)
plot(xi2,
     yi2,
     xlab = "",
     ylab = "",
     xlim=c(0,36),
     ylim=c(1,8.5))
par(new = TRUE)
plot(xi3,
     yi3,
     xlab = "",
     ylab = "",
     xlim=c(0,36),
     ylim=c(1,8.5))
par(new = TRUE)
plot(xi4,
     yi4,
     xlab = "",
     ylab = "",
     xlim=c(0,36),
     ylim=c(1,8.5))

lines(spline(x1, y1, n = 201), col = "black")
lines(spline(x1, y1, n = 201), col = "black")
lines(spline(x2, y2, n = 201), col = "black")
lines(spline(x3, y3, n = 201), col = "black")
lines(spline(x4, y4, n = 201), col = "black")
lines(spline(x5, y5, n = 201), col = "black")
lines(spline(x6, y6, n = 201), col = "black")

lines(spline(x7, y7, n = 201), col = "black")
lines(spline(x8, y8, n = 201), col = "black")
lines(spline(x9, y9, n = 201), col = "black")
lines(spline(x10, y10, n = 201), col = "black")
lines(spline(x11, y11, n = 201), col = "black")
lines(spline(x12, y12, n = 201), col = "black")
lines(spline(x13, y13, n = 201), col = "black")
lines(spline(x14, y14, n = 201), col = "black")


lines(spline(x18, y18, n = 201), col = "black")
lines(spline(x19, y19, n = 201), col = "black")
lines(spline(x20, y20, n = 201), col = "black")
lines(spline(x21, y21, n = 201), col = "black")
lines(spline(x22, y22, n = 201), col = "black")

lines(spline(x23, y23, n = 201), col = "black")

lines(spline(x24, y24, n = 201), col = "black")