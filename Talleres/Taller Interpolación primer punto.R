rm(list=ls())

#El polinomio interpolante que incluye a todos los puntos es un polinomio unico (puntos distintos (xi,yi))

#Puntos: (-2,5),(1,2),(2,-3) 

matriz <- matrix(c(1,-2,4,1,1,1,1,2,4), nrow=3,ncol = 3, byrow=TRUE)
a <- c(5,2,-3)

print(matriz)
print(a)

det(matriz) #corroborar que tenga solucion (det != 0)

coeficientes <- (solve(matriz,a)) #se encuentran los coeficientes del polinomio

cat("Polinomio interpolante es: ", coeficientes[1],"+(",coeficientes[2],"x)+ (",coeficientes[3],"x^2)")