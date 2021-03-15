library(pracma)
library(mi)
library(Rmpfr)
n=50
tol = 2^-50
a = 0.0
b= 5.0
cantidadNumeros = function(n, precision) {
  return(formatC(n, format = "f", digits = precision))
}


brent = function(f, x0, x1, n, tol)
{
  fx0 = f(x0)
  fx1 = f(x1)
  if (abs(fx0) < abs(fx1)){
    aux1 = x0
    x0 = x1
    x1 = aux1
    
    aux2 = fx0
    fx0 = fx1
    fx1 = aux2
  }
  x2 = x0
  fx2 = fx0
  bandera = TRUE
  i = 0
  d = 0
  
  while (i < n & abs(x1 - x0) > tol){
    fx0 = f(x0)
    fx1 = f(x1)
    fx2 = f(x2)
    
    if (fx0 != fx2 & fx1 != fx2){
      op1 = (x0 * fx1 * fx2) / ((fx0 - fx1) *  (fx0 - fx2))
      op2 = (x1 * fx0 * fx2) / ((fx1 - fx0) * (fx1 - fx2))
      op3 = (x2 * fx0 * fx1) / ((fx2 - fx0) * (fx2 - fx1))
      s = op1 + op2 + op3
    } else{
      s = x1 - ((fx1 * (x1 - x0)) / (fx1 - fx0))
    }
    
    if ((s < ((3 * x0 + x1) / 4) | s > x1) |
        (bandera == TRUE & (abs(s - x1)) >= (abs(x1 - x2) / 2)) |
        (bandera == FALSE & (abs(s - x1)) >= (abs(x2 - d) / 2)) |
        (bandera == TRUE & (abs(x1 - x2)) < tol) |
        (bandera == FALSE & (abs(x2 - d)) < tol)){
      s = (x0 + x1) / 2
      bandera = TRUE
    } else{
      bandera = FALSE
    }
    fs = f(s)
    d = x2
    x2 = x1
    
    if ((fx0 * fs) < 0){
      x1 = s
    } else{
      x0 = s
    }
    
    if (abs(fx0) < abs(fx1)){
      aux3 = x0
      x0 = x1
      x1 = aux3
    }
    
    
    i = i + 1
  }
  cat("Raiz: ", cantidadNumeros(x1, 20), "\n", "Número de iteraciones: ", i)
}

funcion = function(x) x^3 -2*x^2+(4/3)*x-(8/27)
brent(funcion,a,b,n,tol)