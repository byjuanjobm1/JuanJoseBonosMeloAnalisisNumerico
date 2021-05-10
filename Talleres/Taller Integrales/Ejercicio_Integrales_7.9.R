rm(list=ls())


f = function(x){
  return(1+sin(exp(3*x)))
}
x = seq(-1, 1, by=0.1)
y = f(x)
#SIMPSON
simpson = function(f, a,b, n) {
  integ = integrate(f,a,b)
  val2 = integ$value
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2)
  i2 = seq(2, n-2, by = 2)
  y = f(a+(0:n)*h)
  abs(h/3 * ( f(a) + f(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) ))
  suma = abs(h/3 * ( f(a) + f(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) ))
  error = abs( val2-suma)
  cat("Integral Simpson: ",suma, "\n")
  cat("Error Simpson: ",error, "\n")
}

#TRAPECIO COMPUESTA
trapezoid = function(f, a, b, n) {
  integ = integrate(f,a,b)
  val2 = integ$value
  h = (b-a)/n
  x = seq(a, b, by=h)
  y = f(x)
  s = h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  suma = h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  cat("Integral trapecio: ",suma,"\n")
  error = abs(val2-suma)
  if (error == 0) {
    cat("Error trapecio: 0 ","\n")
  } else {
    cat("Error trapecio: ",error,"\n")
  }
  
}

a = -1          # Limite Inferior. 
b = 1      # Limite Superior. 
tol = 1e-8    # Error permitido 
n = 10
trapezoid(f,a,b,n)
simpson(f,a,b,n)