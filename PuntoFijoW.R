i=0
rAitken = c(0)
resultados = c(0)
tol = 1e-52
obetenerPresicion = function(tol){
  n = log(tol, 10)*-1
  return (n)
}
cantidadNumeros = function(n, precision){
  return(formatC(n,format = "f", digits = precision))
}
precision = obetenerPresicion(tol)
aitken = function (resultados)
{
  cont = 1
  while(cont < length(resultados)){
    rAitken[cont]= cantidadNumeros(resultados[cont+2]-(((resultados[cont+2]-resultados[cont+1])^2)/(resultados[cont+2]-(2*resultados[cont+1])+resultados[cont])), precision)
    cont = cont+1;
  }
  return(rAitken)
}
  
puntoFijo = function(a,b) 
{
  
  listaErrorAnt = c(0)
  listaErrorAct = c(0)
  cont = 0
  num = vector ( "double", 3)
  #Intervalo
  a1 = a
  b1 = b
  cat("===================================\n")
  cat("Iteración","          x", "\n")
  cat("===================================\n")

  if(Gx(a) > a || Gx(b) < b)
  {
    
    x=(a+b)/2
    dx = 0
    while (cantidadNumeros(Gx(x),precision) != cantidadNumeros(x,precision))
    {
      eAnterior = dx
      dx=abs(a-b)/2
      
      if(dx > tol)
      {
        if (Gx(x) < x)
        {
          b = x
        }
        else {a = x}
      }
      else 
      {
        break
      }
      x=(a+b)/2
      if(i==44) break
      i = i+1
      cat("    ", i,"    ",cantidadNumeros(c(x), precision), "\n")
      resultados[i] = x
      if(i>1)
      {
        listaErrorAnt = c(listaErrorAnt , eAnterior)
        listaErrorAct = c(listaErrorAct, dx)
      }
      
    }
    
    #imprime la relaci[on de error -> Convergencia lineal
    points(listaErrorAnt, listaErrorAct, col = "blue")
    lines(listaErrorAnt, listaErrorAct, col = "blue")
    
    cat("I = ", i, " Raíz = ", cantidadNumeros(c(x),precision), " con error menor que -> ",tol, "\n")
    #cat("I = ", i, " Raíz = ",formatC(c(x),digits=14, width = -15, format = "f", flag = " "), "invervalo [", a1,",", b1,"] ", " con error menor que -> ",tol)
  }
  else
  {
    cat("No tiene raíz la funcion en ese intervalo\n")
  }
  return(resultados)
}

imprimir = function(rAitken,resultados){
  cont = 1
  cat("===================================\n")
  cat("Iteración","     Aitken", "\n")
  cat("===================================\n")
  while(cont<length(resultados)){
    cat("    ", cont,"         ",rAitken[cont], "\n")
    cont=cont+1
  }
}

#PROBLEMA
#A
#Fx = function(x) (cos(x))^2-x^2
#Gx = function(x) cos(x)

#B
#Fx = function(x) x*sin(x)-1
#Gx = function(x) 1/sin(x)

#C
#Fx = function(x) x^3+2*x^2+(4/3)*x-8/27
#Gx = function(x) ((-x^3+2*x^2+(8/27))*3)/4

#D
#Gx = function(x) ((9.80665*68.1/x)*(1-exp(-(x/68.1)*10))-40)
#Gx = function(x) ((9.80665*68.1)/40)*(1-exp(-(x/68.1)*10))


#E
#Fx = function(x) x^3-2*x-5
Gx = function(x) (2*x+5)/x^2

#graficando relación de error
plot.function(Gx, xlim=c(0,0.5), ylim=c(0,1),  main = "Relación error",xlab = " Error i ",ylab = " Error i+1 ",col ="white")
abline(h = 0, v = 0:2/2, lty = 3, col = "green")
resultados = puntoFijo(-1,2)
rAitken = aitken(resultados)
imprimir(rAitken,resultados)

