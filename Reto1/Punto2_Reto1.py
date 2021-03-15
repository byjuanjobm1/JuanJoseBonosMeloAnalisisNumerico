#PUNTO NUMERO 2 RETO1
import numpy

#Para esta implementacion x[0]= x; x[1]= y;

def F(x):

  #Igualamos ambas ecuaciones a 0
  #Funcion1: x^2 + xy - 10 = 0
  funcion1= x[0]**2+(x[0])*(x[1])-10
  #Funcion2: y + 3xy^2 -57 = 0
  funcion2= x[1]+3*(x[0])*(x[1]**2)-57
  #Nos devuelve la matriz 
  return numpy.array([funcion1,funcion2])

#Jacobiana de F
def JacobianaF(x):

   #Nos devuelve la matriz donde sus componentes estan dadas por derivadas con respecto x, y
  return numpy.array([[2*x[0]+x[1],x[0]],[3*x[1]**2,1+2*x[1]*3*x[0]]])
 

numeroIteraciones = 100
x= numpy.array([1.5,3.5])

for j in range(numeroIteraciones):
  cajon=x
  #Sacar la Jacobiana Inversa
  Jacobianainversa=numpy.linalg.inv(JacobianaF(x))
  # x= JacobianaInversa x F
  x = x -numpy.dot(Jacobianainversa,F(x))
  #Sacar Error
  e=numpy.linalg.norm(x-cajon)
  #Imprimir numero Iteraciones - result - error
  print(j,x,e)
  #cumpla con el error (sea menor)
  if e<1e-16:
    break