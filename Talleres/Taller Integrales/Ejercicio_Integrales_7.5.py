import numpy 
from scipy import integrate
from scipy.interpolate import lagrange


def Trapecio(interpolacion,a,b,n):
    cont = 0
    h = (b-a)/n
    inta = interpolacion(a)
    intb = interpolacion(b)
    for i in range(1,n):
     cont = cont +  interpolacion((i*h)+a)
     result = (h*(inta + intb + (cont*2)))/2
     
     return result
 
 
def Simpson(interpolacion,a,b,n):
    cont = 0
    h = (b-a)/n
    inta = interpolacion(a)
    intb = interpolacion(b)
    for i in range(1, n):
        inth = interpolacion((h*i)+a)
        cont = cont + inth * (2*((i%2)+1))
        result = (h*(inta+intb+cont))/3
        
        return result
    

def Interpol():
     x = numpy.array([0,0.2,0.4,0.6,0.8]) 
     y = numpy.array([3.592,3.110,3.017,2.865,2.658])
     print("Vector 1: ",x)
     print("Vector 2: ",y)
     pol = lagrange(x,y)
     print("Polinomio interpolado con Lagrange: ", pol)
     return pol    
     
     

     
    
     


interp = Interpol()
n = 10
print("Con n = ",n)
print("Aproximacion usando la Regla del Trapecio:")  
for i in range(1,n+1):
    print(i,"  ", Trapecio(interp,0,0.8,i))
print("Aproximacion usando Regla de Simpson:") 
for i in range(1,n+1):
    print(i,"  ", Simpson(interp,0,0.8,i)) 
print("Aproximacion usando Romberg con polinomio de grado 2:")  
print(integrate.romberg(interp,0,0.8,divmax = 2))
  

        