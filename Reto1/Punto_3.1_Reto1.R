
library(pracma)
f = function(x) x^3-2*x^2+(4*x/3)-(8/27)
#listaTolerancias(1e-10,1e-32,1e-50)
brent(f, 0.5, 1,maxiter=100, tol=2^-50)
