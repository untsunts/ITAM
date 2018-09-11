%Guillermo Santiago Novoa P?rez -000125089
%%
function[x, k] = GC(A,b,x0,tol,maxiter)
%%
%Este programa recibe de argumentos una matriz s.p.d., un vector (gradiente
%inicial), una tolerancia que sirve como parametro para saber si el metodo
%sigue o no 
%(si la norma del residuo / gradiente es mayor a la tolerancia),
%y un numero maximo de iteraciones para parar por si no se llega al
%"optimo".
%El programa aplica el metodo del gradiente conjugado a los parametros de
%entrada y regresa un vector x el cual es la solucion al problema de
%minimizacion de la funcion cuadratica (o la mejor aproximacion si es que
%no se llega a la solucion en ese numero maximo de iteraciones).
%De punto inicial (o semilla), el programa utiliza al origen en R^n.
%%
%
k = 0;
%n = length(b);
x = x0;
r = A*x-b;
d = -r;
rr = r'*r;
normr0=sqrt(rr);
normrr=sqrt(rr);
%
%%
    while(normrr> normr0*tol && k <= maxiter)  
        Ad = A*d;
        dAd = d'*Ad;       
            alfa = rr/dAd;
            x = x + alfa*d;
            raux = rr;
            r = r + alfa*Ad;
            rr = r'*r;
            beta = rr/raux;
            d = -r + beta*d;
            k = k + 1;
%%          if (dAd <=0 )
%            fprintf('curvatura cero o negativa')
%            break;
%           end
%%
        normrr=sqrt(rr);
    end
end