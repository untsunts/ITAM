%Guillermo Santiago Novoa P?rez -000125089

function[x] = GC(A,b,tol1,maxiter)
%Este programa recibe de argumentos una matriz s.p.d., un vector (gradiente
%inicial), dos tolerancias que sirven como parametro para saber si el metodo
%sigue o no 
%(si la norma del residuo / gradiente es mayor a la tolerancia o si la A
%norma de la direccion k es menor a la segunda tolerancia),
%y un numero maximo de iteraciones para parar por si no se llega al
%"optimo".
%El programa aplica el metodo del gradiente conjugado a los parametros de
%entrada y regresa un vector x el cual es la solucion al problema de
%minimizacion de la funcion cuadratica (o la mejor aproximacion si es que
%no se llega a la solucion en ese numero maximo de iteraciones).
%De punto inicial (o semilla), el programa utiliza al origen en R^n.

%
n = length(b);
x = zeros(n,1);   %semilla
r = A*x-b;
d = -r;
k = 1;
rr = r'*r;
%

while(sqrt(rr)> tol1 && k <= maxiter)
    
    Ad = A*d;
    dAd = d'*Ad;
    
    if (dAd >= tol1) %checa si la A norma de d es mayor a la tolerancia dos para que el programa pueda continuar
        if(dAd <=0)
            fprintf('curvatura cero o negativa \n');
            break;
        else
            fprintf('se detuvo el metodo por el segundo criterio de paro, dtAd<=tol2 \n');
            break;
        end
            
            alfa = rr/dAd;
            x = x + alfa*d;
            raux = rr;
            r = r - alfa*Ad;
            rr = r'*r;
            beta = rr/raux;
            d = -r + beta*d;
            k = k + 1;
    end
end