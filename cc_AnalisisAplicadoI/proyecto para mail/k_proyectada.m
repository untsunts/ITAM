function [ k ] = k_proyectada( cond, H, x0, xf, cota)
% Funci?n que, a partir de la aproximaci?n inicial, la x ?ptima y la cota
% que se quiera asegurar para el error de aproximaci?n, entrega una k que
% expresa el n?mero te?rico de iteraciones que necesita el m?todo
% programado anteriormente (de m?ximo descenso) para pasar esa cota.

L = (cond - 1)/(cond + 1);
e_inicial =(( x0 - xf )'* H* ( x0 - xf ))^2;
LD = (-1)* ( 5*log(10) + log( e_inicial ))/(2* ( log( L ))) - 1;
k = ceil( LD );

end

