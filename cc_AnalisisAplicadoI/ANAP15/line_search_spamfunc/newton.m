%
% ... Ejemplo rudimentario del metodo de Newton globalizado para 
% optimizacion de funciones sin restricciones. El ejemplo ilustra
%
% 1. como usar AMPL via spamfunc
% 2. como usar la busqueda lineal de Overton con spamfunc
%
%
function [x] = newton ( nombre );
path(path,'/Users/jmorales/bin2');
%
nombre_ampl = strcat(nombre, '.nl');
%
% ... tolerancias y valores iniciales
%
[ x0, xlow, xupp, y0, clow, cupp ] = spamfunc( nombre_ampl );
n = length(x0);
x = x0;

[ f, c ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
[ g, A ] = spamfunc( x, 1 );                  % evaluar el gradiente 
[ H ]    = spamfunc( y0 );                    % evaluar la Hessiana

%
TOL = 1.0d-8; k = 0; x = x0;  k = 0; ITMAX = 200;
c1 = 1.0e-4; c2 = 0.9; prtlevel = 2;
%
norm_g = norm(g);

fprintf(1,'   k        f            ||g||    alpha     nfg \n');
fprintf(1,'------------------------------------------------\n');

while  norm_g > TOL  &  k < ITMAX
    
    p_N = -H\g;
    
    [alpha, x, f, g, fail, nsteps] = ...
                    linesch_sw(x, f, g, p_N, nombre, c1, c2, prtlevel, []);
   
    [ H ] = spamfunc( y0 );
   
    norm_g   = norm(g); 
    k = k + 1;
    fprintf(' %3i  %14.8e   %8.2e   %5.3f    %3i  \n', ...
                k,   f,      norm_g,  alpha, nsteps );   
end
