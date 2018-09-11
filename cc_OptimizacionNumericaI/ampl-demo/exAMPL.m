
function exAMPL ( nombre )
path(path,'/Users/Sofia/Documents/Analisis Aplicado/ampl-demo/bin2');
%
nombre_ampl = strcat(nombre, '.nl');
%
% x0     punto inicial
% xlow   cotas inferiores para x
% xupp   cotas superiores para x
% y0     multiplicadores de Lagrange iniciales
% clow   cotas inferiores para las restricciones
% cupp   cotas superiores para las restricciones
%
%[ x0, xlow, xupp, y0, clow, cupp ] = spamfunc( nombre_ampl );
[ x0, ~, ~, y0, ~, ~] = spamfunc( nombre_ampl );

n = length(x0);
x = x0;

[ f, ~ ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
[ g, ~ ] = spamfunc( x, 1 );                  % evaluar el gradiente 
[ H ]    = spamfunc( y0 );  %H = full(H);      % evaluar la Hessiana

fprintf( ' Nombre del problema      %10s \n', nombre);
fprintf( ' Numero de variables      %3i \n', n);
fprintf( ' Objectivo                %14.8e \n', f);

fprintf( ' Punto inicial \n');  x
fprintf( ' Gradiente \n');      g
fprintf( ' Hessiana \n');       H


