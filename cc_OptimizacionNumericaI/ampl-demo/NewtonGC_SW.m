function[x, iterN, ng] = NewtonGC_SW(nombre, maxiter)

iterN = 0;

path(path,'/Users/Sofia/Documents/MATLAB/Analisis Aplicado/ampl-demo/bin2');
%
nombre_ampl = strcat(nombre, '.nl');


[ x0, ~, ~, y0, ~, ~] = spamfunc( nombre_ampl );
x = x0;

n = size(x0,1);

TOL = 1.0e-6;
maxGC = 2*n;

c1 = 1.0e-4;
c2 = 0.9;
prtlevel = 2;

%
% x0     punto inicial
% xlow   cotas inferiores para x
% xupp   cotas superiores para x
% y0     multiplicadores de Lagrange iniciales
% clow   cotas inferiores para las restricciones
% cupp   cotas superiores para las restricciones
%
%[ x0, xlow, xupp, y0, clow, cupp ] = spamfunc( nombre_ampl );

[ f, ~ ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
[ g, ~ ] = spamfunc( x, 1 );                  % evaluar el gradiente 
[ H ]    = spamfunc( y0 );  %H = full(H);      % evaluar la Hessiana

%[f, g, H] = feval(fun,x0);

%   fgname:  name of function that returns function and gradient
%            it expects as input only x and pars, a parameter structure 
%            it is invoked by: [f,g] = feval(fgname, x, pars)

ng = norm(g);

fprintf(1,'   k        f            ||g||    alpha     nfg \n');
fprintf(1,'------------------------------------------------\n');

while(ng > TOL && iterN < maxiter)
    
    [p] = GC(H,-g,TOL,maxGC);
    
    [ alpha, x, f, g, ~ , nsteps ] = linesch_sw(x, f, g, p, nombre, c1, c2, prtlevel);
    
    %function [alpha, x, f, grad, fail, nsteps] = ...
           %linesch_sw(f0, grad0, d, nombre, c1, c2, prtlevel);
    
    %[ faux, ~ ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
    %[ gaux, ~ ] = spamfunc( x, 1 );                  % evaluar el gradiente 
    
    [ H ]    = spamfunc( y0 );  %H = full(H);      % evaluar la Hessiana
    
    %[faux, gaux, H] = feval(fun,x);
    
    ng = norm(g);    
    iterN = iterN + 1;
    fprintf(' %3i  %14.8e   %8.2e   %5.3f    %3i  \n',  iterN,   f, ng,  alpha, nsteps ); 
    
end

fprintf('\n');

end
