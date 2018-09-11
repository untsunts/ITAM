%Guillermo Santiago Novoa P?rez    000125089
%
diary on;
% Este es un script que ayuda a correr el programa (funci?n) mad_max. Dadas las
% especificaciones del mismo, se correr? 4 veces mad_max, las primeras 3
% con gradiente aleatorio y n?meros de condici?n 10^1, 10^2, y 10^3
% respectivamente y la cuarta para poder comprobar diferentes resultados de
% la tarea calculando el gradiente a partir de una soluci?n dada (x* =
% (1,1,1,1...)).

n = 10;                     
max_iter = 10000;           
TOL = 10^(-9);

% 10^1
ncond = 10^1;
fprintf(1,'\n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e\n\n', ncond, TOL);
A = GenMat(n,ncond);
g = randn(n,1);
x0 = randn(n,1);
[ H10, j ] = mad_max(A,g,x0,max_iter,TOL);

% 10^2
ncond = 10^2;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n\n', ncond, TOL);
A = GenMat(n,ncond);
g = randn(n,1);
x0 = randn(n,1);
[ H100, k ] = mad_max(A,g,x0,max_iter,TOL);

%10^3
ncond = 10^3;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n\n', ncond, TOL);
A = GenMat(n,ncond);
g = randn(n,1);
x0 = randn(n,1);
[ H1000, l ] = mad_max(A,g,x0,max_iter,TOL);
%

%10^3 , x conocida antes de empezar a iterar.
ncond = 10^3;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n', ncond, TOL);
fprintf('\n \t\t CASO X* CONOCIDA (X* = e) \n\n');
A = GenMat(n,ncond);
x1 = ones(n,1);
g = -A*x1;
x0 = randn(n,1);
[ Hcon, con ] = mad_max(A,g,x0,max_iter,TOL);
%
%
diary off;