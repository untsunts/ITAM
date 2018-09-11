%Guillermo Santiago Novoa P?rez    000125089
%
%diary resumen ;
%diary on;
% Este es un script que ayuda a correr el programa (funci?n) mad_max. Dadas las
% especificaciones del mismo, se corre 5 veces mad_max, las primeras 3
% con gradiente aleatorio y n?meros de condici?n 10^1, 10^2, y 10^3
% respectivamente y las siguientes dos veces para poder comprobar diferentes resultados de
% la tarea calculando el gradiente a partir de una soluci?n dada (x* =
% (1,1,1,1...)).

fprintf('\n programa por: Guillermo Santiago Novoa Perez\n');
    n = 10;                     
    max_iter = 10000;           
    TOL = 10^(-9);
    
%1
% 10^1
    ncond = 10^1;
fprintf(1,'\n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e\n\n', ncond, TOL);
    A = GenMat(n,ncond);
    g = randn(n,1);
    x0 = randn(n,1);
[ H10 , a ] = mad_max(A,g,x0,max_iter,TOL);

%2
% 10^2
    ncond = 10^2;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n\n', ncond, TOL);
    A = GenMat(n,ncond);
    g = randn(n,1);
    x0 = randn(n,1);
[ H100, a ] = mad_max(A,g,x0,max_iter,TOL);

%3
% 10^3
    ncond = 10^3;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n\n', ncond, TOL);
    A = GenMat(n,ncond);
    g = randn(n,1);
    x0 = randn(n,1);
[ H1000, a ] = mad_max(A,g,x0,max_iter,TOL);





%-------------------------- ITERACIONES CON X CONOCIDA
%4
%10^1 , x conocida antes de empezar a iterar.
    ncond = 10^1;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n', ncond, TOL);
fprintf('\n \t\t CASO X* CONOCIDA (X* = e) \n\n');
    A = GenMat(n,ncond);
    x1 = ones(n,1);
    g = -A*x1;                                 % se genera el gradiente de forma que x1 sea soluci?n al sistema
    x0 = randn(n,1);
[ Hcon, con ] = mad_max(A,g,x0,max_iter,TOL);
    L = ((ncond - 1)/(ncond + 1))^2;
        Q1(1,1) = ((Hcon(:,1)-x1)'* A* (Hcon(:,1)-x1))^2 ;
        Q1(2,1) = 0;
        Q1(3,1) = 0;
        Q1(4,1) = 0;
    for i=2:con
        Q1( 1, i ) = ((Hcon(:,i)-x1)'* A* (Hcon(:,i)-x1))^2 ;       % norma H de x_k - x*
        Q1( 2, i ) = L*Q1( 1, i - 1 );                              % norma H de x_k-1 - x* por L (cota superior de Q1(1,i)
        Q1( 3, i ) = ( Q1( 2, i ) - Q1( 1, i ) )/ Q1( 1,i ) ;       % diferencia (o error) relativ@ de Q1(2, i) y Q1(1, i)
        Q1( 4, i ) = ( Q1( 2, i ) - Q1( 1, i ) ) ;                  % diferencia ( o error) absolut@
    end
%-----
%Aqu?(se separ? del for de arriba para tener m?s claridad, aunque se podr?a haber realizado dentro)
%se guardan en una matriz la k te?rica (n?mero de iteraciones
%proyectado anal?ticamente para que el error de aproximaci?n alcance 10^(-5) ), la k verdadera (n?mero
%de iteraciones reales necesitadas por el m?todo) y el error de
%aproximaci?n en la iteraci?n k (te?rica).
kcon = 0;
cota = 10^(-5);
k(1,1) = k_proyectada( ncond, A, x0, x1, cota);
for i=1:con
    if( Q1(1,i) < cota && kcon == 0 )
        kcon = i;
    end
end
k(1,2) = kcon;
xk = Hcon(:,k(1,1));
k(1,3) = (xk - x1)'* A* (xk - x1);
%---------------------


%5
%10^3 , x conocida antes de empezar a iterar.
    ncond = 10^3;
fprintf(1,' \n\n \t\t Numero de condicion : %10.0f \n \t\t Tolerancia : %8.2e \n', ncond, TOL);
fprintf('\n \t\t CASO X* CONOCIDA (X* = e) \n\n');
    A = GenMat(n,ncond);
    x1 = ones(n,1);
    g = -A*x1;
    x0 = randn(n,1);
[ Hcon, con ] = mad_max(A,g,x0,max_iter,TOL);
    L = ((ncond - 1)/(ncond + 1))^2;
        Q2(1,1) = ((Hcon(:,1)-x1)'* A* (Hcon(:,1)-x1))^2 ;
        Q2(2,1) = 0;
        Q2(3,1) = 0;
        Q2(4,1) = 0;
    for i=2:con
        Q2( 1, i ) = ((Hcon(:,i)-x1)'* A* (Hcon(:,i)-x1))^2 ;       % norma H de x_k - x*
        Q2( 2, i ) = L*Q2( 1, i - 1 );                              % norma H de x_k-1 - x* por L (cota superior de Q2(1,i)
        Q2( 3, i ) = ( Q2( 2, i ) - Q2( 1, i ) )/ Q2( 1,i ) ;       % diferencia (o error) relativ@ de Q2(2, i) y Q2(1, i)
        Q2( 4, i ) = ( Q2( 2, i ) - Q2( 1, i ) ) ;                  % diferencia ( o error) absolut@
    end

%-----
%Aqu? (se separ? del for de arriba para tener m?s claridad, aunque se podr?a haber realizado dentro)
%se guardan en una matriz la k te?rica (n?mero de iteraciones
%proyectado anal?ticamente para que el error de aproximaci?n alcance 10^(-5) ), la k verdadera (n?mero
%de iteraciones reales necesitadas por el m?todo) y el error de
%aproximaci?n en la iteraci?n k (te?rica).
kcon = 0;
cota = 10^(-5);
k(2,1) = k_proyectada( ncond, A, x0, x1, cota);
for i=1:con
    if( Q2(1,i) < cota && kcon == 0 )
        kcon = i;
    end
end
k(2,2) = kcon;
xk = Hcon(:,k(2,1));
k(2,3) = (xk - x1)'* A* (xk - x1);
%-------------

%
%diary off;
