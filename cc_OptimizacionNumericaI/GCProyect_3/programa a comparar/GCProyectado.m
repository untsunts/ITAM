function[x0] = GCProyectado(G, b, c)
%Ricardo Ortega Araujo
%25/Oct/2015
tic
%Este programa realiza la rutina del gradiente conjugado para matrices
%definidas positivas con un proyector que resuleve el problema

%   min 1/2p'Gp+p'c
%   Ap-b = 0

%Entradas
% G es la matriz simétrica positiva definida.
% A es la matriz de las restricciones
% b define el vector de Ax = b
% x0 es el punto inicial

%Primero se definirán las tolerancias y las iteraciones.

m = length(b);
n = length(G);
%tol1 = 10e-6;
tol2 = 10e-8;   
maxiter = 2*n;
% Se define la matriz de restricciones.
A = G(1:m,:);
x0 = A\b;


% Se determina el primer residuo r y el primer residuo proyectado y.

r = (G * x0 + c);
K = [eye(n,n) A'; A zeros(m,m)];
[L,D,P,S,neg,ran] = ldl(K);
    sol = P' * S * [r; zeros(m,1)];
    sol = L \ sol;
    sol = D \ sol;
    sol = L' \ sol;
    sol = S*P*sol;
y = sol(1:n,:);
d = -y;


iter = 0;

fprintf(1,'\n\n          i               ||r*y||            ||y||        d^t*G*d         \n\n');

while norm(r) > tol2 && iter < maxiter && d'*G*d > tol2
    
    aux1 = r' * y;
    aux2 = G * d;
    aux3 = d' * aux2 ;
    
    alpha = aux1 / aux3;
    x0 = x0 + alpha * d;
    r1 = r + alpha * G * d;
    
    K = [eye(n,n) A'; A zeros(m,m)];
    [L,D,P,S,neg,ran] = ldl(K);
    sol = P' * S * [r1;zeros(m,1)];
    sol = L \ sol;
    sol = D \ sol;
    sol = L' \ sol;
    sol = S*P*sol;
    
    y1 = sol(1:n,:);

    beta = r1' * y1 / aux1;
    d = - y1 + beta * d;
    r = r1;
    y = y1;
    
    normy = norm(y);
    normry = norm(r'*y) ;
    iter = iter + 1;
    
    fprintf(1,'       %4i           %14.8e     %14.8e       %14.8e        \n', iter, normry,normy, aux3);
    
    
end
t = toc
end
