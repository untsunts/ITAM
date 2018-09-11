function [ f, k, x ] = MPI_Mehrotra(Q,c,A,b)

% Sofia Samaniego de la Fuente 


% Este algoritmo resuelve el problema cuadratico:
%
%                     min (1/2)x'*Q*x + c'*x 
%                          s.a. Ax = b 
%
% a traves del Metodo de Puntos Interiores

%--------------------------------------------------------------------------

%--- Inicializacion de variables

tol = 1.e-4;
maxiter = 200;

[m,n] = size(A);
e = ones(n,1);

k = 0;

%--- Puntos iniciales

x = e;
s = e;
lambda = A'\(Q*x-s+c); 
mu = (x'*s)/n;

Xinv = sparse(diag(1./x));
S = sparse(diag(s));

K = [(Xinv*S + Q), A'; A, 0*speye(m,m)];
K = sparse(K);
rp = A*x - b;
rd = Q*x + c - A'*lambda - s;
LD = [(-rd -s); -rp];

norm_rd = norm(rd, Inf);
norm_rp = norm(rp, Inf);
norm_mu = norm(mu, Inf);


while ((norm_rd > tol || norm_rp > tol || norm_mu > tol) && k < maxiter)
    
    if k == 0
       fprintf('\n   k          f           barr_log         ||rp||       ||rd||      ||mu||\n');
       fprintf('------------------------------------------------------------------------------\n');
    end
    
    %--- Sistema KKT
    
    [L,D,P,S]= ldl(K);
    delta  = LDL_Solver( L, D, P, S, LD );
    
    dx = delta(1:n);
    dl = - delta(n+1:n+m);
    ds = Q*dx - A'*dl + rd;
   
    
    %--- Recorte de alfas
    
    alpha_x = Max_Steplength(x, dx);
    alpha_s = Max_Steplength(s, ds);

    mu_aff = ((x + alpha_x*dx)'*(s + alpha_s*ds))/n;
    sigma = (mu_aff/mu)^3;
    
    LD =  LD + [(sigma*(Xinv*(mu*e)) - Xinv*((diag(dx)*diag(ds)*e))); zeros(m,1)];
    delta  = LDL_Solver( L, D, P, S, LD );
    
    dx = delta(1:n); 
    dl = - delta(n+1:n+m); 
    
    alpha_x = Max_Steplength(x, dx);
    alpha_s = Max_Steplength(s, ds);
    
    alpha_x = min(0.995*alpha_x,1);
    alpha_s = min(0.995*alpha_s,1);
    alpha_l = min(alpha_x,alpha_s);
    
    x = x + alpha_x*dx;
    lambda = lambda + alpha_l*dl;
    s = s + alpha_s*ds;
    mu = x'*s/n;
    
    Xinv = sparse(diag(1./x));
    S = sparse(diag(s));
    
    K = [Xinv*S + Q, A'; A, 0*speye(m,m)];
    K = sparse(K);
    rp = A*x - b;
    rd = Q*x + c - A'*lambda - s;
    LD = [(-rd -s); -rp];

    norm_rd = norm(rd, Inf);
    norm_rp = norm(rp, Inf);
    norm_mu = norm(mu, Inf);
    
    f = (1/2)*(x'*Q*x)+ c'*x ;
    barr = f - mu*(e'*log(s));
    k = k+1;
    
    fprintf(' %3i  %15.8e  %15.8e   %9.2e    %9.2e   %9.2e \n', k,   f, barr, norm_rp, norm_rd, mu  );
       
end


end

