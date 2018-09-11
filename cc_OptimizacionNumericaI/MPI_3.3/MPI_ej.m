function [ x, lambda, s ] = MPI_ej( Q, A, b, c, x,lambda,s, mu ,tol)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
[m,n]=size(A);
e=ones(n,1);
maxiter=100;
k=0;

    sigma = 1;
    xinv=1./x;
    Xinv=diag(xinv);
    S=diag(s);

    rp=A*x-b;
    rd=-A'*lambda-s+c+Q*x;

while k<maxiter &&  (norm(S*x-mu*e)>tol||(norm(rd)>tol || norm(rp)>tol)) 
    k=k+1;
    M= [(Q+Xinv*S), A'; A, speye(m,m)*0];
    F=[-rd-s+sigma*Xinv*mu*e; -rp];
    M = sparse(M);
    %if condest(M)<Inf
    
   [L,D,P,S]=ldl(M);
    %end
   d = LDL_Solver(L,D,P,S,F);
   dx = d(1:n);
   dlambda = -d(n+1:m+n);
   
   %Calculo de alphax
   index_x = find(dx<0);
    alfa_x = min(-x(index_x)./dx(index_x));
    
    if isempty(alfa_x) 
        alfa_x = 1;
    else
        alfa_x = min(alfa_x,1);
        if (alfa_x ~= 1)
            alfa_x = 0.9995*alfa_x;
        end
    end
   
   %Calculo de alphas
   ds = Q*dx-(A'*dlambda)+rd;
   index_s = find(ds<0);
    alfa_s = min(-s(index_s)./ds(index_s));
    
    if isempty(alfa_s) 
        alfa_s = 1;
    else
        alfa_s = min(alfa_s,1);
        if (alfa_s ~= 1)
            alfa_s = 0.9995*alfa_s;
        end
    end
   
   %Calculo de alphalambda
   alpha_lambda = min(alfa_x,alfa_s);
   
   %Se redefinen 
   x = x+alfa_x*dx;
   lambda = lambda+alpha_lambda*dlambda;
   s = s+alfa_s*ds;
   
   fprintf('\n %4i    %11.4e     %8.2e     %8.2e     %8.2e     \n',k,c'*x,norm(rp),norm(rd),mu)
    % mu=x'*s/n;    
    xinv=1./x;
    Xinv=diag(xinv);
    S=diag(s);
    rp = A*x -b;
    rd = Q*x -A'*lambda -s +c;
end
   
end  

