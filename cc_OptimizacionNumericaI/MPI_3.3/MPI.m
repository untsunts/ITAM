function [ x,k,rp,rd,mu ] = MPI( Q, A, b, c,sigma ,tol)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
[m,n]=size(A);
e=ones(n,1);
maxiter=100;
k=0;

    x=ones(n,1);
    s=ones(n,1);
    mu=(x'*s)/n;
    lambda=ones(m,1);
    xinv=1./x;
    Xinv=diag(xinv);
    S=diag(s);

    rp=A*x-b;
    rd=-A'*lambda-s+c+Q*x;

while k<maxiter &&(mu>1e-8 || (norm(rd)>tol || norm(rp)>tol)) 
    k=k+1;
    M= [(Q+Xinv*S), A'; A, speye(m)*0];
    F=[-rd-s+sigma*Xinv*mu*e; -rp];
    M = sparse(M);
    %if condest(M)<Inf
    
   [L,D,P,S]=ldl(M);
    %end
   F = P'*S*F;
   d=S*P*(L'\(D\(L\F)));
   dx=d(1:n);
   dlambda = d(n+1:m+n);
   
   %Calculo de alphax
   neg=dx<0;
   alpha=min(dx(neg));
   alpha_ind=(dx<=alpha);
   alphax = -x(alpha_ind)/alpha;
    if(alphax>1 || alphax<0)
       alphax=1;
    end   
   alphax=.9995*alphax;
   
   %Calculo de alphas
   ds=Q*dx-A'*dlambda+rd;
   neg=ds<0;
   alpha=min(ds(neg));
   alpha_ind=(ds<=alpha);
   alphas = -s(alpha_ind)/alpha;
   if(alphas > 1 || alphas < 0)
       alphas=1;
   end
   alphas=.9995*alphas;
   
   %Calculo de alphalambda
   alphalambda=min(alphax,alphas);
   
   %Se redefinen 
   x=x+alphax*dx;
   lambda=lambda+alphalambda*dlambda;
   s=s+alphas*ds;
   
   fprintf('\n %4i    %11.4e     %8.2e     %8.2e     %8.2e     \n',k,c'*x,norm(rp),norm(rd),mu)
    mu=x'*s/n;    
    xinv=1./x;
    Xinv=diag(xinv);
    S=diag(s);
    rp=A*x-b;
    rd=A'*lambda+s-c;
end
   
end  

