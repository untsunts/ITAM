%Guillermo Santiago Novoa P?rez
%%
function[x, k, r] = GCPre(G,b,M,maxiter,tol)
%%
k = 0;
n = length(G);
m = length(M) - n;
%iid = eye(n);
c = b(n+1:n+m);
caux = [zeros(n,1);c];
[L,D,P,S,neg,ran] = ldl(M);
x0 = LDL_solver(L,D,P,S,caux);
x = x0(1:n);
g = b(1:n);
r = G*x-g;
res = [r;zeros(m,1)];
%d = -r; 
y = LDL_solver(L,D,P,S,res);
%y = L'\r;
%y = L\y;
%rr = r'*r;
y = y(1:n);
ry = r'*y;
d = -y;
%normr0 = norm(r);     % salida poisson2
%normrr = normr0;
normr0y = norm(ry,1);  %  salida poisson
normrry = normr0y;   
%
%%
    while(normrry> normr0y*tol && k <= maxiter)    
        Ad = G*d;
        dAd = d'*Ad;
            alfa = ry/dAd;
            x = x + alfa*d;
            raux = ry;
            r = r + alfa*Ad;
            res = [r;zeros(m,1)];
%            y = L'\r;
%            y = L\y;
            y = LDL_solver(L,D,P,S,res);
            y = y(1:n);
            ry = r'*y;
            beta = ry/raux;
            d = -y + beta*d;
            k = k + 1;
%%
           if (dAd <=0 )
            fprintf('curvatura cero o negativa')
            return  
           end
%%
        %normrr=norm(r);        
        normrry=norm(ry,1);
    end
   r = normrry/normr0y; 
end