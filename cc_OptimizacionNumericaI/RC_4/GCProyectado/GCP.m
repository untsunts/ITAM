function[x,t,k,N,err] = GCP(G,A,c,b,maxiter)

tic

tol = 1e-6;

n = size(G,1);
m = size(A,1);

M = [speye(n,n),A'; A,0*speye(m,m)];
N = nan(maxiter,1);
err = nan(maxiter,1);

[ L , D , P, S, ~, ~] = ldl(M); 

x = LDL_Solver(L,D,P,S,[zeros(n,1);b]);
x = x(1:n);
r = G*x + c;
y = LDL_Solver( L, D, P, S, [r;zeros(m,1)] );
y = y(1:n);
d = -y;
ry = r'*y;

e1 = sqrt(abs(ry));
cp = e1*tol;

norm_ry = e1;

k = 0;

while((norm_ry > cp) && k <= maxiter)
    
    Gd = G*d;
    dGd = d'*Gd;
    
    if (dGd <= 0)
        fprintf('curvatura cero o negativa');
        return
    else
        
        alpha = ry/dGd;
        x = x+alpha*d;
        aux = ry;
        r = r+alpha*Gd;
        y = LDL_Solver( L, D, P, S, [r;zeros(m,1)] );
        y = y(1:n);
        ry = r'*y;
        beta = ry/aux;
        d = -y + beta*d;
        k = k+1;
        norm_ry = sqrt(abs(ry));
        N(k) = norm_ry;
        err(k) = sqrt(abs(ry))/e1;
    end
    
end
t = toc;
k = k-1;
