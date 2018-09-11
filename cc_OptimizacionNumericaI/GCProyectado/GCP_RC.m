function[x,t,k,N,err,frontera,np,curv, N_ini, err_ini] = GCP_RC(G,A,c,b,maxiter,delta, v)

tic

tol = 1e-6;
frontera = 0;
curv = 0;

n = size(G,1);
m = size(A,1);

M = [speye(n,n),A'; A,0*speye(m,m)];
N = nan(maxiter,1);
err = nan(maxiter,1);

[ L , D , P, S, ~, ~] = ldl(M); 

%x = LDL_Solver(L,D,P,S,[zeros(n,1);b]);
%x = x(1:n);
x = v;

r = G*x + c;
y = LDL_Solver( L, D, P, S, [r;zeros(m,1)] );
y = y(1:n);
d = -y;
ry = r'*y;

e1 = sqrt(abs(ry));
cp = e1*tol;

norm_ry = e1;

N_ini = norm_ry;
err_ini = sqrt(abs(ry))/e1;

k = 0;

while((norm_ry > cp) && k <= maxiter)
    
    Gd = G*d;
    dGd = d'*Gd;
    
    if (dGd <= 0)
        if (k == 0)
            np = norm(x);
            x = (delta/np)*x;
            np = norm(x);
            t = toc;
            return
        end
        tau = step(d, x, delta);  
        frontera = 1;
        curv = 1;
        x = x + tau*d;
        np = norm(x);
        t = toc;
        return
    else
        if k == 0
            np = norm(x);
            if np >= delta
                x = (delta/np)*x; 
                t = toc;
                return
            end
        end
        alpha = ry/dGd;
        if norm(x+alpha*d)>= delta
            tau = step (d, x, delta);
            x =  x + tau*d;
            frontera = 1;
            np = norm(x);
            t = toc;
            return
        end
    end
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

t = toc;
k = k-1;
np = norm(x);

end

