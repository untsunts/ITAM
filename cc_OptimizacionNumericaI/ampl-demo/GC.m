function[x] = GC(A,b,tol,maxiter)

n = size(b,1);
x = zeros(n,1);
r = b-A*x;
d = r;
k = 1;
rr = r'*r;

while(sqrt(rr)> tol && k <= maxiter)
    Ad = A*d;
    dAd = d'*Ad;
    if (dAd <= 0)
        %fprintf('curvatura cero o negativa');
        break;
    else
        alpha = rr/dAd;
        x = x+alpha*d;
        aux = rr;
        r = r-alpha*Ad;
        rr = r'*r;
        beta = rr/aux;
        d = r + beta*d;
        k = k+1;
    end
end