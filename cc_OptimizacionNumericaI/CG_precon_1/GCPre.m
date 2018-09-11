%Guillermo Santiago Novoa P?rez
%%
function[x, k] = GCPre(A,b,L,x0,tol,maxiter)
%%
k = 0;
n = length(b);
%iid = eye(n);
x = x0;
r = A*x-b;
%d = -r;
y = L'\r;
y = L\y;
%rr = r'*r;
ry = r'*y;
d = -y;
normr0 = norm(r);
normrr = norm(r);
%
%%
    while(normrr> normr0*tol && k <= maxiter)    
        Ad = A*d;
        dAd = d'*Ad;
            alfa = ry/dAd;
            x = x + alfa*d;
            raux = ry;
            r = r + alfa*Ad;
            y = L'\r;
            y = L\y;
            ry = r'*y;
            beta = ry/raux;
            d = -y + beta*d;
            k = k + 1;
%%          if (dAd <=0 )
%            fprintf('curvatura cero o negativa')
%            break;  
%           end
%%
        normrr=norm(r);
    end
end