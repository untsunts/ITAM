
function[x] = PruebaGCP(G, b, c)
m = length(b);
n = length(G);
A = G(1:m,:);
b = ones(m,1);
K = [G A'; A zeros(m,m)];
[L,D,P,S,neg,ran] = ldl(K);
    sol = P' * S * [-c; b];
    sol = L \ sol;
    sol = D \ sol;
    sol = L' \ sol;
    sol = S*P*sol;
x = sol(1:n,:);
end