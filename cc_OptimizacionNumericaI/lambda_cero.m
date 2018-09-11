function[lambda] = lambda_cero(g, A)

[Q,R] = qr(A);
R=R';

x = R\g;

lambda = Q*x;

end