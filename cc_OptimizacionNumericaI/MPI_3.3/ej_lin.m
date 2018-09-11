Q = speye(5)*0;
A = [1,8/3,1,0,0;1,1,0,1,0;2,0,0,0,1];
b = [4;2;3];
c = [-2;-0.5;0;0;0];
x0 = ones(5,1);
s0 = x0;
lambda0 = ones(3,1);
tol = 10e-6;
[x1, lambda1, s1] = MPI_ej(Q,A,b,c,x0,lambda0,s0,1,tol);
[x2, lambda2, s2] = MPI_ej(Q,A,b,c,x1,lambda1,s1,.1,tol);
[x3, lambda3, s3] = MPI_ej(Q,A,b,c,x2,lambda2,s2,.01,tol);
[x4, lambda4, s4] = MPI_ej(Q,A,b,c,x3,lambda3,s3,.001,tol);

