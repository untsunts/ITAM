function[] = Minimanorma(m,n)

b=randn(m,1);
A = randn(m,n);
Sol_M = A\b
A=A';
[Q,R]=qr(A,0);
y=R'\b;
x_min=Q*y

normx=norm(x_min,2)
normsol=norm(Sol_M,2)

end