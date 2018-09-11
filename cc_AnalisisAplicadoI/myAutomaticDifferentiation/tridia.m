function f = tridia(x);

n     = 100;
alpha = 2.0;
beta  = 1.0;
gamma = 1.0;
delta = 1.0;

f = gamma*(x(1)*delta-1.0)^2;

for i=2:n
    f = f + i*( -beta*x(i-1) + alpha*x(i))^2;
end

