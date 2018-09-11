function [ t, k, e, f ] = GCProyectado_RC(nombre, m, maxiter)

% Pagina 450 Nocedal

n = m*m;

G = gallery(nombre,m);

A1 = G(1:m,:);
A2 = G(1:2*m,:);
A3 = G(1:m*(m-1),:);

v1 = [G,A1';A1,0*speye(m,m)]*(0.5*ones(n+m,1));
v2 = [G,A2';A2,0*speye(2*m,2*m)]*(0.5*ones(n+2*m,1));
v3 = [G,A3';A3,0*speye(m*(m-1),m*(m-1))]*(0.5*ones(n+m*(m-1),1));

g1 = - v1(1:n);
c1 = - v1(n+1:n+m);

g2 = - v2(1:n);
c2 = - v2(n+1:n+2*m);

g3 = - v3(1:n);
c3 = - v3(n+1:n+m*(m-1));

delta = 100;

[ d1 , ~ ] = Dogleg (A1, c1, delta);
[ d2 , ~ ] = Dogleg (A2, c2, delta);
[ d3 , ~ ] = Dogleg (A3, c3, delta);

r1 = A1*d1 + c1;
r2 = A2*d2 + c2;
r3 = A3*d3 + c3;

[p1,t1,k1,e_a1,e_r1,frontera1,np1,curv1] = GCP_RC(G,A1,g1,-c1+r1,maxiter,delta);
[p2,t2,k2,e_a2,e_r2,frontera2,np2,curv2] = GCP_RC(G,A2,g2,-c2+r2,maxiter,delta);
[p3,t3,k3,e_a3,e_r3,frontera3,np3,curv3] = GCP_RC(G,A3,g3,-c3+r3,maxiter,delta);

k4 = max(k1,k2);
k4 = max(k4,k3);

e_a = [e_a1(1:k4),e_a2(1:k4),e_a3(1:k4)];
e_r = [e_r1(1:k4),e_r2(1:k4),e_r3(1:k4)];


figure(1);
subplot(2,1,1);
semilogy(e_a,'DisplayName','e_a');
subplot(2,1,2);
semilogy(e_r,'DisplayName','e_r');

e1 = norm(ones(n,1)-p1)/norm(ones(n,1));
e2 = norm(ones(n,1)-p2)/norm(ones(n,1));
e3 = norm(ones(n,1)-p3)/norm(ones(n,1));

f1 = norm(A1*p1+c1-r1,Inf)/norm(A1,Inf);
f2 = norm(A2*p2+c2-r2,Inf)/norm(A2,Inf);
f3 = norm(A3*p3+c3-r3,Inf)/norm(A3,Inf);

e = [e1,e2,e3];
t = [t1,t2,t3];
k = [k1 k2 k3];
f = [f1 f2 f3];

end



