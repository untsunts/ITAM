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

delta = 10000;

fout   = fopen('Reporte_GCP','w');
    
fprintf(fout,'\n     n     m    delta       ||r||         frontera      ||p||    curv     iter    CPU(s)    ||Ap + c - r||/||A||     \n');
fprintf(fout,'------------------------------------------------------------------------------------------------------------------------\n');

%%

%for j = 1:10


[ d1 , ~ ] = Dogleg (A1, c1, delta);
[ d2 , ~ ] = Dogleg (A2, c2, delta);
[ d3 , ~ ] = Dogleg (A3, c3, delta);

r1 = A1*d1 + c1;
r2 = A2*d2 + c2;
r3 = A3*d3 + c3;

normr1 = norm(r1);
normr2 = norm(r2);
normr3 = norm(r3);

[p1,t1,k1,e_a1,e_r1,frontera1,np1,curv1,N_ini1,err_ini1] = GCP_RC(G,A1,g1,-c1+r1,maxiter,delta,d1);
[p2,t2,k2,e_a2,e_r2,frontera2,np2,curv2,N_ini2,err_ini2] = GCP_RC(G,A2,g2,-c2+r2,maxiter,delta,d2);
[p3,t3,k3,e_a3,e_r3,frontera3,np3,curv3,N_ini3,err_ini3] = GCP_RC(G,A3,g3,-c3+r3,maxiter,delta,d3);

k4 = max(k1,k2);
k4 = max(k4,k3);

e_a = [e_a1(1:k4),e_a2(1:k4),e_a3(1:k4)];
e_r = [e_r1(1:k4),e_r2(1:k4),e_r3(1:k4)];


%figure(1);
%subplot(2,1,1);
%semilogy(e_a,'DisplayName','e_a');
%subplot(2,1,2);
%semilogy(e_r,'DisplayName','e_r');

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

if k1 == 0 
    ea1 = N_ini1;
    er1 = err_ini1;
else
    ea1 = e_a1(k1);
    er1 = e_r1(k1);
end

if k2 == 0 
    ea2 = N_ini2;
    er2 = err_ini2;
else
    ea2 = e_a2(k2);
    er2 = e_r2(k2);
end
if k3 == 0 
    ea3 = N_ini3;
    er3 = err_ini3;
else
    ea3 = e_a3(k3);
    er3 = e_r3(k3);
end

fprintf(fout,'\n     %5i     %5i    %7i       %8.6e        %3i      %6.4f    %3i     %5i    %8.6e   %10.5e    %10.5e    %10.5e    ', n, m, delta, normr1, frontera1, np1, curv1, k1,t1, ea1, er1, f(1));
fprintf(fout,'\n     %5i     %5i    %7i       %8.6e        %3i      %6.4f    %3i     %5i    %8.6e   %10.5e    %10.5e    %10.5e     ', n, 2*m, delta, normr2, frontera2, np2, curv2, k2,t2,ea2, er2, f(2));
fprintf(fout,'\n     %5i     %5i    %7i       %8.6e        %3i      %6.4f    %3i     %5i    %8.6e   %10.5e    %10.5e    %10.5e     ', n, m*(m-1), delta, normr3, frontera3, np3, curv3, k3,t3,ea3, er3, f(3));


%delta = delta*5;


%end

fclose(fout);

end



