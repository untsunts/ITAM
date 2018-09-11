 function [ p, flag ] = driver_CG(n)
%
% ... funcion para probar el metodo de gradiente conjugado con una region
% de confianza.
%
rng('default');
G = randn(n,n);
G = (G + G')/2;        %  G es simetrica  n x n
% m = min(eig(G));
% G = G -1.2*m*eye(n);
% eig(G);
G = gallery('lehmer',n);
c     = -G*ones(n,1);
TOL   = 1.0e-6;
delta = 50.0e0;


[ p, flag ] = CG_TR( G, c, delta, TOL);
