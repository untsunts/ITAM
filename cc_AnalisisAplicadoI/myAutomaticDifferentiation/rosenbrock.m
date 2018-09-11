%
% ... Rosenbrock function
%  j-l morales
%  marzo, 2015
%  ITAM

function f = rosenbrock(x);
c = 100;
f = c*( x(2) - x(1)^2 )^2  +  ( 1 - x(1) )^2;