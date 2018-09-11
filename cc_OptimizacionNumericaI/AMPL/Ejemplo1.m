%The problem is not convex
%SIF input: Ph. Toint, June 1993.
%classification QQR2-AN-5-3

param xinit{1..5};
var x{i in 1..5} := xinit[i];

minimize f:
	0.01*x[1]^2+x[2]^2;
subject to cons1:
	x[1]+x[2]-x[3]^2 = 25.0;
subject to cons2:
	x[1]^2+x[2]^2-x[4]^2 = 25.0;
subject to cons3:
	x[1]-x[5]^2 = 2.0;

data;
param xinit:=
1	15.811
2	1.5811
3	0.0
4	15.083
5	3.7164;

  display f; display x;