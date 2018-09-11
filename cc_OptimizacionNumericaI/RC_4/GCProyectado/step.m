function [ tau ] = step ( d, z, delta );

a = d'*d; 
b = 2*(z'*d); 
c = z'*z - delta^2;

tau  = (-b + sqrt(b^2 - 4*a*c))/(2*a);
