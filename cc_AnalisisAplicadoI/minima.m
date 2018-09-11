% Dada una matriz y un vector, calcula la soluci?n de m?nima norma. 
% Guillermo Santiago Novoa P?rez no hizo nada, todo fue Sofi (o Sofo, no
% sabemos)
%21 de agosto de 2015

function[x_m, y_m, error]= minima(A, c);

        x_m = A\c;  % Solucion de matlab
        y_m = A'*inv(A*A')*c; %Solucion de minima norma
        error = norm(y_m - x_m);
end