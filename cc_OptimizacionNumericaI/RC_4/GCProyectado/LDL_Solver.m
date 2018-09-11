function [ x ] = LDL_Solver( L, D, P, S, b )
% Resuelve el sistema lineal Ax = b con A matriz sparse
% a traves de la factorizacion LDL

    x = P'*S*b;
    x = L\x;
    x = D\x;
    x = L'\x;
    x = S*P*x;

end

