function[x_fin, iter, norm_c, f_fin] = Newton( f, c , x0, tol, max_iter)
% Guillermo Santiago Novoa P?rez
% Est? funci?n calcula una aproximaci?n para un minimizador local del
% problema de optimizaci?n:
%           minimizar   f(x)
%           sujeta a    c(x)=0
%       
%       f: R^n----->R
%       c: R^n----->R
%       f, c son continuamente diferenciables 
%           
% par?metros de entrada:
%       f   - funci?n a minimizar (escrita en matlab)
%       c   - restricciones que se deben cumplir (escrita en matlab)
%       x0  - punto inicial
%       tol - criterio de paro, norma del gradiente de la funci?n de Lagrange
%       max_iter - criterio de paro, n?mero m?ximo de iteraciones
% par?metros de salida:
%       x_fin   - aproximaci?n final
%       iter    - n?mero de iteraciones
%       norm_c  - norma de las restricciones en la aproximaci?n
%       f_fin   - valore de la funci?n objetivo en x_fin
%
% fase de inicio:
% calcular la norma del gradiente de la funci?n de Lagrange
norma_L = 
iter = 0;
while norma_L>tol && iter<max_iter
    
end

end