function[M, k]=  mad_max(H, g, x0, max_iter, TOL)
%Guillermo Santiago Novoa P?rez
%
%
%Este programa emplea el m?todo del gradiente para encontrar puntos ?ptimos
%que minimicen funciones cuadr?ticas con matrices hessianas s.p.d.
%
%
%Los par?metros de la funci?n son:
%   H := matriz (hessiana) s.p.d.
%   g := vector (aleatorio generalmente) que representa al gradiente de la
%funci?n a minimizar.
%   x0 := vector (aleatorio generalmente), semilla o aproximaci?n inicial. 
%   max_iter := N?mero m?ximo de iteraciones a realizar antes de que el
%programa pare (si no se ha llegado a un punto minimizador.
%   TOL := Valor que ayuda a decidir cu?ndo se est? lo suficientemente
%cerca del valor buscado como para dejar de buscar.
%
%Se regresan:
%   M := Matriz con los valores de las iteraciones de x 
%   k := N?mero de iteraciones necesitadas para alcanzar TOL.


fprintf(' \n  k                f_k              ||gradf_k||          alfa_k         \n\n ');
      
k = 0;
x = x0;
gradf_k = H* x + g;
normf = norm( gradf_k );
M(:,1) = x0;

fprintf('\n');

while ( normf >= TOL && k <= max_iter )

    k = k + 1 ;    
    alfa_k =  (gradf_k'* gradf_k) / (gradf_k'* H* gradf_k);
    x = x - alfa_k* gradf_k ;
    gradf_k = H* x + g ;
    normf = norm( gradf_k ) ;
    f_k = (0.5) * x'* H* x + g'* x;
    
    M(:,k+1) = x;
    i = 1 + k/1000;  
    % i es un contador que va creciendo de manera linear con las
    % iteraciones, se usa en el siguiente if para imprimir solamente las
    % iteraciones que muestren un cambio significativo (en descenso de f).
        
    if(k == 1 || norm( f_k_p - f_k )/norm( f_k_p ) > 10^(-i) )  
        % Error relativo entre el valor de f en la iteraci?n k y el ?ltimo valor impreso
        % se imprime s?lo cuando el ?ltimo valor impreso y el actual valor
        % de f han cambiado en 10^(-i) relativamente.
        
        fprintf(1, '%4i        %14.8f         %14.8e      %14.8e       \n', k, f_k, normf, alfa_k);
        f_k_p = f_k;

    end
end

if(k == 0)
    f_k = (0.5) * x'* H* x + g'* x;
    normf = norm( H* x + g );
    fprintf(1,' 0           %14.8f          %14.8e          NA      \n\n', f_k, normf );
    fprint(' Problema resuelto con solucion inicial \n');
end
if(k >= max_iter)
    fprintf('\n\n \t EL METODO NO CONVERGE\n\n');
else
    fprintf(' \n k_final         f_final            ||gradf||               \n\n ');
    fprintf(1, '%4i       %14.8f         %14.8e            \n \n X_final :  \n\n', k, f_k, normf);
    fprintf(1, '\t %14.6e \n', x);
end
end
 