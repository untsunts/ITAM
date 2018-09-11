
% Guillermo Santiago Novoa P?rez
% Gradiente Conjugado Proyectado
% 23 de octubre del 2015.
% 

function[]=GCproy_exp(nombre,n,maxiter,tol)

 
salida = strcat(nombre , '.out');      % creamos un archivo de texto en el que vamos a imprimir los resultados (usando perl)
%salida = strcat(nombre , '2.out');    % salida usando diferente condici?n
%de paro en GCPre 
fout = fopen(salida, 'w');

if( nombre(1) =='p' )
    m = floor(sqrt(n));
    n = m^2;
    G = gallery(nombre,m);
else
    m = floor(n/50)+1;
    G = gallery(nombre,n);
    G = sparse(G);
end

%% datos del problema
    fprintf( fout, '\n Guillermo Santiago Novoa P?rez' );
    fprintf( fout, '\n Nombre del problema                        %s  \n', nombre); 
    fprintf( fout, ' N?mero de variables                     %8i  \n', n);
    fprintf( fout, ' N?mero de restricciones               %8i \n', m);
    fprintf( fout, ' Numero maximo de iteraciones               %5i \n', maxiter);
    fprintf( fout, ' Tolerancia                                 %8.2e \n\n',tol); 
    
    
            fprintf(fout,'\n  --------------------------------------------------------------------------------------------------------- ');
            fprintf(fout,'\n  --------------------------------------------------------------------------------------------------------- ');
            fprintf(fout,'\n\t |    nombre     |   iter   |     m     ');               
            fprintf(fout,'|      |x-x*|/|x*|      ');
            fprintf(fout,'|      |x-x*|     ');
            fprintf(fout,'|      errel     | \n');
            fprintf(fout,'  ---------------------------------------------------------------------------------------------------------\n ');
            fprintf(fout,' ---------------------------------------------------------------------------------------------------------\n\n ');
 
            i=0;
%% algoritmo
for rang = 1:4
    if rang == 4
        m = n-m;
    else
        m = rang*m;
    end
    i=i+1;
    A = G(1:m,:);
    x_fin = ones(n+m,1);
    % nb =norm(b); 
    %
    % Checar dimensiones y ver qui?n es G realmente (qui?n deber?a serlo)
    %
    Z = 0*speye(m,m);
    K = [G,A';A,Z];
    b = K * x_fin;        
    %b = A * x_fin;        
    %x0 = zeros(n,1);   ahora vamos a usar otro x inicial.
    M = [speye(n,n), A';A, Z];
    
    %[L,D,P,S,neg,ran] = ldl(K); 
    [x,k,r] = GCPre(G,b,M,maxiter,tol);
    x_vec(:,i) = x;
    x_fin = x_fin(1:n);
    fin = norm(x-x_fin,1)/norm(x_fin,1);
    fin2 = norm(x-x_fin,1);
    fprintf(fout,'\t | %10s      |   %5i     |   %6i   ', nombre, k, m );
    fprintf(fout,'|   %8.5e   ',fin );
    fprintf(fout,'|   %8.5e   ',fin2 );
    fprintf(fout,'|   %8.5e   | \n',r );
    
            
    if( nombre(1) =='p' )
        m = floor(sqrt(n));
    else
        m = floor(n/50)+1;
    end

end
%% iteraci?n extra
    i=i+1;
    m = n-1;
    A = G(1:m,:);
    x_fin = ones(n+m,1);
    % nb =norm(b); 
    %
    % Checar dimensiones y ver qui?n es G realmente (qui?n deber?a serlo)
    %
    %g = 
    Z = speye(m,m)*0;
    K = [G,A';A,Z];
    b = K * x_fin;        
    %b = A * x_fin;        
    %x0 = zeros(n,1);
    M = [speye(n,n), A';A, Z];
    %r;
    %[L,D,P,S,neg,ran] = ldl(K); 
    [x,k,r] = GCPre(G,b,M,maxiter,tol);
    x_vec(:,i) = x;
    x_fin = x_fin(1:n);
    fin = norm(x-x_fin,1)/norm(x_fin,1);
    fin2 = norm(x-x_fin,1);
    fprintf(fout,'\t | %10s      |   %5i     |   %6i   ', nombre, k, m );
    fprintf(fout,'|   %8.5e   ',fin );
    fprintf(fout,'|   %8.5e   ',fin2 );    
    fprintf(fout,'|   %8.5e   | \n',r );
    
%%
            fprintf(fout,'\n  --------------------------------------------------------------------------------------------\n ');
            fprintf(fout,' --------------------------------------------------------------------------------------------\n\n ');
            fprintf(fout,'\t |         x          | \n');
            fprintf(fout,'\t |   %11.8e   | \n',x );
   
            
end
