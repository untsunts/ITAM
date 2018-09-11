% Guillermo Santiago Novoa P?rez
%%
function  GC_pcg(nombre, maxiter, tol, n)

salida = strcat(nombre, '1.out');      % creamos un archivo de texto en el que vamos a imprimir los resultados (usando perl)
fout = fopen(salida, 'w');


%% documentaci?n

    fprintf( fout, '\n Guillermo Santiago Novoa P?rez' );
    fprintf( fout, '\n Nombre del problema                      %s  \n', nombre);
    fprintf( fout, ' Numero maximo de iteraciones             %4i \n', maxiter);
    fprintf( fout, ' Tolerancia                               %8.2e \n\n',tol); 
    
            fprintf(fout,'\n\n                                  GC                      pcg    \n');
            fprintf(fout, '\n|     n    |  nombre  |    tiempo       iter   |    tiempo       iter   |\n');
            fprintf(fout,'---------------------------------------------------------------------------\n')
    %%  gallery (iniciaci?n)
        if( nombre(1) =='p' )
            m = floor(sqrt(n))
            n = m^2;
            A = gallery(nombre,m);
        else
            A = gallery(nombre,n);     
        end
        x_fin = ones(n,1);
        b = A * x_fin;
        nb =norm(b);
        x0 = zeros(n,1);
    %   iid = eye(n);       
    %%  m?todos
        % GC
        tic
            [xgc,kgc] = GC(A,b,x0,tol,maxiter);
        tgc = toc;
        % pcg
        tic
            [xpcg,fl0,rr0,kpcg] = pcg(A,b,tol,maxiter);
        tpcg = toc;
    %% resultados
       fprintf(fout,'|  %6i  |  %6s  | %11.8e  %4i   | %11.8e  %4i   |\n', n, nombre,tgc,kgc,tpcg,kpcg);
    %
end