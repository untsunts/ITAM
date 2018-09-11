% Guillermo Santiago Novoa P?rez
%%
function[rvchol,rv0,rvichol,rv1,rvmichol,rv2]= GC_precon(nombre, maxiter, tol);

salida = strcat(nombre, '2.out');      % creamos un archivo de texto en el que vamos a imprimir los resultados (usando perl)
fout = fopen(salida, 'w');

%% documentaci?n

    fprintf( fout, '\n Guillermo Santiago Novoa P?rez' );
    fprintf( fout, ' Nombre del problema                      %s  \n', nombre);
    fprintf( fout, ' Numero maximo de iteraciones             %4i \n', maxiter);
    fprintf( fout, ' Tolerancia                               %8.2e \n\n',tol); 
    
%%
    for i=1:50
        if(i == 1)
            fprintf(fout,'\n\n\n                            GC                             Cholesky                                                    pcg                                                                          GCPre             \n');
            fprintf(fout, '\n|     n    |    tiempo(GC)       iter(GC)    |    tiempo(C)       iter(C)    |  tiempo_pcg   iter_pcg  | tiempo_pcg(iC)   iter_pcg(iC) | tiempo_pcg(miC) iter_pcg(miC) |  tiempo_GC(iC)   iter_GC(iC)  | tiempo_GC(miC)   iter_GC(miC) |     \n');
            fprintf(fout, '|----------|---------------------------------|-------------------------------|-------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------| \n');
        end
    %%  gallery (iniciaci?n)
        if( nombre(1) =='p' )
            m = i * 10;
            n = m^2;
            A = gallery(nombre,m);
        else
            n = i*200;
            A = gallery(nombre,n);     
        end
        x_fin = ones(n,1);
        b = A * x_fin;
        nb =norm(b);
        x0 = zeros(n,1);
%        iid = eye(n);
    %%  GC
        tic
            [xgc,kgc] = GC(A,b,x0,tol,2*maxiter);
        tgc = toc;
            rvgc(i,1) = norm(b-A*xgc)/nb;
        %
    %%  cholesky
        tic
            L = chol(A);
        tpchol = toc;
        % L'\b
        tic
            xc = L'\b;
            xc = L\xc;
        tschol = toc;
        tchol = tpchol + tschol;
            rvchol(i,1) = norm(b-A*xc)/nb;
            kc = 1;
        % pcg
        tic
            [xc_pcg,fl0,rr0,kc_pcg,rv0] = pcg(A,b,tol,maxiter);
        tschol_pcg = toc;
        tchol_pcg = tpchol + tschol_pcg;
    %%  incomplete cholesky
    A=sparse(A);
        tic
            iL = ichol(A);
        tpichol = toc;
        % GRPre
        tic
            [xic,kic] = GCPre(A,b,iL,x0,tol,maxiter);
        tsichol = toc;
        tichol = tpichol + tsichol;        
            rvichol(i,1) = norm(b-A*xic)/nb;
        % pcg
        tic
            [xic_pcg,fl1,rr1,kic_pcg,rv1] = pcg(A,b,tol,maxiter,iL,iL');
        tsichol_pcg = toc;
        tichol_pcg = tpichol + tsichol_pcg;
    %%  modified incomplete cholesky
        tic
            opts.michol = 'on';
            miL = ichol(A, opts);
        tpmichol = toc;
        % GRPre
        tic
            [xmic,kmic] = GCPre(A,b,miL,x0,tol,maxiter);
        tsmichol = toc;
        tmichol = tpmichol + tsmichol;
            rvmichol(i,1) = norm(b-A*xmic)/nb;
        % pcg
        tic
            [xmic_pcg,fl2,rr2,kmic_pcg,rv2] = pcg(A,b,tol,maxiter,miL,miL');
        tsmichol_pcg = toc;
        tmichol_pcg = tpmichol + tsmichol_pcg;
 
    %%  impresi?n de datos
        fprintf(fout,'|  %6i  |   %11.8e         %5i  |   %11.8e       %5i  |   %11.8e %5i  |   %11.8e       %5i  |   %11.8e       %5i  |   %11.8e       %5i  |   %11.8e       %5i  |\n', n,tgc,kgc,tchol,kc,tchol_pcg,kc_pcg,tichol_pcg,kic_pcg,tmichol_pcg,kmic_pcg,tichol,kic,tmichol,kmic);
    %    
    end
fprintf(fout,'\n\n');
figure;
semilogy(0:kc_pcg,rv0/nb,'b.');
hold on;
semilogy(0:kic_pcg,rv1/nb,'r.');
semilogy(0:kmic_pcg,rv2/nb,'k.');
legend('No Preconditioner','IC(0)','MIC(0)');
xlabel('iteration number');
ylabel('relative residual');
hold off;
end