% Guillermo Santiago Novoa P?rez
%%
function[rvchol,kc,rv0,it_cpcg,rvichol,it_ichol,rv1,it_icpcg,rvmichol,it_michol,rv2,it_micpcg]= GC_precon_tex(nombre, maxiter, tol);

salida = strcat(nombre, '_tex.out');      % creamos un archivo de texto en el que vamos a imprimir los resultados (usando perl)
fout = fopen(salida, 'w');

%% documentaci?n

    fprintf( fout, '\n Guillermo Santiago Novoa P?rez' );
    fprintf( fout, ' Nombre del problema                      %s  \n', nombre);
    fprintf( fout, ' Numero maximo de iteraciones             %4i \n', maxiter);
    fprintf( fout, ' Tolerancia                               %8.2e \n\n',tol); 
    
%%
    for i=1:50
        if(i == 1)
            %    $k$     & $f_k$  & $||\nabla f_k||$ & $\alpha_k$ \\
            fprintf(fout,'\n\n\n                            GC                             Cholesky                                                    pcg                                                                          GCPre             \n');
            fprintf(fout, '\n &     n    &    tiempo(GC)       iter(GC)    &    tiempo(C)       iter(C)    &  tiempo_pcg   iter_pcg  & tiempo_pcg(iC)   iter_pcg(iC) & tiempo_pcg(miC) iter_pcg(miC) &  tiempo_GC(iC)   iter_GC(iC)  & tiempo_GC(miC)   iter_GC(miC) &     \n');
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
            it_gc(i,1) = kgc;
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
            it_cpcg(i,1) = kc_pcg;
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
            it_ichol(i,1) = kic;
        % pcg
        tic
            [xic_pcg,fl1,rr1,kic_pcg,rv1] = pcg(A,b,tol,maxiter,iL,iL');
        tsichol_pcg = toc;
            it_icpcg(i,1) = kic_pcg;
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
            it_michol(i,1) = kmic;
            rvmichol(i,1) = norm(b-A*xmic)/nb;
        % pcg
        tic
            [xmic_pcg,fl2,rr2,kmic_pcg,rv2] = pcg(A,b,tol,maxiter,miL,miL');
        tsmichol_pcg = toc;
            it_micpcg(i,1) = kmic_pcg;
        tmichol_pcg = tpmichol + tsmichol_pcg;
 
    %%  impresi?n de datos
        fprintf(fout,'&  %6i  &   %11.8e         %5i  &   %11.8e       %5i  &   %11.8e %5i  &   %11.8e       %5i  &   %11.8e       %5i  &   %11.8e       %5i  &   %11.8e       %5i  & \\ \n', n,tgc,kgc,tchol,kc,tchol_pcg,kc_pcg,tichol_pcg,kic_pcg,tmichol_pcg,kmic_pcg,tichol,kic,tmichol,kmic);
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
fprintf(fout,'');
end