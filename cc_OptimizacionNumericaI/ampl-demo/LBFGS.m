function[x, j, ngvec] = LBFGS(nombre,maxiter)

    clc

    path(path,'/Users/Sofia/Documents/MATLAB/Analisis Aplicado/ampl-demo/bin2');
    %
    nombre_ampl = strcat(nombre, '.nl');

    [ x0, ~, ~, ~ , ~, ~] = spamfunc( nombre_ampl );
    x = x0;

    j = 0;

    n = size(x0,1);
    m = 10;

    TOL = 1.0e-5;

    c1 = 1.0e-4;
    c2 = 0.9;

    prtlevel = 2;
    
    S = zeros(n,m);
    Y = zeros(n,m);
    Rho = zeros(m,1);
    
    ngvec = nan(maxiter,1);

    [ f, ~ ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
    [ g, ~ ] = spamfunc( x, 1 );                  % evaluar el gradiente 
    
    ng = norm(g,Inf);
    
    tic
    
    if ng > TOL
        
        j = j + 1;

        fprintf(' iter                 f                 normg            curvatura \n');

        p = - g;

        xaux = x;
        gaux = g;

        [ ~, x, f, g, ~ , ~ ] = linesch_sw(x, f, g, p, nombre, c1, c2, prtlevel);

        y = g - gaux;
        s = x - xaux;

        S(:,j) = s;
        Y(:,j) = y;

        curv = s'*y;
        rho = 1/curv;

        Rho(j,1) = rho;

        ng = norm(g,Inf);   
        ngvec(j) =ng;

        fprintf(' %3.0i        %14.6e        %14.6e        %14.6e        \n', j, f, ng,curv);

    end
    
    while (ng > TOL && j < maxiter)
        
        j = j + 1;

        gamma = curv/(y'*y);
        %H0 = gamma*eye(n);
        H0 = gamma;
        
        [ p ] = Matriz_Vector(m,H0,g,S,Y,Rho,j); 
        
        p = -p;
        
        xaux = x;
        gaux = g;

        [ ~, x, f, g, ~ , ~ ] = linesch_sw(x, f, g, p, nombre, c1, c2, prtlevel);

        y = g - gaux;
        s = x - xaux;     
        
        jind = mod(j,m);
        
        if jind == 0
            jind = m;
        end

        S(:,jind) = s;
        Y(:,jind) = y;

        curv = s'*y;
        rho = 1/curv;

        Rho(jind,1) = rho;

        ng = norm(g,Inf);   
        ngvec(j) = ng;
        
        if mod(j,50)==0
            fprintf(' %3.0i        %14.6e        %14.6e        %14.6e        \n', j, f, ng,curv);
        end

    end
    t = toc;
    fprintf('Tiempo: %8.8f \n',t);
    
    ngvec = ngvec(1:j);
   
end
