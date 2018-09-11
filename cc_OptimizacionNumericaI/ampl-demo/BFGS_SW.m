
function[x, iter, ng] = BFGS_SW(nombre,maxiter)

    path(path,'/Users/Sofia/Documents/MATLAB/Analisis Aplicado/ampl-demo/bin2');
    %
    nombre_ampl = strcat(nombre, '.nl');

    [ x0, ~, ~, ~ , ~, ~] = spamfunc( nombre_ampl );
    x = x0;

    iter = 0;

    n = size(x0,1);

    TOL = 1.0e-5;

    c1 = 1.0e-4;
    c2 = 0.9;

    prtlevel = 2;


    [ f, ~ ] = spamfunc( x, 0 );                  % evaluar la funcion objetivo
    [ g, ~ ] = spamfunc( x, 1 );                  % evaluar el gradiente 
    
    H = eye(n);
    
    ng = norm(g);

    fprintf(' iter                 f                 normg            curvatura \n');

    
    while(ng > TOL && iter < maxiter)

        p = - H*g;

        xaux = x;
        gaux = g;

        [ ~, x, f, g, ~ , ~ ] = linesch_sw(x, f, g, p, nombre, c1, c2, prtlevel);

        y = g - gaux;
        s = x - xaux;

        curv = s'*y;
        rho = 1/curv;
        V = eye(n) - rho*y*s';

        H = V'*H*V + rho*(s*s');


        ng = norm(g);   

        iter = iter + 1;

        fprintf(' %3.0i        %14.6e        %14.6e        %14.6e        \n', iter, f, ng,curv);

    end
    
end
