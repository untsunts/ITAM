function [ x ] = Broyden( fun  , x0, maxiter)
%Resolucion de S.E. no lineales sin recurrir a la Jacobiana
    
    iter = 0;

    TOL = 1.0e-8;
    %maxiterGC = 1000;

    [f,J,~] = feval(fun,x0);
    B = J;
    x=x0;

    nf = norm(f);
    %conv = norm(B-J);

    fprintf(' iter         ||f||             ||Bk - Jk||        ||pB - pN||       \n');
 

    while(nf > TOL && iter < maxiter)

        %[pB] = GC(B,-f,TOL,maxiterGC);
        %[pN] = GC(H,-f,TOL,maxiterGC);
       
        pB = B\-f;
        pN = J\-f;
        
        xaux = x;
        faux = f;

        x = x + pB;
        [f, J, ~] = feval(fun,x);
        
        y = f - faux;
        s = x - xaux;

        B = B + ((y-B*s)*s')/(s'*s);

        nf = norm(f);
        conv = norm(B-J);
        conv2 = norm(pB - pN);

        iter = iter + 1;

        fprintf(' %3.0i       %10.6e        %10.6e        %10.6e\n' ,iter, nf,conv,conv2);

    end

end

