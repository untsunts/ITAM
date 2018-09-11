function[A] =  GenMat(n,ncond)
% generador de matrices de dimensi?n n con n?mero de condici?n ncond.

    ncond = sqrt(ncond);
    randn('state',1);
    R = randn(n);
    [U,L,V] = svd(R);

     D(1,1) = 1.0d0;
     for i=2:n
         D(i,i) = ncond^( -1/(n-i+1));
     end
     A = U*D*U';    
     condA = cond(A);
     A = A'*A;
     x = randn(n,1);  b = A*x;
     
     x_G = A\b;
     rG = norm( b - A*x_G)/ (norm(A)*norm(x_G));
     
     x_i = inv(A)*b;
     r_i = norm( b-A*x_i )/(norm(A)*norm(x_i));
 
end