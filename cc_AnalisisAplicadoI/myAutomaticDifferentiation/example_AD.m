function example_AD (func, tol, xo , maxiter);
%
% Example for using the classes myAD and myA2D
% Note that these classes only provide vectors and not matrices
%
% myAD and myA2D - Automatic Differentiation of 1st and 2nd Derivative
% Copyright (C) 2006 Martin Fink (martinfink "at" gmx.at)
%
% ... minor changes 
%--------------------------------------------------------------------------

% ... evaluar el gradiente en el punto inicial

n = length(x0);
f = feval(func, x0);

pAD   = myAD(x0);
outAD = func(pAD);

grad  = getderivs(outAD)
grad  = grad';

pA2D   = myA2D(x0);
outA2D = func(pA2D);
d2fA2D = getsecderiv(outA2D);
hess = sparse(n,n);
hess(1:n,:) = d2fA2D(:,:,1:n);


    for j=1:30
    n = 2;
    p = ones(n,1);
    f = feval(func, p);
    %
    % Use automatic differentiation when only first order derivative is needed
    %
    pAD   = myAD(p);
    outAD = func( pAD);
    
    % Use automatic differentiation when first and second order derivative is needed
    pA2D   = myA2D(p);
    outA2D = func(pA2D);
    
    disp('Function value');
    
    fx = getvalue(outAD)
    
    disp('First derivatives:');
    
    dfAD  = getderivs(outAD)
    dfAD  = dfAD';
    
    % Second derivative of function values with respect to parameters
    
    disp('Second derivatives:');
    
    d2fA2D = getsecderiv(outA2D);
    
    hess = sparse(n,n);
    hess(1:n,:) = d2fA2D(:,:,1:n);
end
hess
%spy(hess)

