function [ p, flag ] = CG_TR (  B, g, delta, TOL );
%
%--------------------------------------------------------------------------         
%
% ... This procedure approximately solves the following trust region
%     subproblem 
%   
%               minimize    1/2 p'Bp + g'p  
%                           ||p|| <= Delta                      
%
%     by means of the conjugate gradient method adapted to satisfy the
%     trust region constraint
%
%     INPUT
%
%         B    Hessian matrix of the model (symmetric)
%         g    gradient vector
%     delta    radius of the TR
%       TOL    tolerance to terminate the iteration based on the residual
%
%    OUTPUT
%         p    an approximate solution 
%      flag    termination condition 
%
%               JL Morales 
%               IEMS, Northwestern University
%               2014
%--------------------------------------------------------------------------         
%
n      = length(g);
maxit  = 2*n;
%
z = zeros(n,1);                  % ... initial solution 
r = g;
d = -r;
if norm(r) < TOL 
    flag = 'norm of the residual too small';
    p = z;
    return
end

rTr = r'*r;   

for i=1:maxit
    fprintf(' iter = %4i  ||p|| = %14.8e  ||r|| = %8.2e \n', i-1, norm(z), norm(r));
    
    Bd  = B*d;                   % perform matrix-vector multiplication
    dBd = d'*Bd;       
          
    if dBd <= 0                  % test for zero or negative curvature
        tau  = step ( d, z, delta );
%        
        flag = 'negative curvature';
        p    =  z + tau*d;
        r    =  r + tau*Bd;
        fprintf(' iter = %4i  ||p|| = %14.8e  ||r|| = %8.2e  %10s \n', i, norm(p), norm(r), flag);
        return
    else                          % is the iterate inside de TR?
        alpha = rTr/dBd;
        
        if norm(z + alpha*d) > delta
            tau  = step ( d, z, delta );
            flag = 'boundary was hit';
            p    =  z + tau*d;
            r    =  r + tau*Bd;
            fprintf(' iter = %4i  ||p|| = %14.8e  ||r|| = %8.2e  %10s \n', i, norm(p), norm(r), flag);
            return
        end
    end                           % update approximation and residual
    z  =  z + alpha*d;
    r  =  r + alpha*Bd;
    if norm(r) < TOL 
        flag = 'norm of the residual too small';
        p = z;
        fprintf(' iter = %4i  ||p|| = %14.8e  ||r|| = %8.2e  %10s \n', i, norm(p), norm(r), flag);
        return
    end
%                                  % compute a new direction
    rTr1 = r'*r;
    beta = rTr1/rTr;
    d    = -r + beta*d;
    rTr  = rTr1;
end
p = z;
flag = 'max number of iter';
fprintf(' iter = %4i  ||p|| = %14.8e  ||r|| = %8.2e  %10s \n', i, norm(p), norm(r), flag);
%
% ... solve the quadratic to compute the stepsize to the boundary
%
function [ tau ] = step ( d, z, delta );

a = d'*d; 
b = 2*(z'*d); 
c = z'*z - delta^2;

tau  = (-b + sqrt(b^2 - 4*a*c))/(2*a);













