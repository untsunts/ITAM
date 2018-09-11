%
function IPM =  ipm_on ( point, fungrad, name, factor  , maxbar ,...
                         mu0  , TOL , separate );
%--------------------------------------------------------------------------
%   
% IPM        An Interior Point Method 
%            developed by the OTC at NU team.
%            July, 2002.
%     
%            Features:   
%
%             * AMPL interface 
%             * NO fixed variables allowed
%             * free variables allowed
%                
%--------------------------------------------------------------------------
path(path,'/Users/jmorales/bin'); 
%
% ... open output file
%
name1 = strcat(name,'.out1'); fout1 = fopen(name1,'w');   
%
% ... get initial point from stub; compute initial values: 
%     obj function and derivatives. 
%
stub  = strcat(name,'.nl');
[ x, s, y ] = feval ( point );
% 
n_x = length(x);        % ... number of variables
n_s = length(s);        % ... number of slacks
s   = zeros(n_s,1);     %
%
[ f, c, g, A, ~ ] = feval (fungrad, x, s, y );
%
m   = length(c);        % ... number of c/s (incl. bnds)
m_e = m - n_s;          % ... number of equality c/s

thresh  =  1.0d-1;      % ... minimum value for the initial slacks
%
s  = Initials ( c, m_e, n_s, thresh );
e  = ones(n_s, 1);     % ... all ones vector
%
% ... set initial barrier, Lagrange multipliers, constraints
%     compute W with LS Lagrange multipliers
%
mu = mu0;

infeas  = norm(c,2);  
bar  = f  - mu*log( s )'*e;

%[ y ]  = Lagrange( A, n_x, n_s, m, m_e, mu, g, s );

[ f, c, g, A, W ] = feval (fungrad, x, s, y );
%
% ... print characteristics of the problem 
%
Print_head ( fout1, name  , n_x, m  , n_s   , m_e    , factor,...
                 mu0  , maxbar, f  , bar, infeas, separate );
%
% -------------------------------------------------------------------------
%
function s = Initials ( c, n_e, n_s, thresh );
zero = 0.0d0;
%
% ... This subroutine computes initial values for the slacks 
%     associated with inequality constraints c(x) <= 0 
%
%     if  c(x0) <=0   AND   c(x0) < thresh   
%         s0 =  c(x0)
%     else 
%         s0 =  thresh
%     end 
%
s = zeros(n_s,1);
for i=1:n_s
    j  = n_e + i;
    cj = c(j);
    if cj <= zero & cj < -thresh
        s(i) = -cj;
    else
        s(i) = thresh;
    end
end     
%
% -------------------------------------------------------------------------
%
function print = Print_head ( fout , name  , n_x   , m       , n_s, ...
                              m_e  , factor, mu0   , maxbar, ...
                              f    , bar   , infeas, separate );
%
% ... this routine prints initial values                          
%
if separate == 0
   SEP = 'Equal   ';
else
   SEP = 'Separate';
end
%
fprintf(fout,'\n An Interior Point Method solving problem: %s \n', name);
fprintf(fout,' Problem dimensions: n_x = %4i;\tm   = %4i            \n', n_x, m);
fprintf(fout,'                     n_s = %4i;\tm_e = %4i            \n', n_s, m_e);
fprintf(fout,'                                                      \n');
fprintf(fout,' Parameters:         factor ................. %7.1e   \n', factor);
fprintf(fout,'                     mu0 .................... %7.1e   \n', mu0);
fprintf(fout,'                     maxbar ................. %4i     \n', maxbar);
fprintf(fout,'                     Step lengths ........... %s      \n', SEP );
fprintf(fout,'                                                     \n');
fprintf(fout,' Objective   ............... % 14.8e   \n', f);
fprintf(fout,' Barrier     ............... % 14.8e   \n', bar);
fprintf(fout,' ||c||_2     ............... % 14.8e   \n', infeas);
%
%--------------------------------------------------------------------------
%
