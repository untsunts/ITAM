function PCS_global ( nombre, itermax, tol );
%
% directorio en donde reside spamfunc.mexmaci64
path(path,'/usr/local/bin');   %spamfunc.mexmaci64 fue cambiado al folder que contiene todas las funciones.

%--------------------------------------------------------------------------
%
nombreAMPL = strcat(nombre, '.nl');    %nombre del archivo del que se lee el modelo en AMPL
salida = strcat(nombre, '.out');      % creamos un archivo de texto en el que vamos a imprimir los resultados (usando perl)
fout = fopen(salida, 'w');
fout2   = fopen('resultados.out','a');
%fout2   = fopen('resultados2.out','a');
    
if strcmp(nombre,'bt1')
    fprintf(fout2,'\n   problema       n       m         f         iter   feval      CPU(s)     inercia  \n');
    fprintf(fout2,'---------------------------------------------------------------------------------------------\n');
end
    

tic   %medir tiempo
ro = 1/10;
c1 = 10e-5;
iter = 0;
iterf = 0;

[ x0, xlow, xupp, lm_0, clow, cupp ] = spamfunc(nombreAMPL);  %define variables iniciales

n = length(x0);      % numero de variables
m = length(lm_0);    % numero de restricciones
% 
% ... evaluar el objetivo y las restricciones en el punto inicial
%
[ f0, c0 ] = spamfunc ( x0, 0 );
c0 = c0 - clow;    % ajuste para obtener las restricciones adecuadamente
%
% ... evaluar el gradiente de f y la Jacobiana de c en el punto inicial
%
[ g0, A0 ] = spamfunc ( x0, 1 );
%
%lm = lambda_cero(g0,A0);       % obtener una lambda inicial para calcular la matriz KKT en alg?n lugar favorable
lm = A0'\g0;
%
% ... evaluar el gradiente de la Lagrangiana
%
gL = g0 - A0'*lm;
%
% ... evaluar la Hessiana de la Lagrangiana en el punto inicial
%
%[ W0 ] = spamfunc ( -lm_0 );

fprintf( fout, ' Nombre del problema                      %s  \n', nombre);
fprintf( fout, ' Numero de variables                    %4i \n', n);
fprintf( fout, ' Numero de restricciones                %4i \n', m);
fprintf( fout, ' Numero maximo de iteraciones            %4i \n', itermax);
fprintf( fout, ' Tolerancia                               %8.2e \n\n', tol);   
fprintf( fout, ' Objetivo en el punto inicial            % 21.15e \n', f0);
fprintf( fout, ' Norma de las restricciones en el punto inicial             % 8.2e \n', norm(c0, inf));
fprintf( fout, ' Norma del gradiente de la Lagrangiana en el punto inicial    %8.2e \n', norm(gL) );

[ W0 ] = spamfunc ( -lm );      
 
mu0 = norm(lm,Inf);% c?lculo del parametro de la primera penalizacion 

mu = mu0;
iterf = iterf+1;

K = [W0, A0'; A0, zeros(m,m)];    % matriz de KKT

[L,D,P,S,neg,ran] = ldl(K);    

      normaL  = norm(gL,Inf);
      normaLinf  = norm(gL,Inf);
      normaR  = norm(c0,Inf);
      normaRinf  = norm(c0,Inf);

    if (ran == (n+m) && neg == m)     %checar si la inercia es correcta para iniciar nuestro m?todo
      inercia_correcta = true;
    else
        inercia_correcta = false;
        fprintf(fout,'\n inercia incorrecta  \n');
        fprintf(fout2,' %10s   %5i   %5i   %7.4e   %5i %5i     %7.4e  %3i\n', nombre, n, m, f0, iter, iterf, toc, inercia_correcta );
        return
    end
    
    if (iter == 0)
        fprintf(fout,'\n   k         f             ||c||      ||gL||     alpha        mu     \n');
        fprintf(fout,'-----------------------------------------------------------------------\n');
    end
    
    
    while(iter <= itermax && (normaL > tol*(1+normaLinf) || normaR > tol*(1+normaRinf)) && inercia_correcta)
    % no nos hemos pasado de iteraciones, no hemos llegado a un punto lo suficientemente cercano a la soluci?n y la inercia sigue siendo correcta    

        
          GyC  = [-g0;-c0];   % vector del lado derecho de la ecuaci?n de KKT
          iter = iter + 1;
          h    = LDL_solver(L,D,P,S,GyC);
          hx   = h(1:n);
          %x_aux   = x0+hx;       %actualizaciones de x y lambda (no es
          %necesario todav?a dado que falta aceptar la alfa)
          diflm = -h(n+1:n+m)-lm;
          
          
          %actualizaci?n de miu
          
          quad = hx'*W0*hx;
          normaC = norm(c0,Inf);
          sigma = (quad>0);
          mu1 = ((g0'*hx+(sigma*0.5)*quad)/((1-ro)*normaC)) + 10e-5;
          mu = max(mu,mu1);
          
          %c?lculo de la alfa para garantizar descenso suficiente
          j = 0;
          alfa = 1;
          aux = false;
          while (aux == false && j<=20)
              j = j + 1;
              %funci?n de m?rito
              F = f0 + mu*normaC;
              %x nueva a prueba
              x_aux = x0 + alfa*hx;
              %[ f0, c0 ] = spamfunc ( x0, 0 );
              [ f_aux, c_aux ] = spamfunc ( x_aux, 0 );
              c_aux = c_aux - clow;
              iterf = iterf + 1;
              normaCaux = norm(c_aux,Inf);
              %funci?n de m?rito en la x de prueba
              Faux = f_aux + mu*normaCaux;
              %c?lculo de la derivada direccional
              deltaF = g0'*hx - mu*normaC;
              %ver si el paso es aceptable
              aux = (Faux <= F + c1*alfa*deltaF);
              if(aux == false)
                  alfa = alfa*0.5;
              end
          end
          
          if(j > 20)
              fprintf(fout,'\n Problema mal escalado \n');
              fprintf(fout2,' %10s ,    problema mal escalado ,     iter = %3i \n', nombre, iter);
              return
          end
          
          x0 = x0 + alfa*hx;
          lm = lm + alfa*diflm;
%
%         Volver a calcular todos los par?metros en la nueva x y lambda
%          
          f0 = f_aux;
          c0 = c_aux;
%            
          [ g0, A0 ] = spamfunc ( x0, 1 );
%          
          gL = g0 - A0'*lm;
%
          [ W0 ] = spamfunc ( -lm );
%          
          
          K = [W0, A0'; A0, zeros(m,m)];
          
          [L,D,P,S,neg,ran] = ldl(K);
%
%
%
          normaL  = norm(gL,Inf);
          normaR  = norm(c0,Inf);
          
          fprintf(fout,'  %3i    %15.8e    %9.2e     %9.2e  %9.2e  %9.2e  \n', iter, f0, normaR, normaL, alfa, mu);

          %  
          if (ran == (n+m) && neg == m)    %checar si la inercia sigue siendo correcta        
              inercia_correcta = true;
          else
              inercia_correcta = false;
              fprintf(fout,'\n inercia incorrecta');
          end
          %
        
    end
    
    t=toc;    
        fprintf(fout2,' %10s   %5i   %5i   %7.4e   %5i %5i     %7.4e  %3i\n', nombre, n, m, f0, iter, iterf, toc, inercia_correcta );
    if inercia_correcta
        fprintf(fout,'\n\n Minimizador local del modelo: \n');
        fprintf(fout,' \t  %15.8e \n',x0);         
    end

    fprintf(fout,'\n tiempo de programa :  %9.2e', t);
    fclose(fout);
    fclose(fout2);
end