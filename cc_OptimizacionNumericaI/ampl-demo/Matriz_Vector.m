function [ r ] = Matriz_Vector(m,H0,q,S,Y,Rho,k) 
% Calcula producto matriz vector con doble iteracion

base = [1:m]';

if k<(m+1)
    %arr1 = [k-1;arr1];
    arr2 = 1:(k-1);
    arr1 = flip(arr2);
else
    clear arr1 arr2
    arr2 = circshift(base,m-mod(k-1,m));
    arr1 = flip(arr2);
end

lg = length(arr1);
alpha = nan(lg,1);

for i = 1:lg
    ind1 = arr1(i);
    alpha(ind1) = Rho(ind1)*(S(:,ind1)'*q); 
    q = q - alpha(ind1)*Y(:,ind1);

end

r = H0*q;

for i = 1:lg   

    ind2 = arr2(i);
    beta = Rho(ind2)*(Y(:,ind2)'*r); %Y(i)
    r = r + S(:,ind2)*(alpha(ind2)-beta);
        
end

%Haux2 = Rho(1)*(S(:,1)*S(:,1)') + V'*H0*V;

%paux = Haux*q;
%paux2 = Haux2*q;

%B = [r,paux,paux2];


