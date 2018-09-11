function[x] =  LDL_solver(L,D,P,S,b);


x = S*b;
x = P'*x;
x = L\x;
x = D\x;
x = L'\x;
x = P*x;
x = S*x;

end