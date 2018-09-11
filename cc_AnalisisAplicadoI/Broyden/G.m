function [ G, J, H ] = G( x )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

    G = [x(1) + x(2) - 3; x(1)^2 + x(2)^2 - 9];
    J = [1,1;2*x(1),2*x(2)];
    H = [0,0;2,2];

end

