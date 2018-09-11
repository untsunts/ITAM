function [ F, J, H ] = F( x )
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

    F = [x(1)^2 + x(2)^2 - 2; exp(x(1)-1)+x(2)^3 - 2];
    J = [2*x(1),2*x(2);exp(x(1)-1),3*x(2)^2];
    H = [2,2;exp(x(1)-1),6*x(2)];
    
end

