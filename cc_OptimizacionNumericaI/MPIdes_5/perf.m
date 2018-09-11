function [ ] = perf( T ,logplot)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

if (nargin< 2) logplot = 0; end
colors  = ['m';'b';'r';'g';'c';'k';'y'];
[np,ns] = size(T);
minperf = min(T,[],2);
r = zeros(np,ns);
for p = 1: np
r(p,:) = T(p,:)/minperf(p);
end
if (logplot) r = log2(r); end
max_ratio = max(max(r));
r(find(isnan(r))) = 2*max_ratio;
r = sort(r);
clf;
for s = 1: ns
[xs,ys] = stairs(r(:,s),[1:np]/np); 
option = ['-' colors(s) 'x' ];
plot(xs,ys,option,'MarkerSize',3); 
hold on;
end
axis([ 0.1 1.1*max_ratio 0 1 ]);

end

