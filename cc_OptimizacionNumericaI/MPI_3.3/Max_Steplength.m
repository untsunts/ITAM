function [alpha_x] = Max_Steplength(x, dx)
    
    index_x = find(dx<0);
    alpha_x = min(-x(index_x)./dx(index_x));
    
    if isempty(alpha_x) 
        alpha_x = 1;
    end

end

