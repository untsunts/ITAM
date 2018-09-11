# AMPL Model by ...
#
#
# Permission to use, copy, modify, and distribute this software and
# its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies and that the copyright notice and this
# permission notice appear in all supporting documentation.                     

#   Source: problem from test

#   The problem is not convex.

var x{1..2};

minimize f:
	2*x[1]+x[2];

subject to cons1:
	(1-x[1])^3 + x[2] = 0;

let x[1] :=2;
let x[2] :=1;
 
display f;
display x;