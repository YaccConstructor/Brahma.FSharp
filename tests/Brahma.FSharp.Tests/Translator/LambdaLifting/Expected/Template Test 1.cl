int fUnitFunc ()
{int x = 3 ;
 return x ;}
__kernel void brahmaKernel (__global int * buf)
{int f = fUnitFunc () ;
 buf [0] = f ;}
