int xUnitFunc ()
{int y = 3 ;
 return y ;}
int fUnitFunc ()
{int x = xUnitFunc () ;
 return x ;}
__kernel void brahmaKernel (__global int * buf)
{int f = fUnitFunc () ;
 buf [0] = f ;}
