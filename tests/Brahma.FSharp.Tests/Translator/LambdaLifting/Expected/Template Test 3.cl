int f1UnitFunc ()
{int f = 5 ;
 return f ;}
__kernel void brahmaKernel (__global int * buf)
{int f1 = f1UnitFunc () ;
 buf [0] = f1 ;}
