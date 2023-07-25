int f1UnitFunc ()
{int f = 5 ;
 return f ;}
int f2UnitFunc ()
{int f1 = f1UnitFunc () ;
 return f1 ;}
__kernel void brahmaKernel (__global int * buf)
{int f2 = f2UnitFunc () ;
 buf [0] = f2 ;}
