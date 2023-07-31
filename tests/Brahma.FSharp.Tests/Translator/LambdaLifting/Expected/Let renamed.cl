int f (private int x)
{int g = (1 + x) ;
 return g ;}
int ItemUnitFunc ()
{return f (1) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;}
