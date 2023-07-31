int x (private int n)
{int r = 8 ;
 int h = (r + n) ;
 return h ;}
int ItemUnitFunc ()
{return x (9) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;}
