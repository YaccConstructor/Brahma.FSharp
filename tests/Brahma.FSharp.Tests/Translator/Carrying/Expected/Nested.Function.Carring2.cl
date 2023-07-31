int f (private int x, private int y)
{int gg = 0 ;
 for (int i = 1 ; (i <= x) ; i ++)
 {gg = (gg + y) ;} ;
 return gg ;}
int g (private int x0)
{return f (2, x0) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = g (2) ;
 buf [1] = g (3) ;}
