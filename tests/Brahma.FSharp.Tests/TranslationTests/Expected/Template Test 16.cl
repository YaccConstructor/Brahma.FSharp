int z (private int a)
{return (a + 1) ;}
int f (__global int * buf, private int y)
{if ((y == 0))
 {return z (9) ;}
 else
 {return buf [2] ;} ;}
int ItemUnitFunc (__global int * buf)
{return f (buf, 0) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc (buf) ;}
