int g (private int x3, private int m)
{return (m + x3) ;}
int f (private int x, private int y)
{int y1 = y ;
 int y2 = y1 ;
 return g (x, y2) ;}
int ItemUnitFunc ()
{return f (1, 7) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;}
