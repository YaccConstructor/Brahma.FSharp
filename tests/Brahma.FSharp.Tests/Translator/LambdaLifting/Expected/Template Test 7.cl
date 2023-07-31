int x (private int y1)
{return (6 - y1) ;}
int f (private int y)
{return x (y) ;}
int ItemUnitFunc ()
{return f (7) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;}
