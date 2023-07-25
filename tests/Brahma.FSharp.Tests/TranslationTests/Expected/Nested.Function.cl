int f (private int x, private int y)
{return (x - y) ;}
int ItemUnitFunc ()
{return f (2, 3) ;}
int ItemUnitFunc2 ()
{return f (4, 5) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;
 buf [1] = ItemUnitFunc2 () ;}
