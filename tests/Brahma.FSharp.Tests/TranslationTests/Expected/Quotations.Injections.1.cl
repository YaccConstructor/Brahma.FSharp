int ItemUnitFunc ()
{return (2 * 2) ;}
int ItemUnitFunc2 ()
{return (4 * 4) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;
 buf [1] = ItemUnitFunc2 () ;}
