int ItemUnitFunc ()
{return (2 - 3) ;}
int ItemUnitFunc2 ()
{return (4 - 5) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc () ;
 buf [1] = ItemUnitFunc2 () ;}
