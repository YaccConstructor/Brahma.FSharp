int Argi (__global int * buf, private int index)
{if ((index == 0))
 {return buf [1] ;}
 else
 {return buf [2] ;} ;}
int f (__global int * buf, private int y)
{return Argi (buf, y) ;}
int ItemUnitFunc (__global int * buf)
{return f (buf, 0) ;}
__kernel void brahmaKernel (__global int * buf)
{buf [0] = ItemUnitFunc (buf) ;}
