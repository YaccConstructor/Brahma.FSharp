__kernel void brahmaKernel (__global int * buf)
{for (int i = 1 ; (i <= 3) ; i ++)
 {buf [0] = i ;} ;}