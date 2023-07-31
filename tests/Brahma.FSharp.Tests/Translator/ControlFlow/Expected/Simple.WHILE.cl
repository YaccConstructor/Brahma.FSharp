__kernel void brahmaKernel (__global int * buf)
{while ((buf [0] < 5))
 {buf [0] = (buf [0] + 1) ;} ;}