__kernel void brahmaKernel (__global int * buf)
{while (((buf [0] < 5) && ((buf [1] < 6) || (buf [2] > 2))))
 {buf [0] = (2 + buf [0]) ;} ;}
