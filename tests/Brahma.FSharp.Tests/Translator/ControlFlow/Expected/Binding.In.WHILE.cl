__kernel void brahmaKernel (__global int * buf)
{while ((buf [0] < 5))
 {int x = (buf [0] + 1) ;
  buf [0] = (x * x) ;} ;}