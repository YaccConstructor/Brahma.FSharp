__kernel void brahmaKernel (__global int * buf)
{for (int i = 0 ; (i <= 3) ; i ++)
 {int x = (i * i) ;
  buf [0] = x ;} ;}