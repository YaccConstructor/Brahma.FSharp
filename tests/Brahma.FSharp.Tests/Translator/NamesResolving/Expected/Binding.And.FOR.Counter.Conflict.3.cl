__kernel void brahmaKernel (__global int * buf)
{for (int i = 0 ; (i <= 1) ; i ++)
 {int i1 = (i + 2) ;
  buf [i1] = 2 ;} ;}
