__kernel void brahmaKernel (__global int * buf)
{int x = 1 ;
 int y = (x + 1) ;
 buf [0] = y ;}