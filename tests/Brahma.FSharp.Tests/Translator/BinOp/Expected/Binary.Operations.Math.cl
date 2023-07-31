__kernel void brahmaKernel (__global int * buf)
{int x = 0 ;
 int y = (x + 1) ;
 int z = (y * 2) ;
 int a = (z - x) ;
 int i = (a / 2) ;
 buf [0] = i ;}