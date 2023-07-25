__kernel void brahmaKernel (__global int * xs)
{int i = 0 ;
 while ((i < 10))
 {xs [0] = (i * 2) ;
  printf ("i = %d, xs.[0]*10 = %d\n", i, (xs [0] + 10)) ;
  i = (i + 1) ;} ;}
