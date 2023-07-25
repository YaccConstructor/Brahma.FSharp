__kernel void brahmaKernel (__global int * xs)
{int gid = get_global_id (0) ;
 int x = 10 ;
 printf ("%d %d", x, xs [gid]) ;}