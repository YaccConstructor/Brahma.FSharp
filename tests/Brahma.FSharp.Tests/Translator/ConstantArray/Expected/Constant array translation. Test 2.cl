__constant int cArray1 [ 3 ] = { 1, 2, 3} ;
__kernel void brahmaKernel (__global int * buf)
{buf [0] = (1 + cArray1 [1]) ;}
