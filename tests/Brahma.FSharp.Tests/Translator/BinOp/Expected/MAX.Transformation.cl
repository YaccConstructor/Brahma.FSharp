#pragma OPENCL EXTENSION cl_khr_fp64 : enable
__kernel void brahmaKernel (__global double * buf)
{double tempVarY = 1 ;
 buf [0] = max (buf [0], tempVarY) ;
 buf [0] = max (buf [0], tempVarY) ;}
