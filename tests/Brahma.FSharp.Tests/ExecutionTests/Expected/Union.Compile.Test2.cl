#pragma OPENCL EXTENSION cl_khr_fp64 : enable
typedef struct du0 {int tag ;
                    union du0_Data {struct AType0 {int Item1 ;
                                                   double Item2 ;} A ;
                                    struct BType0 {double Item ;} B ;} data ;} du0 ;
__kernel void brahmaKernel ()
{du0 x = (du0) { 1, { .B = { 5 } } } ;
 int y = 5 ;
 y = 7 ;}
