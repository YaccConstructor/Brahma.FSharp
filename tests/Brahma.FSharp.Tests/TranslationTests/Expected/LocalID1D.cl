__kernel void brahmaKernel (__global int * buf)
{int id = get_local_id (0) ;
 buf [id] = 0 ;}