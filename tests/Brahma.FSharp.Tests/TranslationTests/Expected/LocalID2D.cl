__kernel void brahmaKernel (__global int * buf)
{int v = get_local_id (0) ;
 int id = get_local_id (1) ;
 buf [id] = v ;}