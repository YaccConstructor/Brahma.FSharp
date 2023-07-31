__kernel void brahmaKernel (__global int * buf)
{int gSizeX = get_global_size (0) ;
 int gSizeY = get_global_size (1) ;
 int gSizeZ = get_global_size (2) ;
 int lSizeX = get_local_size (0) ;
 int lSizeY = get_local_size (1) ;
 int lSizeZ = get_local_size (2) ;
  ;}
