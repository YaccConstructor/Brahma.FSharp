__kernel void brahmaKernel ()
{__local int xs [ 5 ] ;
 xs [get_local_id (0)] = get_local_id (0) ;}
