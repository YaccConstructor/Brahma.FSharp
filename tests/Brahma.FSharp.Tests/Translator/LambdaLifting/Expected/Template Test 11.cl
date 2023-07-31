int r (private int p, private int l)
{return (l + p) ;}
int mUnitFunc (private int p)
{return r (p, 9) ;}
int z (private int k)
{return (k + 1) ;}
__kernel void brahmaKernel (__global int * buf)
{int p = 1 ;
 int m = mUnitFunc (p) ;
 buf [0] = m ;}
