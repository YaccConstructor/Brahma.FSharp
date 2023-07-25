int x (private int a1)
{return (a1 + 1) ;}
int s1UnitFunc (private int * s2Ref)
{return x (* (s2Ref)) ;}
__kernel void brahmaKernel (__global int * a)
{int s = 1 ;
 int s2 = 2 ;
 __private int * s2Ref = & s2 ;
 int s1 = s1UnitFunc (s2Ref) ;
 a [0] = s1 ;}
