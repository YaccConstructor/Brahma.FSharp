int g (__global int * m, private int k)
{return ((k + m [0]) + m [1]) ;}
int xUnitFunc (__global int * m, private int n)
{return ((5 - n) + g (m, 4)) ;}
int z (private int a, __global int * m, private int t)
{return ((m [2] + a) - t) ;}
int y (private int l, __global int * m, private int n, private int a)
{int x = xUnitFunc (m, n) ;
 return z (a, m, ((a + x) + l)) ;}
int rUnitFunc (private int l, __global int * m, private int n)
{return y (l, m, n, 6) ;}
int x1 (__global int * m, private int n)
{int l = m [9] ;
 int r = rUnitFunc (l, m, n) ;
 return (r + m [3]) ;}
__kernel void brahmaKernel (__global int * m)
{int p = m [0] ;
 m [0] = x1 (m, 7) ;}
