typedef struct du0 {int tag ;
                    union du0_Data {struct Case2Type0 {int Item ;} Case2 ;
                                    struct Case3Type0 {int Item1 ;
                                                       int Item2 ;} Case3 ;} data ;} du0 ;
__kernel void brahmaKernel ()
{du0 t = (du0) { 0 } ;
 int m = 5 ;
 if (((t) . tag == 1))
 {int x = (((t) . data) . Case2) . Item ;
  m = x ;}
 else
 {if (((t) . tag == 2))
  {int z = (((t) . data) . Case3) . Item2 ;
   int y = (((t) . data) . Case3) . Item1 ;
   m = (y + z) ;}
  else
  {m = 5 ;} ;} ;}
