typedef struct du0 {int tag ;
                    union du0_Data {struct Case2Type0 {int Item ;} Case2 ;
                                    struct Case3Type0 {int Item1 ;
                                                       int Item2 ;} Case3 ;} data ;} du0 ;
__kernel void brahmaKernel ()
{du0 t = (du0) { 0 } ;
 int x = 5 ;
 if (((t) . tag == 1))
 {x = 6 ;}
 else
 {if (((t) . tag == 2))
  {x = 7 ;}
  else
  {x = 5 ;} ;} ;}
