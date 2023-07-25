typedef struct du0 {int tag ;
                    union du0_Data {struct SimpleTwoType0 {int Item ;} SimpleTwo ;} data ;} du0 ;
typedef struct du1 {int tag ;
                    union du1_Data {struct OuterType0 {int Item ;} Outer ;
                                    struct InnerType0 {du0 Item ;} Inner ;} data ;} du1 ;
__kernel void brahmaKernel ()
{du1 x = (du1) { 1, { .Inner = { (du0) { 1, { .SimpleTwo = { 29 } } } } } } ;
 int y = 5 ;
 y = 7 ;}
