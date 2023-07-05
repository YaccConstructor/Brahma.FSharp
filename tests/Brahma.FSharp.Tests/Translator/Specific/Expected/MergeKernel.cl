__kernel void brahmaKernel
(private int firstSide, private int secondSide, private int sumOfSides,
 __global int * firstRowsBuffer, __global int * firstColumnsBuffer, __global int * firstValuesBuffer,
 __global int * secondRowsBuffer, __global int * secondColumnsBuffer,
 __global int * secondValuesBuffer, __global int * allRowsBuffer, __global int * allColumnsBuffer,
 __global int * allValuesBuffer)
{int i = get_global_id (0) ;
 __local int beginIdxLocal ;
 __local int endIdxLocal ;
 int localID = get_local_id (0) ;
 if ((localID < 2))
 {int x = (((localID * (256 - 1)) + i) - 1) ;
  if ((x >= sumOfSides))
  {x = (sumOfSides - 1) ;} ;
  int diagonalNumber = x ;
  int leftEdge = ((diagonalNumber + 1) - secondSide) ;
  if ((leftEdge < 0))
  {leftEdge = 0 ;} ;
  int rightEdge = (firstSide - 1) ;
  if ((rightEdge > diagonalNumber))
  {rightEdge = diagonalNumber ;} ;
  while ((leftEdge <= rightEdge))
  {int middleIdx = ((leftEdge + rightEdge) / 2) ;
   ulong firstIndex
   = (((ulong) firstRowsBuffer [middleIdx] << 32) | (ulong) firstColumnsBuffer [middleIdx]) ;
   ulong secondIndex
   =
   (((ulong) secondRowsBuffer [(diagonalNumber - middleIdx)] << 32) |
    (ulong) secondColumnsBuffer [(diagonalNumber - middleIdx)]) ;
   if ((firstIndex < secondIndex))
   {leftEdge = (middleIdx + 1) ;}
   else
   {rightEdge = (middleIdx - 1) ;} ;} ;
  if ((localID == 0))
  {beginIdxLocal = leftEdge ;}
  else
  {endIdxLocal = leftEdge ;} ;} ;
 barrier(CLK_LOCAL_MEM_FENCE) ;
 int beginIdx = beginIdxLocal ;
 int endIdx = endIdxLocal ;
 int firstLocalLength = (endIdx - beginIdx) ;
 int x43 = (256 - firstLocalLength) ;
 if ((endIdx == firstSide))
 {x43 = (((secondSide - i) + localID) + beginIdx) ;} ;
 int secondLocalLength = x43 ;
 __local ulong localIndices [ 256 ] ;
 if ((localID < firstLocalLength))
 {localIndices [localID] =
  (((ulong) firstRowsBuffer [(beginIdx + localID)] << 32) |
   (ulong) firstColumnsBuffer [(beginIdx + localID)]) ;} ;
 if ((localID < secondLocalLength))
 {localIndices [(firstLocalLength + localID)] =
  (((ulong) secondRowsBuffer [(i - beginIdx)] << 32) | (ulong) secondColumnsBuffer [(i - beginIdx)])
  ;} ;
 barrier(CLK_LOCAL_MEM_FENCE) ;
 if ((i < sumOfSides))
 {int leftEdge76 = ((localID + 1) - secondLocalLength) ;
  if ((leftEdge76 < 0))
  {leftEdge76 = 0 ;} ;
  int rightEdge79 = (firstLocalLength - 1) ;
  if ((rightEdge79 > localID))
  {rightEdge79 = localID ;} ;
  while ((leftEdge76 <= rightEdge79))
  {int middleIdx84 = ((leftEdge76 + rightEdge79) / 2) ;
   ulong firstIndex85 = localIndices [middleIdx84] ;
   ulong secondIndex90 = localIndices [((firstLocalLength + localID) - middleIdx84)] ;
   if ((firstIndex85 < secondIndex90))
   {leftEdge76 = (middleIdx84 + 1) ;}
   else
   {rightEdge79 = (middleIdx84 - 1) ;} ;} ;
  int boundaryX = rightEdge79 ;
  int boundaryY = (localID - leftEdge76) ;
  uchar isValidX = (boundaryX >= 0) ;
  uchar isValidY = (boundaryY >= 0) ;
  ulong fstIdx = 0 ;
  if (isValidX)
  {fstIdx = localIndices [boundaryX] ;} ;
  ulong sndIdx = 0 ;
  if (isValidY)
  {sndIdx = localIndices [(firstLocalLength + boundaryY)] ;} ;
  if (((! isValidX) || (isValidY && (fstIdx < sndIdx))))
  {allRowsBuffer [i] = (int) (sndIdx >> 32) ;
   allColumnsBuffer [i] = (int) sndIdx ;
   allValuesBuffer [i] = secondValuesBuffer [(((i - localID) - beginIdx) + boundaryY)] ;}
  else
  {allRowsBuffer [i] = (int) (fstIdx >> 32) ;
   allColumnsBuffer [i] = (int) fstIdx ;
   allValuesBuffer [i] = firstValuesBuffer [(beginIdx + boundaryX)] ;} ;} ;}
