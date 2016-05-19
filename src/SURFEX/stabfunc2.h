C   Internal function FMI
C   Stability function for momentum in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 19
c
      FUNCTION FMI(Z2,Z02,LZZ02,ILMO2,X,X0)
#include "impnone.h"
C
      REAL              ::  FMI
      REAL, INTENT(IN ) :: Z2,Z02,LZZ02,ILMO2
      REAL, INTENT(OUT) :: X,X0
c
      X =(1-CI*Z2 *BETA*ILMO2)**(0.16666666)
      X0=(1-CI*Z02*BETA*ILMO2)**(0.16666666)
      FMI=LZZ02+LOG((X0+1)**2*SQRT(X0**2-X0+1)*(X0**2+X0+1)**1.5
     %               /((X+1)**2*SQRT(X**2-X+1)*(X**2+X+1)**1.5))
     %              +RAC3*ATAN(RAC3*((X**2-1)*X0-(X0**2-1)*X)/
     %              ((X0**2-1)*(X**2-1)+3*X*X0))
c
      RETURN
      END FUNCTION FMI
c
C   Internal function FHI
C   Stability function for heat and moisture in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 17
c
      FUNCTION FHI(Z2,Z0T2,LZZ0T2,ILMO2,Y,Y0)
#include "impnone.h"
C
      REAL              :: FHI
      REAL, INTENT(IN ) :: Z2,Z0T2,LZZ0T2,ILMO2
      REAL, INTENT(OUT) :: Y,Y0
c
      Y =(1-CI*Z2  *BETA*ILMO2)**(0.33333333)
      Y0=(1-CI*Z0T2*BETA*ILMO2)**(0.33333333)
      FHI=BETA*(LZZ0T2+1.5*LOG((Y0**2+Y0+1)/(Y**2+Y+1))+RAC3*
     %        ATAN(RAC3*2*(Y-Y0)/((2*Y0+1)*(2*Y+1)+3)))
c
      RETURN
      END FUNCTION FHI
C
C   Internal function psi
C   Stability function for momentum in the stable regime (unsl>0)
c   Reference :  Y. Delage, BLM, 82 (p23-48) (Eqs.33-37)
c
      FUNCTION PSI(Z2,HI2,ILMO2)
#include "impnone.h"
C
      REAL :: PSI
      REAL a,b,c,d
      REAL, INTENT(IN ) :: ILMO2,Z2,HI2
c
      d = 4*AS*BETA*ILMO2
      c = d*hi2 - hi2**2
      b = d - 2*hi2
      a = sqrt(1 + b*z2 - c*z2**2)
      psi = 0.5 * (a-z2*hi2-log(1+b*z2*0.5+a)-
     +            b/(2*sqrt(c))*asin((b-2*c*z2)/d))
c
      RETURN
      END FUNCTION PSI
