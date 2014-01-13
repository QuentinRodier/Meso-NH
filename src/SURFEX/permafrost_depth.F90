!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################
SUBROUTINE PERMAFROST_DEPTH (KNI,KPATCH,PPERM,PSOILDEPTH)
!     ###################################################
!
!!****  *PERMAFROST_DEPTH*  
!!
!!    PURPOSE
!!    -------
! 
!     Extended  ground depth to 12m over permafrost area
!
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    30/08/12 
!-------------------------------------------------------------------------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!*      0.1    declarations of arguments
!
IMPLICIT NONE
!
INTEGER,             INTENT(IN   ) :: KNI        ! number of point
!
INTEGER,             INTENT(IN   ) :: KPATCH     ! patch number
!
REAL, DIMENSION(:),  INTENT(IN   ) :: PPERM      ! permafrost area (fraction)
!
REAL, DIMENSION(:,:),INTENT(INOUT) :: PSOILDEPTH ! output soil depth distribution (m)
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER :: ZPERMFRAC  = 0.25   ! permafrost limit area (fraction)
!
REAL, PARAMETER :: ZPERMDEPTH = 12.0   ! permafrost depth (m)
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(KNI) :: ZPERM
!
INTEGER :: JJ, JPATCH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PERMAFROST_DEPTH',0,ZHOOK_HANDLE)
!
ZPERM(:)=0.0
WHERE(PPERM(:)/=XUNDEF)ZPERM(:)=PPERM(:)
!
DO JPATCH=1,KPATCH
   DO JJ=1,KNI
      IF(ZPERM(JJ)>=ZPERMFRAC.AND.PSOILDEPTH(JJ,JPATCH)/=XUNDEF)THEN
         PSOILDEPTH(JJ,JPATCH)=ZPERMDEPTH
       ENDIF
   ENDDO
ENDDO

!
IF (LHOOK) CALL DR_HOOK('PERMAFROST_DEPTH',1,ZHOOK_HANDLE)
!
END SUBROUTINE PERMAFROST_DEPTH





