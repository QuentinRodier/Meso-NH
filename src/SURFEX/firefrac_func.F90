!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    FUNCTION FIREFRA_FUNC (PX) RESULT (ZFIREFRA_RESULT)
!   ############################################################################
!
!!****  *FIREFRA_FUNC*  
!!
!!    PURPOSE
!!    -------
!!
!!     
!!    AUTHOR
!!    ------
!!
!!	R. Alkama           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2013 
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PX               ! fire index
!
!*      0.2    declarations of local variables
REAL, DIMENSION(SIZE(PX))        :: ZFIREFRA_RESULT ! fire fraction
REAL, DIMENSION(SIZE(PX))        :: ZXM1             ! intermediate variable
REAL, DIMENSION(SIZE(PX))        :: ZXM2             ! intermediate variable
REAL, DIMENSION(SIZE(PX))        :: ZXM3             ! intermediate variable
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!  wind gradient
!
IF (LHOOK) CALL DR_HOOK('FIREFRA_FUNC',0,ZHOOK_HANDLE)
!
ZXM1(:) = PX(:) - 1.0
ZXM2(:) = ZXM1(:)*ZXM1(:)
ZXM3(:) = ZXM1(:)*ZXM1(:)*ZXM1(:)
!
ZFIREFRA_RESULT(:) = PX(:) * EXP(ZXM1(:)/(0.45*ZXM3(:)+2.83*ZXM2(:)+2.96*ZXM1(:)+1.04))
ZFIREFRA_RESULT(:) = MIN(ZFIREFRA_RESULT,1.0)
!
IF (LHOOK) CALL DR_HOOK('FIREFRA_FUNC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION FIREFRA_FUNC
