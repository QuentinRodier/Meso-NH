!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ################
FUNCTION CONTROL_TEMP_FUNC_MG(PTEMP) RESULT (PRESULT)
!
!   ###############################################################
!!**   CONTROL_TEMP_FUNC_MG
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!     Temperature control factor of decomposition.
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/09
!!      B. Decharme 05/2012 : Optimization and ISBA-DIF coupling
!!      
!-------------------------------------------------------------------------------

!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XTT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PTEMP ! temperature (°C)
!
!*      0.2    declarations of local variables
!
REAL                            :: ZCOEF1
REAL, PARAMETER                 :: ZCOEF2 = 10.0
REAL, PARAMETER                 :: ZCOEF3 = 30.0
!
REAL, PARAMETER                 :: ZTSUP   = 4.0 ! Limit to produce methanogenesis in °C
REAL, PARAMETER                 :: ZTINF   = 3.0 ! Limit to produce methanogenesis in °C
!
REAL, DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2)) :: ZLIM, ZQ10
!
REAL, DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2)) :: PRESULT ! temperature control factor
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CONTROL_TEMP_FUNC_MG',0,ZHOOK_HANDLE)
!
!*       1 Calculates temperature control factor
!
ZCOEF1 = LOG(2.0)
ZQ10(:,:) = EXP( (ZCOEF1/ZCOEF2) * (PTEMP(:,:)-ZCOEF3) )
!
ZLIM(:,:) = MIN(1.0,MAX(0.0,PTEMP(:,:)-ZTINF)/(ZTSUP-ZTINF))
!
PRESULT(:,:) = MIN( 1., ZQ10(:,:)*ZLIM(:,:) )
!
IF (LHOOK) CALL DR_HOOK('CONTROL_TEMP_FUNC_MG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END FUNCTION CONTROL_TEMP_FUNC_MG
