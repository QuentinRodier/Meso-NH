!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######spl
      SUBROUTINE PUT_RAD_SEA_n(HPROGRAM,KI,PSST,PTICE,PDIR_ALB,PSCA_ALB,PICE_ALB)
!     ###########################################################################
!
!!****  *PUT_RAD_SEA_n* - routine to modify sea/ice sst and albedos
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
!
USE MODD_SEAFLUX_n,  ONLY : XSST,XTICE,XDIR_ALB,XSCA_ALB,XICE_ALB 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(IN)  :: PSST     
REAL, DIMENSION(KI), INTENT(IN)  :: PTICE     
REAL, DIMENSION(KI), INTENT(IN)  :: PDIR_ALB
REAL, DIMENSION(KI), INTENT(IN)  :: PSCA_ALB
REAL, DIMENSION(KI), INTENT(IN)  :: PICE_ALB
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_RAD_SEA_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PSST) /= SIZE(XSST) ) THEN
  WRITE(ILUOUT,*) 'try to get PSST field from oceanic model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the oceanic model (PSST) :', SIZE(PSST)
  WRITE(ILUOUT,*) 'size of field over SURFEX sea               (XSST) :', SIZE(XSST)
  STOP
ELSE
  XSST    (:)=PSST    (:)
  XTICE   (:)=PTICE   (:)
  XDIR_ALB(:)=PDIR_ALB(:)
  XSCA_ALB(:)=PSCA_ALB(:)
  XICE_ALB(:)=PICE_ALB(:)
END IF
IF (LHOOK) CALL DR_HOOK('PUT_RAD_SEA_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE PUT_RAD_SEA_n
