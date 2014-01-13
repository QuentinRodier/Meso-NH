!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_VEG_n(HPROGRAM)
!     ###############################################
!
!!****  *WRITE_PGD_TEB_VEG_n* - writes ISBA fields describing urban gardens
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
!!	A. Lemonsu & C. de Munck   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_VEG_n,         ONLY : CISBA
USE MODD_TEB_GARDEN_n,      ONLY : NGROUND_LAYER, XSOILGRID,         &
                                   XCLAY, XSAND, XRUNOFFB, XWDRAIN  
USE MODD_DATA_TEB_GARDEN_n, ONLY : NTIME
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',0,ZHOOK_HANDLE)
!
!* soil scheme option
!
YRECFM='GD_ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(CISBA=='DIF') THEN
  YRECFM='GD_SOILGRID'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOILGRID,IRESP,HCOMMENT=YCOMMENT,HDIR='-')
ENDIF
!
!* number of soil layers
!
YRECFM='GD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for vegetation characteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GD_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NTIME,IRESP,HCOMMENT=YCOMMENT)
!
! * clay fraction
!
YRECFM='GD_CLAY'
YCOMMENT='X_Y_GD_CLAY'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * sand fraction
!
YRECFM='GD_SAND'
YCOMMENT='X_Y_GD_SAND'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * orographic runoff coefficient
!
YRECFM='GD_RUNOFFB'
YCOMMENT='X_Y_GD_RUNOFFB'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!        
! * subgrid drainage coefficient
!
YRECFM='GD_WDRAIN'
YCOMMENT='X_Y_GD_WDRAIN'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_VEG_n
