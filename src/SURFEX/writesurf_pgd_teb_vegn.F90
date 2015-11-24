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
USE MODD_SURF_PAR,          ONLY : XUNDEF, NUNDEF
USE MODD_TEB_n,             ONLY : LECOCLIMAP, XGARDEN
USE MODD_TEB_VEG_n,         ONLY : CISBA
USE MODD_TEB_GARDEN_n,      ONLY : NGROUND_LAYER, XSOILGRID,         &
                                   XCLAY, XSAND, XRUNOFFB, XWDRAIN,  &
                                   XDG, NWG_LAYER
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
INTEGER :: JL ! loop counter

REAL(KIND=JPRB) :: ZHOOK_HANDLE
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK
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
!-------------------------------------------------------------------------------
!
!*    3.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (LECOCLIMAP .AND. ASSOCIATED(XDG)) THEN
  ALLOCATE(ZWORK(SIZE(XDG,1)))
!
!* Soil depth for each patch
!
  DO JL=1,SIZE(XDG,2)
    IF (JL<10) THEN
      WRITE(YRECFM,FMT='(A9,I1)') 'GD_ECO_DG',JL
    ELSE
      WRITE(YRECFM,FMT='(A9,I2)') 'GD_ECO_DG',JL          
    ENDIF
    YCOMMENT='soil depth from ecoclimap'//' (M)'
    ZWORK(:) = XDG(:,JL)
    IF (ASSOCIATED(XGARDEN)) THEN  ! in PGD step, XGARDEN is not associated. In other steps, it is.
      WHERE (XGARDEN==0.) ZWORK=XUNDEF
    END IF
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!* Number of soil layers for moisture
!
  IF (CISBA=='DIF') THEN
    YRECFM='GD_ECO_WG_L'
    YCOMMENT='Number of soil layers for moisture in ISBA-DIF'
    ZWORK=FLOAT(NWG_LAYER(:))
    IF (ASSOCIATED(XGARDEN)) THEN
      WHERE (XGARDEN==0.) ZWORK=FLOAT(NUNDEF)
    END IF
    CALL WRITE_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  END IF

  DEALLOCATE(ZWORK)
END IF

!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_VEG_n
