!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_VEG_n (DGU, U, &
                                           DTGD, TGDO, TGDP, TVG,TM, &
                                          HPROGRAM)
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
!!      A. Lemonsu & C. de Munck   *Meteo France*
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
!
!
!
!
USE MODD_SURF_PAR,          ONLY : XUNDEF, NUNDEF
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_TEB_GARDEN_OPTION_n, ONLY : TEB_GARDEN_OPTIONS_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
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
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: TGDO
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JJ, JLAYER,JL
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK
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
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TVG%CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(TVG%CISBA=='DIF') THEN
  DO JLAYER=1,TGDO%NGROUND_LAYER
     WRITE(YLVL,'(I4)') JLAYER     
     YRECFM='GD_SGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Depth of TEB Garden soilgrid layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDO%XSOILGRID(JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO 
ENDIF
!
!* number of soil layers
!
YRECFM='GD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDO%NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for vegetation characteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GD_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DTGD%NTIME,IRESP,HCOMMENT=YCOMMENT)
!
! * clay fraction
!
YRECFM='GD_CLAY'
YCOMMENT='X_Y_GD_CLAY'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * sand fraction
!
YRECFM='GD_SAND'
YCOMMENT='X_Y_GD_SAND'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * orographic runoff coefficient
!
YRECFM='GD_RUNOFFB'
YCOMMENT='X_Y_GD_RUNOFFB'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!        
! * subgrid drainage coefficient
!
YRECFM='GD_WDRAIN'
YCOMMENT='X_Y_GD_WDRAIN'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TGDP%XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*    3.      ISBA diagnostic PGD fields stored in PGD file for improved efficiency in PREP step
!             ----------------------------------------------------------------------------------
!
IF (TM%TOP%LECOCLIMAP .AND. ASSOCIATED(TGDP%XDG)) THEN
  ALLOCATE(ZWORK(SIZE(TGDP%XDG,1)))
!
!* Soil depth for each patch
!
  DO JL=1,SIZE(TGDP%XDG,2)
    IF (JL<10) THEN
      WRITE(YRECFM,FMT='(A9,I1)') 'GD_ECO_DG',JL
    ELSE
      WRITE(YRECFM,FMT='(A9,I2)') 'GD_ECO_DG',JL          
    ENDIF
    YCOMMENT='soil depth from ecoclimap'//' (M)'
    ZWORK(:) = TGDP%XDG(:,JL)
    IF (ASSOCIATED(TM%T%CUR%XGARDEN)) THEN  ! in PGD step, XGARDEN is not associated. In other steps, it is.
      WHERE (TM%T%CUR%XGARDEN==0.) ZWORK=XUNDEF
    END IF
    CALL WRITE_SURF(DGU,U,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  END DO
!* Number of soil layers for moisture
!
  IF (TVG%CISBA=='DIF') THEN
    YRECFM='GD_ECO_WG_L'
    YCOMMENT='Number of soil layers for moisture in ISBA-DIF'
    ZWORK=FLOAT(TGDP%NWG_LAYER(:))
    IF (ASSOCIATED(TM%T%CUR%XGARDEN)) THEN
      WHERE (TM%T%CUR%XGARDEN==0.) ZWORK=FLOAT(NUNDEF)
    END IF
    CALL WRITE_SURF(DGU,U,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  END IF

  DEALLOCATE(ZWORK)
END IF


!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_VEG_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_VEG_n
