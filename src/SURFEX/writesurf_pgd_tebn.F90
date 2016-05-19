!     #########
      SUBROUTINE WRITESURF_PGD_TEB_n(HPROGRAM)
!     ###############################################
!
!!****  *WRITE_PGD_TEB_n* - writes TEB fields
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      B. Decharme 07/2011 : delete argument HWRITE
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TEB_n,          ONLY : CBEM, NROOF_LAYER, NROAD_LAYER, NWALL_LAYER, &
                                XZS, XCOVER, LCOVER, LECOCLIMAP, LGARDEN,    &
                                LGREENROOF, LHYDRO,                          &
                                NTEB_PATCH, CBLD_ATYPE
USE MODD_BEM_n,          ONLY : NFLOOR_LAYER, CCOOL_COIL, CHEAT_COIL, LAUTOSIZE
USE MODD_TEB_GRID_n,     ONLY : XLAT, XLON, XMESH_SIZE, CGRID, XGRID_PAR
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_TEB_VEG_n,      ONLY : CISBA, CPEDOTF, CPHOTO, LTR_ML
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITESURF_PGD_TEB_PAR_n
USE MODI_WRITESURF_PGD_TEB_VEG_n
USE MODI_WRITESURF_PGD_TEB_GREENROOF_n
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
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_N',0,ZHOOK_HANDLE)
!
!*       1.     Dimension initializations:
!               -------------------------
!
!
!* number of TEB patches
!
YRECFM='TEB_PATCH'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NTEB_PATCH,IRESP,HCOMMENT=YCOMMENT)
!
!
!* number of roof layers
!
YRECFM='ROOF_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NROOF_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of road layers
!
YRECFM='ROAD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NROAD_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of wall layers
!
YRECFM='WALL_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NWALL_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if fields are computed from ecoclimap or not
!
YRECFM='ECOCLIMAP'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LECOCLIMAP,IRESP,HCOMMENT=YCOMMENT)
!
!
!* Type of Building Energy Model
!
YRECFM='BEM'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CBEM,IRESP,HCOMMENT=YCOMMENT) 
!
IF (CBEM=='BEM') THEN
  YRECFM='COOL_COIL'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,CCOOL_COIL,IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HEAT_COIL'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,CHEAT_COIL,IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='AUTOSIZE'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,LAUTOSIZE,IRESP,HCOMMENT=YCOMMENT)
END IF
!
!* Type of averaging of buildings characteristics
!
YRECFM='BLD_ATYPE'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CBLD_ATYPE,IRESP,HCOMMENT=YCOMMENT)
!
!
!
!* number of floor layers
!
IF (CBEM=="BEM") THEN
  YRECFM='FLOOR_LAYER'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(HPROGRAM,YRECFM,NFLOOR_LAYER,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!------------------------------------------------------------------------------
!
! * ISBA fields for urban green areas
! 
IF (LGARDEN) THEN
!
! * Greenroofs and hydrology (only activated if LGARDEN)
!
YRECFM='LGREENROOF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LGREENROOF,IRESP,HCOMMENT=YCOMMENT) 
!
YRECFM='LHYDRO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LHYDRO,IRESP,HCOMMENT=YCOMMENT) 
!
! * General ISBA options for urban vegetation
!
! * Pedo-transfert function
!
YRECFM='GD_PEDOTF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CPEDOTF,IRESP,HCOMMENT=YCOMMENT)
!
! * type of photosynthesis
!
YRECFM='GD_PHOTO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CPHOTO,IRESP,HCOMMENT=YCOMMENT)
!
!* new radiative transfert
!
YRECFM='GD_TR_ML'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,LTR_ML,IRESP,HCOMMENT=YCOMMENT)
!
! * ISBA fields specific to urban gardens
!
 CALL WRITESURF_PGD_TEB_VEG_n(HPROGRAM)
!
! * ISBA fields specific to urban greenroofs
!
IF (LGREENROOF) CALL WRITESURF_PGD_TEB_GREENROOF_n(HPROGRAM)
!
ENDIF
!
!------------------------------------------------------------------------------
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
YRECFM='COVER_LIST'
YCOMMENT='(LOGICAL LIST)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,LCOVER(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP)
!
!-------------------------------------------------------------------------------
 CALL WRITESURF_PGD_TEB_PAR_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_n
