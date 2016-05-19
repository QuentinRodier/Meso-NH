!     #########
      SUBROUTINE PGD_TEB(HPROGRAM,OECOCLIMAP,OGARDEN)
!     ##############################################################
!
!!**** *PGD_TEB* monitor for averaging and interpolations of TEB physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    A. Lemonsu      05/2009         Key for garden option
!!    G. Pigeon     /09/12: WALL, ROOF, FLOOR, MASS LAYER default to 5
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_TEB_n,          ONLY : XCOVER, LCOVER, XZS,                   &
                                NROAD_LAYER, NWALL_LAYER, NROOF_LAYER, &
                                LECOCLIMAP, LGARDEN, NTEB_PATCH,       &
                                CBLD_ATYPE, CBEM, LGREENROOF, LHYDRO 
USE MODD_BEM_n,          ONLY : NFLOOR_LAYER, CCOOL_COIL, CHEAT_COIL, LAUTOSIZE
USE MODD_TEB_GRID_n,     ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE, NDIM
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PGD_TEB_PAR
USE MODI_PGD_TEB_VEG
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_TEB
USE MODI_TEST_NAM_VAR_SURF
USE MODI_PGD_BEM_PAR
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_COVER_TEX_TEB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
LOGICAL,          INTENT(IN)  :: OECOCLIMAP ! T if parameters are computed with ecoclimap
!                                           ! F if all parameters must be specified
LOGICAL,          INTENT(IN)  :: OGARDEN    ! T if urban green areas
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER         :: ILUOUT    ! output listing logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)

NROOF_LAYER  = 5
NROAD_LAYER  = 5
NWALL_LAYER  = 5
NFLOOR_LAYER = 5
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_TEB(HPROGRAM,NTEB_PATCH,CBEM,CCOOL_COIL,CHEAT_COIL,LAUTOSIZE,&
                      NROAD_LAYER,NROOF_LAYER,NWALL_LAYER,NFLOOR_LAYER,        &
                      LGREENROOF,LHYDRO                                        )
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CBLD',CBEM,'DEF','BEM ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCOOL_COIL',CCOOL_COIL,'IDEAL ','DXCOIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHEAT_COIL',CHEAT_COIL,'IDEAL ','FINCAP')
!
IF (.NOT. OGARDEN) THEN
  IF (LGREENROOF) CALL ABOR1_SFX('ERROR: You cannot activate LGREENROOF if LGARDEN is FALSE')
  IF (LHYDRO    ) CALL ABOR1_SFX('ERROR: You cannot activate LHYDRO     if LGARDEN is FALSE')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n('TOWN  ',NDIM)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (NDIM,JPCOVER))
ALLOCATE(XZS        (NDIM))
ALLOCATE(XLAT       (NDIM))
ALLOCATE(XLON       (NDIM))
ALLOCATE(XMESH_SIZE (NDIM))
!
 CALL PACK_PGD(HPROGRAM, 'TOWN  ',                    &
                CGRID,  XGRID_PAR,                   &
                LCOVER, XCOVER, XZS,                 &
                XLAT, XLON, XMESH_SIZE               )  
!
!-------------------------------------------------------------------------------
!
!*    5.      TEB specific fields
!             -------------------
!
LECOCLIMAP = OECOCLIMAP
 CALL PGD_TEB_PAR(HPROGRAM,OGARDEN,LGREENROOF,CBLD_ATYPE)
!
!-------------------------------------------------------------------------------
!
!*    6.      Prints of cover parameters in a tex file
!             ----------------------------------------
!
IF (OECOCLIMAP) CALL WRITE_COVER_TEX_TEB
!
!
!-------------------------------------------------------------------------------
!
!*    7.      Case of urban green areas (and hydrology)
!             -----------------------------------------
!
LGARDEN       = OGARDEN
!
IF (LGARDEN) CALL PGD_TEB_VEG(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of Building Energy Model
!             -----------------------------
!
IF (CBEM .EQ. 'BEM') CALL PGD_BEM_PAR(HPROGRAM,LAUTOSIZE)
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB
