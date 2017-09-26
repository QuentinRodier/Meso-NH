!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB (DTCO, DGU, UG, U, USS, DTI, TM, GDM, GRM, &
                          HPROGRAM,OECOCLIMAP,OGARDEN)
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
!!    M. Moge       02/2015 : MPPDB_CHECK
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
!
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
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
#ifdef MNH_PARALLEL
USE MODE_MPPDB
!
#endif
USE MODI_WRITE_COVER_TEX_TEB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
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

TM%TOP%NROOF_LAYER  = 5
TM%TOP%NROAD_LAYER  = 5
TM%TOP%NWALL_LAYER  = 5
TM%BOP%NFLOOR_LAYER = 5
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_TEB(HPROGRAM,TM%TOP%NTEB_PATCH,TM%TOP%CBEM,TM%BOP%CCOOL_COIL,&
                       TM%BOP%CHEAT_COIL,TM%BOP%LAUTOSIZE,TM%TOP%NROAD_LAYER,&
                       TM%TOP%NROOF_LAYER,TM%TOP%NWALL_LAYER,TM%BOP%NFLOOR_LAYER, &
                      TM%TOP%LGREENROOF,TM%TOP%LHYDRO,TM%TOP%LSOLAR_PANEL     )
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CBLD',TM%TOP%CBEM,'DEF','BEM ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCOOL_COIL',TM%BOP%CCOOL_COIL,'IDEAL ','DXCOIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHEAT_COIL',TM%BOP%CHEAT_COIL,'IDEAL ','FINCAP')
!
IF (.NOT. OGARDEN) THEN
  IF (TM%TOP%LGREENROOF) CALL ABOR1_SFX('ERROR: You cannot activate LGREENROOF if LGARDEN is FALSE')
  IF (TM%TOP%LHYDRO    ) CALL ABOR1_SFX('ERROR: You cannot activate LHYDRO     if LGARDEN is FALSE')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'TOWN  ',TM%TG%NDIM)
!
ALLOCATE(TM%TOP%LCOVER     (JPCOVER))
ALLOCATE(TM%TOP%XCOVER     (TM%TG%NDIM,JPCOVER))
ALLOCATE(TM%TOP%XZS        (TM%TG%NDIM))
ALLOCATE(TM%TG%XLAT       (TM%TG%NDIM))
ALLOCATE(TM%TG%XLON       (TM%TG%NDIM))
ALLOCATE(TM%TG%XMESH_SIZE (TM%TG%NDIM))
!
 CALL PACK_PGD(DTCO, U, &
               HPROGRAM, 'TOWN  ',                    &
                TM%TG%CGRID,  TM%TG%XGRID_PAR,      &
                TM%TOP%LCOVER, TM%TOP%XCOVER, TM%TOP%XZS,  &
                TM%TG%XLAT, TM%TG%XLON, TM%TG%XMESH_SIZE      )
#ifdef MNH_PARALLEL  
 CALL MPPDB_CHECK_SURFEX3D(TM%TOP%XCOVER,"PGD_TEB after PACK_PGD:XCOVER",PRECISION,ILUOUT, 'TOWN  ',SIZE(TM%TOP%XCOVER,2))
 CALL MPPDB_CHECK_SURFEX2D(TM%TG%XLAT,"PGD_TEB after PACK_PGD:XLAT",PRECISION,ILUOUT, 'TOWN  ')
 CALL MPPDB_CHECK_SURFEX2D(TM%TG%XLON,"PGD_TEB after PACK_PGD:XLON",PRECISION,ILUOUT, 'TOWN  ')
 CALL MPPDB_CHECK_SURFEX2D(TM%TG%XMESH_SIZE,"PGD_TEB after PACK_PGD:XMESH_SIZE",PRECISION,ILUOUT, 'TOWN  ')
#endif
!
!-------------------------------------------------------------------------------
!
!*    5.      TEB specific fields
!             -------------------
!
TM%TOP%LECOCLIMAP = OECOCLIMAP
 CALL PGD_TEB_PAR(DTCO, DGU, UG, U, USS, TM%BDD, TM%DTT, DTI, TM%TG, &
                  HPROGRAM,OGARDEN,TM%TOP%LGREENROOF,TM%TOP%CBLD_ATYPE)
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
TM%TOP%LGARDEN       = OGARDEN
!
IF (TM%TOP%LGARDEN) CALL PGD_TEB_VEG(DTCO, UG, U, USS, GDM, GRM, TM%TOP, TM%TG,  &
                                  HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of Building Energy Model
!             -----------------------------
!
IF (TM%TOP%CBEM .EQ. 'BEM') CALL PGD_BEM_PAR(DTCO, DGU, UG, U, USS, TM%DTB, DTI, TM%TG, &
                                          HPROGRAM,TM%BOP%LAUTOSIZE)
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB
