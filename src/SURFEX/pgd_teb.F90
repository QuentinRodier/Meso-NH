!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB (DTCO, UG, U, USS, TOP, BOP, SPAOP, TG, BDD, DTT, DTB, &
                          GDO, GDK, DTGD, GDIR, GRO, GRS, GRK, DTGR, DTH, HPROGRAM)
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
!!    E.Redon/A.Lemonsu  12/2015       Key for street trees and green walls
!!    M. Goret           02/2017       Remove WALL, ROOF, FLOOR, MASS LAYER default as their are initialised by READ_NAM_PGD_TEB
!!    M. Goret           02/2017       Add TM%BOP as arg. of pgd_bem_par
!!    M. Goret           03/2017       Add a test before calling INI_DATA_PARAM_TEB to avoid bug
!!    M. Goret           04/2017       Add NTIME_CHANGE
!!    M. Goret           04/2017       Reactivate test on CHEAT_COIL + add IDEAL2 option for CCOOL_COIL, for debugging
!!    M. Goret           04/2017       Add test to avoid bug when the two variables for BEMCOMP are not equal
!!    M. Goret           05/2017       Add TM%TOP as pgd_teb_par arg. and suppress TM%TOP%NTIME_CHANGE
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SPARTACUS_OPTION_n, ONLY : SPARTACUS_OPTIONS_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_DATA_TEB_HYDRO_n, ONLY : DATA_TEB_HYDRO_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PGD_TEB_PAR
USE MODI_PGD_TEB_VEG
USE MODI_GET_LUOUT
USE MODI_READ_NAM_PGD_TEB
USE MODI_READ_NAM_PGD_SPARTACUS
USE MODI_TEST_NAM_VAR_SURF
USE MODI_PGD_BEM_PAR
USE MODI_PGD_TEB_HYDRO_PAR
USE MODI_INI_DATA_PARAM_TEB
USE MODI_ABOR1_SFX
USE MODI_INIT_TEB_SOIL_GRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef MNH_PARALLEL
USE MODE_MPPDB
!
#endif
!
USE MODI_WRITE_COVER_TEX_TEB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: SPAOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(GRID_t), INTENT(INOUT) :: TG
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GDO
TYPE(ISBA_K_t), INTENT(INOUT) :: GDK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTGD
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: GDIR
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: GRO
TYPE(ISBA_S_t), INTENT(INOUT) :: GRS
TYPE(ISBA_K_t), INTENT(INOUT) :: GRK
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTGR
TYPE(DATA_TEB_HYDRO_t) :: DTH
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
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

TOP%NROOF_LAYER  = 5
TOP%NWALL_LAYER  = 5
BOP%NFLOOR_LAYER = 5
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_TEB(HPROGRAM,TOP%NTEB_PATCH,TOP%CBEM,BOP%CCOOL_COIL,            &
                       BOP%CHEAT_COIL,BOP%LAUTOSIZE,TOP%CROAD_GRID,                &
                       TOP%NROOF_LAYER,TOP%NWALL_LAYER,BOP%NFLOOR_LAYER,           &
                       BOP%NMASS_LAYER,BOP%NBEMCOMP, TOP%NTIME_CHANGE,             &
                       TOP%LGREENROOF,TOP%CURBTREE,TOP%LURBHYDRO,TOP%LSOLAR_PANEL, &
                       TOP%LSPARTACUS, TOP%LEXPLW, TOP%LCHECK_TEB,                 &
                       TOP%XEPS_BDGT_GLOB, TOP%XEPS_BDGT_FAC, "OK"                 )
!
 CALL READ_NAM_PGD_SPARTACUS(HPROGRAM, SPAOP%LDO_SW, SPAOP%LDO_LW,          &
     SPAOP%LUSE_SW_DIRECT_ALBEDO, SPAOP%LDO_VEGETATION, SPAOP%LDO_URBAN,    &
     SPAOP%N_VEGETATION_REGION_URBAN, SPAOP%N_VEGETATION_REGION_FOREST,     &
     SPAOP%NSW, SPAOP%NLW, SPAOP%N_STREAM_SW_URBAN, SPAOP%N_STREAM_LW_URBAN,&
     SPAOP%N_STREAM_SW_FOREST, SPAOP%N_STREAM_LW_FOREST,                    &
     SPAOP%LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN,                           &
     SPAOP%LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST,                          &
     SPAOP%XVEGETATION_ISOLATION_FACTOR_URBAN,                              &
     SPAOP%XVEGETATION_ISOLATION_FACTOR_FOREST,                             &
     SPAOP%XMIN_VEGETATION_FRACTION, SPAOP%XMIN_BUILDING_FRACTION, "OK"     )
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CBLD',TOP%CBEM,'DEF','BEM ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CURBTREE',TOP%CURBTREE,'NONE','TREE','GRWL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CCOOL_COIL',BOP%CCOOL_COIL,'IDEAL ','IDEAL2','DXCOIL')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CHEAT_COIL',BOP%CHEAT_COIL,'IDEAL ','FINCAP')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CROAD_GRID',TOP%CROAD_GRID,'LOW3  ','LOW5   ','MEDIUM','HIGH  ')
!
IF (.NOT. U%LGARDEN) THEN
  IF (TOP%LGREENROOF) CALL ABOR1_SFX('ERROR: You cannot activate LGREENROOF if LGARDEN is FALSE')
  IF (TOP%CURBTREE /='NONE') CALL ABOR1_SFX('ERROR: You cannot activate CURBTREE   if LGARDEN is FALSE')
  IF (TOP%LURBHYDRO    ) CALL ABOR1_SFX('ERROR: You cannot activate LURBHYDRO  if LGARDEN is FALSE')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'TOWN  ',TG%NDIM)
!
ALLOCATE(TOP%LCOVER     (JPCOVER))
ALLOCATE(TOP%XZS        (TG%NDIM))
ALLOCATE(TG%XLAT       (TG%NDIM))
ALLOCATE(TG%XLON       (TG%NDIM))
ALLOCATE(TG%XMESH_SIZE (TG%NDIM))
!
 CALL PACK_PGD(DTCO, U, HPROGRAM, 'TOWN  ', TG, TOP%LCOVER, TOP%XCOVER, TOP%XZS  )  
!
#ifdef MNH_PARALLEL  
 CALL MPPDB_CHECK_SURFEX3D(TOP%XCOVER,"PGD_TEB after PACK_PGD:XCOVER",PRECISION,ILUOUT, 'TOWN  ',SIZE(TOP%XCOVER,2))
 CALL MPPDB_CHECK_SURFEX2D(TG%XLAT,"PGD_TEB after PACK_PGD:XLAT",PRECISION,ILUOUT, 'TOWN  ')
 CALL MPPDB_CHECK_SURFEX2D(TG%XLON,"PGD_TEB after PACK_PGD:XLON",PRECISION,ILUOUT, 'TOWN  ')
 CALL MPPDB_CHECK_SURFEX2D(TG%XMESH_SIZE,"PGD_TEB after PACK_PGD:XMESH_SIZE",PRECISION,ILUOUT, 'TOWN  ')
#endif
!-------------------------------------------------------------------------------
!
!*    5.      TEB specific fields
!             -------------------
!
TOP%LECOCLIMAP = U%LECOCLIMAP
TOP%LGARDEN    = U%LGARDEN
 CALL PGD_TEB_PAR(DTCO, UG, U, USS, BDD, DTT, TOP, TG%NDIM, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*   10.      Defines the vertical grid in the TEB soil
!             -----------------------------------------
!
! The same grid is used for road, below buildings, and, if CISBA='DIF', for
! gardens
!
CALL INIT_TEB_SOIL_GRID(HPROGRAM, TOP, TG%NDIM)
!
!-------------------------------------------------------------------------------
!
!*    6.      Prints of cover parameters in a tex file
!             ----------------------------------------
!
IF (U%LECOCLIMAP .AND. NRANK==NPIO) CALL WRITE_COVER_TEX_TEB
!
!
!-------------------------------------------------------------------------------
!
!*    7.      Case of urban green areas (and hydrology)
!             -----------------------------------------
!
!
CALL PGD_TEB_VEG(DTCO, UG, U, USS, GDO, GDK, DTGD, GDIR, &
                 GRO, GRS, GRK, DTGR, TOP, TG%NDIM, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    8.      Case of Building Energy Model
!             -----------------------------
!
IF (TOP%CBEM .EQ. 'BEM') CALL PGD_BEM_PAR(DTCO, UG, U, USS, DTB, BOP, TG%NDIM, HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*    9.      Case of urban hydrology
!             -----------------------
!
IF (TOP%LURBHYDRO) CALL PGD_TEB_HYDRO_PAR(DTCO, UG, U, USS, DTH, TG%NDIM, HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB
