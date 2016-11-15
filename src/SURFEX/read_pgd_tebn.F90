!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_n (DTCO, U, TM,GCP, &
                                 HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_TEB_n* - reads TEB physiographic fields
!!                       
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      M. Moge     02/2015 READ_SURF
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_READ_GRID
USE MODI_READ_LCOVER
USE MODI_READ_PGD_TEB_PAR_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_READ_LECOCLIMAP
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',TM%TG%NDIM)
!
!*       2.     Other dimension initializations:
!               --------------------------------
!
 CALL READ_SURF(&
                HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
                HPROGRAM,'BUG',IBUGFIX,IRESP)
!
!* number of TEB patches
!
IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<=2) THEN
  TM%TOP%NTEB_PATCH=1
ELSE
  YRECFM='TEB_PATCH'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%NTEB_PATCH,IRESP)
END IF
!
!* number of road and roof layers
!
YRECFM='ROAD_LAYER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%NROAD_LAYER,IRESP)

YRECFM='ROOF_LAYER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%NROOF_LAYER,IRESP)

YRECFM='WALL_LAYER'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%NWALL_LAYER,IRESP)
!
!
!* type of averaging for Buildings (to allow ascendant compatibility)
!* type of Building Energy Model
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
  TM%TOP%CBLD_ATYPE='ARI'
  TM%TOP%CBEM = 'DEF'
ELSE
  YRECFM='BLD_ATYPE'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%CBLD_ATYPE,IRESP)
  YRECFM='BEM'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%CBEM,IRESP)
END IF
!
IF (TM%TOP%CBEM=="BEM") THEN
  YRECFM='FLOOR_LAYER'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%BOP%NFLOOR_LAYER,IRESP)
  YRECFM='COOL_COIL'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%BOP%CCOOL_COIL,IRESP)
  YRECFM='HEAT_COIL'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%BOP%CHEAT_COIL,IRESP)
  YRECFM='AUTOSIZE'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%BOP%LAUTOSIZE,IRESP)
ENDIF
!
!* Case of urban green roofs
!
IF (TM%TOP%LGARDEN) THEN
  IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
    TM%TOP%LGREENROOF = .FALSE.
  ELSE
    YRECFM='LGREENROOF'
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%LGREENROOF,IRESP)
  END IF
!
!* Case of urban hydrology
!
  IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=3)) THEN
    TM%TOP%LHYDRO = .FALSE.
  ELSE
    YRECFM='LURBAN_HYDRO'
    CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%LHYDRO,IRESP)
  END IF
ENDIF
!
!* Solar panels
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=3)) THEN
  TM%TOP%LSOLAR_PANEL = .FALSE.
ELSE
  YRECFM='SOLAR_PANEL'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%LSOLAR_PANEL,IRESP)
END IF
!
!
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(TM%TOP%LCOVER(JPCOVER))
 CALL READ_LCOVER(&
                  HPROGRAM,TM%TOP%LCOVER)
!
ALLOCATE(TM%TOP%XCOVER(TM%TG%NDIM,COUNT(TM%TOP%LCOVER)))
#ifdef MNH_PARALLEL
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',TM%TOP%XCOVER(:,:),TM%TOP%LCOVER,IRESP,HDIR='H')
#else
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',TM%TOP%XCOVER(:,:),TM%TOP%LCOVER,IRESP)
#endif
!
!* orography
!
ALLOCATE(TM%TOP%XZS(TM%TG%NDIM))
YRECFM='ZS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TM%TOP%XZS(:),IRESP)
!
!
!* latitude, longitude 
!
ALLOCATE(TM%TG%XLAT      (TM%TG%NDIM))
ALLOCATE(TM%TG%XLON      (TM%TG%NDIM))
ALLOCATE(TM%TG%XMESH_SIZE(TM%TG%NDIM))
 CALL READ_GRID(&
                HPROGRAM,TM%TG%CGRID,TM%TG%XGRID_PAR,&
                TM%TG%XLAT,TM%TG%XLON,TM%TG%XMESH_SIZE,IRESP)
!
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
 CALL READ_LECOCLIMAP(&
                      HPROGRAM,TM%TOP%LECOCLIMAP)
!
 CALL READ_PGD_TEB_PAR_n(DTCO, U,GCP, &
                         TM%BDD, TM%DTB, TM%DTT, TM%TG, TM%TOP, &
                         HPROGRAM,TM%TG%NDIM,'-')
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_N',1,ZHOOK_HANDLE)
!
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_n
