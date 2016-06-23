!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_n (DGU, U, TM, GDM, GRM, &
                                      HPROGRAM)
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      B. Decharme 07/2011 : delete argument HWRITE
!!      M. Moge     02/2015 parallelization using WRITE_LCOVER
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODE_WRITE_SURF_COV, ONLY : WRITE_SURF_COV
!
USE MODI_WRITE_SURF
USE MODI_WRITE_GRID
USE MODI_WRITE_LCOVER
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
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
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
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%NTEB_PATCH,IRESP,HCOMMENT=YCOMMENT)
!
!
!* number of roof layers
!
YRECFM='ROOF_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%NROOF_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of road layers
!
YRECFM='ROAD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%NROAD_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of wall layers
!
YRECFM='WALL_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%NWALL_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* flag indicating if fields are computed from ecoclimap or not
!
YRECFM='ECOCLIMAP'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%LECOCLIMAP,IRESP,HCOMMENT=YCOMMENT)
!
!
!* Type of Building Energy Model
!
YRECFM='BEM'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%CBEM,IRESP,HCOMMENT=YCOMMENT) 
!
IF (TM%TOP%CBEM=='BEM') THEN
  YRECFM='COOL_COIL'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%BOP%CCOOL_COIL,IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HEAT_COIL'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%BOP%CHEAT_COIL,IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='AUTOSIZE'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%BOP%LAUTOSIZE,IRESP,HCOMMENT=YCOMMENT)
END IF
!
!* Type of averaging of buildings characteristics
!
YRECFM='BLD_ATYPE'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%CBLD_ATYPE,IRESP,HCOMMENT=YCOMMENT)
!
!
!
!* number of floor layers
!
IF (TM%TOP%CBEM=="BEM") THEN
  YRECFM='FLOOR_LAYER'
  YCOMMENT=YRECFM
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%BOP%NFLOOR_LAYER,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!
!* Use of solar panels
!
YRECFM='SOLAR_PANEL'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%LSOLAR_PANEL,IRESP,HCOMMENT=YCOMMENT)
!
!------------------------------------------------------------------------------
!
! * ISBA fields for urban green areas
! 
IF (TM%TOP%LGARDEN) THEN
!
! * Greenroofs and hydrology (only activated if LGARDEN)
!
YRECFM='LGREENROOF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%LGREENROOF,IRESP,HCOMMENT=YCOMMENT) 
!
YRECFM='LURBAN_HYDRO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%LHYDRO,IRESP,HCOMMENT=YCOMMENT) 
!
! * General ISBA options for urban vegetation
!
! * Pedo-transfert function
!
YRECFM='GD_PEDOTF'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,GDM%TVG%CPEDOTF,IRESP,HCOMMENT=YCOMMENT)
!
! * type of photosynthesis
!
YRECFM='GD_PHOTO'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,GDM%TVG%CPHOTO,IRESP,HCOMMENT=YCOMMENT)
!
!* new radiative transfert
!
YRECFM='GD_TR_ML'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,GDM%TVG%LTR_ML,IRESP,HCOMMENT=YCOMMENT)
!
! * ISBA fields specific to urban gardens
!
 CALL WRITESURF_PGD_TEB_VEG_n(DGU, U, &
                              GDM%DTGD, GDM%TGDO, GDM%TGDP, GDM%TVG,TM, &
                              HPROGRAM)
!
! * ISBA fields specific to urban greenroofs
!
IF (TM%TOP%LGREENROOF) CALL WRITESURF_PGD_TEB_GREENROOF_n(DGU, U, &
                                                       GRM%TGRO, GRM%TGRP, &
                                                       HPROGRAM)
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
!
 CALL WRITE_LCOVER(DGU,U,HPROGRAM,TM%TOP%LCOVER)
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF_COV(DGU, U, &
                     HPROGRAM,'COVER',TM%TOP%XCOVER(:,:),TM%TOP%LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!* orography
!
YRECFM='ZS'
YCOMMENT='ZS'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TM%TOP%XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
!* latitude, longitude
!
 CALL WRITE_GRID(DGU, U, &
                 HPROGRAM,TM%TG%CGRID,TM%TG%XGRID_PAR,TM%TG%XLAT,TM%TG%XLON,TM%TG%XMESH_SIZE,IRESP)
!
!-------------------------------------------------------------------------------
 CALL WRITESURF_PGD_TEB_PAR_n(TM%BDD, TM%DTB, GDM%DTGD, GRM%DTGR, TM%DTT, DGU, U, GDM%TGDO, &
                              GDM%TGDP, GRM%TGRO, GDM%TIR, TM%TOP, &
                              HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_n
