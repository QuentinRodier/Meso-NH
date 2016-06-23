!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################################
      SUBROUTINE WRITE_PGD_SURF_ATM_n (YSC, &
                                       HPROGRAM)
!     ####################################
!
!!****  *WRITE_PGD_SURF_ATM_n* - routine to write pgd surface variables 
!!                               in their respective files or in file
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2011 according to previous write_surf_atmn.f90
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_SURF_PAR,        ONLY : NVERSION, NBUGFIX
USE MODD_IO_SURF_FA,      ONLY : LFANOCOMPACT
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_PGD_SEA_n
USE MODI_WRITE_PGD_INLAND_WATER_n
USE MODI_WRITE_PGD_NATURE_n
USE MODI_WRITE_PGD_TOWN_n
USE MODI_END_IO_SURF_n
!
USE MODI_FLAG_UPDATE
!
USE MODI_WRITESURF_COVER_n
USE MODI_WRITESURF_SSO_n
USE MODI_WRITESURF_DUMMY_n
USE MODI_WRITESURF_SNAP_n
USE MODI_WRITESURF_CH_EMIS_n
USE MODI_WRITE_GRID
!
USE MODI_WRITE_ECOCLIMAP2_DATA
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
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=3)   :: YWRITE
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!*       0.     Initialize some options:
!               ------------------------
!
 CPROGNAME = HPROGRAM
!
 CALL FLAG_UPDATE(YSC%IM%DGI, YSC%DGU, &
                  .FALSE.,.TRUE.,.FALSE.,.FALSE.)
!
!*       1.     Configuration and cover fields:
!               ------------------------------
!
!
!         Initialisation for IO
!
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%DGU, YSC%U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
!
YWRITE='PGD'
YCOMMENT='(-)'
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'VERSION',NVERSION,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'BUG    ',NBUGFIX ,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'STORAGETYPE',YWRITE,IRESP,YCOMMENT)
!
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'SEA   ',YSC%U%CSEA   ,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'WATER ',YSC%U%CWATER ,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'NATURE',YSC%U%CNATURE,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'TOWN  ',YSC%U%CTOWN  ,IRESP,YCOMMENT)
!
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,  IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'DIM_SEA   ',YSC%U%NDIM_SEA,   IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'DIM_NATURE',YSC%U%NDIM_NATURE,IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'DIM_WATER ',YSC%U%NDIM_WATER, IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'DIM_TOWN  ',YSC%U%NDIM_TOWN,  IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'ECOCLIMAP ',YSC%U%LECOCLIMAP ,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'WATER_TO_NAT',YSC%U%LWATER_TO_NATURE,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'TOWN_TO_ROCK',YSC%U%LTOWN_TO_ROCK,IRESP,YCOMMENT)
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'GARDEN',YSC%U%LGARDEN,IRESP,YCOMMENT)
IF (HPROGRAM.NE.'BINARY' .AND. HPROGRAM.NE.'TEXTE ') THEN
   CALL WRITE_ECOCLIMAP2_DATA(YSC%DGU, YSC%U, &
                              HPROGRAM)
ENDIF
!
 CALL WRITE_GRID(YSC%DGU, YSC%U, &
                 HPROGRAM,YSC%UG%CGRID,YSC%UG%XGRID_PAR,YSC%UG%XLAT,YSC%UG%XLON,&
                 YSC%UG%XMESH_SIZE,IRESP,YSC%USS%XZ0EFFJPDIR)
!
 CALL WRITESURF_COVER_n(YSC%DGU, &
                        YSC%U, &
                        HPROGRAM)
 CALL WRITESURF_SSO_n(YSC%DGU, YSC%U, &
                      YSC%USS, &
                      HPROGRAM)
 CALL WRITESURF_DUMMY_n(YSC%DGU, YSC%U, &
                        YSC%DUU, &
                        HPROGRAM)
!
YCOMMENT='CH_EMIS'
 CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'CH_EMIS',YSC%CHU%LCH_EMIS,IRESP,HCOMMENT=YCOMMENT)
!
IF (YSC%CHU%LCH_EMIS) THEN
  YCOMMENT='CH_EMIS_OPT'
  CALL WRITE_SURF(YSC%DGU, YSC%U, &
                 HPROGRAM,'CH_EMIS_OPT',YSC%CHU%CCH_EMIS,IRESP,HCOMMENT=YCOMMENT)
END IF
!
IF (YSC%CHU%LCH_EMIS) THEN
  IF (YSC%CHU%CCH_EMIS=='AGGR') THEN
    CALL WRITESURF_CH_EMIS_n(YSC%DGU, YSC%U, &
                             YSC%CHE, &
                             HPROGRAM)
  ELSE IF (YSC%CHU%CCH_EMIS=='SNAP') THEN
    CALL WRITESURF_SNAP_n(YSC%DGU, YSC%U, &
                          YSC%CHN, &
                          HPROGRAM)
  ENDIF
ENDIF
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!
!*       2.     Sea
!               ---
!
IF (YSC%U%NDIM_SEA>0) CALL WRITE_PGD_SEA_n(YSC%DTCO, YSC%DGU, YSC%U, &
                                YSC%SM%DTS, YSC%SM%SG, YSC%SM%S, &
                                           HPROGRAM)
!
!
!*       3.     Inland water
!               ------------
!
IF (YSC%U%NDIM_WATER>0) CALL WRITE_PGD_INLAND_WATER_n(YSC%DTCO, YSC%DGU, YSC%U, &
                                                      YSC%WM%WG, YSC%WM%W, YSC%FM%FG, YSC%FM%F, &
                                                      HPROGRAM)
!
!
!*       4.     Vegetation scheme
!               -----------------
!
IF (YSC%U%NDIM_NATURE>0) CALL WRITE_PGD_NATURE_n(YSC%DTCO, YSC%DGU, YSC%U, &
                                                YSC%IM%DTI, YSC%DTZ, YSC%IM%IG, YSC%IM%I, &
                                                 HPROGRAM)
!
!
!*       5.     Urban scheme
!               ------------
!
IF (YSC%U%NDIM_TOWN>0) CALL WRITE_PGD_TOWN_n(YSC%DTCO, YSC%DGU, YSC%U, &
                                             YSC%TM, YSC%GDM, YSC%GRM, &
                                         HPROGRAM)
!
!
IF (LHOOK) CALL DR_HOOK('WRITE_PGD_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_PGD_SURF_ATM_n
