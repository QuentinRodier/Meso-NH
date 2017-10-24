!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE PGD_SURF_ATM (YSC, &
                               HPROGRAM,HFILE,HFILETYPE,OZS)
!     ###########################################################
!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!!      A. Lemonsu      05/2009         Ajout de la clef LGARDEN pour TEB
!!      J. Escobar      11/2013         Add USE MODI_READ_NAM_PGD_CHEMISTRY
!!      B. Decharme     02/2014         Add LRM_RIVER
!!      M.Moge          02/2015         check with MPPDB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_PGD_GRID,        ONLY : LLATLONMASK
!
USE MODI_GET_LUOUT
USE MODI_READ_PGD_ARRANGE_COVER
USE MODI_READ_PGD_COVER_GARDEN
USE MODI_INI_DATA_COVER
USE MODI_READ_PGD_SCHEMES
USE MODI_READ_NAM_PGD_CHEMISTRY
USE MODI_READ_NAM_WRITE_COVER_TEX
USE MODI_WRITE_COVER_TEX_START
USE MODI_WRITE_COVER_TEX_COVER
USE MODI_LATLON_GRID
USE MODI_PUT_PGD_GRID
USE MODI_LATLONMASK
USE MODI_PGD_FRAC
USE MODI_PGD_COVER
USE MODI_PGD_OROGRAPHY
USE MODI_PGD_NATURE
USE MODI_PGD_TOWN
USE MODI_PGD_INLAND_WATER
USE MODI_PGD_SEA
USE MODI_PGD_DUMMY
USE MODI_PGD_CHEMISTRY
USE MODI_PGD_CHEMISTRY_SNAP
USE MODI_WRITE_COVER_TEX_END
USE MODI_INIT_READ_DATA_COVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_NAM_PGD_CHEMISTRY
#ifdef MNH_PARALLEL
!
USE MODE_MPPDB
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE    ! atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE! atmospheric file type
LOGICAL,              INTENT(IN)  :: OZS      ! .true. if orography is imposed by atm. model
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL :: LRM_RIVER   !delete inland river coverage. Default is false
!
INTEGER :: ILUOUT ! logical unit of output listing file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_SURF_ATM',0,ZHOOK_HANDLE)
!
LRM_RIVER = .FALSE.
!
 CPROGNAME=HPROGRAM
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Set default constant values 
!             ---------------------------
!
 CALL READ_PGD_ARRANGE_COVER(HPROGRAM,YSC%U%LWATER_TO_NATURE,YSC%U%LTOWN_TO_ROCK)
!
 CALL READ_PGD_COVER_GARDEN(HPROGRAM,YSC%U%LGARDEN)
!
 CALL INIT_READ_DATA_COVER(HPROGRAM)
!
 CALL INI_DATA_COVER(YSC%DTCO, YSC%U)
!
!*    1.2     surface schemes
 CALL READ_PGD_SCHEMES(HPROGRAM,YSC%U%CNATURE,YSC%U%CSEA,YSC%U%CTOWN,YSC%U%CWATER)
!
!*    1.3     prints all parameters in a Latex file
 CALL READ_NAM_WRITE_COVER_TEX(HPROGRAM)
!
 CALL WRITE_COVER_TEX_START(HPROGRAM)
 CALL WRITE_COVER_TEX_COVER
!-------------------------------------------------------------------------------
!
!*    2.      Grid
!             ----
!
ALLOCATE(YSC%UG%XLAT(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%XLON(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%XMESH_SIZE(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%XJPDIR(YSC%U%NSIZE_FULL))
 CALL LATLON_GRID(YSC%UG%CGRID,YSC%UG%NGRID_PAR,YSC%U%NSIZE_FULL,ILUOUT,&
                  YSC%UG%XGRID_PAR,YSC%UG%XLAT,YSC%UG%XLON,YSC%UG%XMESH_SIZE,YSC%UG%XJPDIR)
#ifdef MNH_PARALLEL
! CALL MPPDB_CHECK_SURFEX2D(YSC%UG%XLAT,"PGD_SURF_ATM_n after LATLON_GRID:XLAT",PRECISION,ILUOUT)
! CALL MPPDB_CHECK_SURFEX2D(YSC%UG%XLON,"PGD_SURF_ATM_n after LATLON_GRID:XLON",PRECISION,ILUOUT)
! CALL MPPDB_CHECK_SURFEX2D(YSC%UG%XMESH_SIZE,"PGD_SURF_ATM_n after LATLON_GRID:XMESH_SIZE",PRECISION,ILUOUT)
#endif
!
!
!*    2.3     Stores the grid in the module MODD_PGD_GRID
!
 CALL PUT_PGD_GRID(YSC%UG%CGRID,YSC%U%NSIZE_FULL,YSC%UG%NGRID_PAR,YSC%UG%XGRID_PAR)
!
!*    2.4     mask to limit the number of input data to read
 CALL LATLONMASK      (YSC%UG%CGRID,YSC%UG%NGRID_PAR,YSC%UG%XGRID_PAR,LLATLONMASK)
!
!-------------------------------------------------------------------------------
!
!*    3.      surface cover
!             -------------
!
 CALL PGD_FRAC(YSC%DTCO, YSC%UG, YSC%U, YSC%USS, &
               HPROGRAM,YSC%U%LECOCLIMAP)
IF (YSC%U%LECOCLIMAP) CALL PGD_COVER(YSC%DGU, YSC%DTCO, YSC%UG, YSC%U, YSC%USS, &
                                 HPROGRAM,LRM_RIVER)
!
!-------------------------------------------------------------------------------
!
!*    4.      Orography
!             ---------
!
 CALL PGD_OROGRAPHY(YSC%DGU, YSC%DTCO, YSC%UG, YSC%U, YSC%USS, &
                    HPROGRAM,YSC%U%XSEA,YSC%U%XWATER,HFILE,HFILETYPE,OZS)
!
!_______________________________________________________________________________
!
!*    5.      Additionnal fields for nature scheme
!             ------------------------------------
!
IF (YSC%U%NDIM_NATURE>0) CALL PGD_NATURE(YSC%DTCO, YSC%IM%DTI, YSC%DTZ, YSC%DGU, YSC%IM%IG, &
                                         YSC%IM%I, YSC%UG, YSC%U, YSC%USS, &
                                         HPROGRAM,YSC%U%LECOCLIMAP)  
!_______________________________________________________________________________
!
!*    6.      Additionnal fields for town scheme
!             ----------------------------------
!
IF (YSC%U%NDIM_TOWN>0) CALL PGD_TOWN(YSC%DTCO, YSC%DGU, YSC%UG, YSC%U, YSC%USS, &
                                     YSC%IM%DTI, YSC%TM, YSC%GDM, YSC%GRM, &
                                 HPROGRAM,YSC%U%LECOCLIMAP,YSC%U%LGARDEN)  
!_______________________________________________________________________________
!
!*    7.      Additionnal fields for inland water scheme
!             ------------------------------------------
!
IF (YSC%U%NDIM_WATER>0) CALL PGD_INLAND_WATER(YSC%DTCO, YSC%FM%FG, YSC%FM%F, YSC%UG, YSC%U, &
                                           YSC%USS, YSC%WM%WG, YSC%WM%W, &
                                          HPROGRAM,YSC%U%LECOCLIMAP,LRM_RIVER)   
!_______________________________________________________________________________
!
!*    8.      Additionnal fields for sea scheme
!             ---------------------------------
!
IF (YSC%U%NDIM_SEA>0) CALL PGD_SEA(YSC%DTCO, YSC%SM%DTS, YSC%SM%SG, YSC%SM%S, YSC%UG, YSC%U, YSC%USS, &
                               HPROGRAM)  
!
!_______________________________________________________________________________
!
!*    9.      Dummy fields
!             ------------
!
 CALL PGD_DUMMY(YSC%DTCO, YSC%DUU, YSC%UG, YSC%U, YSC%USS, &
                HPROGRAM)
!_______________________________________________________________________________
!
!*   10.      Chemical Emission fields
!             ------------------------
!
 CALL READ_NAM_PGD_CHEMISTRY(HPROGRAM,YSC%CHU%CCH_EMIS)
IF (YSC%CHU%CCH_EMIS=='SNAP') THEN
  CALL PGD_CHEMISTRY_SNAP(YSC%CHN, YSC%DTCO, YSC%UG, YSC%U, YSC%USS, &
                          HPROGRAM,YSC%CHU%LCH_EMIS)
ELSE IF (YSC%CHU%CCH_EMIS=='AGGR') THEN
  CALL PGD_CHEMISTRY(YSC%CHE, YSC%DTCO, YSC%UG, YSC%U, YSC%USS, &
                     HPROGRAM,YSC%CHU%LCH_EMIS)
ENDIF
!_______________________________________________________________________________
!
!*   11.     Writing in cover latex file
!            ---------------------------
!
 CALL WRITE_COVER_TEX_END(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('PGD_SURF_ATM',1,ZHOOK_HANDLE)
!_______________________________________________________________________________
!
END SUBROUTINE PGD_SURF_ATM
