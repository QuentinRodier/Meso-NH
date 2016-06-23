!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_WATFLUX (DTCO, U, WG, W, &
                              HPROGRAM)
!     ##############################################################
!
!!**** *PGD_WATFLUX* monitor for averaging and interpolations of WATFLUX physiographic fields
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
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_GRID_n, ONLY : WATFLUX_GRID_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_COVER_TEX_WATER
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_WATFLUX',0,ZHOOK_HANDLE)
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'WATER ',WG%NDIM)
!
ALLOCATE(W%LCOVER     (JPCOVER))
ALLOCATE(W%XZS        (WG%NDIM))
ALLOCATE(WG%XLAT       (WG%NDIM))
ALLOCATE(WG%XLON       (WG%NDIM))
ALLOCATE(WG%XMESH_SIZE (WG%NDIM))
!
 CALL PACK_PGD(DTCO, U, &
               HPROGRAM, 'WATER ',                    &
                WG%CGRID,  WG%XGRID_PAR,                     &
                W%LCOVER, W%XCOVER, W%XZS,                   &
                WG%XLAT, WG%XLON, WG%XMESH_SIZE                 )  
!
!-------------------------------------------------------------------------------
 CALL WRITE_COVER_TEX_WATER
IF (LHOOK) CALL DR_HOOK('PGD_WATFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_WATFLUX
