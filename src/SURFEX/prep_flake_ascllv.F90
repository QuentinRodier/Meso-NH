!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_FLAKE_ASCLLV (DTCO, UG, U, USS, &
                              HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_FLAKE_ASCLLV* - prepares FLAKE field from prescribed values
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2007
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PGD_GRID,       ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_FLAKE,     ONLY : CTYPE, CFILE_FLAKE
USE MODI_PGD_FIELD
USE MODI_GET_LATLONMASK_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, POINTER, DIMENSION(:,:)   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: IL
!
REAL, ALLOCATABLE, DIMENSION(:)     :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_ASCLLV',0,ZHOOK_HANDLE)
 CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'FULL  ',NL)
!
!*      2.    get water dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'WATER ',IL)
!
ALLOCATE(ZFIELD(IL))
!
!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(UG, &
                       LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!
SELECT CASE(HSURF)
!
!
!*      5.    surface temperature

  CASE('TS     ')

    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TS_WATER: temperature','WAT',CFILE_FLAKE,   &
                        CTYPE,XUNDEF,ZFIELD(:))  

    ALLOCATE(PFIELD(IL,1))
    PFIELD(:,1) = ZFIELD(:)

END SELECT
!
!*      6.     Interpolation method
!              --------------------
!
 CINTERP_TYPE='NONE  '
DEALLOCATE(ZFIELD)
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_FLAKE_ASCLLV
