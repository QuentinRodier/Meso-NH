!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_ASCLLV (DTCO, UG, U, USS, &
                             HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_ASCLLV* - prepares ISBA field from prescribed values
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
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_ISBA,      ONLY : CTYPE_HUG     , CTYPE_TG     , &
                                  CFILE_HUG_SURF, CFILE_TG_SURF, &
                                  CFILE_HUG_ROOT, CFILE_TG_ROOT, &
                                  CFILE_HUG_DEEP, CFILE_TG_DEEP  
USE MODI_PGD_FIELD
USE MODI_GET_LATLONMASK_n
USE MODI_ABOR1_SFX
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
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: JV ! loop counter
INTEGER :: JLAYER
INTEGER :: IL
!
REAL, ALLOCATABLE, DIMENSION(:,:)     :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ASCLLV',0,ZHOOK_HANDLE)
 CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'FULL  ',NL)
!
!*      2.    get nature dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'NATURE',IL)
!
ALLOCATE(ZFIELD(IL,3))
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
!*      4.    Profile of soil relative humidity
!
  CASE('WG     ')

    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_SURF: relative humidity','NAT',CFILE_HUG_SURF,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,1))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_ROOT: relative humidity','NAT',CFILE_HUG_ROOT,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,2))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_DEEP: relative humidity','NAT',CFILE_HUG_DEEP,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,3))  

    ALLOCATE(PFIELD(IL,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = ZFIELD(:,1)
      PFIELD(:,2,JV) = ZFIELD(:,2)
      PFIELD(:,3,JV) = ZFIELD(:,3)
    END DO

!*      5.    Profile of temperatures

  CASE('TG     ')

    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_SURF: temperature','NAT',CFILE_TG_SURF,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,1))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_ROOT: temperature','NAT',CFILE_TG_ROOT,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,2))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_DEEP: temperature','NAT',CFILE_TG_DEEP,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,3))  

    ALLOCATE(PFIELD(IL,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = ZFIELD(:,1)
      PFIELD(:,2,JV) = ZFIELD(:,2)
      PFIELD(:,3,JV) = ZFIELD(:,3)
    END DO
!
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_ASCLLV: '//TRIM(HSURF)//" initialization not implemented !")
!
END SELECT
!
!*      6.     Interpolation method
!              --------------------
!
 CINTERP_TYPE='NONE  '
DEALLOCATE(ZFIELD)
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_ASCLLV
