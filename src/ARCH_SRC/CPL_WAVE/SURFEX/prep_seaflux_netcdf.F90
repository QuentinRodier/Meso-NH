!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_NETCDF(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_SEAFLUX_NETCDF* - prepares SEAFLUX fields from oceanic analyses in NETCDF
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
!!     C. Lebeaupin Brossier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!      Modified    09/2013 : S. Senesi : extends to SSS and SIC fields 
!!      Modified    03/2014 : M.N. Bouin  ! possibility of wave parameters
!!                                        ! from external source
!!------------------------------------------------------------------
!
USE MODE_READ_NETCDF_MERCATOR
!
!USE MODD_TYPE_DATE_SURF
!
USE MODD_PREP,       ONLY : CINGRID_TYPE
USE MODD_GRID_LATLONREGUL,  ONLY : NILENGTH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
!TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time
!CHARACTER(LEN=6)              :: YINMODEL ! model from which GRIB file originates
REAL, DIMENSION(:),       POINTER :: ZFIELD   ! field read
 CHARACTER(LEN=28) :: YNCVAR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Grid type
!              ---------
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_NETCDF',0,ZHOOK_HANDLE)
CINGRID_TYPE='LATLON '
!
!*      2.     Reading of field
!              ----------------
!-----------------
SELECT CASE(HSURF)
!-----------------
!
!* 2.1 Orography
!      ---------
!
  CASE('ZS     ')
    YNCVAR='topo'
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    CALL READ_NETCDF_ZS_SEA(HFILE,YNCVAR,ZFIELD)
    ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
    PFIELD(:,1) = ZFIELD(:)
    DEALLOCATE(ZFIELD)
!
!
!* 2.2 Temperature profiles
!      --------------------
!
  CASE('SST    ','SSS    ','SIC    ')
    IF ( HSURF == 'SST    ') THEN
       YNCVAR='temperature'
    ELSE IF ( HSURF == 'SSS    ') THEN
       YNCVAR='sss'
    ELSE IF ( HSURF == 'SIC    ') THEN
       YNCVAR='sic'
    END IF
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    CALL READ_NETCDF_SST(HFILE,YNCVAR,ZFIELD)
    ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
    PFIELD(:,1) = ZFIELD(:)
    DEALLOCATE(ZFIELD)
!
!
!* 2.3 Wave parameters
!      --------------------
!
  CASE('HS     ')
    YNCVAR='significant_h'
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    CALL READ_NETCDF_WAVE(HFILE,YNCVAR,ZFIELD)
    ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
    PFIELD(:,1) = ZFIELD(:)
    DEALLOCATE(ZFIELD)
!
  CASE('TP     ')
    YNCVAR='peak_period'
    CALL PREP_NETCDF_GRID(HFILE,YNCVAR)
    CALL READ_NETCDF_WAVE(HFILE,YNCVAR,ZFIELD)
    ALLOCATE(PFIELD(MAX(1,NILENGTH),1))
    PFIELD(:,1) = ZFIELD(:)
    DEALLOCATE(ZFIELD)
!
END SELECT
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_NETCDF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SEAFLUX_NETCDF
