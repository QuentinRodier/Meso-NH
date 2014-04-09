!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_NETCDF(HPROGRAM,HSURF,HFILE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_NETCDF* - prepares ISBA fields from initialization files in NETCDF
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
!!     M. Lafaysse
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!      J.Escobar   11/2013  Add USE MODI_GET_TYPE_DIM_n
!!------------------------------------------------------------------
!

USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE

USE MODI_ABOR1_SFX

USE MODE_READ_CDF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE

INCLUDE 'netcdf.inc'
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:),       POINTER :: ZFIELD   ! field read

REAL,DIMENSION(:,:),ALLOCATABLE:: ZFIELD_2D

! CHARACTER(LEN=28) :: YNCVAR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER::IERROR !error status
INTEGER::JJ,JK,JLOOP ! loop counters
INTEGER::INLAYERS ! vertical dimension length
INTEGER::IL ! nature dimension length
INTEGER::ID_FILE,ID_VAR ! Netcdf IDs for file and variable
INTEGER::INVARDIMS !number of dimensions of netcdf input variable
INTEGER,DIMENSION(:),ALLOCATABLE::IVARDIMSID
INTEGER::ILENDIM,ILENDIM1,ILENDIM2

SELECT CASE (TRIM(HSURF))
  CASE ('TG','WG','WGI')
    INLAYERS=3 ! 3 soil layers for initialization
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_NETCDF: '//TRIM(HSURF)//" initialization not implemented !")
END SELECT
!
INLAYERS=3
!
!------------------------------------------------------------------------------------
!              ---------
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_NETCDF',0,ZHOOK_HANDLE)

!*      1.    get nature dimension
!
 CALL GET_TYPE_DIM_n('NATURE',IL)
!
!*      2.     Reading of field
!              ----------------

! Open netcdf file
IERROR=NF_OPEN(HFILE,NF_NOWRITE,ID_FILE)
 CALL HANDLE_ERR_CDF(IERROR,"can't open file "//TRIM(HFILE))

! Look for variable ID
IERROR=NF_INQ_VARID(ID_FILE,TRIM(HSURF),ID_VAR)
 CALL HANDLE_ERR_CDF(IERROR,"can't find variable "//TRIM(HSURF))

! Number of dimensions
IERROR=NF_INQ_VARNDIMS(ID_FILE,ID_VAR,INVARDIMS)
if (IERROR/=NF_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions number")

! Id of dimensions
ALLOCATE(IVARDIMSID(INVARDIMS))
IERROR=NF_INQ_VARDIMID(ID_FILE,ID_VAR,IVARDIMSID)
if (IERROR/=NF_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions ids")

ALLOCATE(ZFIELD(IL))

SELECT CASE (INVARDIMS)
  CASE (1)
    ! Check dimension length
    IERROR=NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(1),ILENDIM)
    if (IERROR/=NF_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
  CASE (2)
    IERROR=NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(1),ILENDIM1)
    if (IERROR/=NF_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")
    IERROR=NF_INQ_DIMLEN(ID_FILE,IVARDIMSID(2),ILENDIM2)
    if (IERROR/=NF_NOERR) CALL HANDLE_ERR_CDF(IERROR,"can't get variable dimensions lengths")

    ILENDIM=ILENDIM1*ILENDIM2

  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_NETCDF: incorrect number of dimensions for variable '//TRIM(HSURF))

END SELECT
!
IF(ILENDIM/=IL) CALL ABOR1_SFX('PREP_ISBA_NETCDF: incorrect number of points '// &
                                'in netcdf file for variable '//TRIM(HSURF))
!
! Read 1D variable
IERROR=NF_GET_VAR_DOUBLE(ID_FILE,ID_VAR,ZFIELD)
 CALL HANDLE_ERR_CDF(IERROR,"can't read variable "//TRIM(HSURF))
!
! Close netcdf file
IERROR=NF_CLOSE(ID_FILE)
!
ALLOCATE(PFIELD(IL,INLAYERS,NVEGTYPE)) !will be deallocated later by prep_hor_isba_field
!
! For now initial values are identical for all tiles / soil layers.
DO JJ=1,INLAYERS
  DO JK=1,NVEGTYPE
    PFIELD(:,JJ,JK)=ZFIELD
  END DO
END DO
!
DEALLOCATE(ZFIELD)
!
!Interpolation method
CINTERP_TYPE='NONE'
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_NETCDF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_NETCDF
