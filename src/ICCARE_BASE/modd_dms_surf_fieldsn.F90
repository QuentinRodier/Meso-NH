!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_DMS_SURF_FIELDS_n
!     ####################
!
!!****  *MODD_DMS_SURF_FIELDS* - declaration of megan physiographic data arrays
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     megan physiographic data arrays.
!
!!
!!    AUTHOR
!!    ------
!!	P. Tulet & M. Leriche  *LACy & LA*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2017                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DMS_SURF_FIELDS_t
!
  INTEGER                                          :: NDMS_NBR
!                          ! number of megan pgd fields chosen by user
  CHARACTER(LEN=3) , DIMENSION(:), POINTER         :: CDMS_AREA
!                          ! areas where megan pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
!                          !
  CHARACTER(LEN=20), DIMENSION(:), POINTER         :: CDMS_NAME
!                          ! name of the megan pgd fields (for information)
  REAL,              DIMENSION(:,:), POINTER       :: XDMS_FIELDS
!                          ! megan pgd fields themselves
!
!-------------------------------------------------------------------------------
!
END TYPE DMS_SURF_FIELDS_t

 CONTAINS
!
!
SUBROUTINE DMS_SURF_FIELDS_INIT(YDMS_SURF_FIELDS)
TYPE(DMS_SURF_FIELDS_t), INTENT(INOUT) :: YDMS_SURF_FIELDS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DMS_SURF_FIELDS_N:DMS_SURF_FIELDS_INIT",0,ZHOOK_HANDLE)
NULLIFY(YDMS_SURF_FIELDS%CDMS_NAME)
NULLIFY(YDMS_SURF_FIELDS%CDMS_AREA)
NULLIFY(YDMS_SURF_FIELDS%XDMS_FIELDS)
YDMS_SURF_FIELDS%NDMS_NBR=0
IF (LHOOK) CALL DR_HOOK("MODD_DMS_SURF_FIELDS_N:DMS_SURF_FIELDS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DMS_SURF_FIELDS_INIT


END MODULE MODD_DMS_SURF_FIELDS_n
