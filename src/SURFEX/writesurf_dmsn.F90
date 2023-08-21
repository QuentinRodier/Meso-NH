!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_DMS_n(HSELECT, DSF, HPROGRAM)
!     ##########################################
!
!!****  *WRITESURF_DMS_n* - routine to write dummy surface fields
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!      P. Tulet *LAERO*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      06/2021
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DMS_SURF_FIELDS_n, ONLY : DMS_SURF_FIELDS_t
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
TYPE(DMS_SURF_FIELDS_t), INTENT(INOUT) :: DSF
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JDMS         ! loop counter
CHARACTER(LEN=3)  :: YDMS
!
CHARACTER(LEN=20) :: YSTRING20      ! string
CHARACTER(LEN=3 ) :: YSTRING03      ! string
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=MNH_LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Number of megan fields :
!               ----------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_DMS_N',0,ZHOOK_HANDLE)
!
YRECFM='DMS_GR_NBR'
YCOMMENT=' '
!
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DSF%NDMS_NBR,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       2.     DMS fields :
!               ------------
!
DO JDMS=1,DSF%NDMS_NBR
  !
  WRITE(YDMS,'(I3.3)') (JDMS)
  YRECFM='DMS_NB'//ADJUSTL(YDMS(:LEN_TRIM(YDMS)))
  YSTRING20=DSF%CDMS_NAME(JDMS)
  YSTRING03=DSF%CDMS_AREA(JDMS)
  YCOMMENT='X_Y_'//ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//'_'//ADJUSTL(YSTRING20(:LEN_TRIM(YSTRING20)))//&
          '_'//ADJUSTL(YSTRING03(:LEN_TRIM(YSTRING03)))
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,DSF%XDMS_FIELDS(:,JDMS),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DMS_NAME'//ADJUSTL(YDMS(:LEN_TRIM(YDMS)))
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,DSF%CDMS_NAME(JDMS),IRESP,HCOMMENT=YCOMMENT)
  !
  END DO
IF (LHOOK) CALL DR_HOOK('WRITESURF_DMS_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_DMS_n
