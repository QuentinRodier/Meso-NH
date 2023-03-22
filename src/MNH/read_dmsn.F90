!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_DMS_n(DSF, U, HPROGRAM)
!     #################################
!
!!****  *READ_DMS_n* - routine to read oceanic DMS surface fields
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!      P. Tulet  *LAERO*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     06/2021
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DMS_SURF_FIELDS_n, ONLY : DMS_SURF_FIELDS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DMS_SURF_FIELDS_t), INTENT(INOUT) :: DSF
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JDMS         ! loop counter
CHARACTER(LEN=3)  :: YDMS
!
 CHARACTER(LEN=20 ):: YSTRING20      ! string
 CHARACTER(LEN=3  ):: YSTRING03      ! string
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=MNH_LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       2.     Number of dummy fields :
!               ----------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DMS_N',0,ZHOOK_HANDLE)
!
YRECFM='DMS_GR_NBR'
YCOMMENT=' '
!
 CALL READ_SURF(HPROGRAM,YRECFM,DSF%NDMS_NBR,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       3.     Dummy fields :
!               ------------
!
ALLOCATE(DSF%CDMS_NAME(DSF%NDMS_NBR))
ALLOCATE(DSF%CDMS_AREA(DSF%NDMS_NBR))
ALLOCATE(DSF%XDMS_FIELDS(U%NSIZE_FULL,DSF%NDMS_NBR))
DSF%CDMS_NAME(:) = ' '
DSF%CDMS_AREA(:) = 'SEA'
!
!
DO JDMS=1,DSF%NDMS_NBR
  !
  WRITE(YDMS,'(I3.3)') (JDMS)
  YRECFM='DMS_NB'//ADJUSTL(YDMS(:LEN_TRIM(YDMS)))
  YSTRING20=DSF%CDMS_NAME(JDMS)
  YSTRING03=DSF%CDMS_AREA(JDMS)
  YCOMMENT='X_Y_'//ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//'_'//ADJUSTL(YSTRING20(:LEN_TRIM(YSTRING20)))//&
          '_'//ADJUSTL(YSTRING03(:LEN_TRIM(YSTRING03)))

  CALL READ_SURF(HPROGRAM,YRECFM,DSF%XDMS_FIELDS(:,JDMS),IRESP,HCOMMENT=YCOMMENT)
   
  YRECFM='DMS_NAME'//ADJUSTL(YDMS(:LEN_TRIM(YDMS)))
  CALL READ_SURF(HPROGRAM,YRECFM,DSF%CDMS_NAME(JDMS),IRESP,HCOMMENT=YCOMMENT)
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('READ_DMS_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DMS_n
