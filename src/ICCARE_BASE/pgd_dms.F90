!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_DMS(DTCO, UG, U, USS, DSF, HPROGRAM, OCH_DMSEMIS)
!     ##############################################################
!
!!**** *PGD_DMS* monitor for averaging and interpolations of physiographic fields
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
!!      P. Tulet *LAERO*
!!
!!    MODIFICATION
!!    ------------
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DMS_SURF_FIELDS_n,ONLY : DMS_SURF_FIELDS_t
!
USE MODD_PGD_GRID,           ONLY : NL
USE MODD_PGDWORK,            ONLY : CATYPE
USE MODD_SURF_PAR,           ONLY : XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_PGD_FIELD
USE MODI_READ_NAM_PGD_DMS
USE MODI_UNPACK_SAME_RANK
USE MODI_GET_SURF_SIZE_n
USE MODI_GET_SURF_MASK_n
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DMS_SURF_FIELDS_t), INTENT(INOUT) :: DSF
!
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(OUT)   :: OCH_DMSEMIS     ! emission flag

!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: JNBR      ! loop counter on dummy fields
INTEGER                           :: ILU, IL_SEA, IL_LAND, IL
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                             :: IDMS_NBR
CHARACTER(LEN=20), DIMENSION(1000)  :: YDMS_NAME
CHARACTER(LEN=3),  DIMENSION(1000)  :: YDMS_AREA
CHARACTER(LEN=3),  DIMENSION(1000)  :: CDMS_ATYPE    ! avg type for dummy pgd fields
!                                                      ! 'ARI' , 'INV'
CHARACTER(LEN=28), DIMENSION(1000)  :: CDMS_FILE     ! data files
CHARACTER(LEN=6),  DIMENSION(1000)  :: CDMS_FILETYPE ! type of these files
REAL, DIMENSION(:), ALLOCATABLE     :: ZDMS_FIELD, ZDMS_FIELDS
INTEGER, DIMENSION(:), ALLOCATABLE  :: IMASK
CHARACTER(LEN=6)                    :: YMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_DMS',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_DMS(HPROGRAM, IDMS_NBR, YDMS_NAME, YDMS_AREA, &
                        CDMS_ATYPE, CDMS_FILE, CDMS_FILETYPE      )  
!
DSF%NDMS_NBR     = IDMS_NBR
!
ALLOCATE(DSF%CDMS_NAME(DSF%NDMS_NBR))
ALLOCATE(DSF%CDMS_AREA(DSF%NDMS_NBR))
DSF%CDMS_NAME(:) = YDMS_NAME(1:DSF%NDMS_NBR)
DSF%CDMS_AREA(:) = YDMS_AREA(1:DSF%NDMS_NBR)
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocation
!             ----------
!
ALLOCATE(DSF%XDMS_FIELDS(NL,DSF%NDMS_NBR))
 CALL GET_SURF_SIZE_n(DTCO, U,'LAND', IL_LAND)
 CALL GET_SURF_SIZE_n(DTCO, U,'SEA   ',IL_SEA)
!
ALLOCATE(ZDMS_FIELDS (NL))
!
!-------------------------------------------------------------------------------
OCH_DMSEMIS = DSF%NDMS_NBR > 0
!-------------------------------------------------------------------------------
!
!
!*    4.      Computations
!             ------------
!
DO JNBR=1,DSF%NDMS_NBR

  CATYPE = CDMS_ATYPE(JNBR)
  SELECT CASE (DSF%CDMS_AREA(JNBR))
    CASE ('LAN')
      IL = IL_LAND
      YMASK='LAND  '
    CASE ('SEA')
      IL = IL_SEA
      YMASK='SEA   '
    CASE ('ALL')
      IL = NL
      YMASK='FULL  '
    CASE DEFAULT
      CALL ABOR1_SFX('PGD_DMS (1): DMS AREA NOT SUPPORTED')
  END SELECT
  ALLOCATE(ZDMS_FIELD (IL))
  ALLOCATE(IMASK(IL))
!
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,DSF%CDMS_NAME(JNBR),DSF%CDMS_AREA(JNBR),CDMS_FILE(JNBR), &
                 CDMS_FILETYPE(JNBR),XUNDEF,ZDMS_FIELD(:)              )  
  CATYPE = 'ARI'
!
!*    4.2     Expends field on all surface points
  ILU=0
  CALL GET_SURF_MASK_n(DTCO, U, &
                       YMASK,IL,IMASK,ILU,ILUOUT)
  CALL UNPACK_SAME_RANK(IMASK,ZDMS_FIELD(:),ZDMS_FIELDS(:))
  DEALLOCATE(ZDMS_FIELD)
  DEALLOCATE(IMASK)
!
!*    4.3      Weights field on all surface points 
!              (zero weight where field is not defined)
  SELECT CASE (DSF%CDMS_AREA(JNBR))
    CASE ('LAN')
      DSF%XDMS_FIELDS(:,JNBR) = (U%XNATURE(:)+U%XTOWN(:))*ZDMS_FIELDS(:)
    CASE ('SEA')
      DSF%XDMS_FIELDS(:,JNBR) = U%XSEA*ZDMS_FIELDS(:)
    CASE ('ALL')
      DSF%XDMS_FIELDS(:,JNBR) = ZDMS_FIELDS(:)
    CASE DEFAULT
      CALL ABOR1_SFX('PGD_DMS (2): DMS AREA NOT SUPPORTED')
  END SELECT

END DO

DEALLOCATE(ZDMS_FIELDS)

IF (LHOOK) CALL DR_HOOK('PGD_DMS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_DMS
