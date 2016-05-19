!     #########
      SUBROUTINE PGD_DUMMY(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_DUMMY* monitor for averaging and interpolations of physiographic fields
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
!!    Original    10/12/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,           ONLY : NL
USE MODD_PGDWORK,            ONLY : CATYPE
USE MODD_SURF_PAR,           ONLY : XUNDEF
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : NDUMMY_NBR, CDUMMY_AREA, CDUMMY_NAME, XDUMMY_FIELDS
!
USE MODI_GET_LUOUT
USE MODI_PGD_FIELD
USE MODI_READ_NAM_PGD_DUMMY
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
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: JNBR      ! loop counter on dummy fields
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                             :: IDUMMY_NBR
 CHARACTER(LEN=20), DIMENSION(1000)  :: YDUMMY_NAME
 CHARACTER(LEN=3),  DIMENSION(1000)  :: YDUMMY_AREA
 CHARACTER(LEN=3),  DIMENSION(1000)  :: CDUMMY_ATYPE    ! avg type for dummy pgd fields
!                                                      ! 'ARI' , 'INV'
 CHARACTER(LEN=28), DIMENSION(1000)  :: CDUMMY_FILE     ! data files
 CHARACTER(LEN=6),  DIMENSION(1000)  :: CDUMMY_FILETYPE ! type of these files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_DUMMY',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_DUMMY(HPROGRAM, IDUMMY_NBR, YDUMMY_NAME, YDUMMY_AREA, &
                          CDUMMY_ATYPE, CDUMMY_FILE, CDUMMY_FILETYPE      )  
!
NDUMMY_NBR     = IDUMMY_NBR
CDUMMY_NAME(:) = YDUMMY_NAME(:)
CDUMMY_AREA(:) = YDUMMY_AREA(:)
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocation
!             ----------
!
ALLOCATE(XDUMMY_FIELDS(NL,NDUMMY_NBR))
!
!-------------------------------------------------------------------------------
!
!*    4.      Computations
!             ------------
!
DO JNBR=1,NDUMMY_NBR
  CATYPE = CDUMMY_ATYPE(JNBR)
  CALL PGD_FIELD(HPROGRAM,CDUMMY_NAME(JNBR),CDUMMY_AREA(JNBR),CDUMMY_FILE(JNBR), &
                   CDUMMY_FILETYPE(JNBR),XUNDEF,XDUMMY_FIELDS(:,JNBR)              )  
  CATYPE = 'ARI'
END DO
IF (LHOOK) CALL DR_HOOK('PGD_DUMMY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_DUMMY
