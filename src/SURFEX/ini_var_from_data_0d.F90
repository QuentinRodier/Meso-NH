!     #########
      SUBROUTINE INI_VAR_FROM_DATA_0D(HPROGRAM, HATYPE,  HNAME, HTYPE, HFNAM, &
                                        HFTYP, PUNIF, PFIELD, OPRESENT)
!     ##############################################################
!
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    16/11/10
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_PGDWORK,       ONLY : CATYPE
!
USE MODI_PGD_FIELD
USE MODI_READ_FROM_SURFEX_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), INTENT(IN) :: HFTYP
REAL, INTENT(IN) :: PUNIF
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_DATA_0D',0,ZHOOK_HANDLE)
!
PFIELD(:)=XUNDEF
OPRESENT=.FALSE.
!
IF (HFTYP.EQ.'FA    ' .OR. HFTYP.EQ.'ASCII ' .OR. HFTYP.EQ.'LFI   ') THEN

  OPRESENT=.TRUE.
  SELECT CASE (HTYPE)
    CASE ('LAN')
      CALL READ_FROM_SURFEX_FILE(HFTYP,HFNAM,'SURF  ','      ',PFIELD)
    CASE ('TWN')
      CALL READ_FROM_SURFEX_FILE(HFTYP,HFNAM,'TOWN  ','      ',PFIELD)              
    CASE ('NAT')
      CALL READ_FROM_SURFEX_FILE(HFTYP,HFNAM,'NATURE','      ',PFIELD)              
    CASE ('SEA')
      CALL READ_FROM_SURFEX_FILE(HFTYP,HFNAM,'SEA   ','      ',PFIELD)              
    CASE ('WAT')
      CALL READ_FROM_SURFEX_FILE(HFTYP,HFNAM,'WATER ','      ',PFIELD)              
   END SELECT

ELSE

  CATYPE = HATYPE
  CALL PGD_FIELD(HPROGRAM,HNAME,HTYPE,HFNAM,HFTYP,PUNIF,PFIELD(:),OPRESENT=OPRESENT)

ENDIF
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_DATA_0D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_0D

