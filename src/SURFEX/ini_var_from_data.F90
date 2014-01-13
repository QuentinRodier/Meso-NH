!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
MODULE MODI_INI_VAR_FROM_DATA
!
INTERFACE INI_VAR_FROM_DATA
!
SUBROUTINE INI_VAR_FROM_DATA_1D(HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM, &
                                HFTYP, PUNIF, PFIELD, OPRESENT)
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), DIMENSION(:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:), INTENT(IN) :: HFTYP
REAL, DIMENSION(:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD
LOGICAL, INTENT(OUT) :: OPRESENT
!
END SUBROUTINE INI_VAR_FROM_DATA_1D
!
!
      SUBROUTINE INI_VAR_FROM_DATA_2D(HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM, &
                                      HFTYP, PUNIF, PFIELD_TIME, OPRESENT)
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
 CHARACTER(LEN=28), DIMENSION(:,:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:,:), INTENT(IN) :: HFTYP
REAL, DIMENSION(:,:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD_TIME
LOGICAL, INTENT(OUT) :: OPRESENT
!
END SUBROUTINE INI_VAR_FROM_DATA_2D
!
!
END INTERFACE INI_VAR_FROM_DATA
!
END MODULE MODI_INI_VAR_FROM_DATA
!
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_1D(HPROGRAM, HATYPE, HNAME ,HTYPE, HFNAM, &
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
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_DATA_ISBA_n, ONLY : XPAR_VEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=28), DIMENSION(:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:), INTENT(IN) :: HFTYP
REAL, DIMENSION(:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=40) :: YNAME
LOGICAL, DIMENSION(SIZE(PFIELD,2)) :: LPRESENT 
INTEGER               :: JI, JJ  ! loop counter on vegtypes
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_1D',0,ZHOOK_HANDLE)
!
OPRESENT=.FALSE.
YNAME=ADJUSTL(HNAME)
!
DO JI=1,SIZE(PFIELD,2)
  CALL INI_VAR_FROM_DATA_0D(HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JI), &
              HFTYP(JI), PUNIF(JI), PFIELD(:,JI), LPRESENT(JI))
ENDDO
!
IF (ANY(LPRESENT(:))) THEN

  OPRESENT=.TRUE.

  IF (SIZE(PFIELD,2)==NVEGTYPE .AND. YNAME(1:7).NE.'VEGTYPE') THEN

    !if a vegtype are missing, the last present gives it his values
    DO JI=2,SIZE(PFIELD,2)
      IF (.NOT.LPRESENT(JI)) THEN
        DO JJ=JI,1,-1
          IF (LPRESENT(JJ)) THEN
            PFIELD(:,JI)=PFIELD(:,JJ)
            LPRESENT(JI)=.TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO

    DO JI=1,SIZE(PFIELD,2)
      IF (LPRESENT(JI)) THEN
        WHERE (XPAR_VEGTYPE(:,JI).EQ.0.) PFIELD(:,JI)=0.0
      ELSE
        PFIELD(:,JI)=0.
      ENDIF
    ENDDO

  ELSEIF (.NOT.ALL(LPRESENT)) THEN
    CALL ABOR1_SFX("INI_VAR_FROM_DATA_1D: MISSING INPUT DATA FOR "//HNAME)
  ENDIF
ENDIF
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_1D
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_2D(HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM, &
                                       HFTYP, PUNIF, PFIELD_TIME, OPRESENT)
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
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_DATA_ISBA_n, ONLY : XPAR_VEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_PUT_IN_TIME
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=28), DIMENSION(:,:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:,:), INTENT(IN) :: HFTYP
REAL, DIMENSION(:,:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD_TIME
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL, DIMENSION(SIZE(PFIELD_TIME,3)) :: LPRESENT
LOGICAL, DIMENSION(SIZE(PFIELD_TIME,2)) :: LPRESENT_TIME
INTEGER               :: JI, JJ  ! loop counter on vegtypes
INTEGER               :: JTIME
INTEGER               :: ITIME
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_2D',0,ZHOOK_HANDLE)
!
OPRESENT=.FALSE.
LPRESENT_TIME(:)=.FALSE.
ITIME=0
!
DO JTIME=1,SIZE(PFIELD_TIME,2)

  DO JI=1,SIZE(PFIELD_TIME,3)

    CALL INI_VAR_FROM_DATA_0D(HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JI,JTIME), &
              HFTYP(JI,JTIME), PUNIF(JI,JTIME), PFIELD_TIME(:,JTIME,JI),&
              LPRESENT(JI))

  ENDDO 

  IF (ANY(LPRESENT(:))) THEN

    LPRESENT_TIME(JTIME)=.TRUE.
    OPRESENT=.TRUE.
    ITIME=ITIME+1

    IF (SIZE(PFIELD_TIME,3)==NVEGTYPE) THEN

      DO JI=2,SIZE(PFIELD_TIME,3)
        IF (.NOT.LPRESENT(JI)) THEN
          DO JJ=JI,1,-1
            IF (LPRESENT(JJ)) THEN
              PFIELD_TIME(:,JTIME,JI)=PFIELD_TIME(:,JTIME,JJ)
              LPRESENT(JI)=.TRUE.
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DO JI=1,SIZE(PFIELD_TIME,3)
        IF (LPRESENT(JI)) THEN
          WHERE (XPAR_VEGTYPE(:,JI).EQ.0.) PFIELD_TIME(:,JTIME,JI)=0.0
        ELSE
          PFIELD_TIME(:,JTIME,JI)=0.
        ENDIF
      ENDDO

    ELSEIF (.NOT.ALL(LPRESENT)) THEN
      CALL ABOR1_SFX("INI_VAR_FROM_DATA_1D: MISSING INPUT DATA FOR "//HNAME)
    ENDIF

  ENDIF

ENDDO
!
IF (OPRESENT) THEN
  IF (SIZE(PFIELD_TIME,2)==36) THEN
     CALL PUT_IN_TIME(ITIME,36,PFIELD_TIME)
  ELSE
    IF (ANY(LPRESENT_TIME(:)) .AND. .NOT.ALL(LPRESENT_TIME(:))) &
      CALL ABOR1_SFX("INI_VAR_FROM_DATA_2D: MISSING INPUT DATA FOR "//HNAME)
  ENDIF
ENDIF
!
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_2D
